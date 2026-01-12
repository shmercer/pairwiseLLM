#' Run a round-based adaptive BT workflow end-to-end
#'
#' This exported runner implements the core adaptive loop:
#' \enumerate{
#' \item propose pairs (initial random bootstrap, then adaptive),
#' \item score pairs using a provided judge function (LLM/human/simulator),
#' \item append results,
#' \item fit a BT model,
#' \item compute stopping metrics and decide whether to stop,
#' \item repeat until stopping criteria are met or \code{max_rounds} is reached.
#' }
#'
#' In addition to per-round stop metrics, \code{bt_run_adaptive()} records a compact
#' per-round \emph{state snapshot} (counts of unique unordered pairs judged, ID appearance
#' distribution, position imbalance, and basic missingness checks). This is intended for
#' debugging and monitoring convergence/coverage over rounds.
#'
#' \strong{No reverse-order checks during adaptive sampling.}
#' Optionally, after stopping (or hitting \code{max_rounds}), you can run a
#' post-hoc reverse-order audit on a random subset of already-judged pairs and
#' compute forward-vs-reverse consistency via \code{\link{compute_reverse_consistency}}.
#'
#' The default modeling function is \code{\link{fit_bt_model}} and the default
#' stopping+pairing helper is \code{\link{bt_adaptive_round}}.
#'
#' @param samples A tibble/data.frame with columns \code{ID} and \code{text}.
#' \code{ID} must be unique and non-missing; \code{text} is the content shown to the judge.
#'
#' @param judge_fun A function that accepts a tibble of pairs with columns
#' \code{ID1}, \code{text1}, \code{ID2}, \code{text2} and returns a tibble with
#' columns \code{ID1}, \code{ID2}, and \code{better_id}.
#' If \code{judge} is provided, the returned tibble must also include that judge column.
#' \code{better_id} may be returned as the literal winning ID (\code{ID1} or \code{ID2})
#' or as common positional labels (e.g., \code{SAMPLE_1}/\code{SAMPLE_2},
#' \code{ID1}/\code{ID2}, \code{1}/\code{2}, \code{0}/\code{1}, \code{A}/\code{B},
#' \code{LEFT}/\code{RIGHT}); these are normalized to the corresponding IDs.
#' Blank/NA-like winners are treated as missing (\code{NA}) and are ignored in scoring.
#'
#' @param initial_results Optional tibble/data.frame of already-scored pairs with columns
#' \code{ID1}, \code{ID2}, \code{better_id} (and optional judge column). When provided,
#' these results are used as the starting state (and no bootstrap is performed unless
#' \code{initial_results} is empty).
#'
#' @param judge Optional character scalar giving the name of the column in results that
#' identifies the judge/backend/model (e.g., \code{"gpt4o"} vs \code{"claude"}).
#' When provided, the column must be present in outputs from \code{judge_fun} and is
#' passed to \code{\link{build_bt_data}} so engines that support judge effects can use it.
#'
#' @param engine Character scalar passed to \code{fit_fun} as its \code{engine} argument.
#' Default \code{"sirt"}.
#' @param fit_verbose Logical; passed to \code{fit_fun} as \code{verbose}. Default \code{FALSE}.
#' @param return_diagnostics Logical; passed to \code{fit_fun}. If \code{TRUE}, attempt to
#' return engine-specific diagnostics (e.g., item fit, separation/reliability). Default \code{TRUE}.
#' @param include_residuals Logical; passed to \code{fit_fun}. If \code{TRUE}, request
#' residual/probability outputs when supported (may increase compute/memory). Default \code{FALSE}.
#'
#' @param fit_engine_running Character. Estimator used for the \emph{running} ability
#'   vector that drives adaptive pair selection.
#'
#'   \describe{\item{\code{"bt"}}{Use the BT fit returned by \code{fit_fun}.}}
#'   \describe{\item{\code{"rank_centrality"}}{Compute Rank Centrality scores from the
#'   current comparison graph (often more stable early on sparse graphs) while still
#'   using BT standard errors as an uncertainty heuristic when available.}}
#'   \describe{\item{\code{"hybrid"}}{Start with Rank Centrality as the running ability
#'   estimate, then switch to BT once the graph is sufficiently connected and Rank
#'   Centrality ranks are stable. See \code{stage1_*} controls.}}
#'
#'   Default \code{"bt"}.
#' @param rc_smoothing Numeric. Smoothing parameter forwarded to
#'   \code{\link{fit_rank_centrality}} when
#'   \code{fit_engine_running = "rank_centrality"} (and stored in per-round fits).
#'   See \code{\link{fit_rank_centrality}}.
#' @param rc_damping Numeric. Damping parameter forwarded to
#'   \code{\link{fit_rank_centrality}} when
#'   \code{fit_engine_running = "rank_centrality"} (and stored in per-round fits).
#'   See \code{\link{fit_rank_centrality}}.
#'
#' @param stage1_k_conn Integer. For \code{fit_engine_running = "hybrid"}, require the
#'   connectivity gate to hold for this many consecutive rounds before allowing a stage
#'   switch.
#' @param stage1_k_stab Integer. For \code{fit_engine_running = "hybrid"}, require the
#'   stage-1 stability gate (as defined by \code{stage1_stability_metric}) to hold for this
#'   many consecutive rounds before allowing a stage switch.
#' @param stage1_min_pct_nodes_with_degree_gt0 Numeric in \code{[0, 1]}. For
#'   \code{fit_engine_running = "hybrid"}, minimum fraction of nodes that have appeared in
#'   at least one comparison (degree > 0) for the connectivity gate.
#' @param stage1_min_largest_component_frac Numeric in \code{[0, 1]}. For
#'   \code{fit_engine_running = "hybrid"}, minimum fraction of nodes in the largest
#'   connected component for the connectivity gate.
#' @param stage1_min_degree_median Numeric. For \code{fit_engine_running = "hybrid"},
#'   minimum median node degree for the connectivity gate.
#' @param stage1_min_degree_min_lcc Numeric. For \code{fit_engine_running = "hybrid"},
#'   minimum node degree within the largest connected component (LCC) for the
#'   connectivity gate.
#' @param stage1_min_degree_min Numeric. (Deprecated) For
#'   \code{fit_engine_running = "hybrid"}, minimum node degree over \emph{all} nodes
#'   for the connectivity gate. This can effectively force 100% coverage when any node
#'   is still unseen (degree 0). Prefer \code{stage1_min_degree_min_lcc}.
#' @param stage1_min_spearman Numeric. For \code{fit_engine_running = "hybrid"}, minimum
#'   Spearman correlation between consecutive RC rank vectors to count as stable.
#' @param stage1_stability_metric Character. For \code{fit_engine_running = "hybrid"},
#'   which stability metric to use in stage1 when deciding whether to switch to BT.
#'   One of \code{"topk_jaccard"} (top-\code{k} overlap on RC ranks) or \code{"spearman"}
#'   (Spearman correlation of RC ranks). Default \code{"topk_jaccard"}.
#' @param stage1_topk Integer or \code{NULL}. For \code{stage1_stability_metric = "topk_jaccard"},
#'   the \code{k} used for the top-\code{k} overlap check. If \code{NULL} (default),
#'   uses \code{max(10, ceiling(0.1 * N))} where \code{N = nrow(samples)}.
#' @param stage1_topk_overlap Numeric in \code{[0, 1]}. For
#'   \code{stage1_stability_metric = "topk_jaccard"}, minimum top-\code{k} overlap fraction
#'   between consecutive RC rounds required to count as stable. Default \code{0.85}.
#' @param stage1_allow_degenerate_stability Logical. If \code{FALSE} (default), degenerate
#'   RC ranks (e.g., constant/undefined Spearman) do not count as stable in stage1.
#'   If \code{TRUE}, degenerate stability is treated as perfectly stable (legacy behavior).
#' @param stage1_max_bridge_edge_frac Numeric in \code{[0, 1]}. For \code{fit_engine_running = "hybrid"},
#'   additional mixing-guard threshold on the bridge-edge fraction proxy (computed only on scheduled
#'   checks). Stage1 can switch to BT only when \code{bridge_edge_frac <= stage1_max_bridge_edge_frac}
#'   for \code{stage1_k_mix} consecutive checks. Default \code{0.02}.
#' @param stage1_k_mix Integer. For \code{fit_engine_running = "hybrid"}, number of consecutive
#'   mixing checks that must pass before switching stage1\eqn{\to}stage2. Default \code{2}.
#' @param stage1_check_mix_every Integer. For \code{fit_engine_running = "hybrid"}, compute the bridge-edge
#'   fraction proxy every \code{stage1_check_mix_every} rounds (to reduce runtime). Default \code{2}.
#' @param stage1_max_rounds Integer. For \code{fit_engine_running = "hybrid"}, maximum
#'   number of Stage 1 rounds to attempt before triggering stage-1 fail-safe escalation.
#' @param stage1_escalated_explore_frac Numeric in \code{[0, 1]}. For \code{fit_engine_running = "hybrid"},
#'   when Stage 1 reaches \code{stage1_max_rounds} without meeting the connectivity gate,
#'   increase exploration to at least this fraction. Default \code{0.5}.
#' @param stage1_escalated_k_neighbors Integer (or \code{NULL}/\code{Inf}). For \code{fit_engine_running = "hybrid"},
#'   when Stage 1 reaches \code{stage1_max_rounds} without meeting the connectivity gate,
#'   expand neighbor windows by setting \code{k_neighbors} to at least this value.
#'   Use \code{Inf} for "all neighbors". Default \code{Inf}.
#' @param stage1_escalate_allow_unordered_repeats Logical. For \code{fit_engine_running = "hybrid"},
#'   when Stage 1 is escalated, optionally allow unordered repeats to avoid a hard
#'   \code{"no_new_pairs"} stop when constraints are too restrictive. Default \code{FALSE}.
#' @param stage1_explore_frac Fraction of each round reserved for exploration while in stage1 (hybrid only).
#' @param stage2_explore_frac Fraction of each round reserved for exploration while in stage2 (hybrid only).
#' @param stage2_min_rounds Minimum number of BT rounds to run after switching to stage2 before allowing precision/stability stops.
#' @param final_refit Logical. If \code{TRUE} (default), compute a final combined
#'   estimates table (Rank Centrality plus optional BT variants) at the end of the
#'   run via \code{\link{compute_final_estimates}}. Suggested dependencies are
#'   used when available; if unavailable or if a BT fit fails, the function falls
#'   back to Rank Centrality and records the reason in \code{out$fit_provenance}.
#' @param fit_engine_final Preferred final BT engine used when \code{final_refit = TRUE}.
#'   Options are \code{"bt_firth"}, \code{"bt_mle"}, \code{"bt_bayes"}, and \code{"none"}.
#'   See \code{\link{compute_final_estimates}} for details. Default \code{"bt_firth"}.
#' @param final_bt_bias_reduction Logical. Passed to \code{\link{compute_final_estimates}}
#'   as \code{bt_bias_reduction}. Default \code{TRUE}. Typically leave \code{TRUE}
#'   when \code{fit_engine_final = "bt_firth"}.
#'
#' @param round_size Integer. Number of new pairs to propose and score in each adaptive round.
#' If \code{0}, the runner will fit once (if possible) and then stop without proposing new pairs.
#' @param init_round_size Integer. Number of bootstrap (random) pairs to score before the first
#' model fit when \code{initial_results} is \code{NULL} or empty. Default: \code{round_size}.
#' @param max_rounds Integer. Maximum number of adaptive rounds to run (excluding the bootstrap
#' scoring step). Default \code{50}.
#' @param min_rounds Integer. Minimum number of adaptive rounds to run before allowing
#'   stability- or precision-based stopping. Hard stops (no new pairs, budget exhausted,
#'   max rounds) can still terminate earlier. Default \code{2}.
#' @param stop_stability_rms Numeric. Threshold on RMS change in \code{theta} between consecutive
#'   fits; lower values indicate greater stability. Default \code{0.01}.
#' @param stop_stability_consecutive Integer. Number of consecutive rounds the stability criteria
#'   must hold before stopping. Default \code{2}.
#' @param stop_topk Integer. Size \code{k} for the top-\code{k} overlap stability check. Default \code{50}.
#' @param stop_topk_overlap Numeric in \code{[0, 1]}. Minimum overlap fraction between consecutive top-\code{k}
#'   sets required to consider rankings stable. Default \code{0.95}.
#' @param stop_topk_ties Character. How to handle ties at the \code{k}-th boundary for the top-\code{k}
#'   overlap check. One of \code{"id"} (deterministic) or \code{"random"}. Default \code{"id"}.
#' @param stop_min_largest_component_frac Numeric in \code{[0, 1]}. Minimum fraction of nodes that must lie in the
#'   largest connected component for the comparison graph to be considered healthy. The formal argument default is \code{NA}, but when left \code{NA} the runner applies an internal effective default of \code{0.98} (unless overridden). Set explicitly (e.g., \code{0}) to disable this gating.
#' @param stop_min_degree Integer. Minimum node degree required for the comparison graph to be considered
#'   healthy. The formal argument default is \code{NA}, but when left \code{NA} the runner applies an internal effective default of \code{1} (unless overridden). Set explicitly (e.g., \code{0L}) to disable this gating.
#' @param stop_reason_priority Optional character vector specifying a priority order for stop reasons when
#'   multiple stopping criteria are met on the same round. If \code{NULL}, a default priority is used.
#' @param stop_max_bridge_edge_frac Numeric in \code{[0, 1]}. Additional mixing guard for stopping: when the
#'   comparison graph is otherwise healthy, stopping is only allowed once the bridge-edge fraction proxy
#'   is at most this value for \code{stop_k_mix} consecutive checks. Set to \code{NA} to disable. Default \code{0.02}.
#' @param stop_k_mix Integer. Number of consecutive mixing checks that must pass before allowing stopping.
#'   Default \code{2}.
#' @param stop_check_mix_every Integer. Compute the bridge-edge fraction proxy every
#'   \code{stop_check_mix_every} rounds (to reduce runtime). Default \code{2}.
#' @param spectral_gap_check Character. Optional end-of-run (and optionally pre-stop/pre-switch) spectral
#'   gap check on a lazy random walk over the comparison graph. Values:
#'   \itemize{
#'     \item \code{"never"}: do not compute.
#'     \item \code{"final"}: compute once at the end.
#'     \item \code{"pre_stop"}: compute immediately before stopping (when a stop would occur) and at the end.
#'     \item \code{"pre_switch_and_final"}: compute immediately before switching stage1\eqn{\to}stage2 and at the end.
#'   }
#'   This check is non-blocking by default; it is recorded in \code{out$spectral_gap_checks}.
#' @param spectral_gap_weights Character. Edge weighting scheme for the spectral-gap estimator:
#'   \code{"count"} (use edge multiplicity) or \code{"binary"} (unweighted). Default \code{"count"}.
#' @param spectral_gap_max_iter Integer. Maximum iterations for the spectral-gap estimator. Default \code{200}.
#' @param spectral_gap_tol Numeric. Convergence tolerance for the spectral-gap estimator. Default \code{1e-6}.
#' @param spectral_gap_warn_below Numeric in \code{[0, 1]}. Threshold below which \code{spectral_gap_warn = TRUE}
#'   is recorded in \code{out$spectral_gap_checks}. Default \code{0.01}.
#'
#'
#' @param se_probs Numeric vector of probabilities in \code{[0, 1]} used when summarizing the
#' distribution of standard errors for stopping diagnostics (e.g., median, 90th percentile).
#' Passed to \code{\link{bt_adaptive_round}}.
#' @param fit_bounds Numeric length-2 vector giving lower/upper acceptable
#'   infit/outfit bounds when available. Passed to \code{\link{bt_adaptive_round}}.
#' @param stopping_tier Preset stopping thresholds to use
#'   (good/strong/very_strong) when available. Passed to
#'   \code{\link{bt_adaptive_round}}.
#'
#' @param reliability_target Numeric. Target reliability/separation-based criterion used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param sepG_target Numeric. Target separation index (or analogous) used by
#' \code{\link{bt_adaptive_round}} for stopping decisions.
#' @param rel_se_p90_target Numeric. Target value for the 90th percentile of item SE (or a
#' comparable uncertainty summary) used for stopping. Passed to \code{\link{bt_adaptive_round}}.
#' @param rel_se_p90_min_improve Numeric. Minimum required improvement in the uncertainty summary
#' relative to the previous round; if improvement falls below this, stopping may be allowed
#' (depending on other criteria). Passed to \code{\link{bt_adaptive_round}}.
#' @param max_item_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of item misfit flags
#' (based on \code{fit_bounds}/diagnostics) before stopping is disallowed. Passed to
#' \code{\link{bt_adaptive_round}}.
#' @param max_judge_misfit_prop Numeric between 0 and 1 (inclusive). Maximum
#' allowed proportion of judge misfit flags before stopping is disallowed
#' (when judge diagnostics are available). Passed to \code{\link{bt_adaptive_round}}.
#'
#' @param k_neighbors Integer. When ability estimates are available, restrict candidate pair
#' selection to approximately local neighborhoods in \code{theta} (e.g., near neighbors) to focus
#' comparisons where they are most informative. If \code{theta} is not available (early rounds),
#' selection falls back to non-theta heuristics. Passed to \code{\link{bt_adaptive_round}} /
#' \code{\link{select_adaptive_pairs}}.
#' @param min_judgments Integer. Minimum number of total judgments per item to prioritize before
#' focusing on adaptive informativeness/uncertainty. Passed to \code{\link{bt_adaptive_round}}.
#' @param repeat_policy Character repeat planning policy. Options:
#'   \itemize{
#'     \item \code{"none"}: do not plan repeat checks.
#'     \item \code{"reverse_only"}: plan a subset of opposite-direction repeats for
#'       eligible unordered pairs (A,B).
#'     \item \code{"forbid_unordered"}: convenience alias that behaves like the
#'       legacy \code{forbid_repeats = TRUE} (no planned repeats and forbids
#'       selecting unordered repeats from the candidate pool).
#'   }
#' @param repeat_cap Non-negative integer cap on the number of planned repeat
#'   pairs per unordered pair key. For \code{repeat_policy = "reverse_only"}, each
#'   unordered pair is eligible for at most \code{repeat_cap} planned reverse
#'   repeats.
#' @param repeat_frac Numeric in \code{[0, 1]}. Target fraction of the requested
#'   per-round \code{round_size} pairs that should be reserved for repeat checks
#'   (when eligible repeat pairs exist).
#' @param repeat_n Optional non-negative integer. If provided, overrides
#'   \code{repeat_frac} and targets this many planned repeat pairs per round.
#' @param repeat_guard_min_degree Integer. Guard for enabling repeat planning:
#'   do not plan repeats until the minimum graph degree across IDs is at least
#'   this value.
#' @param repeat_guard_largest_component_frac Numeric in \code{[0, 1]}. Guard for
#'   enabling repeat planning: do not plan repeats until the largest connected
#'   component contains at least this fraction of IDs.
#' @param forbid_repeats Logical. If \code{TRUE}, unordered pairs (A,B) are not repeated across
#' rounds. Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#' @param balance_positions Logical. If \code{TRUE}, attempt to balance how often each item appears
#' in the first vs second position (\code{ID1} vs \code{ID2}) to mitigate positional bias.
#' Passed to \code{\link{bt_adaptive_round}} / \code{\link{select_adaptive_pairs}}.
#' @param embeddings Optional numeric embeddings matrix with one row per item.
#' If provided, adaptive candidate generation is augmented with embedding-based
#' neighbor candidates (in addition to theta-neighbors). Use
#' \code{\link{validate_embeddings}} to align/reorder rows.
#' @param embed_k Integer. Number of nearest neighbors (by cosine similarity) to
#' precompute per item from \code{embeddings}. Only used when \code{embeddings}
#' is provided.
#' @param embed_far_k Integer. Number of additional "far" candidates to sample
#' per item (uniformly at random) to preserve global mixing. Only used when
#' \code{embeddings} is provided.
#' @param embed_quota_frac Numeric in \code{[0, 1]}. Minimum fraction of selected pairs
#'   that should come from embedding-neighbor candidates when embeddings are used.
#' @param candidate_pool_cap Non-negative integer or Inf. Global cap on the size
#'   of the candidate pool considered during pair selection.
#' @param per_anchor_cap Non-negative integer or Inf. Per-item cap on the number
#'   of candidates retained (after scoring) for each anchor item.
#' @param w_embed Non-negative numeric. Optional weight on an embedding-source bonus
#'   term used in candidate scoring.
#' @param embed_score_mode Character. How to compute the embedding-source bonus.
#'
#' @param seed_pairs Optional integer seed used for bootstrap pair generation as
#' \code{seed_pairs}, and for adaptive rounds as \code{seed_pairs + round}.
#' The RNG state is restored to its prior value (or returned to "uninitialized" if it was missing).
#' Note: this controls pair selection reproducibility; it does not control randomness inside
#' \code{judge_fun} unless your \code{judge_fun} uses it explicitly.
#'
#' (RNG state is restored afterward).
#'
#' @param fit_fun Function used to fit the BT model. Default \code{\link{fit_bt_model}}.
#' Primarily intended as a test hook; most users should keep the default.
#' @param build_bt_fun Function used to convert results into BT data. Default
#' \code{\link{build_bt_data}}. Primarily intended as a test hook.
#'
#' @param checkpoint_dir Optional directory path for writing checkpoint files during
#'   a run. If provided, the runner writes a checkpoint file named
#'   \code{run_state.rds} (and optionally per-round snapshot files) after completed
#'   rounds. Use this to resume long jobs after interruption or errors.
#'
#' @param resume_from Optional directory path containing a prior checkpoint file
#'   \code{run_state.rds} created by \code{bt_run_adaptive()}. When provided, the
#'   run resumes from the saved state (including accumulated results and round
#'   counters) rather than starting from scratch. The \code{samples} argument must
#'   match the checkpoint's sample IDs (and typically the same runner configuration
#'   should be used).
#'
#' @param checkpoint_every Integer controlling how frequently per-round snapshot
#'   checkpoint files are written. A value of \code{1} writes a snapshot after every
#'   completed round; a value of \code{2} writes snapshots every other round, etc.
#'   The main file \code{run_state.rds} is still updated at safe points even when
#'   \code{checkpoint_every > 1}.
#'
#' @param checkpoint_store_fits Logical indicating whether to store fitted model
#'   objects (e.g., BT model fits and diagnostics) inside checkpoint files. Set to
#'   \code{FALSE} to reduce checkpoint size and speed up checkpoint writes; in that
#'   case, model fits will be recomputed as needed after resuming.
#'
#' @param checkpoint_overwrite Logical indicating whether to overwrite an existing
#'   \code{run_state.rds} file in \code{checkpoint_dir}. If \code{FALSE} and a
#'   checkpoint file already exists, the function should error rather than overwrite.
#'
#' @param ... Additional arguments passed through to \code{fit_fun}.
#'
#' @details
#' \strong{Checkpointing and resuming:} If \code{checkpoint_dir} is provided, this
#' function writes a checkpoint representing the \emph{last completed safe point}
#' of the adaptive loop (i.e., after a round has fully completed). If the run is
#' interrupted or errors occur mid-round, the checkpoint corresponds to the most
#' recently completed round. Resume by calling the function again with
#' \code{resume_from = checkpoint_dir} (and typically the same key settings such as
#' \code{round_size}, repeat controls, and selection strategy).
#'
#' @return A list with elements:
#' \describe{
#' \item{results}{All accumulated forward-direction results (ID1, ID2, better_id, ...).}
#' \item{bt_data}{BT data built from \code{results}.}
#' \item{fits}{List of per-round fit objects (one per adaptive round). Each fit is
#' tagged with per-round metadata in the \code{attr(fit, "bt_run_adaptive")} attribute.}
#' \item{final_fit}{The final fit object (the last element of \code{fits}), or \code{NULL}
#' if no fit was run.}
#' \item{theta}{A compact tibble with columns \code{ID}, \code{theta}, \code{se}, and
#' \code{rank}, representing the final ability scale used for most downstream workflows.
#' When a BT final fit succeeds, this corresponds to the final BT scale; otherwise it
#' falls back to Rank Centrality.}
#' \item{theta_engine}{A single string describing which engine produced \code{theta}
#' (e.g., \code{"bt_firth"}, \code{"bt_mle"}, \code{"bt_bayes"}, \code{"rank_centrality"}).}
#' \item{estimates}{NULL unless \code{final_refit=TRUE}; then a final combined estimates
#' tibble produced by \code{\link{compute_final_estimates}} (BT + Rank Centrality).}
#' \item{final_models}{NULL unless \code{final_refit=TRUE}; then a list containing
#' the fitted model objects (\code{bt_fit}, \code{rc_fit}) and a summary \code{diagnostics} list.}
#' \item{fit_provenance}{NULL unless \code{final_refit=TRUE}; then a list describing which
#' final engines were requested and used, including any fallback reason.}
#' \item{stop_reason}{A single string describing why the loop ended.
#' Standard reasons include: \code{"stability_reached"}, \code{"precision_reached"},
#' \code{"no_new_pairs"}, \code{"pair_budget_exhausted"}, and \code{"max_rounds_reached"}.}
#' \item{stop_round}{Integer round index at which the loop ended (\code{NA} if no rounds were run).}
#' \item{rounds}{A tibble summarizing each adaptive round (metrics + stop flag + stop_reason).}
#' \item{stop_audit}{A one-row-per-round stop audit table making stop gating explicit (precision/stability vs hard stops, graph health, min rounds, mixing guard, budgets).}
#' \item{state}{A tibble with one row per adaptive round containing bookkeeping summaries
#'   of the accumulated results at that round (e.g., \code{n_unique_unordered_pairs},
#'   appearance quantiles, \code{pos_imbalance_max}, \code{n_self_pairs},
#'   \code{n_missing_better_id}), plus \code{round}, \code{stop}, and \code{stop_reason}.}
#' \item{pairs_bootstrap}{Pairs used in the bootstrap scoring step (may be empty).}
#' \item{run_summary}{One row tibble summarizing the run.}
#' \item{metrics}{One row per round of schema-stable diagnostic metrics.}
#' \item{pairing_diagnostics}{Planned and derived pairing diagnostics per round, including
#'   whether selection fell back to controlled-random pairing (via
#'   \code{fallback_path == "controlled_random"}) and its trigger
#'   (\code{fallback_trigger}).}
#' \item{spectral_gap_checks}{Optional spectral-gap check results (computed at the end and/or on demand).}
#' }
#'
#' @examples
#' # Minimal self-contained example that does not require sirt:
#' samples <- tibble::tibble(
#'   ID = c("A", "B", "C", "D"),
#'   text = paste("text", c("A", "B", "C", "D"))
#' )
#'
#' # A tiny "judge" simulator (deterministic by latent ability):
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' judge_fun <- function(pairs) {
#'   simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
#' }
#'
#' # A tiny fit function (test-style), so the example runs without external engines:
#' fit_fun <- function(bt_data, ...) {
#'   bt_data <- as.data.frame(bt_data)
#'   ids <- sort(unique(c(bt_data[[1]], bt_data[[2]])))
#'   wins <- stats::setNames(rep(0L, length(ids)), ids)
#'   n_j <- stats::setNames(rep(0L, length(ids)), ids)
#'   for (i in seq_len(nrow(bt_data))) {
#'     a <- as.character(bt_data[[1]][i])
#'     b <- as.character(bt_data[[2]][i])
#'     r <- as.numeric(bt_data[[3]][i])
#'     if (is.finite(r)) {
#'       if (r == 1) wins[a] <- wins[a] + 1L else wins[b] <- wins[b] + 1L
#'       n_j[a] <- n_j[a] + 1L
#'       n_j[b] <- n_j[b] + 1L
#'     }
#'   }
#'   theta <- as.numeric(wins - stats::median(wins))
#'   se <- 1 / sqrt(pmax(1L, as.integer(n_j)))
#'   list(
#'     engine = "mock",
#'     reliability = 0.95,
#'     theta = tibble::tibble(ID = names(wins), theta = theta, se = se),
#'     diagnostics = list(sepG = 3.5)
#'   )
#' }
#'
#' out <- bt_run_adaptive(
#'   samples = samples,
#'   judge_fun = judge_fun,
#'   fit_fun = fit_fun,
#'   engine = "mock",
#'   fit_engine_running = "bt",
#'   round_size = 1,
#'   init_round_size = 1,
#'   max_rounds = 1,
#'   final_refit = FALSE,
#'   return_diagnostics = FALSE,
#'   rel_se_p90_target = NA_real_,
#'   rel_se_p90_min_improve = NA_real_
#' )
#' out$rounds
#'
#' @import tibble
#' @import dplyr
#' @export
bt_run_adaptive <- function(samples,
                            judge_fun,
                            initial_results = NULL,
                            judge = NULL,
                            engine = "sirt",
                            fit_verbose = FALSE,
                            return_diagnostics = TRUE,
                            include_residuals = FALSE,
                            fit_engine_running = c("rank_centrality", "bt", "hybrid"),
                            rc_smoothing = 0.5,
                            rc_damping = 0.0,
                            stage1_k_conn = 2L,
                            stage1_k_stab = 3L,
                            stage1_min_pct_nodes_with_degree_gt0 = 0.95,
                            stage1_min_largest_component_frac = 0.95,
                            stage1_min_degree_median = 2,
                            stage1_min_degree_min_lcc = 1,
                            stage1_min_degree_min = 0,
                            stage1_min_spearman = 0.97,
                            stage1_stability_metric = c("topk_jaccard", "spearman"),
                            stage1_topk = NULL,
                            stage1_topk_overlap = 0.85,
                            stage1_allow_degenerate_stability = FALSE,
                            stage1_max_rounds = 10L,
                            stage1_max_bridge_edge_frac = 0.02,
                            stage1_k_mix = 2L,
                            stage1_check_mix_every = 2L,
                            stage1_escalated_explore_frac = 0.5,
                            stage1_escalated_k_neighbors = Inf,
                            stage1_escalate_allow_unordered_repeats = FALSE,
                            stage1_explore_frac = 0.25,
                            stage2_explore_frac = 0.10,
                            stage2_min_rounds = 3L,
                            final_refit = TRUE,
                            fit_engine_final = c("bt_firth", "bt_mle", "bt_bayes", "none"),
                            final_bt_bias_reduction = TRUE,
                            round_size = 50,
                            init_round_size = round_size,
                            max_rounds = 50,
                            se_probs = c(0.5, 0.9, 0.95),
                            fit_bounds = c(0.7, 1.3),
                            stopping_tier = c("strong", "good", "very_strong"),
                            reliability_target = 0.90,
                            sepG_target = 3.0,
                            rel_se_p90_target = 0.30,
                            rel_se_p90_min_improve = 0.01,
                            max_item_misfit_prop = 0.05,
                            max_judge_misfit_prop = 0.05,
                            k_neighbors = 10,
                            min_judgments = 12,
                            repeat_policy = "reverse_only",
                            repeat_cap = 1L,
                            repeat_frac = 0.05,
                            repeat_n = NULL,
                            repeat_guard_min_degree = 1L,
                            repeat_guard_largest_component_frac = 0.90,
                            forbid_repeats = NULL,
                            balance_positions = TRUE,
                            embeddings = NULL,
                            embed_k = 30,
                            embed_far_k = 0,
                            seed_pairs = NULL,
                            # PR7: stopping controls
                            min_rounds = 2L,
                            stop_stability_rms = 0.01,
                            stop_topk = 50L,
                            stop_topk_overlap = 0.95,
                            stop_min_largest_component_frac = NA_real_,
                            stop_min_degree = NA_integer_,
                            stop_reason_priority = NULL,
                            stop_stability_consecutive = 2L,
                            stop_max_bridge_edge_frac = 0.02,
                            stop_k_mix = NULL,
                            stop_check_mix_every = 2L,
                            spectral_gap_check = c("never", "final", "pre_stop", "pre_switch_and_final"),
                            spectral_gap_weights = c("count", "binary"),
                            spectral_gap_max_iter = 200L,
                            spectral_gap_tol = 1e-6,
                            spectral_gap_warn_below = 0.01,
                            stop_topk_ties = c("id", "random"),
                            fit_fun = fit_bt_model,
                            build_bt_fun = build_bt_data,
                            checkpoint_dir = NULL,
                            resume_from = NULL,
                            checkpoint_every = 1L,
                            checkpoint_store_fits = TRUE,
                            checkpoint_overwrite = TRUE,
                            embed_quota_frac = 0.25,
                            candidate_pool_cap = Inf,
                            per_anchor_cap = Inf,
                            w_embed = 1,
                            embed_score_mode = "rank_decay",
                            ...) {
  tag_fit <- function(fit,
                      round_index,
                      stage,
                      n_results,
                      n_pairs_this_round,
                      stop_reason) {
    meta <- list(
      round_index = as.integer(round_index),
      stage = as.character(stage),
      n_results = as.integer(n_results),
      n_pairs_this_round = as.integer(n_pairs_this_round),
      stop_reason = as.character(stop_reason)
    )
    attr(fit, "bt_run_adaptive") <- meta
    fit
  }

  # PR4.1/PR8.2.4: theta fallback helper is centralized in R/bt_helpers.R

  samples <- tibble::as_tibble(samples)
  if (!all(c("ID", "text") %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }

  # Capture and sanitize `...` forwarded to fit_fun. This prevents collisions like
  # `verbose` (often intended for runner logging) being passed twice to fit_fun.
  .fit_dots <- list(...)
  if (!is.null(.fit_dots$verbose) && missing(fit_verbose)) {
    fit_verbose <- isTRUE(.fit_dots$verbose)
  }
  .fit_dots <- .clean_fit_dots(.fit_dots)


  ids <- as.character(samples$ID)
  if (length(ids) < 2L) stop("`samples` must contain at least 2 rows.", call. = FALSE)
  if (anyNA(ids) || any(ids == "")) stop("`samples$ID` must be non-missing and non-empty.", call. = FALSE)
  if (any(duplicated(ids))) stop("`samples$ID` must be unique.", call. = FALSE)

  # Optional: embedding-augmented candidate generation.
  embedding_neighbors <- NULL
  if (!is.null(embeddings)) {
    if (!is.numeric(embed_k) || length(embed_k) != 1L || is.na(embed_k) || embed_k < 0) {
      stop("`embed_k` must be a single non-negative integer.", call. = FALSE)
    }
    embed_k <- as.integer(embed_k)
    if (!is.numeric(embed_far_k) || length(embed_far_k) != 1L || is.na(embed_far_k) || embed_far_k < 0) {
      stop("`embed_far_k` must be a single non-negative integer.", call. = FALSE)
    }
    embed_far_k <- as.integer(embed_far_k)
    embedding_neighbors <- .compute_embedding_neighbors(embeddings, ids = ids, k = embed_k)
  }

  if (!is.function(judge_fun)) stop("`judge_fun` must be a function.", call. = FALSE)

  if (!is.null(judge)) {
    if (!is.character(judge) || length(judge) != 1L || is.na(judge) || nchar(judge) == 0L) {
      stop("`judge` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  round_size <- as.integer(round_size)
  if (is.na(round_size) || round_size < 0L) stop("`round_size` must be a non-negative integer.", call. = FALSE)

  init_round_size <- as.integer(init_round_size)
  if (is.na(init_round_size) || init_round_size < 0L) stop("`init_round_size` must be a non-negative integer.", call. = FALSE)

  max_rounds <- as.integer(max_rounds)
  if (is.na(max_rounds) || max_rounds < 0L) stop("`max_rounds` must be a non-negative integer.", call. = FALSE)

  checkpoint_every <- as.integer(checkpoint_every)
  if (is.na(checkpoint_every) || checkpoint_every < 1L) {
    stop("`checkpoint_every` must be an integer >= 1.", call. = FALSE)
  }

  if (!is.null(resume_from) && (is.null(checkpoint_dir) || !nzchar(checkpoint_dir))) {
    checkpoint_dir <- resume_from
  }

  fit_engine_running <- match.arg(fit_engine_running)
  fit_engine_running_requested <- fit_engine_running
  fit_engine_final <- match.arg(fit_engine_final)

  # ---- PR3: hybrid stage-1 (Rank Centrality) -> stage-2 (BT) controls ----
  stage1_k_conn <- as.integer(stage1_k_conn)
  if (is.na(stage1_k_conn) || stage1_k_conn < 1L) {
    stop("`stage1_k_conn` must be an integer >= 1.", call. = FALSE)
  }
  stage1_k_stab <- as.integer(stage1_k_stab)
  if (is.na(stage1_k_stab) || stage1_k_stab < 1L) {
    stop("`stage1_k_stab` must be an integer >= 1.", call. = FALSE)
  }
  stage1_max_rounds <- as.integer(stage1_max_rounds)
  if (is.na(stage1_max_rounds) || stage1_max_rounds < 1L) {
    stop("`stage1_max_rounds` must be an integer >= 1.", call. = FALSE)
  }

  stage1_max_bridge_edge_frac_was_missing <- missing(stage1_max_bridge_edge_frac)
  stage1_max_bridge_edge_frac <- as.double(stage1_max_bridge_edge_frac)
  if (length(stage1_max_bridge_edge_frac) != 1L || isTRUE(is.nan(stage1_max_bridge_edge_frac)) ||
    (!is.na(stage1_max_bridge_edge_frac) && stage1_max_bridge_edge_frac < 0)) {
    stop("`stage1_max_bridge_edge_frac` must be NA or a single non-negative number.", call. = FALSE)
  }

  # Mixing-guard defaults are tuned for N ~= 200--2000. For very small toy runs
  # (common in tests/examples), the bridge-edge fraction tends to be large even
  # for "healthy" graphs. To avoid blocking hybrid switching by default on small
  # graphs, treat the default threshold as disabled when N < 200.
  stage1_max_bridge_edge_frac_eff <- stage1_max_bridge_edge_frac
  if (length(ids) < 200L && isTRUE(stage1_max_bridge_edge_frac_was_missing) && is.finite(stage1_max_bridge_edge_frac_eff) &&
    isTRUE(abs(stage1_max_bridge_edge_frac_eff - 0.02) < 1e-12)) {
    stage1_max_bridge_edge_frac_eff <- NA_real_
  }
  stage1_k_mix <- as.integer(stage1_k_mix)
  if (is.na(stage1_k_mix) || stage1_k_mix < 0L) {
    stop("`stage1_k_mix` must be an integer >= 0.", call. = FALSE)
  }
  stage1_check_mix_every <- as.integer(stage1_check_mix_every)
  if (is.na(stage1_check_mix_every) || stage1_check_mix_every < 1L) {
    stop("`stage1_check_mix_every` must be an integer >= 1.", call. = FALSE)
  }

  # In very short runs, cap the stage-1 horizon so escalation/switch logic can still activate.
  stage1_max_rounds_eff <- min(stage1_max_rounds, max(1L, max_rounds - 1L))

  if (!is.numeric(stage1_escalated_explore_frac) || length(stage1_escalated_explore_frac) != 1L ||
    is.na(stage1_escalated_explore_frac) || stage1_escalated_explore_frac < 0 ||
    stage1_escalated_explore_frac > 1) {
    stop("`stage1_escalated_explore_frac` must be a single number in [0, 1].", call. = FALSE)
  }

  # Allow NULL / Inf as a convenience for "all neighbors".
  if (is.null(stage1_escalated_k_neighbors) || isTRUE(is.infinite(stage1_escalated_k_neighbors))) {
    stage1_escalated_k_neighbors <- Inf
  }
  if (!is.numeric(stage1_escalated_k_neighbors) || length(stage1_escalated_k_neighbors) != 1L ||
    is.na(stage1_escalated_k_neighbors)) {
    stop("`stage1_escalated_k_neighbors` must be a positive integer, or NULL/Inf for all neighbors.", call. = FALSE)
  }
  if (is.finite(stage1_escalated_k_neighbors)) {
    if (stage1_escalated_k_neighbors < 1) {
      stop("`stage1_escalated_k_neighbors` must be positive (>= 1), or NULL/Inf for all neighbors.", call. = FALSE)
    }
    if (abs(stage1_escalated_k_neighbors - round(stage1_escalated_k_neighbors)) > 1e-12) {
      stop("`stage1_escalated_k_neighbors` must be an integer (or NULL/Inf for all neighbors).", call. = FALSE)
    }
    stage1_escalated_k_neighbors <- as.integer(stage1_escalated_k_neighbors)
  }

  stage1_escalate_allow_unordered_repeats <- isTRUE(stage1_escalate_allow_unordered_repeats)

  stage2_min_rounds <- as.integer(stage2_min_rounds)
  if (is.na(stage2_min_rounds) || stage2_min_rounds < 0L) {
    stop("`stage2_min_rounds` must be a non-negative integer.", call. = FALSE)
  }
  if (!is.numeric(stage1_min_spearman) || length(stage1_min_spearman) != 1L || is.na(stage1_min_spearman)) {
    stop("`stage1_min_spearman` must be a single numeric value.", call. = FALSE)
  }
  stage1_stability_metric <- match.arg(stage1_stability_metric, c("topk_jaccard", "spearman"))
  stage1_allow_degenerate_stability <- isTRUE(stage1_allow_degenerate_stability)

  if (!is.null(stage1_topk)) {
    if (!is.numeric(stage1_topk) || length(stage1_topk) != 1L || is.na(stage1_topk) || stage1_topk < 1) {
      stop("`stage1_topk` must be NULL or a single positive integer.", call. = FALSE)
    }
  }
  if (!is.numeric(stage1_topk_overlap) || length(stage1_topk_overlap) != 1L ||
    is.na(stage1_topk_overlap) || stage1_topk_overlap < 0 || stage1_topk_overlap > 1) {
    stop("`stage1_topk_overlap` must be a single number in [0, 1].", call. = FALSE)
  }

  stage1_topk_eff <- if (is.null(stage1_topk)) {
    max(10L, as.integer(ceiling(0.1 * length(ids))))
  } else {
    as.integer(stage1_topk)
  }
  stage1_topk_eff <- max(1L, min(as.integer(stage1_topk_eff), as.integer(length(ids))))
  if (!is.numeric(stage1_min_pct_nodes_with_degree_gt0) || length(stage1_min_pct_nodes_with_degree_gt0) != 1L ||
    is.na(stage1_min_pct_nodes_with_degree_gt0) || stage1_min_pct_nodes_with_degree_gt0 < 0 ||
    stage1_min_pct_nodes_with_degree_gt0 > 1) {
    stop("`stage1_min_pct_nodes_with_degree_gt0` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(stage1_min_largest_component_frac) || length(stage1_min_largest_component_frac) != 1L ||
    is.na(stage1_min_largest_component_frac) || stage1_min_largest_component_frac < 0 ||
    stage1_min_largest_component_frac > 1) {
    stop("`stage1_min_largest_component_frac` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.numeric(stage1_min_degree_median) || length(stage1_min_degree_median) != 1L || is.na(stage1_min_degree_median)) {
    stop("`stage1_min_degree_median` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(stage1_min_degree_min_lcc) || length(stage1_min_degree_min_lcc) != 1L || is.na(stage1_min_degree_min_lcc)) {
    stop("`stage1_min_degree_min_lcc` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(stage1_min_degree_min) || length(stage1_min_degree_min) != 1L || is.na(stage1_min_degree_min)) {
    stop("`stage1_min_degree_min` must be a single numeric value.", call. = FALSE)
  }

  if (isTRUE(stage1_min_degree_min > 0)) {
    warning(
      "`stage1_min_degree_min` is deprecated and can block hybrid stage switching when some nodes are unseen. ",
      "Prefer `stage1_min_degree_min_lcc` for enforcing minimum degree within the LCC.",
      call. = FALSE
    )
  }

  # --- repeat policy (PR6) ---
  if (!is.null(forbid_repeats)) {
    warning("`forbid_repeats` is deprecated; use `repeat_policy`.", call. = FALSE)
    if (isTRUE(forbid_repeats)) {
      repeat_policy <- "none"
    } else {
      repeat_policy <- "reverse_only"
    }
  }
  repeat_policy <- match.arg(repeat_policy, c("allow", "none", "reverse_only", "forbid_unordered"))
  if (identical(repeat_policy, "allow")) repeat_policy <- "none"
  if (!is.numeric(rc_smoothing) || length(rc_smoothing) != 1L || is.na(rc_smoothing) || rc_smoothing < 0) {
    stop("`rc_smoothing` must be a single non-negative number.", call. = FALSE)
  }
  if (!is.numeric(rc_damping) || length(rc_damping) != 1L || is.na(rc_damping) || rc_damping < 0 || rc_damping >= 1) {
    stop("`rc_damping` must be a single number in [0, 1).", call. = FALSE)
  }
  final_refit <- isTRUE(final_refit)
  final_bt_bias_reduction <- isTRUE(final_bt_bias_reduction)


  # --- stop mixing guard (bridge-edge fraction) ---
  stop_max_bridge_edge_frac_was_missing <- missing(stop_max_bridge_edge_frac)
  stop_max_bridge_edge_frac <- as.double(stop_max_bridge_edge_frac)
  if (!is.na(stop_max_bridge_edge_frac) && stop_max_bridge_edge_frac < 0) {
    stop("`stop_max_bridge_edge_frac` must be NA or a non-negative number.", call. = FALSE)
  }

  # Mixing-guard defaults are tuned for N ~= 200--2000. Disable the default
  # bridge threshold on very small toy runs so stopping isn't blocked by
  # bridge-heavy graphs that are otherwise fine for small-N.
  stop_max_bridge_edge_frac_eff <- stop_max_bridge_edge_frac
  if (length(ids) < 200L && isTRUE(stop_max_bridge_edge_frac_was_missing) && is.finite(stop_max_bridge_edge_frac_eff) &&
    isTRUE(abs(stop_max_bridge_edge_frac_eff - 0.02) < 1e-12)) {
    stop_max_bridge_edge_frac_eff <- NA_real_
  }

  if (is.null(stop_k_mix)) {
    stop_k_mix_eff <- as.integer(stop_stability_consecutive)
  } else {
    stop_k_mix_eff <- as.integer(stop_k_mix)
    if (is.na(stop_k_mix_eff) || stop_k_mix_eff < 0L) {
      stop("`stop_k_mix` must be NULL or a non-negative integer.", call. = FALSE)
    }
  }
  stop_check_mix_every <- as.integer(stop_check_mix_every)
  if (is.na(stop_check_mix_every) || stop_check_mix_every < 1L) {
    stop("`stop_check_mix_every` must be an integer >= 1.", call. = FALSE)
  }

  # Track whether the stop-stage mixing guard is configured (used for stop metadata).
  stop_mix_guard_active <- !is.na(stop_max_bridge_edge_frac_eff) &&
    is.finite(stop_max_bridge_edge_frac_eff) &&
    isTRUE(stop_k_mix_eff > 0L)


  # --- stop graph-health gating (effective defaults) ---
  # For backwards compatibility, the formal argument defaults remain NA. When left as NA,
  # apply internal effective defaults so that stability/precision stops are not allowed
  # on partially observed or disconnected graphs.
  stop_min_largest_component_frac <- as.double(stop_min_largest_component_frac)
  if (length(stop_min_largest_component_frac) != 1L || isTRUE(is.nan(stop_min_largest_component_frac)) ||
    (!is.na(stop_min_largest_component_frac) && (stop_min_largest_component_frac < 0 || stop_min_largest_component_frac > 1))) {
    stop("`stop_min_largest_component_frac` must be NA or a single number in [0, 1].", call. = FALSE)
  }

  stop_min_degree <- if (is.na(stop_min_degree)) NA_integer_ else as.integer(stop_min_degree)
  if (!is.na(stop_min_degree) && stop_min_degree < 0L) {
    stop("`stop_min_degree` must be NA or a non-negative integer.", call. = FALSE)
  }

  stop_min_largest_component_frac_eff <- if (!is.na(stop_min_largest_component_frac)) {
    stop_min_largest_component_frac
  } else {
    0.98
  }
  stop_min_degree_eff <- if (!is.na(stop_min_degree)) {
    as.integer(stop_min_degree)
  } else {
    1L
  }
  stop_gating_active <- isTRUE(is.finite(as.double(stop_min_degree_eff)) ||
    is.finite(as.double(stop_min_largest_component_frac_eff)))

  # --- spectral gap check (optional; end-of-run by default) ---
  spectral_gap_check <- match.arg(spectral_gap_check, c("never", "final", "pre_stop", "pre_switch_and_final"))
  spectral_gap_weights <- match.arg(spectral_gap_weights, c("count", "binary"))
  spectral_gap_max_iter <- as.integer(spectral_gap_max_iter)
  if (is.na(spectral_gap_max_iter) || spectral_gap_max_iter < 1L) {
    stop("`spectral_gap_max_iter` must be an integer >= 1.", call. = FALSE)
  }
  spectral_gap_tol <- as.double(spectral_gap_tol)
  if (is.na(spectral_gap_tol) || spectral_gap_tol <= 0) {
    stop("`spectral_gap_tol` must be a single number > 0.", call. = FALSE)
  }
  spectral_gap_warn_below <- as.double(spectral_gap_warn_below)
  if (is.na(spectral_gap_warn_below) || spectral_gap_warn_below < 0) {
    stop("`spectral_gap_warn_below` must be a single non-negative number.", call. = FALSE)
  }

  make_running_fit <- function(bt_data, fit_bt, engine_running) {
    bt_tbl <- .as_theta_tibble(fit_bt$theta, arg_name = "fit_fun()$theta")
    bt_tbl <- tibble::as_tibble(bt_tbl)
    bt_tbl$ID <- as.character(bt_tbl$ID)

    # Ensure every ID in `samples` is present (for consistent downstream joins)
    bt_tbl <- dplyr::right_join(bt_tbl, tibble::tibble(ID = as.character(ids)), by = "ID")

    theta_bt <- bt_tbl$theta
    se_bt <- bt_tbl$se

    theta_rc <- rep(NA_real_, length(ids))
    pi_rc <- rep(NA_real_, length(ids))
    rc_fit <- NULL

    if (identical(engine_running, "rank_centrality")) {
      rc_fit <- fit_rank_centrality(
        bt_data,
        ids = ids,
        smoothing = rc_smoothing,
        damping = rc_damping
      )
      rc_tbl <- tibble::as_tibble(rc_fit$theta)
      rc_tbl$ID <- as.character(rc_tbl$ID)
      rc_tbl <- dplyr::right_join(rc_tbl, tibble::tibble(ID = as.character(ids)), by = "ID")
      theta_rc <- rc_tbl$theta
      pi_rc <- rc_tbl$pi
    }

    theta_running <- if (identical(engine_running, "rank_centrality")) theta_rc else theta_bt

    theta_out <- tibble::tibble(
      ID = as.character(ids),
      theta = theta_running,
      se = se_bt,
      theta_bt = theta_bt,
      se_bt = se_bt,
      theta_rc = theta_rc,
      pi_rc = pi_rc
    )

    list(
      engine = fit_bt$engine,
      engine_running = as.character(engine_running),
      reliability = fit_bt$reliability,
      theta = theta_out,
      diagnostics = fit_bt$diagnostics,
      bt_fit = fit_bt,
      rc_fit = rc_fit
    )
  }

  .add_pair_key_direction <- function(df) {
    df <- tibble::as_tibble(df)
    if (!all(c("ID1", "ID2") %in% names(df))) {
      return(df)
    }
    if (!"pair_key" %in% names(df)) {
      df <- dplyr::mutate(df, pair_key = .unordered_pair_key(.data$ID1, .data$ID2))
    }
    if (!"direction" %in% names(df)) {
      df <- dplyr::mutate(df, direction = .pair_direction(.data$ID1, .data$ID2))
    }
    df
  }

  # helper: random bootstrap pairs (unique unordered, optional position balancing)
  .bootstrap_pairs <- function(n_pairs, existing_pairs, seed = NULL) {
    n_pairs <- as.integer(n_pairs)
    if (is.na(n_pairs) || n_pairs < 0L) {
      stop("`n_pairs` must be a non-negative integer.", call. = FALSE)
    }

    if (n_pairs == 0L) {
      return(tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character()))
    }

    existing_pairs <- tibble::as_tibble(existing_pairs)

    # Position counts from existing results
    pos1_counts <- integer(length(ids))
    pos2_counts <- integer(length(ids))

    if (nrow(existing_pairs) > 0L) {
      tab1 <- table(factor(existing_pairs$ID1, levels = ids))
      tab2 <- table(factor(existing_pairs$ID2, levels = ids))
      pos1_counts <- as.integer(tab1)
      pos2_counts <- as.integer(tab2)
    }

    imbalance <- pos1_counts - pos2_counts

    key_fun <- function(a, b) {
      .unordered_pair_key(a, b)
    }

    existing_key <- character()
    if (nrow(existing_pairs) > 0L) {
      existing_key <- unique(key_fun(existing_pairs$ID1, existing_pairs$ID2))
    }

    pairs_id <- .with_seed_restore(
      seed,
      f = function() {
        out_ID1 <- character()
        out_ID2 <- character()
        out_key <- character()

        max_tries <- max(200L, n_pairs * 50L)
        tries <- 0L

        while (length(out_ID1) < n_pairs && tries < max_tries) {
          tries <- tries + 1L
          pair <- sample(ids, size = 2L, replace = FALSE)
          a <- pair[[1]]
          b <- pair[[2]]
          k <- key_fun(a, b)

          if (isTRUE(forbid_repeats) && (k %in% c(existing_key, out_key))) {
            next
          }

          if (isTRUE(balance_positions)) {
            ia <- match(a, ids)
            ib <- match(b, ids)

            if (imbalance[ia] > imbalance[ib]) {
              id1 <- b
              id2 <- a
            } else if (imbalance[ib] > imbalance[ia]) {
              id1 <- a
              id2 <- b
            } else {
              if (stats::runif(1) < 0.5) {
                id1 <- a
                id2 <- b
              } else {
                id1 <- b
                id2 <- a
              }
            }

            imbalance[match(id1, ids)] <- imbalance[match(id1, ids)] + 1L
            imbalance[match(id2, ids)] <- imbalance[match(id2, ids)] - 1L
          } else {
            id1 <- a
            id2 <- b
          }

          out_ID1 <- c(out_ID1, id1)
          out_ID2 <- c(out_ID2, id2)
          out_key <- c(out_key, k)
        }

        tibble::tibble(ID1 = out_ID1, ID2 = out_ID2)
      },
      arg_name = "seed_pairs"
    )

    .add_pair_texts(pairs_id, samples = samples)
  }

  # ---- resume / checkpoint state ----
  checkpoint_payload_last <- NULL
  rounds_tbl_prev <- tibble::tibble()
  state_tbl_prev <- tibble::tibble()
  start_round <- 1L

  if (!is.null(resume_from)) {
    chk <- .bt_read_checkpoint(resume_from)
    .bt_validate_checkpoint(chk, run_type = "adaptive", ids = ids)

    if (!is.null(chk$random_seed)) {
      # best-effort restore RNG state for deterministic continuation
      try(assign(".Random.seed", chk$random_seed, envir = .GlobalEnv), silent = TRUE)
    }

    results <- tibble::as_tibble(chk$results)
    pairs_bootstrap <- tibble::as_tibble(chk$pairs_bootstrap %||% tibble::tibble(
      ID1 = character(), text1 = character(), ID2 = character(), text2 = character()
    ))

    fits <- chk$fits %||% list()
    final_fit <- chk$final_fit %||% NULL
    prev_metrics <- chk$prev_metrics %||% NULL
    prev_fit_for_stability <- if (length(fits) > 0L) fits[[length(fits)]] else NULL
    stability_streak <- 0L

    stop_reason <- chk$stop_reason %||% NA_character_
    stop_round <- chk$stop_round %||% NA_integer_

    rounds_tbl_prev <- tibble::as_tibble(chk$rounds %||% tibble::tibble())
    state_tbl_prev <- tibble::as_tibble(chk$state %||% tibble::tibble())

    start_round <- as.integer(chk$next_round %||% (nrow(rounds_tbl_prev) + 1L))
    if (is.na(start_round) || start_round < 1L) start_round <- nrow(rounds_tbl_prev) + 1L

    # if the checkpoint indicates completion, return it as-is
    if (isTRUE(chk$completed)) {
      out <- chk$out %||% list(
        results = results,
        bt_data = if (nrow(results) == 0L) tibble::tibble(object1 = character(), object2 = character(), result = numeric()) else build_bt_fun(results, judge = judge),
        fits = fits,
        final_fit = final_fit,
        estimates = NULL,
        final_models = NULL,
        stop_reason = stop_reason,
        stop_round = stop_round,
        rounds = rounds_tbl_prev,
        state = state_tbl_prev,
        pairs_bootstrap = pairs_bootstrap
      )
      return(.as_pairwise_run(out, run_type = "adaptive"))
    }
  }

  # normalize initial results (NULL or 0-row => empty start)
  if (is.null(resume_from)) {
    if (is.null(initial_results) || (is.data.frame(initial_results) && nrow(initial_results) == 0L)) {
      results <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
      if (!is.null(judge)) results[[judge]] <- character()
    } else {
      results <- .validate_judge_results(initial_results, ids = ids, judge_col = judge)
      results <- tibble::as_tibble(results)
      results <- .add_pair_key_direction(results)
    }
  }

  # bootstrap scoring step (if no initial results)
  if (is.null(resume_from)) {
    pairs_bootstrap <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
  }

  if (is.null(resume_from) && nrow(results) == 0L && init_round_size > 0L) {
    pairs_bootstrap <- .bootstrap_pairs(
      n_pairs = init_round_size,
      existing_pairs = results,
      seed = seed_pairs
    )

    if (nrow(pairs_bootstrap) > 0L) {
      res0 <- .coerce_judge_output(judge_fun(pairs_bootstrap))
      res0 <- .validate_judge_results(res0, ids = ids, judge_col = judge)
      res0 <- .add_pair_key_direction(res0)
      results <- dplyr::bind_rows(results, res0)
      results <- .add_pair_key_direction(results)
    }
  }

  if (is.null(resume_from)) {
    fits <- list()
    final_fit <- NULL
    prev_metrics <- NULL
    prev_fit_for_stability <- NULL
    stability_streak <- 0L
    stop_reason <- NA_character_
    stop_round <- NA_integer_
  }

  rounds_list <- list()
  state_list <- list()
  pairing_diag_list <- list()
  stop_audit_list <- list()
  metrics_hist <- tibble::tibble()
  spectral_gap_checks <- .spectral_gap_checks_template()

  .make_checkpoint_payload <- function(next_round, completed = FALSE, out = NULL) {
    rounds_now <- dplyr::bind_rows(rounds_tbl_prev, if (length(rounds_list) == 0L) tibble::tibble() else dplyr::bind_rows(rounds_list))
    state_now <- dplyr::bind_rows(state_tbl_prev, if (length(state_list) == 0L) tibble::tibble() else dplyr::bind_rows(state_list))
    seed_now <- NULL
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      seed_now <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    list(
      run_type = "adaptive",
      ids = ids,
      created_at = as.character(Sys.time()),
      results = results,
      pairs_bootstrap = pairs_bootstrap,
      fits = if (isTRUE(checkpoint_store_fits)) fits else NULL,
      final_fit = if (isTRUE(checkpoint_store_fits)) final_fit else NULL,
      prev_metrics = prev_metrics,
      metrics = metrics_hist,
      stop_reason = stop_reason,
      stop_round = stop_round,
      rounds = rounds_now,
      state = state_now,
      next_round = as.integer(next_round),
      random_seed = seed_now,
      completed = isTRUE(completed),
      out = out
    )
  }

  .write_checkpoint_now <- function(payload, round_index = NULL) {
    if (is.null(checkpoint_dir) || !nzchar(checkpoint_dir)) {
      return(invisible(NULL))
    }
    .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", overwrite = checkpoint_overwrite)
    if (!is.null(round_index)) {
      .bt_write_checkpoint(checkpoint_dir, payload, basename = "run_state", round = round_index, overwrite = checkpoint_overwrite)
    }
    invisible(NULL)
  }

  # on exit (including errors/interrupts), write the last completed checkpoint
  if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
    on.exit(
      {
        if (!is.null(checkpoint_payload_last)) {
          .bt_write_checkpoint(checkpoint_dir, checkpoint_payload_last, basename = "run_state", overwrite = checkpoint_overwrite)
        }
      },
      add = TRUE
    )
  }

  # ---- PR3: hybrid stage tracking (persists via rounds table when resuming) ----
  stage <- as.character(fit_engine_running_requested)
  conn_streak <- 0L
  stab_streak <- 0L
  mix_streak <- 0L
  mix_checked_this_round <- NA
  stage1_rounds <- 0L
  stage2_rounds <- 0L
  prev_rc_rank <- NULL
  prev_rc_top_ids <- NULL
  stage1_escalated <- FALSE
  stage1_escalation_round <- NA_integer_

  if (identical(fit_engine_running_requested, "hybrid")) {
    stage <- "stage1_rc"
    if (!is.null(rounds_tbl_prev) && nrow(rounds_tbl_prev) > 0L && "stage" %in% names(rounds_tbl_prev)) {
      last_stage <- rounds_tbl_prev$stage[[nrow(rounds_tbl_prev)]]
      if (is.character(last_stage) && length(last_stage) == 1L && nzchar(last_stage)) {
        stage <- last_stage
      }
    }
    if (!is.null(rounds_tbl_prev) && nrow(rounds_tbl_prev) > 0L) {
      if ("conn_streak" %in% names(rounds_tbl_prev)) {
        conn_streak <- as.integer(rounds_tbl_prev$conn_streak[[nrow(rounds_tbl_prev)]])
        if (is.na(conn_streak)) conn_streak <- 0L
      }
      if ("stab_streak" %in% names(rounds_tbl_prev)) {
        stab_streak <- as.integer(rounds_tbl_prev$stab_streak[[nrow(rounds_tbl_prev)]])
        if (is.na(stab_streak)) stab_streak <- 0L
      }
      if ("mix_streak" %in% names(rounds_tbl_prev)) {
        mix_streak <- as.integer(rounds_tbl_prev$mix_streak[[nrow(rounds_tbl_prev)]])
        if (is.na(mix_streak)) mix_streak <- 0L
      }
      if ("stage1_rounds" %in% names(rounds_tbl_prev)) {
        stage1_rounds <- as.integer(rounds_tbl_prev$stage1_rounds[[nrow(rounds_tbl_prev)]])
        if (is.na(stage1_rounds)) stage1_rounds <- 0L
      }
      if ("stage2_rounds" %in% names(rounds_tbl_prev)) {
        stage2_rounds <- as.integer(rounds_tbl_prev$stage2_rounds[[nrow(rounds_tbl_prev)]])
        if (is.na(stage2_rounds)) stage2_rounds <- 0L
      }

      if ("stage1_escalated" %in% names(rounds_tbl_prev)) {
        stage1_escalated <- isTRUE(rounds_tbl_prev$stage1_escalated[[nrow(rounds_tbl_prev)]])
      }
      if ("stage1_escalation_round" %in% names(rounds_tbl_prev)) {
        stage1_escalation_round <- as.integer(rounds_tbl_prev$stage1_escalation_round[[nrow(rounds_tbl_prev)]])
        if (is.na(stage1_escalation_round)) stage1_escalation_round <- NA_integer_
      }
    }
  }

  # Theta used for pairing heuristics (may differ from final refit theta).
  # Initialize so examples/tests that start from empty state don't error.
  theta_for_pairs <- NULL

  round_seq <- if (start_round <= max_rounds) seq.int(from = start_round, to = max_rounds) else integer(0)
  for (r in round_seq) {
    stage_switched_this_round <- FALSE

    if (nrow(results) == 0L) break

    # Mixing checks are computed on a schedule; record whether we actually
    # evaluated the mixing proxy on this round for transparency.
    mix_checked_this_round <- NA

    engine_running_now <- as.character(fit_engine_running_requested)
    if (identical(fit_engine_running_requested, "hybrid")) {
      engine_running_now <- if (identical(stage, "stage1_rc")) "rank_centrality" else "bt"
    }

    bt_data <- if (is.null(judge)) {
      build_bt_fun(results, judge = NULL)
    } else {
      build_bt_fun(results, judge = judge)
    }

    fit_args <- c(
      list(bt_data),
      list(
        engine = engine,
        verbose = fit_verbose,
        return_diagnostics = return_diagnostics,
        include_residuals = include_residuals
      ),
      .fit_dots
    )
    fit_bt <- .call_user_fun(fit_fun, fit_args)

    fit <- make_running_fit(bt_data, fit_bt, engine_running = engine_running_now)
    fit <- tag_fit(
      fit,
      round_index = r,
      stage = "round_fit",
      n_results = nrow(results),
      n_pairs_this_round = NA_integer_,
      stop_reason = NA_character_
    )
    fits[[length(fits) + 1L]] <- fit
    final_fit <- fit


    # ---- PR7: compute stop metrics (precision + stability) ----
    metrics <- bt_stop_metrics(
      fit,
      se_probs = se_probs,
      fit_bounds = fit_bounds,
      prev_fit = prev_fit_for_stability,
      stability_topk = stop_topk,
      stability_topk_ties = stop_topk_ties,
      stability_seed = if (is.null(seed_pairs)) NULL else (as.integer(seed_pairs) + as.integer(r))
    )

    # Ensure a consistent superset schema before stopping logic.
    metrics <- .bt_align_metrics(metrics, se_probs = se_probs)

    # Keep a per-round history of bt_stop_metrics output (schema-aligned).

    stopping_tier <- match.arg(stopping_tier)
    tier_params <- bt_stop_tiers()[[stopping_tier]]
    tier_params <- utils::modifyList(
      tier_params,
      list(
        reliability_target = reliability_target,
        sepG_target = sepG_target,
        rel_se_p90_target = rel_se_p90_target,
        rel_se_p90_min_improve = rel_se_p90_min_improve,
        max_item_misfit_prop = max_item_misfit_prop,
        max_judge_misfit_prop = max_judge_misfit_prop
      )
    )

    precision_decision <- do.call(
      bt_should_stop,
      c(list(metrics = metrics, prev_metrics = prev_metrics), tier_params)
    )
    precision_reached <- isTRUE(precision_decision$stop)

    # ---- PR7: graph health gating ----
    gs <- .graph_state_from_pairs(results, ids = ids)
    gm <- gs$metrics
    degree_min <- as.double(gm$degree_min)
    degree_min_lcc <- as.double(gm$degree_min_lcc)
    degree_median <- as.double(gm$degree_median)
    largest_component_frac <- as.double(gm$largest_component_frac)
    pct_nodes_with_degree_gt0 <- as.double(gm$pct_nodes_with_degree_gt0)
    bridge_edge_count <- if ("bridge_edge_count" %in% names(gm)) as.integer(gm$bridge_edge_count) else NA_integer_
    bridge_edge_frac <- if ("bridge_edge_frac" %in% names(gm)) as.double(gm$bridge_edge_frac) else NA_real_
    n_components <- as.integer(gm$n_components)

    mix_ok <- NA

    # Spectral gap checks (lazy random walk)
    sg_cached <- NULL
    spectral_gap_est_val <- NA_real_
    lambda2_est_val <- NA_real_
    spectral_gap_iters_val <- NA_integer_
    spectral_gap_converged_val <- NA
    spectral_gap_warn_val <- NA
    spectral_gap_when_val <- NA_character_


    # ---- PR3: hybrid stage switching diagnostics (Rank Centrality stability + connectivity) ----
    rho_spearman_rc <- NA_real_
    topk_overlap_rc <- NA_real_
    conn_ok <- NA
    stab_ok <- NA
    stage_after <- stage

    if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc")) {
      stage1_rounds <- as.integer(stage1_rounds) + 1L

      # ---- Stage 1 stability (RC) ----
      rc_vals <- fit$theta$theta_rc
      if (!is.null(rc_vals) && any(is.finite(rc_vals))) {
        rc_rank <- base::rank(rc_vals, ties.method = "average", na.last = "keep")

        # Deterministic ordering for top-k overlap (break ties by ID)
        rc_tbl <- tibble::tibble(ID = as.character(ids), theta = as.double(rc_vals))
        rc_tbl <- dplyr::filter(rc_tbl, is.finite(.data$theta))
        rc_tbl <- dplyr::arrange(rc_tbl, dplyr::desc(.data$theta), .data$ID)
        rc_top_ids <- rc_tbl$ID

        if (!is.null(prev_rc_rank)) {
          ok_rank <- !is.na(rc_rank) & !is.na(prev_rc_rank)
          n_ok <- sum(ok_rank)

          uniq_now <- if (n_ok > 0L) length(unique(rc_rank[ok_rank])) else 0L
          uniq_prev <- if (n_ok > 0L) length(unique(prev_rc_rank[ok_rank])) else 0L

          # Spearman stability on RC ranks
          if (isTRUE(n_ok >= 2L) && isTRUE(uniq_now >= 2L) && isTRUE(uniq_prev >= 2L)) {
            rho_spearman_rc <- suppressWarnings(stats::cor(
              rc_rank,
              prev_rc_rank,
              method = "spearman",
              use = "pairwise.complete.obs"
            ))
          } else {
            rho_spearman_rc <- NA_real_
          }

          if (is.na(rho_spearman_rc) && isTRUE(stage1_allow_degenerate_stability)) {
            # Legacy behavior: treat degenerate/undefined rank correlations as perfectly stable.
            if (isTRUE(n_ok < 2L) || isTRUE(uniq_now < 2L) || isTRUE(uniq_prev < 2L) ||
              isTRUE(all(rc_rank[ok_rank] == prev_rc_rank[ok_rank]))) {
              rho_spearman_rc <- 1
            }
          }

          # Top-k overlap stability (on RC ranks)
          if (!is.null(prev_rc_top_ids)) {
            k_eff <- min(as.integer(stage1_topk_eff), length(prev_rc_top_ids), length(rc_top_ids))
            if (isTRUE(k_eff >= 1L)) {
              prev_top <- utils::head(prev_rc_top_ids, k_eff)
              curr_top <- utils::head(rc_top_ids, k_eff)
              topk_overlap_rc <- length(intersect(prev_top, curr_top)) / as.double(k_eff)
            } else {
              topk_overlap_rc <- NA_real_
            }

            if (!isTRUE(stage1_allow_degenerate_stability)) {
              # If ranks are degenerate/undefined, do not count as stable.
              if (isTRUE(n_ok < 2L) || isTRUE(uniq_now < 2L) || isTRUE(uniq_prev < 2L)) {
                topk_overlap_rc <- NA_real_
              }
            } else if (is.na(topk_overlap_rc)) {
              # Legacy: if overlap isn't defined, treat as stable.
              if (isTRUE(n_ok < 2L) || isTRUE(uniq_now < 2L) || isTRUE(uniq_prev < 2L)) {
                topk_overlap_rc <- 1
              }
            }
          }
        }

        prev_rc_rank <- rc_rank
        prev_rc_top_ids <- rc_top_ids
      }

      # ---- connectivity gate ----
      conn_ok <- isTRUE(pct_nodes_with_degree_gt0 >= stage1_min_pct_nodes_with_degree_gt0) &&
        isTRUE(largest_component_frac >= stage1_min_largest_component_frac) &&
        isTRUE(degree_median >= stage1_min_degree_median) &&
        isTRUE(degree_min_lcc >= stage1_min_degree_min_lcc) &&
        isTRUE(degree_min >= stage1_min_degree_min)

      conn_streak <- if (isTRUE(conn_ok)) as.integer(conn_streak) + 1L else 0L

      # ---- stability gate ----
      if (identical(stage1_stability_metric, "spearman")) {
        stab_ok <- is.finite(rho_spearman_rc) && isTRUE(rho_spearman_rc >= stage1_min_spearman)
      } else {
        stab_ok <- is.finite(topk_overlap_rc) && isTRUE(topk_overlap_rc >= stage1_topk_overlap)
      }
      stab_streak <- if (isTRUE(stab_ok)) as.integer(stab_streak) + 1L else 0L

      # ---- mixing gate (bridge-edge fraction proxy) ----
      mix_req_met <- TRUE
      if (!is.na(stage1_max_bridge_edge_frac_eff) && isTRUE(stage1_k_mix > 0L)) {
        do_mix_check <- (as.integer(stage1_rounds) %% as.integer(stage1_check_mix_every) == 0L)
        if (isTRUE(do_mix_check) &&
          isTRUE(n_components == 1L) &&
          isTRUE(largest_component_frac >= stage1_min_largest_component_frac)) {
          mix_checked_this_round <- TRUE
          if (is.finite(bridge_edge_frac)) {
            mix_ok <- isTRUE(bridge_edge_frac <= stage1_max_bridge_edge_frac_eff)
          } else {
            mix_ok <- FALSE
          }
          mix_streak <- if (isTRUE(mix_ok)) as.integer(mix_streak) + 1L else 0L
        } else {
          mix_checked_this_round <- FALSE
          mix_ok <- NA
        }
        mix_req_met <- isTRUE(mix_streak >= stage1_k_mix)
      }

      if (isTRUE(conn_streak >= stage1_k_conn) &&
        isTRUE(stab_streak >= stage1_k_stab) &&
        isTRUE(mix_req_met)) {
        # Optional spectral gap check before switching to stage2
        if (identical(spectral_gap_check, "pre_switch_and_final")) {
          if (is.null(sg_cached)) {
            sg_cached <- .estimate_spectral_gap_lazy_rw(
              pairs = results,
              ids = ids,
              weights = spectral_gap_weights,
              max_iter = spectral_gap_max_iter,
              tol = spectral_gap_tol
            )
          }
          sg_warn <- isTRUE(is.finite(sg_cached$spectral_gap_est[[1]]) &&
            (sg_cached$spectral_gap_est[[1]] < spectral_gap_warn_below))
          spectral_gap_checks <- dplyr::bind_rows(
            spectral_gap_checks,
            dplyr::mutate(
              sg_cached,
              when = "pre_switch",
              round = as.integer(r),
              spectral_gap_warn = sg_warn
            )
          )
          spectral_gap_est_val <- sg_cached$spectral_gap_est[[1]]
          lambda2_est_val <- sg_cached$lambda2_est[[1]]
          spectral_gap_iters_val <- sg_cached$iters[[1]]
          spectral_gap_converged_val <- sg_cached$converged[[1]]
          spectral_gap_warn_val <- sg_warn
          spectral_gap_when_val <- "pre_switch"
        }

        stage_after <- "stage2_bt"
        stage <- stage_after
        stage2_rounds <- as.integer(stage2_rounds) + 1L

        # Do not allow mixing streak state from stage1 to carry into stop gating
        # in stage2. Stage1 mixing checks are about whether it is safe to switch;
        # stop mixing checks should require their own consecutive passes.
        mix_streak <- 0L
        stage_switched_this_round <- TRUE
      }

      # ---- Workstream E: Stage 1 max-rounds fail-safe escalation ----
      # If Stage 1 cannot meet the connectivity gate within `stage1_max_rounds`,
      # escalate exploration + broaden candidate construction rather than stopping.
      if (isTRUE(stage1_rounds >= stage1_max_rounds_eff) && identical(stage_after, "stage1_rc")) {
        if (!isTRUE(stage1_escalated)) {
          stage1_escalated <- TRUE
          stage1_escalation_round <- as.integer(r + 1L)
        }
      }
    } else if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage2_bt")) {
      stage2_rounds <- as.integer(stage2_rounds) + 1L
    }

    # If no graph-health thresholds are set, treat the graph as healthy (no gating).
    graph_healthy <- TRUE
    if (is.finite(as.double(stop_min_degree_eff))) {
      graph_healthy <- isTRUE(graph_healthy) && isTRUE(degree_min >= as.double(stop_min_degree_eff))
    }
    if (is.finite(as.double(stop_min_largest_component_frac_eff))) {
      graph_healthy <- isTRUE(graph_healthy) && isTRUE(largest_component_frac >= as.double(stop_min_largest_component_frac_eff))
    }

    graph_healthy_base <- graph_healthy
    # ---- PR5/PR7: optional mixing guard for stopping (bridge-edge fraction proxy) ----
    mix_req_met_for_stop <- TRUE
    if (!is.na(stop_max_bridge_edge_frac_eff) &&
      isTRUE(stop_k_mix_eff > 0L) &&
      !(identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc"))) {
      do_mix_check <- (as.integer(r) %% as.integer(stop_check_mix_every) == 0L)
      if (isTRUE(do_mix_check) && !isTRUE(stage_switched_this_round)) {
        mix_checked_this_round <- TRUE
        if (isTRUE(n_components == 1L) && is.finite(bridge_edge_frac)) {
          mix_ok <- isTRUE(bridge_edge_frac <= stop_max_bridge_edge_frac_eff)
        } else {
          mix_ok <- FALSE
        }
        mix_streak <- if (isTRUE(mix_ok)) as.integer(mix_streak) + 1L else 0L
      } else {
        mix_checked_this_round <- FALSE
        mix_ok <- NA
      }
      mix_req_met_for_stop <- isTRUE(mix_streak >= stop_k_mix_eff)
    }
    graph_healthy <- isTRUE(graph_healthy) && isTRUE(mix_req_met_for_stop)

    # ---- PR7/PR8.0: stability criterion (computed regardless; stopping is gated by graph health) ----
    stability_pass <- FALSE
    if (!is.null(prev_fit_for_stability)) {
      stability_pass <- is.finite(metrics$rms_theta_delta) &&
        metrics$rms_theta_delta <= as.double(stop_stability_rms) &&
        is.finite(metrics$topk_overlap) &&
        metrics$topk_overlap >= as.double(stop_topk_overlap)
    }

    if (isTRUE(stability_pass)) {
      stability_streak <- as.integer(stability_streak) + 1L
    } else {
      stability_streak <- 0L
    }

    stability_reached <- isTRUE(stability_streak >= as.integer(stop_stability_consecutive))

    # In hybrid Stage 1, treat precision/stability as switch diagnostics, not stop rules.
    # Hard stops (no new pairs, budget, max rounds) can still terminate earlier.
    if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc")) {
      precision_reached <- FALSE
      stability_reached <- FALSE
    }


    # Enforce a minimum number of stage-2 (BT) rounds before allowing
    # precision/stability-based stopping. Hard stops (no new pairs, budget,
    # max rounds) can still terminate earlier.
    if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage2_bt") &&
      isTRUE(stage2_rounds < stage2_min_rounds)) {
      precision_reached <- FALSE
      stability_reached <- FALSE
    }

    # ---- PR7: propose next pairs (unless budget exhausted) ----
    budget_exhausted <- (as.integer(round_size) == 0L)

    pairs_next <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
    if (!isTRUE(budget_exhausted)) {
      # Theta used for pairing heuristics (may differ from final refit theta).
      # Use the *current* fit each round so the adaptive selector sees updated
      # ordering and uncertainty.
      theta_for_pairs <- fit$theta

      if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage2_bt")) {
        # Stage 2: BT-based theta + BT standard errors.
        theta_for_pairs <- dplyr::mutate(theta_for_pairs, theta = .data$theta_bt, se = .data$se_bt)
      } else if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc")) {
        # Stage 1 (Workstream C): Rank-Centrality theta, but use a degree-based
        # uncertainty proxy instead of BT SE.
        #   u_i = 1 / sqrt(max(degree_i, 1))
        deg_vec <- gs$degree
        if (!is.null(deg_vec) && length(deg_vec) > 0L && is.null(names(deg_vec)) && !is.null(gs$ids)) {
          if (length(gs$ids) == length(deg_vec)) names(deg_vec) <- gs$ids
        }

        deg_i <- rep(0L, nrow(theta_for_pairs))
        if (!is.null(deg_vec) && length(deg_vec) > 0L && !is.null(names(deg_vec))) {
          deg_i <- deg_vec[match(theta_for_pairs$ID, names(deg_vec))]
        }
        deg_i <- suppressWarnings(as.integer(deg_i))
        deg_i[is.na(deg_i) | deg_i < 0L] <- 0L

        se_proxy <- 1 / sqrt(pmax(deg_i, 1L))
        theta_for_pairs <- dplyr::mutate(theta_for_pairs, theta = .data$theta_rc, se = as.double(se_proxy))
      } else if (identical(fit_engine_running_requested, "rank_centrality")) {
        # RC mode: use Rank-Centrality theta, but pair using a degree-based
        # uncertainty proxy instead of BT SE.
        #   u_i = 1 / sqrt(max(degree_i, 1))
        deg_vec <- gs$degree
        if (!is.null(deg_vec) && length(deg_vec) > 0L && is.null(names(deg_vec)) && !is.null(gs$ids)) {
          if (length(gs$ids) == length(deg_vec)) names(deg_vec) <- gs$ids
        }

        deg_i <- rep(0L, nrow(theta_for_pairs))
        if (!is.null(deg_vec) && length(deg_vec) > 0L && !is.null(names(deg_vec))) {
          deg_i <- deg_vec[match(theta_for_pairs$ID, names(deg_vec))]
        }
        deg_i <- suppressWarnings(as.integer(deg_i))
        deg_i[is.na(deg_i) | deg_i < 0L] <- 0L

        se_proxy <- 1 / sqrt(pmax(deg_i, 1L))
        theta_for_pairs <- dplyr::mutate(theta_for_pairs, theta = .data$theta_rc, se = as.double(se_proxy))
      }
      # stage-dependent knobs (used by adaptive pairing + diagnostics)
      repeat_policy_now <- repeat_policy
      repeat_cap_now <- repeat_cap
      repeat_frac_now <- repeat_frac
      repeat_n_now <- repeat_n
      k_neighbors_now <- k_neighbors
      forbid_repeats_now <- forbid_repeats
      explore_frac_now <- if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc")) {
        stage1_explore_frac
      } else {
        stage2_explore_frac
      }
      if (identical(stage, "stage1_rc") && is.finite(stage1_explore_frac)) {
        explore_frac_now <- stage1_explore_frac
      }

      # Workstream E: if Stage 1 failed to switch by `stage1_max_rounds`,
      # increase exploration and broaden candidate windows.
      stage1_escalation_round_i <- suppressWarnings(as.integer(stage1_escalation_round))
      stage1_escalated_active <- isTRUE(stage1_escalated) &&
        !is.na(stage1_escalation_round_i) &&
        as.integer(r) >= stage1_escalation_round_i

      # Hybrid stage1: disable repeats by default (connectivity > repeats).
      # NOTE: Escalation may relax `forbid_repeats_now` (opt-in) to allow
      # unordered repeats, but stage1 still does not *plan* repeats.
      if (identical(stage, "stage1_rc")) {
        repeat_policy_now <- "none"
        repeat_cap_now <- 0
        repeat_frac_now <- 0
        repeat_n_now <- 0
        forbid_repeats_now <- TRUE
      }

      if (identical(fit_engine_running_requested, "hybrid") && identical(stage, "stage1_rc") && isTRUE(stage1_escalated_active)) {
        explore_frac_now <- max(as.double(explore_frac_now), as.double(stage1_escalated_explore_frac))

        if (isTRUE(is.infinite(stage1_escalated_k_neighbors))) {
          k_neighbors_now <- Inf
        } else {
          # Keep existing k_neighbors if already wider.
          k_neighbors_now <- max(as.integer(k_neighbors_now), as.integer(stage1_escalated_k_neighbors))
        }

        if (isTRUE(stage1_escalate_allow_unordered_repeats)) {
          # Allow unordered repeats (opt-in) by disabling the forbid_unordered gate.
          forbid_repeats_now <- FALSE
        }
      }

      pairs_next <- select_adaptive_pairs(
        samples = samples,
        theta = theta_for_pairs,
        existing_pairs = results,
        embedding_neighbors = embedding_neighbors,
        n_pairs = round_size,
        k_neighbors = k_neighbors_now,
        min_judgments = min_judgments,
        repeat_policy = repeat_policy_now,
        repeat_cap = repeat_cap_now,
        repeat_frac = repeat_frac_now,
        repeat_n = repeat_n_now,
        explore_frac = explore_frac_now,
        graph_state = gs,
        repeat_guard_min_degree = repeat_guard_min_degree,
        repeat_guard_largest_component_frac = repeat_guard_largest_component_frac,
        forbid_repeats = forbid_repeats_now,
        balance_positions = balance_positions,
        embed_far_k = embed_far_k,
        embed_quota_frac = embed_quota_frac,
        candidate_pool_cap = candidate_pool_cap,
        per_anchor_cap = per_anchor_cap,
        w_embed = w_embed,
        embed_score_mode = embed_score_mode,
        seed = if (is.null(seed_pairs)) NULL else (as.integer(seed_pairs) + as.integer(r))
      )
    }

    no_new_pairs <- (!isTRUE(budget_exhausted) && nrow(pairs_next) == 0L)
    max_rounds_reached <- ((as.integer(r) - 1L) >= as.integer(max_rounds))

    stop_chk <- .stop_decision(
      round = r,
      min_rounds = min_rounds,
      no_new_pairs = no_new_pairs,
      budget_exhausted = budget_exhausted,
      max_rounds_reached = max_rounds_reached,
      graph_healthy = graph_healthy,
      stability_reached = stability_reached,
      precision_reached = precision_reached,
      stop_reason_priority = stop_reason_priority
    )
    .validate_stop_decision(stop_chk)

    # If the only thing preventing stopping is the mix guard, label it explicitly
    if (isTRUE(stop_mix_guard_active) &&
      identical(stop_chk$details$stop_blocked_by, "graph_unhealthy") &&
      isTRUE(graph_healthy_base) && !isTRUE(mix_req_met_for_stop)) {
      stop_chk$details$stop_blocked_by <- "mix_guard"
    }

    mixing_guard_pass <- if (isTRUE(stop_mix_guard_active)) isTRUE(mix_req_met_for_stop) else NA


    stop_audit_row <- .stop_decision_record(
      round_index = r,
      stop_chk = stop_chk,
      precision_reached = precision_reached,
      stability_reached = stability_reached,
      graph_healthy = graph_healthy,
      min_rounds_satisfied = isTRUE(as.integer(r) >= as.integer(min_rounds)),
      mixing_guard_pass = if (isTRUE(stop_mix_guard_active)) isTRUE(mix_req_met_for_stop) else NA,
      no_new_pairs = no_new_pairs,
      max_rounds_hit = max_rounds_reached,
      pair_budget_exhausted = budget_exhausted
    )
    stop_audit_list[[length(stop_audit_list) + 1L]] <- stop_audit_row

    # Optional spectral gap check before stopping
    if (isTRUE(stop_chk$stop) && identical(spectral_gap_check, "pre_stop")) {
      if (is.null(sg_cached)) {
        sg_cached <- .estimate_spectral_gap_lazy_rw(
          pairs = results,
          ids = ids,
          weights = spectral_gap_weights,
          max_iter = spectral_gap_max_iter,
          tol = spectral_gap_tol
        )
      }
      sg_warn <- is.finite(sg_cached$spectral_gap_est[[1]]) &&
        (sg_cached$spectral_gap_est[[1]] < spectral_gap_warn_below)
      spectral_gap_checks <- dplyr::bind_rows(
        spectral_gap_checks,
        dplyr::mutate(
          sg_cached,
          when = "pre_stop",
          round = as.integer(r),
          spectral_gap_warn = sg_warn
        )
      )
      spectral_gap_est_val <- sg_cached$spectral_gap_est[[1]]
      lambda2_est_val <- sg_cached$lambda2_est[[1]]
      spectral_gap_iters_val <- sg_cached$iters[[1]]
      spectral_gap_converged_val <- sg_cached$converged[[1]]
      spectral_gap_warn_val <- sg_warn
      spectral_gap_when_val <- "pre_stop"
    }

    # Record graph/stability diagnostics into the per-round metrics (these columns exist in the schema template).
    # Use runner-local scalar values (renamed) to avoid accidentally reading
    # the mostly-NA placeholder columns from the metrics template.
    degree_min_val <- as.double(degree_min)
    degree_min_lcc_val <- as.double(degree_min_lcc)
    largest_component_frac_val <- as.double(largest_component_frac)
    bridge_edge_count_val <- as.integer(bridge_edge_count[[1]])
    bridge_edge_frac_val <- as.double(bridge_edge_frac[[1]])
    graph_healthy_val <- as.logical(graph_healthy)
    stop_min_degree_eff_val <- as.integer(stop_min_degree_eff)
    stop_min_largest_component_frac_eff_val <- as.double(stop_min_largest_component_frac_eff)
    stop_gating_active_val <- as.logical(stop_gating_active)
    stability_streak_val <- as.integer(stability_streak)
    stability_pass_val <- as.logical(stability_pass)
    stage1_escalation_round_val <- suppressWarnings(as.integer(stage1_escalation_round))
    stage1_escalated_val <- isTRUE(stage1_escalated) &&
      !is.na(stage1_escalation_round_val) &&
      as.integer(r) >= stage1_escalation_round_val

    mix_ok_val <- if (is.na(mix_ok)) NA else as.logical(mix_ok)
    mix_checked_this_round_val <- if (is.na(mix_checked_this_round)) NA else as.logical(mix_checked_this_round)
    mix_streak_val <- as.integer(mix_streak)

    # Pair-plan bridging progress metrics (planned pairs for this round)
    n_component_bridge_pairs_planned_val <- 0L
    n_component_bridge_pairs_valid_val <- 0L
    if (!is.null(pairs_next) && nrow(pairs_next) > 0L && "pair_type" %in% names(pairs_next)) {
      is_component_bridge <- pairs_next$pair_type == "component_bridge"
      n_component_bridge_pairs_planned_val <- as.integer(sum(is_component_bridge, na.rm = TRUE))

      if (all(c("component_id_1", "component_id_2") %in% names(pairs_next))) {
        valid_bridge <- is_component_bridge &
          !is.na(pairs_next$component_id_1) &
          !is.na(pairs_next$component_id_2) &
          (pairs_next$component_id_1 != pairs_next$component_id_2)
        n_component_bridge_pairs_valid_val <- as.integer(sum(valid_bridge, na.rm = TRUE))
      }
    }

    metrics <- metrics %>%
      dplyr::mutate(
        degree_min = degree_min_val,
        degree_min_lcc = degree_min_lcc_val,
        largest_component_frac = largest_component_frac_val,
        bridge_edge_count = bridge_edge_count_val,
        bridge_edge_frac = bridge_edge_frac_val,
        graph_healthy = graph_healthy_val,
        stop_min_degree_eff = stop_min_degree_eff_val,
        stop_min_largest_component_frac_eff = stop_min_largest_component_frac_eff_val,
        stop_gating_active = stop_gating_active_val,
        stability_streak = stability_streak_val,
        stability_pass = stability_pass_val,
        mix_ok = mix_ok_val,
        mix_checked_this_round = mix_checked_this_round_val,
        mix_streak = mix_streak_val,
        spectral_gap_est = spectral_gap_est_val,
        lambda2_est = lambda2_est_val,
        spectral_gap_iters = spectral_gap_iters_val,
        spectral_gap_converged = spectral_gap_converged_val,
        spectral_gap_warn = spectral_gap_warn_val,
        spectral_gap_when = spectral_gap_when_val,
        stage1_escalated = stage1_escalated_val,
        stage1_escalation_round = stage1_escalation_round_val,
        n_component_bridge_pairs_planned = as.integer(n_component_bridge_pairs_planned_val),
        n_component_bridge_pairs_valid = as.integer(n_component_bridge_pairs_valid_val)
      )

    metrics_hist <- dplyr::bind_rows(metrics_hist, metrics)

    this_reason <- stop_chk$reason %||% NA_character_

    this_blocked_by <- stop_chk$details$stop_blocked_by %||% NA_character_
    this_blocked_candidates <- stop_chk$details$stop_blocked_candidates %||% NA_character_

    diag_pairs_round <- NULL
    planned_repeat_pairs <- attr(pairs_next, "planned_repeat_pairs")
    diag_pairs <- attr(pairs_next, "pairing_diagnostics")
    if (!is.null(diag_pairs)) {
      diag_pairs_round <- dplyr::mutate(diag_pairs, round = as.integer(r))
    }

    # If we are stopping before scoring new pairs, state reflects current results
    st_now <- .bt_round_state(results, ids = ids, judge_col = judge)
    st_now <- dplyr::mutate(
      st_now,
      round = as.integer(r),
      stop = isTRUE(stop_chk$stop),
      stop_reason = this_reason,
      stop_blocked_by = this_blocked_by,
      stop_blocked_candidates = this_blocked_candidates
    )
    state_list[[length(state_list) + 1L]] <- st_now

    metrics_clean <- dplyr::select(metrics, -dplyr::any_of(c("stop", "stop_reason")))
    round_meta <- tibble::tibble(
      round = as.integer(r),
      n_new_pairs_scored = 0L,
      n_total_results = as.integer(nrow(results)),
      pairing_stage = as.character(stage),
      rho_spearman_rc = as.double(rho_spearman_rc),
      topk_overlap_rc = as.double(topk_overlap_rc),
      stage1_stability_metric = as.character(stage1_stability_metric),
      stage1_topk = as.integer(stage1_topk_eff),
      conn_ok = conn_ok,
      stab_ok = stab_ok,
      conn_streak = as.integer(conn_streak),
      stab_streak = as.integer(stab_streak),
      stage1_rounds = as.integer(stage1_rounds),
      stage2_rounds = as.integer(stage2_rounds),
      stop = isTRUE(stop_chk$stop),
      stop_reason = this_reason,
      stop_blocked_by = this_blocked_by,
      stop_blocked_candidates = this_blocked_candidates,
      precision_reached = isTRUE(precision_reached)
    )

    # Guard against future schema drift: ensure we never cbind duplicate names.
    metrics_clean <- dplyr::select(
      metrics_clean,
      -dplyr::any_of(intersect(names(metrics_clean), names(round_meta)))
    )

    rounds_list[[length(rounds_list) + 1L]] <- dplyr::bind_cols(
      round_meta,
      metrics_clean,
      .name_repair = "check_unique"
    )
    prev_metrics <- metrics
    prev_fit_for_stability <- fit

    # Guardrail validators
    .validate_stop_metrics_tbl(dplyr::slice(metrics, 1L))
    .validate_rounds_schema(dplyr::bind_rows(rounds_list))

    if (isTRUE(stop_chk$stop)) {
      stop_reason <- this_reason
      stop_round <- as.integer(r)

      fits[[length(fits)]] <- tag_fit(
        fits[[length(fits)]],
        round_index = r,
        stage = "round_fit",
        n_results = nrow(results),
        n_pairs_this_round = NA_integer_,
        stop_reason = this_reason
      )
      final_fit <- fits[[length(fits)]]

      checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = TRUE)
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
      break
    }

    res_next <- .coerce_judge_output(judge_fun(pairs_next))
    res_next <- .validate_judge_results(res_next, ids = ids, judge_col = judge)
    res_next <- .add_pair_key_direction(res_next)

    n_added <- nrow(res_next)

    if (n_added > 0L) {
      results <- dplyr::bind_rows(results, res_next)
      results <- .add_pair_key_direction(results)

      # If unordered repeats are forbidden, enforce uniqueness of unordered pairs
      # after appending new results (important for resume-from-checkpoint paths).
      if (repeat_policy %in% c("none", "forbid_unordered")) {
        key_unordered <- .unordered_pair_key(results$ID1, results$ID2)
        keep <- !duplicated(key_unordered)
        if (length(keep) == nrow(results)) {
          results <- results[keep, , drop = FALSE]
          results <- .add_pair_key_direction(results)
        }
      }

      rounds_list[[length(rounds_list)]][["n_new_pairs_scored"]] <- as.integer(n_added)
      rounds_list[[length(rounds_list)]][["n_total_results"]] <- as.integer(nrow(results))

      # Finalize pairing diagnostics (repeat completion + consistency).
      if (!is.null(diag_pairs_round)) {
        planned_keys <- character()
        if (!is.null(planned_repeat_pairs) && is.data.frame(planned_repeat_pairs) && nrow(planned_repeat_pairs) > 0L) {
          planned_repeat_pairs <- tibble::as_tibble(planned_repeat_pairs)
          if ("pair_key" %in% names(planned_repeat_pairs)) {
            planned_keys <- as.character(planned_repeat_pairs$pair_key)
          } else if (all(c("ID1", "ID2") %in% names(planned_repeat_pairs))) {
            planned_keys <- .unordered_pair_key(planned_repeat_pairs$ID1, planned_repeat_pairs$ID2)
          }
          planned_keys <- unique(planned_keys)
        }

        n_repeat_completed <- 0L
        # Deterministic: use 0 when no repeats were completed in the round.
        repeat_consistency_round <- 0
        if (length(planned_keys) > 0L) {
          res_next_pk <- dplyr::mutate(res_next, pair_key = .unordered_pair_key(.data$ID1, .data$ID2))
          res_next_pk <- dplyr::filter(res_next_pk, .data$pair_key %in% planned_keys)
          res_next_pk <- dplyr::filter(res_next_pk, !is.na(.data$better_id) & nzchar(.data$better_id))

          n_repeat_completed <- nrow(res_next_pk)

          prior <- dplyr::filter(results, .data$pair_key %in% planned_keys)
          prior <- dplyr::filter(prior, !is.na(.data$better_id) & nzchar(.data$better_id))
          prior <- dplyr::group_by(prior, .data$pair_key)
          prior <- dplyr::slice_head(prior, n = 1L)
          prior <- dplyr::ungroup(prior)
          prior <- dplyr::select(prior, "pair_key", prior_better_id = "better_id")

          joined <- dplyr::inner_join(
            prior,
            dplyr::select(res_next_pk, "pair_key", new_better_id = "better_id"),
            by = "pair_key"
          )
          if (nrow(joined) > 0L) {
            repeat_consistency_round <- mean(joined$prior_better_id == joined$new_better_id)
          }
        }

        # Cumulative consistency among unordered pairs with exactly two completed judgments.
        # Deterministic: use 0 when no pairs have two completed judgments.
        repeat_consistency_cum <- 0
        by_key <- dplyr::filter(results, !is.na(.data$better_id) & nzchar(.data$better_id))
        by_key <- dplyr::group_by(by_key, .data$pair_key)
        by_key <- dplyr::summarise(by_key,
          n_done = dplyr::n(),
          n_unique_winner = dplyr::n_distinct(.data$better_id),
          .groups = "drop"
        )
        two_done <- dplyr::filter(by_key, .data$n_done == 2L)
        if (nrow(two_done) > 0L) {
          repeat_consistency_cum <- mean(two_done$n_unique_winner == 1L)
        }

        diag_pairs_round <- dplyr::mutate(
          diag_pairs_round,
          n_repeat_completed = as.integer(n_repeat_completed),
          repeat_consistency_rate_round = repeat_consistency_round,
          repeat_consistency_rate_cumulative = repeat_consistency_cum
        )
        pairing_diag_list[[length(pairing_diag_list) + 1L]] <- diag_pairs_round
      }

      # update the most recent state row to reflect post-append counts
      state_list[[length(state_list)]] <- dplyr::mutate(
        .bt_round_state(results, ids = ids, judge_col = judge),
        round = as.integer(r),
        stop = FALSE,
        stop_reason = NA_character_
      )

      # checkpoint bookkeeping updated at end-of-round (below)
    } else {
      this_reason <- .bt_resolve_stop_reason(no_new_results = TRUE)
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- this_reason
      stop_reason <- this_reason
      stop_round <- as.integer(r)

      fits[[length(fits)]] <- tag_fit(
        fits[[length(fits)]],
        round_index = r,
        stage = "round_fit",
        n_results = nrow(results),
        n_pairs_this_round = NA_integer_,
        stop_reason = this_reason
      )
      final_fit <- fits[[length(fits)]]

      # mark state with terminal reason
      state_list[[length(state_list)]] <- dplyr::mutate(
        state_list[[length(state_list)]],
        stop = TRUE,
        stop_reason = this_reason
      )

      checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = TRUE)
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
      break
    }

    # checkpoint after successfully finishing this round
    checkpoint_payload_last <- .make_checkpoint_payload(next_round = r + 1L, completed = FALSE)
    if ((r %% checkpoint_every) == 0L) {
      .write_checkpoint_now(checkpoint_payload_last, round_index = r)
    }
  }

  if (is.na(stop_reason)) {
    if (max_rounds == 0L) {
      stop_reason <- .bt_resolve_stop_reason(max_rounds_is_zero = TRUE)
      stop_round <- 0L
    } else if (start_round > max_rounds) {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      stop_round <- as.integer(max_rounds)
    } else if (round_size == 0L && init_round_size == 0L && (nrow(rounds_tbl_prev) + length(rounds_list) == 0L)) {
      stop_reason <- .bt_resolve_stop_reason(round_size_zero = TRUE)
      stop_round <- 0L
    } else if (nrow(rounds_tbl_prev) + length(rounds_list) == 0L) {
      stop_reason <- .bt_resolve_stop_reason(no_results = TRUE)
    } else {
      stop_reason <- .bt_resolve_stop_reason(reached_max_rounds = TRUE)
      stop_round <- as.integer(max_rounds)
      rounds_list[[length(rounds_list)]][["stop_reason"]] <- stop_reason
      if (length(fits) > 0L) {
        fits[[length(fits)]] <- tag_fit(
          fits[[length(fits)]],
          round_index = max_rounds,
          stage = "round_fit",
          n_results = nrow(results),
          n_pairs_this_round = NA_integer_,
          stop_reason = "max_rounds_reached"
        )
        final_fit <- fits[[length(fits)]]
      }
      if (length(state_list) > 0L) {
        state_list[[length(state_list)]] <- dplyr::mutate(
          state_list[[length(state_list)]],
          stop = TRUE,
          stop_reason = "max_rounds_reached"
        )
      }
    }
  }

  rounds_tbl <- dplyr::bind_rows(
    rounds_tbl_prev,
    if (length(rounds_list) == 0L) tibble::tibble() else dplyr::bind_rows(rounds_list)
  )
  state_tbl <- dplyr::bind_rows(
    state_tbl_prev,
    if (length(state_list) == 0L) tibble::tibble() else dplyr::bind_rows(state_list)
  )

  # ---- Normalize stopping columns (some early-exit paths can yield list-cols) ----
  if (!"stop_reason" %in% names(rounds_tbl)) {
    rounds_tbl$stop_reason <- NA_character_
  }
  if (is.list(rounds_tbl$stop_reason)) {
    rounds_tbl$stop_reason <- vapply(
      rounds_tbl$stop_reason,
      function(x) {
        if (is.null(x)) {
          return(NA_character_)
        }
        as.character(x)[1]
      },
      character(1)
    )
  } else {
    rounds_tbl$stop_reason <- as.character(rounds_tbl$stop_reason)
  }

  if (!"stop_blocked_by" %in% names(rounds_tbl)) {
    rounds_tbl$stop_blocked_by <- NA_character_
  }
  if (is.list(rounds_tbl$stop_blocked_by)) {
    rounds_tbl$stop_blocked_by <- vapply(
      rounds_tbl$stop_blocked_by,
      function(x) {
        if (is.null(x)) {
          return(NA_character_)
        }
        as.character(x)[1]
      },
      character(1)
    )
  } else {
    rounds_tbl$stop_blocked_by <- as.character(rounds_tbl$stop_blocked_by)
  }

  if (!"stop_blocked_candidates" %in% names(rounds_tbl)) {
    rounds_tbl$stop_blocked_candidates <- NA_character_
  }
  if (is.list(rounds_tbl$stop_blocked_candidates)) {
    rounds_tbl$stop_blocked_candidates <- vapply(
      rounds_tbl$stop_blocked_candidates,
      function(x) {
        if (is.null(x)) {
          return(NA_character_)
        }
        as.character(x)[1]
      },
      character(1)
    )
  } else {
    rounds_tbl$stop_blocked_candidates <- as.character(rounds_tbl$stop_blocked_candidates)
  }

  # ---- Workstream B: schema-stable stage bookkeeping (rounds table) ----
  if (!"pairing_stage" %in% names(rounds_tbl)) {
    rounds_tbl$pairing_stage <- character(nrow(rounds_tbl))
  }
  rounds_tbl$pairing_stage <- as.character(rounds_tbl$pairing_stage)

  if (!"stage" %in% names(rounds_tbl)) {
    rounds_tbl$stage <- rounds_tbl$pairing_stage
  }
  rounds_tbl$stage <- as.character(rounds_tbl$stage)
  if (nrow(rounds_tbl) > 0L) {
    stage_missing <- is.na(rounds_tbl$stage) | rounds_tbl$stage == ""
    if (any(stage_missing)) {
      rounds_tbl$stage[stage_missing] <- rounds_tbl$pairing_stage[stage_missing]
    }
  }

  if (!"stage1_rounds" %in% names(rounds_tbl)) {
    rounds_tbl$stage1_rounds <- integer(nrow(rounds_tbl))
  }
  rounds_tbl$stage1_rounds <- as.integer(rounds_tbl$stage1_rounds)
  rounds_tbl$stage1_rounds[is.na(rounds_tbl$stage1_rounds)] <- 0L

  if (!"stage2_rounds" %in% names(rounds_tbl)) {
    rounds_tbl$stage2_rounds <- integer(nrow(rounds_tbl))
  }
  rounds_tbl$stage2_rounds <- as.integer(rounds_tbl$stage2_rounds)
  rounds_tbl$stage2_rounds[is.na(rounds_tbl$stage2_rounds)] <- 0L

  # ---- Workstream E: schema-stable Stage 1 escalation logging ----
  if (!"stage1_escalated" %in% names(rounds_tbl)) {
    rounds_tbl$stage1_escalated <- rep(FALSE, nrow(rounds_tbl))
  }
  rounds_tbl$stage1_escalated <- as.logical(rounds_tbl$stage1_escalated)
  rounds_tbl$stage1_escalated[is.na(rounds_tbl$stage1_escalated)] <- FALSE

  if (!"stage1_escalation_round" %in% names(rounds_tbl)) {
    rounds_tbl$stage1_escalation_round <- rep(NA_integer_, nrow(rounds_tbl))
  }
  rounds_tbl$stage1_escalation_round <- suppressWarnings(as.integer(rounds_tbl$stage1_escalation_round))

  if (!"precision_reached" %in% names(rounds_tbl)) {
    rounds_tbl$precision_reached <- logical(nrow(rounds_tbl))
  }
  rounds_tbl$precision_reached <- as.logical(rounds_tbl$precision_reached)
  rounds_tbl$precision_reached[is.na(rounds_tbl$precision_reached)] <- FALSE


  if (!"stop" %in% names(rounds_tbl)) {
    rounds_tbl$stop <- rep(FALSE, nrow(rounds_tbl))
  }
  if (is.list(rounds_tbl$stop)) {
    rounds_tbl$stop <- vapply(
      rounds_tbl$stop,
      function(x) {
        if (is.null(x)) {
          return(FALSE)
        }
        isTRUE(x)
      },
      logical(1)
    )
  } else {
    rounds_tbl$stop <- as.logical(rounds_tbl$stop)
  }
  rounds_tbl$stop[is.na(rounds_tbl$stop)] <- FALSE
  # If a stop_reason is recorded, the run stopped for that round.
  rounds_tbl$stop <- rounds_tbl$stop | !is.na(rounds_tbl$stop_reason)

  # ---- Spectral gap check: final (optional) ----
  if (!identical(spectral_gap_check, "never") &&
    nrow(results) > 0L &&
    (identical(spectral_gap_check, "final") ||
      identical(spectral_gap_check, "pre_stop") ||
      identical(spectral_gap_check, "pre_switch_and_final"))) {
    final_round <- if (nrow(rounds_tbl) > 0L) as.integer(max(rounds_tbl$round, na.rm = TRUE)) else NA_integer_
    sg_final <- .estimate_spectral_gap_lazy_rw(
      pairs = results,
      ids = ids,
      weights = spectral_gap_weights,
      max_iter = spectral_gap_max_iter,
      tol = spectral_gap_tol
    )
    sg_warn <- isTRUE(is.finite(sg_final$spectral_gap_est[[1]]) && (sg_final$spectral_gap_est[[1]] < spectral_gap_warn_below))
    spectral_gap_checks <- dplyr::bind_rows(
      spectral_gap_checks,
      dplyr::mutate(sg_final,
        when = "final",
        round = final_round,
        spectral_gap_warn = sg_warn
      )
    )
  }


  bt_data_final <- if (nrow(results) == 0L) {
    tibble::tibble(object1 = character(), object2 = character(), result = numeric())
  } else if (is.null(judge)) {
    build_bt_fun(results, judge = NULL)
  } else {
    build_bt_fun(results, judge = judge)
  }

  # Optional final refit / combined estimates table.
  estimates <- NULL
  final_models <- NULL
  theta <- NULL
  theta_engine <- NA_character_
  fit_provenance <- list()

  final_refit_attempted <- FALSE
  final_refit_failed <- FALSE
  final_refit_disabled <- FALSE

  if (!isTRUE(final_refit) && nrow(results) > 0L) {
    final_refit_disabled <- TRUE
  }
  if (isTRUE(final_refit) && nrow(results) > 0L) {
    final_refit_attempted <- TRUE
    tmp <- tryCatch(
      compute_final_estimates(
        results = results,
        ids = ids,
        fit_engine_final = fit_engine_final,
        bt_bias_reduction = final_bt_bias_reduction,
        rc_smoothing = rc_smoothing,
        rc_damping = rc_damping
      ),
      error = function(e) e
    )
    if (inherits(tmp, "error")) {
      final_refit_failed <- TRUE
      warning(
        "`final_refit = TRUE` requested but final estimates could not be computed: ",
        conditionMessage(tmp),
        call. = FALSE
      )
    } else {
      estimates <- tmp$estimates
      final_models <- list(
        bt_fit = tmp$bt_fit,
        rc_fit = tmp$rc_fit,
        diagnostics = tmp$diagnostics
      )

      fit_provenance <- tmp$provenance

      # Guarantee a simple top-level theta object for downstream convenience.
      # If a BT variant succeeded, use that; otherwise fall back to Rank Centrality.
      if (is.data.frame(estimates) && nrow(estimates) > 0L && identical(unique(estimates$bt_status), "succeeded")) {
        theta <- tibble::tibble(
          ID = as.character(estimates$ID),
          theta = as.double(estimates$theta_bt_firth),
          se = as.double(estimates$se_bt_firth),
          rank = as.integer(estimates$rank_bt_firth)
        )
        theta_engine <- as.character(unique(estimates$bt_engine_used))[1]
      } else if (is.data.frame(estimates) && nrow(estimates) > 0L) {
        theta <- tibble::tibble(
          ID = as.character(estimates$ID),
          theta = as.double(estimates$theta_rc),
          se = NA_real_,
          rank = as.integer(estimates$rank_rc)
        )
        theta_engine <- "rank_centrality"
      }
    }
  }

  # ---- PR4.1 fallback: ensure theta exists whenever we have a running fit ----
  if (is.null(theta) && length(fits) > 0L) {
    last_fit <- fits[[length(fits)]]
    fb <- .theta_from_last_running_fit(last_fit, id_vec = ids)

    if (!is.null(fb$theta)) {
      theta <- fb$theta
      theta_engine <- fb$engine

      if (is.null(fit_provenance)) fit_provenance <- list()
      fit_provenance$fallback_used <- TRUE
      fit_provenance$fallback_source <- "last_running_fit"

      fit_provenance$fallback_reason <- dplyr::case_when(
        final_refit_disabled ~ "final_refit_disabled",
        final_refit_failed ~ "final_refit_failed",
        final_refit_attempted ~ "final_refit_unavailable",
        TRUE ~ "final_refit_unavailable"
      )
    }
  }

  stop_audit <- if (length(stop_audit_list) == 0L) {
    tibble::tibble(
      round_index = integer(),
      stop_decision = logical(),
      stop_reason = character(),
      precision_reached = logical(),
      stability_reached = logical(),
      graph_healthy = logical(),
      min_rounds_satisfied = logical(),
      mixing_guard_pass = logical(),
      no_new_pairs = logical(),
      max_rounds_hit = logical(),
      pair_budget_exhausted = logical()
    )
  } else {
    dplyr::bind_rows(stop_audit_list)
  }

  pairing_diagnostics <- if (length(pairing_diag_list) == 0L) {
    tibble::tibble(
      round = integer(),
      n_candidates_raw = integer(),
      n_candidates_scored = integer(),
      n_candidates_after_constraints = integer(),
      n_candidates_after_caps = integer(),
      n_selected = integer(),
      n_repeat_eligible = integer(),
      n_repeat_planned = integer(),
      n_repeat_completed = integer(),
      repeat_consistency_rate_round = double(),
      repeat_consistency_rate_cumulative = double(),
      repeat_policy = character(),
      repeat_cap = integer(),
      repeat_frac = double(),
      repeat_n = integer(),
      repeat_quota_n = integer(),
      repeat_guard_passed = logical(),
      repeat_guard_min_degree = integer(),
      repeat_guard_largest_component_frac = double(),
      graph_degree_min = double(),
      graph_largest_component_frac = double(),
      degree_min_before = double(),
      largest_component_frac_before = double(),
      degree_min_after = double(),
      largest_component_frac_after = double(),
      fallback_path = character(),
      fallback_trigger = character(),
      n_pairs_source_normal = integer(),
      n_pairs_source_bridge = integer(),
      n_pairs_source_repeat_reverse = integer(),
      n_pairs_source_random = integer(),
      n_selected_theta = integer(),
      n_selected_embed = integer(),
      n_selected_far = integer(),
      embed_neighbor_hit_rate = double(),
      embed_quota_frac = double(),
      candidate_pool_cap = integer(),
      per_anchor_cap = integer(),
      w_embed = double(),
      embed_score_mode = character()
    )
  } else {
    dplyr::bind_rows(pairing_diag_list)
  }

  # Pairing diagnostics contract: add stable per-round fields derived from `rounds_tbl`.
  # (Forward-compatible: extra columns are retained.)
  if (!is.null(pairing_diagnostics) && nrow(pairing_diagnostics) > 0L &&
    !is.null(rounds_tbl) && nrow(rounds_tbl) > 0L) {
    rounds_contract <- rounds_tbl %>%
      dplyr::transmute(
        round = .data$round,
        n_pairs_completed = as.integer(.data$n_new_pairs_scored),
        degree_min = as.double(.data$degree_min),
        largest_component_frac = as.double(.data$largest_component_frac),
        rms_theta_delta = as.double(.data$rms_theta_delta),
        topk_overlap = as.double(.data$topk_overlap),
        stop = as.logical(.data$stop),
        stop_reason = as.character(.data$stop_reason),
        stop_blocked_by = as.character(.data$stop_blocked_by),
        stop_blocked_candidates = as.character(.data$stop_blocked_candidates)
      )

    pairing_diagnostics <- pairing_diagnostics %>%
      dplyr::left_join(rounds_contract, by = "round")

    if ("n_selected" %in% names(pairing_diagnostics)) {
      pairing_diagnostics$n_pairs_planned <- as.integer(pairing_diagnostics$n_selected)
    } else {
      pairing_diagnostics$n_pairs_planned <- as.integer(NA_integer_)
    }
  }

  if (!is.null(pairing_diagnostics) && nrow(pairing_diagnostics) == 0L) {
    # Ensure stable columns exist even when no rounds were executed.
    pairing_diagnostics$n_pairs_planned <- integer()
    pairing_diagnostics$n_pairs_completed <- integer()
    pairing_diagnostics$degree_min <- double()
    pairing_diagnostics$largest_component_frac <- double()
    pairing_diagnostics$rms_theta_delta <- double()
    pairing_diagnostics$topk_overlap <- double()
    pairing_diagnostics$stop <- logical()
    pairing_diagnostics$stop_reason <- character()
    pairing_diagnostics$stop_blocked_by <- character()
    pairing_diagnostics$stop_blocked_candidates <- character()
  }
  # PR8 contract: fit_provenance must always be a list (possibly empty).
  if (is.null(fit_provenance)) fit_provenance <- list()

  out <- list(
    results = results,
    metrics = metrics_hist,
    bt_data = bt_data_final,
    fits = fits,
    final_fit = final_fit,
    theta = theta,
    theta_engine = theta_engine,
    estimates = estimates,
    final_models = final_models,
    fit_provenance = fit_provenance,
    stop_reason = stop_reason,
    stop_round = stop_round,
    rounds = rounds_tbl,
    stop_audit = stop_audit,
    spectral_gap_checks = spectral_gap_checks,
    state = state_tbl,
    n_fallback_random_rounds = if (!is.null(pairing_diagnostics) && "fallback_path" %in% names(pairing_diagnostics)) {
      as.integer(sum(pairing_diagnostics$fallback_path == "controlled_random", na.rm = TRUE))
    } else {
      0L
    },
    pairing_diagnostics = pairing_diagnostics,
    pairs_bootstrap = pairs_bootstrap
  )

  # write a final checkpoint including the returned object
  if (!is.null(checkpoint_dir) && nzchar(checkpoint_dir)) {
    next_round_final <- if (is.na(stop_round)) as.integer(nrow(rounds_tbl) + 1L) else as.integer(stop_round + 1L)
    checkpoint_payload_last <- .make_checkpoint_payload(next_round = next_round_final, completed = TRUE, out = out)
    .write_checkpoint_now(checkpoint_payload_last, round_index = stop_round)
  }

  validate_pairwise_run_output(out)
  .as_pairwise_run(out, run_type = "adaptive")
}

#' Simulate a judge for BT pairwise comparisons
#'
#' This helper produces synthetic pairwise comparison results for testing,
#' vignettes, and benchmarking. It takes a table of pairs (with texts) and returns
#' \code{better_id} outcomes based on a latent "true ability" vector.
#'
#' The simulator can be deterministic (always pick higher true ability) or
#' stochastic using a Bradley–Terry / logistic probability model.
#'
#' @param pairs A tibble/data.frame with columns \code{ID1}, \code{text1}, \code{ID2}, \code{text2}.
#' @param true_theta Named numeric vector of latent abilities. Names must include
#' the IDs used in \code{pairs}. Missing IDs are treated as 0.
#' @param judges Optional character vector of judge identifiers. If length > 1,
#' each row is assigned a judge (round-robin by default).
#' @param judge_col Optional character scalar column name to use for judge labels.
#' If \code{NULL} (default), no judge column is returned.
#' @param deterministic Logical; if \code{TRUE}, always choose the higher \code{true_theta}.
#' If equal, break ties at random (seed-controlled). Default \code{FALSE}.
#' @param seed Optional integer seed; RNG state is restored afterwards.
#' @param round_robin Logical; if \code{TRUE} (default) and \code{length(judges) > 1},
#' assign judges in repeating order. If \code{FALSE}, assign judges uniformly at random.
#'
#' @return A tibble with columns \code{ID1}, \code{ID2}, \code{better_id}, and optionally
#' a judge column.
#'
#' @examples
#' pairs <- tibble::tibble(
#'   ID1 = c("A", "B"),
#'   text1 = c("a", "b"),
#'   ID2 = c("C", "D"),
#'   text2 = c("c", "d")
#' )
#' true_theta <- c(A = 2, B = 1, C = 0, D = -1)
#' simulate_bt_judge(pairs, true_theta, deterministic = TRUE, seed = 1)
#'
#' @export
simulate_bt_judge <- function(pairs,
                              true_theta,
                              judges = "judge_1",
                              judge_col = NULL,
                              deterministic = FALSE,
                              seed = NULL,
                              round_robin = TRUE) {
  pairs <- tibble::as_tibble(pairs)
  req <- c("ID1", "text1", "ID2", "text2")
  miss <- setdiff(req, names(pairs))
  if (length(miss) > 0L) stop("`pairs` must contain columns: ", paste(req, collapse = ", "), call. = FALSE)

  if (!is.numeric(true_theta) || is.null(names(true_theta))) {
    stop("`true_theta` must be a named numeric vector.", call. = FALSE)
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L || is.na(judge_col) || nchar(judge_col) == 0L) {
      stop("`judge_col` must be a non-empty character scalar when provided.", call. = FALSE)
    }
  }

  judges <- as.character(judges)
  if (length(judges) < 1L || anyNA(judges) || any(judges == "")) {
    stop("`judges` must contain at least one non-missing, non-empty label.", call. = FALSE)
  }

  n <- nrow(pairs)
  if (n == 0L) {
    out0 <- tibble::tibble(ID1 = character(), ID2 = character(), better_id = character())
    if (!is.null(judge_col)) out0[[judge_col]] <- character()
    return(out0)
  }

  id1 <- trimws(as.character(pairs$ID1))
  id2 <- trimws(as.character(pairs$ID2))

  th1 <- true_theta[id1]
  th2 <- true_theta[id2]
  th1[is.na(th1)] <- 0
  th2[is.na(th2)] <- 0

  out <- .with_seed_restore(
    seed,
    f = function() {
      better <- character(n)

      if (isTRUE(deterministic)) {
        for (i in seq_len(n)) {
          if (th1[i] > th2[i]) {
            better[i] <- id1[i]
          } else if (th2[i] > th1[i]) {
            better[i] <- id2[i]
          } else {
            better[i] <- if (stats::runif(1) < 0.5) id1[i] else id2[i]
          }
        }
      } else {
        for (i in seq_len(n)) {
          d <- th1[i] - th2[i]
          p <- 1 / (1 + exp(-d))
          better[i] <- if (stats::runif(1) < p) id1[i] else id2[i]
        }
      }

      out <- tibble::tibble(
        ID1 = id1,
        ID2 = id2,
        better_id = better
      )

      if (!is.null(judge_col)) {
        if (length(judges) == 1L) {
          out[[judge_col]] <- rep(judges[[1]], n)
        } else if (isTRUE(round_robin)) {
          out[[judge_col]] <- judges[((seq_len(n) - 1L) %% length(judges)) + 1L]
        } else {
          out[[judge_col]] <- judges[sample.int(length(judges), size = n, replace = TRUE)]
        }
      }

      out
    },
    arg_name = "seed"
  )

  out
}
