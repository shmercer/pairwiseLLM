#' Run one adaptive round: compute metrics, decide stopping, and propose next pairs
#'
#' This helper is intended for round-based adaptive comparative judgement workflows.
#' Given a current Bradley–Terry fit (typically from \code{\link{fit_bt_model}}),
#' it computes stopping metrics, applies stopping rules, and (if not stopping)
#' proposes a new set of pairs for the next round using \code{\link{select_adaptive_pairs}}.
#'
#' This function does not fit the model itself; it expects you to provide a fit.
#' A typical loop is:
#' \enumerate{
#'   \item Fit a BT model using current results
#'   \item Call \code{bt_adaptive_round()} to get \code{pairs_next}
#'   \item Score \code{pairs_next} (LLM/humans), append results, repeat
#' }
#'
#' @param samples A tibble/data frame with columns \code{ID} and \code{text}.
#' @param fit A list returned by \code{\link{fit_bt_model}} containing at least
#'   \code{$theta} with columns \code{ID}, \code{theta}, and \code{se}.
#' @param existing_pairs Optional data frame of previously judged pairs (used for
#'   repeat prevention, judgment counts, and position balancing). Supported formats:
#'   \itemize{
#'     \item \code{ID1}, \code{ID2}
#'     \item \code{object1}, \code{object2}
#'   }
#' @param prev_metrics Optional one-row tibble of prior-round metrics (as returned
#'   by \code{\link{bt_stop_metrics}}), used for stability-based stopping.
#' @param round_size Integer number of new pairs to propose for the next round.
#' @param se_probs Numeric vector of probabilities for SE quantiles passed to
#'   \code{\link{bt_stop_metrics}}. Default: \code{c(0.5, 0.9, 0.95)}.
#' @param fit_bounds Numeric length-2 vector giving lower/upper acceptable
#'   infit/outfit bounds. Default: \code{c(0.7, 1.3)}.
#' @param stopping_tier Preset stopping thresholds to use
#'   (good/strong/very_strong).
#'
#' @param reliability_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$reliability >= reliability_target}.
#' @param sepG_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$sepG >= sepG_target}.
#' @param rel_se_p90_target Optional numeric. If not \code{NA}, require
#'   \code{metrics$rel_se_p90 <= rel_se_p90_target} to meet the precision target.
#' @param rel_se_p90_min_improve Optional numeric. If not \code{NA} and \code{prev_metrics}
#'   is provided, allow stopping when improvement in \code{rel_se_p90} stalls.
#' @param max_item_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$item_misfit_prop <= max_item_misfit_prop} (when available).
#' @param max_judge_misfit_prop Optional numeric. If not \code{NA}, require
#'   \code{metrics$judge_misfit_prop <= max_judge_misfit_prop} (when available).
#'
#' @param k_neighbors Integer number of adjacent neighbors (in theta order) to consider
#'   in candidate generation. Passed to \code{\link{select_adaptive_pairs}}.
#' @param min_judgments Integer minimum desired number of judgments per item.
#'   Passed to \code{\link{select_adaptive_pairs}}.
#' @param repeat_policy Character repeat planning policy passed to
#'   \code{\link{select_adaptive_pairs}}. \code{"none"} disables planning repeat
#'   pairs. \code{"reverse_only"} plans a subset of opposite-direction repeats for
#'   eligible unordered pairs.
#' @param repeat_cap Non-negative integer cap on the number of planned repeat
#'   pairs per unordered pair key. For \code{repeat_policy = "reverse_only"},
#'   each unordered pair is eligible for at most \code{repeat_cap} planned reverse
#'   repeats.
#' @param repeat_frac Numeric in \code{[0, 1]}. Target fraction of the planned
#'   \code{round_size} pairs that should be reserved for repeat checks (when
#'   eligible repeat pairs exist).
#' @param repeat_n Optional non-negative integer. If provided, overrides
#'   \code{repeat_frac} and targets this many planned repeat pairs for the round.
#' @param repeat_guard_min_degree Integer. Guard for enabling repeat planning:
#'   do not plan repeats until the minimum graph degree across IDs is at least
#'   this value.
#' @param repeat_guard_largest_component_frac Numeric in \code{[0, 1]}. Guard for
#'   enabling repeat planning: do not plan repeats until the largest connected
#'   component contains at least this fraction of IDs.
#' @param forbid_repeats Logical; passed to \code{\link{select_adaptive_pairs}}.
#' @param balance_positions Logical; passed to \code{\link{select_adaptive_pairs}}.
#' @param embedding_neighbors Optional embedding-based neighbor lists used to
#'   augment candidate generation. This should be a named list mapping each ID to
#'   a character vector of neighbor IDs (or a matrix/data.frame with rownames = IDs
#'   and neighbor IDs in columns). Passed to \code{\link{select_adaptive_pairs}}.
#' @param embed_far_k Integer; number of additional "far" candidates to sample
#'   per item when embedding_neighbors is provided. Passed to
#'   select_adaptive_pairs().
#' @param embed_quota_frac Numeric in \code{[0, 1]}. Minimum fraction of selected pairs
#'   that should come from embedding-neighbor candidates. Passed to
#'   select_adaptive_pairs().
#' @param candidate_pool_cap Global cap on candidate pairs (after constraints and
#'   optional per-anchor capping). Passed to select_adaptive_pairs().
#' @param per_anchor_cap Optional cap on candidate pairs retained per anchor
#'   item (after scoring). Passed to select_adaptive_pairs().
#' @param w_embed Non-negative numeric weight for embedding-neighbor candidates
#'   in scoring. Passed to select_adaptive_pairs().
#' @param embed_score_mode Character; how to compute the embedding score when
#'   w_embed > 0. Passed to select_adaptive_pairs().
#' @param seed Optional integer seed for reproducibility; passed to \code{\link{select_adaptive_pairs}}.
#'
#' @return A list with:
#' \describe{
#'   \item{metrics}{One-row tibble from \code{\link{bt_stop_metrics}}.}
#'   \item{decision}{List from \code{\link{bt_should_stop}} containing \code{stop}, \code{details}, and \code{improve}.}
#'   \item{pairs_next}{Tibble of proposed pairs with columns \code{ID1}, \code{text1}, \code{ID2}, \code{text2}.}
#' }
#'
#' @examples
#' samples <- tibble::tibble(
#'   ID = c("A", "B", "C"),
#'   text = c("text A", "text B", "text C")
#' )
#'
#' # Mock fit object (for example/documentation)
#' fit <- list(
#'   engine = "mock",
#'   reliability = 0.95,
#'   theta = tibble::tibble(
#'     ID = c("A", "B", "C"),
#'     theta = c(0.0, 0.1, 0.2),
#'     se = c(0.5, 0.5, 0.5)
#'   ),
#'   diagnostics = list(sepG = 3.5)
#' )
#'
#' out <- bt_adaptive_round(samples, fit, round_size = 2, seed = 1)
#' out$decision$stop
#' out$pairs_next
#'
#' @export
bt_adaptive_round <- function(samples,
                              fit,
                              existing_pairs = NULL,
                              prev_metrics = NULL,
                              round_size,
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
                              embedding_neighbors = NULL,
                              embed_far_k = 0,
                              embed_quota_frac = 0.25,
                              candidate_pool_cap = Inf,
                              per_anchor_cap = Inf,
                              w_embed = 1,
                              embed_score_mode = "rank_decay",
                              seed = NULL) {
  samples <- tibble::as_tibble(samples)
  req_s <- c("ID", "text")
  if (!all(req_s %in% names(samples))) {
    stop("`samples` must contain columns: ID, text", call. = FALSE)
  }

  if (!is.list(fit) || is.null(fit$theta)) {
    stop("`fit` must be a list (from `fit_bt_model()`) containing `$theta`.", call. = FALSE)
  }

  round_size <- as.integer(round_size)
  if (is.na(round_size) || round_size < 0L) {
    stop("`round_size` must be a non-negative integer.", call. = FALSE)
  }

  metrics <- bt_stop_metrics(fit, se_probs = se_probs, fit_bounds = fit_bounds)

  # Ensure a consistent superset schema before stopping logic. Some thresholds
  # depend on columns that may be absent when diagnostics are unavailable.
  metrics <- .bt_align_metrics(metrics, se_probs = se_probs)

  stopping_tier <- match.arg(stopping_tier)
  tier_params <- bt_stop_tiers()[[stopping_tier]]

  stop_overrides <- list()
  if (!missing(reliability_target)) stop_overrides$reliability_target <- reliability_target
  if (!missing(sepG_target)) stop_overrides$sepG_target <- sepG_target
  if (!missing(rel_se_p90_target)) stop_overrides$rel_se_p90_target <- rel_se_p90_target
  if (!missing(rel_se_p90_min_improve)) stop_overrides$rel_se_p90_min_improve <- rel_se_p90_min_improve
  if (!missing(max_item_misfit_prop)) stop_overrides$max_item_misfit_prop <- max_item_misfit_prop
  if (!missing(max_judge_misfit_prop)) stop_overrides$max_judge_misfit_prop <- max_judge_misfit_prop


  tier_params <- utils::modifyList(tier_params, stop_overrides)


  decision <- do.call(
    bt_should_stop,
    c(list(metrics = metrics, prev_metrics = prev_metrics), tier_params)
  )

  # If caller requests no new pairs, return empty next-pairs but still return metrics/decision.
  if (round_size == 0L) {
    pairs_next <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
    return(list(metrics = metrics, decision = decision, pairs_next = pairs_next))
  }

  # If stopping criteria met, propose no new pairs.
  if (isTRUE(decision$stop)) {
    pairs_next <- tibble::tibble(ID1 = character(), text1 = character(), ID2 = character(), text2 = character())
    return(list(metrics = metrics, decision = decision, pairs_next = pairs_next))
  }

  pairs_next <- select_adaptive_pairs(
    samples = samples,
    theta = fit$theta,
    existing_pairs = existing_pairs,
    embedding_neighbors = embedding_neighbors,
    n_pairs = round_size,
    k_neighbors = k_neighbors,
    min_judgments = min_judgments,
    repeat_policy = repeat_policy,
    repeat_cap = repeat_cap,
    repeat_frac = repeat_frac,
    repeat_n = repeat_n,
    repeat_guard_min_degree = repeat_guard_min_degree,
    repeat_guard_largest_component_frac = repeat_guard_largest_component_frac,
    forbid_repeats = forbid_repeats,
    balance_positions = balance_positions,
    embed_far_k = embed_far_k,
    embed_quota_frac = embed_quota_frac,
    candidate_pool_cap = candidate_pool_cap,
    per_anchor_cap = per_anchor_cap,
    w_embed = w_embed,
    embed_score_mode = embed_score_mode,
    seed = seed
  )

  list(metrics = metrics, decision = decision, pairs_next = pairs_next)
}
