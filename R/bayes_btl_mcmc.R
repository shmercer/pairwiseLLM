# -------------------------------------------------------------------------
# Full Bayesian BTL inference via CmdStanR (adaptive-compatible).
# -------------------------------------------------------------------------

.btl_mcmc_require_cmdstanr <- function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    rlang::abort(paste0(
      "CmdStanR is required for MCMC audit. Install with:\n",
      "install.packages(\"cmdstanr\", repos = c(\"https://mc-stan.org/r-packages/\", getOption(\"repos\")))\n",
      "Then install CmdStan: cmdstanr::install_cmdstan()"
    ))
  }
  cmdstan_path <- tryCatch(
    cmdstanr::cmdstan_path(),
    error = function(e) ""
  )
  if (!nzchar(cmdstan_path)) {
    rlang::abort(
      "CmdStan is not available. Run: cmdstanr::install_cmdstan()"
    )
  }
  invisible(TRUE)
}

.btl_mcmc_validate_pair_counts <- function(pair_counts, n_pairs) {
  if (is.null(pair_counts)) {
    return(as.integer(n_pairs))
  }
  if (!is.numeric(pair_counts)) {
    rlang::abort("`pair_counts` must be an integer vector or NULL.")
  }
  pair_counts <- as.integer(pair_counts)
  if (length(pair_counts) < 1L || any(is.na(pair_counts))) {
    rlang::abort("`pair_counts` must be a non-empty integer vector.")
  }
  if (any(pair_counts < 1L)) {
    rlang::abort("`pair_counts` must contain positive integers.")
  }
  if (any(pair_counts > n_pairs)) {
    rlang::abort("`pair_counts` must be <= nrow(results).")
  }
  sort(unique(pair_counts))
}

.btl_mcmc_validate_seed <- function(seed, name) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    rlang::abort(paste0("`", name, "` must be a length-1 numeric seed or NULL."))
  }
  as.integer(seed)
}

#' Build canonical `results_tbl` data for Bayesian BTL MCMC
#'
#' Converts non-adaptive pairwise outcomes (for example, rows like
#' \code{example_writing_pairs} with \code{ID1}, \code{ID2}, \code{better_id})
#' into the canonical \code{results_tbl} schema required by
#' [fit_bayes_btl_mcmc()].
#'
#' The output is deterministic and schema-valid:
#' \itemize{
#'   \item stable \code{unordered_key} / \code{ordered_key} values,
#'   \item deterministic \code{pair_uid} as \code{"<unordered_key>#<occurrence>"},
#'   \item deterministic \code{iter} and \code{received_at} sequences.
#' }
#'
#' @param results A data frame or tibble containing columns \code{ID1},
#'   \code{ID2}, and \code{better_id}.
#' @param phase Length-1 phase label for all rows. Must be one of
#'   \code{"phase1"}, \code{"phase2"}, or \code{"phase3"}. Defaults to
#'   \code{"phase2"}.
#' @param backend Length-1 backend label to record in output metadata.
#' @param model Length-1 model label to record in output metadata.
#' @param iter_start Integer starting value for \code{iter}. Defaults to
#'   \code{1L}.
#' @param received_at_start Length-1 \code{POSIXct} timestamp for the first
#'   row. Subsequent rows increment by one second.
#'
#' @return A tibble in canonical \code{results_tbl} format with columns:
#'   \code{pair_uid}, \code{unordered_key}, \code{ordered_key}, \code{A_id},
#'   \code{B_id}, \code{better_id}, \code{winner_pos}, \code{phase},
#'   \code{iter}, \code{received_at}, \code{backend}, \code{model}.
#'
#' @examples
#' data("example_writing_pairs", package = "pairwiseLLM")
#'
#' results_tbl <- build_btl_results_data(example_writing_pairs)
#' head(results_tbl)
#'
#' ids <- sort(unique(c(results_tbl$A_id, results_tbl$B_id)))
#' ids
#'
#' @export
build_btl_results_data <- function(
    results,
    phase = "phase2",
    backend = "non_adaptive_import",
    model = "unknown",
    iter_start = 1L,
    received_at_start = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
) {
  if (!is.data.frame(results)) {
    rlang::abort("`results` must be a data frame or tibble.")
  }
  results <- tibble::as_tibble(results)

  required_cols <- c("ID1", "ID2", "better_id")
  .adaptive_required_cols(results, "results", required_cols)

  if (!is.character(phase) || length(phase) != 1L || is.na(phase) || !nzchar(phase)) {
    rlang::abort("`phase` must be a non-empty length-1 character value.")
  }
  .adaptive_check_phase(rep(phase, nrow(results)), "phase")

  if (!is.character(backend) || length(backend) != 1L || is.na(backend) || !nzchar(backend)) {
    rlang::abort("`backend` must be a non-empty length-1 character value.")
  }
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    rlang::abort("`model` must be a non-empty length-1 character value.")
  }

  if (!is.numeric(iter_start) || length(iter_start) != 1L || is.na(iter_start)) {
    rlang::abort("`iter_start` must be a length-1 numeric value.")
  }
  iter_start <- as.integer(iter_start)

  if (!inherits(received_at_start, "POSIXct") || length(received_at_start) != 1L || is.na(received_at_start)) {
    rlang::abort("`received_at_start` must be a non-missing length-1 POSIXct value.")
  }

  A_id <- as.character(results$ID1)
  B_id <- as.character(results$ID2)
  better_id <- as.character(results$better_id)

  if (any(is.na(A_id) | A_id == "")) {
    rlang::abort("`results$ID1` must not contain missing or empty values.")
  }
  if (any(is.na(B_id) | B_id == "")) {
    rlang::abort("`results$ID2` must not contain missing or empty values.")
  }
  if (any(A_id == B_id)) {
    rlang::abort("`results` must not contain self-pairs (`ID1 == ID2`).")
  }
  if (any(is.na(better_id) | better_id == "")) {
    rlang::abort("`results$better_id` must not contain missing or empty values.")
  }

  invalid_winner <- !(better_id == A_id | better_id == B_id)
  if (any(invalid_winner)) {
    rlang::abort("`results$better_id` must match `ID1` or `ID2` for every row.")
  }

  n <- nrow(results)
  unordered_key <- make_unordered_key(A_id, B_id)
  ordered_key <- make_ordered_key(A_id, B_id)

  occurrence <- if (n == 0L) {
    integer()
  } else {
    dplyr::tibble(unordered_key = unordered_key) |>
      dplyr::group_by(.data$unordered_key) |>
      dplyr::mutate(occurrence = dplyr::row_number()) |>
      dplyr::ungroup() |>
      dplyr::pull(.data$occurrence)
  }

  iter <- if (n == 0L) {
    integer()
  } else {
    as.integer(iter_start + seq_len(n) - 1L)
  }

  received_at <- if (n == 0L) {
    as.POSIXct(character(), tz = attr(received_at_start, "tzone", exact = TRUE) %||% "UTC")
  } else {
    received_at_start + as.difftime(seq_len(n) - 1L, units = "secs")
  }

  out <- tibble::tibble(
    pair_uid = paste0(unordered_key, "#", occurrence),
    unordered_key = unordered_key,
    ordered_key = ordered_key,
    A_id = A_id,
    B_id = B_id,
    better_id = better_id,
    winner_pos = as.integer(ifelse(better_id == A_id, 1L, 2L)),
    phase = rep(phase, n),
    iter = iter,
    received_at = received_at,
    backend = rep(backend, n),
    model = rep(model, n)
  )

  validate_results_tbl(out)
  out
}

# Build summary counts from observed results without relying on the legacy
# scaffold state constructor.
.btl_mcmc_summary_counts <- function(results, ids) {
  ids <- as.character(ids)
  deg <- stats::setNames(rep.int(0L, length(ids)), ids)
  pos1 <- deg
  pos2 <- deg

  if (nrow(results) == 0L) {
    keys <- character()
  } else {
    A_id <- as.character(results$A_id)
    B_id <- as.character(results$B_id)
    keep_key <- logical(nrow(results))
    for (idx in seq_len(nrow(results))) {
      A <- A_id[[idx]]
      B <- B_id[[idx]]
      if (!A %in% ids || !B %in% ids || identical(A, B)) {
        next
      }
      keep_key[[idx]] <- TRUE
      deg[[A]] <- deg[[A]] + 1L
      deg[[B]] <- deg[[B]] + 1L
      pos1[[A]] <- pos1[[A]] + 1L
      pos2[[B]] <- pos2[[B]] + 1L
    }
    keys <- make_unordered_key(A_id[keep_key], B_id[keep_key])
  }

  all_keys <- if (length(ids) > 1L) {
    combos <- utils::combn(ids, 2L)
    make_unordered_key(combos[1L, ], combos[2L, ])
  } else {
    character()
  }
  pair_count <- stats::setNames(rep.int(0L, length(all_keys)), all_keys)
  if (length(keys) > 0L) {
    key_tab <- table(keys)
    pair_count[names(key_tab)] <- as.integer(key_tab)
  }

  list(
    deg = deg,
    pos1 = pos1,
    pos2 = pos2,
    pair_count = pair_count
  )
}

.btl_mcmc_summary_state <- function(results, ids, config, fit_contract) {
  results <- tibble::as_tibble(results)
  counts <- .btl_mcmc_summary_counts(results, ids)
  iter_at_refit <- if (nrow(results) > 0L && "iter" %in% names(results)) {
    max(as.integer(results$iter), na.rm = TRUE)
  } else {
    0L
  }

  structure(
    list(
      ids = as.character(ids),
      N = as.integer(length(ids)),
      deg = counts$deg,
      pos1 = counts$pos1,
      pos2 = counts$pos2,
      imb = counts$pos1 - counts$pos2,
      pos_count = counts$pos1,
      pair_count = counts$pair_count,
      history_pairs = tibble::tibble(
        A_id = as.character(results$A_id %||% character()),
        B_id = as.character(results$B_id %||% character())
      ),
      comparisons_scheduled = as.integer(nrow(results)),
      comparisons_observed = as.integer(nrow(results)),
      iter = as.integer(iter_at_refit),
      mode = "standalone",
      posterior = list(
        epsilon_mean = as.double(fit_contract$epsilon_mean %||% NA_real_),
        stop_metrics = .adaptive_stop_metrics_defaults(),
        mcmc_config_used = fit_contract$mcmc_config_used %||% list(),
        model_variant = fit_contract$model_variant %||% NA_character_
      ),
      config = list(
        mcmc = config,
        round_log = round_log_schema()
      ),
      batch_log = batch_log_schema(),
      stop_reason = NA_character_
    ),
    class = "adaptive_state"
  )
}

.btl_mcmc_inference_contract_from_results <- function(results, inference_contract = NULL) {
  if (is.null(inference_contract)) {
    inference_contract <- list()
  }
  if (!is.list(inference_contract)) {
    rlang::abort("`inference_contract` must be a list or NULL.")
  }
  results <- tibble::as_tibble(results)

  phase_levels <- inference_contract$phase_levels %||% NULL
  if (is.null(phase_levels) && "phase" %in% names(results)) {
    phase_levels <- as.character(results$phase)
  }
  phase_levels <- as.character(phase_levels %||% character())
  phase_levels <- unique(phase_levels[!is.na(phase_levels) & phase_levels != ""])
  if (length(phase_levels) > 0L) {
    .adaptive_check_phase(phase_levels, "inference_contract$phase_levels")
  }

  judge_scope_levels <- inference_contract$judge_scope_levels %||% NULL
  if (is.null(judge_scope_levels) && "judge_scope" %in% names(results)) {
    judge_scope_levels <- as.character(results$judge_scope)
  }
  judge_scope_levels <- as.character(judge_scope_levels %||% character())
  judge_scope_levels <- unique(judge_scope_levels[!is.na(judge_scope_levels) & judge_scope_levels != ""])
  bad_scope <- setdiff(judge_scope_levels, c("shared", "within", "link"))
  if (length(bad_scope) > 0L) {
    rlang::abort("`inference_contract$judge_scope_levels` must be shared, within, or link.")
  }

  judge_param_mode <- inference_contract$judge_param_mode %||% NULL
  if (is.null(judge_param_mode)) {
    if (length(judge_scope_levels) == 0L || identical(judge_scope_levels, "shared")) {
      judge_param_mode <- "global_shared"
    } else {
      judge_param_mode <- "phase_specific"
    }
  }
  judge_param_mode <- as.character(judge_param_mode)
  if (length(judge_param_mode) != 1L || is.na(judge_param_mode) ||
    !judge_param_mode %in% c("global_shared", "phase_specific")) {
    rlang::abort("`inference_contract$judge_param_mode` must be global_shared or phase_specific.")
  }

  phase_boundary_detected <- inference_contract$phase_boundary_detected %||% NULL
  if (is.null(phase_boundary_detected)) {
    phase_boundary_detected <- any(phase_levels %in% c("phase3"))
  }
  phase_boundary_detected <- as.logical(phase_boundary_detected)
  if (length(phase_boundary_detected) != 1L || is.na(phase_boundary_detected)) {
    rlang::abort("`inference_contract$phase_boundary_detected` must be TRUE or FALSE.")
  }

  list(
    judge_param_mode = judge_param_mode,
    phase_levels = phase_levels,
    judge_scope_levels = judge_scope_levels,
    phase_boundary_detected = phase_boundary_detected
  )
}

#' Full Bayesian BTL inference via CmdStanR (adaptive-compatible)
#'
#' Runs full Bayesian posterior inference for a Bradley–Terry–Luce (BTL) style
#' model using the package’s CmdStan machinery, but in a standalone
#' (non-adaptive) context. The function is designed so downstream diagnostics
#' and reporting can reuse the existing adaptive summary tools (notably
#' [summarize_items()] and [summarize_refits()]) without requiring new summary
#' functions.
#'
#' Internally, the function can optionally refit the model on increasing subsets
#' of the observed comparisons (via \code{pair_counts}). Each refit is treated
#' as a "refit" in the adaptive logging sense, producing:
#' \itemize{
#'   \item one round-log row per refit (compatible with \code{round_log_schema()}),
#'   \item one item-log table per refit (compatible with \code{.adaptive_item_log_schema()}).
#' }
#'
#' @param results Canonical \code{results_tbl} with \code{A_id}, \code{B_id}, and
#'   \code{better_id} (plus the standard adaptive results columns). See
#'   \code{validate_results_tbl()} for required structure. For legacy
#'   \code{ID1}/\code{ID2}/\code{better_id} data, first use
#'   [build_btl_results_data()].
#' @param ids Character vector of all sample ids (length \code{N}).
#' @param model_variant Model variant label: \code{"btl"}, \code{"btl_e"},
#'   \code{"btl_b"}, or \code{"btl_e_b"}. Defaults to \code{"btl_e_b"}.
#' @param cmdstan List of CmdStan settings. Common fields:
#'   \describe{
#'     \item{chains}{Number of chains (defaults to \code{min(8, physical_cores)} via
#'       internal resolution).}
#'     \item{iter_warmup}{Warmup iterations (default \code{1000}).}
#'     \item{iter_sampling}{Sampling iterations (default \code{1000}).}
#'     \item{seed}{Optional integer seed forwarded to CmdStan (default \code{NULL}).}
#'     \item{core_fraction}{Fraction of physical cores for parallelization (default \code{0.8}).}
#'     \item{output_dir}{Optional directory for CmdStan output.}
#'   }
#' @param pair_counts Optional integer vector of subset sizes (e.g.,
#'   \code{c(200, 500, 1000)}). When provided, the model is fit once per subset
#'   size and the round log contains one row per fit. If \code{NULL}, a single
#'   fit is run using all rows in \code{results}.
#' @param subset_method Subset strategy when \code{pair_counts} is provided:
#'   \code{"first"} (default) uses the first \code{n} rows of \code{results} for
#'   each refit; \code{"sample"} draws a random permutation once and then takes
#'   the first \code{n} rows of that permutation for each refit.
#' @param seed Optional integer seed for deterministic subset selection when
#'   \code{subset_method = "sample"}. When \code{NULL}, falls back to
#'   \code{cmdstan$seed} if provided.
#' @param inference_contract Optional list of inference-routing semantics to
#'   attach to each fit contract. When omitted, values are inferred from
#'   \code{results$phase} and optional \code{results$judge_scope}.
#'
#' @return A list with:
#' \describe{
#'   \item{item_log_list}{List of item-log tables, one per refit, matching the
#'     canonical adaptive item log schema. This is the preferred structure for
#'     reuse with [summarize_items()].}
#'   \item{item_summary}{A single tibble formed by row-binding \code{item_log_list}
#'     (kept for backward compatibility). Each row corresponds to an item within
#'     a refit; \code{refit_id} identifies the refit.}
#'   \item{round_log}{Tibble matching the canonical adaptive round log schema
#'     (one row per refit).}
#'   \item{fits}{List of BTL fit contracts (one per refit).}
#'   \item{fit}{Single fit contract (only when one refit is run).}
#' }
#'
#' @examples
#' \dontrun{
#' results <- tibble::tibble(
#'   pair_uid = "A:B#1",
#'   unordered_key = "A:B",
#'   ordered_key = "A:B",
#'   A_id = "A",
#'   B_id = "B",
#'   better_id = "A",
#'   winner_pos = 1L,
#'   phase = "phase2",
#'   iter = 1L,
#'   received_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
#'   backend = "openai",
#'   model = "gpt-test"
#' )
#'
#' fit <- fit_bayes_btl_mcmc(
#'   results,
#'   ids = c("A", "B"),
#'   model_variant = "btl_e_b"
#' )
#'
#' # Generate summaries
#' summarize_refits(fit)
#' summarize_items(fit)
#' }
#'
#' @export
fit_bayes_btl_mcmc <- function(
    results,
    ids,
    model_variant = "btl_e_b",
    cmdstan = list(
      iter_warmup = 1000,
      iter_sampling = 1000,
      seed = NULL,
      core_fraction = 0.8
    ),
    pair_counts = NULL,
    subset_method = c("first", "sample"),
    seed = NULL,
    inference_contract = NULL
) {
  if (is.list(model_variant) && !is.character(model_variant)) {
    cmdstan <- model_variant
    model_variant <- "btl_e_b"
  }
  results <- tibble::as_tibble(results)
  validate_results_tbl(results)

  ids <- .btl_validate_ids(ids)
  model_variant <- normalize_model_variant(model_variant %||% "btl_e_b")

  cmdstan <- cmdstan %||% list()
  if (!is.list(cmdstan)) {
    rlang::abort("`cmdstan` must be a list.")
  }
  output_dir <- cmdstan$output_dir %||% NULL
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
      rlang::abort("`cmdstan$output_dir` must be a length-1 character path.")
    }
  }
  resolved_cmdstan <- .btl_mcmc_resolve_cmdstan_config(cmdstan)
  if (is.na(resolved_cmdstan$parallel_chains) || resolved_cmdstan$parallel_chains < 1L) {
    rlang::abort("`cmdstan$parallel_chains` must be a positive integer.")
  }

  subset_method <- match.arg(subset_method)
  total_pairs <- nrow(results)
  if (total_pairs < 1L) {
    rlang::abort("`results` must contain at least one row.")
  }
  pair_counts <- .btl_mcmc_validate_pair_counts(pair_counts, total_pairs)

  selection_seed <- .btl_mcmc_validate_seed(seed %||% cmdstan$seed %||% NULL, "seed")
  mcmc_seed <- .btl_mcmc_validate_seed(cmdstan$seed %||% seed %||% NULL, "cmdstan$seed")

  perm <- seq_len(total_pairs)
  if (identical(subset_method, "sample")) {
    perm <- .pairwiseLLM_with_seed(selection_seed, function() {
      sample(perm, size = length(perm), replace = FALSE)
    })
  }

  fits <- vector("list", length(pair_counts))
  item_logs <- vector("list", length(pair_counts))
  round_log <- round_log_schema()

  for (idx in seq_along(pair_counts)) {
    n_pairs <- pair_counts[[idx]]
    subset_idx <- perm[seq_len(n_pairs)]
    results_subset <- results[subset_idx, , drop = FALSE]

    bt_data <- .btl_mcmc_prepare_bt_data(results_subset, ids)
    mcmc_config <- btl_mcmc_config(length(ids), list(
      model_variant = model_variant,
      cmdstan = cmdstan
    ))

    mcmc_fit <- .fit_bayes_btl_mcmc_adaptive(bt_data, config = mcmc_config, seed = mcmc_seed)
    contract_meta <- .btl_mcmc_inference_contract_from_results(
      results_subset,
      inference_contract = inference_contract
    )
    fit_contract <- as_btl_fit_contract_from_mcmc(mcmc_fit, ids = ids)
    fit_contract$inference_contract <- contract_meta
    validate_btl_fit_contract(fit_contract, ids = ids)
    fits[[idx]] <- fit_contract

    state <- .btl_mcmc_summary_state(
      results = results_subset,
      ids = ids,
      config = mcmc_config,
      fit_contract = fit_contract
    )

    metrics <- btl_mcmc_fill_terminal_stop_metrics(state, mcmc_config)
    metrics$proposed_pairs <- as.integer(nrow(results_subset))
    round_row <- build_round_log_row(
      state = state,
      fit = fit_contract,
      metrics = metrics,
      stop_out = list(stop_decision = NA, stop_reason = NA_character_),
      config = mcmc_config,
      round_id = as.integer(idx),
      batch_size = mcmc_config$batch_size,
      window_W = mcmc_config$W,
      exploration_rate = mcmc_config$explore_rate,
      new_pairs = as.integer(nrow(results_subset))
    )
    round_log <- dplyr::bind_rows(round_log, round_row)

    item_log <- build_item_log(state, fit = fit_contract)
    item_log$refit_id <- rep.int(as.integer(idx), nrow(item_log))
    item_log <- dplyr::relocate(item_log, "refit_id", .before = 1L)
    item_log <- .adaptive_align_log_schema(item_log, .adaptive_item_log_schema())
    item_logs[[idx]] <- item_log
  }

  item_summary <- dplyr::bind_rows(item_logs)

  out <- list(
    item_log_list = item_logs,
    item_summary = item_summary,
    round_log = round_log,
    fits = fits
  )
  if (length(fits) == 1L) {
    out$fit <- fits[[1L]]
  }
  out
}
