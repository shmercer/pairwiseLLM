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
#'   \code{validate_results_tbl()} for required structure.
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
    seed = NULL
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

  samples <- tibble::tibble(
    ID = as.character(ids),
    text = rep("", length(ids))
  )

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
    fit_contract <- as_btl_fit_contract_from_mcmc(mcmc_fit, ids = ids)
    fits[[idx]] <- fit_contract

    state <- btl_mcmc_state_new(samples, config = list(), seed = seed)
    state$config$mcmc <- mcmc_config
    state$mode <- "standalone"

    ingest <- btl_mcmc_ingest_results_incremental(state, results_subset)
    state <- ingest$state
    state$comparisons_scheduled <- as.integer(nrow(results_subset))
    state$posterior$mcmc_config_used <- fit_contract$mcmc_config_used
    state$posterior$model_variant <- fit_contract$model_variant

    metrics <- btl_mcmc_fill_terminal_stop_metrics(state, mcmc_config)
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
