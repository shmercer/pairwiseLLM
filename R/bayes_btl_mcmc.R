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
#' Final Bayesian posterior inference for adaptive ranking, using the adaptive
#' v3 CmdStan machinery. The output mirrors adaptive logs and summaries so
#' downstream consumers can reuse the same schemas.
#'
#' @param results Canonical \\code{results_tbl} with \\code{A_id}, \\code{B_id}, and
#'   \\code{better_id} (plus the standard adaptive results columns).
#' @param ids Character vector of all sample ids (length \\code{N}).
#' @param model_variant Model variant label: \\code{"btl"}, \\code{"btl_e"},
#'   \\code{"btl_b"}, or \\code{"btl_e_b"}. Defaults to \\code{"btl_e_b"}.
#' @param cmdstan List of CmdStan settings: \\code{chains} (defaults to
#'   \\code{min(8, physical_cores)}), \\code{iter_warmup} (1000),
#'   \\code{iter_sampling} (1000), \\code{seed} (NULL),
#'   \\code{core_fraction} (0.8), and optional \\code{output_dir}.
#' @param pair_counts Optional integer vector of subset sizes (e.g.,
#'   \\code{c(200, 500, 1000)}). When provided, the model is fit once per subset
#'   size and the round log contains one row per fit.
#' @param subset_method Subset strategy when \\code{pair_counts} is provided:
#'   \\code{"first"} (default) or \\code{"sample"}.
#' @param seed Optional integer seed for deterministic subset selection. When
#'   \\code{NULL}, falls back to \\code{cmdstan$seed} if provided.
#'
#' @return A list with:
#' \\describe{
#'   \\item{item_summary}{Tibble matching the adaptive item log schema (one row per
#'   item per fit; \\code{refit_id} identifies the fit).}
#'   \\item{round_log}{Tibble matching the adaptive round log schema (one row per
#'   fit).}
#'   \\item{fits}{List of adaptive v3 fit contracts (one per fit).}
#'   \\item{fit}{Single fit contract (only when one fit is run).}
#' }
#'
#' @examples
#' \\dontrun{
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
#' fit <- fit_bayes_btl_mcmc(
#'   results,
#'   ids = c("A", "B"),
#'   model_variant = "btl_e_b"
#' )
#' fit$item_summary
#' fit$round_log
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

    bt_data <- .btl_mcmc_v3_prepare_bt_data(results_subset, ids)
    v3_config <- adaptive_v3_config(length(ids), list(
      model_variant = model_variant,
      cmdstan = cmdstan
    ))

    mcmc_fit <- .fit_bayes_btl_mcmc_adaptive(bt_data, config = v3_config, seed = mcmc_seed)
    fit_contract <- as_v3_fit_contract_from_mcmc(mcmc_fit, ids = ids)
    fits[[idx]] <- fit_contract

    state <- adaptive_state_new(samples, config = list(), seed = seed)
    state$config$v3 <- v3_config
    state$mode <- "standalone"

    ingest <- .adaptive_ingest_results_incremental(state, results_subset)
    state <- ingest$state
    state$comparisons_scheduled <- as.integer(nrow(results_subset))
    state$posterior$mcmc_config_used <- fit_contract$mcmc_config_used
    state$posterior$model_variant <- fit_contract$model_variant

    metrics <- .adaptive_fill_terminal_stop_metrics(state, v3_config)
    round_row <- build_round_log_row(
      state = state,
      fit = fit_contract,
      metrics = metrics,
      stop_out = list(stop_decision = NA, stop_reason = NA_character_),
      config = v3_config,
      round_id = as.integer(idx),
      batch_size = v3_config$batch_size,
      window_W = v3_config$W,
      exploration_rate = v3_config$explore_rate,
      new_pairs = as.integer(nrow(results_subset))
    )
    round_log <- dplyr::bind_rows(round_log, round_row)

    item_log <- build_item_log(state, fit = fit_contract)
    item_log$refit_id <- rep.int(as.integer(idx), nrow(item_log))
    item_log <- dplyr::relocate(item_log, refit_id, .before = 1L)
    item_log <- .adaptive_align_log_schema(item_log, .adaptive_item_log_schema())
    item_logs[[idx]] <- item_log
  }

  item_summary <- dplyr::bind_rows(item_logs)
  out <- list(
    item_summary = item_summary,
    round_log = round_log,
    fits = fits
  )
  if (length(fits) == 1L) {
    out$fit <- fits[[1L]]
  }
  out
}
