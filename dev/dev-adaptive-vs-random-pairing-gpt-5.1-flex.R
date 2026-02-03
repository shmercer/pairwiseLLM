suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  rlang::abort("OPENAI_API_KEY is not set. Please set it before running this script.")
}

# -------------------------------------------------------------------------
# Dev: Adaptive pairing vs random pairing (OpenAI GPT-5.1, service_tier = flex)
#
# Goal:
# - Run a live adaptive session on `example_writing_samples` (N = 20).
# - Run a random-pairing session that collects the *same number* of completed
#   judgments as the adaptive run uses at each refit.
# - Fit Bayesian BTL (MCMC) for the random run at the same pair counts, compute
#   the canonical stop metrics, and store round logs for both.
# - Generate one figure per stop metric (by pair count; lagged metrics shown
#   only once eligible).
#
# Notes:
# - This script makes live API calls. Ensure your OpenAI key is set (e.g.
#   Sys.setenv(OPENAI_API_KEY = "...")).
# - CmdStanR + CmdStan must be installed for MCMC fitting.
# -------------------------------------------------------------------------

# ---- Config ----
model <- "gpt-5.1"
service_tier <- "flex" # Alternative: "priority"
endpoint <- "responses"
reasoning <- "low"

seed <- 123L
workers <- 8L

adaptive_cfg <- list(
  eap_reliability_min = 0.85,
  theta_corr_min = 0.90,
  theta_sd_rel_change_max = 0.20,
  rank_spearman_min = 0.90
)

resume_run_tag <- Sys.getenv("PAIRWISELLM_RESUME_RUN_TAG", unset = "")
run_tag <- if (nzchar(resume_run_tag)) resume_run_tag else format(Sys.time(), "%Y%m%d-%H%M%S")
base_dir <- file.path("dev-output", paste0("adaptive_vs_random_", model, "_", service_tier), run_tag)
adaptive_dir <- file.path(base_dir, "adaptive")
random_dir <- file.path(base_dir, "random")
fig_dir <- file.path(base_dir, "figures")

dir.create(adaptive_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(random_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

paths_adaptive <- list(
  state_path = file.path(adaptive_dir, "state.rds"),
  output_dir = adaptive_dir
)

paths_random <- list(
  results_path = file.path(random_dir, "results.rds"),
  failed_attempts_path = file.path(random_dir, "failed_attempts.rds"),
  pairs_pool_path = file.path(random_dir, "pairs_pool.rds"),
  pairs_submitted_path = file.path(random_dir, "pairs_submitted.rds"),
  fit_path = file.path(random_dir, "fit_bayes_btl_mcmc.rds"),
  round_log_path = file.path(random_dir, "round_log.rds")
)

paths_compare <- list(
  adaptive_round_log_path = file.path(adaptive_dir, "round_log.rds"),
  combined_round_log_path = file.path(base_dir, "round_log_combined.rds"),
  combined_round_log_csv = file.path(base_dir, "round_log_combined.csv")
)

make_unordered_key <- function(id1, id2) {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  paste(pmin(id1, id2), pmax(id1, id2), sep = ":")
}

make_ordered_key <- function(A_id, B_id) {
  paste(as.character(A_id), as.character(B_id), sep = ":")
}

or_else <- function(x, y) {
  if (is.null(x)) y else x
}

load_or <- function(path, build_fn) {
  if (file.exists(path)) {
    return(readRDS(path))
  }
  obj <- build_fn()
  saveRDS(obj, path)
  obj
}

# ---- Data ----
samples_from_state <- function(state) {
  if (is.null(state$ids) || is.null(state$texts)) {
    rlang::abort("Checkpoint state is missing `ids` or `texts`.")
  }
  tibble::tibble(
    ID = as.character(state$ids),
    text = as.character(unname(state$texts))
  )
}

resume_out_path <- file.path(adaptive_dir, "out.rds")
resume_state_path <- paths_adaptive$state_path

samples <- NULL
if (nzchar(resume_run_tag) && (file.exists(resume_out_path) || file.exists(resume_state_path))) {
  st <- if (file.exists(resume_out_path)) {
    readRDS(resume_out_path)$state
  } else {
    readRDS(resume_state_path)
  }
  samples <- samples_from_state(st)
  message("Resuming with checkpoint items (N = ", nrow(samples), ") from: ", base_dir)
} else {
  data("example_writing_samples", package = "pairwiseLLM")
  samples <- example_writing_samples |>
    dplyr::select(.data$ID, text = .data$text) |>
    dplyr::mutate(ID = as.character(.data$ID))
}

td <- trait_description("overall_quality")

# ---- Adaptive live run ----
adaptive_out <- load_or(
  file.path(adaptive_dir, "out.rds"),
  function() {
    adaptive_rank_run_live(
      samples = samples,
      model = model,
      trait_name = td$name,
      trait_description = td$description,
      backend = "openai",
      seed = seed,
      submission = list(
        parallel = TRUE,
        workers = workers,
        progress = TRUE,
        verbose = TRUE,
        endpoint = endpoint,
        reasoning = reasoning,
        service_tier = service_tier
      ),
      adaptive = adaptive_cfg,
      paths = paths_adaptive
    )
  }
)

adaptive_round_log <- tibble::as_tibble(or_else(adaptive_out$state$config$round_log, tibble::tibble()))
saveRDS(adaptive_round_log, paths_compare$adaptive_round_log_path)

pair_counts <- adaptive_round_log |>
  dplyr::filter(!is.na(.data$completed_pairs)) |>
  dplyr::distinct(.data$completed_pairs) |>
  dplyr::arrange(.data$completed_pairs) |>
  dplyr::pull(.data$completed_pairs) |>
  as.integer()

if (length(pair_counts) < 1L) {
  rlang::abort("Adaptive run produced no refits with `completed_pairs`.")
}

target_pairs <- max(pair_counts)

v3_config <- or_else(adaptive_out$state$config$v3, adaptive_v3_config(nrow(samples), adaptive_cfg))
model_variant <- or_else(v3_config$model_variant, "btl_e_b")

# ---- Random pairing: collect live judgments (with replacement pairs if needed) ----
build_random_pairs_pool <- function(samples, seed) {
  # Random baseline pairs are sampled WITHOUT replacement from the set of all
  # unordered pairs. Each unordered pair appears exactly once in the pool.
  #
  # (We still randomize presentation order A/B once per unordered pair.)
  ids <- as.character(samples$ID)
  comb <- utils::combn(ids, 2)
  base <- tibble::tibble(
    i_id = comb[1, ],
    j_id = comb[2, ]
  ) |>
    dplyr::mutate(unordered_key = make_unordered_key(.data$i_id, .data$j_id))

  set.seed(seed)
  order_flag <- sample(c(TRUE, FALSE), size = nrow(base), replace = TRUE)

  pool <- base |>
    dplyr::mutate(
      A_id = dplyr::if_else(order_flag, .data$i_id, .data$j_id),
      B_id = dplyr::if_else(order_flag, .data$j_id, .data$i_id),
      ordered_key = make_ordered_key(.data$A_id, .data$B_id),
      pair_uid = paste0(.data$unordered_key, "#1")
    ) |>
    dplyr::select(.data$pair_uid, .data$unordered_key, .data$ordered_key, .data$A_id, .data$B_id)

  set.seed(seed + 1L)
  pool$.shuffle <- sample.int(nrow(pool), size = nrow(pool), replace = FALSE)
  pool <- pool |>
    dplyr::arrange(.data$.shuffle) |>
    dplyr::select(-.data$.shuffle)

  pool
}

pairs_pool <- load_or(paths_random$pairs_pool_path, function() {
  build_random_pairs_pool(samples, seed = seed)
})

build_submit_pairs_tbl <- function(pairs_meta, samples) {
  s <- samples |>
    dplyr::select(.data$ID, .data$text) |>
    dplyr::mutate(ID = as.character(.data$ID))

  pairs_meta |>
    dplyr::left_join(s, by = c("A_id" = "ID")) |>
    dplyr::rename(text1 = .data$text) |>
    dplyr::left_join(s, by = c("B_id" = "ID")) |>
    dplyr::rename(text2 = .data$text) |>
    dplyr::transmute(
      ID1 = as.character(.data$A_id),
      text1 = as.character(.data$text1),
      ID2 = as.character(.data$B_id),
      text2 = as.character(.data$text2),
      pair_uid = as.character(.data$pair_uid),
      phase = "phase2",
      iter = 1L
    )
}

collect_random_results <- function(target_pairs, pairs_pool, samples) {
  results <- if (file.exists(paths_random$results_path)) readRDS(paths_random$results_path) else tibble::tibble()
  failed_attempts <- if (file.exists(paths_random$failed_attempts_path)) readRDS(paths_random$failed_attempts_path) else tibble::tibble()
  pairs_submitted <- if (file.exists(paths_random$pairs_submitted_path)) readRDS(paths_random$pairs_submitted_path) else tibble::tibble()

  results <- tibble::as_tibble(results)
  pairs_submitted <- tibble::as_tibble(pairs_submitted)
  failed_attempts <- tibble::as_tibble(failed_attempts)

  next_idx <- nrow(pairs_submitted) + 1L

  while (nrow(results) < target_pairs) {
    remaining <- target_pairs - nrow(results)
    buffer <- max(5L, as.integer(ceiling(0.10 * remaining)))
    request_n <- min(nrow(pairs_pool) - next_idx + 1L, remaining + buffer)

    if (request_n < 1L) {
      rlang::abort(paste0(
        "Random pairing pool exhausted before reaching target. ",
        "Needed ", target_pairs, ", got ", nrow(results), "."
      ))
    }

    batch_meta <- pairs_pool[next_idx:(next_idx + request_n - 1L), , drop = FALSE]
    next_idx <- next_idx + request_n

    submit_pairs <- build_submit_pairs_tbl(batch_meta, samples)

    res <- submit_llm_pairs(
      pairs = submit_pairs,
      model = model,
      trait_name = td$name,
      trait_description = td$description,
      backend = "openai",
      verbose = TRUE,
      progress = TRUE,
      parallel = TRUE,
      workers = workers,
      endpoint = endpoint,
      reasoning = reasoning,
      service_tier = service_tier
    )

    results <- dplyr::bind_rows(results, or_else(res$results, tibble::tibble()))
    failed_attempts <- dplyr::bind_rows(failed_attempts, or_else(res$failed_attempts, tibble::tibble()))
    pairs_submitted <- dplyr::bind_rows(pairs_submitted, submit_pairs)

    # Persist after each chunk (supports resuming partial runs).
    saveRDS(results, paths_random$results_path)
    saveRDS(failed_attempts, paths_random$failed_attempts_path)
    saveRDS(pairs_submitted, paths_random$pairs_submitted_path)
  }

  # Keep the first `target_pairs` completed judgments (preserves submission order).
  results <- results[seq_len(target_pairs), , drop = FALSE]
  saveRDS(results, paths_random$results_path)

  list(
    results = results,
    failed_attempts = failed_attempts,
    pairs_submitted = pairs_submitted
  )
}

random_collect <- collect_random_results(
  target_pairs = target_pairs,
  pairs_pool = pairs_pool,
  samples = samples
)

random_results <- tibble::as_tibble(random_collect$results)
pairwiseLLM:::validate_results_tbl(random_results)
missing_ids <- setdiff(
  unique(c(random_results$A_id, random_results$B_id)),
  as.character(samples$ID)
)
if (length(missing_ids) > 0L) {
  rlang::abort(paste0(
    "Random results contain item IDs not present in `samples`. ",
    "This usually means you resumed into the wrong run folder or changed datasets.\n",
    "Missing IDs (showing up to 8): ",
    paste(utils::head(missing_ids, 8), collapse = ", "), "\n",
    "Fix: set PAIRWISELLM_RESUME_RUN_TAG to the run folder that created these results, ",
    "or unset it to start a new run."
  ))
}

# ---- Random pairing: refit BTL at adaptive pair counts (MCMC) ----
.btl_mcmc_require_cmdstanr <- getFromNamespace(".btl_mcmc_require_cmdstanr", "pairwiseLLM")
.btl_mcmc_require_cmdstanr()

random_fit <- load_or(paths_random$fit_path, function() {
  fit_bayes_btl_mcmc(
    results = random_results,
    ids = samples$ID,
    model_variant = model_variant,
    cmdstan = or_else(v3_config$cmdstan, list()),
    pair_counts = pair_counts,
    subset_method = "first",
    seed = seed
  )
})

compute_round_log_from_refits <- function(samples, results, pair_counts, fits, v3_config, seed) {
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(), seed = seed)
  state$config$v3 <- v3_config
  # Must match the validated adaptive state contract.
  # We keep the method label ("random") outside the state in the combined logs.
  state$mode <- "adaptive"
  state$phase <- "phase2"
  state$posterior$theta_mean_history <- list()

  text_map <- stats::setNames(as.character(samples$text), as.character(samples$ID))

  round_rows <- vector("list", length(pair_counts))
  prev_n <- 0L

  for (idx in seq_along(pair_counts)) {
    n_pairs <- as.integer(pair_counts[[idx]])
    new_idx <- (prev_n + 1L):n_pairs
    if (length(new_idx) > 0L) {
      ingest <- pairwiseLLM:::.adaptive_ingest_results_incremental(state, results[new_idx, , drop = FALSE])
      state <- ingest$state

      new_results <- tibble::as_tibble(ingest$new_results)
      if (nrow(new_results) > 0L) {
        A_id <- as.character(new_results$A_id)
        B_id <- as.character(new_results$B_id)
        A_text <- unname(text_map[A_id])
        B_text <- unname(text_map[B_id])

        if (any(is.na(A_text)) || any(is.na(B_text))) {
          missing <- unique(c(A_id[is.na(A_text)], B_id[is.na(B_text)]))
          rlang::abort(paste0(
            "Cannot reconstruct `history_pairs`: missing texts for some IDs.\n",
            "Missing IDs (showing up to 8): ",
            paste(utils::head(missing, 8), collapse = ", ")
          ))
        }

        pairs_rows <- pairwiseLLM:::as_pairs_tbl(
          pair_uid = as.character(new_results$pair_uid),
          unordered_key = as.character(new_results$unordered_key),
          ordered_key = as.character(new_results$ordered_key),
          A_id = A_id,
          B_id = B_id,
          A_text = as.character(A_text),
          B_text = as.character(B_text),
          phase = as.character(new_results$phase),
          iter = as.integer(new_results$iter),
          created_at = as.POSIXct(new_results$received_at, tz = "UTC"),
          backend = as.character(new_results$backend),
          model = as.character(new_results$model)
        )

        state$history_pairs <- dplyr::bind_rows(state$history_pairs, pairs_rows)
        state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))
      }
    }

    state$comparisons_scheduled <- as.integer(nrow(state$history_pairs))

    fit <- fits[[idx]]
    state$fit <- fit
    state$posterior$model_variant <- fit$model_variant
    state$posterior$mcmc_config_used <- fit$mcmc_config_used

    state$posterior$diagnostics_pass <- pairwiseLLM:::diagnostics_gate(
      fit,
      v3_config,
      near_stop = pairwiseLLM:::near_stop_from_state(state)
    )

    stop_metrics <- pairwiseLLM:::compute_stop_metrics(
      state = state,
      fit = fit,
      candidates_with_utility = tibble::tibble(),
      config = v3_config
    )
    stop_tmp <- pairwiseLLM:::should_stop(
      metrics = stop_metrics,
      state = state,
      config = v3_config,
      fit = fit
    )

    round_row <- pairwiseLLM:::build_round_log_row(
      state = state,
      fit = fit,
      metrics = stop_metrics,
      stop_out = list(
        stop_decision = or_else(stop_tmp$stop_decision, NA),
        stop_reason = or_else(stop_tmp$stop_reason, NA_character_)
      ),
      config = v3_config,
      round_id = as.integer(idx),
      batch_size = v3_config$batch_size,
      window_W = v3_config$W,
      exploration_rate = v3_config$explore_rate,
      new_pairs = as.integer(n_pairs - prev_n)
    )

    round_rows[[idx]] <- round_row
    # Align with adaptive's lag logic: the live adaptive wrapper's initial refit
    # does not append to `theta_mean_history`, so lag eligibility starts one
    # refit later than "round_id > stability_lag".
    if (idx > 1L) {
      state <- pairwiseLLM:::.adaptive_update_theta_history(state, fit = fit)
    }
    prev_n <- n_pairs
  }

  dplyr::bind_rows(round_rows)
}

random_round_log <- compute_round_log_from_refits(
  samples = samples,
  results = random_results,
  pair_counts = pair_counts,
  fits = random_fit$fits,
  v3_config = v3_config,
  seed = seed
)

saveRDS(random_round_log, paths_random$round_log_path)

# ---- Combine logs for comparison ----
adaptive_round_log_cmp <- adaptive_round_log |>
  dplyr::mutate(
    method = "adaptive",
    run_tag = run_tag,
    pair_count = as.integer(.data$completed_pairs)
  )

random_round_log_cmp <- random_round_log |>
  dplyr::mutate(
    method = "random",
    run_tag = run_tag,
    pair_count = as.integer(.data$completed_pairs)
  )

round_log_combined <- dplyr::bind_rows(adaptive_round_log_cmp, random_round_log_cmp) |>
  dplyr::arrange(.data$pair_count, .data$method)

saveRDS(round_log_combined, paths_compare$combined_round_log_path)
utils::write.csv(round_log_combined, paths_compare$combined_round_log_csv, row.names = FALSE)

# ---- Figures: one per stop metric ----
metric_specs <- tibble::tibble(
  metric = c(
    "diagnostics_pass",
    "reliability_EAP",
    "eap_pass",
    "rho_theta_lag",
    "theta_corr_pass",
    "delta_sd_theta_lag",
    "delta_sd_theta_pass",
    "rho_rank_lag",
    "rho_rank_pass",
    "rank_stability_pass",
    "stop_decision",
    "divergences",
    "max_rhat",
    "min_ess_bulk"
  ),
  kind = c(
    "logical",
    "numeric",
    "logical",
    "numeric_lag",
    "logical_lag",
    "numeric_lag",
    "logical_lag",
    "numeric_lag",
    "logical_lag",
    "logical_lag",
    "logical",
    "numeric",
    "numeric",
    "numeric"
  )
) |>
  dplyr::mutate(
    threshold = dplyr::case_when(
      .data$metric == "reliability_EAP" ~ as.double(or_else(v3_config$eap_reliability_min, NA_real_)),
      .data$metric == "rho_theta_lag" ~ as.double(or_else(v3_config$theta_corr_min, NA_real_)),
      .data$metric == "delta_sd_theta_lag" ~ as.double(or_else(v3_config$theta_sd_rel_change_max, NA_real_)),
      .data$metric == "rho_rank_lag" ~ as.double(or_else(v3_config$rank_spearman_min, NA_real_)),
      TRUE ~ NA_real_
    )
  )

plot_metric <- function(df, metric, kind, threshold = NA_real_) {
  d <- df |>
    dplyr::select(.data$method, .data$pair_count, .data$lag_eligible, value = dplyr::all_of(metric)) |>
    dplyr::filter(!is.na(.data$pair_count))

  if (kind %in% c("numeric_lag", "logical_lag")) {
    d <- dplyr::filter(d, .data$lag_eligible %in% TRUE)
  }

  if (kind %in% c("logical", "logical_lag")) {
    d <- dplyr::mutate(
      d,
      value = dplyr::case_when(
        is.na(.data$value) ~ NA_integer_,
        .data$value ~ 1L,
        TRUE ~ 0L
      )
    )
  }

  d <- dplyr::filter(d, !is.na(.data$value))

  p <- ggplot(d, aes(x = .data$pair_count, y = .data$value, color = .data$method)) +
    geom_line() +
    geom_point() +
    labs(
      title = metric,
      x = "Completed pairs (BTL refit count)",
      y = metric,
      color = NULL
    ) +
    theme_minimal(base_size = 12)

  if (is.finite(threshold)) {
    p <- p + geom_hline(yintercept = threshold, linetype = "dashed", color = "grey40")
  }

  if (kind %in% c("logical", "logical_lag")) {
    p <- p +
      scale_y_continuous(breaks = c(0, 1), limits = c(0, 1))
  }

  out_path <- file.path(fig_dir, paste0("stop_metric_", metric, ".png"))
  ggsave(out_path, plot = p, width = 8, height = 4.5, dpi = 150)
  invisible(out_path)
}

walk2(
  metric_specs$metric,
  seq_len(nrow(metric_specs)),
  function(m, i) {
    plot_metric(
      df = round_log_combined,
      metric = m,
      kind = metric_specs$kind[[i]],
      threshold = metric_specs$threshold[[i]]
    )
  }
)

message("Done. Outputs written under: ", base_dir)
