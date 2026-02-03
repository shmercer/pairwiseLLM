# dev/adaptive_vs_degree_balanced_random_10000pairs_N1000.R
#
# pairwiseLLM 1.2.0 — Adaptive vs Degree-Balanced Random (LIVE LLM calls)
# -----------------------------------------------------------------------
# Goal
# ----
# Collect EXACTLY 10,000 completed pairwise judgments for each mode:
#   (1) adaptive pairing (v3 engine; batch_size = 100; refit_B = 100)
#   (2) degree-balanced random pairing (non-utility baseline; exposure-balanced)
#
# Then:
#   - Refit Bayesian BTL (MCMC) for BOTH modes at identical intervals:
#       pair_counts = 100, 200, ..., 10,000
#   - Compute canonical stop/stability metrics (the same fields as adaptive round_log)
#   - Add a few extra, easy-to-interpret diagnostics per refit:
#       max_degree, sd_degree, frac_zero_degree, degree_gini, unique_pair_rate
#   - Save artifacts continuously so the run is resumable and debuggable.
#
# Resumability / Postmortem artifacts
# ----------------------------------
# Adaptive:
#   - state.rds is checkpointed by adaptive_rank_resume() each step.
#   - we save first-10k results + pairs to RDS for later debugging.
# Random:
#   - results.rds, failed_attempts.rds, pairs_submitted.rds are saved after every chunk.
#   - pairs_completed.rds is saved at the end (pairs corresponding to the kept 10k results).
#
# Package source verification (pairwiseLLM 1.2.0 tarball)
# -------------------------------------------------------
# - build_round_log_row() already includes:
#     mean_degree, min_degree, n_unique_pairs_seen, scheduled_pairs, completed_pairs,
#     pos_balance_mean/sd, starve_rate_since_last_refit, fallback_rate_since_last_refit, etc.
#   (see R/adaptive_contracts.R)
# - v3 config fields batch_size, refit_B, W, explore_rate exist and are validated
#   (see R/adaptive_contracts.R)
# - compute_stop_metrics(), should_stop(), diagnostics_gate() exist
#   (see R/adaptive_stopping.R, R/adaptive_diagnostics.R)
#
# -----------------------------------------------------------------------

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

`%||%` <- function(x, y) if (is.null(x)) y else x

if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  rlang::abort("OPENAI_API_KEY is not set. Please set it before running this script.")
}

# -----------------------------
# Config
# -----------------------------
model <- "gpt-5.1"
service_tier <- "flex"
endpoint <- "responses"
reasoning <- "none"

seed <- 123L
workers_adaptive <- 8L
workers_random <- 8L

target_pairs <- 10000L
refit_interval <- 100L
pair_counts <- seq.int(refit_interval, target_pairs, by = refit_interval)

td <- trait_description("overall_quality")

# -------------------------------------------------------------------------
# Adaptive v3 overrides for this study
# -------------------------------------------------------------------------
# We want:
#   - batch_size = 100 (pairs scheduled per iteration target)
#   - refit_B    = 100 (refit after each 100 new completed pairs)
#
# To avoid early stopping in a fixed-budget comparison, we set extremely strict
# stop thresholds that are valid under validate_config() but practically impossible
# to satisfy exactly:
#   eap_reliability_min     = 1
#   theta_corr_min          = 1
#   rank_spearman_min       = 1
#   theta_sd_rel_change_max = 0
adaptive_v3_overrides <- list(
  batch_size = as.integer(refit_interval),
  refit_B = as.integer(refit_interval),
  eap_reliability_min = 1,
  theta_corr_min = 1,
  rank_spearman_min = 1,
  theta_sd_rel_change_max = 0
)

adaptive_cfg <- list(
  v3 = adaptive_v3_overrides
)

# -----------------------------
# Run folder layout (resumable)
# -----------------------------
resume_run_tag <- Sys.getenv("PAIRWISELLM_RESUME_RUN_TAG", unset = "")
run_tag <- if (nzchar(resume_run_tag)) resume_run_tag else format(Sys.time(), "%Y%m%d-%H%M%S")

base_dir <- file.path(
  "dev-output",
  paste0("adaptive_vs_db_random_", model, "_", service_tier, "_N1000"),
  run_tag
)
adaptive_dir <- file.path(base_dir, "adaptive")
random_dir <- file.path(base_dir, "random_degree_balanced")
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
  pairs_submitted_path = file.path(random_dir, "pairs_submitted.rds"),
  fit_path = file.path(random_dir, "fit_bayes_btl_mcmc.rds"),
  round_log_path = file.path(random_dir, "round_log_refit100.rds"),
  pairs_completed_path = file.path(random_dir, "pairs_completed.rds")
)

paths_compare <- list(
  adaptive_results_path = file.path(adaptive_dir, "results_first10000.rds"),
  adaptive_pairs_path = file.path(adaptive_dir, "pairs_first10000.rds"),
  adaptive_round_log_path = file.path(adaptive_dir, "round_log_refit100.rds"),
  combined_round_log_path = file.path(base_dir, "round_log_combined_refit100.rds"),
  combined_round_log_csv = file.path(base_dir, "round_log_combined_refit100.csv")
)

# -----------------------------
# Small utilities
# -----------------------------
or_else <- function(x, y) if (is.null(x)) y else x

load_or <- function(path, build_fn) {
  if (file.exists(path)) return(readRDS(path))
  obj <- build_fn()
  saveRDS(obj, path)
  obj
}

make_unordered_key <- function(id1, id2) {
  id1 <- as.character(id1)
  id2 <- as.character(id2)
  paste(pmin(id1, id2), pmax(id1, id2), sep = ":")
}

make_ordered_key <- function(A_id, B_id) {
  paste(as.character(A_id), as.character(B_id), sep = ":")
}

# -----------------------------
# Data (N = 1000) + resume guard
# -----------------------------
data("example_writing_samples1000", package = "pairwiseLLM")

samples <- example_writing_samples1000 |>
  dplyr::select(.data$ID, text = .data$text) |>
  dplyr::mutate(ID = as.character(.data$ID))

# If resuming, ensure the checkpoint corresponds to the SAME dataset.
if (nzchar(resume_run_tag) && file.exists(paths_adaptive$state_path)) {
  st <- readRDS(paths_adaptive$state_path)

  if (is.null(st$ids) || is.null(st$texts)) {
    rlang::abort("Adaptive checkpoint is missing `ids` or `texts`.")
  }

  ids_ckpt <- as.character(st$ids)
  ids_data <- as.character(samples$ID)

  if (!setequal(ids_ckpt, ids_data)) {
    only_in_ckpt <- setdiff(ids_ckpt, ids_data)
    only_in_data <- setdiff(ids_data, ids_ckpt)

    rlang::abort(paste0(
      "Refusing to resume: checkpoint item IDs do not match the loaded dataset.\n",
      "Loaded dataset: example_writing_samples1000 (N = ", length(ids_data), ")\n",
      "Checkpoint items: N = ", length(ids_ckpt), "\n\n",
      "IDs only in checkpoint (up to 8): ",
      paste(utils::head(only_in_ckpt, 8), collapse = ", "),
      "\n",
      "IDs only in dataset (up to 8): ",
      paste(utils::head(only_in_data, 8), collapse = ", "),
      "\n\n",
      "Fix: unset PAIRWISELLM_RESUME_RUN_TAG to start fresh, or set it to the correct run folder."
    ))
  }

  samples <- tibble::tibble(
    ID = ids_ckpt,
    text = as.character(unname(st$texts))
  )

  message("Resuming with checkpoint items (N = ", nrow(samples), ") from: ", base_dir)
}

text_map <- stats::setNames(as.character(samples$text), as.character(samples$ID))

# -----------------------------
# Logging helpers
# -----------------------------
degree_summary <- function(results, ids) {
  ids <- as.character(ids)
  deg <- stats::setNames(integer(length(ids)), ids)
  if (!is.data.frame(results) || nrow(results) < 1L) {
    return(list(
      min_degree = 0L, mean_degree = 0, max_degree = 0L, sd_degree = 0,
      frac_zero_degree = 1, gini_degree = NA_real_,
      n_unique_pairs_seen = 0L, unique_pair_rate = NA_real_
    ))
  }

  A <- as.character(results$A_id)
  B <- as.character(results$B_id)
  tabA <- table(A)
  tabB <- table(B)
  commonA <- intersect(names(tabA), names(deg))
  commonB <- intersect(names(tabB), names(deg))
  deg[commonA] <- deg[commonA] + as.integer(tabA[commonA])
  deg[commonB] <- deg[commonB] + as.integer(tabB[commonB])

  min_degree <- min(deg)
  mean_degree <- mean(as.double(deg))
  max_degree <- max(deg)
  sd_degree <- stats::sd(as.double(deg))
  frac_zero_degree <- mean(deg == 0L)

  # Gini coefficient (simple, stable)
  x <- sort(as.double(deg))
  if (sum(x) == 0) {
    gini <- NA_real_
  } else {
    n <- length(x)
    gini <- (2 * sum(x * seq_len(n))) / (n * sum(x)) - (n + 1) / n
  }

  ukeys <- make_unordered_key(A, B)
  n_unique <- dplyr::n_distinct(ukeys)
  unique_rate <- n_unique / nrow(results)

  list(
    min_degree = as.integer(min_degree),
    mean_degree = as.double(mean_degree),
    max_degree = as.integer(max_degree),
    sd_degree = as.double(sd_degree),
    frac_zero_degree = as.double(frac_zero_degree),
    gini_degree = as.double(gini),
    n_unique_pairs_seen = as.integer(n_unique),
    unique_pair_rate = as.double(unique_rate)
  )
}

log_progress <- function(prefix, results, ids) {
  s <- degree_summary(results, ids)
  msg <- paste0(
    prefix,
    " completed_pairs=", nrow(results),
    " | deg(min/mean/max/sd)=", s$min_degree, "/",
    sprintf("%.2f", s$mean_degree), "/", s$max_degree, "/",
    sprintf("%.2f", s$sd_degree),
    " | frac_zero_deg=", sprintf("%.3f", s$frac_zero_degree),
    " | gini_deg=", ifelse(is.na(s$gini_degree), "NA", sprintf("%.3f", s$gini_degree)),
    " | unique_pairs=", s$n_unique_pairs_seen,
    " (rate=", ifelse(is.na(s$unique_pair_rate), "NA", sprintf("%.3f", s$unique_pair_rate)), ")"
  )
  message(msg)
  invisible(s)
}

# -----------------------------
# Random: degree-balanced sampler
# -----------------------------
make_degree_balanced_pairs <- function(ids,
                                      n_needed,
                                      degrees,
                                      seen_keys,
                                      seed,
                                      alpha = 1.0,
                                      avoid_repeats = TRUE,
                                      max_attempts = NULL) {
  ids <- as.character(ids)
  degrees <- as.integer(degrees[ids])
  degrees[is.na(degrees)] <- 0L

  if (is.null(max_attempts)) {
    max_attempts <- max(20000L, n_needed * 100L)
  }

  seen <- NULL
  if (avoid_repeats) {
    seen <- new.env(parent = emptyenv())
    if (length(seen_keys) > 0L) {
      for (k in seen_keys) assign(k, TRUE, envir = seen)
    }
  }

  # weights: inverse-degree (smaller degree => higher chance to be selected)
  weights <- function() {
    w <- 1 / ((as.double(degrees) + 1) ^ alpha)
    w / sum(w)
  }

  out <- vector("list", n_needed)
  k <- 0L
  attempts <- 0L

  while (k < n_needed) {
    attempts <- attempts + 1L
    if (attempts > max_attempts) {
      rlang::abort("Degree-balanced sampler hit max_attempts. Consider avoid_repeats=FALSE or smaller n_needed.")
    }

    set.seed(seed + attempts)
    w <- weights()
    A <- ids[[sample.int(length(ids), 1L, prob = w)]]
    B <- ids[[sample.int(length(ids), 1L, prob = w)]]
    if (A == B) next

    ukey <- make_unordered_key(A, B)
    if (!is.null(seen) && exists(ukey, envir = seen, inherits = FALSE)) next

    # Randomize A/B presentation order deterministically
    set.seed(seed + 999999L + attempts)
    if (runif(1) < 0.5) {
      tmp <- A; A <- B; B <- tmp
    }

    k <- k + 1L
    out[[k]] <- tibble::tibble(
      pair_uid = paste0(ukey, "#db_", attempts),
      unordered_key = ukey,
      ordered_key = make_ordered_key(A, B),
      A_id = as.character(A),
      B_id = as.character(B)
    )

    # update seen and degrees locally so within-chunk balancing improves
    if (!is.null(seen)) assign(ukey, TRUE, envir = seen)
    degrees[A] <- degrees[A] + 1L
    degrees[B] <- degrees[B] + 1L
  }

  dplyr::bind_rows(out)
}

build_submit_pairs_tbl <- function(pairs_meta, text_map) {
  A_text <- unname(text_map[as.character(pairs_meta$A_id)])
  B_text <- unname(text_map[as.character(pairs_meta$B_id)])
  if (any(is.na(A_text)) || any(is.na(B_text))) {
    missing <- unique(c(pairs_meta$A_id[is.na(A_text)], pairs_meta$B_id[is.na(B_text)]))
    rlang::abort(paste0("Missing texts for some IDs: ", paste(utils::head(missing, 8), collapse = ", ")))
  }

  tibble::tibble(
    ID1 = as.character(pairs_meta$A_id),
    text1 = as.character(A_text),
    ID2 = as.character(pairs_meta$B_id),
    text2 = as.character(B_text),
    pair_uid = as.character(pairs_meta$pair_uid),
    phase = "phase2",
    iter = 1L
  )
}

collect_random_results_degree_balanced <- function(target_pairs,
                                                   alpha = 1.0,
                                                   avoid_repeats = TRUE) {
  results <- if (file.exists(paths_random$results_path)) readRDS(paths_random$results_path) else tibble::tibble()
  failed_attempts <- if (file.exists(paths_random$failed_attempts_path)) readRDS(paths_random$failed_attempts_path) else tibble::tibble()
  pairs_submitted <- if (file.exists(paths_random$pairs_submitted_path)) readRDS(paths_random$pairs_submitted_path) else tibble::tibble()

  results <- tibble::as_tibble(results)
  failed_attempts <- tibble::as_tibble(failed_attempts)
  pairs_submitted <- tibble::as_tibble(pairs_submitted)

  ids <- as.character(samples$ID)

  # initial progress
  log_progress("Random(DB) resume", results, ids)

  while (nrow(results) < target_pairs) {
    remaining <- target_pairs - nrow(results)
    buffer <- max(50L, as.integer(ceiling(0.10 * remaining)))
    request_n <- remaining + buffer

    # Recompute degrees + seen keys from COMPLETED results (robust under resume/retries)
    s <- degree_summary(results, ids)
    degrees <- stats::setNames(integer(length(ids)), ids)
    if (nrow(results) > 0L) {
      # reuse degree_summary computation indirectly by rebuilding from results:
      # (fast enough for 10k)
      A <- as.character(results$A_id)
      B <- as.character(results$B_id)
      tabA <- table(A); tabB <- table(B)
      commonA <- intersect(names(tabA), names(degrees))
      commonB <- intersect(names(tabB), names(degrees))
      degrees[commonA] <- degrees[commonA] + as.integer(tabA[commonA])
      degrees[commonB] <- degrees[commonB] + as.integer(tabB[commonB])
    }

    seen_keys <- character(0)
    if (avoid_repeats && nrow(results) > 0L) {
      seen_keys <- make_unordered_key(results$A_id, results$B_id)
    }

    meta <- make_degree_balanced_pairs(
      ids = ids,
      n_needed = request_n,
      degrees = degrees,
      seen_keys = seen_keys,
      seed = seed + nrow(results) + 1L,  # advance seed as results accumulate
      alpha = alpha,
      avoid_repeats = avoid_repeats
    )

    submit_pairs <- build_submit_pairs_tbl(meta, text_map)

    res <- submit_llm_pairs(
      pairs = submit_pairs,
      model = model,
      trait_name = td$name,
      trait_description = td$description,
      backend = "openai",
      verbose = TRUE,
      progress = TRUE,
      parallel = TRUE,
      workers = workers_random,
      endpoint = endpoint,
      reasoning = reasoning,
      service_tier = service_tier
    )

    new_results <- tibble::as_tibble(or_else(res$results, tibble::tibble()))
    results <- dplyr::bind_rows(results, new_results)
    failed_attempts <- dplyr::bind_rows(failed_attempts, or_else(res$failed_attempts, tibble::tibble()))
    pairs_submitted <- dplyr::bind_rows(pairs_submitted, submit_pairs)

    # Persist after each chunk
    saveRDS(results, paths_random$results_path)
    saveRDS(failed_attempts, paths_random$failed_attempts_path)
    saveRDS(pairs_submitted, paths_random$pairs_submitted_path)

    log_progress("Random(DB)", results, ids)
  }

  # Keep first target_pairs completed judgments
  results <- results[seq_len(target_pairs), , drop = FALSE]
  saveRDS(results, paths_random$results_path)

  # Save a "pairs_completed" artifact (pairs corresponding to kept results)
  pairs_completed <- pairs_submitted
  if ("pair_uid" %in% names(results) && "pair_uid" %in% names(pairs_submitted)) {
    keep_uids <- results$pair_uid
    pairs_completed <- pairs_submitted |> dplyr::filter(.data$pair_uid %in% keep_uids)
  }
  saveRDS(pairs_completed, paths_random$pairs_completed_path)

  list(results = results, failed_attempts = failed_attempts, pairs_submitted = pairs_submitted)
}

# -------------------------------------------------------------------
# PART A: ADAPTIVE — run until exactly 10,000 completed pairs
# -------------------------------------------------------------------
adaptive_start_or_resume <- function() {
  if (file.exists(paths_adaptive$state_path)) {
    st0 <- adaptive_state_load(paths_adaptive$state_path)
    submission_info <- list(
      backend = st0$config$backend %||% "openai",
      model = st0$config$model %||% model,
      trait_name = st0$config$trait_name %||% td$name,
      trait_description = st0$config$trait_description %||% td$description,
      prompt_template = st0$config$prompt_template %||% NULL,
      output_dir = paths_adaptive$output_dir,
      state_path = paths_adaptive$state_path
    )
    list(mode = "resume", submission_info = submission_info)
  } else {
    start_out <- adaptive_rank_start(
      samples = samples,
      model = model,
      trait_name = td$name,
      trait_description = td$description,
      backend = "openai",
      mode = "live",
      submission = list(
        parallel = TRUE,
        workers = workers_adaptive,
        progress = TRUE,
        verbose = TRUE,
        endpoint = endpoint,
        reasoning = reasoning,
        service_tier = service_tier
      ),
      adaptive = adaptive_cfg,
      paths = paths_adaptive,
      seed = seed
    )
    list(mode = "start", submission_info = start_out$submission_info)
  }
}

adaptive_collect_to_target <- function(target_pairs) {
  init <- adaptive_start_or_resume()

  repeat {
    st <- if (file.exists(paths_adaptive$state_path)) adaptive_state_load(paths_adaptive$state_path) else NULL
    completed <- as.integer(st$comparisons_observed %||% 0L)

    # degree + coverage logging from state (preferred)
    if (!is.null(st) && !is.null(st$deg)) {
      deg <- as.double(st$deg)
      msg <- paste0(
        "Adaptive completed_pairs=", completed, " / ", target_pairs,
        " | deg(min/mean/max/sd)=",
        min(deg), "/", sprintf("%.2f", mean(deg)), "/", max(deg), "/", sprintf("%.2f", stats::sd(deg)),
        " | frac_zero_deg=", sprintf("%.3f", mean(deg == 0)),
        " | pos_balance_mean=",
        if (!is.null(st$pos1) && length(st$pos1) == length(st$deg)) {
          pos1 <- as.double(st$pos1)
          pb <- rep(NA_real_, length(deg))
          pos <- deg > 0
          pb[pos] <- (pos1[pos] / deg[pos]) - 0.5
          sprintf("%.3f", mean(pb, na.rm = TRUE))
        } else {
          "NA"
        }
      )
      message(msg)
    } else {
      message("Adaptive completed_pairs=", completed, " / ", target_pairs)
    }

    if (completed >= target_pairs) break

    adaptive_rank_resume(
      state_path = paths_adaptive$state_path,
      mode = "live",
      submission_info = init$submission_info,
      submission = list(
        parallel = TRUE,
        workers = workers_adaptive,
        progress = TRUE,
        verbose = TRUE,
        endpoint = endpoint,
        reasoning = reasoning,
        service_tier = service_tier
      ),
      adaptive = adaptive_cfg,
      seed = seed
    )
  }

  st_final <- adaptive_state_load(paths_adaptive$state_path)

  adaptive_results_all <- tibble::as_tibble(st_final$history_results %||% tibble::tibble())
  adaptive_pairs_all <- tibble::as_tibble(st_final$history_pairs %||% tibble::tibble())

  if (nrow(adaptive_results_all) < target_pairs) {
    rlang::abort("Adaptive state has fewer completed results than target; cannot proceed.")
  }

  adaptive_results <- adaptive_results_all[seq_len(target_pairs), , drop = FALSE]

  if ("pair_uid" %in% names(adaptive_results) && "pair_uid" %in% names(adaptive_pairs_all)) {
    keep_uids <- adaptive_results$pair_uid
    adaptive_pairs <- adaptive_pairs_all |>
      dplyr::filter(.data$pair_uid %in% keep_uids)
  } else {
    adaptive_pairs <- adaptive_pairs_all
  }

  saveRDS(adaptive_results, paths_compare$adaptive_results_path)
  saveRDS(adaptive_pairs, paths_compare$adaptive_pairs_path)

  list(state = st_final, results = adaptive_results, pairs = adaptive_pairs)
}

adaptive_out <- adaptive_collect_to_target(target_pairs)

pairwiseLLM:::validate_results_tbl(tibble::as_tibble(adaptive_out$results))

# -------------------------------------------------------------------
# PART B: RANDOM (degree-balanced) — collect to 10,000 completed pairs
# -------------------------------------------------------------------
random_collect <- collect_random_results_degree_balanced(
  target_pairs = target_pairs,
  alpha = 1.0,
  avoid_repeats = TRUE
)

random_results <- tibble::as_tibble(random_collect$results)
pairwiseLLM:::validate_results_tbl(random_results)

# -------------------------------------------------------------------
# PART C: Comparable refits at identical intervals (100..10000 by 100)
# -------------------------------------------------------------------
.btl_mcmc_require_cmdstanr <- getFromNamespace(".btl_mcmc_require_cmdstanr", "pairwiseLLM")
.btl_mcmc_require_cmdstanr()

# Use the same model variant the adaptive run uses (for comparability)
v3_config_final <- adaptive_out$state$config$v3 %||% adaptive_v3_config(nrow(samples), adaptive_v3_overrides)
model_variant <- v3_config_final$model_variant %||% "btl_e_b"

# Extra diagnostics to add to each refit row (beyond canonical round_log schema)
extra_degree_metrics_from_state <- function(state) {
  deg <- as.double(state$deg %||% numeric(0))
  if (length(deg) == 0L) {
    return(tibble(
      max_degree = NA_integer_,
      sd_degree = NA_real_,
      frac_zero_degree = NA_real_,
      degree_gini = NA_real_,
      unique_pair_rate = NA_real_
    ))
  }

  # gini
  x <- sort(deg)
  if (sum(x) == 0) {
    gini <- NA_real_
  } else {
    n <- length(x)
    gini <- (2 * sum(x * seq_len(n))) / (n * sum(x)) - (n + 1) / n
  }

  # unique_pair_rate from canonical fields if present
  # (n_unique_pairs_seen / completed_pairs)
  n_unique <- as.double(state$posterior$stop_metrics$n_unique_pairs_seen %||% NA_real_)
  n_completed <- as.double(state$posterior$stop_metrics$completed_pairs %||% NA_real_)
  unique_rate <- if (is.finite(n_unique) && is.finite(n_completed) && n_completed > 0) n_unique / n_completed else NA_real_

  tibble(
    max_degree = as.integer(max(deg)),
    sd_degree = as.double(stats::sd(deg)),
    frac_zero_degree = as.double(mean(deg == 0)),
    degree_gini = as.double(gini),
    unique_pair_rate = as.double(unique_rate)
  )
}

compute_round_log_from_refits <- function(samples, results, pair_counts, fits, v3_config, seed) {
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(), seed = seed)
  state$config$v3 <- v3_config
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

    # IMPORTANT: build_round_log_row() already includes mean_degree + min_degree.
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
    ) |>
      # Add extra metrics (interpretable, not duplicative of canonical fields)
      dplyr::bind_cols(extra_degree_metrics_from_state(state))

    round_rows[[idx]] <- round_row

    # Match adaptive lag-history convention: first refit does not create lag entry
    if (idx > 1L) {
      state <- pairwiseLLM:::.adaptive_update_theta_history(state, fit = fit)
    }

    prev_n <- n_pairs
  }

  dplyr::bind_rows(round_rows)
}

# --- Fit + round log for RANDOM(DB) ---
random_fit <- load_or(paths_random$fit_path, function() {
  fit_bayes_btl_mcmc(
    results = random_results,
    ids = samples$ID,
    model_variant = model_variant,
    cmdstan = or_else(v3_config_final$cmdstan, list()),
    pair_counts = pair_counts,
    subset_method = "first",
    seed = seed
  )
})

random_round_log <- compute_round_log_from_refits(
  samples = samples,
  results = random_results,
  pair_counts = pair_counts,
  fits = random_fit$fits,
  v3_config = v3_config_final,
  seed = seed
)
saveRDS(random_round_log, paths_random$round_log_path)

# --- Fit + round log for ADAPTIVE (refit on first 10k results at same intervals) ---
adaptive_results <- readRDS(paths_compare$adaptive_results_path)

adaptive_fit <- load_or(file.path(adaptive_dir, "fit_bayes_btl_mcmc_refit100.rds"), function() {
  fit_bayes_btl_mcmc(
    results = adaptive_results,
    ids = samples$ID,
    model_variant = model_variant,
    cmdstan = or_else(v3_config_final$cmdstan, list()),
    pair_counts = pair_counts,
    subset_method = "first",
    seed = seed
  )
})

adaptive_round_log_refit100 <- compute_round_log_from_refits(
  samples = samples,
  results = adaptive_results,
  pair_counts = pair_counts,
  fits = adaptive_fit$fits,
  v3_config = v3_config_final,
  seed = seed
)
saveRDS(adaptive_round_log_refit100, paths_compare$adaptive_round_log_path)

# -------------------------------------------------------------------
# PART D: Combine logs + plots
# -------------------------------------------------------------------
adaptive_round_log_cmp <- adaptive_round_log_refit100 |>
  dplyr::mutate(
    method = "adaptive",
    run_tag = run_tag,
    pair_count = as.integer(.data$completed_pairs)
  )

random_round_log_cmp <- random_round_log |>
  dplyr::mutate(
    method = "random_degree_balanced",
    run_tag = run_tag,
    pair_count = as.integer(.data$completed_pairs)
  )

round_log_combined <- dplyr::bind_rows(adaptive_round_log_cmp, random_round_log_cmp) |>
  dplyr::arrange(.data$pair_count, .data$method)

saveRDS(round_log_combined, paths_compare$combined_round_log_path)
utils::write.csv(round_log_combined, paths_compare$combined_round_log_csv, row.names = FALSE)

# --- Figures: canonical stop metrics + the extra degree metrics ---
metric_specs <- tibble::tibble(
  metric = c(
    # canonical
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
    "min_ess_bulk",
    "n_unique_pairs_seen",
    "mean_degree",
    "min_degree",
    "pos_balance_mean",
    "pos_balance_sd",
    "starve_rate_since_last_refit",
    "fallback_rate_since_last_refit",
    # extras (added by this script)
    "max_degree",
    "sd_degree",
    "frac_zero_degree",
    "degree_gini",
    "unique_pair_rate"
  ),
  kind = c(
    # canonical
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
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    # extras
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
) |>
  dplyr::mutate(
    threshold = dplyr::case_when(
      .data$metric == "reliability_EAP" ~ as.double(or_else(v3_config_final$eap_reliability_min, NA_real_)),
      .data$metric == "rho_theta_lag" ~ as.double(or_else(v3_config_final$theta_corr_min, NA_real_)),
      .data$metric == "delta_sd_theta_lag" ~ as.double(or_else(v3_config_final$theta_sd_rel_change_max, NA_real_)),
      .data$metric == "rho_rank_lag" ~ as.double(or_else(v3_config_final$rank_spearman_min, NA_real_)),
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
      x = "Completed pairs (refit every 100)",
      y = metric,
      color = NULL
    ) +
    theme_minimal(base_size = 12)

  if (is.finite(threshold)) {
    p <- p + geom_hline(yintercept = threshold, linetype = "dashed", color = "grey40")
  }

  if (kind %in% c("logical", "logical_lag")) {
    p <- p + scale_y_continuous(breaks = c(0, 1), limits = c(0, 1))
  }

  out_path <- file.path(fig_dir, paste0("metric_", metric, "_refit100.png"))
  ggsave(out_path, plot = p, width = 8, height = 4.5, dpi = 150)
  invisible(out_path)
}

purrr::walk2(
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

message("Done.")
message("Outputs written under: ", base_dir)
message("Adaptive checkpoint: ", paths_adaptive$state_path)
message("Adaptive first-10k results: ", paths_compare$adaptive_results_path)
message("Random(DB) results: ", paths_random$results_path)
message("Combined log CSV: ", paths_compare$combined_round_log_csv)
