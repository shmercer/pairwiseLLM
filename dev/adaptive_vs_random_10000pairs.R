#!/usr/bin/env Rscript

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
# Dev: Adaptive pairing vs degree-balanced random pairing (OpenAI GPT-5.1)
#
# Goal:
# - Run a live ADAPTIVE session on `example_writing_samples1000` (N = 1000)
#   and collect EXACTLY target_pairs completed judgments (default 10,000),
#   with submission iterations of exactly 100 pairs (including warm start).
# - Run a DEGREE-BALANCED RANDOM baseline that also collects 10,000 judgments,
#   with the same 100-pair submission cadence.
# - Fit Bayesian BTL (MCMC) for the random run at the same pair counts (every 100),
#   compute canonical stop metrics, and store round logs for both.
# - Generate one figure per stop metric (by pair count; lagged metrics shown
#   only once eligible).
#
# Notes:
# - This script makes live API calls. Ensure your OpenAI key is set.
# - CmdStanR + CmdStan must be installed for MCMC fitting.
# - Designed to be resumable: artifacts are written after every chunk.
# - You said you will RESTART (not resume from the prior 1000-pair run).
#   This script will always start a new run_tag unless you explicitly set
#   PAIRWISELLM_RESUME_RUN_TAG (not recommended for a clean restart).
# -------------------------------------------------------------------------

# ---- Config ----
model <- "gpt-5.1"
service_tier <- "flex" # Alternative: "priority"
endpoint <- "responses"
reasoning <- "none"

seed <- 123L
workers <- 8L

target_pairs <- 10000L
submit_chunk <- 100L              # hard requirement: submit exactly 100 pairs per iteration
refit_interval <- 100L            # refit every 100 observed pairs
pair_counts <- seq.int(refit_interval, target_pairs, by = refit_interval)

# Adaptive stop thresholds: make stopping effectively impossible until we reach target_pairs.
# (We still compute stop metrics/logs; we just don't want early stop.)
adaptive_cfg <- list(
  # Ensure stop checks aren't eligible until we have all data we want.
  M1_target = target_pairs,
  # Keep default d1 unless you want to change it.
  d1 = 8,
  # Critical: override phase scheduling targets so Phase 1/2/3 submit 100 at a time.
  batch_overrides = list(
    BATCH1 = submit_chunk,
    BATCH2 = submit_chunk,
    BATCH3 = submit_chunk,
    CW     = submit_chunk
  ),
  # v3 internal controls (refit cadence + selection batch size)
  v3 = list(
    batch_size = submit_chunk,
    refit_B = refit_interval,

    # "Never stop early" knobs:
    eap_reliability_min = 1.10,   # reliability is <= 1, so this won't pass
    theta_corr_min = 0.999,
    rank_spearman_min = 0.999,
    theta_sd_rel_change_max = 0.0
  )
)

# If you want to *allow* stopping in other experiments, restore the defaults you were using:
# adaptive_cfg$v3$eap_reliability_min <- 0.85
# adaptive_cfg$v3$theta_corr_min <- 0.90
# adaptive_cfg$v3$theta_sd_rel_change_max <- 0.20
# adaptive_cfg$v3$rank_spearman_min <- 0.90
# adaptive_cfg$M1_target <- NULL

# Resumability toggle:
# For a clean restart, leave PAIRWISELLM_RESUME_RUN_TAG unset.
resume_run_tag <- Sys.getenv("PAIRWISELLM_RESUME_RUN_TAG", unset = "")
if (nzchar(resume_run_tag)) {
  message("NOTE: PAIRWISELLM_RESUME_RUN_TAG is set. For a clean restart, unset it.")
}

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
  pairs_submitted_path = file.path(random_dir, "pairs_submitted.rds"),
  pair_key_set_path = file.path(random_dir, "pair_key_set.rds"),
  degree_path = file.path(random_dir, "degree.rds"),
  fit_path = file.path(random_dir, "fit_bayes_btl_mcmc.rds"),
  round_log_path = file.path(random_dir, "round_log.rds")
)

paths_compare <- list(
  adaptive_round_log_path = file.path(adaptive_dir, "round_log.rds"),
  combined_round_log_path = file.path(base_dir, "round_log_combined.rds"),
  combined_round_log_csv = file.path(base_dir, "round_log_combined.csv")
)

# ---- Helpers ----
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
  if (file.exists(path)) return(readRDS(path))
  obj <- build_fn()
  saveRDS(obj, path)
  obj
}

# Non-exported helper (used by our refit-log reconstruction)
adaptive_state_load <- getFromNamespace("adaptive_state_load", "pairwiseLLM")

# ---- Data ----
data("example_writing_samples1000", package = "pairwiseLLM")
samples <- example_writing_samples1000 |>
  dplyr::select("ID", text = "text") |>
  dplyr::mutate(ID = as.character(.data$ID))

td <- trait_description("overall_quality")
text_map <- stats::setNames(as.character(samples$text), as.character(samples$ID))
ids <- as.character(samples$ID)
n_items <- length(ids)

# ---- Logging: degree summaries for a results table ----
degree_summary_from_results <- function(results, ids) {
  if (nrow(results) < 1) {
    return(list(mean_degree = 0, min_degree = 0))
  }
  deg <- integer(length(ids))
  names(deg) <- ids
  A <- as.character(results$A_id)
  B <- as.character(results$B_id)
  tabA <- table(A)
  tabB <- table(B)
  deg[names(tabA)] <- deg[names(tabA)] + as.integer(tabA)
  deg[names(tabB)] <- deg[names(tabB)] + as.integer(tabB)
  list(mean_degree = mean(deg), min_degree = min(deg))
}

# ---- Adaptive live run: start + resume to EXACT target_pairs ----
adaptive_collect_to_target <- function(target_pairs) {
  if (file.exists(paths_adaptive$state_path)) {
    # If you're truly restarting, delete the directory or use a new run_tag.
    message("Found existing adaptive state at: ", paths_adaptive$state_path)
    message("Resuming from checkpoint.")
    state <- adaptive_state_load(paths_adaptive$state_path)
    submission_info <- readRDS(file.path(adaptive_dir, "submission_info.rds"))
  } else {
    message("Starting new adaptive run in: ", adaptive_dir)

    out0 <- adaptive_rank_start(
      samples = samples,
      model = model,
      trait_name = td$name,
      trait_description = td$description,
      backend = "openai",
      mode = "live",
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
      paths = paths_adaptive,
      seed = seed
    )

    state <- out0$state
    submission_info <- out0$submission_info

    saveRDS(state, paths_adaptive$state_path)
    saveRDS(submission_info, file.path(adaptive_dir, "submission_info.rds"))
  }

  t0 <- Sys.time()
  last_completed <- as.integer(or_else(state$comparisons_observed, 0L))
  last_print <- Sys.time()

  while (as.integer(or_else(state$comparisons_observed, 0L)) < target_pairs) {
    now_completed <- as.integer(or_else(state$comparisons_observed, 0L))
    remaining <- target_pairs - now_completed

    # Safety: ensure we keep submitting exactly submit_chunk, unless we're very close to target.
    # If remaining < submit_chunk, we still schedule submit_chunk; we will truncate later if needed.
    # (That keeps the iteration sizes consistent, which you asked for.)
    res <- adaptive_rank_resume(
      state = state,
      mode = "live",
      submission_info = submission_info,
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
      seed = seed
    )

    state <- res$state
    submission_info <- res$submission_info

    # Persist after each resume iteration
    saveRDS(state, paths_adaptive$state_path)
    saveRDS(submission_info, file.path(adaptive_dir, "submission_info.rds"))

    # Lightweight progress logging (also useful if something goes wrong later)
    now_completed2 <- as.integer(or_else(state$comparisons_observed, 0L))
    delta <- now_completed2 - last_completed
    if (delta < 0) delta <- 0

    if (as.numeric(difftime(Sys.time(), last_print, units = "secs")) >= 5 || delta > 0) {
      # Degree summaries from observed results in state (history_results)
      hist_res <- tibble::as_tibble(or_else(state$history_results, tibble()))
      degsum <- degree_summary_from_results(hist_res, ids)

      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      rate <- if (elapsed > 0) now_completed2 / elapsed else NA_real_

      message(sprintf(
        "[ADAPTIVE] completed=%d (+%d) / %d | mean_deg=%.2f min_deg=%d | elapsed=%.1fs rate=%.3f pairs/s",
        now_completed2, delta, target_pairs, degsum$mean_degree, degsum$min_degree, elapsed, rate
      ))
      last_print <- Sys.time()
    }

    last_completed <- now_completed2

    # If the adaptive code ever stops early (mode == stopped), break and error
    if (!is.null(state$mode) && identical(state$mode, "stopped") && now_completed2 < target_pairs) {
      rlang::abort(paste0(
        "Adaptive run stopped early at completed_pairs=", now_completed2,
        " < target_pairs=", target_pairs,
        ". You can relax the 'never stop early' thresholds or set adaptive$M1_target higher."
      ))
    }
  }

  # Truncate to exactly target_pairs observed results if overshot.
  # This keeps comparability exact.
  if (!is.null(state$history_results) && nrow(state$history_results) > target_pairs) {
    state$history_results <- state$history_results[seq_len(target_pairs), , drop = FALSE]
    state$comparisons_observed <- target_pairs
    saveRDS(state, paths_adaptive$state_path)
  }

  state
}

adaptive_state <- adaptive_collect_to_target(target_pairs = target_pairs)
adaptive_out <- list(state = adaptive_state) # keep script structure similar to prior versions

adaptive_round_log <- tibble::as_tibble(or_else(adaptive_out$state$config$round_log, tibble::tibble()))
saveRDS(adaptive_round_log, paths_compare$adaptive_round_log_path)

# Determine v3 config + model variant used (for matching random refits)
v3_config <- or_else(adaptive_out$state$config$v3, adaptive_v3_config(nrow(samples), adaptive_cfg))
model_variant <- or_else(v3_config$model_variant, "btl_e_b")

# ---- Degree-balanced random pairing baseline (100 pairs per iteration) ----
# Strategy:
# - Maintain per-item degree counts (observed comparisons).
# - Each new pair is formed by sampling two items from the lowest-degree pool
#   (with some random tie-breaking) to keep degrees balanced.
# - Avoid repeating unordered pairs using a set (environment) keyed by unordered_key.
# - Randomize A/B order per pair.
#
# This baseline is easy to interpret:
# - It matches the adaptive run's enforced degree-balancing in warm start and beyond.
# - It avoids "tight windows" and samples globally from all items, but with an explicit
#   balancing constraint to control graph coverage.

random_pair_set_new <- function() {
  # Environment as a hash set for unordered keys
  e <- new.env(parent = emptyenv())
  e
}

random_pair_set_has <- function(set_env, key) {
  exists(key, envir = set_env, inherits = FALSE)
}

random_pair_set_add <- function(set_env, key) {
  assign(key, TRUE, envir = set_env)
  invisible(TRUE)
}

sample_degree_balanced_pairs <- function(ids, deg, set_env, n_pairs, seed_offset = 0L) {
  set.seed(seed + seed_offset)

  out_A <- character(n_pairs)
  out_B <- character(n_pairs)
  out_unordered <- character(n_pairs)

  # Precompute index map
  id_vec <- ids
  deg_vec <- deg
  names(deg_vec) <- id_vec

  # Helper: draw one id from the k-lowest degree items
  draw_lowdeg <- function(k = 50L) {
    # Choose from the k lowest-degree items (or all if N smaller)
    ord <- order(deg_vec, runif(length(deg_vec)))
    pool <- id_vec[ord[seq_len(min(k, length(id_vec)))]]
    sample(pool, 1)
  }

  i <- 1L
  tries <- 0L
  max_tries <- 200000L

  while (i <= n_pairs) {
    tries <- tries + 1L
    if (tries > max_tries) {
      rlang::abort("Degree-balanced random pairing: too many failed attempts to find new pairs.")
    }

    a <- draw_lowdeg(k = 80L)
    b <- draw_lowdeg(k = 80L)
    if (identical(a, b)) next

    key <- make_unordered_key(a, b)
    if (random_pair_set_has(set_env, key)) next

    # accept
    random_pair_set_add(set_env, key)

    # randomize presentation order
    if (runif(1) < 0.5) {
      A <- a; B <- b
    } else {
      A <- b; B <- a
    }

    out_A[[i]] <- A
    out_B[[i]] <- B
    out_unordered[[i]] <- key

    # update degrees immediately so the batch itself stays balanced
    deg_vec[[A]] <- deg_vec[[A]] + 1L
    deg_vec[[B]] <- deg_vec[[B]] + 1L

    i <- i + 1L
  }

  list(
    A_id = out_A,
    B_id = out_B,
    unordered_key = out_unordered,
    deg = deg_vec
  )
}

build_submit_pairs_tbl <- function(A_id, B_id, unordered_key, samples, iter) {
  s <- samples |>
    dplyr::select("ID", "text") |>
    dplyr::mutate(ID = as.character(.data$ID))

  pairs_meta <- tibble::tibble(
    A_id = as.character(A_id),
    B_id = as.character(B_id),
    unordered_key = as.character(unordered_key),
    ordered_key = make_ordered_key(A_id, B_id),
    pair_uid = paste0(unordered_key, "#", iter)
  )

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
      iter = as.integer(iter)
    )
}

collect_random_results <- function(target_pairs, submit_chunk) {
  results <- if (file.exists(paths_random$results_path)) readRDS(paths_random$results_path) else tibble::tibble()
  failed_attempts <- if (file.exists(paths_random$failed_attempts_path)) readRDS(paths_random$failed_attempts_path) else tibble::tibble()
  pairs_submitted <- if (file.exists(paths_random$pairs_submitted_path)) readRDS(paths_random$pairs_submitted_path) else tibble::tibble()

  # Degree state + pair-key set for resumability
  deg <- if (file.exists(paths_random$degree_path)) readRDS(paths_random$degree_path) else {
    d <- integer(length(ids)); names(d) <- ids; d
  }
  pair_set <- if (file.exists(paths_random$pair_key_set_path)) readRDS(paths_random$pair_key_set_path) else random_pair_set_new()

  results <- tibble::as_tibble(results)
  pairs_submitted <- tibble::as_tibble(pairs_submitted)
  failed_attempts <- tibble::as_tibble(failed_attempts)

  iter <- if (nrow(pairs_submitted) < 1) 1L else (max(as.integer(pairs_submitted$iter), na.rm = TRUE) + 1L)

  t0 <- Sys.time()
  last_print <- Sys.time()

  while (nrow(results) < target_pairs) {
    remaining <- target_pairs - nrow(results)

    # Keep iteration size exactly submit_chunk (even if remaining < submit_chunk),
    # then truncate results at end to exactly target_pairs for comparability.
    n_to_submit <- submit_chunk

    # Generate degree-balanced pairs
    samp <- sample_degree_balanced_pairs(
      ids = ids,
      deg = deg,
      set_env = pair_set,
      n_pairs = n_to_submit,
      seed_offset = iter
    )
    deg <- samp$deg

    submit_pairs <- build_submit_pairs_tbl(
      A_id = samp$A_id,
      B_id = samp$B_id,
      unordered_key = samp$unordered_key,
      samples = samples,
      iter = iter
    )

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

    # Persist after each chunk (resumability + post-mortem debugging)
    saveRDS(results, paths_random$results_path)
    saveRDS(failed_attempts, paths_random$failed_attempts_path)
    saveRDS(pairs_submitted, paths_random$pairs_submitted_path)
    saveRDS(deg, paths_random$degree_path)
    saveRDS(pair_set, paths_random$pair_key_set_path)

    # Logging
    if (as.numeric(difftime(Sys.time(), last_print, units = "secs")) >= 5) {
      degsum <- list(mean_degree = mean(deg), min_degree = min(deg))
      elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      rate <- if (elapsed > 0) nrow(results) / elapsed else NA_real_
      message(sprintf(
        "[RANDOM-DEG] completed=%d / %d | mean_deg=%.2f min_deg=%d | elapsed=%.1fs rate=%.3f pairs/s",
        nrow(results), target_pairs, degsum$mean_degree, degsum$min_degree, elapsed, rate
      ))
      last_print <- Sys.time()
    }

    iter <- iter + 1L
  }

  # Truncate to exactly target_pairs completed judgments (preserves completion order).
  if (nrow(results) > target_pairs) {
    results <- results[seq_len(target_pairs), , drop = FALSE]
    saveRDS(results, paths_random$results_path)
  }

  list(
    results = results,
    failed_attempts = failed_attempts,
    pairs_submitted = pairs_submitted
  )
}

random_collect <- collect_random_results(target_pairs = target_pairs, submit_chunk = submit_chunk)
random_results <- tibble::as_tibble(random_collect$results)

# Validate that results match expected schema and IDs
pairwiseLLM:::validate_results_tbl(random_results)
missing_ids <- setdiff(unique(c(random_results$A_id, random_results$B_id)), ids)
if (length(missing_ids) > 0L) {
  rlang::abort(paste0(
    "Random results contain item IDs not present in `samples`.\n",
    "Missing IDs (showing up to 8): ", paste(utils::head(missing_ids, 8), collapse = ", ")
  ))
}

# ---- Random pairing: refit BTL at pair_counts (MCMC) ----
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

# ---- Build a comparable round_log from the random refits (same internal metrics) ----
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

        if (any(is.na(A_text)) || any(is.na(B_text))) {
          missing <- unique(c(A_id[is.na(A_text)], B_id[is.na(B_text)]))
          rlang::abort(paste0(
            "Cannot reconstruct `history_pairs`: missing texts for some IDs.\n",
            "Missing IDs (showing up to 8): ", paste(utils::head(missing, 8), collapse = ", ")
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

    # Align lag logic with the adaptive wrapper: do not append theta history on first refit
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
    method = "random_deg_balanced",
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
    "min_ess_bulk",
    # additional graph coverage signals (already in round log)
    "mean_degree",
    "min_degree",
    "mean_degree_scheduled",
    "min_degree_scheduled",
    "pos_balance_sd",
    "pos_balance_sd_scheduled"
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
    "numeric",
    "numeric",
    "numeric",
    "numeric",
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
  if (!metric %in% names(df)) return(invisible(NULL))

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
  if (nrow(d) < 1) return(invisible(NULL))

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
    p <- p + scale_y_continuous(breaks = c(0, 1), limits = c(0, 1))
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
