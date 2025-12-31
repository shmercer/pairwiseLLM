# Realistic end-to-end live run for adaptive core linking + thorough diagnostics
#
# Goals
#   - Approximate real usage: real (computed) embeddings, 2-3 batches/waves.
#   - Run long enough to reach high reliability and stable rankings.
#   - Capture detailed per-round artifacts to verify linking correctness.
#
# Notes
#   - This script makes *live* OpenAI API calls via `submit_openai_pairs_live()`.
#   - You must have an API key available (e.g., Sys.getenv("OPENAI_API_KEY")).
#   - If you prefer a different cheap model, change `MODEL` below.

suppressPackageStartupMessages({
  library(pairwiseLLM)
  library(dplyr)
  library(tibble)
  library(readr)
})

# ---- Run configuration ----
MODEL <- "gpt-4.1-mini"           # cheap and usually sufficient for pairwise comparisons
ENDPOINT <- "responses"          # "responses" or "chat.completions"
TRAIT_NAME <- "Overall writing quality"
TRAIT_DESC <- paste(
  "Which writing sample is better overall?",
  "Prefer clearer, more coherent, better-developed responses.",
  "If one sample is meaningfully stronger, choose it; otherwise choose the stronger of the two."
)

# Diagnostics output directory
RUN_STAMP <- format(Sys.time(), "%Y%m%d_%H%M%S")
OUT_DIR <- file.path("dev-output", paste0("live_core_link_debug_", RUN_STAMP))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

message("Writing diagnostics to: ", normalizePath(OUT_DIR, winslash = "/", mustWork = FALSE))

# ---- Data: built-in examples ----
data("example_writing_samples", package = "pairwiseLLM")
samples <- example_writing_samples %>%
  mutate(ID = as.character(ID)) %>%
  arrange(ID)

# Sanity: built-in data has quality_score increasing with ID (higher = better)
quality_map <- samples %>% select(ID, quality_score)

# ---- Real embeddings + core selection ----
# Compute sentence-transformer embeddings (cached to OUT_DIR for easy reuse).
# If you want to force recompute, set overwrite = TRUE.
emb <- compute_text_embeddings(
  x = samples,
  ids = samples$ID,
  engine = "sentence_transformers",
  model = "all-MiniLM-L6-v2",
  normalize = TRUE,
  cache_dir = file.path(OUT_DIR, "embeddings_cache"),
  use_cache = TRUE,
  overwrite = FALSE,
  show_progress = interactive()
)

# Choose a modest core (6) from embeddings.
core_tbl <- select_core_set(
  samples = samples,
  core_size = 6,
  method = "auto",
  embeddings = emb,
  distance = "cosine",
  seed = 1
)
core_ids <- core_tbl$ID

# Split remaining items into 2 batches (7 + 7). For 3 batches, change split.
new_ids_all <- setdiff(samples$ID, core_ids)
# Stable split for reproducibility
new_ids_all <- sort(new_ids_all)

batches <- split(new_ids_all, rep(1:2, length.out = length(new_ids_all)))

message("Core IDs (", length(core_ids), "): ", paste(core_ids, collapse = ", "))
message("Batch sizes: ", paste(vapply(batches, length, integer(1)), collapse = ", "))

# ---- Judge function (OpenAI live) ----
# bt_run_adaptive_core_linking() passes pairs with ID1,text1,ID2,text2.
judge_fun <- function(pairs) {
  chunk_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  req_path <- file.path(OUT_DIR, paste0("requested_pairs_", chunk_stamp, ".csv"))
  readr::write_csv(tibble::as_tibble(pairs), req_path)

  res <- submit_openai_pairs_live(
    pairs = pairs,
    model = MODEL,
    endpoint = ENDPOINT,
    trait_name = TRAIT_NAME,
    trait_description = TRAIT_DESC,
    verbose = TRUE,
    progress = TRUE,
    include_raw = TRUE,
    validate = TRUE,
    validate_strict = FALSE
  )

  if (!is.list(res) || is.null(res$results)) {
    stop("submit_openai_pairs_live() returned an unexpected object (missing $results).", call. = FALSE)
  }

  results <- tibble::as_tibble(res$results)

  failed <- res$failed_pairs
  if (is.null(failed)) {
    failed <- tibble::tibble()
  } else {
    failed <- tibble::as_tibble(failed)
  }

  resp_path <- file.path(OUT_DIR, paste0("judged_pairs_", chunk_stamp, ".csv"))
  readr::write_csv(results, resp_path)

  fail_path <- file.path(OUT_DIR, paste0("failed_pairs_", chunk_stamp, ".csv"))
  readr::write_csv(failed, fail_path)

  if (nrow(failed) > 0) {
    message(sprintf("WARNING: %d/%d pairs failed validation/parsing in this chunk.",
                    nrow(failed), nrow(pairs)))
  }

  if (!("better_id" %in% names(results))) {
    message("WARNING: judged results missing `better_id` column.")
  } else {
    message(sprintf("Judged %d pairs; %d have NA better_id.",
                    nrow(results), sum(is.na(results$better_id))))
  }

  # Return the tibble that the runner expects
  results
}

# ---- Run ----
# Settings tuned for "let it run" debugging on a cheap model.
# You can increase round_size / max_rounds_per_batch aggressively if desired.
out <- bt_run_adaptive_core_linking(
  samples = samples %>% select(ID, text),
  batches = batches,
  core_ids = core_ids,
  judge_fun = judge_fun,

  # Pairing + allocation
  seed_pairs = 24,
  round_size = 24,
  init_round_size = 24,
  max_rounds_per_batch = 10,
  allocation = "precision_ramp",
  within_batch_frac = 0.20,
  core_audit_frac = 0.10,
  k_neighbors = 10,
  forbid_repeats = TRUE,
  balance_positions = TRUE,

  # Stopping targets (set fairly strict)
  reliability_target = 0.98,
  sepG_target = 4,
  rel_se_p90_target = 0.20,

  # Keep going unless clearly converged
  rel_se_p90_min_improve = 0.01,
  max_item_misfit_prop = NA_real_,
  max_judge_misfit_prop = NA_real_,

  # Linking behavior: allow auto linking if drift grows
  linking = "auto",
  linking_method = "mean_sd",
  linking_cor_target = 0.995,
  linking_p90_abs_shift_target = 0.25,

  # Drift guardrail (do not allow stopping if core drift is too large)
  core_max_abs_shift_target = 0.75,

  # Keep verbosity reasonably high for debugging
  verbose = TRUE
)

# ---- Persist top-level artifacts ----
readr::write_csv(out$results, file.path(OUT_DIR, "results_all.csv"))
readr::write_csv(out$metrics, file.path(OUT_DIR, "metrics_all.csv"))
readr::write_csv(out$batch_summary, file.path(OUT_DIR, "batch_summary.csv"))

# ---- Per-round theta dumps + correlations with true quality_score ----
# out$fits is a list; each entry contains $theta and various indices.
if (!is.null(out$fits) && length(out$fits) > 0L) {
  theta_dir <- file.path(OUT_DIR, "theta_by_fit")
  dir.create(theta_dir, recursive = TRUE, showWarnings = FALSE)

  # Helper to compute correlation between theta and known quality_score
  theta_quality_cor <- function(theta_tbl) {
    merged <- dplyr::left_join(theta_tbl, quality_map, by = "ID")
    if (!all(c("theta", "quality_score") %in% names(merged))) return(NA_real_)
    if (nrow(merged) < 3) return(NA_real_)
    suppressWarnings(stats::cor(merged$theta, merged$quality_score, use = "pairwise.complete.obs"))
  }

  fit_summaries <- vector("list", length(out$fits))

  for (k in seq_along(out$fits)) {
    f <- out$fits[[k]]
    if (is.null(f$theta)) next

    stage <- f$stage %||% "round"
    bi <- f$batch_index %||% NA_integer_
    ri <- f$round_index %||% NA_integer_
    tag <- sprintf("%03d_%s_b%02d_r%02d", k, stage, bi, ri)

    # Write theta table
    theta_path <- file.path(theta_dir, paste0("theta_", tag, ".csv"))
    readr::write_csv(f$theta, theta_path)

    # Correlations
    cor_all <- theta_quality_cor(f$theta)

    # Core-only correlation (if all present)
    theta_core <- dplyr::filter(f$theta, .data$ID %in% core_ids)
    cor_core <- theta_quality_cor(theta_core)

    fit_summaries[[k]] <- tibble::tibble(
      fit_index = k,
      stage = stage,
      batch_index = bi,
      round_index = ri,
      n_ids = nrow(f$theta),
      reliability = f$reliability %||% NA_real_,
      sepG = (f$diagnostics$sepG %||% NA_real_),
      cor_theta_quality_all = cor_all,
      cor_theta_quality_core = cor_core
    )
  }

  fit_summary_tbl <- dplyr::bind_rows(fit_summaries)
  readr::write_csv(fit_summary_tbl, file.path(OUT_DIR, "fit_summary.csv"))

  # Print a quick view
  message("\nPer-fit summary (tail):")
  print(utils::tail(fit_summary_tbl, 10))
}

# ---- Linking checks: compare consecutive fits on shared IDs ----
# This helps validate that IDs are being merged consistently across rounds.
if (!is.null(out$fits) && length(out$fits) >= 2L) {
  link_dir <- file.path(OUT_DIR, "theta_stability")
  dir.create(link_dir, recursive = TRUE, showWarnings = FALSE)

  stability <- list()
  for (k in 2:length(out$fits)) {
    a <- out$fits[[k - 1]]$theta
    b <- out$fits[[k]]$theta
    if (is.null(a) || is.null(b)) next

    ab <- dplyr::inner_join(
      a %>% select(ID, theta_prev = theta),
      b %>% select(ID, theta_cur = theta),
      by = "ID"
    )
    if (nrow(ab) < 3) next

    stability[[length(stability) + 1L]] <- tibble::tibble(
      fit_prev = k - 1,
      fit_cur = k,
      n_shared = nrow(ab),
      cor_theta = suppressWarnings(stats::cor(ab$theta_prev, ab$theta_cur, use = "pairwise.complete.obs")),
      cor_theta_core = {
        ab_core <- dplyr::filter(ab, ID %in% core_ids)
        if (nrow(ab_core) < 3) NA_real_ else suppressWarnings(stats::cor(ab_core$theta_prev, ab_core$theta_cur, use = "pairwise.complete.obs"))
      }
    )

    # Save the joined table for deeper inspection if needed
    readr::write_csv(ab, file.path(link_dir, sprintf("theta_join_fit%03d_to_fit%03d.csv", k - 1, k)))
  }

  stability_tbl <- dplyr::bind_rows(stability)
  readr::write_csv(stability_tbl, file.path(OUT_DIR, "theta_stability_summary.csv"))

  message("\nTheta stability summary:")
  print(stability_tbl)
}

message("\nDone. Outputs written to: ", normalizePath(OUT_DIR, winslash = "/", mustWork = FALSE))
