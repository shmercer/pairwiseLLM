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

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Run configuration ----
MODEL <- "gpt-4.1-mini"          # cheap and usually sufficient for pairwise comparisons
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
new_ids_all <- sort(new_ids_all)
batches <- split(new_ids_all, rep(1:2, length.out = length(new_ids_all)))

message("Core IDs (", length(core_ids), "): ", paste(core_ids, collapse = ", "))
message("Batch sizes: ", paste(vapply(batches, length, integer(1)), collapse = ", "))

# ---- Helper diagnostics ----
.compute_pair_type_counts <- function(results_tbl, core_ids) {
  if (is.null(results_tbl) || nrow(results_tbl) == 0) {
    return(tibble::tibble(n_total = 0L, n_core_new = 0L, n_new_new = 0L, n_core_core = 0L))
  }
  r <- tibble::as_tibble(results_tbl)
  in_core_1 <- r$ID1 %in% core_ids
  in_core_2 <- r$ID2 %in% core_ids
  pair_type <- dplyr::case_when(
    in_core_1 & in_core_2 ~ "core_core",
    (!in_core_1) & (!in_core_2) ~ "new_new",
    TRUE ~ "core_new"
  )
  tibble::tibble(
    n_total = nrow(r),
    n_core_new = sum(pair_type == "core_new"),
    n_new_new = sum(pair_type == "new_new"),
    n_core_core = sum(pair_type == "core_core")
  )
}

.compute_wlt_and_degree <- function(results_tbl, ids_all) {
  # results_tbl expected columns: ID1, ID2, better_id (may contain NA)
  if (is.null(results_tbl) || nrow(results_tbl) == 0) {
    return(list(
      wlt = tibble::tibble(ID = ids_all, wins = 0L, losses = 0L, ties = 0L, judged = 0L),
      degree = tibble::tibble(ID = ids_all, n_appear = 0L, n_pos1 = 0L, n_pos2 = 0L)
    ))
  }
  r <- tibble::as_tibble(results_tbl)

  # Degree / appearances
  n_pos1 <- as.integer(table(factor(r$ID1, levels = ids_all)))
  n_pos2 <- as.integer(table(factor(r$ID2, levels = ids_all)))
  deg <- tibble::tibble(
    ID = ids_all,
    n_appear = n_pos1 + n_pos2,
    n_pos1 = n_pos1,
    n_pos2 = n_pos2
  )

  # W/L/T from better_id
  wins <- rep(0L, length(ids_all)); names(wins) <- ids_all
  losses <- rep(0L, length(ids_all)); names(losses) <- ids_all
  ties <- rep(0L, length(ids_all)); names(ties) <- ids_all
  judged <- rep(0L, length(ids_all)); names(judged) <- ids_all

  ok <- !is.na(r$better_id) & r$better_id != ""
  rr <- r[ok, , drop = FALSE]

  for (i in seq_len(nrow(rr))) {
    a <- rr$ID1[i]
    b <- rr$ID2[i]
    w <- rr$better_id[i]
    judged[a] <- judged[a] + 1L
    judged[b] <- judged[b] + 1L
    if (w == a && w != b) {
      wins[a] <- wins[a] + 1L
      losses[b] <- losses[b] + 1L
    } else if (w == b && w != a) {
      wins[b] <- wins[b] + 1L
      losses[a] <- losses[a] + 1L
    } else {
      # tie/other
      ties[a] <- ties[a] + 1L
      ties[b] <- ties[b] + 1L
    }
  }

  wlt <- tibble::tibble(
    ID = ids_all,
    wins = as.integer(wins),
    losses = as.integer(losses),
    ties = as.integer(ties),
    judged = as.integer(judged)
  )

  list(wlt = wlt, degree = deg)
}

.theta_quality_cor <- function(theta_tbl, quality_map) {
  merged <- dplyr::left_join(theta_tbl, quality_map, by = "ID")
  if (!all(c("theta", "quality_score") %in% names(merged))) return(NA_real_)
  if (nrow(merged) < 3) return(NA_real_)
  suppressWarnings(stats::cor(merged$theta, merged$quality_score, use = "pairwise.complete.obs"))
}

# ---- Judge function (OpenAI live) ----
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

  results
}

# ---- Run ----
# Aggressive settings: prioritize good behavior + convergence.
out <- bt_run_adaptive_core_linking(
  samples = samples %>% select(ID, text),
  batches = batches,
  core_ids = core_ids,
  judge_fun = judge_fun,

  # Pairing + allocation (bigger, to converge more reliably)
  seed_pairs = 60,
  round_size = 40,
  init_round_size = 40,
  max_rounds_per_batch = 25,
  allocation = "precision_ramp",
  within_batch_frac = 0.20,
  core_audit_frac = 0.10,
  k_neighbors = 10,
  forbid_repeats = TRUE,
  balance_positions = TRUE,

  # Stopping targets (strict)
  reliability_target = 0.98,
  sepG_target = 6,
  rel_se_p90_target = 0.15,
  rel_se_p90_min_improve = 0.005,

  max_item_misfit_prop = NA_real_,
  max_judge_misfit_prop = NA_real_,

  # Linking behavior: robust + stabilized reference
  linking = "auto",
  linking_method = "median_iqr",
  reference_scale_method = "median_iqr",
  reference_max_abs = 6,

  linking_cor_target = 0.995,
  linking_p90_abs_shift_target = 0.25,

  # Drift guardrail
  core_max_abs_shift_target = 0.75,

  verbose = TRUE
)

# ---- Persist top-level artifacts ----
readr::write_csv(out$results, file.path(OUT_DIR, "results_all.csv"))
readr::write_csv(out$metrics, file.path(OUT_DIR, "metrics_all.csv"))
readr::write_csv(out$batch_summary, file.path(OUT_DIR, "batch_summary.csv"))

# ---- Fit-level artifact directory ----
fit_dir <- file.path(OUT_DIR, "fit_artifacts")
dir.create(fit_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Per-fit dumps: theta_raw + theta_linked + quality corr + win/degree + core drift ----
if (!is.null(out$fits) && length(out$fits) > 0L) {
  summaries <- vector("list", length(out$fits))

  # Identify a baseline reference theta if the runner exposes it; otherwise use first fit's core.
  # (With the new package changes, reference is stabilized internally, but may not be returned.
  # This script estimates drift vs fit1 core as a pragmatic external check.)
  baseline_theta_core <- NULL
  if (!is.null(out$fits[[1]]$theta)) {
    baseline_theta_core <- out$fits[[1]]$theta %>%
      dplyr::filter(.data$ID %in% core_ids) %>%
      dplyr::select(ID, theta_ref = theta)
  }

  for (k in seq_along(out$fits)) {
    f <- out$fits[[k]]
    if (is.null(f$theta)) next

    stage <- f$stage %||% "round"
    bi <- f$batch_index %||% NA_integer_
    ri <- f$round_index %||% NA_integer_
    tag <- sprintf("%03d_%s_b%02d_r%02d", k, stage, bi, ri)

    theta_tbl <- tibble::as_tibble(f$theta)

    # Write theta tables: raw and linked (if present)
    readr::write_csv(theta_tbl, file.path(fit_dir, paste0("theta_", tag, ".csv")))

    if ("theta_linked" %in% names(theta_tbl)) {
      linked_tbl <- theta_tbl %>% dplyr::select(ID, theta_linked, dplyr::everything())
      readr::write_csv(linked_tbl, file.path(fit_dir, paste0("theta_linked_", tag, ".csv")))
    }

    # Pair type composition so you can see if linking edges are present
    plan <- .compute_pair_type_counts(
      results_tbl = out$results %>% dplyr::filter((.data$batch_index %||% NA_integer_) <= bi),
      core_ids = core_ids
    )

    # Win/Loss/Tie and degree summaries from *all results available by that fit*
    # If your results table has batch/round indices, filter up to (bi, ri).
    results_upto <- out$results
    if ("batch_index" %in% names(results_upto)) {
      if (!is.na(bi)) results_upto <- dplyr::filter(results_upto, .data$batch_index <= bi)
    }
    if ("round_index" %in% names(results_upto) && !is.na(ri)) {
      results_upto <- dplyr::filter(results_upto, .data$round_index <= ri | .data$batch_index < bi)
    }

    wdeg <- .compute_wlt_and_degree(results_upto, ids_all = samples$ID)
    readr::write_csv(wdeg$wlt, file.path(fit_dir, paste0("wlt_", tag, ".csv")))
    readr::write_csv(wdeg$degree, file.path(fit_dir, paste0("degree_", tag, ".csv")))

    # Quality correlations: raw theta and linked theta (if present)
    cor_raw_all <- .theta_quality_cor(theta_tbl %>% dplyr::select(ID, theta), quality_map)
    cor_raw_core <- .theta_quality_cor(theta_tbl %>% dplyr::filter(ID %in% core_ids) %>% dplyr::select(ID, theta), quality_map)

    cor_link_all <- NA_real_
    cor_link_core <- NA_real_
    if ("theta_linked" %in% names(theta_tbl)) {
      cor_link_all <- .theta_quality_cor(theta_tbl %>% dplyr::transmute(ID, theta = .data$theta_linked), quality_map)
      cor_link_core <- .theta_quality_cor(theta_tbl %>% dplyr::filter(ID %in% core_ids) %>% dplyr::transmute(ID, theta = .data$theta_linked), quality_map)
    }

    # External drift check on core vs baseline fit1 core
    core_drift_tbl <- tibble::tibble()
    core_p90_abs_shift <- NA_real_
    if (!is.null(baseline_theta_core)) {
      core_now <- theta_tbl %>% dplyr::filter(.data$ID %in% core_ids) %>% dplyr::select(ID, theta_now = theta)
      core_join <- dplyr::inner_join(baseline_theta_core, core_now, by = "ID") %>%
        mutate(abs_shift = abs(.data$theta_now - .data$theta_ref))
      core_p90_abs_shift <- as.numeric(stats::quantile(core_join$abs_shift, probs = 0.90, na.rm = TRUE))
      core_drift_tbl <- core_join
      readr::write_csv(core_drift_tbl, file.path(fit_dir, paste0("core_drift_vs_fit1_", tag, ".csv")))
    }

    summaries[[k]] <- tibble::tibble(
      fit_index = k,
      stage = stage,
      batch_index = bi,
      round_index = ri,
      n_ids = nrow(theta_tbl),
      reliability = f$reliability %||% NA_real_,
      sepG = (f$diagnostics$sepG %||% NA_real_),

      cor_theta_quality_all = cor_raw_all,
      cor_theta_quality_core = cor_raw_core,
      cor_theta_linked_quality_all = cor_link_all,
      cor_theta_linked_quality_core = cor_link_core,

      core_p90_abs_shift_vs_fit1 = core_p90_abs_shift,

      n_total_pairs = plan$n_total,
      n_core_new = plan$n_core_new,
      n_new_new = plan$n_new_new,
      n_core_core = plan$n_core_core
    )
  }

  fit_summary_tbl <- dplyr::bind_rows(summaries)
  readr::write_csv(fit_summary_tbl, file.path(OUT_DIR, "fit_summary.csv"))

  message("\nPer-fit summary (tail):")
  print(utils::tail(fit_summary_tbl, 12))
}

# ---- Linking checks: compare consecutive fits on shared IDs (raw + linked) ----
if (!is.null(out$fits) && length(out$fits) >= 2L) {
  link_dir <- file.path(OUT_DIR, "theta_stability")
  dir.create(link_dir, recursive = TRUE, showWarnings = FALSE)

  stability <- list()

  for (k in 2:length(out$fits)) {
    a <- out$fits[[k - 1]]$theta
    b <- out$fits[[k]]$theta
    if (is.null(a) || is.null(b)) next

    a <- tibble::as_tibble(a)
    b <- tibble::as_tibble(b)

    # Raw theta join
    ab <- dplyr::inner_join(
      a %>% select(ID, theta_prev = theta),
      b %>% select(ID, theta_cur = theta),
      by = "ID"
    )
    if (nrow(ab) >= 3) {
      readr::write_csv(ab, file.path(link_dir, sprintf("theta_raw_join_fit%03d_to_fit%03d.csv", k - 1, k)))

      cor_all <- suppressWarnings(stats::cor(ab$theta_prev, ab$theta_cur, use = "pairwise.complete.obs"))
      ab_core <- dplyr::filter(ab, ID %in% core_ids)
      cor_core <- if (nrow(ab_core) < 3) NA_real_ else suppressWarnings(stats::cor(ab_core$theta_prev, ab_core$theta_cur, use = "pairwise.complete.obs"))

      stability[[length(stability) + 1L]] <- tibble::tibble(
        fit_prev = k - 1,
        fit_cur = k,
        n_shared = nrow(ab),
        kind = "theta_raw",
        cor_theta = cor_all,
        cor_theta_core = cor_core
      )
    }

    # Linked theta join (if present in both)
    if ("theta_linked" %in% names(a) && "theta_linked" %in% names(b)) {
      abL <- dplyr::inner_join(
        a %>% select(ID, theta_prev = theta_linked),
        b %>% select(ID, theta_cur = theta_linked),
        by = "ID"
      )
      if (nrow(abL) >= 3) {
        readr::write_csv(abL, file.path(link_dir, sprintf("theta_linked_join_fit%03d_to_fit%03d.csv", k - 1, k)))

        cor_all <- suppressWarnings(stats::cor(abL$theta_prev, abL$theta_cur, use = "pairwise.complete.obs"))
        ab_core <- dplyr::filter(abL, ID %in% core_ids)
        cor_core <- if (nrow(ab_core) < 3) NA_real_ else suppressWarnings(stats::cor(ab_core$theta_prev, ab_core$theta_cur, use = "pairwise.complete.obs"))

        stability[[length(stability) + 1L]] <- tibble::tibble(
          fit_prev = k - 1,
          fit_cur = k,
          n_shared = nrow(abL),
          kind = "theta_linked",
          cor_theta = cor_all,
          cor_theta_core = cor_core
        )
      }
    }
  }

  stability_tbl <- dplyr::bind_rows(stability)
  readr::write_csv(stability_tbl, file.path(OUT_DIR, "theta_stability_summary.csv"))

  message("\nTheta stability summary:")
  print(stability_tbl)
}

message("\nDone. Outputs written to: ", normalizePath(OUT_DIR, winslash = "/", mustWork = FALSE))
