# Live feature-suite for adaptive + core-linking workflow using OpenAI (LIVE).
#
# Goal:
#   Systematically exercise the new runner options with real API calls:
#     - core selection modes (pam vs embeddings)
#     - linking modes (never / auto / always)
#     - allocation modes (fixed vs precision_ramp)
#     - reliability-based stopping rules
#
# Usage:
#   source("dev/dev-openai-adaptive-feature-suite-live.R")
#
# Notes:
#   - Keep costs low: uses gpt-4o-mini and small batches.
#   - Embeddings are deterministic random by default to avoid Python/reticulate setup.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
  library(glue)
  library(readr)
  library(devtools)
})

devtools::load_all()

if (nchar(Sys.getenv("OPENAI_API_KEY")) == 0L) {
  stop("OPENAI_API_KEY env var is not set.", call. = FALSE)
}

# ----------------------------
# Output directory (always on)
# ----------------------------
.ts <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.stamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
.dbg <- function(...) message("[", .ts(), "] ", paste0(..., collapse = ""))

OUT_DIR <- file.path(getwd(), "dev", "feature_suite_outputs", paste0("run_", .stamp()))
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
.dbg("Feature-suite outputs will be written to: ", OUT_DIR)

write_bt_run_diagnostics <- function(out, dir, name) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(out, file.path(dir, paste0(name, "_out.rds")))
  if (!is.null(out$metrics)) write_csv(out$metrics, file.path(dir, paste0(name, "_metrics.csv")))
  if (!is.null(out$state)) write_csv(out$state, file.path(dir, paste0(name, "_state.csv")))
  if (!is.null(out$results)) write_csv(out$results, file.path(dir, paste0(name, "_results.csv")))
  if (!is.null(out$batch_summary)) write_csv(out$batch_summary, file.path(dir, paste0(name, "_batch_summary.csv")))
  invisible(TRUE)
}

# ----------------------------
# Global knobs (keep it cheap)
# ----------------------------
MODEL <- "gpt-4o-mini"
ENDPOINT <- "chat.completions"
TEMPERATURE <- 0

N_TOTAL <- 30
CORE_SIZE <- 10
BOOTSTRAP_N <- 12
ROUND_SIZE <- 10
MAX_ROUNDS <- 4

BATCH_SIZE <- 5
N_BATCHES <- ceiling((N_TOTAL - CORE_SIZE) / BATCH_SIZE) + 1L

`%||%` <- function(x, y) if (is.null(x)) y else x

.short <- function(x, n = 220) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  ifelse(nchar(x) <= n, x, paste0(substr(x, 1, n), "…"))
}

.print_results_summary <- function(res, label = "results") {
  if (is.null(res)) {
    .dbg(label, ": <NULL>")
    return(invisible(NULL))
  }
  res <- tibble::as_tibble(res)

  # Validation summary can fail if caller provided unusual columns; don't let it crash the run.
  rep <- tryCatch(
    validate_backend_results(
      res,
      backend = "openai",
      normalize_winner = TRUE,
      strict = FALSE,
      return_report = TRUE
    ),
    error = function(e) {
      .dbg(label, ": validate_backend_results() error: ", conditionMessage(e))
      list(
        n_rows = nrow(res),
        n_missing_winner = NA_integer_,
        n_missing_id = NA_integer_,
        n_invalid_winner = NA_integer_
      )
    }
  )

  .dbg(label, ": n=", rep$n_rows,
       " | missing_winner=", rep$n_missing_winner,
       " | missing_id=", rep$n_missing_id,
       " | invalid_winner=", rep$n_invalid_winner)

  if ("status_code" %in% names(res)) {
    sc <- sort(table(res$status_code), decreasing = TRUE)
    .dbg(label, ": status_code=", paste(names(sc), sc, sep = ":", collapse = ", "))
  }
  if ("error_message" %in% names(res)) {
    em <- res %>%
      filter(!is.na(error_message), error_message != "") %>%
      count(error_message, sort = TRUE)
    if (nrow(em) > 0) {
      .dbg(label, ": top error_message=", paste0(head(em$error_message, 2), collapse = " | "))
    }
  }

  # Show a few problematic rows (guard against missing columns)
  has_better <- "better_id" %in% names(res)
  bad <- res %>%
    mutate(.missing_winner = if (has_better) (is.na(.data$better_id) | .data$better_id == "") else TRUE) %>%
    filter(.missing_winner) %>%
    select(any_of(c("ID1", "ID2", "better_id", "better_sample", "status_code", "error_message", "content"))) %>%
    head(3)

  if (nrow(bad) > 0) {
    .dbg(label, ": example rows with missing winner:")
    print(bad %>% mutate(content = .short(content, 240)))
  }

  invisible(rep)
}

# ----------------------------
# Data + batches
# ----------------------------

qualities <- c(
  "Excellent: precise, well-structured, and well-supported.",
  "Very good: clear structure and strong reasoning.",
  "Good: mostly clear with minor issues.",
  "Okay: somewhat vague and repetitive.",
  "Poor: unclear and error-prone.",
  "Bad: incoherent, off-topic, or incomplete."
)

make_samples <- function(n_total) {
  tibble(
    ID = sprintf("S%03d", seq_len(n_total)),
    text = map_chr(seq_len(n_total), function(i) {
      q <- qualities[((i - 1L) %% length(qualities)) + 1L]
      glue("{q} (Sample {i})")
    })
  )
}

samples <- make_samples(N_TOTAL)

# Batches: first batch contains core IDs, then add new IDs in chunks.
core_ids_default <- samples$ID[seq_len(CORE_SIZE)]
rest_ids <- setdiff(samples$ID, core_ids_default)
batch_ids <- split(rest_ids, ceiling(seq_along(rest_ids) / BATCH_SIZE))
batches_default <- c(list(core_ids_default), unname(batch_ids))

# ----------------------------
# Embeddings (deterministic)
# ----------------------------
make_dummy_embeddings <- function(ids, d = 48, seed = 1234) {
  set.seed(seed)
  m <- matrix(rnorm(length(ids) * d), nrow = length(ids), ncol = d)
  rownames(m) <- ids
  m
}
embeddings_dummy <- make_dummy_embeddings(samples$ID, d = 48)

# ----------------------------
# LLM judge
# ----------------------------
td <- trait_description("overall_quality")
tmpl <- get_prompt_template("default")

ensure_pair_text <- function(pairs, samples_tbl) {
  pairs <- tibble::as_tibble(pairs)
  if (all(c("text1", "text2") %in% names(pairs))) {
    return(pairs)
  }
  s <- dplyr::select(samples_tbl, ID, text)
  pairs %>%
    left_join(rename(s, ID1 = ID, text1 = text), by = "ID1") %>%
    left_join(rename(s, ID2 = ID, text2 = text), by = "ID2")
}

# Scenario-aware judge so we can persist raw results per call.
.current_scenario <- NULL
.current_call_idx <- 0L

judge_openai_live <- function(pairs) {
  t0 <- Sys.time()
  .current_call_idx <<- .current_call_idx + 1L

  pairs <- ensure_pair_text(pairs, samples)

  out <- submit_llm_pairs(
    pairs = pairs,
    backend = "openai",
    model = MODEL,
    endpoint = ENDPOINT,
    trait_name = td$name,
    trait_description = td$description,
    prompt_template = tmpl,
    temperature = TEMPERATURE,
    include_raw = TRUE,
    include_thoughts = FALSE,
    status_every = 1L
  )

  .dbg("judge_openai_live: elapsed=", round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2), "s")
  .print_results_summary(out$results, label = "openai_live")

  # Persist judge results per call (helps post-mortems when runner errors)
  if (!is.null(.current_scenario)) {
    d <- file.path(OUT_DIR, .current_scenario, "judge_calls")
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    saveRDS(out, file.path(d, sprintf("call_%03d_out.rds", .current_call_idx)))
    if (!is.null(out$results)) {
      readr::write_csv(out$results, file.path(d, sprintf("call_%03d_results.csv", .current_call_idx)))
    }
  }

  out
}

# ----------------------------
# Scenario runner
# ----------------------------
summarize_run <- function(out, scenario) {
  res_n <- if (!is.null(out$results)) nrow(out$results) else NA_integer_
  stop_reason <- out$stop_reason %||% NA_character_
  stop_round <- out$stop_round %||% NA_integer_
  last_fit <- NULL
  if (!is.null(out$final_fits) && length(out$final_fits) > 0L) {
    last_fit <- out$final_fits[[length(out$final_fits)]]
  }
  linking_applied <- NA
  if (!is.null(last_fit) && !is.null(last_fit$linking)) {
    linking_applied <- isTRUE(last_fit$linking$applied)
  }
  tibble(
    scenario = scenario,
    n_results = res_n,
    stop_reason = stop_reason,
    stop_round = stop_round,
    linking_applied = linking_applied
  )
}

run_scenario <- function(scn) {
  .dbg("===== Scenario: ", scn$name, " =====")

  # Reset per-scenario judge call index and label (for per-call persistence)
  .current_scenario <<- scn$name
  .current_call_idx <<- 0L

  # Merge defaults with scenario overrides
  args <- modifyList(
    list(
      samples = samples,
      batches = batches_default,
      judge_fun = judge_openai_live,
      engine = "auto",
      fit_verbose = FALSE,
      return_diagnostics = TRUE,
      include_residuals = FALSE,
      seed_pairs = 2025,

      # Core + rounds
      core_ids = core_ids_default,
      round_size = ROUND_SIZE,
      init_round_size = BOOTSTRAP_N,
      max_rounds_per_batch = MAX_ROUNDS,

      # Linking defaults
      linking = "never",
      linking_method = "mean_sd",
      linking_min_n = 3L,

      # Allocation defaults
      allocation = "fixed",
      allocation_fun = NULL,

      # Stopping defaults
      stopping_tier = "strong",

      # Smoke-friendly defaults
      forbid_repeats = TRUE,
      balance_positions = TRUE,
      k_neighbors = Inf,
      min_judgments = NULL
    ),
    scn$args
  )

  # Scenario directory + args snapshot
  scn_dir <- file.path(OUT_DIR, scn$name)
  dir.create(scn_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(args, file.path(scn_dir, "scenario_args.rds"))

  out <- tryCatch(
    {
      do.call(bt_run_adaptive_core_linking, args)
    },
    error = function(e) {
      .dbg("ERROR in ", scn$name, ": ", conditionMessage(e))
      # Persist what we can.
      saveRDS(list(error = conditionMessage(e), scenario = scn), file.path(scn_dir, "ERROR.rds"))
      stop(e)
    }
  )

  # Persist full scenario outputs
  write_bt_run_diagnostics(out, scn_dir, scn$name)

  # Print a compact end summary
  .dbg("Finished: ", scn$name,
       " | stop_reason=", out$stop_reason %||% "<NA>",
       " | stop_round=", out$stop_round %||% "<NA>",
       " | n_results=", if (!is.null(out$results)) nrow(out$results) else "<NA>")

  if (!is.null(out$metrics) && nrow(out$metrics) > 0L) {
    .dbg("Last metrics row:")
    print(tail(out$metrics, 1) %>% select(any_of(c(
      "batch_index", "round_index",
      "reliability", "sepG", "rel_se_p90",
      "item_misfit_prop", "judge_misfit_prop",
      "linking_applied", "linking_reason",
      "core_n", "n_pairs_proposed"
    ))))
  }

  out
}

# ----------------------------
# Scenario matrix
# ----------------------------
scenarios <- list(
  list(
    name = "A_baseline_fixed_core_no_linking",
    args = list(
      core_ids = core_ids_default,
      linking = "never"
    )
  ),
  list(
    name = "B_linking_always_fixed_core",
    args = list(
      core_ids = core_ids_default,
      linking = "always"
    )
  ),
  list(
    name = "C_linking_auto_forced",
    args = list(
      core_ids = core_ids_default,
      linking = "auto",
      # Force auto-trigger even when drift is small.
      linking_cor_target = 0.999999,
      linking_p90_abs_shift_target = 0.000001,
      linking_max_abs_shift_target = 0.000001
    )
  ),
  list(
    name = "D_core_selection_pam",
    args = list(
      core_ids = NULL,
      core_method = "pam",
      core_size = CORE_SIZE
    )
  ),
  list(
    name = "E_core_selection_embeddings",
    args = list(
      core_ids = NULL,
      core_method = "embeddings",
      core_size = CORE_SIZE,
      embeddings = embeddings_dummy,
      embeddings_metric = "cosine"
    )
  ),
  list(
    name = "F_allocation_precision_ramp",
    args = list(
      allocation = "precision_ramp"
    )
  ),
  list(
    name = "G_reliability_easy_stop",
    args = list(
      stopping_tier = "strong",
      # Make stopping trivially achievable to exercise the stop path.
      reliability_target = 0.00,
      sepG_target = 0.00,
      rel_se_p90_target = 1.00
    )
  ),
  list(
    name = "H_reliability_strict_no_stop",
    args = list(
      stopping_tier = "very_strong",
      # Keep the run short; likely ends by max rounds.
      max_rounds_per_batch = 2L
    )
  )
)

# ----------------------------
# Execute
# ----------------------------
outs <- list()
rows <- list()

# Always persist the final RDS even if we error mid-run.
.on_exit_save <- function() {
  saveRDS(outs, file.path(OUT_DIR, "outs_partial_or_full.rds"))
}
on.exit(.on_exit_save(), add = TRUE)

for (i in seq_along(scenarios)) {
  scn <- scenarios[[i]]
  out <- run_scenario(scn)
  outs[[scn$name]] <- out
  rows[[i]] <- summarize_run(out, scn$name)
}

summary_tbl <- bind_rows(rows) %>%
  arrange(scenario)

.dbg("\n===== Feature-suite summary =====")
print(summary_tbl)

# Persist summary + outs
write_csv(summary_tbl, file.path(OUT_DIR, "feature_suite_summary.csv"))
saveRDS(list(summary = summary_tbl, outs = outs), file.path(OUT_DIR, "feature_suite_all.rds"))

invisible(list(summary = summary_tbl, outs = outs, out_dir = OUT_DIR))
