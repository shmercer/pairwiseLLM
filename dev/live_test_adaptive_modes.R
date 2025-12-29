# Live test script for adaptive runners (developer utility)
# Run from an interactive R session:
#   source(system.file("dev/live_test_adaptive_modes.R", package = "pairwiseLLM"))

suppressPackageStartupMessages({
  library(tibble)
  library(dplyr)
})

set.seed(1)

# ----- common fixtures -----
samples <- tibble(ID = LETTERS[1:24], text = paste0("text_", LETTERS[1:24]))
true_theta <- stats::setNames(seq(2, -3.0, length.out = nrow(samples)), samples$ID)

judge_fun <- function(pairs) {
  # Deterministic simulation to make debug output reproducible
  simulate_bt_judge(pairs, true_theta = true_theta, deterministic = TRUE, seed = 1)
}

mock_fit <- function(bt_data, ...) {
  ids <- sort(unique(c(bt_data$object1, bt_data$object2)))
  list(
    engine = "mock",
    reliability = NA_real_,
    theta = tibble(ID = ids, theta = seq_along(ids), se = rep(1, length(ids))),
    diagnostics = list(sepG = NA_real_)
  )
}

cat("\n=== bt_run_adaptive() end-to-end ===\n")
tmp <- tempfile("pairwiseLLM_chk_")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

out <- bt_run_adaptive(
  samples = samples,
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 20,
  init_round_size = 20,
  max_rounds = 3,
  reliability_target = Inf, # avoid early stop
  checkpoint_dir = tmp,
  checkpoint_store_fits = FALSE,
  seed_pairs = 1
)

print(out$stop_reason)
print(out$rounds)
print(out$state)

cat("\nCheckpoint written to:\n", tmp, "\n", sep = "")
print(list.files(tmp, full.names = TRUE))

cat("\n=== Resume test (should succeed) ===\n")
out2 <- bt_run_adaptive(
  samples = samples,
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 20,
  init_round_size = 20,
  max_rounds = 3,
  reliability_target = Inf,
  resume_from = tmp,
  checkpoint_dir = tmp,
  checkpoint_store_fits = FALSE,
  seed_pairs = 1
)
print(out2$stop_reason)

cat("\n=== Resume mismatch test (should error with Requested/Checkpoint) ===\n")
samples_other <- tibble(ID = c("X","Y","Z"), text = c("x","y","z"))
try(
  bt_run_adaptive(
    samples = samples_other,
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    max_rounds = 1,
    resume_from = tmp
  )
)

cat("\n=== bt_run_adaptive_core_linking() end-to-end ===\n")
batches <- list(LETTERS[6:12], LETTERS[13:18])
core_ids <- LETTERS[1:5]

tmp2 <- tempfile("pairwiseLLM_chk_")
dir.create(tmp2, recursive = TRUE, showWarnings = FALSE)

out3 <- bt_run_adaptive_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "never",
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 12,
  max_rounds_per_batch = 2,
  within_batch_frac = 1,
  core_audit_frac = 0,
  reliability_target = Inf,
  checkpoint_dir = tmp2,
  checkpoint_store_fits = FALSE,
  seed_pairs = 1,
  verbose = TRUE
)

print(out3$batch_summary)
print(out3$stop_reason)

cat("\n=== adaptive_core_linking resume mismatch test (batches) ===\n")
batches2 <- list(LETTERS[6:12], LETTERS[13:16]) # changed
try(
  bt_run_adaptive_core_linking(
    samples = samples,
    batches = batches2,
    core_ids = core_ids,
    linking = "never",
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 12,
    max_rounds_per_batch = 2,
    within_batch_frac = 1,
    core_audit_frac = 0,
    reliability_target = Inf,
    resume_from = tmp2,
    checkpoint_dir = tmp2,
    checkpoint_store_fits = FALSE,
    seed_pairs = 1,
    verbose = TRUE
  )
)

cat("\n=== bt_run_core_linking() end-to-end ===\n")
tmp3 <- tempfile("pairwiseLLM_chk_")
dir.create(tmp3, recursive = TRUE, showWarnings = FALSE)

out4 <- bt_run_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "never",
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 12,
  max_rounds_per_batch = 2,
  within_batch_frac = 1,
  core_audit_frac = 0,
  reliability_target = Inf,
  checkpoint_dir = tmp3,
  checkpoint_store_fits = FALSE,
  seed_pairs = 1,
  verbose = TRUE
)

print(out4$batch_summary)
print(out4$stop_reason)

cat("\n=== core_linking resume mismatch test (core_ids) ===\n")
core_ids2 <- LETTERS[1:4]
try(
  bt_run_core_linking(
    samples = samples,
    batches = batches,
    core_ids = core_ids2,
    linking = "never",
    judge_fun = judge_fun,
    fit_fun = mock_fit,
    engine = "mock",
    round_size = 12,
    max_rounds_per_batch = 2,
    within_batch_frac = 1,
    core_audit_frac = 0,
    reliability_target = Inf,
    resume_from = tmp3,
    checkpoint_dir = tmp3,
    checkpoint_store_fits = FALSE,
    seed_pairs = 1,
    verbose = TRUE
  )
)


cat("\n=== Real stop cases (sanity checks) ===\n")

cat("\n-- adaptive: max_rounds = 0 --\n")
out_stop1 <- bt_run_adaptive(
  samples = samples,
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  max_rounds = 0,
  reliability_target = Inf,
  seed_pairs = 1
)
print(out_stop1$stop_reason)

cat("\n-- adaptive: round_size = 0 --\n")
out_stop2 <- bt_run_adaptive(
  samples = samples,
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 0,
  init_round_size = 0,
  max_rounds = 10,
  reliability_target = Inf,
  seed_pairs = 1
)
print(out_stop2$stop_reason)

cat("\n-- adaptive_core_linking: max_rounds_per_batch = 0 --\n")
out_stop3 <- bt_run_adaptive_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "never",
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  max_rounds_per_batch = 0,
  reliability_target = Inf,
  seed_pairs = 1
)
print(out_stop3$stop_reason)

cat("\n-- adaptive_core_linking: round_size = 0 --\n")
out_stop4 <- bt_run_adaptive_core_linking(
  samples = samples,
  batches = batches,
  core_ids = core_ids,
  linking = "never",
  judge_fun = judge_fun,
  fit_fun = mock_fit,
  engine = "mock",
  round_size = 0,
  max_rounds_per_batch = 10,
  reliability_target = Inf,
  seed_pairs = 1
)
print(out_stop4$stop_reason)
