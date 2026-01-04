test_that("487-01 bt_run_core_linking early-exits when no pairs are allowed", {
  samples <- tibble::tibble(
    ID = c("a", "b", "c"),
    text = c("A", "B", "C")
  )

  chk_dir <- tempfile("chk_")
  dir.create(chk_dir)

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(batch1 = c("c")),
    core_ids = c("a", "b"),
    embeddings = NULL,
    judge_fun = function(pairs) {
      tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
    },
    engine = "mock",
    checkpoint_dir = chk_dir,
    init_round_size = 0,
    round_size = 0,
    max_rounds_per_batch = 0,
    verbose = FALSE
  )

  expect_equal(out$stop_reason, "pair_budget_exhausted")
  expect_equal(out$stop_round, 0L)
  expect_true(file.exists(file.path(chk_dir, "run_state.rds")))
})

test_that("487-02 bt_run_core_linking can resume from a completed checkpoint", {
  chk_dir <- tempfile("chk_")
  dir.create(chk_dir)

  samples <- tibble::tibble(
    ID = c("a", "b"),
    text = c("A", "B")
  )

  payload <- list(
    run_type = "core_linking",
    completed = TRUE,
    stop_reason = "max_rounds",
    current_batch = 1L,
    out = list(stop_reason = "max_rounds", results = tibble::tibble()),
    results = tibble::tibble(),
    fits = list(),
    rounds = tibble::tibble(
      batch = integer(),
      round = integer(),
      n_new = integer(),
      n_pairs = integer(),
      n_results = integer(),
      n_unique_pairs = integer(),
      reason = character()
    ),
    metrics = tibble::tibble(batch = 1L, round = 0L)
  )

  pairwiseLLM:::.bt_write_checkpoint(checkpoint_dir = chk_dir, payload = payload)

  out <- pairwiseLLM::bt_run_core_linking(
    samples = samples,
    batches = list(batch1 = c("a")),
    core_ids = c("a", "b"),
    embeddings = NULL,
    judge_fun = function(pairs) {
      tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
    },
    engine = "mock",
    resume_from = chk_dir,
    verbose = FALSE
  )

  expect_equal(out$stop_reason, "max_rounds")
  expect_s3_class(out, "pairwiseLLM_run")
})
