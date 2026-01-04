test_that("447-01 bt_run_adaptive_core_linking early-exits when no pairs are allowed", {
  samples <- tibble::tibble(
    ID = c("a", "b"),
    text = c("A", "B")
  )

  chk_dir <- tempfile("chk_")
  dir.create(chk_dir)

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("a", "b")),
    core_ids = c("a", "b"),
    judge_fun = function(pairs, ...) tibble::tibble(),
    init_round_size = 0L,
    round_size = 0L,
    max_rounds_per_batch = 0L,
    checkpoint_dir = chk_dir
  )

  expect_equal(out$stop_reason, "pair_budget_exhausted")
  expect_true(file.exists(file.path(chk_dir, "run_state.rds")))
})

test_that("447-02 bt_run_adaptive_core_linking can tolerate a fit_fun that returns theta = NULL", {
  samples <- tibble::tibble(
    ID = c("a", "b", "c"),
    text = c("A", "B", "C")
  )

  initial_results <- tibble::tibble(
    ID1 = "a",
    ID2 = "b",
    better_id = "a"
  )

  judge_fun <- function(pairs, ...) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, engine, ...) {
    # Exercise the bt_run_adaptive_core_linking branch that fills theta when NULL.
    list(engine = "mock", theta = NULL, reliability = NA_real_, diagnostics = list())
  }

  out <- pairwiseLLM::bt_run_adaptive_core_linking(
    samples = samples,
    batches = list(c("a", "b", "c")),
    core_ids = c("a", "b"),
    initial_results = initial_results,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    init_round_size = 0L,
    round_size = 0L,
    max_rounds_per_batch = 0L
  )

  expect_identical(out$stop_reason, "max_rounds_reached")

  expect_true(is.list(out$final_fits))
  expect_true("batch1" %in% names(out$final_fits))
  expect_true(is.data.frame(out$final_fits$batch1$theta))
  expect_true(all(c("ID", "theta") %in% names(out$final_fits$batch1$theta)))
})
