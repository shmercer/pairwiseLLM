test_that("bt_run_adaptive_core_linking validates seed and seed_pairs", {
  samples <- tibble::tibble(ID = LETTERS[1:4], text = paste0("t", 1:4))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  # Seed must be integerish
  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C", "D")),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      round_size = 0,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      seed = "not_an_int"
    ),
    "seed"
  )


  # seed_pairs validates even if seed is NULL
  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C", "D")),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      round_size = 0,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      seed_pairs = "bad_seed"
    ),
    "seed_pairs"
  )

  # seed and seed_pairs can't disagree
  expect_error(
    pairwiseLLM::bt_run_adaptive_core_linking(
      samples = samples,
      batches = list(c("C", "D")),
      judge_fun = judge_fun,
      core_ids = c("A", "B"),
      round_size = 0,
      init_round_size = 0,
      max_rounds_per_batch = 0,
      seed = 1L,
      seed_pairs = 2L
    ),
    "seed"
  )
})
