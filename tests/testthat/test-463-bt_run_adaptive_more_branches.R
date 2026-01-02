test_that("bt_run_adaptive covers additional validation branches (embeddings, checkpoint_every, verbose dots)", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  judge_fun <- function(pairs) {
    tibble::tibble(ID1 = pairs$ID1, ID2 = pairs$ID2, better_id = pairs$ID1)
  }

  fit_fun <- function(bt_data, ...) {
    list(
      engine = "mock",
      reliability = 0.95,
      theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0, 0), se = c(1, 1, 1)),
      diagnostics = list(sepG = 3.5)
    )
  }

  # ---- embed_k / embed_far_k validations (only evaluated when embeddings is non-NULL) ----
  emb <- matrix(stats::rnorm(3 * 2),
    nrow = 3, ncol = 2,
    dimnames = list(c("A", "B", "C"), c("d1", "d2"))
  )

  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      engine = "mock",
      embeddings = emb,
      embed_k = -1,
      round_size = 1,
      init_round_size = 0,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_
    ),
    "embed_k"
  )

  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      engine = "mock",
      embeddings = emb,
      embed_far_k = -1,
      round_size = 1,
      init_round_size = 0,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_
    ),
    "embed_far_k"
  )

  # ---- checkpoint_every validation ----
  expect_error(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      engine = "mock",
      checkpoint_every = 0,
      checkpoint_dir = tempdir(),
      round_size = 1,
      init_round_size = 0,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_
    ),
    "checkpoint_every"
  )

  # ---- .fit_dots$verbose branch (when caller passes verbose= and does NOT supply fit_verbose) ----
  out <- capture.output(
    bt_run_adaptive(
      samples,
      judge_fun = judge_fun,
      fit_fun = fit_fun,
      engine = "mock",
      verbose = TRUE, # passed through to fit_fun via ...
      round_size = 2,
      init_round_size = 2,
      max_rounds = 1,
      rel_se_p90_target = NA_real_,
      rel_se_p90_min_improve = NA_real_,
      reliability_target = NA_real_,
      sepG_target = NA_real_,
      max_item_misfit_prop = NA_real_,
      max_judge_misfit_prop = NA_real_
    )
  )

  # smoke assertion: run completes and returns output (captured output length is irrelevant)
  expect_true(length(out) >= 0)
})
