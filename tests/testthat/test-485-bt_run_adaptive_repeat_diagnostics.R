test_that("bt_run_adaptive reports repeat consistency deterministically", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("tA", "tB")
  )

  # Deterministic judge: always picks ID1 as the winner
  judge_fun <- function(pairs_tbl) {
    tibble::tibble(ID1 = pairs_tbl$ID1, ID2 = pairs_tbl$ID2, better_id = pairs_tbl$ID1)
  }

  # Minimal fit function to avoid heavy modeling (still returns required structure)
  fit_fun <- function(bt_data, engine = "btm", verbose = FALSE, ...) {
    ids <- sort(unique(c(bt_data$ID1, bt_data$ID2)))
    tibble_theta <- tibble::tibble(ID = ids, theta = seq_along(ids), se = rep(0.1, length(ids)))
    list(
      engine = engine,
      reliability = 0,
      theta = tibble_theta,
      diagnostics = list(sepG = 0)
    )
  }

  run <- bt_run_adaptive(
    samples = samples,
    judge_fun = judge_fun,
    fit_fun = fit_fun,
    init_round_size = 1,
    round_size = 1,
    max_rounds = 1,
    reliability_target = 2, # impossible => forces scoring a new round
    sepG_target = Inf,
    rel_se_p90_target = 0,
    rel_se_p90_min_improve = 0,
    max_item_misfit_prop = 1,
    max_judge_misfit_prop = 1,
    repeat_policy = "reverse_only",
    repeat_frac = 1,
    repeat_guard_min_degree = 1,
    repeat_guard_largest_component_frac = 0.5,
    seed_pairs = 1
  )

  diag <- run$pairing_diagnostics
  expect_true(is.data.frame(diag))
  expect_true(all(c("n_repeat_planned", "n_repeat_completed", "repeat_consistency_rate_round", "repeat_consistency_rate_cumulative") %in% names(diag)))

  # With 2 IDs and repeat_frac=1, we should plan and complete 1 reverse repeat
  diag_r1 <- diag[diag$round == 1, , drop = FALSE]
  expect_equal(diag_r1$n_repeat_planned[[1]], 1)
  expect_equal(diag_r1$n_repeat_completed[[1]], 1)

  # Winner is always ID1; reverse repeat flips ID1, so it should be inconsistent
  expect_equal(diag_r1$repeat_consistency_rate_round[[1]], 0)
  expect_equal(diag_r1$repeat_consistency_rate_cumulative[[1]], 0)
})
