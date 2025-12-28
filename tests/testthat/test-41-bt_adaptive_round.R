test_that("bt_adaptive_round validates inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  fit <- list(
    reliability = 0.95,
    theta = tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(1, 1)),
    diagnostics = list(sepG = 3.5)
  )

  expect_error(
    bt_adaptive_round(samples[, "ID"], fit, round_size = 1),
    "samples.*ID, text"
  )

  expect_error(
    bt_adaptive_round(samples, list(), round_size = 1),
    "containing `\\$theta`"
  )

  expect_error(
    bt_adaptive_round(samples, fit, round_size = -1),
    "non-negative"
  )
})

test_that("bt_adaptive_round returns empty pairs when round_size = 0 (but returns metrics/decision)", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  fit <- list(
    engine = "mock",
    reliability = 0.95,
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0.0, 0.1, 0.2),
      se = c(0.5, 0.5, 0.5)
    ),
    diagnostics = list(sepG = 3.5)
  )

  out <- bt_adaptive_round(samples, fit, round_size = 0)

  expect_s3_class(out$metrics, "tbl_df")
  expect_true(is.list(out$decision))
  expect_s3_class(out$pairs_next, "tbl_df")
  expect_equal(nrow(out$pairs_next), 0L)
})

test_that("bt_adaptive_round stops and returns empty pairs when stopping criteria are met", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  fit <- list(
    engine = "mock",
    reliability = 0.95,
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0.0, 1.0, 2.0),
      se = c(0.05, 0.05, 0.05)
    ),
    diagnostics = list(sepG = 3.5)
  )

  out <- bt_adaptive_round(
    samples, fit,
    round_size = 2,
    rel_se_p90_target = 0.50, # easy precision target
    reliability_target = 0.90,
    sepG_target = 3.0
  )

  expect_true(out$decision$stop)
  expect_equal(nrow(out$pairs_next), 0L)
})

test_that("bt_adaptive_round proposes pairs when not stopping", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste0("t", c("A", "B", "C", "D"))
  )

  fit <- list(
    engine = "mock",
    reliability = 0.95,
    theta = tibble::tibble(
      ID = c("A", "B", "C", "D"),
      theta = c(0.0, 0.01, 2.0, 2.01),
      se = c(2.0, 2.0, 2.0, 2.0)
    ),
    diagnostics = list(sepG = 3.5)
  )

  existing <- tibble::tibble(ID1 = "A", ID2 = "B")

  out <- bt_adaptive_round(
    samples, fit,
    existing_pairs = existing,
    round_size = 2,
    rel_se_p90_target = 0.0001, # extremely strict -> not met
    rel_se_p90_min_improve = NA_real_, # disable stability stop
    seed = 1
  )

  expect_false(out$decision$stop)
  expect_equal(nrow(out$pairs_next), 2L)

  keys <- paste0(pmin(out$pairs_next$ID1, out$pairs_next$ID2), "-", pmax(out$pairs_next$ID1, out$pairs_next$ID2))
  expect_false("A-B" %in% keys)
})

test_that("bt_adaptive_round can stop via stability when prev_metrics are provided", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))

  fit <- list(
    engine = "mock",
    reliability = 0.95,
    theta = tibble::tibble(
      ID = c("A", "B", "C"),
      theta = c(0.0, 1.0, 2.0),
      se = c(1.0, 1.0, 1.0)
    ),
    diagnostics = list(sepG = 3.5)
  )

  prev <- tibble::tibble(
    engine = "mock",
    n_items = 3,
    theta_sd = 1,
    se_mean = 1,
    se_max = 1,
    rel_se_mean = 1,
    rel_se_p90 = 0.41,
    reliability = 0.95,
    sepG = 3.5,
    item_misfit_prop = NA_real_,
    judge_misfit_prop = NA_real_
  )

  out <- bt_adaptive_round(
    samples, fit,
    prev_metrics = prev,
    round_size = 2,
    rel_se_p90_target = 0.10, # not met
    rel_se_p90_min_improve = 0.03 # treat small improvement as stalled -> stop
  )

  expect_true(out$decision$stop)
  expect_equal(nrow(out$pairs_next), 0L)
})
