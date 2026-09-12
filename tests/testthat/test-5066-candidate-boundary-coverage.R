test_that("direct cross products preserve routing ranks and exact bounded subsets", {
  rank <- c(a = 1L, b = 3L, c = 2L, d = 4L)
  strata <- c(a = 1L, b = 3L, c = 2L, d = 4L)
  f <- .adaptive_link_direct_cross_pairs
  pairs <- f(c("a", "b"), c("c", "d"), rank, strata)
  expect_identical(pairs$pair_key, c("a:c", "a:d", "b:c", "b:d"))
  expect_identical(pairs$i, c("a", "a", "c", "b"))
  expect_identical(pairs$dist_stratum_global, c(1L, 3L, 1L, 1L))
  expect_equal(nrow(f(character(), "c", rank, strata)), 0L)
  expect_error(f("a", "unknown", rank, strata), "finite routing ranks")
  expect_error(f("a", "c", rank, strata[-3]), "finite routing strata")
  bounded <- function(...) {
    .adaptive_link_direct_cross_pairs_bounded(
      c("a", "b"), c("c", "d"), rank, strata, "local_link", list(min = 0L, max = 1L), ...)
  }
  empty <- bounded(active_hub_ids = character())
  expect_equal(empty$n_after_route_filters, 4L)
  expect_equal(empty$n_after_active_domain, 0L)
  expect_equal(nrow(empty$candidates), 0L)
  out <- bounded(active_hub_ids = c("a", "b"), reserved_keys = "a:c", C_max = 10L, seed = 91L)
  expect_equal(out$total_legal, 2L)
  expect_setequal(make_unordered_key(out$candidates$i, out$candidates$j), c("b:c", "b:d"))
  withr::local_seed(18)
  before <- .Random.seed
  small <- bounded(active_hub_ids = c("a", "b"), C_max = 1L, seed = 91L)
  expect_equal(nrow(small$candidates), 1L)
  expect_equal(small$total_legal, 3L)
  expect_identical(.Random.seed, before)
  expect_error(.validate_candidate_window(2L, 1L), "n_ids")
  expect_error(.adaptive_pair_seed(1L, NA_character_), "unordered_key")
})

test_that("simulation accepts explicit item qualities and ignores invalid connectivity edges", {
  items <- tibble::tibble(item_id = c("a", "b", "c"), quality_score = c(-1, 0, 1))
  expect_error(.adaptive_simulation_run(items = items["item_id"]), "quality_score")
  out <- .adaptive_simulation_run(items = items, n_steps = 2L, run_seed = 91L, judge_seed = 8L)
  expect_setequal(out$state$item_ids, items$item_id)
  log <- out$state$step_log
  expect_true(.adaptive_warm_start_connectivity(log, items$item_id))
  invalid <- log[1, ]
  invalid$i <- 99L
  expect_true(.adaptive_warm_start_connectivity(dplyr::bind_rows(invalid, log), items$item_id))
  state <- adaptive_rank_start(items, seed = 91L)
  state$step_log <- log[1, setdiff(names(log), c("status", "round_stage", "starvation_reason"))]
  profile <- .adaptive_efficiency_profile(state, config = list(refit_pairs_target = 100L))
  expect_true(is.na(profile$latest_step_status))
  expect_true(is.na(profile$latest_step_round_stage))
  expect_true(is.na(profile$latest_step_starvation_reason))
})
