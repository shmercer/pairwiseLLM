selector_296_baseline <- function() {
  readRDS(testthat::test_path("fixtures", "selector-296", "baseline.rds"))
}

test_that("terminal recovery selects a viable pair after all exploration attempts miss", {
  fixture <- selector_296_baseline()
  state <- fixture$states$sparse
  before <- state
  withr::local_seed(296)
  rng_before <- .Random.seed
  stage_original <- pairwiseLLM:::.adaptive_select_stage
  partner_original <- pairwiseLLM:::.adaptive_select_partner
  stages <- list()
  partner_calls <- 0L
  testthat::local_mocked_bindings(
    .adaptive_select_stage = function(...) {
      args <- list(...)
      out <- stage_original(...)
      stages[[args$stage$name]] <<- out
      out
    },
    .adaptive_select_partner = function(...) {
      partner_calls <<- partner_calls + 1L
      partner_original(...)
    },
    .package = "pairwiseLLM"
  )
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_true(fixture$failures$sparse$candidate_starved)
  expect_identical(names(stages), c("base", "expand_locality", "uncertainty_pool", "dup_relax", "global_safe"))
  expect_identical(partner_calls, 30L) # Ten attempts in each of three viable stages.
  expect_false(out$candidate_starved)
  expect_identical(c(out$i, out$j), c(2L, 4L))
  expect_identical(out$n_candidates_scored, 5L)
  expect_identical(out$fallback_used, "global_safe")
  expect_identical(out$fallback_path, paste(names(stages), collapse = ">"))
  expect_false(out$is_explore_step)
  expect_true(is.na(out$explore_mode))
  expect_true(is.na(out$explore_reason))
  expect_true(is.na(out$starvation_reason))
  expect_identical(out$local_priority_mode, "standard")
  expect_identical(out[names(stages$global_safe$counts)], stages$global_safe$counts)
  expect_identical(state, before)
  expect_identical(.Random.seed, rng_before)
})

test_that("an empty final stage preserves the last viable pool and its accounting", {
  fixture <- selector_296_baseline()
  state <- fixture$states$relaxed
  original <- pairwiseLLM:::.adaptive_select_stage
  stages <- list()
  testthat::local_mocked_bindings(.adaptive_select_stage = function(...) {
    args <- list(...)
    out <- original(...)
    stages[[args$stage$name]] <<- out
    out
  }, .package = "pairwiseLLM")
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_identical(stages$global_safe$counts$n_candidates_scored, 0L)
  expect_identical(stages$dup_relax$counts$n_candidates_scored, 4L)
  expect_false(out$candidate_starved)
  expect_identical(out$fallback_used, "dup_relax")
  expect_identical(out$fallback_path, fixture$failures$relaxed$fallback_path)
  expect_identical(out[names(stages$dup_relax$counts)], stages$dup_relax$counts)
  expect_identical(out$explore_rate_used, 2 * pairwiseLLM:::adaptive_defaults(6L)$explore_rate)
  expect_identical(c(out$i, out$j), c(2L, 3L))
  # The selected third observation still reverses the previous presentation.
  expect_identical(c(out$A, out$B), c(3L, 2L))
  expect_identical(out$star_cap_rejects, stages$dup_relax$star_caps$rejects)
  expect_identical(out$star_override_used, stages$dup_relax$star_override_used)
})

test_that("coverage narrowing cannot erase a viable recovery pool", {
  fixture <- selector_296_baseline()
  state <- fixture$states$quota
  expect_identical(state$meta$seed, 1111L)
  expect_true(fixture$failures$quota$candidate_starved)
  # Every stage chooses coverage override, but item 1 has no eligible partner.
  for (stage in seq_len(5L)) {
    draw <- pairwiseLLM:::.adaptive_with_seed(
      pairwiseLLM:::.adaptive_stage_seed(1111L, 1L, stage, 1L), stats::runif(1L)
    )
    expect_lt(draw, 0.20)
  }
  out <- pairwiseLLM:::select_next_pair(state, step_id = 1L)
  expect_false(out$candidate_starved)
  expect_identical(c(out$i, out$j), c(2L, 4L))
  expect_identical(out$n_candidates_scored, 5L)
  expect_false(out$is_explore_step)
  expect_true(is.na(out$explore_reason))
})

test_that("true exhaustion and successful pre-fix selections remain exactly unchanged", {
  fixture <- selector_296_baseline()
  expect_identical(fixture$sha, "565dad453407a84bdd31321a9f50bf0d067b9c91")
  expect_identical(pairwiseLLM:::select_next_pair(fixture$states$exhausted, 1L), fixture$failures$exhausted)
  withr::local_seed(296)
  rng_before <- .Random.seed
  for (nm in names(fixture$successful)) {
    case <- fixture$successful[[nm]]
    expect_identical(pairwiseLLM:::select_next_pair(case$state, 1L), case$selection, info = nm)
  }
  expect_identical(.Random.seed, rng_before)
  outputs <- lapply(fixture$successful, `[[`, "selection")
  expect_true(any(vapply(outputs, function(x) x$is_explore_step, logical(1L))))
  expect_true(any(vapply(outputs, function(x) !x$is_explore_step, logical(1L))))
  expect_true(any(vapply(outputs, function(x) x$fallback_used != "base", logical(1L))))
})

test_that("shared exploitation keeps utility fallbacks and deterministic ID ties", {
  state <- selector_296_baseline()$states$sparse
  choose <- function(cand, mode = "pairing_trueskill_u0", generation_stage = "local_link", link = FALSE) {
    pairwiseLLM:::.adaptive_select_exploitation(
      cand, state, state$round, generation_stage, 0L, 3L,
      state$controller, link, mode
    )
  }
  cand <- tibble::tibble(i = c("3", "2", "2"), j = c("5", "5", "4"), u0 = c(0.1, 0.2, 0.2))
  for (order in list(1:3, 3:1, c(2L, 1L, 3L))) {
    out <- choose(cand[order, ])
    expect_identical(out$selected$i, "2")
    expect_identical(out$selected$j, "4")
    expect_identical(out$local_priority_mode, "standard")
  }
  cand$u0 <- c(0.1, NA_real_, Inf)
  expect_identical(choose(cand)$selected$i, "3")
  # Unknown/missing primary utility falls back to U0, then to IDs.
  expect_identical(choose(cand, NA_character_)$selected$i, "3")
  cand$u0[] <- NA_real_
  expect_identical(choose(cand)$selected$j, "4")
  cand$u0 <- NULL
  expect_identical(choose(cand)$selected$j, "4")
  expect_identical(choose(cand, NA_character_)$selected$j, "4")
  expect_true(is.na(choose(cand, generation_stage = "long_link")$local_priority_mode))
  expect_true(is.na(choose(cand, link = TRUE)$local_priority_mode))
})

test_that("shared exploitation preserves identified local boundary priorities", {
  state <- selector_296_baseline()$states$sparse
  state$controller$global_identified <- TRUE
  state$controller$boundary_k <- 2L
  state$controller$boundary_window <- 2L
  state$controller$boundary_frac <- 0.5
  cand <- tibble::tibble(i = c("4", "2"), j = c("5", "6"), u0 = c(0.25, 0.20), p = c(0.5, 0.4))
  choose <- function(committed) {
    pairwiseLLM:::.adaptive_select_exploitation(
      cand, state, state$round, "local_link", committed, 4L,
      state$controller, FALSE, "pairing_trueskill_u0"
    )
  }
  boundary <- choose(0L)
  expect_identical(boundary$local_priority_mode, "boundary")
  expect_identical(boundary$selected$i, "2")
  near_tie <- choose(4L)
  expect_identical(near_tie$local_priority_mode, "near_tie")
  expect_identical(near_tie$selected$i, "4")
})

test_that("pre-fix saved evidence resumes with the same repaired selection", {
  session_dir <- testthat::test_path("fixtures", "selector-296", "session")
  before <- readRDS(file.path(session_dir, "state.rds"))
  loaded <- pairwiseLLM::load_adaptive_session(session_dir)
  for (field in c("step_log", "round_log", "history_pairs", "trueskill_state", "round")) {
    expect_identical(loaded[[field]], before[[field]], info = field)
  }
  expected <- pairwiseLLM:::select_next_pair(before, step_id = 1L)
  expect_false(expected$candidate_starved)
  expect_identical(pairwiseLLM:::select_next_pair(loaded, step_id = 1L), expected)
  saved <- withr::local_tempdir()
  pairwiseLLM::save_adaptive_session(loaded, saved)
  reloaded <- pairwiseLLM::load_adaptive_session(saved)
  expect_identical(pairwiseLLM:::select_next_pair(reloaded, step_id = 1L), expected)
  expect_identical(reloaded$step_log, before$step_log)
  expect_identical(reloaded$meta$schema_version, before$meta$schema_version)
})

test_that("a recovered step commits identically across save and resume", {
  state <- pairwiseLLM::load_adaptive_session(testthat::test_path("fixtures", "selector-296", "session"))
  # At step 13 this seed exhausts exploration in every viable fallback pool.
  state$meta$seed <- 1014L
  saved <- withr::local_tempdir()
  pairwiseLLM::save_adaptive_session(state, saved)
  resumed <- pairwiseLLM::load_adaptive_session(saved)
  judge <- function(...) list(is_valid = TRUE, Y = 1L)
  direct <- pairwiseLLM:::run_one_step(state, judge)
  restored <- pairwiseLLM:::run_one_step(resumed, judge)
  expect_identical(direct$step_log, restored$step_log)
  expect_identical(direct$history_pairs, restored$history_pairs)
  expect_identical(direct$trueskill_state, restored$trueskill_state)
  expect_identical(direct$step_log[seq_len(12L), ], state$step_log)
  expect_identical(nrow(direct$history_pairs), 13L)
  expect_identical(direct$step_log$status[[13L]], "ok")
  expect_identical(direct$step_log$fallback_used[[13L]], "global_safe")
  expect_false(direct$step_log$is_explore_step[[13L]])
})
