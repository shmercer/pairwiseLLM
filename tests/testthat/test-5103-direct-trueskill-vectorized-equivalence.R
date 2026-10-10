# Regression oracle: compare with the EXACT scalar implementation originally
# used inside .adaptive_select_direct(). No provider, CmdStan or estimator calls.

test_that("vectorized direct scoring exactly matches scalar TrueSkill at representative sizes", {
  for (n in c(4L, 57L, 91L, 229L)) {
    ids <- sprintf("item%03d", seq_len(n))
    ts <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = ids))
    ts$items$mu <- 25 + sin(seq_len(n) * 0.73) * 18
    ts$items$sigma <- 0.5 + (seq_len(n) %% 17) / 3
    ts$beta <- 25 / 6
    for (focal in ids[c(1L, n %/% 2L, n)]) {
      partners <- rev(setdiff(ids, focal))
      original <- vapply(partners, function(partner) {
        pairwiseLLM:::trueskill_win_probability(focal, partner, ts)
      }, numeric(1L))
      optimized <- pairwiseLLM:::.adaptive_direct_partner_probabilities(
        focal, partners, ts)
      expect_identical(optimized, original)
      for (policy in c("trueskill_p50", "trueskill_pollitt")) {
        expected_d <- pairwiseLLM:::.adaptive_pairing_target_distance(original, policy)
        new_d <- pairwiseLLM:::.adaptive_pairing_target_distance(optimized, policy)
        expect_identical(new_d, expected_d)
        expect_identical(order(new_d, partners), order(expected_d, partners))
      }
    }
    # Reordering the trueskill data frame cannot affect ID-aligned results.
    ts$items <- ts$items[n:1L, , drop = FALSE]
    focal <- ids[[1L]]
    partners <- rev(ids[-1L])
    reference <- vapply(partners, function(partner) {
      pairwiseLLM:::trueskill_win_probability(focal, partner, ts)
    }, numeric(1L))
    expect_identical(
      pairwiseLLM:::.adaptive_direct_partner_probabilities(focal, partners, ts),
      reference)
  }
})

test_that("vector partner scoring retains scalar errors, result names and RNG state", {
  ids <- sprintf("item%02d", 1:7)
  ts <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = ids))
  ts$items$mu <- c(25, 25.01, 26, 24, 19, 25, 30)
  ts$items$sigma <- c(1.5, 2, 2, 0.75, 9, 6, 1)
  withr::local_seed(1703)
  rng_before <- .Random.seed
  out <- pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], ids[2:7], ts)
  expect_identical(names(out), ids[2:7])
  expect_identical(.Random.seed, rng_before)
  expect_length(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], character(), ts), 0L)
  expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    "unknown", ids[[2L]], ts), "present")
  expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], "unknown", ts), "present")
  expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], ids[[1L]], ts), "distinct")
  expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], NA_character_, ts), "non-missing")
  bad <- ts
  bad$items$sigma[[1L]] <- NA_real_
  expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(
    ids[[1L]], ids[[2L]], bad), "finite")
  expect_identical(.Random.seed, rng_before)
})

test_that("p50 and Pollitt selected pairs equal independent scalar-target oracle", {
  for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
    state <- pairwiseLLM:::new_adaptive_state(letters[1:4],
      now_fn = function() as.POSIXct("2026-09-10",tz="UTC"))
    state <- pairwiseLLM:::.adaptive_apply_controller_config(state,
      list(pairing_strategy = strategy))
    state <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
    # A has degree zero; all other items have degree two.
    state$history_pairs <- tibble::tibble(
      A_id = c("b","c","d"), B_id = c("c","d","b"))
    ts <- state$trueskill_state
    ts$items$mu <- c(25, 25.01, 28.25, 12)
    ts$items$sigma <- c(4.2, 4.4, 6.9, 1.1)
    state$trueskill_state <- ts
    partners <- c("b","c","d")
    p <- vapply(partners, function(id) {
      pairwiseLLM:::trueskill_win_probability("a",id,ts)
    },numeric(1L))
    scores <- pairwiseLLM:::.adaptive_pairing_target_distance(p,strategy)
    expected <- partners[[order(scores,partners)[[1L]]]]
    withr::local_seed(8813)
    before <- .Random.seed
    selected <- pairwiseLLM:::select_next_pair(state)
    expect_identical(state$item_ids[[selected$i]], "a")
    expect_identical(state$item_ids[[selected$j]], expected)
    expect_identical(selected$pairing_strategy, strategy)
    expect_identical(selected$round_stage, "direct_pairing")
    expect_identical(selected$n_candidates_scored, 3L)
    expect_identical(.Random.seed,before)
  }
})
