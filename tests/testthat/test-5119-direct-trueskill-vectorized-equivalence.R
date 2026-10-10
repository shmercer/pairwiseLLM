# Regression oracle: compare with the EXACT scalar implementation originally
# used inside .adaptive_select_direct(). No provider, CmdStan or estimator calls.

test_that("vectorized direct scoring exactly matches scalar TrueSkill at representative sizes", {
  for (n in c(2L, 4L, 57L, 91L, 229L)) {
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
      now_fn = function() as.POSIXct("2026-09-10", tz = "UTC"))
    state <- pairwiseLLM:::.adaptive_apply_controller_config(state,
      list(pairing_strategy = strategy))
    state <- pairwiseLLM:::.adaptive_round_activate_if_ready(state)
    # A has degree zero; all other items have degree two.
    state$history_pairs <- tibble::tibble(
      A_id = c("b", "c", "d"), B_id = c("c", "d", "b"))
    ts <- state$trueskill_state
    ts$items$mu <- c(25, 25.01, 28.25, 12)
    ts$items$sigma <- c(4.2, 4.4, 6.9, 1.1)
    state$trueskill_state <- ts
    partners <- c("b", "c", "d")
    p <- vapply(partners, function(id) {
      pairwiseLLM:::trueskill_win_probability("a", id, ts)
    }, numeric(1L))
    scores <- pairwiseLLM:::.adaptive_pairing_target_distance(p, strategy)
    expected <- partners[[order(scores, partners)[[1L]]]]
    withr::local_seed(8813)
    before <- .Random.seed
    selected <- pairwiseLLM:::select_next_pair(state)
    expect_identical(state$item_ids[[selected$i]], "a")
    expect_identical(state$item_ids[[selected$j]], expected)
    expect_identical(selected$pairing_strategy, strategy)
    expect_identical(selected$round_stage, "direct_pairing")
    expect_identical(selected$n_candidates_scored, 3L)
    expect_identical(.Random.seed, before)
  }
})

test_that("frozen scalar math agrees at ties, Pollitt targets, and numeric extremes", {
  reference <- direct_scalar_reference_326()
  for (n in c(2L, 4L, 57L, 91L, 229L)) {
    for (distribution in c("cold", "heterogeneous")) {
      ts <- direct_fixture_326(n, distribution = distribution)$trueskill_state
      ts$items <- ts$items[rev(seq_len(n)), ]
      before <- serialize(ts, NULL)
      ids <- ts$items$item_id
      for (focal in unique(ids[c(1L, n %/% 2L, n)])) {
        partners <- rev(setdiff(ids, focal))
        expected <- reference$probabilities(focal, partners, ts)
        actual <- pairwiseLLM:::.adaptive_direct_partner_probabilities(focal, partners, ts)
        expect_identical(actual, expected)
        for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
          old_distance <- reference$.adaptive_pairing_target_distance(expected, strategy)
          distance <- pairwiseLLM:::.adaptive_pairing_target_distance(actual, strategy)
          expect_identical(distance, old_distance)
          expect_identical(order(distance, partners), order(old_distance, partners))
        }
      }
      expect_identical(serialize(ts, NULL), before)
    }
  }
  ts <- direct_fixture_326(8L, distribution = "cold")$trueskill_state
  ids <- ts$items$item_id
  scenarios <- list(
    # Identical means give exact lexical ties; these targets also test ulp-scale
    # asymmetry around 1/3 and 2/3 without rounding or complementing probabilities.
    -2 * qnorm(c(0.5, 1 / 3, 2 / 3, 0.5, 1 / 3, 2 / 3,
      1 / 3 + .Machine$double.eps, 2 / 3 - .Machine$double.eps)),
    c(0, 0, 1e-300, -1e-300, 1e100, -1e100, 1e308, -1e308)
  )
  for (mu in scenarios) {
    ts$items$mu <- mu
    for (scale in c(1, 1e-200, 1e200)) {
      ts$items$sigma <- rep(scale, 8L)
      ts$beta <- scale
      for (i in c(1L, 7L, 8L)) {
        partners <- rev(ids[-i])
        actual <- pairwiseLLM:::.adaptive_direct_partner_probabilities(ids[i], partners, ts)
        expected <- reference$probabilities(ids[i], partners, ts)
        expect_identical(actual, expected)
        for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
          distance <- pairwiseLLM:::.adaptive_pairing_target_distance(actual, strategy)
          old_distance <- reference$.adaptive_pairing_target_distance(expected, strategy)
          expect_identical(distance, old_distance)
          expect_identical(partners[order(distance, partners)[1L]],
            partners[order(old_distance, partners)[1L]])
        }
      }
    }
  }
})

test_that("whole direct selections equal the frozen selector with all diagnostics", {
  reference <- direct_scalar_reference_326()
  withr::local_seed(5119L)
  before_rng <- .Random.seed
  for (n in c(2L, 4L, 57L, 91L, 229L)) {
    for (strategy in c("trueskill_p50", "trueskill_pollitt")) {
      for (distribution in c("cold", "heterogeneous")) {
        state <- direct_fixture_326(n, strategy, distribution)
        state$warm_start_done <- TRUE
        state$trueskill_state$items <- state$trueskill_state$items[rev(seq_len(n)), ]
        for (seed in c(1L, 326L, 8813L)) {
          state$meta$seed <- seed
          saved <- serialize(state, NULL)
          expected <- testthat::with_mocked_bindings(pairwiseLLM:::select_next_pair(state),
            .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
          actual <- pairwiseLLM:::select_next_pair(state)
          expect_identical(actual, expected)
          expect_identical(state$item_ids[c(actual$i, actual$j, actual$A, actual$B)],
            state$item_ids[c(expected$i, expected$j, expected$A, expected$B)])
          expect_identical(serialize(state, NULL), saved)
        }
      }
    }
  }
  expect_identical(.Random.seed, before_rng)
})

test_that("repeated pairs, duplicate limits and reservoir starvation retain exact diagnostics", {
  reference <- direct_scalar_reference_326()
  for (strategy in c("trueskill_p50", "trueskill_pollitt", "random")) {
    state <- direct_fixture_326(4L, strategy)
    state$warm_start_done <- TRUE
    pairs <- t(utils::combn(state$item_ids, 2L))
    histories <- list(
      tibble::tibble(A_id = pairs[, 1L], B_id = pairs[, 2L]),
      tibble::tibble(A_id = c(pairs[, 1L], pairs[, 2L]), B_id = c(pairs[, 2L], pairs[, 1L]))
    )
    f <- reservoir_fixture()
    for (history in histories) {
      state$history_pairs <- history
      old <- testthat::with_mocked_bindings(pairwiseLLM:::select_next_pair(state),
        .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
      expect_identical(pairwiseLLM:::select_next_pair(state), old)
    }
    for (used in c(0L, nrow(f$outcomes) - 1L, nrow(f$outcomes))) {
      state <- reservoir_start(f, strategy)
      state$warm_start_done <- TRUE
      state$history_pairs <- f$outcomes[seq_len(used), c("A_id", "B_id")]
      old <- testthat::with_mocked_bindings(pairwiseLLM:::select_next_pair(state),
        .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
      expect_identical(pairwiseLLM:::select_next_pair(state), old)
    }
  }
})

test_that("complete trajectories retain histories, presentation, state, diagnostics and RNG", {
  reference <- direct_scalar_reference_326()
  withr::local_seed(326L)
  RNGkind("L'Ecuyer-CMRG")
  before_rng <- .Random.seed
  before_kind <- RNGkind()
  for (strategy in c("trueskill_p50", "trueskill_pollitt", "random", "hybrid")) {
    initial <- direct_fixture_326(8L, strategy)
    saved <- serialize(initial, NULL)
    scalar_input <- unserialize(saved)
    optimized_input <- unserialize(saved)
    expected <- testthat::with_mocked_bindings(direct_run_326(scalar_input),
      .adaptive_select_direct = reference$.adaptive_select_direct, .package = "pairwiseLLM")
    actual <- direct_run_326(optimized_input)
    # A run populates an existing memo environment by reference. Compare that
    # legacy side effect too; never share this cache between the two executions.
    expect_identical(serialize(actual, NULL), serialize(expected, NULL))
    expect_identical(serialize(optimized_input, NULL), serialize(scalar_input, NULL))
    expect_identical(serialize(initial, NULL), saved)
    expect_identical(.Random.seed, before_rng)
    expect_identical(RNGkind(), before_kind)
    if (strategy != "hybrid") expect_true(any(actual$step_log$round_stage == "direct_pairing"))
  }
  # The vector helper must never be entered by either unaffected policy.
  testthat::local_mocked_bindings(.adaptive_direct_partner_probabilities = function(...) {
    stop("Direct TrueSkill scoring entered by an unaffected policy")
  }, .package = "pairwiseLLM")
  for (strategy in c("random", "hybrid")) expect_no_error(direct_run_326(direct_fixture_326(8L, strategy)))
})

test_that("validation failures do not mutate state or RNG and scalar gates stay intact", {
  reference <- direct_scalar_reference_326()
  ts <- direct_fixture_326(4L)$trueskill_state
  ids <- ts$items$item_id
  withr::local_seed(326L)
  before_rng <- .Random.seed
  invalid <- list(unclass(ts), structure(1L, class = "trueskill_state"))
  bad <- ts
  bad$items <- NULL
  invalid[[length(invalid) + 1L]] <- bad
  for (field in c("item_id", "mu", "sigma")) {
    bad <- ts
    bad$items[[field]] <- NULL
    invalid[[length(invalid) + 1L]] <- bad
  }
  for (id in list(c(ids[1L], ids[1L], ids[3:4]), c(NA_character_, ids[2:4]), c("", ids[2:4]))) {
    bad <- ts
    bad$items$item_id <- id
    invalid[[length(invalid) + 1L]] <- bad
  }
  for (field in c("mu", "sigma")) {
    for (value in list(NA_real_, Inf, "invalid")) {
      bad <- ts
      bad$items[[field]][1L] <- value
      invalid[[length(invalid) + 1L]] <- bad
    }
  }
  for (value in c(0, -1)) {
    bad <- ts
    bad$items$sigma[1L] <- value
    invalid[[length(invalid) + 1L]] <- bad
  }
  for (value in list(NULL, 0, -1, NA_real_, Inf, c(1, 2))) {
    bad <- ts
    bad$beta <- value
    invalid[[length(invalid) + 1L]] <- bad
  }
  for (bad in invalid) {
    saved <- serialize(bad, NULL)
    old <- tryCatch(reference$probabilities(ids[1L], ids[-1L], bad), error = identity)
    new <- tryCatch(pairwiseLLM:::.adaptive_direct_partner_probabilities(ids[1L], ids[-1L], bad),
      error = identity)
    expect_s3_class(new, "error")
    expect_identical(class(new), class(old))
    expect_identical(conditionMessage(new), conditionMessage(old))
    expect_identical(serialize(bad, NULL), saved)
  }
  for (focal in list(character(), c(ids[1L], ids[2L]), NA_character_, "", "foreign")) {
    expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(focal, ids[2L], ts))
  }
  for (partner in list(NA_character_, "", "foreign", ids[1L])) {
    expect_error(pairwiseLLM:::.adaptive_direct_partner_probabilities(ids[1L], partner, ts))
    expect_error(reference$probabilities(ids[1L], partner, ts))
  }
  expect_identical(pairwiseLLM:::.adaptive_direct_partner_probabilities(ids[1L], character(), ts), numeric())
  expect_identical(.Random.seed, before_rng)
})

test_that("scoring and selection do not create a previously absent random seed", {
  withr::local_preserve_seed()
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  state <- direct_fixture_326(4L)
  state$warm_start_done <- TRUE
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  pairwiseLLM:::.adaptive_direct_partner_probabilities(state$item_ids[1L], state$item_ids[-1L], state$trueskill_state)
  pairwiseLLM:::select_next_pair(state)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})
