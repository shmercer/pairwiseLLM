test_that("adaptive schemas cover construction, options, and input errors", {
  out <- pairwiseLLM:::as_pairs_tbl(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
    utility = 1.5,
    extra_col = "extra"
  )

  required <- c(
    "pair_uid", "unordered_key", "ordered_key",
    "A_id", "B_id", "A_text", "B_text",
    "phase", "iter", "created_at"
  )
  expect_equal(names(out)[seq_along(required)], required)
  expect_true("extra_col" %in% names(out))

  expect_error(pairwiseLLM:::validate_pairs_tbl("bad"), "data frame")

  bad_phase <- out
  bad_phase$phase <- "phaseX"
  expect_error(pairwiseLLM:::validate_pairs_tbl(bad_phase), "phase")

  bad_type <- out
  bad_type$utility <- "nope"
  expect_error(pairwiseLLM:::validate_pairs_tbl(bad_type), "utility")
})

test_that("adaptive schemas reject invalid results and failed attempts", {
  results <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    better_id = "A",
    winner_pos = 1L,
    phase = "phase1",
    iter = 1L,
    received_at = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test"
  )

  results_bad <- results
  results_bad$better_id <- NA_character_
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "better_id")

  results_bad <- results
  results_bad$winner_pos <- 3L
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "winner_pos")

  results_bad <- results
  results_bad$winner_pos <- 2L
  expect_error(pairwiseLLM:::validate_results_tbl(results_bad), "winner_pos")

  expect_error(pairwiseLLM:::validate_results_tbl("bad"), "data frame")

  failed <- tibble::tibble(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    phase = "phase1",
    iter = 1L,
    attempted_at = as.POSIXct("2026-01-03 00:00:00", tz = "UTC"),
    backend = "openai",
    model = "gpt-test",
    error_code = "bad_code",
    error_detail = "oops"
  )
  expect_error(pairwiseLLM:::validate_failed_attempts_tbl(failed), "error_code")
  expect_error(pairwiseLLM:::validate_failed_attempts_tbl("bad"), "data frame")
})

test_that("adaptive state helper and validation branches are covered", {
  expect_equal(pairwiseLLM:::.adaptive_unordered_keys("A"), character())

  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  expect_silent(pairwiseLLM:::validate_state(state))

  expect_error(
    pairwiseLLM:::adaptive_state_new(tibble::tibble(ID = "A"), config = list()),
    "must contain columns"
  )

  dup_samples <- tibble::tibble(ID = c("A", "A"), text = c("x", "y"))
  expect_error(pairwiseLLM:::adaptive_state_new(dup_samples, config = list()), "unique")

  bad <- state
  bad$schema_version <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "schema_version")

  bad <- state
  bad$ids <- 1:2
  expect_error(pairwiseLLM:::validate_state(bad), "ids")

  bad <- state
  bad$N <- 2.5
  expect_error(pairwiseLLM:::validate_state(bad), "N")

  bad <- state
  bad$N <- 3L
  expect_error(pairwiseLLM:::validate_state(bad), "N")

  bad <- state
  bad$texts <- c(A = 1, B = 2)
  expect_error(pairwiseLLM:::validate_state(bad), "texts")

  bad <- state
  names(bad$texts) <- c("B", "A")
  expect_error(pairwiseLLM:::validate_state(bad), "names")

  bad <- state
  bad$deg <- as.numeric(bad$deg)
  expect_error(pairwiseLLM:::validate_state(bad), "deg")

  bad <- state
  bad$pos1[["A"]] <- 1L
  expect_error(pairwiseLLM:::validate_state(bad), "deg")

  bad <- state
  bad$imb <- bad$pos1 - bad$pos2 + 1L
  expect_error(pairwiseLLM:::validate_state(bad), "imb")

  bad <- state
  bad$unordered_count <- as.numeric(bad$unordered_count)
  expect_error(pairwiseLLM:::validate_state(bad), "unordered_count")

  bad <- state
  bad$ordered_seen <- 1
  expect_error(pairwiseLLM:::validate_state(bad), "ordered_seen")

  bad <- state
  bad$history_pairs <- tibble::tibble(foo = 1)
  expect_error(pairwiseLLM:::validate_state(bad), "pairs")

  bad <- state
  bad$comparisons_scheduled <- 1.2
  expect_error(pairwiseLLM:::validate_state(bad), "comparisons_scheduled")

  bad <- state
  bad$comparisons_observed <- 1.2
  expect_error(pairwiseLLM:::validate_state(bad), "comparisons_observed")

  bad <- state
  bad$comparisons_scheduled <- 1L
  expect_error(pairwiseLLM:::validate_state(bad), "comparisons_scheduled")

  bad <- state
  bad$comparisons_observed <- 1L
  expect_error(pairwiseLLM:::validate_state(bad), "comparisons_observed")

  good_pairs <- pairwiseLLM:::as_pairs_tbl(
    pair_uid = "A:B#1",
    unordered_key = "A:B",
    ordered_key = "A:B",
    A_id = "A",
    B_id = "B",
    A_text = "alpha",
    B_text = "beta",
    phase = "phase1",
    iter = 1L,
    created_at = as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  )
  bad <- state
  bad$history_pairs <- good_pairs
  bad$comparisons_scheduled <- 1L
  bad$comparisons_observed <- 1L
  bad$history_results <- pairwiseLLM:::.adaptive_empty_results_tbl()
  expect_error(pairwiseLLM:::validate_state(bad), "history_results")

  bad <- state
  bad$phase <- 1
  expect_error(pairwiseLLM:::validate_state(bad), "phase")

  bad <- state
  bad$iter <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "iter")

  bad <- state
  bad$budget_max <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "budget_max")

  bad <- state
  bad$M1_target <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "M1_target")

  bad <- state
  bad$last_check_at <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "last_check_at")

  bad <- state
  bad$stop_candidate <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "stop_candidate")

  bad <- state
  bad$checks_passed_in_row <- 1.5
  expect_error(pairwiseLLM:::validate_state(bad), "checks_passed_in_row")
})

test_that("adaptive constraints cover key helpers, duplicates, and exposure", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())

  state_empty <- state
  state_empty$unordered_count <- integer()
  expect_equal(pairwiseLLM:::pair_uid_from_state(state_empty, "A:B"), "A:B#1")

  state_missing <- state
  state_missing$unordered_count <- c("X:Y" = 1L)
  expect_equal(pairwiseLLM:::pair_uid_from_state(state_missing, "A:B"), "A:B#1")

  state_named <- state
  state_named$unordered_count <- c("A:B" = 1L)
  expect_equal(pairwiseLLM:::pair_uid_from_state(state_named, "A:B"), "A:B#2")

  state_counts <- state
  state_counts$unordered_count <- integer()
  expect_true(pairwiseLLM:::duplicate_allowed(state_counts, "A", "B"))

  state_env <- state
  state_env$unordered_count <- c("A:B" = 1L)
  state_env$ordered_seen <- new.env(parent = emptyenv())
  state_env$ordered_seen[["B:A"]] <- TRUE
  expect_true(pairwiseLLM:::duplicate_allowed(state_env, "A", "B"))
  state_env$ordered_seen[["A:B"]] <- TRUE
  expect_false(pairwiseLLM:::duplicate_allowed(state_env, "A", "B"))

  expect_error(pairwiseLLM:::record_presentation(state, "A", "Z"), "ids")

  state_env <- state
  state_env$unordered_count <- integer()
  state_env$ordered_seen <- new.env(parent = emptyenv())
  state_env <- pairwiseLLM:::record_presentation(state_env, "A", "B")
  expect_equal(state_env$unordered_count[["A:B"]], 1L)
  expect_true(isTRUE(state_env$ordered_seen[["A:B"]]))
})

test_that("adaptive_with_seed preserves RNG state", {
  withr::local_seed(1)
  baseline_pre <- stats::runif(3)
  baseline_post <- stats::runif(3)

  withr::local_seed(1)
  pre <- stats::runif(3)
  pairwiseLLM:::.adaptive_with_seed(42, stats::runif(1))
  post <- stats::runif(3)

  expect_equal(pre, baseline_pre)
  expect_equal(post, baseline_post)
})

test_that("position balancing covers all ordering branches", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "beta")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list())
  state$pos1[["A"]] <- 0L
  state$pos2[["A"]] <- 2L
  state$pos1[["B"]] <- 1L
  state$pos2[["B"]] <- 0L
  state$deg <- state$pos1 + state$pos2
  state$imb <- state$pos1 - state$pos2

  out <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B")
  expect_equal(out$A_id, "A")
  expect_equal(out$B_id, "B")

  out1 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 1)
  out2 <- pairwiseLLM:::choose_order_with_position_balance(state, "A", "B", seed = 2)
  expect_true(all(c(out1$A_id, out2$A_id) %in% c("A", "B")))
})

test_that("core and batch sizing helpers cover edge cases", {
  cores <- testthat::with_mocked_bindings(
    detectCores = function(logical = FALSE) {
      if (isTRUE(logical)) 4L else NA_integer_
    },
    .package = "parallel",
    {
      pairwiseLLM:::detect_physical_cores()
    }
  )
  expect_equal(cores, 4L)

  cores <- testthat::with_mocked_bindings(
    detectCores = function(logical = FALSE) stop("boom"),
    .package = "parallel",
    {
      pairwiseLLM:::detect_physical_cores()
    }
  )
  expect_equal(cores, 1L)

  expect_error(pairwiseLLM:::compute_core_budget(core_fraction = -1), "core_fraction")
  expect_error(pairwiseLLM:::compute_core_budget(min_cores = 0), "min_cores")
  expect_error(pairwiseLLM:::compute_core_budget(max_cores = 0), "max_cores")

  budget <- testthat::with_mocked_bindings(
    pairwiseLLM:::compute_core_budget(core_fraction = 1, max_cores = 4),
    detect_physical_cores = function() 10L
  )
  expect_equal(budget, 4L)

  budget <- testthat::with_mocked_bindings(
    pairwiseLLM:::compute_core_budget(core_fraction = 0.2, min_cores = 2),
    detect_physical_cores = function() 2L
  )
  expect_equal(budget, 2L)

  sizes <- pairwiseLLM:::compute_batch_sizes(100, overrides = list(BATCH2 = 999L, BATCH3 = 111L))
  expect_equal(sizes$BATCH2, 999L)
  expect_equal(sizes$BATCH3, 111L)
})

test_that("normalize_results covers duplicate custom_id and suffixed columns", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    pair_uid = "pair-dup"
  )
  raw <- tibble::tibble(
    custom_id = c("pair-dup", "pair-dup"),
    ID1 = c("A", "A"),
    ID2 = c("B", "B"),
    better_id = c("A", "A"),
    ordered_occurrence_index = c(1, 2)
  )

  out <- pairwiseLLM:::.normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_true(any(out$failed_attempts$error_code == "http_error"))

  pairs2 <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta"
  )
  raw2 <- tibble::tibble(
    better_id = "A",
    `ID1.x` = "A",
    `ID1.y` = "A",
    `pair_uid.y` = "A:B#1"
  )

  out2 <- pairwiseLLM:::.normalize_llm_results(
    raw = raw2,
    pairs = pairs2,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_equal(out2$results$ID1, "A")
  expect_equal(out2$results$pair_uid, "A:B#1")
})

test_that("normalize_results coerces iter fields for results and failed attempts", {
  pairs <- tibble::tibble(
    ID1 = "A",
    text1 = "alpha",
    ID2 = "B",
    text2 = "beta",
    iter = 2
  )
  raw <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "A"
  )
  out <- pairwiseLLM:::.normalize_llm_results(
    raw = raw,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_type(out$results$iter, "integer")

  raw_bad <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    better_id = "C"
  )
  out_bad <- pairwiseLLM:::.normalize_llm_results(
    raw = raw_bad,
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    include_raw = FALSE
  )
  expect_type(out_bad$failed_attempts$iter, "integer")
})

test_that("failed_attempts helper validates required fields and carries metadata", {
  expect_error(
    pairwiseLLM:::.pairwiseLLM_failed_attempts_from_pairs(
      pairs = tibble::tibble(foo = "A"),
      backend = "openai",
      model = "gpt-test",
      error_code = "http_error",
      error_detail = "HTTP 500"
    ),
    "must contain columns"
  )

  pairs <- tibble::tibble(
    ID1 = "A",
    ID2 = "B",
    phase = "phase1",
    iter = 3,
    pair_uid = "A:B#1"
  )
  out <- pairwiseLLM:::.pairwiseLLM_failed_attempts_from_pairs(
    pairs = pairs,
    backend = "openai",
    model = "gpt-test",
    error_code = "http_error",
    error_detail = "HTTP 500"
  )
  expect_equal(out$pair_uid, "A:B#1")
  expect_equal(out$phase, "phase1")
  expect_type(out$iter, "integer")
})
