test_that("submit_openai_pairs_live retries plan setup once", {
  pairs <- tibble::tibble(
    custom_id = c("p1", "p2"),
    ID1 = c("A", "B"),
    text1 = c("a", "b"),
    ID2 = c("B", "C"),
    text2 = c("b", "c")
  )

  # Make the first plan call fail, second succeed.
  calls <- 0L
  testthat::local_mocked_bindings(
    .future_plan = function(strategy, ...) {
      calls <<- calls + 1L
      # The second call attempts to set the multisession plan.
      if (calls == 2L) {
        stop("validate failed")
      }
      NULL
    },
    .future_lapply = function(X, FUN, ...) {
      lapply(X, FUN)
    },
    openai_compare_pair_live = function(...) {
      tibble::tibble(
        ID1 = "A",
        ID2 = "B",
        model = "mock",
        object_type = "chat.completion",
        status_code = 200L,
        error_message = NA_character_,
        thoughts = NA_character_,
        content = NA_character_,
        better_sample = "ID1",
        better_id = "A",
        prompt_tokens = 0,
        completion_tokens = 0,
        total_tokens = 0
      )
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::submit_openai_pairs_live(
    pairs = pairs,
    api_key = "sk-test",
    parallel = TRUE,
    workers = 2L,
    verbose = FALSE,
    return_mode = "all"
  )

  expect_true(is.list(out))
  expect_equal(nrow(out$results), 2L)
  expect_true(calls >= 3L) # get old plan + first try + retry
})

test_that("submit_anthropic_pairs_live falls back when parallel chunk fails", {
  pairs <- tibble::tibble(
    custom_id = c("p1"),
    ID1 = "A",
    text1 = "a",
    ID2 = "B",
    text2 = "b"
  )

  testthat::local_mocked_bindings(
    .future_plan = function(strategy, ...) NULL,
    .future_lapply = function(...) {
      stop("Parallel Save Failure: disk full")
    },
    anthropic_compare_pair_live = function(...) {
      tibble::tibble(
        ID1 = "A",
        ID2 = "B",
        model = "mock",
        object_type = "message",
        status_code = 200L,
        error_message = NA_character_,
        thoughts = NA_character_,
        content = NA_character_,
        better_sample = "ID2",
        better_id = "B",
        prompt_tokens = 0,
        completion_tokens = 0,
        total_tokens = 0
      )
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::submit_anthropic_pairs_live(
    pairs = pairs,
    api_key = "sk-test",
    parallel = TRUE,
    workers = 2L,
    verbose = FALSE,
    return_mode = "all"
  )

  expect_true(is.list(out))
  expect_equal(out$results$better_sample[[1]], "ID2")
})
