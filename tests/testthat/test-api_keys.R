# =====================================================================
#   test-api_keys.R
#   Tests for .get_api_key() and check_llm_api_keys()
# =====================================================================

testthat::test_that(".get_api_key prefers explicit api_key over env var", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old), add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "FROM_ENV")

  res <- .get_api_key(
    api_key = "EXPLICIT",
    env_var = "OPENAI_API_KEY",
    service = "OpenAI"
  )

  testthat::expect_equal(res, "EXPLICIT")
})

testthat::test_that(".get_api_key falls back to env var when api_key is NULL", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old), add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "FROM_ENV")

  res <- .get_api_key(
    api_key = NULL,
    env_var = "OPENAI_API_KEY",
    service = "OpenAI"
  )

  testthat::expect_equal(res, "FROM_ENV")
})

testthat::test_that(".get_api_key errors with informative message when nothing is set", {
  old <- Sys.getenv("SOME_FAKE_KEY", unset = "")
  on.exit(Sys.setenv(SOME_FAKE_KEY = old), add = TRUE)

  Sys.setenv(SOME_FAKE_KEY = "")

  testthat::expect_error(
    .get_api_key(
      api_key = NULL,
      env_var = "SOME_FAKE_KEY",
      service = "FakeService"
    ),
    regexp = "No API key found for FakeService"
  )
})

testthat::test_that("check_llm_api_keys returns expected structure when keys are missing", {
  old_openai     <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic  <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  on.exit({
    Sys.setenv(OPENAI_API_KEY = old_openai)
    Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
  }, add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "")
  Sys.setenv(ANTHROPIC_API_KEY = "")

  status <- check_llm_api_keys(verbose = FALSE)

  testthat::expect_s3_class(status, "tbl_df")
  testthat::expect_true(all(c("backend", "service", "env_var", "has_key") %in% names(status)))

  # Ensure we have at least the expected backends
  testthat::expect_true(all(c("openai", "anthropic") %in% status$backend))

  openai_row <- status[status$backend == "openai", , drop = FALSE]
  anth_row   <- status[status$backend == "anthropic", , drop = FALSE]

  testthat::expect_equal(openai_row$env_var, "OPENAI_API_KEY")
  testthat::expect_equal(anth_row$env_var, "ANTHROPIC_API_KEY")

  testthat::expect_false(openai_row$has_key)
  testthat::expect_false(anth_row$has_key)
})

testthat::test_that("check_llm_api_keys reports keys as present when env vars are set", {
  old_openai     <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic  <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  on.exit({
    Sys.setenv(OPENAI_API_KEY = old_openai)
    Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
  }, add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "OPENAI_KEY_FOR_TEST")
  Sys.setenv(ANTHROPIC_API_KEY = "ANTHROPIC_KEY_FOR_TEST")

  status <- check_llm_api_keys(verbose = FALSE)

  openai_row <- status[status$backend == "openai", , drop = FALSE]
  anth_row   <- status[status$backend == "anthropic", , drop = FALSE]

  testthat::expect_true(openai_row$has_key)
  testthat::expect_true(anth_row$has_key)
})
