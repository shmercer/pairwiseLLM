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

testthat::test_that(".get_api_key errors with informative message when nothing
                    is set", {
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

testthat::test_that("check_llm_api_keys returns expected structure when keys
                    are missing", {
  old_openai <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  on.exit(
    {
      Sys.setenv(OPENAI_API_KEY = old_openai)
      Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  Sys.setenv(OPENAI_API_KEY = "")
  Sys.setenv(ANTHROPIC_API_KEY = "")
  Sys.setenv(TOGETHER_API_KEY = "")

  status <- check_llm_api_keys(verbose = FALSE)

  testthat::expect_s3_class(status, "tbl_df")
  testthat::expect_true(all(c("backend", "service", "env_var", "has_key")
  %in% names(status)))

  # Ensure we have at least the expected backends
  testthat::expect_true(all(c("openai", "anthropic", "together") %in%
    status$backend))

  openai_row <- status[status$backend == "openai", , drop = FALSE]
  anth_row <- status[status$backend == "anthropic", , drop = FALSE]
  together_row <- status[status$backend == "together", , drop = FALSE]

  testthat::expect_equal(openai_row$env_var, "OPENAI_API_KEY")
  testthat::expect_equal(anth_row$env_var, "ANTHROPIC_API_KEY")
  testthat::expect_equal(together_row$env_var, "TOGETHER_API_KEY")

  testthat::expect_false(openai_row$has_key)
  testthat::expect_false(anth_row$has_key)
  testthat::expect_false(together_row$has_key)
})

testthat::test_that("check_llm_api_keys reports keys as present when env
                    vars are set", {
  old_openai <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  on.exit(
    {
      Sys.setenv(OPENAI_API_KEY = old_openai)
      Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  Sys.setenv(OPENAI_API_KEY = "OPENAI_KEY_FOR_TEST")
  Sys.setenv(ANTHROPIC_API_KEY = "ANTHROPIC_KEY_FOR_TEST")
  Sys.setenv(TOGETHER_API_KEY = "TOGETHER_KEY_FOR_TEST")

  status <- check_llm_api_keys(verbose = FALSE)

  openai_row <- status[status$backend == "openai", , drop = FALSE]
  anth_row <- status[status$backend == "anthropic", , drop = FALSE]
  together_row <- status[status$backend == "together", , drop = FALSE]

  testthat::expect_true(openai_row$has_key)
  testthat::expect_true(anth_row$has_key)
  testthat::expect_true(together_row$has_key)
})

# ---------------------------------------------------------------------
# Additional coverage for check_llm_api_keys() message branches
# ---------------------------------------------------------------------

testthat::test_that("check_llm_api_keys(verbose = TRUE) prints all-set message", {
  old_openai <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  old_gemini <- Sys.getenv("GEMINI_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")

  on.exit(
    {
      Sys.setenv(OPENAI_API_KEY = old_openai)
      Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
      Sys.setenv(GEMINI_API_KEY = old_gemini)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  Sys.setenv(
    OPENAI_API_KEY    = "OPENAI_KEY_FOR_TEST",
    ANTHROPIC_API_KEY = "ANTHROPIC_KEY_FOR_TEST",
    GEMINI_API_KEY    = "GEMINI_KEY_FOR_TEST",
    TOGETHER_API_KEY  = "TOGETHER_KEY_FOR_TEST"
  )

  msgs <- testthat::capture_messages({
    status <- check_llm_api_keys(verbose = TRUE)
  })

  testthat::expect_s3_class(status, "tbl_df")
  testthat::expect_true(any(grepl("^All known LLM API keys are set:", msgs)))
})

testthat::test_that("check_llm_api_keys(verbose = TRUE) prints none-set guidance", {
  old_openai <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  old_gemini <- Sys.getenv("GEMINI_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")

  on.exit(
    {
      Sys.setenv(OPENAI_API_KEY = old_openai)
      Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
      Sys.setenv(GEMINI_API_KEY = old_gemini)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  Sys.setenv(
    OPENAI_API_KEY    = "",
    ANTHROPIC_API_KEY = "",
    GEMINI_API_KEY    = "",
    TOGETHER_API_KEY  = ""
  )

  msgs <- testthat::capture_messages({
    status <- check_llm_api_keys(verbose = TRUE)
  })

  testthat::expect_s3_class(status, "tbl_df")
  testthat::expect_true(any(grepl("No LLM API keys are currently set for known backends", msgs)))
  testthat::expect_true(any(grepl("OPENAI_API_KEY", msgs)))
  testthat::expect_true(any(grepl("ANTHROPIC_API_KEY", msgs)))
  testthat::expect_true(any(grepl("GEMINI_API_KEY", msgs)))
  testthat::expect_true(any(grepl("TOGETHER_API_KEY", msgs)))
})

testthat::test_that("check_llm_api_keys(verbose = TRUE) prints mixed status per backend", {
  old_openai <- Sys.getenv("OPENAI_API_KEY", unset = "")
  old_anthropic <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  old_gemini <- Sys.getenv("GEMINI_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")

  on.exit(
    {
      Sys.setenv(OPENAI_API_KEY = old_openai)
      Sys.setenv(ANTHROPIC_API_KEY = old_anthropic)
      Sys.setenv(GEMINI_API_KEY = old_gemini)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  # Only OpenAI and Together set
  Sys.setenv(
    OPENAI_API_KEY    = "OPENAI_KEY_FOR_TEST",
    ANTHROPIC_API_KEY = "",
    GEMINI_API_KEY    = "",
    TOGETHER_API_KEY  = "TOGETHER_KEY_FOR_TEST"
  )

  msgs <- testthat::capture_messages({
    status <- check_llm_api_keys(verbose = TRUE)
  })

  testthat::expect_s3_class(status, "tbl_df")
  testthat::expect_true(any(grepl("^Some LLM API keys are not set:", msgs)))

  # Per-backend lines
  testthat::expect_true(any(grepl("OpenAI \\(openai\\): OPENAI_API_KEY is set\\.", msgs)))
  testthat::expect_true(any(grepl("Anthropic \\(anthropic\\): ANTHROPIC_API_KEY is not set", msgs)))
  testthat::expect_true(any(grepl("Google Gemini \\(gemini\\): GEMINI_API_KEY is not set", msgs)))
  testthat::expect_true(any(grepl("Together\\.ai \\(together\\): TOGETHER_API_KEY is set\\.", msgs)))
})

# ---------------------------------------------------------------------
# Coverage for .gemini_api_key() / .together_api_key()
# ---------------------------------------------------------------------

testthat::test_that(".gemini_api_key and .together_api_key prefer explicit api_key", {
  # Even if env vars are set, explicit argument wins
  old_gemini <- Sys.getenv("GEMINI_API_KEY", unset = "")
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")

  on.exit(
    {
      Sys.setenv(GEMINI_API_KEY = old_gemini)
      Sys.setenv(TOGETHER_API_KEY = old_together)
    },
    add = TRUE
  )

  Sys.setenv(GEMINI_API_KEY = "GEMINI_FROM_ENV")
  Sys.setenv(TOGETHER_API_KEY = "TOGETHER_FROM_ENV")

  testthat::expect_equal(.gemini_api_key("GEMINI_EXPLICIT"), "GEMINI_EXPLICIT")
  testthat::expect_equal(.together_api_key("TOGETHER_EXPLICIT"), "TOGETHER_EXPLICIT")
})

testthat::test_that(".gemini_api_key and .together_api_key fallback to env vars and error when missing", {
  # GEMINI: env fallback
  old_gemini <- Sys.getenv("GEMINI_API_KEY", unset = "")
  on.exit(Sys.setenv(GEMINI_API_KEY = old_gemini), add = TRUE)

  Sys.setenv(GEMINI_API_KEY = "GEMINI_FROM_ENV")
  testthat::expect_equal(.gemini_api_key(NULL), "GEMINI_FROM_ENV")

  # TOGETHER: error path when nothing set
  old_together <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  on.exit(Sys.setenv(TOGETHER_API_KEY = old_together), add = TRUE)

  Sys.setenv(TOGETHER_API_KEY = "")
  testthat::expect_error(
    .together_api_key(NULL),
    "No API key found for Together\\.ai"
  )
})

# ---------------------------------------------------------------------
# Extra small edge case: empty string api_key should behave like NULL
# ---------------------------------------------------------------------

testthat::test_that(".get_api_key treats empty string api_key like missing and uses env", {
  old <- Sys.getenv("OPENAI_API_KEY", unset = "")
  on.exit(Sys.setenv(OPENAI_API_KEY = old), add = TRUE)

  Sys.setenv(OPENAI_API_KEY = "FROM_ENV")

  res <- .get_api_key(
    api_key = "",
    env_var = "OPENAI_API_KEY",
    service = "OpenAI"
  )

  testthat::expect_equal(res, "FROM_ENV")
})

testthat::test_that("check_llm_api_keys provides verbose guidance when keys missing", {
  # Unset all keys
  withr::with_envvar(
    c("OPENAI_API_KEY" = "", "ANTHROPIC_API_KEY" = "", "GEMINI_API_KEY" = "", "TOGETHER_API_KEY" = ""),
    {
      testthat::expect_message(
        check_llm_api_keys(verbose = TRUE),
        "No LLM API keys are currently set"
      )
    }
  )

  # Set one key
  withr::with_envvar(
    c("OPENAI_API_KEY" = "TEST", "ANTHROPIC_API_KEY" = ""),
    {
      testthat::expect_message(
        check_llm_api_keys(verbose = TRUE),
        "Some LLM API keys are not set"
      )
    }
  )
})
