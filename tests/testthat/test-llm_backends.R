# =====================================================================
#   test-llm_backends.R
#   Tests for llm_compare_pair() and submit_llm_pairs()
# =====================================================================

# ---------------------------------------------------------------------
# Defaults: backend and endpoint
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair uses default backend and endpoint", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "gpt-4.1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(ID1,
                                        text1,
                                        ID2,
                                        text2,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        endpoint,
                                        api_key,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      # Note: backend and endpoint are omitted, using defaults
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Defaults should resolve to backend = "openai", endpoint =
      # "chat.completions"
      testthat::expect_equal(call$endpoint, "chat.completions")
      # api_key default is from Sys.getenv("OPENAI_API_KEY"); we only check that
      # an argument was passed through (value may vary by environment).
      testthat::expect_true("api_key" %in% names(call))

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------

testthat::test_that("submit_llm_pairs uses default backend and endpoint", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "gpt-4.1",
    object_type = "chat.completion",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c(NA_character_, NA_character_),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(10, 11),
    completion_tokens = c(5, 6),
    total_tokens = c(15, 17)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_openai_pairs_live = function(pairs,
                                        model,
                                        trait_name,
                                        trait_description,
                                        prompt_template,
                                        endpoint,
                                        api_key,
                                        verbose,
                                        status_every,
                                        progress,
                                        include_raw,
                                        ...) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        endpoint          = endpoint,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      # Note: backend and endpoint are omitted, using defaults
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "gpt-4.1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Defaults should resolve to backend = "openai", endpoint =
      # "chat.completions"
      testthat::expect_equal(call$endpoint, "chat.completions")
      testthat::expect_true("api_key" %in% names(call))
      # Defaults for verbose / status_every / progress / include_raw
      testthat::expect_true(call$verbose)
      testthat::expect_equal(call$status_every, 1L)
      testthat::expect_true(call$progress)
      testthat::expect_false(call$include_raw)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: anthropic backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to anthropic backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "claude-3-5-sonnet-latest",
    object_type       = "message",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Some internal reasoning.",
    content           = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_2",
    better_id         = ID2,
    prompt_tokens     = 20,
    completion_tokens = 10,
    total_tokens      = 30
  )

  calls <- list()

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      api_key,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = "ANTH_KEY",
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Should not receive an endpoint argument for anthropic
      testthat::expect_false("endpoint" %in% names(call))

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "claude-3-5-sonnet-latest")
      testthat::expect_equal(call$api_key, "ANTH_KEY")
      # include_thoughts should be forwarded via ...
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("llm_compare_pair uses Anthropic env var when
                    api_key is NULL", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "claude-3-5-sonnet-latest",
    object_type       = "message",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()
  old_env <- Sys.getenv("ANTHROPIC_API_KEY", unset = "")
  on.exit(Sys.setenv(ANTHROPIC_API_KEY = old_env), add = TRUE)
  Sys.setenv(ANTHROPIC_API_KEY = "ENV_ANTH_KEY")

  testthat::with_mocked_bindings(
    anthropic_compare_pair_live = function(ID1,
                                           text1,
                                           ID2,
                                           text2,
                                           model,
                                           trait_name,
                                           trait_description,
                                           prompt_template,
                                           api_key,
                                           include_raw,
                                           ...) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = NULL,
        include_raw       = FALSE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # When api_key is NULL, llm_compare_pair should fall back to the env var
      testthat::expect_equal(call$api_key, "ENV_ANTH_KEY")

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to anthropic backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "claude-3-5-sonnet-latest",
    object_type = "message",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Thoughts 1", "Thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(20, 22),
    completion_tokens = c(10, 12),
    total_tokens = c(30, 34)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_anthropic_pairs_live = function(
      pairs,
      model,
      trait_name,
      trait_description,
      prompt_template,
      api_key,
      verbose,
      status_every,
      progress,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "claude-3-5-sonnet-latest",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "anthropic",
        api_key           = "ANTH_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "claude-3-5-sonnet-latest")
      testthat::expect_equal(call$api_key, "ANTH_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: gemini backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to gemini backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "gemini-2.0-pro-exp",
    object_type       = "generateContent",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Gemini thinking text.",
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 30,
    completion_tokens = 15,
    total_tokens      = 45
  )

  calls <- list()

  testthat::with_mocked_bindings(
    gemini_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      api_key,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "gemini-2.0-pro-exp",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "gemini",
        api_key           = "GEMINI_KEY",
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Should not receive an endpoint argument for gemini
      testthat::expect_false("endpoint" %in% names(call))

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "gemini-2.0-pro-exp")
      testthat::expect_equal(call$api_key, "GEMINI_KEY")
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to gemini backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "gemini-2.0-pro-exp",
    object_type = "generateContent",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Gemini thoughts 1", "Gemini thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(30, 32),
    completion_tokens = c(15, 17),
    total_tokens = c(45, 49)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_gemini_pairs_live = function(
      pairs,
      model,
      trait_name,
      trait_description,
      prompt_template,
      api_key,
      verbose,
      status_every,
      progress,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "gemini-2.0-pro-exp",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "gemini",
        api_key           = "GEMINI_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        include_thoughts  = TRUE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "gemini-2.0-pro-exp")
      testthat::expect_equal(call$api_key, "GEMINI_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)
      testthat::expect_true("include_thoughts" %in% names(call$dots))
      testthat::expect_true(call$dots$include_thoughts)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: ollama backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to ollama backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "mistral-small3.2:24b",
    object_type       = "ollama.generate",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 50,
    completion_tokens = 20,
    total_tokens      = 70
  )

  calls <- list()

  testthat::with_mocked_bindings(
    ollama_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "ollama",
        include_raw       = TRUE,
        host              = "http://127.0.0.1:11434",
        think             = TRUE,
        num_ctx           = 16384
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # Basic routing and core args
      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "mistral-small3.2:24b")
      testthat::expect_true(call$include_raw)

      # Ollama backend should not receive endpoint or api_key
      testthat::expect_false("endpoint" %in% names(call))
      testthat::expect_false("api_key" %in% names(call))

      # ... should carry Ollama-specific options
      testthat::expect_true("host" %in% names(call$dots))
      testthat::expect_true("think" %in% names(call$dots))
      testthat::expect_true("num_ctx" %in% names(call$dots))

      testthat::expect_equal(call$dots$host, "http://127.0.0.1:11434")
      testthat::expect_true(call$dots$think)
      testthat::expect_equal(call$dots$num_ctx, 16384)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to ollama backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "mistral-small3.2:24b",
    object_type = "ollama.generate",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c(NA_character_, NA_character_),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(50, 52),
    completion_tokens = c(20, 22),
    total_tokens = c(70, 74)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_ollama_pairs_live = function(
      pairs,
      model,
      trait_name,
      trait_description,
      prompt_template,
      verbose,
      status_every,
      progress,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "mistral-small3.2:24b",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "ollama",
        verbose           = FALSE,
        status_every      = 2,
        progress          = FALSE,
        include_raw       = TRUE,
        host              = "http://127.0.0.1:11434",
        think             = FALSE,
        num_ctx           = 8192
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "mistral-small3.2:24b")
      testthat::expect_false(call$verbose)
      testthat::expect_equal(call$status_every, 2L)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)

      # Ollama-specific options should be forwarded via ...
      testthat::expect_true("host" %in% names(call$dots))
      testthat::expect_true("think" %in% names(call$dots))
      testthat::expect_true("num_ctx" %in% names(call$dots))

      testthat::expect_equal(call$dots$host, "http://127.0.0.1:11434")
      testthat::expect_false(call$dots$think)
      testthat::expect_equal(call$dots$num_ctx, 8192)

      # Wrapper returns backend result
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

# ---------------------------------------------------------------------
# Routing: together backend
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair routes to together backend", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "deepseek-ai/DeepSeek-R1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = "Some internal reasoning.",
    content           = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_2",
    better_id         = ID2,
    prompt_tokens     = 20,
    completion_tokens = 10,
    total_tokens      = 30
  )

  calls <- list()

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      tag_prefix = "<BETTER_SAMPLE>",
      tag_suffix = "</BETTER_SAMPLE>",
      api_key,
      include_raw = FALSE,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        tag_prefix        = tag_prefix,
        tag_suffix        = tag_suffix,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = "TOGETHER_KEY",
        include_raw       = TRUE,
        temperature       = 0.6
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$ID1, ID1)
      testthat::expect_equal(call$ID2, ID2)
      testthat::expect_equal(call$model, "deepseek-ai/DeepSeek-R1")
      testthat::expect_equal(call$trait_name, td$name)
      testthat::expect_equal(call$trait_description, td$description)
      testthat::expect_equal(call$api_key, "TOGETHER_KEY")
      testthat::expect_true(call$include_raw)

      # Temperature should be forwarded via ...
      testthat::expect_true("temperature" %in% names(call$dots))
      testthat::expect_equal(call$dots$temperature, 0.6)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("llm_compare_pair passes NULL api_key to Together
                    backend when api_key is NULL", {
  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1 <- "S01"
  ID2 <- "S02"
  text1 <- "Text 1"
  text2 <- "Text 2"

  fake_res <- tibble::tibble(
    custom_id         = sprintf("LIVE_%s_vs_%s", ID1, ID2),
    ID1               = ID1,
    ID2               = ID2,
    model             = "deepseek-ai/DeepSeek-R1",
    object_type       = "chat.completion",
    status_code       = 200L,
    error_message     = NA_character_,
    thoughts          = NA_character_,
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()
  old_env <- Sys.getenv("TOGETHER_API_KEY", unset = "")
  on.exit(Sys.setenv(TOGETHER_API_KEY = old_env), add = TRUE)
  Sys.setenv(TOGETHER_API_KEY = "ENV_TOGETHER_KEY")

  testthat::with_mocked_bindings(
    together_compare_pair_live = function(
      ID1,
      text1,
      ID2,
      text2,
      model,
      trait_name,
      trait_description,
      prompt_template,
      tag_prefix = "<BETTER_SAMPLE>",
      tag_suffix = "</BETTER_SAMPLE>",
      api_key,
      include_raw = FALSE,
      ...
    ) {
      calls <<- append(calls, list(list(
        ID1               = ID1,
        ID2               = ID2,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- llm_compare_pair(
        ID1               = ID1,
        text1             = text1,
        ID2               = ID2,
        text2             = text2,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = NULL,
        include_raw       = FALSE
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      # At this level, api_key should still be NULL; the env var is handled
      # inside together_compare_pair_live() via .together_api_key().
      testthat::expect_true(is.null(call$api_key))

      testthat::expect_false(call$include_raw)
      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})

testthat::test_that("submit_llm_pairs routes to together backend", {
  pairs <- tibble::tibble(
    ID1   = c("S01", "S03"),
    text1 = c("Text 1", "Text 3"),
    ID2   = c("S02", "S04"),
    text2 = c("Text 2", "Text 4")
  )

  td <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1 = pairs$ID1,
    ID2 = pairs$ID2,
    model = "deepseek-ai/DeepSeek-R1",
    object_type = "chat.completion",
    status_code = c(200L, 200L),
    error_message = c(NA_character_, NA_character_),
    thoughts = c("Thoughts 1", "Thoughts 2"),
    content = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample = c("SAMPLE_1", "SAMPLE_2"),
    better_id = c("S01", "S04"),
    prompt_tokens = c(20, 22),
    completion_tokens = c(10, 12),
    total_tokens = c(30, 34)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_together_pairs_live = function(
      pairs,
      model,
      trait_name,
      trait_description,
      prompt_template,
      api_key,
      verbose,
      status_every,
      progress,
      include_raw,
      ...
    ) {
      calls <<- append(calls, list(list(
        pairs             = pairs,
        model             = model,
        trait_name        = trait_name,
        trait_description = trait_description,
        prompt_template   = prompt_template,
        api_key           = api_key,
        verbose           = verbose,
        status_every      = status_every,
        progress          = progress,
        include_raw       = include_raw,
        dots              = list(...)
      )))
      fake_res
    },
    {
      res <- submit_llm_pairs(
        pairs             = pairs,
        model             = "deepseek-ai/DeepSeek-R1",
        trait_name        = td$name,
        trait_description = td$description,
        prompt_template   = tmpl,
        backend           = "together",
        api_key           = "TOGETHER_KEY",
        verbose           = FALSE,
        progress          = FALSE,
        include_raw       = TRUE,
        temperature       = 0.6
      )

      testthat::expect_equal(length(calls), 1L)
      call <- calls[[1]]

      testthat::expect_equal(call$model, "deepseek-ai/DeepSeek-R1")
      testthat::expect_equal(call$api_key, "TOGETHER_KEY")
      testthat::expect_false(call$verbose)
      testthat::expect_false(call$progress)
      testthat::expect_true(call$include_raw)

      # Temperature forwarded via ...
      testthat::expect_true("temperature" %in% names(call$dots))
      testthat::expect_equal(call$dots$temperature, 0.6)

      testthat::expect_s3_class(res, "tbl_df")
      testthat::expect_equal(res, fake_res)
    }
  )
})
