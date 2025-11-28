# ---------------------------------------------------------------------
# Defaults: backend and endpoint
# ---------------------------------------------------------------------

testthat::test_that("llm_compare_pair uses default backend and endpoint", {
  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  ID1   <- "S01"
  ID2   <- "S02"
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
    content           = "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
    better_sample     = "SAMPLE_1",
    better_id         = ID1,
    prompt_tokens     = 10,
    completion_tokens = 5,
    total_tokens      = 15
  )

  calls <- list()

  testthat::with_mocked_bindings(
    openai_compare_pair_live = function(
    ID1,
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

      # Defaults should resolve to backend = "openai", endpoint = "chat.completions"
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

  td   <- trait_description("overall_quality")
  tmpl <- set_prompt_template()

  fake_res <- tibble::tibble(
    custom_id         = c("LIVE_S01_vs_S02", "LIVE_S03_vs_S04"),
    ID1               = pairs$ID1,
    ID2               = pairs$ID2,
    model             = "gpt-4.1",
    object_type       = "chat.completion",
    status_code       = c(200L, 200L),
    error_message     = c(NA_character_, NA_character_),
    content           = c(
      "<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>",
      "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"
    ),
    better_sample     = c("SAMPLE_1", "SAMPLE_2"),
    better_id         = c("S01", "S04"),
    prompt_tokens     = c(10, 11),
    completion_tokens = c(5, 6),
    total_tokens      = c(15, 17)
  )

  calls <- list()

  testthat::with_mocked_bindings(
    submit_openai_pairs_live = function(
    pairs,
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
    ...
    ) {
      calls <<- append(calls, list(list(
        pairs           = pairs,
        model           = model,
        trait_name      = trait_name,
        trait_description = trait_description,
        prompt_template = prompt_template,
        endpoint        = endpoint,
        api_key         = api_key,
        verbose         = verbose,
        status_every    = status_every,
        progress        = progress,
        include_raw     = include_raw,
        dots            = list(...)
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

      # Defaults should resolve to backend = "openai", endpoint = "chat.completions"
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
