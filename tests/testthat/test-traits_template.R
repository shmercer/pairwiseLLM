test_that("trait_description returns built-in descriptions", {
  overall <- trait_description("overall_quality")
  org     <- trait_description("organization")

  expect_type(overall, "character")
  expect_type(org, "character")

  expect_true(grepl("overall quality", overall, ignore.case = TRUE))
  expect_true(grepl("organization of the writing", org, ignore.case = TRUE))
})

test_that("trait_description returns custom string when provided", {
  custom_text <- "Clarity of argument and ideas."
  out <- trait_description(custom = custom_text)

  expect_identical(out, custom_text)
})

test_that("trait_description errors on invalid name when no custom text", {
  expect_error(
    trait_description(name = "not_a_trait")
  )
})

test_that("set_prompt_template returns a default template with required placeholders", {
  tmpl <- set_prompt_template()

  expect_type(tmpl, "character")
  # No backslashes needed with fixed = TRUE
  expect_true(grepl("{TRAIT_DESCRIPTION}", tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_1}",        tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_2}",        tmpl, fixed = TRUE))
})

test_that("set_prompt_template accepts a custom template string", {
  custom_tmpl <- "
Trait: {TRAIT_DESCRIPTION}
A: {SAMPLE_1}
B: {SAMPLE_2}
"

  tmpl <- set_prompt_template(template = custom_tmpl)
  expect_identical(tmpl, custom_tmpl)
})

test_that("set_prompt_template reads template from file", {
  tmp <- tempfile("pairwiseLLM-template-", fileext = ".txt")
  custom_tmpl <- "
Compare {SAMPLE_1} and {SAMPLE_2} on {TRAIT_DESCRIPTION}.
"
  writeLines(custom_tmpl, con = tmp)

  tmpl <- set_prompt_template(file = tmp)
  expect_identical(tmpl, custom_tmpl)
})

test_that("set_prompt_template errors if required placeholders are missing", {
  bad_tmpl <- "
This template has only SAMPLE_1: {SAMPLE_1} and no others.
"

  expect_error(
    set_prompt_template(template = bad_tmpl),
    "missing required placeholder"
  )

  tmp <- tempfile("pairwiseLLM-template-bad-", fileext = ".txt")
  writeLines(bad_tmpl, con = tmp)

  expect_error(
    set_prompt_template(file = tmp),
    "missing required placeholder"
  )
})

test_that("build_prompt substitutes placeholders with given values", {
  tmpl <- "
<TRAIT_DESCRIPTION>
{TRAIT_DESCRIPTION}
</TRAIT_DESCRIPTION>

A: {SAMPLE_1}
B: {SAMPLE_2}
"

  trait <- "Overall writing quality."
  text1 <- "This is the first sample."
  text2 <- "This is the second sample."

  prompt <- build_prompt(
    template   = tmpl,
    trait_desc = trait,
    text1      = text1,
    text2      = text2
  )

  expect_true(grepl(trait, prompt, fixed = TRUE))
  expect_true(grepl(text1, prompt, fixed = TRUE))
  expect_true(grepl(text2, prompt, fixed = TRUE))

  # No unsubstituted placeholders should remain
  expect_false(grepl("{TRAIT_DESCRIPTION}", prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_1}",        prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_2}",        prompt, fixed = TRUE))
})

test_that("build_prompt works with default template and built-in trait", {
  tmpl  <- set_prompt_template()
  trait <- trait_description("overall_quality")

  text1 <- "Sample 1 text."
  text2 <- "Sample 2 text."

  prompt <- build_prompt(tmpl, trait, text1, text2)

  expect_true(grepl("Sample 1 text.", prompt, fixed = TRUE))
  expect_true(grepl("Sample 2 text.", prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE",  prompt, fixed = TRUE))
})

test_that("build_prompt works end-to-end with example_writing_samples", {
  data("example_writing_samples", package = "pairwiseLLM")

  tmpl  <- set_prompt_template()
  trait <- trait_description("overall_quality")
  text1 <- example_writing_samples$text[1]
  text2 <- example_writing_samples$text[2]

  prompt <- build_prompt(
    template   = tmpl,
    trait_desc = trait,
    text1      = text1,
    text2      = text2
  )

  expect_true(grepl(text1, prompt, fixed = TRUE))
  expect_true(grepl(text2, prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE", prompt, fixed = TRUE))
})
