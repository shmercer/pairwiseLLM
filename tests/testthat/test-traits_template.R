test_that("trait_description returns built-in name and description", {
  overall <- trait_description("overall_quality")
  org     <- trait_description("organization")

  # Now returns a list with name + description
  expect_type(overall, "list")
  expect_type(org, "list")

  expect_named(overall, c("name", "description"))
  expect_named(org,     c("name", "description"))

  expect_true(is.character(overall$name))
  expect_true(is.character(overall$description))
  expect_true(is.character(org$name))
  expect_true(is.character(org$description))

  # Content checks (not too brittle)
  expect_true(grepl("overall", overall$name, ignore.case = TRUE))
  expect_true(grepl("quality", overall$name, ignore.case = TRUE))
  expect_true(grepl("overall", overall$description, ignore.case = TRUE))

  expect_true(grepl("organ", org$name, ignore.case = TRUE))         # organization / organized
  expect_true(grepl("organ", org$description, ignore.case = TRUE))  # organization of writing, etc.
})

test_that("trait_description returns custom name and description when provided", {
  custom_text <- "Clarity of argument and ideas."
  out <- trait_description(
    custom_name        = "Clarity",
    custom_description = custom_text
  )

  expect_type(out, "list")
  expect_named(out, c("name", "description"))

  expect_identical(out$name, "Clarity")
  expect_identical(out$description, custom_text)
})

test_that("trait_description errors on invalid name when no custom text", {
  expect_error(
    trait_description(name = "not_a_trait")
  )
})

test_that("set_prompt_template returns a default template with required placeholders", {
  tmpl <- set_prompt_template()

  expect_type(tmpl, "character")

  # Template must contain all required placeholders
  expect_true(grepl("{TRAIT_NAME}",        tmpl, fixed = TRUE))
  expect_true(grepl("{TRAIT_DESCRIPTION}", tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_1}",          tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_2}",          tmpl, fixed = TRUE))
})

test_that("set_prompt_template accepts a custom template string", {
  custom_tmpl <- "
Trait: {TRAIT_NAME}
Definition: {TRAIT_DESCRIPTION}
A: {SAMPLE_1}
B: {SAMPLE_2}
"

  tmpl <- set_prompt_template(template = custom_tmpl)
  expect_identical(tmpl, custom_tmpl)
})

test_that("set_prompt_template reads template from file", {
  tmp <- tempfile("pairwiseLLM-template-", fileext = ".txt")
  custom_tmpl <- "
Compare {SAMPLE_1} and {SAMPLE_2} on {TRAIT_NAME}.
Definition: {TRAIT_DESCRIPTION}
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

test_that("build_prompt substitutes trait name, description, and samples", {
  tmpl <- "
<TRAIT_NAME>{TRAIT_NAME}</TRAIT_NAME>
<TRAIT_DESCRIPTION>{TRAIT_DESCRIPTION}</TRAIT_DESCRIPTION>

A: {SAMPLE_1}
B: {SAMPLE_2}
"

  trait_name <- "Overall Quality"
  trait_desc <- "Overall quality of the writing."
  text1 <- "This is the first sample."
  text2 <- "This is the second sample."

  prompt <- build_prompt(
    template   = tmpl,
    trait_name = trait_name,
    trait_desc = trait_desc,
    text1      = text1,
    text2      = text2
  )

  # Substitutions present
  expect_true(grepl(trait_name, prompt, fixed = TRUE))
  expect_true(grepl(trait_desc, prompt, fixed = TRUE))
  expect_true(grepl(text1,      prompt, fixed = TRUE))
  expect_true(grepl(text2,      prompt, fixed = TRUE))

  # No unsubstituted placeholders should remain
  expect_false(grepl("{TRAIT_NAME}",        prompt, fixed = TRUE))
  expect_false(grepl("{TRAIT_DESCRIPTION}", prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_1}",          prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_2}",          prompt, fixed = TRUE))
})

test_that("build_prompt works with default template and built-in trait", {
  tmpl  <- set_prompt_template()
  td    <- trait_description("overall_quality")

  text1 <- "Sample 1 text."
  text2 <- "Sample 2 text."

  prompt <- build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = text1,
    text2      = text2
  )

  expect_true(grepl("Sample 1 text.", prompt, fixed = TRUE))
  expect_true(grepl("Sample 2 text.", prompt, fixed = TRUE))
  expect_true(grepl(td$name,          prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE",  prompt, fixed = TRUE))
})

test_that("build_prompt works end-to-end with example_writing_samples", {
  data("example_writing_samples", package = "pairwiseLLM")

  tmpl <- set_prompt_template()
  td   <- trait_description("overall_quality")
  text1 <- example_writing_samples$text[1]
  text2 <- example_writing_samples$text[2]

  prompt <- build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = text1,
    text2      = text2
  )

  expect_true(grepl(text1,        prompt, fixed = TRUE))
  expect_true(grepl(text2,        prompt, fixed = TRUE))
  expect_true(grepl(td$name,      prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE", prompt, fixed = TRUE))
})
