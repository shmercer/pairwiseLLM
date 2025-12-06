test_that("trait_description returns built-in name and description", {
  overall <- trait_description("overall_quality")
  org <- trait_description("organization")

  # Now returns a list with name + description
  expect_type(overall, "list")
  expect_type(org, "list")

  expect_named(overall, c("name", "description"))
  expect_named(org, c("name", "description"))

  expect_true(is.character(overall$name))
  expect_true(is.character(overall$description))
  expect_true(is.character(org$name))
  expect_true(is.character(org$description))

  # Content checks (not too brittle)
  expect_true(grepl("overall", overall$name, ignore.case = TRUE))
  expect_true(grepl("quality", overall$name, ignore.case = TRUE))
  expect_true(grepl("overall", overall$description, ignore.case = TRUE))

  expect_true(grepl("organ", org$name, ignore.case = TRUE))
  expect_true(grepl("organ", org$description, ignore.case = TRUE))
})

test_that("trait_description returns custom name and description
          when provided", {
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

test_that("set_prompt_template returns a default template with required
          placeholders", {
  tmpl <- set_prompt_template()

  expect_type(tmpl, "character")

  # Template must contain all required placeholders
  expect_true(grepl("{TRAIT_NAME}", tmpl, fixed = TRUE))
  expect_true(grepl("{TRAIT_DESCRIPTION}", tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_1}", tmpl, fixed = TRUE))
  expect_true(grepl("{SAMPLE_2}", tmpl, fixed = TRUE))
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
  expect_true(grepl(text1, prompt, fixed = TRUE))
  expect_true(grepl(text2, prompt, fixed = TRUE))

  # No unsubstituted placeholders should remain
  expect_false(grepl("{TRAIT_NAME}", prompt, fixed = TRUE))
  expect_false(grepl("{TRAIT_DESCRIPTION}", prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_1}", prompt, fixed = TRUE))
  expect_false(grepl("{SAMPLE_2}", prompt, fixed = TRUE))
})

test_that("build_prompt works with default template and built-in trait", {
  tmpl <- set_prompt_template()
  td <- trait_description("overall_quality")

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
  expect_true(grepl(td$name, prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE", prompt, fixed = TRUE))
})

test_that("build_prompt works end-to-end with example_writing_samples", {
  data("example_writing_samples", package = "pairwiseLLM")

  tmpl <- set_prompt_template()
  td <- trait_description("overall_quality")
  text1 <- example_writing_samples$text[1]
  text2 <- example_writing_samples$text[2]

  prompt <- build_prompt(
    template   = tmpl,
    trait_name = td$name,
    trait_desc = td$description,
    text1      = text1,
    text2      = text2
  )

  expect_true(grepl(text1, prompt, fixed = TRUE))
  expect_true(grepl(text2, prompt, fixed = TRUE))
  expect_true(grepl(td$name, prompt, fixed = TRUE))
  expect_true(grepl("BETTER_SAMPLE", prompt, fixed = TRUE))
})

# -------------------------------------------------------------------------
# New tests for txt-based built-in templates + registry API
# -------------------------------------------------------------------------

test_that("default template is loaded from inst/templates via
          get_prompt_template", {
  tmpl1 <- set_prompt_template()
  tmpl2 <- get_prompt_template("default")

  expect_type(tmpl2, "character")
  expect_identical(tmpl1, tmpl2)

  # Still has required placeholders
  expect_true(grepl("{TRAIT_NAME}", tmpl2, fixed = TRUE))
  expect_true(grepl("{TRAIT_DESCRIPTION}", tmpl2, fixed = TRUE))
  expect_true(grepl("{SAMPLE_1}", tmpl2, fixed = TRUE))
  expect_true(grepl("{SAMPLE_2}", tmpl2, fixed = TRUE))
})

test_that("list_prompt_templates includes built-in default from
          inst/templates", {
  all_names <- list_prompt_templates(
    include_builtin    = TRUE,
    include_registered = FALSE
  )

  expect_true("default" %in% all_names)
})

test_that("list_prompt_templates respects include_builtin and
          include_registered flags", {
  # Clear registry to avoid interference from other tests
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  # Register one custom template
  custom_tmpl <- "
Trait: {TRAIT_NAME}
Definition: {TRAIT_DESCRIPTION}
A: {SAMPLE_1}
B: {SAMPLE_2}
"
  register_prompt_template("my_custom", template = custom_tmpl)

  builtin_only <- list_prompt_templates(
    include_builtin    = TRUE,
    include_registered = FALSE
  )
  registered_only <- list_prompt_templates(
    include_builtin    = FALSE,
    include_registered = TRUE
  )
  both <- list_prompt_templates(
    include_builtin    = TRUE,
    include_registered = TRUE
  )

  expect_true("default" %in% builtin_only)
  expect_false("my_custom" %in% builtin_only)

  expect_false("default" %in% registered_only)
  expect_true("my_custom" %in% registered_only)

  expect_true(all(c("default", "my_custom") %in% both))
})

test_that("register_prompt_template stores and retrieves a template by name", {
  # Clear registry
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  custom_tmpl <- "
You are assessing {TRAIT_NAME}.
S1: {SAMPLE_1}
S2: {SAMPLE_2}
Definition: {TRAIT_DESCRIPTION}
"

  invisible(register_prompt_template("short", template = custom_tmpl))

  # Should be listed as registered
  regs <- list_prompt_templates(
    include_builtin    = FALSE,
    include_registered = TRUE
  )
  expect_true("short" %in% regs)

  # And retrievable via get_prompt_template()
  tmpl <- get_prompt_template("short")
  expect_identical(tmpl, custom_tmpl)
})

test_that("register_prompt_template can read from file and validates
          placeholders", {
  # Clear registry
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  good_tmpl <- "
Compare {SAMPLE_1} vs {SAMPLE_2} on {TRAIT_NAME}.
Definition: {TRAIT_DESCRIPTION}
"

  tmp <- tempfile("pairwiseLLM-reg-template-", fileext = ".txt")
  writeLines(good_tmpl, con = tmp)

  invisible(register_prompt_template("from_file", file = tmp))

  tmpl <- get_prompt_template("from_file")
  expect_identical(tmpl, good_tmpl)

  bad_tmpl <- "
Missing placeholders except {SAMPLE_1}.
"
  tmp_bad <- tempfile("pairwiseLLM-reg-template-bad-", fileext = ".txt")
  writeLines(bad_tmpl, con = tmp_bad)

  expect_error(
    register_prompt_template("bad_from_file", file = tmp_bad),
    "missing required placeholder"
  )
})

test_that("register_prompt_template enforces overwrite = FALSE by default", {
  # Clear registry
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  tmpl1 <- "
Template one with {TRAIT_NAME}, {TRAIT_DESCRIPTION}, {SAMPLE_1}, {SAMPLE_2}.
"
  tmpl2 <- "
Template two with {TRAIT_NAME}, {TRAIT_DESCRIPTION}, {SAMPLE_1}, {SAMPLE_2}.
"

  register_prompt_template("dup_name", template = tmpl1)

  expect_error(
    register_prompt_template("dup_name", template = tmpl2),
    "already registered"
  )

  # Overwrite should succeed
  register_prompt_template("dup_name", template = tmpl2, overwrite = TRUE)
  retrieved <- get_prompt_template("dup_name")
  expect_identical(retrieved, tmpl2)
})

test_that("get_prompt_template prefers registered templates over built-ins", {
  # Clear registry
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  builtin_default <- get_prompt_template("default")

  # Register a custom template under the same name
  custom_default <- "
Custom default for {TRAIT_NAME} with {SAMPLE_1} and {SAMPLE_2}.
Definition: {TRAIT_DESCRIPTION}
"
  register_prompt_template("default", template = custom_default)

  # get_prompt_template() should now return the registered version
  tmpl <- get_prompt_template("default")
  expect_identical(tmpl, custom_default)

  # set_prompt_template() still uses the built-in default from inst/templates
  tmpl_set <- set_prompt_template()
  expect_identical(builtin_default, tmpl_set)
})

test_that("get_prompt_template errors on unknown name", {
  # Clear registry (but keep built-ins unaffected)
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  expect_error(
    get_prompt_template("definitely_not_a_template_name"),
    "No prompt template found"
  )
})

test_that("remove_prompt_template removes only registered templates,
          not built-ins", {
  # Clear registry
  rm(
    list = ls(envir = .pwllm_prompt_templates, all.names = TRUE),
    envir = .pwllm_prompt_templates
  )

  custom_tmpl <- "
Registered for removal {TRAIT_NAME}, {TRAIT_DESCRIPTION},
{SAMPLE_1}, {SAMPLE_2}.
"
  register_prompt_template("to_remove", template = custom_tmpl)

  # Ensure it exists
  expect_true("to_remove" %in% list_prompt_templates(
    include_builtin    = FALSE,
    include_registered = TRUE
  ))

  # Remove it
  res <- remove_prompt_template("to_remove")
  expect_true(res)

  expect_false("to_remove" %in% list_prompt_templates(
    include_builtin    = FALSE,
    include_registered = TRUE
  ))

  # Attempting to remove a non-registered name should error by default
  expect_error(
    remove_prompt_template("does_not_exist"),
    "No registered prompt template"
  )

  # But can be silenced with quiet = TRUE
  res2 <- remove_prompt_template("does_not_exist", quiet = TRUE)
  expect_false(res2)

  # Built-in templates such as "default" are not removable:
  expect_error(
    remove_prompt_template("default"),
    "No registered prompt template"
  )
})
