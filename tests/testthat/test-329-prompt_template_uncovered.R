# Tests for uncovered branches in R/prompt_template.R

.restore_prompt_template_registry <- function() {
  old_names <- ls(envir = pairwiseLLM:::.pwllm_prompt_templates, all.names = TRUE)
  old_values <- if (length(old_names) == 0L) {
    list()
  } else {
    mget(old_names, envir = pairwiseLLM:::.pwllm_prompt_templates, inherits = FALSE)
  }

  withr::defer({
    rm(list = ls(envir = pairwiseLLM:::.pwllm_prompt_templates, all.names = TRUE),
       envir = pairwiseLLM:::.pwllm_prompt_templates)
    if (length(old_values) > 0L) {
      list2env(old_values, envir = pairwiseLLM:::.pwllm_prompt_templates)
    }
  })

  invisible(NULL)
}

test_that("329-01 set_prompt_template reads templates from file", {
  tmpl <- paste(
    "Trait: {TRAIT_NAME}",
    "Desc: {TRAIT_DESCRIPTION}",
    "S1: {SAMPLE_1}",
    "S2: {SAMPLE_2}",
    sep = "\n"
  )

  path <- tempfile(fileext = ".txt")
  writeLines(tmpl, path)

  out <- pairwiseLLM::set_prompt_template(file = path)
  expect_identical(out, tmpl)
})

test_that("329-02 set_prompt_template errors when built-in default is missing", {
  # Cover stop() path at prompt_template.R:98-102 by forcing the builtin lookup to fail.
  testthat::local_mocked_bindings(
    .pwllm_get_builtin_template = function(name) NULL,
    .package = "pairwiseLLM"
  )

  expect_error(
    pairwiseLLM::set_prompt_template(),
    "Built-in default template not found",
    fixed = TRUE
  )
})

test_that("329-03 get_prompt_template falls back to set_prompt_template() for default", {
  .restore_prompt_template_registry()

  # Pre-compute the real default template before mocking.
  default_real <- pairwiseLLM::set_prompt_template()

  called <- FALSE
  testthat::local_mocked_bindings(
    .pwllm_get_builtin_template = function(name) {
      # First call (from get_prompt_template) returns NULL to hit the fallback branch.
      # Subsequent calls (from set_prompt_template) return the real default.
      if (!isTRUE(called)) {
        called <<- TRUE
        return(NULL)
      }
      default_real
    },
    .package = "pairwiseLLM"
  )

  out <- pairwiseLLM::get_prompt_template("default")
  expect_identical(out, default_real)
})
