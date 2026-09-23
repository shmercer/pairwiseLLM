# Execute the actual public documentation snippets with offline completed CJ.
rubric_documentation_chunks <- function() {
  path <- testthat::test_path("..", "..", "vignettes", "rubric-calibration.Rmd")
  testthat::skip_if_not(file.exists(path), "Repository vignette source is unavailable.")
  lines <- readLines(path, warn = FALSE)
  starts <- grep("^```\\{r ", lines)
  chunks <- lapply(starts, function(i) {
    end <- which(seq_along(lines) > i & lines == "```")[[1L]]
    lines[seq.int(i + 1L, end - 1L)]
  })
  names(chunks) <- sub("^```\\{r ([^,}]+).*$", "\\1", lines[starts])
  chunks
}

rubric_run_documentation <- function(chunks, names, env) {
  for (name in names) {
    testthat::expect_true(name %in% names(chunks))
    invisible(capture.output(eval(parse(text = chunks[[name]]), envir = env)))
  }
}

rubric_documentation_environment <- function(data) {
  env <- new.env(parent = asNamespace("pairwiseLLM"))
  env$completed_cj <- data$cj
  env$rubric_levels <- c("developing", "proficient", "advanced")
  labels <- data$rubric
  labels$rubric_score <- env$rubric_levels[labels$rubric_score]
  # Deterministic interleaving retains each category and the CJ range.
  held_out <- seq_len(nrow(labels)) %% 5L == 0L
  env$training_labels <- labels[!held_out, ]
  env$evaluation_labels <- labels[held_out, ]
  env
}

test_that("rubric navigation and release metadata are independently synchronized", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Repository docs unavailable.")
  config <- readLines(file.path(root, "_pkgdown.yml"), warn = FALSE)
  topics <- c("fit_rubric_calibration", "predict.pairwiseLLM_rubric_calibration",
    "evaluate_rubric_predictions")
  reference <- config[seq.int(match("reference:", config) + 1L, length(config))]
  for (topic in topics) {
    expect_equal(sum(trimws(reference) == paste("-", topic)), 1L)
    rd <- readLines(file.path(root, "man", paste0(topic, ".Rd")), warn = FALSE)
    expect_true(any(grepl("\\concept{rubric calibration}", rd, fixed = TRUE)))
  }
  expect_true(any(grepl("articles/rubric-calibration.html", config, fixed = TRUE)))
  expect_true(any(trimws(config) == "- rubric-calibration"))
  description <- read.dcf(file.path(root, "DESCRIPTION"))[1L, ]
  expect_match(description[["Depends"]], "R (>= 4.4)", fixed = TRUE)
  expect_match(description[["Suggests"]], "mgcv (>= 1.9-4)", fixed = TRUE)
  expect_match(description[["Suggests"]], "ordinal", fixed = TRUE)
  expect_false(grepl("ordinal|mgcv", description[["Imports"]]))
  codemeta <- jsonlite::fromJSON(file.path(root, "codemeta.json"), simplifyVector = FALSE)
  expect_identical(codemeta$description, gsub("\\s+", " ", description[["Description"]]))
  requirements <- Filter(is.list, codemeta$softwareRequirements)
  r_requirement <- Filter(function(x) identical(x$identifier, "R"), requirements)
  expect_identical(r_requirement[[1L]]$version, ">= 4.4")
  suggestions <- codemeta$softwareSuggestions
  expect_true(all(c("ordinal", "mgcv") %in% vapply(suggestions, `[[`, "", "identifier")))
  mgcv <- Filter(function(x) identical(x$identifier, "mgcv"), suggestions)
  expect_identical(mgcv[[1L]]$version, ">= 1.9-4")
})

test_that("documented percentile workflow executes without modeling backends", {
  chunks <- rubric_documentation_chunks()
  env <- rubric_documentation_environment(rubric_linear_data())
  rubric_run_documentation(chunks, c("rubric-levels", "percentile"), env)
  expect_identical(env$percentile_calibration$method, "percentile")
  expect_equal(nrow(env$percentile_scores), nrow(env$completed_cj$item_log_list[[1L]]))
  expect_false("probabilities" %in% names(env$percentile_scores))
})

test_that("documented same-set probabilities holdout and traits execute", {
  skip_if_not_installed("ordinal")
  chunks <- rubric_documentation_chunks()
  env <- rubric_documentation_environment(rubric_linear_data())
  evidence <- rubric_linear_data(slope = 0.8, variant = "btl_b")
  env$evidence_cj <- evidence$cj
  env$evidence_training_labels <- evidence$rubric
  env$evidence_training_labels$rubric_score <- c("limited", "adequate", "strong")[
    evidence$rubric$rubric_score]
  rubric_run_documentation(chunks,
    c("same-set", "probabilities", "evaluation", "analytic-traits"), env)
  expect_equal(rowSums(env$probability_matrix), rep(1, nrow(env$scores)))
  expect_identical(env$scores$category, env$scores$median_category)
  expect_identical(env$modal_scores$probabilities, env$scores$probabilities)
  expect_equal(env$assessment$metadata$n_training_label_overlap, 0)
  expect_equal(env$assessment$metrics$n, nrow(env$evaluation_labels))
  expect_true(is.finite(env$assessment$metrics$rps))
  expect_identical(env$evidence_calibration$trait, "evidence")
  expect_identical(env$calibration$trait, "organization")
})

test_that("documented monotone workflow executes with its optional backend", {
  skip_if_not_installed("mgcv", "1.9.4")
  skip_if_not_installed("withr")
  chunks <- rubric_documentation_chunks()
  env <- rubric_documentation_environment(rubric_monotone_data())
  rubric_run_documentation(chunks, "monotone", env)
  expect_identical(env$monotone_calibration$method, "ordinal_monotone")
  expect_true(is.finite(env$monotone_assessment$metrics$rps))
  expect_equal(env$monotone_assessment$metadata$n_training_label_overlap, 0)
})

test_that("documented historical calibration consumes actual accepted Phase B", {
  skip_if_not_installed("ordinal")
  chunks <- rubric_documentation_chunks()
  data <- rubric_linked_fixture()
  env <- new.env(parent = asNamespace("pairwiseLLM"))
  env$reference_phase_a <- data$reference
  env$reference_labels <- data$rubric
  env$rubric_levels <- data$levels
  env$completed_phase_b <- data$state
  target_items <- summarize_items(data$state)
  targets <- target_items$global_item_id[target_items$set_id != "1"]
  env$target_labels <- data.frame(item_id = targets, rubric_score = data$levels)
  rubric_run_documentation(chunks,
    c("linked-calibration", "linked-prediction", "linked-evaluation"), env)
  expect_setequal(env$target_scores$global_item_id, targets)
  expect_equal(env$target_assessment$metadata$n_training_label_overlap, 0)
  expect_equal(nrow(env$reference_scores), nrow(data$rubric))
  expect_identical(env$target_assessment$metadata$linking,
    attr(env$target_scores, "linking"))
})
