test_that("test filenames have unique documented numeric prefixes", {
  files <- list.files(testthat::test_path(), pattern = "^test-.*[.]R$")
  expect_true(all(grepl("^test-[0-9]{4}-.+[.]R$", files)))
  prefixes <- as.integer(sub("^test-([0-9]{4})-.*", "\\1", files))
  expect_identical(anyDuplicated(prefixes), 0L)
  expect_true(all(prefixes %/% 1000L %in% c(0L, 2L, 3L, 4L, 5L, 6L, 9L)))
})

test_that("public warm-start help topics have examples, family and cross references", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  paths <- list.files(file.path(root, "man"), pattern = "warm.*[.]Rd$", full.names = TRUE)
  expect_gte(length(paths), 14L)
  topics <- lapply(paths, tools::parse_Rd)
  tags <- function(rd, tag) Filter(function(x) identical(attr(x, "Rd_tag"), tag), rd)
  aliases <- unlist(lapply(topics, function(rd) unlist(tags(rd, "\\alias"))))
  public <- getNamespaceExports("pairwiseLLM")
  public <- public[grepl("warm_start|pairwiseLLM_warm", public)]
  expect_true(all(public %in% aliases))
  for (i in seq_along(topics)) {
    rd <- topics[[i]]
    expect_true("adaptive warm start" %in% unlist(tags(rd, "\\concept")), info = paths[i])
    expect_length(tags(rd, "\\examples"), 1L)
    expect_length(tags(rd, "\\seealso"), 1L)
  }
  config <- paste(readLines(file.path(root, "_pkgdown.yml")), collapse = "\n")
  expect_match(config, 'has_concept("adaptive warm start")', fixed = TRUE)
})

test_that("coefficient documentation states its interpretation and limitations", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  rd_path <- file.path(root, "man", "warm_start_coefficients.Rd")
  vignette_path <- file.path(root, "vignettes", "adaptive-warm-start.Rmd")
  expect_true(file.exists(rd_path))
  expect_true("warm_start_coefficients" %in% getNamespaceExports("pairwiseLLM"))

  rd <- paste(readLines(rd_path, warn = FALSE), collapse = "\n")
  vignette <- paste(readLines(vignette_path, warn = FALSE), collapse = "\n")
  expect_match(rd, "stored elastic-net", fixed = TRUE)
  expect_match(rd, "learned OOF calibration slope", fixed = TRUE)
  expect_match(rd, "one-training-SD increase", fixed = TRUE)
  expect_match(rd, "retained = FALSE", fixed = TRUE)
  expect_match(rd, "retained = TRUE", fixed = TRUE)
  expect_match(rd, "not a unique measure of predictive", fixed = TRUE)
  expect_match(rd, "its own training distribution", fixed = TRUE)
  expect_match(rd, "neither defines an aggregate", fixed = TRUE)
  expect_false(grepl("permutation importance", rd, fixed = TRUE))

  expect_match(vignette, "coef_tbl <- warm_start_coefficients(model)", fixed = TRUE)
  expect_match(vignette, "coef_ensemble <- warm_start_coefficients(ensemble)", fixed = TRUE)
  expect_match(vignette, "assessment_a_std_coefficient", fixed = TRUE)
  expect_match(vignette, "neither estimates an aggregate", fixed = TRUE)
})
