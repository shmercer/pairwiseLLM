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

test_that("warm-mode tables distinguish destinations and the common bootstrap", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  expected <- c(
    "cold|no|no|seeded shuffled chain",
    "btl_only|yes|no|same seeded shuffled chain",
    "trueskill_only|no|yes|same seeded shuffled chain",
    "both|yes|yes|same seeded shuffled chain"
  )
  for (article in c("adaptive-warm-start", "within-set-adaptive-design")) {
    lines <- readLines(file.path(root, "vignettes", paste0(article, ".Rmd")))
    rows <- grep("^\\| `(cold|btl_only|trueskill_only|both)` \\|", lines, value = TRUE)
    cells <- strsplit(gsub("`", "", rows, fixed = TRUE), "|", fixed = TRUE)
    actual <- vapply(cells, function(x) paste(trimws(x[-1L]), collapse = "|"), character(1))
    expect_identical(actual, expected, info = article)
  }
  for (topic in c("adaptive_rank", "adaptive_rank_start")) {
    rd <- paste(readLines(file.path(root, "man", paste0(topic, ".Rd"))), collapse = " ")
    rd <- gsub("[[:space:]]+", " ", rd)
    rd <- gsub("\\\\code\\{([^}]*)\\}", "\\1", rd)
    for (contract in c(
      "Omitted/NULL mode defaults to btl_only with predictive input, otherwise cold",
      "mu = mu0 + sigma0 * prior_mean", "fixed multiplier 1", "unchanged sigma",
      "same seeded connected shuffled bootstrap", "presence alone does not imply BTL warming"
    )) {
      expect_match(rd, contract, fixed = TRUE, info = topic)
    }
  }
})

test_that("adaptive documentation assigns model roles and scopes strategies", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  articles <- c("adaptive-pairing", "within-set-adaptive-design",
    "adaptive-linking", "adaptive-linking-design")
  for (article in articles) {
    text <- paste(readLines(file.path(root, "vignettes", paste0(article, ".Rmd"))), collapse = " ")
    text <- gsub("[[:space:]]+", " ", text)
    for (contract in c("TrueSkill", "rolling anchors", "long-link gate", "reliability",
      "diagnostics", "stopping", "global_identified", "Phase B", "unchanged")) {
      expect_match(text, contract, fixed = TRUE, info = article)
    }
    expect_match(text, "[Dd]irect strategies.*within-set", info = article)
    expect_false(grepl("afterward they use BTL EAP|posterior exists, its win probabilities", text))
  }
  guide <- paste(readLines(file.path(root, "vignettes", "adaptive-pairing.Rmd")), collapse = " ")
  for (contract in c(
    "pairing_strategy", "hybrid", "random", "trueskill_p50", "trueskill_pollitt",
    "abs(p_ts(i > j) - 0.50)",
    "min(abs(p_ts(i > j) - 1/3), abs(p_ts(i > j) - 2/3))",
    "minimum current committed degree", "Pollitt-inspired", "direct_pairing", "target_distance"
  )) {
    expect_match(guide, contract, fixed = TRUE)
  }
})

test_that("public replay help has navigation and strict evidence contracts", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  config <- readLines(file.path(root, "_pkgdown.yml"))
  for (topic in c("validate_adaptive_replay", "make_adaptive_judge_replay")) {
    expect_identical(sum(trimws(config) == paste("-", topic)), 1L)
    rd <- tools::parse_Rd(file.path(root, "man", paste0(topic, ".Rd")))
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    expect_true("adaptive ranking" %in% unlist(rd[tags == "\\concept"]))
    expect_true(any(tags == "\\examples"))
    expect_true(any(tags == "\\seealso"))
  }
  replay <- paste(readLines(file.path(root, "man", "make_adaptive_judge_replay.Rd")), collapse = " ")
  replay <- gsub("[[:space:]]+", " ", replay)
  for (contract in c("exact presented", "never inferred", "No provider calls",
    "dup_max_obs_relaxed = 2L", "fresh judge", "committed history", "retained separately")) {
    expect_match(replay, contract, fixed = TRUE)
  }
})

test_that("numeric warm-start and directed replay vignette examples execute offline", {
  skip_if_not_installed("withr")
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if_not(file.exists(file.path(root, "_pkgdown.yml")), "Source documentation unavailable")
  withr::local_seed(808L)
  rng <- .Random.seed
  run_chunk <- function(article, label) {
    lines <- readLines(file.path(root, "vignettes", paste0(article, ".Rmd")))
    start <- grep(paste0("^```\\{r ", label, "[,}]"), lines)
    stopifnot(length(start) == 1L)
    end <- which(seq_along(lines) > start & lines == "```")[[1L]]
    env <- new.env(parent = asNamespace("pairwiseLLM"))
    invisible(capture.output(eval(parse(text = lines[seq.int(start + 1L, end - 1L)]), env)))
    env
  }
  warm <- run_chunk("adaptive-warm-start", "destination-modes")
  expect_identical(warm$historical$meta$warm_start_mode, "btl_only")
  expect_identical(warm$warmed$meta$warm_start_mode, "both")
  expect_equal(warm$warmed$trueskill_state$items$mu,
    25 + (25 / 3) * warm$numeric_prior$prior_mean)
  expect_identical(warm$warmed$trueskill_state$items$sigma,
    warm$historical$trueskill_state$items$sigma)
  expect_identical(warm$warmed$warm_start_pairs, warm$historical$warm_start_pairs)

  replay <- run_chunk("adaptive-pairing", "directed-replay")
  log <- pairwiseLLM::adaptive_step_log(replay$replay_state)
  expect_identical(nrow(log), 5L)
  expect_identical(sum(log$round_stage == "direct_pairing"), 2L)
  expect_identical(anyDuplicated(log[c("A_id", "B_id")]), 0L)
  expect_identical(log$Y, as.integer(log$A_id < log$B_id))
  expect_identical(nrow(replay$replay_state$round_log), 0L)
  expect_identical(.Random.seed, rng)
})
