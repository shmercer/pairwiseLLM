test_that("model compatibility registry has a stable unique schema", {
  path <- system.file("extdata", "model_compatibility.csv", package = "pairwiseLLM")
  registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  expected <- c(
    "backend", "provider", "model_id", "endpoint", "live_tested",
    "batch_tested", "reasoning_mode", "package_version", "test_date",
    "status", "notes", "official_catalog_url"
  )

  expect_identical(names(registry), expected)
  expect_true(all(registry$status %in% c(
    "tested-current", "tested-legacy", "retired", "unverified"
  )))
  expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", registry$test_date)))
  expect_false(anyDuplicated(registry[c(
    "backend", "model_id", "endpoint", "reasoning_mode", "test_date"
  )]) > 0L)
  expect_true(all(registry$package_version == "1.3.1"))
  expect_true(all(grepl("^https://", registry$official_catalog_url)))
})

test_that("provider smoke matrix covers live and implemented batch surfaces", {
  path <- system.file("extdata", "model_smoke_matrix.csv", package = "pairwiseLLM")
  matrix <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  expect_identical(names(matrix), c(
    "test_id", "backend", "provider", "model_id", "mode", "endpoint",
    "reasoning_mode", "env_var"
  ))
  expect_false(anyDuplicated(matrix$test_id) > 0L)
  expect_setequal(unique(matrix$backend[matrix$mode == "live"]), c(
    "openai", "anthropic", "gemini", "vertex", "together"
  ))
  expect_setequal(unique(matrix$backend[matrix$mode == "batch"]), c(
    "openai", "anthropic", "gemini"
  ))
  expect_true(all(matrix$env_var %in% c(
    "OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GEMINI_API_KEY",
    "VERTEX_API_KEY", "TOGETHER_API_KEY"
  )))
})

test_that("dated provider smoke evidence covers the maintained matrix", {
  matrix_path <- system.file("extdata", "model_smoke_matrix.csv", package = "pairwiseLLM")
  live_path <- system.file(
    "extdata", "model_smoke_results_2026-09-03.csv", package = "pairwiseLLM"
  )
  batch_path <- system.file(
    "extdata", "model_batch_smoke_results_2026-09-03.csv", package = "pairwiseLLM"
  )
  matrix <- utils::read.csv(matrix_path, stringsAsFactors = FALSE)
  evidence <- rbind(
    utils::read.csv(live_path, stringsAsFactors = FALSE),
    utils::read.csv(batch_path, stringsAsFactors = FALSE)
  )

  expect_setequal(evidence$test_id, matrix$test_id)
  expect_true(all(evidence$status == "passed"))
  expect_true(all(evidence$status_code == 200L))
  expect_true(all(evidence$parsed_winner))
  expect_true(all(evidence$test_date == "2026-09-03"))
})

test_that("active documentation excludes retired IDs and removed controls", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "README.Rmd")),
    "Repository documentation sources are unavailable in installed-package tests."
  )
  sources <- c(
    file.path(root, "README.Rmd"),
    list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE),
    setdiff(
      list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE),
      file.path(root, "vignettes", "prompt-template-bias.Rmd")
    )
  )
  text_by_file <- lapply(sources, readLines, warn = FALSE)
  r_sources <- grepl("/R/[^/]+[.]R$", sources)
  text_by_file[r_sources] <- lapply(
    text_by_file[r_sources],
    function(x) x[grepl("^#'", x)]
  )
  text <- unlist(text_by_file, use.names = FALSE)

  retired_ids <- paste(
    "gemini-3-pro-preview|claude-4-5-sonnet|deepseek-ai/DeepSeek-V3|",
    "moonshotai/Kimi-K2-Instruct-0905|Qwen3-235B-A22B-Instruct-2507-tput",
    sep = ""
  )
  expect_false(any(grepl(retired_ids, text)))
  expect_false(any(grepl("multi_spoke_mode|hub_lock_mode", text)))
})

test_that("all R Markdown chunks parse, including unevaluated examples", {
  skip_if_not_installed("knitr")
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "README.Rmd")),
    "Repository documentation sources are unavailable in installed-package tests."
  )
  sources <- c(
    file.path(root, "README.Rmd"),
    list.files(file.path(root, "vignettes"), pattern = "[.]Rmd$", full.names = TRUE)
  )

  for (source in sources) {
    code <- tempfile(fileext = ".R")
    knitr::purl(source, output = code, documentation = 0L, quiet = TRUE)
    expect_no_error(parse(code))
  }
})

test_that("README output records the current README source hash", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "README.Rmd")),
    "Repository documentation sources are unavailable in installed-package tests."
  )
  source <- readLines(file.path(root, "README.Rmd"), warn = FALSE)
  output <- readLines(file.path(root, "README.md"), warn = FALSE)
  marker_pattern <- "<!-- README-source-md5: [0-9a-f]{32} -->"
  source_marker <- grep(marker_pattern, source, value = TRUE)
  output_marker <- grep(marker_pattern, output, value = TRUE)

  expect_length(source_marker, 1L)
  expect_identical(output_marker, source_marker)

  normalized <- sub(marker_pattern, "<!-- README-source-md5: PENDING -->", source)
  path <- tempfile()
  writeLines(normalized, path, useBytes = TRUE)
  expected <- unname(tools::md5sum(path))
  recorded <- sub("^<!-- README-source-md5: ([0-9a-f]{32}) -->$", "\\1", source_marker)
  expect_identical(recorded, expected)
})

test_that("corrected documentation examples retain their contracts", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "README.Rmd")),
    "Repository documentation sources are unavailable in installed-package tests."
  )
  adaptive <- readLines(file.path(root, "vignettes", "adaptive-pairing.Rmd"), warn = FALSE)
  advanced <- readLines(
    file.path(root, "vignettes", "advanced-batch-workflows.Rmd"),
    warn = FALSE
  )

  expect_true(any(grepl('id_col = "ID"', adaptive, fixed = TRUE)))
  expect_false(any(grepl('id_col = "sample_id"', adaptive, fixed = TRUE)))
  expect_true(any(grepl("dplyr::select(", advanced, fixed = TRUE)))
  expect_identical(nrow(pairwiseLLM::make_pairs(pairwiseLLM::example_writing_samples)), 190L)
  expect_identical(pairwiseLLM:::adaptive_defaults(20L)$refit_pairs_target, 20L)
})

test_that("Task 06 documentation distinguishes statistics and estimator inputs", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "README.Rmd")),
    "Repository documentation sources are unavailable in installed-package tests."
  )
  readme <- paste(readLines(file.path(root, "README.Rmd"), warn = FALSE), collapse = "\n")
  bias <- paste(
    readLines(file.path(root, "vignettes", "prompt-template-bias.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  getting_started <- paste(
    readLines(file.path(root, "vignettes", "getting-started.Rmd"), warn = FALSE),
    collapse = "\n"
  )

  expect_true(grepl("Uses the mean and the selected `budget_quantile`", readme, fixed = TRUE))
  expect_false(grepl("using median output tokens", getting_started, fixed = TRUE))
  expect_true(grepl("exact paired test", bias, fixed = TRUE))
  expect_true(grepl("non-significant test as evidence", bias, fixed = TRUE))
  expect_true(grepl("provider calls ran were not recorded", bias, fixed = TRUE))
  expect_true(grepl("compute_reverse_consistency()` does not group", bias, fixed = TRUE))
})

test_that("practical adaptive vignette keeps the wrapper-first within-set contract", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  skip_if(
    !file.exists(file.path(root, "vignettes", "adaptive-pairing.Rmd")),
    "Repository vignette sources are unavailable in installed-package tests."
  )
  adaptive <- readLines(file.path(root, "vignettes", "adaptive-pairing.Rmd"), warn = FALSE)
  text <- paste(adaptive, collapse = "\n")

  expect_gte(sum(grepl("adaptive_rank\\(", adaptive)), 4L)
  expect_true(grepl("n_steps = 22L", text, fixed = TRUE))
  expect_true(grepl("refit_pairs_target` is 20", text, fixed = TRUE))
  expect_true(grepl("validate_session_dir(session_dir)", text, fixed = TRUE))
  expect_true(grepl("load_adaptive_session(session_dir)", text, fixed = TRUE))
  expect_true(grepl("adaptive_results_history(out$state)", text, fixed = TRUE))
  expect_true(grepl("model = \"gpt-5.6-luna\"", text, fixed = TRUE))
  expect_true(grepl('endpoint = "responses"', text, fixed = TRUE))
  expect_true(grepl("utils::read.csv(", text, fixed = TRUE))
  expect_true(grepl('Sys.getenv("OPENAI_API_KEY")', text, fixed = TRUE))
  expect_true(grepl("live$items[, c(", text, fixed = TRUE))
  expect_true(grepl("live$refits[, c(", text, fixed = TRUE))
  expect_true(grepl("judge_invalid_reason", text, fixed = TRUE))
  expect_true(grepl("prompt_tokens", text, fixed = TRUE))
  expect_true(grepl("completion_tokens", text, fixed = TRUE))
  expect_true(grepl("include_raw = TRUE", text, fixed = TRUE))
  expect_true(grepl("resume = FALSE", text, fixed = TRUE))
  expect_true(grepl("resume = TRUE", text, fixed = TRUE))
  expect_gte(sum(grepl("session_dir = live_session", adaptive, fixed = TRUE)), 2L)

  linking_only_controls <- c(
    "run_mode =", "hub_id =", "phase_a_mode =", "phase_a_artifacts =",
    "linking_samples", "link_stage_log"
  )
  expect_false(any(vapply(
    linking_only_controls,
    function(control) grepl(control, text, fixed = TRUE),
    logical(1L)
  )))

  expect_identical(pairwiseLLM:::adaptive_defaults(2L)$refit_pairs_target, 20L)
  expect_identical(pairwiseLLM:::adaptive_defaults(20L)$refit_pairs_target, 20L)
})

test_that("practical linking vignette keeps the wrapper-first public contract", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  path <- file.path(root, "vignettes", "adaptive-linking.Rmd")
  skip_if(
    !file.exists(path),
    "Repository vignette sources are unavailable in installed-package tests."
  )

  linking <- readLines(path, warn = FALSE)
  text <- paste(linking, collapse = "\n")
  pkgdown <- paste(readLines(file.path(root, "_pkgdown.yml"), warn = FALSE), collapse = "\n")

  expect_gte(sum(grepl("adaptive_rank\\(", linking)), 8L)
  expect_true(grepl('run_mode = "link_one_spoke"', text, fixed = TRUE))
  expect_true(grepl('run_mode = "link_multi_spoke"', text, fixed = TRUE))
  expect_true(grepl('phase_a_mode = "run"', text, fixed = TRUE))
  expect_true(grepl('phase_a_mode = "import"', text, fixed = TRUE))
  expect_true(grepl('phase_a_mode = "mixed"', text, fixed = TRUE))
  expect_true(grepl("quality_gate_accepted", text, fixed = TRUE))
  expect_true(grepl("theta_link_eap", text, fixed = TRUE))
  expect_true(grepl("rank_link", text, fixed = TRUE))
  expect_true(grepl("stop_blocker_codes", text, fixed = TRUE))
  expect_true(grepl("validate_session_dir(one_spoke_session)", text, fixed = TRUE))
  expect_true(grepl("load_adaptive_session(one_spoke_session)", text, fixed = TRUE))
  expect_true(grepl('model = "gpt-5.6-luna"', text, fixed = TRUE))
  expect_true(grepl('endpoint = "responses"', text, fixed = TRUE))
  expect_true(grepl("articles/adaptive-linking.html", pkgdown, fixed = TRUE))

  removed_controls <- c("multi_spoke_mode", "hub_lock_mode =")
  expect_false(any(vapply(
    removed_controls,
    function(control) grepl(control, text, fixed = TRUE),
    logical(1L)
  )))
})

test_that("within-set design vignette tracks current adaptive contracts", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  path <- file.path(root, "vignettes", "within-set-adaptive-design.Rmd")
  skip_if(
    !file.exists(path),
    "Repository vignette sources are unavailable in installed-package tests."
  )

  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  active_sources <- c(
    file.path(root, "README.Rmd"),
    file.path(root, "vignettes", "adaptive-pairing.Rmd"),
    file.path(root, "_pkgdown.yml")
  )
  active_text <- paste(unlist(lapply(active_sources, readLines, warn = FALSE)), collapse = "\n")

  expect_false(file.exists(file.path(
    root, "vignettes", "bayesian-btl-adaptive-pairing-design.Rmd"
  )))
  expect_false(grepl("bayesian-btl-adaptive-pairing-design", active_text, fixed = TRUE))
  expect_true(grepl("within-set-adaptive-design", active_text, fixed = TRUE))
  expect_true(grepl("B_{\\mathrm{refit}}=\\operatorname{clamp}", text, fixed = TRUE))
  expect_true(grepl("not classical test-score", text, fixed = TRUE))
  expect_true(grepl("## Foundational concepts", text, fixed = TRUE))
  expect_false(grepl("## How to read this document", text, fixed = TRUE))
  acronym_definitions <- c(
    "comparative judgment (CJ)",
    "Bradley--Terry--Luce (BTL)",
    "Markov chain Monte Carlo (MCMC)",
    "expected a posteriori (EAP)",
    "effective sample size (ESS)"
  )
  expect_true(all(vapply(
    acronym_definitions,
    function(definition) grepl(definition, text, fixed = TRUE),
    logical(1L)
  )))
  stage_explanations <- c(
    "`anchor_link` (anchor link)",
    "`long_link` (long link)",
    "`mid_link` (mid link)",
    "`local_link` (local link)",
    "Conceptual example of one adaptive step"
  )
  expect_true(all(vapply(
    stage_explanations,
    function(explanation) grepl(explanation, text, fixed = TRUE),
    logical(1L)
  )))
  phase_b_mentions <- gregexpr("linking Phase B", text, fixed = TRUE)[[1L]]
  expect_lte(sum(phase_b_mentions > 0L), 1L)

  selector <- pairwiseLLM:::adaptive_defaults(100L)
  expect_identical(selector$refit_pairs_target, 50L)
  expect_identical(selector$round_pairs_target, 25L)
  expect_identical(selector$anchor_frac_total, 0.10)
  expect_identical(selector$anchor_count_min, 10L)
  expect_identical(
    c(selector$anchor_top_weight, selector$anchor_mid_weight, selector$anchor_bottom_weight),
    c(0.30, 0.40, 0.30)
  )
  expect_identical(selector$dup_max_obs, 2L)
  expect_identical(selector$dup_max_obs_relaxed, 3L)
  expect_identical(c(selector$p_long_low, selector$p_long_high), c(0.10, 0.90))

  btl <- pairwiseLLM:::.adaptive_btl_defaults(100L)
  expect_identical(btl$model_variant, "btl_e_b")
  expect_identical(btl$ess_bulk_min, 400)
  expect_identical(btl$ess_bulk_min_near_stop, 1000)
  expect_identical(btl$eap_reliability_min, 0.90)
  expect_identical(btl$stability_lag, 2L)
  expect_identical(btl$theta_corr_min, 0.95)
  expect_identical(btl$theta_sd_rel_change_max, 0.10)
  expect_identical(btl$rank_spearman_min, 0.95)

  scores <- stats::setNames(seq(100, 1), as.character(seq_len(100L)))
  anchors <- pairwiseLLM:::.adaptive_select_rolling_anchors(scores, selector)
  expect_length(anchors, 10L)

  stan_dir <- system.file("stan", package = "pairwiseLLM")
  for (model in c("btl.stan", "btl_e.stan", "btl_b.stan", "btl_e_b.stan")) {
    stan <- paste(readLines(file.path(stan_dir, model), warn = FALSE), collapse = "\n")
    expect_true(grepl("theta_raw - mean(theta_raw)", stan, fixed = TRUE))
    expect_true(grepl("theta_raw ~ normal(0, 1)", stan, fixed = TRUE))
  }
})

test_that("adaptive-linking design vignette tracks the normative Phase A/B contract", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  path <- file.path(root, "vignettes", "adaptive-linking-design.Rmd")
  skip_if(
    !file.exists(path),
    "Repository vignette sources are unavailable in installed-package tests."
  )

  design <- readLines(path, warn = FALSE)
  text <- paste(design, collapse = "\n")
  practical <- paste(
    readLines(file.path(root, "vignettes", "adaptive-linking.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  pairing_guide <- paste(
    readLines(file.path(root, "vignettes", "adaptive-pairing.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  pairing_design <- paste(
    readLines(file.path(root, "vignettes", "within-set-adaptive-design.Rmd"), warn = FALSE),
    collapse = "\n"
  )
  pkgdown <- paste(readLines(file.path(root, "_pkgdown.yml"), warn = FALSE), collapse = "\n")
  readme <- paste(readLines(file.path(root, "README.Rmd"), warn = FALSE), collapse = "\n")

  vignette_titles <- c(
    "Guide: Adaptive Pairing" = pairing_guide,
    "Design: Adaptive Pairing" = pairing_design,
    "Guide: Adaptive Linking" = practical,
    "Design: Adaptive Linking" = text
  )
  for (title in names(vignette_titles)) {
    source <- vignette_titles[[title]]
    expect_true(grepl(paste0('title: "', title, '"'), source, fixed = TRUE))
    expect_true(grepl(paste0("VignetteIndexEntry{", title, "}"), source, fixed = TRUE))
  }

  pkgdown_labels <- c(
    'text: "Guide: Adaptive Pairing"',
    'text: "Design: Adaptive Pairing"',
    'text: "Guide: Adaptive Linking"',
    'text: "Design: Adaptive Linking"'
  )
  pkgdown_positions <- vapply(
    pkgdown_labels,
    function(label) regexpr(label, pkgdown, fixed = TRUE)[[1L]],
    integer(1L)
  )
  expect_true(all(pkgdown_positions > 0L))
  expect_true(all(diff(pkgdown_positions) > 0L))

  expect_true(grepl("adaptive-linking-design.html", practical, fixed = TRUE))
  expect_true(grepl("adaptive-linking-design", pkgdown, fixed = TRUE))
  expect_true(grepl("adaptive-linking-design.html", readme, fixed = TRUE))
  expect_true(grepl("adaptive-linking.html", text, fixed = TRUE))
  expect_true(grepl("within-set-adaptive-design.html", text, fixed = TRUE))

  required_sections <- c(
    "## Foundational concepts",
    "## Phase A artifacts and validation",
    "## Common-scale identification",
    "## D-optimal active selection",
    "## Probes and calibration",
    "## Stopping, blockers, and freezing",
    "## Persistence and resume"
  )
  expect_true(all(vapply(
    required_sections,
    function(section) grepl(section, text, fixed = TRUE),
    logical(1L)
  )))

  acronym_definitions <- c(
    "comparative judgment (CJ)",
    "Bradley--Terry--Luce (BTL)",
    "Markov chain Monte Carlo (MCMC)",
    "expected a posteriori (EAP)",
    "standard deviation (SD)",
    "Effective sample size (ESS)",
    "maximum a posteriori (MAP)",
    "Broyden--Fletcher--Goldfarb--Shanno\n(BFGS)",
    "Expected calibration error (ECE)",
    "root mean squared error (RMSE)"
  )
  expect_true(all(vapply(
    acronym_definitions,
    function(definition) grepl(definition, text, fixed = TRUE),
    logical(1L)
  )))

  normative_terms <- c(
    "anchored-joint estimation",
    "hard-locked hub",
    "globally shared judge parameters",
    'probe_acceleration_mode = "fixed_per_refit"',
    "quality_gate_accepted = TRUE",
    "link_d_opt_gain",
    "coverage_priority = 1",
    "stop_blocker_codes",
    "link_state_frozen = TRUE",
    "`probe_quality_pass` itself is\nnot included in `link_stop_pass`"
  )
  expect_true(all(vapply(
    normative_terms,
    function(term) grepl(term, text, fixed = TRUE),
    logical(1L)
  )))
  expect_false(grepl("multi_spoke_mode|hub_lock_mode", text))

  defaults <- pairwiseLLM:::.adaptive_controller_defaults(300L)
  defaults$run_mode <- "link_multi_spoke"
  defaults <- pairwiseLLM:::.adaptive_controller_resolve_scaled_linking_defaults(
    controller = defaults,
    cfg_names = character(),
    set_ids = c(rep(1L, 100L), rep(2L, 100L), rep(3L, 100L))
  )
  expect_identical(defaults$link_estimation_mode, "anchored_joint")
  expect_identical(defaults$hub_lock_mode, "hard_lock")
  expect_identical(defaults$judge_param_mode, "global_shared")
  expect_identical(defaults$probe_acceleration_mode, "fixed_per_refit")
  expect_identical(defaults$probe_pairs_per_refit_per_spoke, 4L)
  expect_identical(defaults$probe_panel_edges, 160L)
  expect_identical(defaults$probe_edges_min_for_stop, 120L)
  expect_identical(defaults$link_stop_reliability_min, 0.90)
  expect_identical(defaults$probe_brier_max, 0.19)
  expect_identical(defaults$probe_pred_rmse_max, 0.015)
  expect_identical(defaults$theta_global_rmse_max, 0.05)
  expect_identical(defaults$stability_window_refits, 3L)
  expect_identical(defaults$stability_passes_required, 2L)
  expect_identical(defaults$max_pairs_after_stop, 0L)
})
