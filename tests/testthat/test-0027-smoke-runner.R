testthat::test_that("provider smoke runner fails missing keys unless explicitly allowed", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  script <- file.path(root, "inst", "scripts", "smoke_model_compatibility.R")
  testthat::skip_if(
    !file.exists(script),
    "Repository smoke runner is unavailable in installed-package tests."
  )

  run_without_openai_key <- function(allow_missing_keys) {
    output_path <- tempfile(fileext = ".csv")
    args <- c(
      script,
      "--mode=live",
      "--providers=openai",
      paste0("--output=", output_path),
      paste0("--allow-missing-keys=", tolower(as.character(allow_missing_keys)))
    )
    output <- withr::with_dir(
      root,
      suppressWarnings(system2(
        file.path(R.home("bin"), "Rscript"),
        args = shQuote(args),
        stdout = TRUE,
        stderr = TRUE,
        env = c(
          "PAIRWISELLM_RUN_PROVIDER_SMOKE=true",
          "OPENAI_API_KEY=",
          "R_ENVIRON_USER=/dev/null"
        )
      ))
    )
    status <- attr(output, "status")
    if (is.null(status)) status <- 0L
    testthat::expect_true(
      file.exists(output_path),
      info = paste(output, collapse = "\n")
    )

    list(
      status = as.integer(status),
      results = utils::read.csv(output_path, stringsAsFactors = FALSE)
    )
  }

  strict <- run_without_openai_key(FALSE)
  testthat::expect_identical(strict$status, 1L)
  testthat::expect_true(all(strict$results$status == "skipped-no-key"))

  permissive <- run_without_openai_key(TRUE)
  testthat::expect_identical(permissive$status, 0L)
  testthat::expect_true(all(permissive$results$status == "skipped-no-key"))
})

testthat::test_that("smoke matrix covers the selected current model catalog", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  matrix_path <- file.path(root, "inst", "extdata", "model_smoke_matrix.csv")
  testthat::skip_if(
    !file.exists(matrix_path),
    "Repository smoke matrix is unavailable in installed-package tests."
  )
  matrix <- utils::read.csv(
    matrix_path,
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(nrow(matrix), 97L)
  testthat::expect_equal(sum(matrix$mode == "live"), 64L)
  testthat::expect_equal(sum(matrix$mode == "batch"), 33L)

  openai_models <- unique(matrix$model_id[
    matrix$backend == "openai" & matrix$catalog_status == "current"
  ])
  testthat::expect_length(openai_models, 18L)
  testthat::expect_false("gpt-6-astra" %in% openai_models)
  testthat::expect_false(any(grepl("-pro$", openai_models)))

  baseline <- matrix$catalog_status %in% c("current", "preview")
  live_models <- split(matrix$model_id[baseline & matrix$mode == "live"],
    matrix$backend[baseline & matrix$mode == "live"])
  batch_models <- split(matrix$model_id[baseline & matrix$mode == "batch"],
    matrix$backend[baseline & matrix$mode == "batch"])
  testthat::expect_setequal(batch_models$openai, live_models$openai)
  testthat::expect_setequal(batch_models$anthropic, live_models$anthropic)
  testthat::expect_setequal(batch_models$gemini, live_models$gemini)
  testthat::expect_false(any(matrix$backend %in% c("vertex", "together") &
    matrix$mode == "batch"))
})

testthat::test_that("batch orchestration submits all jobs before grouped polling", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  helper <- file.path(root, "inst", "scripts", "smoke_model_compatibility_helpers.R")
  testthat::skip_if(
    !file.exists(helper),
    "Repository smoke helpers are unavailable in installed-package tests."
  )
  source(helper)

  rows <- data.frame(test_id = c("a", "b"), stringsAsFactors = FALSE)
  states <- setNames(lapply(rows$test_id, function(x) new_batch_state()), rows$test_id)
  events <- character()
  polls <- c(a = 0L, b = 0L)
  paces <- 0L

  result <- orchestrate_smoke_batches(
    rows = rows,
    states = states,
    submit_one = function(row) {
      events <<- c(events, paste0("submit-", row$test_id))
      list(remote_id = paste0("remote-", row$test_id), remote_status = "queued")
    },
    poll_one = function(row, state) {
      id <- row$test_id
      polls[[id]] <<- polls[[id]] + 1L
      events <<- c(events, paste0("poll-", id))
      done <- identical(id, "b") || polls[[id]] > 1L
      list(terminal = done, successful = done, remote_status = if (done) "done" else "running")
    },
    collect_one = function(row, state) {
      events <<- c(events, paste0("collect-", row$test_id))
      list(status = "passed", status_code = 200L, parsed_winner = TRUE, error = NA_character_)
    },
    pace = function() {
      paces <<- paces + 1L
      invisible(NULL)
    },
    sleep = function(seconds) invisible(NULL),
    timeout_seconds = Inf
  )

  testthat::expect_equal(events[1:2], c("submit-a", "submit-b"))
  testthat::expect_true(max(which(startsWith(events, "submit"))) <
    min(which(startsWith(events, "poll"))))
  testthat::expect_true(max(which(startsWith(events, "poll"))) <
    min(which(startsWith(events, "collect"))))
  testthat::expect_true(all(vapply(result, function(x) x$status == "passed", logical(1))))
  testthat::expect_equal(paces, 7L)
})

testthat::test_that("list mode is offline and reports filtered configurations", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  script <- file.path(root, "inst", "scripts", "smoke_model_compatibility.R")
  testthat::skip_if(
    !file.exists(script),
    "Repository smoke runner is unavailable in installed-package tests."
  )
  output <- withr::with_dir(root, system2(
    file.path(R.home("bin"), "Rscript"),
    args = shQuote(c(script, "--list=true", "--mode=batch", "--providers=anthropic")),
    stdout = TRUE, stderr = TRUE,
    env = c("PAIRWISELLM_RUN_PROVIDER_SMOKE=false", "R_ENVIRON_USER=/dev/null")
  ))

  testthat::expect_null(attr(output, "status"))
  testthat::expect_true(any(grepl("claude-fable-5-1", output, fixed = TRUE)))
  testthat::expect_false(any(grepl("claude-haiku-4-5-20251001.*thinking", output)))
})

testthat::test_that("promotion blocks incomplete runs and builds tested flags", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  helper <- file.path(
    root, "inst", "scripts", "promote_model_smoke_results_helpers.R"
  )
  testthat::skip_if(
    !file.exists(helper),
    "Repository promotion helpers are unavailable in installed-package tests."
  )
  source(helper)

  results <- data.frame(
    test_id = c("openai_live", "openai_batch", "together_live"),
    backend = c("openai", "openai", "together"),
    provider = c("OpenAI", "OpenAI", "Together AI"),
    model_id = c("model-a", "model-a", "model-b"),
    mode = c("live", "batch", "live"),
    endpoint = c("responses", "responses", "chat.completions"),
    request_profile = c("openai", "openai", "together"),
    reasoning_mode = c("none", "none", "none"),
    env_var = c("OPENAI_API_KEY", "OPENAI_API_KEY", "TOGETHER_API_KEY"),
    package_version = "1.3.1",
    test_date = "2026-09-05",
    status = c("passed", "timed-out", "failed-error-row"),
    catalog_status = "current",
    catalog_checked_on = "2026-09-05",
    catalog_url = c("https://example.com/a", "https://example.com/a", "https://example.com/b"),
    stringsAsFactors = FALSE
  )
  smoke_matrix <- results[c(
    "test_id", "backend", "provider", "model_id", "mode", "endpoint",
    "request_profile", "reasoning_mode", "env_var", "catalog_status",
    "catalog_checked_on", "catalog_url"
  )]

  testthat::expect_error(
    validate_promotion_inputs(results, smoke_matrix),
    "timed-out=1", fixed = TRUE
  )
  testthat::expect_no_error(
    validate_promotion_inputs(results, smoke_matrix, allow_incomplete = TRUE)
  )

  results$status[[2L]] <- "passed"
  existing <- data.frame(
    backend = "gemini", provider = "Google", model_id = "retired-model",
    endpoint = "generateContent", live_tested = FALSE, batch_tested = FALSE,
    reasoning_mode = "none", package_version = "1.3.1", test_date = "2026-01-01",
    status = "retired", notes = "Historical.",
    official_catalog_url = "https://example.com/retired",
    stringsAsFactors = FALSE
  )
  registry <- build_compatibility_registry(results, existing)
  openai <- registry[registry$model_id == "model-a", , drop = FALSE]
  together <- registry[registry$model_id == "model-b", , drop = FALSE]

  testthat::expect_true(openai$live_tested)
  testthat::expect_true(openai$batch_tested)
  testthat::expect_identical(openai$status, "tested-current")
  testthat::expect_false(together$live_tested)
  testthat::expect_identical(together$status, "unverified")
  testthat::expect_true("retired-model" %in% registry$model_id)
})

testthat::test_that("promotion updates dated evidence references", {
  root <- tempfile("smoke-reference-")
  dir.create(root)
  path <- file.path(root, "reference.Rmd")
  writeLines(c(
    "model_smoke_results_2026-09-03.csv",
    "model_batch_smoke_results_2026-09-03.csv"
  ), path)
  helper <- testthat::test_path(
    "..", "..", "inst", "scripts", "promote_model_smoke_results_helpers.R"
  )
  testthat::skip_if(
    !file.exists(helper),
    "Repository promotion helpers are unavailable in installed-package tests."
  )
  source(helper)

  update_dated_evidence_references(path, "2026-09-05")
  contents <- readLines(path)
  testthat::expect_true(all(grepl("2026-09-05", contents, fixed = TRUE)))
})

testthat::test_that("smoke resume restores authoritative static metadata", {
  root <- normalizePath(testthat::test_path("..", ".."), winslash = "/")
  script <- file.path(root, "inst", "scripts", "smoke_model_compatibility.R")
  testthat::skip_if(
    !file.exists(script),
    "Repository smoke runner is unavailable in installed-package tests."
  )
  output_path <- tempfile(fileext = ".csv")
  arguments <- shQuote(c(
    script, "--mode=live", "--providers=openai", "--allow-missing-keys=true",
    paste0("--output=", output_path)
  ))
  run_smoke <- function() {
    withr::with_dir(root, suppressWarnings(system2(
      file.path(R.home("bin"), "Rscript"), args = arguments,
      stdout = TRUE, stderr = TRUE,
      env = c(
        "PAIRWISELLM_RUN_PROVIDER_SMOKE=true", "OPENAI_API_KEY=",
        "R_ENVIRON_USER=/dev/null"
      )
    )))
  }

  run_smoke()
  corrupted <- utils::read.csv(output_path, stringsAsFactors = FALSE)
  corrupted$package_version <- "03-01-01"
  corrupted$test_date <- "09-05-26"
  corrupted$catalog_checked_on <- "09-05-26"
  utils::write.csv(corrupted, output_path, row.names = FALSE, na = "")

  run_smoke()
  resumed <- utils::read.csv(output_path, stringsAsFactors = FALSE)
  testthat::expect_true(all(resumed$package_version == "1.3.1"))
  testthat::expect_true(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", resumed$test_date)))
  testthat::expect_true(all(resumed$catalog_checked_on == "2026-09-05"))
})
