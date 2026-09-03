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
