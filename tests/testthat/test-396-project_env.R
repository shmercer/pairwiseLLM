test_that("project .Renviron helpers print instructions (no file writing)", {
  tmp <- withr::local_tempdir()
  f <- file.path(tmp, ".Renviron")

  expect_false(file.exists(f))

  # Reticulate python helper prints the KEY=... line and returns it (invisibly)
  expect_message(
    entry1 <- set_project_reticulate_python("/tmp/python", file = f),
    "RETICULATE_PYTHON"
  )
  expect_identical(entry1, "RETICULATE_PYTHON=\"/tmp/python\"")
  expect_false(file.exists(f))

  # overwrite is ignored but still accepted for backward compatibility
  expect_message(
    entry2 <- set_project_reticulate_python("/tmp/other", file = f, overwrite = TRUE),
    "RETICULATE_PYTHON"
  )
  expect_identical(entry2, "RETICULATE_PYTHON=\"/tmp/other\"")
  expect_false(file.exists(f))

  # Cache dir helper
  expect_message(
    entry3 <- set_project_embeddings_cache_dir("./.cache/pairwiseLLM", file = f, overwrite = TRUE),
    "PAIRWISELLM_EMBEDDINGS_CACHE_DIR"
  )
  expect_identical(entry3, "PAIRWISELLM_EMBEDDINGS_CACHE_DIR=\"./.cache/pairwiseLLM\"")
  expect_false(file.exists(f))
})
