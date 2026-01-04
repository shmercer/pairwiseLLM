test_that("381-01 compute_text_embeddings uses env cache dir when cache_dir is NULL", {
  cache_dir <- tempfile("emb_cache_")
  dir.create(cache_dir)
  withr::local_envvar(PAIRWISELLM_EMBEDDINGS_CACHE_DIR = cache_dir)

  texts <- c("hello", "world")
  engine <- "sentence_transformers"
  model <- "dummy_model"

  key <- pairwiseLLM:::.embeddings_cache_key(
    texts = texts,
    engine = engine,
    model = model,
    batch_size = 2L,
    normalize = FALSE,
    device = NULL,
    show_progress = FALSE,
    dots = list()
  )
  cache_path <- pairwiseLLM:::.embeddings_cache_path(cache_dir, key)

  emb <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  ok <- pairwiseLLM:::.write_embeddings_cache(cache_path, emb)
  expect_true(ok)

  out <- pairwiseLLM::compute_text_embeddings(
    x = texts,
    engine = engine,
    model = model,
    batch_size = 2L,
    normalize = FALSE,
    device = NULL,
    cache_dir = NULL,
    show_progress = FALSE,
    use_cache = TRUE
  )

  expect_equal(out, emb)
})

test_that("381-02 read_embeddings_cache returns matrix and list paths", {
  tmp <- tempfile(fileext = ".rds")
  emb <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
  saveRDS(emb, tmp)

  out_mat <- pairwiseLLM:::.read_embeddings_cache(tmp)
  expect_true(is.list(out_mat))
  expect_true(is.matrix(out_mat$embeddings))
  expect_equal(out_mat$embeddings, emb)

  tmp2 <- tempfile(fileext = ".rds")
  saveRDS(list(embeddings = emb), tmp2)
  out_list <- pairwiseLLM:::.read_embeddings_cache(tmp2)
  expect_true(is.list(out_list))
  expect_true(is.matrix(out_list$embeddings))
  expect_equal(out_list$embeddings, emb)
})

test_that("381-03 write_embeddings_cache handles back-compat and directory errors", {
  emb <- matrix(c(1, 2), nrow = 1)

  # Back-compat: `cache_path` is supported.
  tmp <- tempfile(fileext = ".rds")
  expect_silent(
    ok <- pairwiseLLM:::.write_embeddings_cache(path = NULL, embeddings = emb, cache_path = tmp)
  )
  expect_true(ok)

  # Missing path should error.
  expect_error(
    pairwiseLLM:::.write_embeddings_cache(path = "", embeddings = emb),
    "`path` must be provided.",
    fixed = TRUE
  )

  # When the parent "directory" exists as a file, writing should warn and return FALSE.
  dir_as_file <- tempfile("not_a_dir_")
  writeLines("x", dir_as_file)
  bad_path <- file.path(dir_as_file, "cache.rds")
  expect_warning(
    ok2 <- pairwiseLLM:::.write_embeddings_cache(bad_path, emb),
    "cache directory exists as a file",
    fixed = TRUE
  )
  expect_false(ok2)

  # If creating the cache directory fails, writing should warn and return FALSE.
  missing_dir <- file.path(tempdir(), paste0("nope_", as.integer(Sys.time())))
  path_in_missing <- file.path(missing_dir, "cache.rds")
  expect_warning(
    {
      testthat::local_mocked_bindings(
        dir.create = function(...) FALSE,
        .package = "base"
      )
      ok3 <- pairwiseLLM:::.write_embeddings_cache(path_in_missing, emb)
    },
    "could not create directory",
    fixed = TRUE
  )
  expect_false(ok3)

  # If saveRDS fails, writing should warn and return FALSE.
  expect_warning(
    {
      testthat::local_mocked_bindings(
        saveRDS = function(...) stop("boom"),
        .package = "base"
      )
      ok4 <- pairwiseLLM:::.write_embeddings_cache(file.path(tempdir(), "cache.rds"), emb)
    },
    "Failed to write embeddings cache: boom",
    fixed = TRUE
  )
  expect_false(ok4)
})
