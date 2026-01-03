testthat::test_that("embeddings cache helpers handle error cases", {
  cache_dir <- withr::local_tempdir()

  # Cache path helper: creates directory and yields stable file path.
  cache_path <- pairwiseLLM:::.embeddings_cache_path(cache_dir = cache_dir, key = "k")
  testthat::expect_true(dir.exists(dirname(cache_path)))
  testthat::expect_true(grepl("embeddings_k\\.rds$", cache_path))

  # Cache path helper errors if cache_dir is a file (cannot create dir).
  bad_dir <- withr::local_tempfile()
  file.create(bad_dir)
  testthat::expect_error(
    pairwiseLLM:::.embeddings_cache_path(cache_dir = bad_dir, key = "k"),
    "Could not create cache directory"
  )

  # Read helper: returns NULL for malformed caches.
  saveRDS(list(not_embeddings = 1), cache_path)
  testthat::expect_null(pairwiseLLM:::.read_embeddings_cache(cache_path))

  # Write helper: returns FALSE + warns on write error.
  bad_path <- file.path(bad_dir, "cache.rds")
  val <- NULL
  testthat::expect_warning(
    val <- pairwiseLLM:::.write_embeddings_cache(
      path = bad_path,
      embeddings = matrix(0, nrow = 1, ncol = 2)
    ),
    "Failed to write embeddings cache"
  )
  testthat::expect_false(val)
})

testthat::test_that(".compute_text_embeddings_cached validates compute_fn and output shape", {
  dir <- withr::local_tempdir()

  testthat::expect_error(
    pairwiseLLM:::.compute_text_embeddings_cached(
      texts = c("a"),
      engine = "dummy",
      model = "dummy",
      batch_size = 1,
      normalize = FALSE,
      device = NULL,
      show_progress = FALSE,
      dots = list(),
      cache_dir = dir,
      use_cache = TRUE,
      overwrite = FALSE,
      compute_fn = 1
    ),
    "must be a function"
  )

  bad_rows <- function(texts) {
    matrix(0, nrow = length(texts) + 1, ncol = 2)
  }

  testthat::expect_error(
    pairwiseLLM:::.compute_text_embeddings_cached(
      texts = c("a", "b"),
      engine = "dummy",
      model = "dummy",
      batch_size = 2,
      normalize = FALSE,
      device = NULL,
      show_progress = FALSE,
      dots = list(),
      cache_dir = dir,
      use_cache = TRUE,
      overwrite = FALSE,
      compute_fn = bad_rows
    ),
    "wrong number of rows"
  )
})

testthat::test_that(".compute_text_embeddings_cached uses cache when available", {
  dir <- withr::local_tempdir()
  calls <- 0L

  compute_fn <- function(texts) {
    calls <<- calls + 1L
    matrix(seq_len(length(texts) * 2), nrow = length(texts), ncol = 2)
  }

  m1 <- pairwiseLLM:::.compute_text_embeddings_cached(
    texts = c("a", "b"),
    engine = "dummy",
    model = "dummy",
    batch_size = 2,
    normalize = FALSE,
    device = NULL,
    show_progress = FALSE,
    dots = list(),
    cache_dir = dir,
    use_cache = TRUE,
    overwrite = FALSE,
    compute_fn = compute_fn
  )

  m2 <- pairwiseLLM:::.compute_text_embeddings_cached(
    texts = c("a", "b"),
    engine = "dummy",
    model = "dummy",
    batch_size = 2,
    normalize = FALSE,
    device = NULL,
    show_progress = FALSE,
    dots = list(),
    cache_dir = dir,
    use_cache = TRUE,
    overwrite = FALSE,
    compute_fn = compute_fn
  )

  testthat::expect_equal(calls, 1L)
  testthat::expect_equal(m1, m2)
})
