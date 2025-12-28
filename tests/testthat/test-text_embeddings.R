test_that("compute_text_embeddings validates inputs", {
  expect_error(compute_text_embeddings(1), "character vector")
  expect_error(compute_text_embeddings(character()), "at least 1")
  expect_error(compute_text_embeddings(c(NA_character_)), "must not contain")

  expect_error(
    compute_text_embeddings(c("a", "b"), ids = c("x")),
    "length equal"
  )

  expect_error(
    compute_text_embeddings(c("a", "b"), batch_size = 0),
    "batch_size"
  )
  expect_error(
    compute_text_embeddings(c("a"), normalize = NA),
    "normalize"
  )
  expect_error(
    compute_text_embeddings(c("a"), show_progress = NA),
    "show_progress"
  )
  expect_error(
    compute_text_embeddings(c("a"), device = NA_character_),
    "device"
  )
})

test_that("compute_text_embeddings errors when reticulate is not available", {
  testthat::local_mocked_bindings(
    .has_reticulate = function() FALSE,
    .package = "pairwiseLLM"
  )
  expect_error(
    compute_text_embeddings(c("a")),
    "reticulate"
  )
})

test_that("compute_text_embeddings errors when python module is missing", {
  testthat::local_mocked_bindings(
    .has_reticulate = function() TRUE,
    .rt_py_module_available = function(module) FALSE,
    .package = "pairwiseLLM"
  )
  expect_error(
    compute_text_embeddings(c("a")),
    "sentence_transformers"
  )
})

test_that("compute_text_embeddings returns a numeric matrix and preserves IDs", {
  called <- new.env(parent = emptyenv())

  fake_st <- list(
    SentenceTransformer = function(model, device = NULL) {
      called$model <- model
      called$device <- device
      structure(list(
        encode = function(sentences,
                          batch_size,
                          show_progress_bar,
                          convert_to_numpy,
                          normalize_embeddings,
                          ...) {
          called$sentences <- sentences
          called$batch_size <- batch_size
          called$show_progress_bar <- show_progress_bar
          called$convert_to_numpy <- convert_to_numpy
          called$normalize_embeddings <- normalize_embeddings
          # Return something that py_to_r would turn into a matrix
          matrix(seq_along(sentences) * 1.0, ncol = 1)
        }
      ), class = "fake_sentence_transformer")
    }
  )

  testthat::local_mocked_bindings(
    .has_reticulate = function() TRUE,
    .rt_py_module_available = function(module) TRUE,
    .rt_import = function(module) fake_st,
    .rt_py_to_r = function(x) x,
    .package = "pairwiseLLM"
  )

  x <- tibble::tibble(ID = c("id1", "id2"), text = c("hello", "goodbye"))
  emb <- compute_text_embeddings(x, batch_size = 5L, normalize = TRUE, device = "cpu", show_progress = FALSE)

  expect_true(is.matrix(emb))
  expect_type(emb, "double")
  expect_equal(nrow(emb), 2L)
  expect_equal(rownames(emb), c("id1", "id2"))

  expect_equal(called$model, "all-MiniLM-L6-v2")
  expect_equal(called$device, "cpu")
  expect_equal(as.character(called$sentences), c("hello", "goodbye"))
  expect_equal(called$batch_size, 5L)
  expect_false(called$show_progress_bar)
  expect_true(called$convert_to_numpy)
  expect_true(called$normalize_embeddings)
})

test_that("compute_text_embeddings handles data.frame input and infers ids from ID when ids is NULL", {
  called <- new.env(parent = emptyenv())

  fake_st <- list(
    SentenceTransformer = function(model, device = NULL) {
      called$model <- model
      called$device_missing <- missing(device)
      called$device <- if (!missing(device)) device else NULL

      # Return "model" object with encode() method
      list(
        encode = function(sentences,
                          batch_size,
                          show_progress_bar,
                          convert_to_numpy,
                          normalize_embeddings,
                          ...) {
          called$sentences <- sentences
          called$batch_size <- batch_size
          called$show_progress_bar <- show_progress_bar
          called$convert_to_numpy <- convert_to_numpy
          called$normalize_embeddings <- normalize_embeddings

          # Pretend "numpy array"
          matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
        }
      )
    }
  )

  testthat::local_mocked_bindings(
    .has_reticulate = function() TRUE,
    .rt_py_module_available = function(module) TRUE,
    .rt_import = function(module) fake_st,
    .rt_py_to_r = function(x) x,
    .package = "pairwiseLLM"
  )

  x_df <- tibble::tibble(ID = c("id1", "id2"), text = c("hello", "goodbye"))

  emb <- compute_text_embeddings(
    x_df,
    ids = NULL, # should infer from x_df$ID
    model = "all-MiniLM-L6-v2",
    batch_size = 7L,
    normalize = TRUE,
    device = NULL, # exercise device=NULL branch
    show_progress = FALSE
  )

  expect_true(is.matrix(emb))
  expect_type(emb, "double")
  expect_equal(dim(emb), c(2L, 2L))
  expect_equal(rownames(emb), c("id1", "id2"))

  # Confirm constructor branch used (device missing when not passed)
  expect_true(isTRUE(called$device_missing))
  expect_equal(called$model, "all-MiniLM-L6-v2")

  # Confirm encode saw the converted text vector and args
  expect_equal(as.character(called$sentences), c("hello", "goodbye"))
  expect_equal(called$batch_size, 7L)
  expect_false(called$show_progress_bar)
  expect_true(called$convert_to_numpy)
  expect_true(called$normalize_embeddings)
})

test_that("compute_text_embeddings wraps encode() errors with a helpful message", {
  fake_st <- list(
    SentenceTransformer = function(model, device = NULL) {
      list(
        encode = function(...) {
          stop("boom from encode")
        }
      )
    }
  )

  testthat::local_mocked_bindings(
    .has_reticulate = function() TRUE,
    .rt_py_module_available = function(module) TRUE,
    .rt_import = function(module) fake_st,
    .rt_py_to_r = function(x) x,
    .package = "pairwiseLLM"
  )

  expect_error(
    compute_text_embeddings(
      c("hello"),
      ids = "id1",
      show_progress = FALSE
    ),
    "Embedding computation failed: boom from encode"
  )
})

testthat::test_that("compute_text_embeddings validates data.frame input with missing text column", {
  df <- tibble::tibble(ID = "A", txt = "hello")
  testthat::expect_error(
    pairwiseLLM::compute_text_embeddings(df),
    "column named 'text'"
  )
})

testthat::test_that("reticulate wrapper helpers are callable", {
  # .has_reticulate() line is always coverable (TRUE/FALSE)
  has_rt <- pairwiseLLM:::.has_reticulate()
  testthat::expect_type(has_rt, "logical")
  testthat::expect_equal(length(has_rt), 1L)

  if (!isTRUE(has_rt)) testthat::skip("reticulate not installed")

  # Ensure reticulate has a Python available; if not, skip so tests are robust
  py_ok <- FALSE
  tryCatch(
    {
      py_ok <- reticulate::py_available(initialize = TRUE)
    },
    error = function(e) {
      py_ok <- FALSE
    }
  )
  if (!isTRUE(py_ok)) testthat::skip("Python not available for reticulate")

  # These hit lines 135, 139, 143
  testthat::expect_type(pairwiseLLM:::.rt_py_module_available("math"), "logical")

  mod <- pairwiseLLM:::.rt_import("math")
  testthat::expect_false(is.null(mod))

  py_vec <- reticulate::r_to_py(c(1, 2, 3))
  r_vec <- pairwiseLLM:::.rt_py_to_r(py_vec)
  testthat::expect_equal(as.numeric(r_vec), c(1, 2, 3))
})
