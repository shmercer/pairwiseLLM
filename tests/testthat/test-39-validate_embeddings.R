test_that("validate_embeddings validates matrix inputs", {
  emb <- matrix(1, nrow = 2, ncol = 3)

  expect_error(validate_embeddings(NULL), "must be provided")
  expect_error(validate_embeddings(list()), "must be a matrix")
  storage.mode(emb) <- "character"
  expect_error(validate_embeddings(emb), "numeric matrix")

  emb2 <- matrix(c(1, NA_real_), nrow = 1)
  expect_error(validate_embeddings(emb2), "must not contain missing")

  expect_error(validate_embeddings(matrix(1), allow_na = NA), "allow_na")
})

test_that("validate_embeddings validates and uses ids with rownames", {
  ids <- c("b", "a")
  emb <- matrix(1:8, nrow = 4, ncol = 2)
  rownames(emb) <- c("a", "b", "c", "d") # superset is ok

  out <- validate_embeddings(emb, ids = ids)
  expect_equal(rownames(out), ids)
  expect_equal(out[, 1], c(emb["b", 1], emb["a", 1]))

  emb_missing <- emb
  rownames(emb_missing) <- c("a", "c", "d", "e")
  expect_error(validate_embeddings(emb_missing, ids = ids), "contain all")

  emb_dup <- emb
  rownames(emb_dup) <- c("a", "a", "c", "d")
  expect_error(validate_embeddings(emb_dup, ids = ids), "rownames.*duplicates")

  emb_na <- emb
  rownames(emb_na) <- c("a", NA, "c", "d")
  expect_error(validate_embeddings(emb_na, ids = ids), "missing/empty")

  expect_error(validate_embeddings(emb, ids = c("a", "a")), "ids.*duplicates")
})

test_that("validate_embeddings uses row order when rownames are absent", {
  ids <- c("a", "b", "c")
  emb <- matrix(1:9, nrow = 3, ncol = 3)
  out <- validate_embeddings(emb, ids = ids)
  expect_equal(out, emb)

  emb2 <- matrix(1:4, nrow = 2, ncol = 2)
  expect_error(validate_embeddings(emb2, ids = ids), "nrow == length\\(ids\\)")
})
