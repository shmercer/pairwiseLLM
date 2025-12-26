testthat::test_that("select_core_set validates inputs", {
  bad <- tibble::tibble(id = "A", txt = "x")
  testthat::expect_error(select_core_set(bad), "columns 'ID' and 'text'")

  one <- tibble::tibble(ID = "A", text = "x")
  testthat::expect_error(select_core_set(one), "at least 2 rows")

  dup <- tibble::tibble(ID = c("A", "A"), text = c("x", "y"))
  testthat::expect_error(select_core_set(dup), "must be unique")

  ok <- tibble::tibble(ID = c("A", "B"), text = c("x", "y"))
  testthat::expect_error(select_core_set(ok, core_size = 1), ">= 2")
  testthat::expect_error(select_core_set(ok, core_pct = 0), "in \\(0, 1\\]")
})

testthat::test_that("select_core_set token_stratified picks spaced word counts", {
  samples <- tibble::tibble(
    ID = paste0("S", 1:6),
    text = c("a", "a b", "a b c", "a b c d", "a b c d e", "a b c d e f")
  )

  core <- select_core_set(samples, core_size = 3, method = "token_stratified", seed = 1)
  testthat::expect_equal(nrow(core), 3)
  testthat::expect_true(all(core$ID %in% samples$ID))
  testthat::expect_equal(core$word_count, c(1L, 4L, 6L)) # positions 1,4,6 for n=6,k=3
})

testthat::test_that("select_core_set random is reproducible with seed", {
  samples <- tibble::tibble(
    ID = paste0("S", 1:10),
    text = rep("x y z", 10)
  )

  a <- select_core_set(samples, core_size = 5, method = "random", seed = 123)
  b <- select_core_set(samples, core_size = 5, method = "random", seed = 123)
  testthat::expect_identical(a$ID, b$ID)
})

testthat::test_that("select_core_set embeddings aligns rownames and selects medoids", {
  samples <- tibble::tibble(
    ID = paste0("S", 1:6),
    text = paste("t", 1:6)
  )

  emb <- rbind(
    c(1, 0),   c(1, 0.1),  c(1, -0.1),
    c(0, 1),   c(0.1, 1),  c(-0.1, 1)
  )
  rownames(emb) <- samples$ID

  # permute rows to test alignment by rownames
  emb_perm <- emb[c("S3", "S2", "S1", "S6", "S5", "S4"), , drop = FALSE]

  core_euc <- select_core_set(samples, core_size = 2, method = "embeddings", embeddings = emb_perm, distance = "euclidean", seed = 1)
  testthat::expect_equal(nrow(core_euc), 2)
  testthat::expect_equal(sort(core_euc$ID), c("S1", "S4"))

  core_cos <- select_core_set(samples, core_size = 2, method = "embeddings", embeddings = emb_perm, distance = "cosine", seed = 1)
  testthat::expect_equal(sort(core_cos$ID), c("S1", "S4"))
})

testthat::test_that("select_core_set errors if embeddings missing", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("x", "y"))
  testthat::expect_error(
    select_core_set(samples, core_size = 2, method = "embeddings", embeddings = NULL),
    "must be provided"
  )
})
