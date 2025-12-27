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

testthat::test_that("select_core_set validates sample IDs and sizing rules", {
  ok <- tibble::tibble(ID = c("A", "B", "C"), text = c("t1", "t2", "t3"))

  testthat::expect_error(
    select_core_set(tibble::tibble(ID = "A", text = "t"), core_size = 2, method = "random"),
    "at least 2"
  )

  testthat::expect_error(
    select_core_set(tibble::tibble(ID = c("A", NA), text = c("t1", "t2")), core_size = 2, method = "random"),
    "non-missing"
  )

  testthat::expect_error(
    select_core_set(tibble::tibble(ID = c("A", ""), text = c("t1", "t2")), core_size = 2, method = "random"),
    "non-missing"
  )

  testthat::expect_error(
    select_core_set(tibble::tibble(ID = c("A", "A"), text = c("t1", "t2")), core_size = 2, method = "random"),
    "unique"
  )

  testthat::expect_error(
    select_core_set(ok, core_size = 4, method = "random"),
    "cannot exceed"
  )
})

testthat::test_that("select_core_set core_pct and core_size validation branches are covered", {
  samples <- tibble::tibble(ID = paste0("S", 1:10), text = paste("t", 1:10))

  testthat::expect_error(
    select_core_set(samples, core_size = NULL, core_pct = 0, method = "random"),
    "core_pct"
  )
  testthat::expect_error(
    select_core_set(samples, core_size = NULL, core_pct = 1.5, method = "random"),
    "core_pct"
  )
  testthat::expect_error(
    select_core_set(samples, core_size = NULL, core_pct = NA_real_, method = "random"),
    "core_pct"
  )
  testthat::expect_error(
    select_core_set(samples, core_size = NULL, core_pct = c(0.1, 0.2), method = "random"),
    "core_pct"
  )

  testthat::expect_error(
    select_core_set(samples, core_size = 1, method = "random"),
    ">= 2"
  )
  testthat::expect_error(
    select_core_set(samples, core_size = NA_integer_, method = "random"),
    ">= 2"
  )

  # core_size=NULL path (core_pct determines k)
  out <- select_core_set(samples, core_size = NULL, core_pct = 0.40, method = "random", seed = 1)
  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(out$ID %in% samples$ID))
})

testthat::test_that("select_core_set seed validation and CRAN-safe restoration (seed present and missing)", {
  samples <- tibble::tibble(ID = paste0("S", 1:6), text = paste("t", 1:6))

  testthat::expect_error(
    select_core_set(samples, core_size = 2, method = "random", seed = c(1, 2)),
    "single integer"
  )
  testthat::expect_error(
    select_core_set(samples, core_size = 2, method = "random", seed = NA_integer_),
    "single integer"
  )
  testthat::expect_error(
    suppressWarnings(select_core_set(samples, core_size = 2, method = "random", seed = "not_a_number")),
    "single integer"
  )

  # Case 1: .Random.seed missing -> after call it should still be missing
  had_seed0 <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed0 <- NULL
  if (had_seed0) old_seed0 <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed0) rm(".Random.seed", envir = .GlobalEnv)

  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  invisible(select_core_set(samples, core_size = 2, method = "random", seed = 1))
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  # restore original pre-test RNG state
  if (had_seed0) assign(".Random.seed", old_seed0, envir = .GlobalEnv)

  # Case 2: .Random.seed present -> after call it should be identical
  set.seed(999)
  old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  invisible(select_core_set(samples, core_size = 2, method = "random", seed = 123))
  new_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  testthat::expect_identical(new_seed, old_seed)
})

testthat::test_that("select_core_set token_stratified returns word_count and stable structure", {
  samples <- tibble::tibble(
    ID = paste0("S", 1:8),
    text = c("a", "a a", "a a a", "a a a a", "a a a a a", "x", "x x", "x x x")
  )
  out <- select_core_set(samples, core_size = 4, method = "token_stratified", seed = 1)
  testthat::expect_equal(nrow(out), 4)
  testthat::expect_true(all(out$ID %in% samples$ID))
  testthat::expect_true(all(!is.na(out$word_count)))
  testthat::expect_true(all(out$core_rank == seq_len(4)))
})

testthat::test_that("select_core_set embeddings validates embeddings alignment inputs", {
  samples <- tibble::tibble(ID = paste0("S", 1:5), text = paste("t", 1:5))

  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = NULL),
    "must be provided"
  )

  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = list(1, 2, 3)),
    "must be a matrix"
  )

  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = matrix("a", nrow = 5, ncol = 2)),
    "numeric matrix"
  )

  emb_bad_rn <- matrix(rnorm(5 * 3), nrow = 5, ncol = 3)
  rownames(emb_bad_rn) <- paste0("X", 1:5)
  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = emb_bad_rn),
    "rownames must contain"
  )

  emb_bad_n <- matrix(rnorm(4 * 3), nrow = 4, ncol = 3)
  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = emb_bad_n),
    "nrow == nrow\\(samples\\)"
  )
})

testthat::test_that("select_core_set auto chooses PAM vs CLARA and 'embeddings' is an alias", {
  samples <- tibble::tibble(
    ID = paste0("S", 1:6),
    text = paste("t", 1:6)
  )
  emb <- matrix(rnorm(6 * 3), nrow = 6, ncol = 3)
  rownames(emb) <- samples$ID

  cap <- new.env(parent = emptyenv())
  cap$called <- character(0)

  testthat::local_mocked_bindings(
    .select_medoids_pam = function(emb, k) {
      cap$called <- c(cap$called, "pam")
      list(medoids = c(1L, 4L), clustering = c(1L, 1L, 1L, 2L, 2L, 2L))
    },
    .select_medoids_clara = function(emb, k, samples = 5L, sampsize = NULL) {
      cap$called <- c(cap$called, "clara")
      list(medoids = c(2L, 5L), clustering = c(1L, 1L, 1L, 2L, 2L, 2L))
    },
    .package = "pairwiseLLM"
  )

  # auto -> PAM when n <= clara_threshold
  out1 <- select_core_set(samples, core_size = 2, method = "auto", embeddings = emb, clara_threshold = 6L, seed = 1)
  testthat::expect_true("pam" %in% cap$called)
  testthat::expect_equal(out1$ID, c("S1", "S4"))

  # auto -> CLARA when n > clara_threshold
  cap$called <- character(0)
  out2 <- select_core_set(samples, core_size = 2, method = "auto", embeddings = emb, clara_threshold = 5L, seed = 1)
  testthat::expect_true("clara" %in% cap$called)
  testthat::expect_equal(out2$ID, c("S2", "S5"))

  # 'embeddings' is an alias for auto (should follow the same branch logic)
  cap$called <- character(0)
  out3 <- select_core_set(samples, core_size = 2, method = "embeddings", embeddings = emb, clara_threshold = 5L, seed = 1)
  testthat::expect_true("clara" %in% cap$called)
  testthat::expect_equal(out3$ID, c("S2", "S5"))
})

testthat::test_that("select_core_set embeddings succeeds with and without rownames; cosine path exercised", {
  samples <- tibble::tibble(ID = paste0("S", 1:6), text = paste("t", 1:6))

  # no rownames: assumes same order
  set.seed(1)
  emb1 <- matrix(rnorm(6 * 4), nrow = 6, ncol = 4)
  out1 <- select_core_set(samples, core_size = 3, method = "embeddings", embeddings = emb1, distance = "cosine", seed = 1)
  testthat::expect_equal(nrow(out1), 3)
  testthat::expect_true(all(out1$ID %in% samples$ID))
  testthat::expect_true(all(!is.na(out1$cluster)))

  # with rownames: align by ID, even if shuffled
  emb2 <- emb1[sample.int(6), , drop = FALSE]
  rownames(emb2) <- samples$ID[sample.int(6)]
  # make rownames match all IDs (shuffle but complete)
  rownames(emb2) <- sample(samples$ID, 6)
  out2 <- select_core_set(samples, core_size = 3, method = "embeddings", embeddings = emb2, distance = "euclidean", seed = 2)
  testthat::expect_equal(nrow(out2), 3)
  testthat::expect_true(all(out2$ID %in% samples$ID))
})

testthat::test_that("select_core_set surfaces embedding-clustering failures with clear error", {
  samples <- tibble::tibble(ID = paste0("S", 1:5), text = paste("t", 1:5))
  emb <- matrix(rnorm(5 * 3), nrow = 5, ncol = 3)
  rownames(emb) <- samples$ID

  testthat::local_mocked_bindings(
    .select_medoids_from_embeddings = function(...) {
      stop("forced failure")
    },
    .package = "pairwiseLLM"
  )

  testthat::expect_error(
    select_core_set(samples, core_size = 3, method = "embeddings", embeddings = emb, distance = "euclidean"),
    "Embedding clustering failed"
  )
})
