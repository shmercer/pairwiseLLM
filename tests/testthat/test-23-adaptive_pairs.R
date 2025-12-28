test_that("select_adaptive_pairs validates inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(1, 1))

  expect_error(select_adaptive_pairs(samples[, "ID"], theta, n_pairs = 1), "samples.*columns")
  expect_error(select_adaptive_pairs(samples, theta[, "ID"], n_pairs = 1), "theta.*columns")

  expect_error(select_adaptive_pairs(samples, theta, n_pairs = -1), "non-negative")
  expect_error(select_adaptive_pairs(samples, theta, n_pairs = 1, k_neighbors = 0), "positive")
  expect_error(select_adaptive_pairs(samples, theta, n_pairs = 1, min_judgments = -5), "non-negative")

  expect_error(
    select_adaptive_pairs(tibble::tibble(ID = "A", text = "a"), theta, n_pairs = 1),
    "At least two samples"
  )
})

test_that("select_adaptive_pairs returns empty tibble when n_pairs = 0", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("a", "b"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(1, 1))

  out <- select_adaptive_pairs(samples, theta, n_pairs = 0)
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), c("ID1", "text1", "ID2", "text2"))
  expect_equal(nrow(out), 0L)
})

test_that("select_adaptive_pairs can handle missing theta rows (fill behavior)", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = c("a", "b", "c")
  )
  # Missing C in theta
  theta <- tibble::tibble(
    ID = c("A", "B"),
    theta = c(0, 0.1),
    se = c(0.5, 0.5)
  )

  out <- select_adaptive_pairs(samples, theta, n_pairs = 1, seed = 1)
  expect_equal(nrow(out), 1L)
  expect_true(all(c(out$ID1, out$ID2) %in% samples$ID))
})

test_that("select_adaptive_pairs forbids repeats against existing ID1/ID2", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = paste0("t", c("A", "B", "C"))
  )
  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 0.01, 2),
    se = c(1, 1, 1)
  )

  existing <- tibble::tibble(ID1 = "A", ID2 = "B")

  out <- select_adaptive_pairs(
    samples, theta,
    existing_pairs = existing,
    n_pairs = 2,
    k_neighbors = 2,
    forbid_repeats = TRUE,
    seed = 1
  )

  # ensure A-B does not appear (unordered)
  keys <- paste0(pmin(out$ID1, out$ID2), "-", pmax(out$ID1, out$ID2))
  expect_false("A-B" %in% keys)
})

test_that("select_adaptive_pairs accepts existing pairs as object1/object2 (BT format)", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C"),
    text = paste0("t", c("A", "B", "C"))
  )
  theta <- tibble::tibble(
    ID = c("A", "B", "C"),
    theta = c(0, 0.01, 2),
    se = c(1, 1, 1)
  )

  existing_bt <- tibble::tibble(object1 = "A", object2 = "B", result = 1)

  out <- select_adaptive_pairs(
    samples, theta,
    existing_pairs = existing_bt,
    n_pairs = 2,
    k_neighbors = 2,
    forbid_repeats = TRUE,
    seed = 1
  )

  keys <- paste0(pmin(out$ID1, out$ID2), "-", pmax(out$ID1, out$ID2))
  expect_false("A-B" %in% keys)
})

test_that("select_adaptive_pairs prioritizes low-judgment items via min_judgments", {
  samples <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    text = paste("Sample", c("A", "B", "C", "D"))
  )

  # Two tight clusters: (A,B) and (C,D)
  theta <- tibble::tibble(
    ID = c("A", "B", "C", "D"),
    theta = c(0.00, 0.01, 3.00, 3.01),
    se = c(1, 1, 1, 1)
  )

  # Make C/D "already judged" many times; A/B not judged
  existing <- tibble::tibble(
    ID1 = rep("C", 20),
    ID2 = rep("D", 20)
  )

  out <- select_adaptive_pairs(
    samples, theta,
    existing_pairs = existing,
    n_pairs = 1,
    k_neighbors = 1,
    min_judgments = 12,
    seed = 1
  )

  # Expect the selected pair to be A-B (not C-D)
  key <- paste0(pmin(out$ID1, out$ID2), "-", pmax(out$ID1, out$ID2))
  expect_identical(key, "A-B")
})

test_that("select_adaptive_pairs balances positions when requested", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )
  theta <- tibble::tibble(
    ID = c("A", "B"),
    theta = c(0, 0.01),
    se = c(1, 1)
  )

  # A has many appearances as ID1 already; B has many as ID2
  existing <- tibble::tibble(
    ID1 = rep("A", 20),
    ID2 = rep("B", 20)
  )

  out <- select_adaptive_pairs(
    samples, theta,
    existing_pairs = existing,
    n_pairs = 1,
    k_neighbors = 1,
    forbid_repeats = FALSE, # allow A-B so we can test orientation
    balance_positions = TRUE,
    seed = 1
  )

  # To reduce imbalance, A should be placed as ID2
  expect_identical(out$ID1, "B")
  expect_identical(out$ID2, "A")
})

test_that("select_adaptive_pairs can return empty when no candidates remain", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("a", "b")
  )
  theta <- tibble::tibble(
    ID = c("A", "B"),
    theta = c(0, 0),
    se = c(1, 1)
  )

  existing <- tibble::tibble(ID1 = "A", ID2 = "B")

  out <- select_adaptive_pairs(
    samples, theta,
    existing_pairs = existing,
    n_pairs = 1,
    k_neighbors = 1,
    forbid_repeats = TRUE,
    seed = 1
  )

  expect_equal(nrow(out), 0L)
})

test_that("select_adaptive_pairs seed handling works when .Random.seed does not exist", {
  # Save and remove any existing .Random.seed to force had_seed == FALSE
  had_seed_before <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed_before) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    rm(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
  } else {
    on.exit(
      {
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )
  }

  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 0.1), se = c(0.5, 0.5))

  out <- select_adaptive_pairs(samples, theta, n_pairs = 1, seed = 1)
  expect_equal(nrow(out), 1L)

  # Since there was no seed before, function should clean it up afterward
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("select_adaptive_pairs restores .Random.seed when it exists", {
  # Ensure .Random.seed exists
  stats::runif(1)

  old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)

  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = c("A", "B", "C"), theta = c(0, 0.1, 0.2), se = c(1, 1, 1))

  out <- select_adaptive_pairs(samples, theta, n_pairs = 1, seed = 1)
  expect_equal(nrow(out), 1L)

  # RNG state should be restored exactly
  expect_identical(get(".Random.seed", envir = .GlobalEnv, inherits = FALSE), old_seed)
})

test_that("select_adaptive_pairs counts existing first-position IDs even when some are out-of-sample", {
  samples <- tibble::tibble(ID = c("A", "B", "C"), text = c("a", "b", "c"))
  theta <- tibble::tibble(ID = samples$ID, theta = c(0, 0.1, 0.2), se = c(1, 1, 1))

  # Mix of in-sample and out-of-sample IDs in ID1; only in-sample should contribute.
  existing_pairs <- tibble::tibble(
    ID1 = c("A", "X", "A"),
    ID2 = c("B", "A", "C")
  )

  out <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 1,
    existing_pairs = existing_pairs,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    seed = 1
  )

  expect_equal(nrow(out), 1L)
  expect_true(all(out$ID1 %in% samples$ID))
  expect_true(all(out$ID2 %in% samples$ID))
})
