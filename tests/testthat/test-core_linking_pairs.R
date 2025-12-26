testthat::test_that("select_core_link_pairs validates required columns and inputs", {
  samples <- tibble::tibble(ID = c("A", "B"), text = c("x", "y"))
  theta <- tibble::tibble(ID = c("A", "B"), theta = c(0, 0), se = c(0.5, 0.5))

  testthat::expect_error(
    select_core_link_pairs(samples = tibble::tibble(ID = "A"), theta = theta, core_ids = "A", round_size = 1),
    "must have columns 'ID' and 'text'"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = tibble::tibble(ID = "A"), core_ids = "A", round_size = 1),
    "`theta` must have columns"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = character(0), round_size = 1),
    "`core_ids` must contain at least 1 ID"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "Z", round_size = 1),
    "present in `samples\\$ID`"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "A", round_size = -1),
    "`round_size` must be >= 0"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "A", round_size = 1, within_batch_frac = 2),
    "within_batch_frac"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "A", round_size = 1, core_audit_frac = 2),
    "core_audit_frac"
  )

  testthat::expect_error(
    select_core_link_pairs(
      samples = samples, theta = theta, core_ids = "A", round_size = 1,
      within_batch_frac = 0.8, core_audit_frac = 0.3
    ),
    "must be <= 1"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "A", round_size = 1, k_neighbors = 0),
    "k_neighbors"
  )

  testthat::expect_error(
    select_core_link_pairs(samples = samples, theta = theta, core_ids = "A", round_size = 1, min_judgments = -1),
    "min_judgments"
  )

  testthat::expect_error(
    select_core_link_pairs(
      samples = samples, theta = theta, core_ids = "A", round_size = 1,
      existing_pairs = tibble::tibble(x = 1)
    ),
    "existing_pairs"
  )
})

testthat::test_that("select_core_link_pairs returns empty tibble when round_size = 0", {
  samples <- tibble::tibble(ID = paste0("S", 1:4), text = paste("t", 1:4))
  theta <- tibble::tibble(ID = samples$ID, theta = 0, se = 0.5)
  out <- select_core_link_pairs(samples, theta, core_ids = c("S1", "S2"), round_size = 0)
  testthat::expect_identical(names(out), c("ID1", "ID2", "pair_type"))
  testthat::expect_equal(nrow(out), 0)
})

testthat::test_that("select_core_link_pairs allocates core_new / new_new / core_core as expected", {
  samples <- tibble::tibble(ID = paste0("S", 1:12), text = paste("t", 1:12))
  theta <- tibble::tibble(ID = samples$ID, theta = seq(-1, 1, length.out = 12), se = rep(0.4, 12))
  core_ids <- paste0("S", 1:4)

  out <- select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = core_ids,
    round_size = 20,
    within_batch_frac = 0.50,
    core_audit_frac = 0.10,
    seed = 1
  )

  testthat::expect_true(all(out$ID1 != out$ID2))
  testthat::expect_true(all(out$pair_type %in% c("core_new", "new_new", "core_core")))

  # core_core pairs should be between core IDs only
  cc <- out[out$pair_type == "core_core", ]
  if (nrow(cc) > 0) {
    testthat::expect_true(all(cc$ID1 %in% core_ids))
    testthat::expect_true(all(cc$ID2 %in% core_ids))
  }

  # new_new pairs should be between non-core IDs only
  nn <- out[out$pair_type == "new_new", ]
  if (nrow(nn) > 0) {
    testthat::expect_true(all(!nn$ID1 %in% core_ids))
    testthat::expect_true(all(!nn$ID2 %in% core_ids))
  }

  # core_new pairs should include one core and one new
  cn <- out[out$pair_type == "core_new", ]
  if (nrow(cn) > 0) {
    one_core <- (cn$ID1 %in% core_ids) + (cn$ID2 %in% core_ids)
    testthat::expect_true(all(one_core == 1))
  }
})

testthat::test_that("select_core_link_pairs forbids repeats against existing_pairs (ID1/ID2 and object1/object2)", {
  samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
  theta <- tibble::tibble(ID = samples$ID, theta = seq(-1, 1, length.out = 8), se = rep(0.5, 8))
  core_ids <- paste0("S", 1:3)

  existing1 <- tibble::tibble(ID1 = "S4", ID2 = "S1")
  out1 <- select_core_link_pairs(
    samples, theta,
    core_ids = core_ids,
    round_size = 10, within_batch_frac = 0, core_audit_frac = 0,
    existing_pairs = existing1, forbid_repeats = TRUE, seed = 1
  )
  keys1 <- paste(pmin(out1$ID1, out1$ID2), pmax(out1$ID1, out1$ID2), sep = "||")
  testthat::expect_false("S1||S4" %in% keys1)

  existing2 <- tibble::tibble(object1 = "S5", object2 = "S2")
  out2 <- select_core_link_pairs(
    samples, theta,
    core_ids = core_ids,
    round_size = 10, within_batch_frac = 0, core_audit_frac = 0,
    existing_pairs = existing2, forbid_repeats = TRUE, seed = 1
  )
  keys2 <- paste(pmin(out2$ID1, out2$ID2), pmax(out2$ID1, out2$ID2), sep = "||")
  testthat::expect_false("S2||S5" %in% keys2)
})

testthat::test_that("select_core_link_pairs is reproducible with seed even when .Random.seed is missing", {
  # Ensure RNG state is uninitialized (mirrors R CMD check failure mode)
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  samples <- tibble::tibble(ID = paste0("S", 1:10), text = paste("t", 1:10))
  theta <- tibble::tibble(ID = samples$ID, theta = rnorm(10), se = runif(10, 0.2, 0.8))
  core_ids <- paste0("S", 1:3)

  out1 <- select_core_link_pairs(samples, theta, core_ids = core_ids, round_size = 12, seed = 123)
  out2 <- select_core_link_pairs(samples, theta, core_ids = core_ids, round_size = 12, seed = 123)
  testthat::expect_identical(out1, out2)
})

testthat::test_that("bt_core_link_round validates fit and returns plan", {
  samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
  theta <- tibble::tibble(ID = samples$ID, theta = seq(-1, 1, length.out = 8), se = rep(0.4, 8))
  fit <- list(theta = theta)

  testthat::expect_error(
    bt_core_link_round(samples, list(), core_ids = "S1", round_size = 1),
    "containing.*\\$theta"
  )

  out <- bt_core_link_round(samples, fit, core_ids = paste0("S", 1:3), round_size = 10, seed = 1)
  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("pairs", "plan") %in% names(out)))
  testthat::expect_equal(nrow(out$plan), 1)
  testthat::expect_true(all(c("n_total", "n_core_new", "n_new_new", "n_core_core") %in% names(out$plan)))
  testthat::expect_equal(out$plan$n_total, nrow(out$pairs))
})
