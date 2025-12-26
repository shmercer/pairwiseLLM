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

testthat::test_that("select_core_link_pairs short-circuits when round_size = 0", {
  samples <- tibble::tibble(ID = paste0("S", 1:6), text = paste("t", 1:6))
  theta <- tibble::tibble(ID = samples$ID, theta = seq_along(samples$ID), se = rep(0.3, nrow(samples)))

  out <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = paste0("S", 1:2),
    round_size = 0
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(names(out), c("ID1", "ID2", "pair_type"))
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("select_core_link_pairs can return empty when no candidates are possible", {
  samples <- tibble::tibble(ID = paste0("S", 1:3), text = paste("t", 1:3))
  theta <- tibble::tibble(ID = samples$ID, theta = c(0, 1, 2), se = c(0.2, 0.2, 0.2))

  # core_ids length 1 => cannot do core-core audit
  # new_ids empty => cannot do within or core-new linking
  out <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = "S1",
    new_ids = character(0),
    round_size = 5,
    core_audit_frac = 0.5,
    within_batch_frac = 0.5,
    seed = 1
  )

  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("select_core_link_pairs normalizes existing_pairs schemas and rejects unknown schemas", {
  samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
  theta <- tibble::tibble(ID = samples$ID, theta = rnorm(8), se = runif(8, 0.2, 0.6))
  core_ids <- paste0("S", 1:3)
  new_ids <- paste0("S", 4:8)

  # object1/object2 schema accepted
  existing_bt <- tibble::tibble(object1 = "S4", object2 = "S1")
  out1 <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = core_ids,
    new_ids = new_ids,
    existing_pairs = existing_bt,
    round_size = 2,
    core_audit_frac = 0,
    within_batch_frac = 0,
    seed = 1
  )
  testthat::expect_true(all(out1$pair_type %in% c("core_new", "new_new", "core_core")))

  # unknown schema rejected
  bad_existing <- tibble::tibble(a = 1, b = 2)
  testthat::expect_error(
    pairwiseLLM::select_core_link_pairs(
      samples = samples,
      theta = theta,
      core_ids = core_ids,
      new_ids = new_ids,
      existing_pairs = bad_existing,
      round_size = 1
    ),
    "existing_pairs"
  )
})

testthat::test_that("select_core_link_pairs produces all pair types with expected allocation when feasible", {
  samples <- tibble::tibble(ID = paste0("S", 1:14), text = paste("t", 1:14))
  theta <- tibble::tibble(
    ID = samples$ID,
    theta = seq(-1, 1, length.out = nrow(samples)),
    se = rep(0.3, nrow(samples))
  )

  core_ids <- paste0("S", 1:6)
  new_ids <- paste0("S", 7:14)

  # round_size=20, core_audit_frac=0.2 => 4 audit
  # remain=16, within_batch_frac=0.25 => 4 within
  # link = 12
  pairs <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = core_ids,
    new_ids = new_ids,
    round_size = 20,
    core_audit_frac = 0.2,
    within_batch_frac = 0.25,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    seed = 1
  )

  testthat::expect_equal(nrow(pairs), 20L)
  testthat::expect_equal(sum(pairs$pair_type == "core_core"), 4L)
  testthat::expect_equal(sum(pairs$pair_type == "new_new"), 4L)
  testthat::expect_equal(sum(pairs$pair_type == "core_new"), 12L)

  testthat::expect_true(all(pairs$ID1 %in% samples$ID))
  testthat::expect_true(all(pairs$ID2 %in% samples$ID))
  testthat::expect_true(all(pairs$ID1 != pairs$ID2))
})

testthat::test_that("forbid_repeats skips forbidden nearest opponent; balance_positions=FALSE preserves order", {
  ids <- c(paste0("C", 1:2), paste0("N", 1:4))
  samples <- tibble::tibble(ID = ids, text = paste("t", ids))

  theta <- tibble::tibble(
    ID = ids,
    theta = c(0.1, 2.0, 0.0, 10, 11, 12),
    se = c(0.2, 0.2, 0.9, 0.2, 0.2, 0.2)
  )

  core_ids <- c("C1", "C2")
  new_ids <- c("N1", "N2", "N3", "N4")

  # Make N2/N3/N4 meet min_judgments=2 (need=0), leaving N1 highest priority.
  existing <- tibble::tibble(
    ID1 = c("N2", "N2", "N3", "N3", "N4", "N4", "N1"),
    ID2 = c("C1", "C2", "C1", "C2", "C1", "C2", "C1") # N1-C1 is forbidden
  )

  pairs <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = core_ids,
    new_ids = new_ids,
    existing_pairs = existing,
    round_size = 1,
    core_audit_frac = 0,
    within_batch_frac = 0,
    min_judgments = 2,
    k_neighbors = 2,
    forbid_repeats = TRUE,
    balance_positions = FALSE,
    seed = 1
  )

  testthat::expect_equal(nrow(pairs), 1L)
  testthat::expect_equal(pairs$pair_type, "core_new")
  # N1 should be picked; nearest core by theta is C1 but forbidden, so expect C2.
  testthat::expect_equal(pairs$ID1, "N1")
  testthat::expect_equal(pairs$ID2, "C2")
})

testthat::test_that("focus with missing theta triggers randomized opponent ordering for within-batch pairs", {
  ids <- c("C1", paste0("N", 1:3))
  samples <- tibble::tibble(ID = ids, text = paste("t", ids))

  theta <- tibble::tibble(
    ID = ids,
    theta = c(0.0, NA_real_, 0.5, 1.0), # N1 theta missing
    se = c(0.2, 0.8, 0.2, 0.2)
  )

  core_ids <- "C1"
  new_ids <- paste0("N", 1:3)

  # Boost N1 priority by giving N2/N3 enough judgments already.
  existing <- tibble::tibble(
    ID1 = c("N2", "N2", "N3", "N3"),
    ID2 = c("C1", "C1", "C1", "C1")
  )

  pairs <- pairwiseLLM::select_core_link_pairs(
    samples = samples,
    theta = theta,
    core_ids = core_ids,
    new_ids = new_ids,
    existing_pairs = existing,
    round_size = 1,
    core_audit_frac = 0,
    within_batch_frac = 1,
    min_judgments = 2,
    forbid_repeats = TRUE,
    balance_positions = TRUE,
    seed = 1
  )

  testthat::expect_equal(nrow(pairs), 1L)
  testthat::expect_equal(pairs$pair_type, "new_new")
  testthat::expect_true(all(c(pairs$ID1, pairs$ID2) %in% new_ids))
})

testthat::test_that("seed restoration: preserves existing .Random.seed and removes it if previously uninitialized", {
  samples <- tibble::tibble(ID = paste0("S", 1:8), text = paste("t", 1:8))
  theta <- tibble::tibble(ID = samples$ID, theta = rnorm(8), se = runif(8, 0.2, 0.6))
  core_ids <- paste0("S", 1:3)
  new_ids <- paste0("S", 4:8)

  # Case 1: .Random.seed absent -> should be absent after call
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  if (had) rm(".Random.seed", envir = .GlobalEnv)

  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  pairwiseLLM::select_core_link_pairs(
    samples = samples, theta = theta, core_ids = core_ids, new_ids = new_ids,
    round_size = 2, seed = 1, core_audit_frac = 0, within_batch_frac = 0
  )
  testthat::expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  # Restore original if needed
  if (had) assign(".Random.seed", old, envir = .GlobalEnv)

  # Case 2: .Random.seed present -> should be identical after call
  set.seed(999)
  old2 <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  pairwiseLLM::select_core_link_pairs(
    samples = samples, theta = theta, core_ids = core_ids, new_ids = new_ids,
    round_size = 2, seed = 1, core_audit_frac = 0, within_batch_frac = 0
  )
  new2 <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  testthat::expect_identical(old2, new2)
})

testthat::test_that("bt_core_link_round validates fit and returns a plan", {
  samples <- tibble::tibble(ID = paste0("S", 1:10), text = paste("t", 1:10))
  theta <- tibble::tibble(ID = samples$ID, theta = rnorm(10), se = runif(10, 0.2, 0.6))
  fit <- list(theta = theta)

  testthat::expect_error(
    pairwiseLLM::bt_core_link_round(samples, list(), core_ids = paste0("S", 1:3), round_size = 2),
    "containing `\\$theta`"
  )

  out <- pairwiseLLM::bt_core_link_round(
    samples = samples,
    fit = fit,
    core_ids = paste0("S", 1:3),
    round_size = 6,
    core_audit_frac = 0.2,
    within_batch_frac = 0.3,
    seed = 1
  )

  testthat::expect_true(is.list(out))
  testthat::expect_true(all(c("pairs", "plan") %in% names(out)))
  testthat::expect_s3_class(out$pairs, "tbl_df")
  testthat::expect_s3_class(out$plan, "tbl_df")
  testthat::expect_equal(nrow(out$plan), 1L)
  testthat::expect_equal(out$plan$n_total, nrow(out$pairs))
  testthat::expect_equal(
    out$plan$n_core_new + out$plan$n_new_new + out$plan$n_core_core,
    out$plan$n_total
  )
})


testthat::test_that("bt_core_link_round can optionally include text1/text2 columns", {
  samples <- tibble::tibble(ID = paste0("S", 1:6), text = paste("t", 1:6))
  theta <- tibble::tibble(ID = samples$ID, theta = seq(-1, 1, length.out = 6), se = rep(0.4, 6))
  fit <- list(theta = theta)

  out_no <- bt_core_link_round(samples, fit, core_ids = paste0("S", 1:2), round_size = 4, seed = 1)
  testthat::expect_true(all(c("ID1", "ID2", "pair_type") %in% names(out_no$pairs)))
  testthat::expect_false(any(c("text1", "text2") %in% names(out_no$pairs)))

  out_yes <- bt_core_link_round(samples, fit, core_ids = paste0("S", 1:2), round_size = 4, seed = 1, include_text = TRUE)
  testthat::expect_true(all(c("ID1", "text1", "ID2", "text2", "pair_type") %in% names(out_yes$pairs)))
  # spot-check mapping consistency
  map <- stats::setNames(samples$text, samples$ID)
  testthat::expect_identical(out_yes$pairs$text1, unname(map[out_yes$pairs$ID1]))
  testthat::expect_identical(out_yes$pairs$text2, unname(map[out_yes$pairs$ID2]))
})
