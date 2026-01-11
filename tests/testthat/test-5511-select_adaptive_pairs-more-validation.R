.make_samples_5511 <- function(ids) {
  tibble::tibble(
    ID = as.character(ids),
    text = paste0("t", seq_along(ids))
  )
}

.make_theta_5511 <- function(ids) {
  tibble::tibble(
    ID = as.character(ids),
    theta = seq_along(ids),
    se = rep(1, length(ids))
  )
}

test_that("select_adaptive_pairs validates legacy forbid_repeats", {
  samples <- .make_samples_5511(c("A", "B"))
  theta <- .make_theta_5511(c("A", "B"))

  expect_error(
    select_adaptive_pairs(
      samples = samples,
      theta = theta,
      existing_pairs = NULL,
      n_pairs = 1,
      forbid_repeats = "no"
    ),
    "forbid_repeats"
  )
})

test_that("select_adaptive_pairs accepts repeat_policy = allow alias", {
  samples <- .make_samples_5511(c("A", "B", "C"))
  theta <- .make_theta_5511(c("A", "B", "C"))

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = NULL,
    n_pairs = 1,
    repeat_policy = "allow",
    seed = 1
  )

  diag <- attr(res, "pairing_diagnostics")
  expect_identical(diag$repeat_policy[[1]], "none")
  expect_equal(nrow(res), 1L)
})

test_that("select_adaptive_pairs validates repeat params", {
  samples <- .make_samples_5511(c("A", "B", "C"))
  theta <- .make_theta_5511(c("A", "B", "C"))

  expect_error(
    select_adaptive_pairs(samples, theta, existing_pairs = NULL, n_pairs = 1, repeat_cap = -1),
    "repeat_cap"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, existing_pairs = NULL, n_pairs = 1, repeat_frac = 2),
    "repeat_frac"
  )

  expect_error(
    select_adaptive_pairs(samples, theta, existing_pairs = NULL, n_pairs = 1, repeat_n = c(1, 2)),
    "repeat_n"
  )

  expect_error(
    select_adaptive_pairs(
      samples,
      theta,
      existing_pairs = NULL,
      n_pairs = 1,
      repeat_guard_min_degree = NA
    ),
    "repeat_guard_min_degree"
  )

  expect_error(
    select_adaptive_pairs(
      samples,
      theta,
      existing_pairs = NULL,
      n_pairs = 1,
      repeat_guard_largest_component_frac = 2
    ),
    "repeat_guard_largest_component_frac"
  )
})

test_that("repeat_n is coerced to integer and can override repeat quota", {
  ids <- c("A", "B", "C")
  samples <- .make_samples_5511(ids)
  theta <- .make_theta_5511(ids)

  # Keep the graph connected so this test isolates repeat_n behavior.
  existing <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("B", "C"),
    better_id = c("A", "B")
  )

  res <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 1,
    repeat_policy = "reverse_only",
    repeat_frac = 1,
    repeat_n = 1.9,
    repeat_guard_min_degree = 0,
    repeat_guard_largest_component_frac = 0,
    seed = 1
  )

  diag <- attr(res, "pairing_diagnostics")
  expect_true(is.integer(diag$repeat_n))
  expect_identical(diag$repeat_n[[1]], 1L)
  expect_identical(diag$n_repeat_planned[[1]], 1L)

  planned <- attr(res, "planned_repeat_pairs")
  expect_equal(nrow(planned), 1L)
  expect_identical(planned$ID1[[1]], "B")
  expect_identical(planned$ID2[[1]], "A")
})
