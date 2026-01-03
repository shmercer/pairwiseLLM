.make_samples <- function(ids) {
  tibble::tibble(
    ID = as.character(ids),
    text = paste0("text_", as.character(ids))
  )
}

.make_theta <- function(ids) {
  tibble::tibble(
    ID = as.character(ids),
    theta = rep(0, length(ids)),
    se = rep(1, length(ids))
  )
}

test_that("repeat guard blocks early reverse repeats", {
  ids <- c("A", "B", "C")
  samples <- .make_samples(ids)
  theta <- .make_theta(ids)

  # One completed judgment leaves node C with degree 0 => guard should fail
  existing <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 1,
    repeat_policy = "reverse_only",
    repeat_frac = 1,
    repeat_guard_min_degree = 1,
    repeat_guard_largest_component_frac = 0.9,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_false(diag$repeat_guard_passed)
  expect_equal(diag$n_repeat_planned, 0)

  # Guard should block repeats entirely (unordered A-B should not appear).
  keys <- paste0(pmin(pairs$ID1, pairs$ID2), "-", pmax(pairs$ID1, pairs$ID2))
  expect_false("A-B" %in% keys)
})

test_that("reverse_only plans the opposite direction for eligible pairs", {
  ids <- c("A", "B")
  samples <- .make_samples(ids)
  theta <- .make_theta(ids)

  existing <- tibble::tibble(ID1 = "A", ID2 = "B", better_id = "A")

  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 1,
    repeat_policy = "reverse_only",
    repeat_frac = 1,
    repeat_guard_min_degree = 1,
    repeat_guard_largest_component_frac = 0.5,
    balance_positions = TRUE,
    seed = 1
  )

  expect_equal(pairs$ID1[[1]], "B")
  expect_equal(pairs$ID2[[1]], "A")

  planned <- attr(pairs, "planned_repeat_pairs")
  expect_true(is.data.frame(planned))
  expect_equal(nrow(planned), 1)
  expect_equal(planned$ID1[[1]], "B")
  expect_equal(planned$ID2[[1]], "A")
})

test_that("repeat_cap prevents a third judgment", {
  ids <- c("A", "B")
  samples <- .make_samples(ids)
  theta <- .make_theta(ids)

  # Two completed judgments already
  existing <- tibble::tibble(
    ID1 = c("A", "B"),
    ID2 = c("B", "A"),
    better_id = c("A", "A")
  )

  pairs <- select_adaptive_pairs(
    samples = samples,
    theta = theta,
    existing_pairs = existing,
    n_pairs = 1,
    repeat_policy = "reverse_only",
    repeat_cap = 1,
    repeat_frac = 1,
    repeat_guard_min_degree = 1,
    repeat_guard_largest_component_frac = 0.5,
    seed = 1
  )

  diag <- attr(pairs, "pairing_diagnostics")
  expect_equal(diag$n_repeat_eligible, 0)
  expect_equal(diag$n_repeat_planned, 0)
})
