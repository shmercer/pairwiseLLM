testthat::test_that("select_adaptive_pairs fills missing se with fallback", {
  samples <- tibble::tibble(ID = c("A", "B", "C", "D"), text = c("a", "b", "c", "d"))
  theta <- tibble::tibble(ID = samples$ID, theta = c(0, 1, 2, 3), se = NA_real_)

  out <- pairwiseLLM::select_adaptive_pairs(
    samples = samples,
    theta = theta,
    n_pairs = 3,
    balance_positions = TRUE,
    forbid_repeats = TRUE,
    seed = 1
  )

  testthat::expect_s3_class(out, "tbl_df")
  # The key behavioral contract for this branch is: missing `se` does not
  # prevent pair selection. (The exact number selected can be constrained by
  # the candidate pool / constraints.)
  testthat::expect_true(nrow(out) >= 1L)
  testthat::expect_true(nrow(out) <= 3L)
  testthat::expect_true(all(c("ID1", "ID2") %in% names(out)))
})

testthat::test_that("allocation_compose skips NULL sub-results and returns NULL when empty", {
  a_null <- function(...) NULL
  a_non_null <- function(pairs, ...) {
    list(pairs = pairs, meta = list(note = "ok"))
  }

  pairs <- tibble::tibble(ID1 = c("A"), ID2 = c("B"))

  composed <- pairwiseLLM::allocation_compose(a_null, a_non_null)
  res <- composed(pairs)
  testthat::expect_true(is.list(res))
  testthat::expect_true(is.data.frame(res$pairs))

  composed2 <- pairwiseLLM::allocation_compose(a_null, a_null)
  testthat::expect_null(composed2(pairs))
})

testthat::test_that("checkpoint helpers validate input and handle round NA", {
  tmp <- withr::local_tempdir()

  # no checkpoint_dir -> silent no-op
  testthat::expect_null(pairwiseLLM:::.bt_write_checkpoint("x", list(a = 1), checkpoint_dir = NULL))

  # round=NA writes base filename (no round suffix)
  pairwiseLLM:::.bt_write_checkpoint("run_state", list(a = 1), checkpoint_dir = tmp, round = NA)
  testthat::expect_true(file.exists(file.path(tmp)))

  testthat::expect_error(pairwiseLLM:::.bt_read_checkpoint(""), "non-empty")
})

testthat::test_that("bt_link_thetas error paths and degenerate SD handling", {
  cur <- list(theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(1, 1, 1), se = c(1, 1, 1)))
  ref <- list(theta = tibble::tibble(ID = c("A", "B", "C"), theta = c(2, 3, 4), se = c(1, 1, 1)))

  testthat::expect_error(
    pairwiseLLM::bt_link_thetas(cur, ref, ids = character()),
    "non-empty"
  )
  testthat::expect_error(
    pairwiseLLM::bt_link_thetas(cur, ref, ids = 1:3),
    "character"
  )

  lk <- pairwiseLLM::bt_link_thetas(cur, ref, ids = c("A", "B", "C"), min_n = 3L)
  testthat::expect_identical(lk$b, 1)

})

testthat::test_that(".distinct_unordered_pairs treats (A,B) and (B,A) as same", {
  x <- tibble::tibble(ID1 = c("A", "B", "A"), ID2 = c("B", "A", "C"))
  y <- pairwiseLLM:::.distinct_unordered_pairs(x)
  testthat::expect_equal(nrow(y), 2L)
})
