test_that("fit_rank_centrality handles moderately large sparse graphs", {
  set.seed(123)

  n <- 800
  m <- 4000
  ids <- sprintf("S%04d", seq_len(n))

  i <- sample.int(n, m, replace = TRUE)
  j <- sample.int(n, m, replace = TRUE)
  ok <- i != j
  i <- i[ok]
  j <- j[ok]
  i <- i[seq_len(min(length(i), m))]
  j <- j[seq_len(min(length(j), m))]

  bt_data <- tibble::tibble(
    object1 = ids[i],
    object2 = ids[j],
    result  = rbinom(length(i), size = 1, prob = 0.5)
  )

  fit <- fit_rank_centrality(bt_data, ids = ids, damping = 0.05, max_iter = 2000, tol = 1e-8)

  expect_equal(fit$diagnostics$n_nodes, n)
  expect_true(all(is.finite(fit$theta$theta)))
  expect_true(all(is.finite(fit$theta$pi)))
  expect_equal(sum(fit$theta$pi), 1, tolerance = 1e-8)

  fit2 <- fit_rank_centrality(bt_data, ids = ids, damping = 0.05, return_transition = TRUE)
  expect_true(inherits(fit2$P, "dgCMatrix"))
})
