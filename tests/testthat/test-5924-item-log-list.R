testthat::test_that("item_log_list appends per refit when outputs disabled", {
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(write_outputs = FALSE)
  )

  fit <- pairwiseLLM:::make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )

  state <- pairwiseLLM:::.adaptive_append_item_log(state, fit = fit)
  state <- pairwiseLLM:::.adaptive_append_item_log(state, fit = fit)

  testthat::expect_true(is.list(state$logs$item_log_list))
  testthat::expect_equal(length(state$logs$item_log_list), 2L)
  testthat::expect_true(all(state$logs$item_log_list[[1L]]$refit_id == 1L))
  testthat::expect_true(all(state$logs$item_log_list[[2L]]$refit_id == 2L))
})

testthat::test_that("item logs write one file per refit when enabled", {
  output_dir <- withr::local_tempdir()
  samples <- tibble::tibble(
    ID = c("A", "B"),
    text = c("alpha", "bravo")
  )
  state <- pairwiseLLM:::adaptive_state_new(samples, config = list(d1 = 2L))
  state$config$v3 <- pairwiseLLM:::adaptive_v3_config(
    state$N,
    list(write_outputs = TRUE, output_dir = output_dir)
  )

  fit <- pairwiseLLM:::make_v3_fit_contract(
    state$ids,
    theta_draws = matrix(
      c(0.1, -0.1, 0.2, -0.2),
      nrow = 2L,
      byrow = TRUE,
      dimnames = list(NULL, state$ids)
    )
  )

  state <- pairwiseLLM:::.adaptive_append_item_log(state, fit = fit, output_dir = output_dir)
  state <- pairwiseLLM:::.adaptive_append_item_log(state, fit = fit, output_dir = output_dir)

  path_1 <- file.path(output_dir, "item_log_refit_0001.rds")
  path_2 <- file.path(output_dir, "item_log_refit_0002.rds")
  testthat::expect_true(file.exists(path_1))
  testthat::expect_true(file.exists(path_2))

  log_1 <- readRDS(path_1)
  log_2 <- readRDS(path_2)
  testthat::expect_true(all(log_1$refit_id == 1L))
  testthat::expect_true(all(log_2$refit_id == 2L))
})
