# Frozen production implementations, independently sourced into a private scope.
bt_reference_324 <- function(path = testthat::test_path("fixtures", "bt-refits-324")) {
  env <- new.env(parent = asNamespace("pairwiseLLM"))
  for (file in c("bt_common.R", "bt_alpha.R", "bt_firth.R", "bt_bootstrap.R",
                 "bt_bootstrap_contracts.R", "bt_bootstrap_schedule.R")) {
    sys.source(file.path(path, file), envir = env)
  }
  env$fit_bt_model <- pairwiseLLM::fit_bt_model
  environment(env$fit_bt_model) <- env
  env
}

# Connected fixed graph, generated before outcomes. Both orientations, repeated
# pairs and lexically nonnumeric labels deliberately exercise the index contract.
bt_fixture_324 <- function(n, graph = "sparse") {
  ids <- sprintf("item %03d", seq_len(n))
  pairs <- t(utils::combn(seq_len(n), 2L))
  if (graph != "complete") {
    pairs <- pairs[pairs[, 2L] - pairs[, 1L] <= 3L | pairs[, 1L] == 1L, , drop = FALSE]
  }
  rows <- rep(seq_len(nrow(pairs)), each = if (graph == "unbalanced") 5L else 4L)
  a <- pairs[rows, 1L]
  b <- pairs[rows, 2L]
  # Deterministic, heterogeneous counts; the first two repeats guarantee wins
  # both ways except in the explicitly separated fixture.
  y <- rep(c(1, 0, 1, 1), length.out = length(rows))
  if (graph == "unbalanced") y <- as.integer(seq_along(rows) %% 5L != 0L)
  if (graph == "separated") y[] <- 1L
  if (graph == "near_zero") y <- rep(c(0, 1), length.out = length(rows))
  if (graph == "near_separated") {
    y[] <- 1L
    y[seq(1L, length(y), by = 41L)] <- 0L
  }
  if (graph == "reversed") {
    tmp <- a
    a <- b
    b <- tmp
    y <- 1L - y
  }
  data.frame(object1 = ids[a], object2 = ids[b], result = as.double(y))
}
