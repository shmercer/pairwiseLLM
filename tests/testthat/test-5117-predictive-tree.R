tree_fixture_5117 <- function(ids = letters[1:8], equal = FALSE) {
  pairs <- t(utils::combn(ids, 2L))
  reverse <- seq_len(nrow(pairs)) %% 3L == 0L
  edges <- tibble::tibble(A_id = ifelse(reverse, pairs[, 2L], pairs[, 1L]),
    B_id = ifelse(reverse, pairs[, 1L], pairs[, 2L]))
  n <- length(ids)
  list(item_ids = ids, edges = edges, initial_prediction = list(item_id = ids,
    mu = if (equal) rep(0, n) else seq(-2, 3, length.out = n),
    sigma = if (equal) rep(1, n) else seq(0.5, 1.5, length.out = n), beta = 1), seed = 315L)
}

tree_build_5117 <- function(f) do.call(pairwiseLLM:::.adaptive_predictive_tree, f)

# Independent reachability and scalar-probability oracle for small graphs.
# At every accepted edge, check all remaining choices rather than reproducing
# the builder's sorted passes or union-find implementation.
expect_tree_trace_5117 <- function(tree, f) {
  ids <- sort(enc2utf8(f$item_ids), method = "radix")
  n <- length(ids)
  a <- match(f$edges$A_id, ids)
  b <- match(f$edges$B_id, ids)
  x <- f$initial_prediction
  ts <- pairwiseLLM:::new_trueskill_state(tibble::tibble(item_id = x$item_id,
    mu = x$mu, sigma = x$sigma), beta = x$beta)
  p <- vapply(seq_along(a), function(k) {
    pairwiseLLM:::trueskill_win_probability(
      ids[[min(a[[k]], b[[k]])]], ids[[max(a[[k]], b[[k]])]], ts)
  }, numeric(1))
  score <- pmin(abs(p - 1 / 3), abs(p - 2 / 3))
  reach <- diag(TRUE, n)
  degree <- integer(n)
  cap <- 2
  diagnostics <- attr(tree, "tree_diagnostics")
  events <- diagnostics$relaxations
  available <- function() !reach[cbind(a, b)] & degree[a] < cap & degree[b] < cap
  expect_equal(nrow(tree), n - 1L)
  expect_identical(names(tree), c("i_id", "j_id"))
  for (step in seq_len(nrow(tree))) {
    event <- which(events$selected_edges == step - 1L)
    for (k in event) {
      expect_false(any(available()))
      expect_equal(events$old_cap[[k]], cap)
      expect_equal(events$new_cap[[k]], 2 * cap)
      expect_equal(events$components[[k]], n - step + 1L)
      cap <- events$new_cap[[k]]
    }
    row <- which(f$edges$A_id == tree$i_id[[step]] & f$edges$B_id == tree$j_id[[step]])
    expect_length(row, 1L)
    expect_true(available()[[row]])
    expect_equal(score[[row]], min(score[available()]), tolerance = 0)
    i <- a[[row]]
    j <- b[[row]]
    connected <- reach[i, ] | reach[j, ]
    reach[connected, connected] <- TRUE
    degree[c(i, j)] <- degree[c(i, j)] + 1L
  }
  expect_true(all(reach))
  expect_identical(diagnostics$degrees, tibble::tibble(item_id = ids, degree = degree))
  expect_equal(sum(diagnostics$degree_histogram$n_items), n)
  expect_equal(sum(with(diagnostics$degree_histogram, degree * n_items)), 2 * (n - 1))
  ops <- diagnostics$operations
  expect_equal(ops$probability_evaluations, nrow(f$edges))
  expect_lte(ops$component_checks, nrow(f$edges))
  expect_lte(ops$passes, max(1, ceiling(log2(n - 1))))
  expect_lte(ops$candidate_visits, nrow(f$edges) * ops$passes)
}

test_that("predictive trees obey priorities, connectivity, orientation, and exposure", {
  for (seed in c(0L, 1L, 315L, -17L)) {
    f <- tree_fixture_5117()
    f$seed <- seed
    before <- serialize(f, NULL)
    tree <- tree_build_5117(f)
    expect_s3_class(tree, "tbl_df")
    expect_identical(serialize(f, NULL), before)
    expect_tree_trace_5117(tree, f)
    # A complete graph always permits these disjoint paths to join at cap two.
    expect_equal(max(attr(tree, "tree_diagnostics")$degrees$degree), 2L)
    expect_equal(nrow(attr(tree, "tree_diagnostics")$relaxations), 0L)
    expect_identical(tree_build_5117(f), tree)
  }
  f <- tree_fixture_5117(c("b", "a"))
  expect_tree_trace_5117(tree_build_5117(f), f)
})

test_that("sparse hubs terminate with preserved edges and auditable cap doubling", {
  f <- tree_fixture_5117(sprintf("i%02d", 1:18), equal = TRUE)
  f$edges <- f$edges[f$edges$A_id == "i01" | f$edges$B_id == "i01", ]
  tree <- tree_build_5117(f)
  expect_tree_trace_5117(tree, f)
  d <- attr(tree, "tree_diagnostics")
  expect_identical(d$relaxations$old_cap, c(2, 4, 8, 16))
  expect_identical(d$relaxations$new_cap, c(4, 8, 16, 32))
  expect_identical(d$relaxations$selected_edges, c(2L, 4L, 8L, 16L))
  expect_identical(d$degrees$degree, c(17L, rep(1L, 17)))
  # Adding leaf-to-leaf alternatives eliminates this otherwise unavoidable hub.
  dense <- tree_fixture_5117(f$item_ids, equal = TRUE)
  balanced <- tree_build_5117(dense)
  expect_tree_trace_5117(balanced, dense)
  expect_equal(max(attr(balanced, "tree_diagnostics")$degrees$degree), 2)
})

test_that("sparse paths, cycles, bridges and deferred cycles satisfy the oracle", {
  withr::local_seed(5117)
  f <- tree_fixture_5117()
  # A path guarantees connectedness, with random deterministic sparse additions.
  a <- match(f$edges$A_id, f$item_ids)
  b <- match(f$edges$B_id, f$item_ids)
  path <- abs(a - b) == 1L
  for (iteration in seq_len(16L)) {
    sparse <- f
    sparse$edges <- f$edges[path | stats::runif(nrow(f$edges)) < 0.2, ]
    sparse$seed <- iteration
    expect_tree_trace_5117(tree_build_5117(sparse), sparse)
  }
  f$edges <- f$edges[path, ]
  expect_tree_trace_5117(tree_build_5117(f), f)
})

test_that("frozen probabilities share the scalar contract and target thirds", {
  x <- list(item_id = letters[1:4], mu = c(0, -2 * stats::qnorm(1 / 3),
    -2 * stats::qnorm(2 / 3), 0), sigma = rep(1, 4), beta = 1)
  state <- pairwiseLLM:::new_trueskill_state(tibble::tibble(
    item_id = x$item_id, mu = x$mu, sigma = x$sigma), beta = x$beta)
  p <- pairwiseLLM:::.trueskill_win_probability_values(x$mu[1], x$mu[-1],
    x$sigma[1], x$sigma[-1], x$beta, check_finite = TRUE)
  expect_equal(p, c(1 / 3, 2 / 3, 1 / 2), tolerance = 1e-15)
  scalar <- vapply(x$item_id[-1], function(j) {
    pairwiseLLM:::trueskill_win_probability("a", j, state)
  }, numeric(1))
  expect_identical(unname(scalar), p)
  expect_identical(pairwiseLLM:::.trueskill_win_probability_vec(
    rep("a", 3), x$item_id[-1], state), p)
  f <- tree_fixture_5117(x$item_id)
  f$initial_prediction <- x
  tree <- tree_build_5117(f)
  expect_tree_trace_5117(tree, f)
  expect_true(setequal(c(tree$i_id[[1]], tree$j_id[[1]]), c("a", "b")) ||
    setequal(c(tree$i_id[[1]], tree$j_id[[1]]), c("a", "c")) ||
    setequal(c(tree$i_id[[1]], tree$j_id[[1]]), c("b", "d")) ||
    setequal(c(tree$i_id[[1]], tree$j_id[[1]]), c("c", "d")))
})

test_that("predictions affect the graph, and seeds only resolve exact ties", {
  f <- tree_fixture_5117()
  first <- tree_build_5117(f)
  # These edge scores are distinct: no tie rank can change the queue.
  for (seed in c(1L, 2L, 3L, 100L)) {
    f$seed <- seed
    actual <- tree_build_5117(f)
    expect_identical(actual$i_id, first$i_id)
    expect_identical(actual$j_id, first$j_id)
  }
  f$initial_prediction$mu <- c(0, 5, -1, 3, -4, 2, 7, -2)
  changed <- tree_build_5117(f)
  key <- function(x) sort(paste(pmin(x$i_id, x$j_id), pmax(x$i_id, x$j_id)))
  expect_false(identical(key(changed), key(first)))
  f <- tree_fixture_5117()
  f$initial_prediction$sigma <- c(5, 0.1, 2, 0.2, 7, 0.1, 4, 0.1)
  expect_false(identical(key(tree_build_5117(f)), key(first)))
  f <- tree_fixture_5117(equal = TRUE)
  one <- tree_build_5117(f)
  f$seed <- f$seed + 1L
  two <- tree_build_5117(f)
  expect_false(identical(key(one), key(two)))
  expect_tree_trace_5117(one, f)
  expect_tree_trace_5117(two, f)
})

test_that("input order, locale, encodings and ambient RNG cannot change trees", {
  withr::local_seed(44)
  f <- tree_fixture_5117(c("a:b", "c", "a", "b:c", "\u00e9", "Z", "\u00e4"), equal = TRUE)
  expected <- tree_build_5117(f)
  f$item_ids <- rev(f$item_ids)
  f$edges <- f$edges[sample.int(nrow(f$edges)), c("B_id", "A_id")]
  for (field in c("item_id", "mu", "sigma")) {
    f$initial_prediction[[field]] <- rev(f$initial_prediction[[field]])
  }
  withr::local_locale(c(LC_COLLATE = "C"))
  expect_identical(tree_build_5117(f), expected)
  alternative <- if (.Platform$OS.type == "windows") "English_United States.1252" else "en_US.UTF-8"
  available <- suppressWarnings(Sys.setlocale("LC_COLLATE", alternative))
  if (nzchar(available)) expect_identical(tree_build_5117(f), expected)
  # All inputs still name the same Unicode IDs with explicit Latin-1 encoding.
  f$item_ids <- iconv(f$item_ids, from = "UTF-8", to = "latin1")
  expect_identical(tree_build_5117(f), expected)
  for (kind in c("Mersenne-Twister", "L'Ecuyer-CMRG")) {
    withr::with_seed(771, {
      before <- .Random.seed
      kinds <- RNGkind()
      expect_identical(tree_build_5117(f), expected)
      expect_identical(.Random.seed, before)
      expect_identical(RNGkind(), kinds)
    }, .rng_kind = kind, .rng_normal_kind = "Box-Muller")
  }
})

test_that("fresh processes reproduce trees without creating a global RNG state", {
  root <- withr::local_tempdir()
  config <- file.path(root, "input.rds")
  result <- file.path(root, "tree.rds")
  f <- tree_fixture_5117(equal = TRUE)
  expected <- tree_build_5117(f)
  dev_path <- if (pkgload::is_dev_package("pairwiseLLM")) getNamespaceInfo("pairwiseLLM", "path") else NULL
  saveRDS(list(fixture = f, libpaths = .libPaths(), dev_path = dev_path, result = result), config)
  code <- paste(
    "cfg <- readRDS(commandArgs(TRUE)[[1]])", ".libPaths(cfg$libpaths)",
    "if (!is.null(cfg$dev_path)) pkgload::load_all(cfg$dev_path, quiet=TRUE) else library(pairwiseLLM)",
    "stopifnot(!exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE))",
    "out <- do.call(pairwiseLLM:::.adaptive_predictive_tree, cfg$fixture)",
    "stopifnot(!exists('.Random.seed', envir=.GlobalEnv, inherits=FALSE))",
    "saveRDS(out, cfg$result)", sep = "\n")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", "-e", shQuote(code), shQuote(config)), stdout = TRUE, stderr = TRUE))
  expect_true(is.null(attr(output, "status")), info = paste(output, collapse = "\n"))
  expect_true(file.exists(result))
  expect_identical(readRDS(result), expected)
})

test_that("only selectable manifest endpoints enter the outcome-blind builder", {
  f <- tree_fixture_5117()
  # The last two edges are held out; audit reversals are stored separately.
  primary <- f$edges[-c(27L, 28L), ]
  outcomes <- transform(primary, Y = rep(c(0L, 1L), length.out = nrow(primary)))
  first <- make_adaptive_replay_reservoir(outcomes, f$item_ids)
  second <- make_adaptive_replay_reservoir(transform(outcomes, Y = 1L - Y), f$item_ids)
  expect_false(identical(first$manifest$digest, second$manifest$digest))
  testthat::local_mocked_bindings(
    .adaptive_reservoir_validate = function(...) stop("outcome access"),
    make_adaptive_replay_reservoir = function(...) stop("outcome access"),
    trueskill_win_probability = function(...) stop("repeated full-state validation"),
    .package = "pairwiseLLM")
  f$edges <- first$manifest$edges
  a <- tree_build_5117(f)
  f$edges <- second$manifest$edges
  expect_identical(tree_build_5117(f), a)
  expect_true(all(paste(a$i_id, a$j_id) %in% paste(primary$A_id, primary$B_id)))
  # Outcome-bearing tables and whole reservoirs are rejected at the boundary.
  f$edges <- outcomes
  expect_error(tree_build_5117(f), "exactly A_id and B_id")
  f$edges <- first
  expect_error(tree_build_5117(f), "exactly A_id and B_id")
})

test_that("disconnected and malformed inputs fail clearly", {
  f <- tree_fixture_5117()
  bad <- f
  for (seed in list(NULL, NA_real_, Inf, 1.5, 2^31, "1", c(1, 2), matrix(1))) {
    bad$seed <- seed
    expect_error(tree_build_5117(bad), "seed")
  }
  for (ids in list("a", c("a", "a"), c("a", NA), c("a", " "), 1:3, matrix(c("a", "b")))) {
    bad <- f
    bad$item_ids <- ids
    expect_error(tree_build_5117(bad), "item_ids")
  }
  for (policy in list(NULL, list(version = 2L), modifyList(
      pairwiseLLM:::.adaptive_predictive_tree_policy(), list(initial_degree_cap = 4L)))) {
    expect_error(do.call(pairwiseLLM:::.adaptive_predictive_tree, c(f, list(policy = policy))), "policy")
  }
  for (edges in list(f$edges[0, ], f$edges[1, ],
      f$edges[!f$edges$A_id %in% "h" & !f$edges$B_id %in% "h", ])) {
    bad <- f
    bad$edges <- edges
    expect_error(tree_build_5117(bad), "disconnected")
  }
  for (edges in list(transform(f$edges, A_id = "outside"), transform(f$edges, A_id = NA_character_),
      transform(f$edges, A_id = B_id), rbind(f$edges, f$edges[1, ]),
      rbind(f$edges, data.frame(A_id = f$edges$B_id[1], B_id = f$edges$A_id[1])))) {
    bad <- f
    bad$edges <- edges
    expect_error(tree_build_5117(bad), "endpoints|non-missing|Self-pairs|unordered edge")
  }
  for (prediction in list(NULL, list(), f$initial_prediction[-1])) {
    bad <- f
    bad$initial_prediction <- prediction
    expect_error(tree_build_5117(bad), "initial_prediction")
  }
  for (ids in list(rep("a", 8), letters[2:9], letters[1:7], c(letters[1:7], NA))) {
    bad <- f
    bad$initial_prediction$item_id <- ids
    expect_error(tree_build_5117(bad), "prediction.*ID|prediction.*item_id")
  }
  for (field in c("mu", "sigma", "beta")) {
    for (value in list(NULL, NA_real_, Inf, "1", matrix(rep(1, 8)), rep(1, 9))) {
      bad <- f
      bad$initial_prediction[field] <- list(value)
      expect_error(tree_build_5117(bad), "Invalid initial prediction")
    }
  }
  for (field in c("sigma", "beta")) {
    for (value in c(0, -1)) {
      bad <- f
      bad$initial_prediction[[field]][] <- value
      expect_error(tree_build_5117(bad), "Invalid initial prediction")
    }
  }
  # Finite inputs whose arithmetic overflows or underflows cannot define scores.
  for (field in c("mu", "sigma", "beta")) {
    bad <- f
    bad$initial_prediction[[field]][] <- if (field == "mu") rep(c(-1e308, 1e308), 4) else 1e308
    expect_error(tree_build_5117(bad), "intermediates")
  }
  bad <- f
  bad$initial_prediction$sigma[] <- 1e-300
  bad$initial_prediction$beta <- 1e-300
  expect_error(tree_build_5117(bad), "intermediates")
})

test_that("edge scoring occurs once and operation counts stay bounded at scale", {
  f <- tree_fixture_5117(sprintf("i%03d", seq_len(150)))
  kernel <- pairwiseLLM:::.trueskill_win_probability_values
  calls <- 0L
  lengths <- integer()
  testthat::local_mocked_bindings(.trueskill_win_probability_values = function(mu_i, ...) {
    calls <<- calls + 1L
    lengths <<- c(lengths, length(mu_i))
    kernel(mu_i, ...)
  }, .package = "pairwiseLLM")
  tree <- tree_build_5117(f)
  expect_identical(calls, 1L)
  expect_identical(lengths, nrow(f$edges))
  ops <- attr(tree, "tree_diagnostics")$operations
  expect_lte(ops$candidate_visits, nrow(f$edges) * ceiling(log2(length(f$item_ids) - 1)))
  expect_lte(ops$component_checks, nrow(f$edges))
  expect_equal(nrow(tree), 149L)
})

test_that("all connected four-item permitted graphs complete", {
  f <- tree_fixture_5117(letters[1:4])
  for (mask in 0:63) {
    g <- f
    g$edges <- f$edges[as.logical(intToBits(mask)[1:6]), ]
    reach <- diag(TRUE, 4)
    for (k in seq_len(nrow(g$edges))) {
      i <- match(g$edges$A_id[[k]], g$item_ids)
      j <- match(g$edges$B_id[[k]], g$item_ids)
      connected <- reach[i, ] | reach[j, ]
      reach[connected, connected] <- TRUE
    }
    if (all(reach)) {
      expect_tree_trace_5117(tree_build_5117(g), g)
    } else {
      expect_error(tree_build_5117(g), "disconnected")
    }
  }
})
