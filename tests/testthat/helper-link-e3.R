link_e3_args <- function(edges = 12L, symmetric = FALSE) {
  args <- link_contract_args("joint_offset", 0L)
  args$hub$items <- args$spoke$items <- data.frame(item_id = c("c", "a", "b"))
  evidence <- function(a, b, n, cross = FALSE) {
    pairs <- if (cross) expand.grid(A = c("a", "b", "c"), B = c("a", "b", "c")) else
      data.frame(A = c("a", "a", "b"), B = c("b", "c", "c"))
    i <- rep(seq_len(nrow(pairs)), each = n)
    data.frame(observation_id = paste0(a, b, "-", seq_along(i)),
      A_set = a, A_item = as.character(pairs$A[i]), B_set = b,
      B_item = as.character(pairs$B[i]), y_A = rep(rep(c(0L, 1L), length.out = n), nrow(pairs)))
  }
  args$phase_a <- list(hub = list(observations = evidence("H", "H", if (symmetric) 40L else 6L)),
    spoke = list(observations = evidence("S", "S", if (symmetric) 40L else 4L)))
  args$cross <- evidence("H", "S", if (symmetric) 12L else 2L, TRUE)
  if (!symmetric) {
    args$phase_a$hub$observations$y_A[1:4] <- 1L
    args$phase_a$spoke$observations$y_A[1:3] <- 0L
    args$cross$y_A[c(1, 3, 5)] <- 0L
    args$cross <- args$cross[seq_len(edges), ]
  } else if (edges == 0L) args$cross <- args$cross[FALSE, ]
  args$judge$beta <- if (symmetric) 0 else .17
  args$judge$epsilon <- if (symmetric) .03 else .12
  args
}

link_e3_input <- function(edges = 12L, symmetric = FALSE) {
  do.call(prepare_link_input, link_e3_args(edges, symmetric))
}

# Independent item-scale likelihood, not the implementation's design matrix.
link_e3_objective <- function(input) {
  rows <- rbind(input$phase_a$hub$value, input$phase_a$spoke$value, input$cross)
  function(q) {
    nh <- nrow(input$hub$items)
    ns <- nrow(input$spoke$items)
    h <- as.double(input$basis$hub$H %*% q[1L + seq_len(nh - 1L)])
    s <- q[1] + as.double(input$basis$spoke$H %*% q[nh + seq_len(ns - 1L)])
    endpoint <- function(set, item) ifelse(set == input$hub$set_id,
      h[match(item, input$hub$items$item_id)], s[match(item, input$spoke$items$item_id)])
    eta <- endpoint(rows$A_set, rows$A_item) - endpoint(rows$B_set, rows$B_item) + input$judge$beta
    p <- (1 - input$judge$epsilon) * plogis(eta) + input$judge$epsilon / 2
    -sum(dbinom(rows$y_A, 1, p, log = TRUE)) -
      dnorm(q[1], input$control$delta_prior$mean, input$control$delta_prior$sd, log = TRUE) -
      sum(dnorm(q[-1], log = TRUE))
  }
}

link_e3_mock_sampler <- function(input, iterations = 1200L, chains = 4L, seed = 278L) {
  withr::local_seed(seed)
  d <- ncol(input$item_transform)
  a <- array(rnorm(iterations * chains * d), c(iterations, chains, d))
  a[, , 1] <- a[, , 1] * input$control$delta_prior$sd + input$control$delta_prior$mean
  vars <- c("delta", if (nrow(input$hub$items) > 1) paste0("u_H[", seq_len(nrow(input$hub$items) - 1L), "]"),
    if (nrow(input$spoke$items) > 1) paste0("u_S[", seq_len(nrow(input$spoke$items) - 1L), "]"))
  dimnames(a) <- list(NULL, as.character(seq_len(chains)), vars)
  list(fit = list(draws = function(...) a, diagnostic_summary = function(...) {
    list(num_divergent = rep(0L, chains), num_max_treedepth = rep(0L, chains), ebfmi = rep(1, chains))
  }), config = list(chains = chains, iter_sampling = iterations, max_treedepth = 12L), model_hash = "synthetic")
}
