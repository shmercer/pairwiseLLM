# Full-rank, correlated synthetic draws; no provider or study artifacts.
link_e2_args <- function(edges = 8L) {
  args <- link_contract_args("gaussian_posterior_bridge", edges)
  H <- stats::contr.helmert(3)
  H <- sweep(H, 2, sqrt(colSums(H^2)), "/")
  u <- rbind(c(-2, -1), c(2, 1), c(-1, -2), c(1, 2), c(-1, 1), c(1, -1))
  hub <- sweep(u, 2, c(-.5, .7), "+") %*% t(H)
  spoke <- sweep(u %*% matrix(c(.6, -.4, .1, .8), 2), 2, c(.4, -.2), "+") %*% t(H)
  colnames(hub) <- colnames(spoke) <- c("a", "b", "c")
  args$hub$items <- args$spoke$items <- data.frame(item_id = c("b", "c", "a"))
  args$phase_a <- list(hub = list(draws = hub), spoke = list(draws = spoke))
  args$cross$A_set <- rep(c("H", "S"), length.out = edges)
  args$cross$B_set <- as.character(ifelse(args$cross$A_set == "H", "S", "H"))
  args$cross$A_item <- rep(c("a", "b", "c"), length.out = edges)
  args$cross$B_item <- rep(c("c", "a", "b", "a"), length.out = edges)
  args$cross$y_A <- rep(c(0L, 1L, 1L), length.out = edges)
  args
}

link_e2_input <- function(edges = 8L) do.call(prepare_link_input, link_e2_args(edges))

link_e2_artifact <- function(identity, draws) {
  list(set_id = identity$set_id, n_items = ncol(draws), n_pairs_committed = 19L,
    fit_model_id = "btl_e_b", phase_scope = "phase_a_set", phase_scope_set_id = identity$set_id,
    items = data.frame(item_id = colnames(draws)), posterior_draws = draws,
    phase_a_within_set_evidence_hash = "synthetic-within-hash",
    phase_a_within_set_evidence = data.frame(y_A = c(1L, 0L)))
}

# Independent free-coordinate density, using ordinary Gaussian and Bernoulli
# calculations; no production kernel, derivative, or stabilization helpers.
link_e2_oracle <- function(input) {
  hu <- input$phase_a$hub$value %*% input$basis$hub$H
  su <- input$phase_a$spoke$value %*% input$basis$spoke$H
  mu <- c(input$control$delta_prior$mean, colMeans(hu), colMeans(su))
  V <- matrix(0, length(mu), length(mu))
  V[1, 1] <- input$control$delta_prior$sd^2
  h <- 1 + seq_len(ncol(hu))
  s <- 1 + ncol(hu) + seq_len(ncol(su))
  V[h, h] <- cov(hu)
  V[s, s] <- cov(su)
  precision <- solve(V)
  objective <- function(q) {
    theta <- as.double(input$item_transform %*% q)
    names(theta) <- paste(c(rep(input$hub$set_id, nrow(input$hub$items)),
      rep(input$spoke$set_id, nrow(input$spoke$items))),
      c(input$hub$items$item_id, input$spoke$items$item_id), sep = ":")
    x <- input$cross
    eta <- theta[paste(x$A_set, x$A_item, sep = ":")] -
      theta[paste(x$B_set, x$B_item, sep = ":")] + input$judge$beta
    p <- (1 - input$judge$epsilon) * plogis(eta) + input$judge$epsilon / 2
    as.double(t(q - mu) %*% precision %*% (q - mu) / 2) +
      as.double(determinant(V, logarithm = TRUE)$modulus) / 2 + length(q) * log(2 * pi) / 2 -
      sum(dbinom(x$y_A, 1, p, log = TRUE))
  }
  fit <- stats::nlminb(mu, objective, control = list(rel.tol = 1e-13, x.tol = 1e-11))
  list(objective = objective, mode = fit$par, covariance = solve(stats::optimHess(fit$par, objective)),
    prior_mean = mu, prior_covariance = V)
}
