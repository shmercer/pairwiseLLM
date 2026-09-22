# An independent dense-grid oracle: direct Bernoulli probabilities, trapezoids,
# and interpolated CDF quantiles. No production likelihood/quadrature helpers.
link_e1_grid <- function(input, step = .002, limits = c(-45, 45), pairs = NULL) {
  delta <- seq(limits[1], limits[2], by = step)
  log_density <- dnorm(delta, input$control$delta_prior$mean,
    input$control$delta_prior$sd, log = TRUE)
  probability <- function(row) {
    a <- if (row$A_set == input$hub$set_id) {
      input$phase_a$hub$value[row$A_item]
    } else {
      input$phase_a$spoke$value[row$A_item] + delta
    }
    b <- if (row$B_set == input$hub$set_id) {
      input$phase_a$hub$value[row$B_item]
    } else {
      input$phase_a$spoke$value[row$B_item] + delta
    }
    (1 - input$judge$epsilon) * plogis(a - b + input$judge$beta) + input$judge$epsilon / 2
  }
  # Multiplicity is explicit, allowing large repeated-judgment oracle cases.
  rows <- input$cross[, -1L, drop = FALSE]
  unique_rows <- unique(rows)
  for (i in seq_len(nrow(unique_rows))) {
    same <- Reduce(`&`, lapply(names(rows), function(k) rows[[k]] == unique_rows[[k]][i]))
    p <- probability(unique_rows[i, ])
    log_density <- log_density + sum(same) * if (unique_rows$y_A[i] == 1) log(p) else log1p(-p)
  }
  density <- exp(log_density - max(log_density))
  area <- function(values) sum((head(values, -1L) + tail(values, -1L)) * step / 2)
  normalizer <- area(density)
  density <- density / normalizer
  mean <- area(delta * density)
  sd <- sqrt(area((delta - mean)^2 * density))
  cumulative <- c(0, cumsum((head(density, -1L) + tail(density, -1L)) * step / 2))
  list(mean = mean, sd = sd, interval = approx(cumulative, delta, c(.025, .975), ties = "ordered")$y,
    log_normalizer = max(log_density) + log(normalizer),
    prediction = if (is.null(pairs)) NULL else vapply(seq_len(nrow(pairs)),
      function(i) area(probability(pairs[i, ]) * density), numeric(1)))
}

link_e1_mixed_args <- function() {
  args <- link_contract_args(edges = 8L)
  args$cross$A_set <- c("H", "S", "H", "S", "H", "S", "H", "S")
  args$cross$B_set <- ifelse(args$cross$A_set == "H", "S", "H")
  args$cross$A_item <- c("a", "b", "b", "a", "a", "b", "a", "b")
  args$cross$B_item <- c("a", "a", "a", "b", "b", "b", "a", "b")
  args$cross$y_A <- c(0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L)
  args
}

link_e1_artifact <- function(identity, points) {
  list(set_id = identity$set_id, n_items = nrow(identity$items), n_pairs_committed = 12L,
    fit_model_id = "btl_e_b", phase_scope = "phase_a_set", phase_scope_set_id = identity$set_id,
    items = data.frame(item_id = names(points), theta_raw_mean = unname(points),
      theta_raw_sd = c(10, 20), rank_mu_raw = rank(-points)),
    phase_a_within_set_evidence_hash = "synthetic-phase-a-evidence",
    phase_a_within_set_evidence = data.frame(y_A = c(0L, 1L)),
    posterior_draws = matrix(1:4, 2L))
}
