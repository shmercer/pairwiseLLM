# Optional E3 audit/reference engine. No CmdStan dependency on the MAP path.
.link_e3_cmdstan_control <- function(x) {
  integer_fields <- c("chains", "parallel_chains", "iter_warmup", "iter_sampling",
    "max_treedepth", "threads_per_chain")
  .link_fields(x, c(integer_fields, "adapt_delta", "seed", "output_dir", "core_fraction"),
    label = "E3 cmdstan controls")
  for (k in intersect(names(x), integer_fields)) x[[k]] <- .btl_mcmc_positive_integer(x[[k]], k)
  for (k in intersect(names(x), c("adapt_delta", "core_fraction"))) {
    x[[k]] <- .link_scalar(x[[k]], paste("E3", k), 0, 1)
    .link_check(x[[k]] > 0 && (k != "adapt_delta" || x[[k]] < 1),
      paste0("Invalid E3 ", k, "."))
  }
  if (!is.null(x$seed)) {
    seed <- .link_scalar(x$seed, "E3 cmdstan seed", 0, .Machine$integer.max)
    .link_check(seed == floor(seed), "E3 cmdstan seed must be an integer.")
    x$seed <- as.integer(seed)
  }
  if (!is.null(x$output_dir)) {
    .link_check(is.character(x$output_dir) && length(x$output_dir) == 1L &&
      !is.na(x$output_dir) && nzchar(x$output_dir), "E3 output_dir must be a nonempty path.")
  }
  x
}

.link_e3_cmdstan_controls <- function(x) {
  utils::modifyList(list(chains = 4L, iter_warmup = 1000L, iter_sampling = 1000L,
    adapt_delta = .90, max_treedepth = 12L), .link_e3_cmdstan_control(x))
}

.link_e3_stan_data <- function(input) {
  rows <- .link_e3_evidence(input)
  nh <- nrow(input$hub$items)
  endpoint <- function(set, item) {
    as.integer(ifelse(set == input$hub$set_id, match(item, input$hub$items$item_id),
      nh + match(item, input$spoke$items$item_id)))
  }
  list(N_H = nh, N_S = nrow(input$spoke$items), M = nrow(rows),
    H_H = unname(input$basis$hub$H), H_S = unname(input$basis$spoke$H),
    A = endpoint(rows$A_set, rows$A_item), B = endpoint(rows$B_set, rows$B_item),
    Y = rows$y_A, beta = input$judge$beta, epsilon = input$judge$epsilon,
    delta_mean = input$control$delta_prior$mean, delta_sd = input$control$delta_prior$sd)
}

.link_e3_model_file <- function() {
  source <- system.file("stan", "link_joint_offset.stan", package = "pairwiseLLM")
  .link_check(nzchar(source), "E3 Stan model file was not found.")
  # CmdStanR reuses an up-to-date executable. Copy to a writable, version-bound
  # cache so compilation never modifies the installed package/source tree.
  key <- .link_hash(list(source = readLines(source, warn = FALSE),
    cmdstan = as.character(cmdstanr::cmdstan_version()),
    cmdstanr = as.character(utils::packageVersion("cmdstanr")), threads = TRUE))
  directory <- file.path(tools::R_user_dir("pairwiseLLM", "cache"), "stan", key)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(directory, "link_joint_offset.stan")
  if (!file.exists(path)) .link_check(file.copy(source, path), "Cannot populate the E3 Stan cache.")
  path
}

.link_e3_sample <- function(data, control, model_fn = cmdstanr::cmdstan_model) {
  config <- .link_e3_cmdstan_controls(control$cmdstan)
  resources <- .btl_mcmc_resolve_cmdstan_config(config)
  model <- model_fn(.link_e3_model_file(), cpp_options = list(stan_threads = TRUE))
  args <- config[intersect(names(config), c("iter_warmup", "iter_sampling", "adapt_delta",
    "max_treedepth", "seed", "output_dir"))]
  args <- c(list(data = data, chains = resources$chains, parallel_chains = resources$parallel_chains,
    threads_per_chain = resources$threads_per_chain, refresh = 0), args)
  .btl_mcmc_resource_message(resources)
  fit <- do.call(model$sample, args)
  list(fit = fit, config = utils::modifyList(config, resources),
    model_hash = .link_hash(readLines(model$stan_file(), warn = FALSE)))
}

.link_e3_free_draws <- function(fit, input) {
  nh <- nrow(input$hub$items) - 1L
  ns <- nrow(input$spoke$items) - 1L
  vars <- c("delta", if (nh) paste0("u_H[", seq_len(nh), "]"),
    if (ns) paste0("u_S[", seq_len(ns), "]"))
  raw <- fit$draws(variables = c("delta", if (nh) "u_H", if (ns) "u_S"), format = "draws_array")
  .link_check(is.numeric(raw) && length(dim(raw)) == 3L && dim(raw)[1] >= 2L &&
    dim(raw)[2] >= 1L && all(vars %in% dimnames(raw)[[3]]) && all(is.finite(raw)),
    "E3 sampler must return finite, chain-preserving draws for every free coordinate.")
  draws <- array(as.double(raw[, , vars, drop = FALSE]),
    dim = c(dim(raw)[1:2], length(vars)),
    dimnames = list(NULL, dimnames(raw)[[2]], colnames(input$item_transform)))
  draws
}

.link_e3_sampler_diagnostics <- function(fit, draws, config) {
  coords <- dimnames(draws)[[3]]
  summary <- tryCatch(posterior::summarise_draws(posterior::as_draws_array(draws),
    "mean", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean"), error = function(e) NULL)
  parameters <- list(coordinate = coords)
  for (k in c("mean", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean")) {
    parameters[[k]] <- if (!is.null(summary) && k %in% names(summary)) {
      as.double(summary[[k]][match(coords, summary$variable)])
    } else {
      rep(NA_real_, length(coords))
    }
    parameters[[k]][!is.finite(parameters[[k]])] <- NA_real_
  }
  parameters$mcse_sd_ratio <- parameters$mcse_mean / parameters$sd
  parameters$mcse_sd_ratio[!is.finite(parameters$mcse_sd_ratio)] <- NA_real_
  raw <- tryCatch(fit$diagnostic_summary(quiet = TRUE), error = function(e) list())
  chains <- dim(draws)[2]
  per_chain <- list(chain = seq_len(chains))
  for (k in c("num_divergent", "num_max_treedepth", "ebfmi")) {
    v <- raw[[k]]
    valid <- is.numeric(v) && length(v) == chains && all(is.finite(v)) && all(v >= 0)
    if (k != "ebfmi") valid <- valid && all(v == floor(v)) && all(v <= dim(draws)[1])
    per_chain[[k]] <- if (valid) as.double(v) else rep(NA_real_, chains)
  }
  per_chain$treedepth_fraction <- per_chain$num_max_treedepth / dim(draws)[1]
  complete_extreme <- function(x, fn) if (all(is.finite(x))) fn(x) else NA_real_
  out <- list(engine = "mcmc", parameters = parameters, per_chain = per_chain,
    divergences = as.integer(sum(per_chain$num_divergent)),
    max_rhat = complete_extreme(parameters$rhat, max),
    min_ess_bulk = complete_extreme(parameters$ess_bulk, min),
    min_ess_tail = complete_extreme(parameters$ess_tail, min),
    max_treedepth_hits = as.integer(sum(per_chain$num_max_treedepth)),
    max_treedepth_fraction = sum(per_chain$num_max_treedepth) / prod(dim(draws)[1:2]),
    config = config, iterations_per_chain = dim(draws)[1], n_chains = chains)
  gate <- c(divergences = isTRUE(out$divergences == 0L),
    rhat = isTRUE(out$max_rhat <= 1.01), bulk_ess = isTRUE(out$min_ess_bulk >= 1000),
    tail_ess = isTRUE(out$min_ess_tail >= 500),
    ebfmi = isTRUE(all(per_chain$ebfmi >= .30)),
    treedepth = isTRUE(out$max_treedepth_fraction <= .01),
    mcse = isTRUE(all(parameters$mcse_sd_ratio <= .05)))
  out$audit_gate <- list(protocol = "D005", thresholds = list(divergences = 0L,
    max_rhat = 1.01, min_ess_bulk = 1000, min_ess_tail = 500, min_ebfmi = .30,
    max_treedepth_fraction = .01, max_mcse_sd_ratio = .05),
    checks = gate, passed = all(gate), failed = names(gate)[!gate])
  out
}

.link_e3_prior_convolution_quantile <- function(shape, prior, p) {
  bounds <- range(shape) + stats::qnorm(p, prior$mean, prior$sd)
  if (bounds[1] == bounds[2]) return(bounds[1])
  stats::uniroot(function(q) mean(stats::pnorm(q - shape, prior$mean, prior$sd)) - p,
    bounds, tol = 1e-9)$root
}

.link_e3_draw_summary <- function(draws, input) {
  coords <- colnames(input$item_transform)
  matrix <- matrix(draws, ncol = length(coords), dimnames = list(NULL, coords))
  mean <- colMeans(matrix)
  V <- stats::cov(matrix)
  theta <- matrix %*% t(input$item_transform)
  intervals <- apply(theta, 2L, stats::quantile, probs = c(.025, .975), names = FALSE)
  delta_interval <- stats::quantile(matrix[, 1], c(.025, .975), names = FALSE)
  prior <- input$control$delta_prior
  nh <- nrow(input$hub$items)
  if (input$judge$epsilon == 1) {
    # The entire likelihood is constant: report the known Gaussian product.
    mean <- stats::setNames(c(prior$mean, rep(0, length(coords) - 1L)), coords)
    V <- diag(c(prior$sd^2, rep(1, length(coords) - 1L)), nrow = length(coords))
    dimnames(V) <- list(coords, coords)
    mu <- as.double(input$item_transform %*% mean)
    sd <- sqrt(diag(input$item_transform %*% V %*% t(input$item_transform)))
    intervals <- rbind(mu + stats::qnorm(.025) * sd, mu + stats::qnorm(.975) * sd)
    delta_interval <- stats::qnorm(c(.025, .975), prior$mean, prior$sd)
  } else if (input$counts$cross == 0L) {
    # Exact factorization: Monte Carlo noise cannot manufacture identification
    # or between-block dependence. Raw draws remain unmodified in prediction.
    mean[1] <- prior$mean
    V[1, ] <- V[, 1] <- 0
    V[1, 1] <- prior$sd^2
    hu <- if (nh > 1L) 1L + seq_len(nh - 1L) else integer()
    ns <- nrow(input$spoke$items)
    su <- if (ns > 1L) nh + seq_len(ns - 1L) else integer()
    V[hu, su] <- 0
    V[su, hu] <- 0
    delta_interval <- stats::qnorm(c(.025, .975), prior$mean, prior$sd)
    for (j in nh + seq_len(ns)) {
      shape <- theta[, j] - matrix[, 1]
      intervals[, j] <- vapply(c(.025, .975), function(p) {
        .link_e3_prior_convolution_quantile(shape, prior, p)
      }, numeric(1))
    }
  }
  means <- as.double(input$item_transform %*% mean)
  variance <- diag(input$item_transform %*% V %*% t(input$item_transform))
  .link_check(all(is.finite(c(means, V, variance))) && all(variance >= 0),
    "E3 MCMC covariance/summary is not finite and valid.")
  list(theta_mean = means, theta_sd = sqrt(variance), lower = as.double(intervals[1, ]),
    upper = as.double(intervals[2, ]), covariance = V,
    delta = list(mean = unname(mean[1]), sd = sqrt(V[1, 1]),
      lower = delta_interval[1], upper = delta_interval[2], identification = .link_e3_identification(input)))
}

.link_e3_mcmc <- function(input, control) {
  .btl_mcmc_require_cmdstanr()
  if (!requireNamespace("posterior", quietly = TRUE)) {
    rlang::abort("E3-MCMC requires the optional 'posterior' package.", class = "pairwiseLLM_mcmc_dependency_missing")
  }
  started <- proc.time()
  diagnostics <- list(fit_attempted = TRUE, fit_valid = FALSE,
    uncertainty_scope = "joint_shapes_and_offset", covariance_jitter = 0,
    sampler = list(engine = "mcmc"))
  result <- tryCatch({
    sampled <- .link_e3_sample(.link_e3_stan_data(input), control)
    draws <- .link_e3_free_draws(sampled$fit, input)
    .link_check(identical(as.integer(dim(draws)[1:2]),
      as.integer(c(sampled$config$iter_sampling, sampled$config$chains))),
      "E3 sampler returned an unexpected chain or draw count.")
    diagnostics$sampler <- .link_e3_sampler_diagnostics(sampled$fit, draws, sampled$config)
    diagnostics$sampler$model_hash <- sampled$model_hash
    diagnostics$sampler$summary_method <- if (input$judge$epsilon == 1) "exact_independent_priors" else
      if (input$counts$cross == 0L) "factorized_shape_draws_exact_offset_prior" else "posterior_draws"
    summaries <- .link_e3_draw_summary(draws, input)
    diagnostics$covariance_valid <- TRUE
    diagnostics$fit_valid <- diagnostics$sampler$audit_gate$passed
    if (!diagnostics$fit_valid) diagnostics$failure_code <- "mcmc_audit_gate_failed"
    # No R6 object, external pointer, CSV dependency, or falsely named mean-as-mode.
    prediction <- list(method = "posterior_draws", free_draws = matrix(draws,
      ncol = dim(draws)[3], dimnames = list(NULL, dimnames(draws)[[3]])),
      chain = rep(seq_len(dim(draws)[2]), each = dim(draws)[1]),
      iteration = rep(seq_len(dim(draws)[1]), dim(draws)[2]))
    prediction$hash <- .link_hash(prediction)
    c(summaries, list(prediction = prediction))
  }, error = function(e) {
    diagnostics$fit_valid <<- FALSE
    diagnostics$covariance_valid <<- FALSE
    diagnostics$failure_code <<- e$failure_code %||% "e3_mcmc_failure"
    diagnostics$sampler$message <<- conditionMessage(e)
    list(theta_mean = rep(NA_real_, nrow(input$item_transform)),
      delta = list(mean = NA_real_, identification = "failed"))
  })
  elapsed <- proc.time() - started
  diagnostics$elapsed_seconds <- unname(elapsed[["elapsed"]])
  diagnostics$cpu_seconds <- unname(sum(elapsed[c("user.self", "sys.self")]))
  do.call(.link_new_result, c(list(input = input, diagnostics = diagnostics), result))
}

.link_e3_mcmc_predict <- function(state, pairs, input) {
  .link_fields(state, c("method", "free_draws", "chain", "iteration", "hash"),
    c("method", "free_draws", "chain", "iteration", "hash"), "E3 MCMC prediction state")
  .link_check(identical(state$method, "posterior_draws") &&
    identical(state$hash, .link_hash(state[names(state) != "hash"])), "Invalid E3 MCMC prediction state.")
  draws <- .link_align_numeric(state$free_draws, colnames(input$item_transform), "E3 free draws")
  .link_check(is.matrix(draws) && nrow(draws) >= 2L &&
    length(state$chain) == nrow(draws) && length(state$iteration) == nrow(draws),
    "Invalid E3 MCMC draw dimensions.")
  X <- .link_pair_surface(pairs, input)
  if (input$judge$epsilon == 1) return(rep(.5, nrow(pairs)))
  vapply(seq_len(nrow(pairs)), function(i) {
    eta <- as.double(draws %*% X[i, ]) + input$judge$beta
    (1 - input$judge$epsilon) * mean(stats::plogis(eta)) + input$judge$epsilon / 2
  }, numeric(1))
}
