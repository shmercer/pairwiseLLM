# Run the actual scheduler under independent simulation and refit randomness.
.bt_bootstrap_with_seed <- function(seed, code) {
  withr::with_seed(seed, code, .rng_kind = "Mersenne-Twister",
    .rng_normal_kind = "Inversion", .rng_sample_kind = "Rejection")
}

.bt_bootstrap_seeds <- function(seed, replicate) {
  stats::setNames(vapply(seq_len(4L), function(stage) {
    .adaptive_stage_seed(seed, replicate, stage, offset = 304L)
  }, integer(1)), c("selector", "outcome", "scheduling_refit", "estimator"))
}

.bt_bootstrap_simulate <- function(pairs, theta, seed) {
  p <- stats::plogis(theta[pairs[[1L]]] - theta[pairs[[2L]]])
  .bt_bootstrap_with_seed(seed, as.integer(stats::runif(nrow(pairs)) < p))
}

.bt_bootstrap_judge <- function(theta, seed, budget) {
  uniforms <- .bt_bootstrap_with_seed(seed, stats::runif(budget))
  force(theta)
  function(A, B, state, ...) {
    step <- nrow(state$history_pairs) + 1L
    p <- stats::plogis(theta[[A$item_id]] - theta[[B$item_id]])
    list(is_valid = TRUE, Y = as.integer(uniforms[[step]] < p), judge_backend = "bt_bootstrap")
  }
}

.bt_bootstrap_schedule_fit <- function(fit_fn, seed) {
  force(seed)
  fit_fn <- fit_fn %||% default_btl_fit_fn
  force(fit_fn)
  function(state, config) {
    refit_seed <- .adaptive_stage_seed(seed, nrow(state$history_pairs), 1L)
    config$cmdstan <- config$cmdstan %||% list()
    config$cmdstan$seed <- refit_seed
    .bt_bootstrap_with_seed(refit_seed, fit_fn(state, config))
  }
}

.bt_bootstrap_schedule <- function(spec, seeds) {
  state <- unserialize(serialize(spec$initial_state, NULL))
  state$meta$seed <- seeds[["selector"]]
  # Keep refit-driven selection and diagnostics, but do not terminate before the
  # requested fixed budget when a statistical stopping criterion first passes.
  state <- .adaptive_apply_controller_config(state, list(max_pairs_after_stop = spec$budget))
  if (.adaptive_reservoir_active(state)) {
    edges <- state$replay_reservoir$edges
    edges$Y <- .bt_bootstrap_simulate(edges, spec$theta, seeds[["outcome"]])
    reservoir <- make_adaptive_replay_reservoir(edges, state$item_ids)
    state <- .adaptive_reservoir_bind(state, reservoir)
    judge <- make_adaptive_judge_replay(reservoir)
  } else {
    judge <- .bt_bootstrap_judge(spec$theta, seeds[["outcome"]], spec$budget)
  }
  fit_fn <- .bt_bootstrap_schedule_fit(spec$schedule_fit_fn, seeds[["scheduling_refit"]])
  tryCatch({
    while (nrow(state$history_pairs) < spec$budget) {
      state <- adaptive_rank_run_live(state, judge, n_steps = 1L, fit_fn = fit_fn,
        btl_config = spec$btl_config, progress = "none")
      if (isTRUE(state$meta$stop_decision) && nrow(state$history_pairs) < spec$budget) {
        .bt_bootstrap_abort(paste0("Schedule ended before the comparison budget: ", state$meta$stop_reason),
          failure_reason = state$meta$stop_reason)
      }
    }
    state
  }, error = function(e) {
    .bt_bootstrap_abort(conditionMessage(e), parent = e, state = state,
      failure_reason = e$failure_reason %||% "schedule_error")
  })
}

.bt_bootstrap_schedule_diagnostics <- function(data, ids, state) {
  keys <- .adaptive_reservoir_key(data[[1L]], data[[2L]])
  degree <- tabulate(match(c(data[[1L]], data[[2L]]), ids), nbins = length(ids))
  connected <- tryCatch({
    .bt_check_connected(data, ids)
    TRUE
  }, error = function(e) FALSE)
  list(n_comparisons = as.integer(nrow(data)), n_unique_pairs = as.integer(length(unique(keys))),
    n_repeated_comparisons = as.integer(nrow(data) - length(unique(keys))),
    min_degree = as.integer(min(degree)), max_degree = as.integer(max(degree)), connected = connected,
    schedule_digest = .warm_start_prior_hash(data[1:2]),
    n_scheduling_refits = if (is.null(state)) 0L else as.integer(nrow(state$round_log)),
    n_attempts = if (is.null(state)) as.integer(nrow(data)) else as.integer(nrow(state$step_log)),
    global_identified = if (is.null(state)) NA else isTRUE(state$controller$global_identified),
    stop_boundary_step_id = if (is.null(state)) NA_integer_ else state$meta$stop_boundary_step_id)
}

.bt_bootstrap_one <- function(index, spec) {
  seeds <- .bt_bootstrap_seeds(spec$seed, index)
  state <- NULL
  data <- data.frame(object1 = character(), object2 = character(), result = double())
  phase <- "schedule"
  warnings <- character()
  failure <- NULL
  theta <- .bt_bootstrap_with_seed(seeds[["selector"]], tryCatch(withCallingHandlers({
    if (spec$mode == "fixed") {
      data <- spec$schedule
      data$result <- .bt_bootstrap_simulate(data, spec$theta, seeds[["outcome"]])
    } else {
      state <- .bt_bootstrap_schedule(spec, seeds)
      rows <- state$step_log[!is.na(state$step_log$pair_id), ]
      data <- data.frame(object1 = rows$A_id, object2 = rows$B_id, result = rows$Y)
    }
    if (nrow(data) != spec$budget) .bt_bootstrap_abort("Replicate did not attain the exact comparison budget.")
    phase <- "estimator"
    .bt_bootstrap_with_seed(seeds[["estimator"]], .bt_bootstrap_refit(data, names(spec$theta), spec$refit))
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    failure <<- list(phase = phase, reason = e$failure_reason %||% class(e)[1L],
      message = e$message, classes = class(e),
      diagnostics = if (spec$keep == "full") e$diagnostics %||% e$parent$diagnostics else NULL)
    if (!is.null(e$state)) {
      state <<- e$state
      rows <- state$step_log[!is.na(state$step_log$pair_id), ]
      data <<- data.frame(object1 = rows$A_id, object2 = rows$B_id, result = rows$Y)
    }
    NULL
  }))
  diagnostics <- c(list(replicate = as.integer(index)), as.list(seeds),
    list(success = is.null(failure), failure_phase = failure$phase %||% NA_character_,
      failure_reason = failure$reason %||% NA_character_, message = failure$message %||% NA_character_),
    .bt_bootstrap_schedule_diagnostics(data, names(spec$theta), state), list(warnings = list(warnings)))
  list(theta = theta, diagnostics = tibble::as_tibble(diagnostics), failure = failure,
    artifacts = if (spec$keep == "full") list(comparisons = data, state = state) else NULL)
}
