# Contracts shared by fixed and schedule-aware parametric BT bootstrap.
.bt_bootstrap_abort <- function(message, ...) {
  rlang::abort(message, class = "pairwiseLLM_bt_bootstrap_error", ...)
}

.bt_bootstrap_integer <- function(x, name, lower = 1L) {
  if (!.bt_real_vector(x) || length(x) != 1L || !is.finite(x) ||
      x != floor(x) || x < lower || x > .Machine$integer.max) {
    .bt_bootstrap_abort(paste0("`", name, "` must be an integer from ", lower, " to .Machine$integer.max."))
  }
  as.integer(x)
}

.bt_bootstrap_theta <- function(object, ids = NULL) {
  if (inherits(object, "pairwiseLLM_bt_lapse") ||
      (is.list(object) && identical(object$engine, "lapse"))) {
    .bt_bootstrap_abort("Lapse/positional fits are outside the simple-BT bootstrap interface.")
  }
  if (is.list(object)) {
    convergence <- object$provenance$convergence$converged
    if (!is.null(convergence) && !isTRUE(convergence)) {
      .bt_bootstrap_abort("A bootstrap fit must report successful convergence.")
    }
    if (identical(object$provenance$uncertainty$valid, FALSE)) {
      .bt_bootstrap_abort("A bootstrap fit with failed uncertainty diagnostics cannot be retained.")
    }
    tbl <- object$theta
    if (!is.data.frame(tbl) || !all(c("ID", "theta") %in% names(tbl))) {
      .bt_bootstrap_abort("A fitted object must contain `theta` with columns `ID` and `theta`.")
    }
    values <- tbl$theta
    labels <- as.character(tbl$ID)
  } else {
    values <- object
    labels <- names(object)
  }
  if (!.bt_real_vector(values) || length(values) < 2L || any(!is.finite(values)) ||
      length(labels) != length(values) || anyNA(labels) || any(!nzchar(labels)) || anyDuplicated(labels)) {
    .bt_bootstrap_abort("Theta must contain at least two finite values with unique, nonempty item IDs.")
  }
  ids <- ids %||% sort(labels, method = "radix")
  if (!setequal(labels, ids)) .bt_bootstrap_abort("Every bootstrap fit must contain exactly the generating item IDs.")
  values <- as.double(values[match(ids, labels)])
  values <- values - mean(values)
  if (any(!is.finite(values))) .bt_bootstrap_abort("Centered theta is outside the finite numerical range.")
  stats::setNames(values, ids)
}

.bt_bootstrap_pairs <- function(schedule, ids) {
  if (!is.data.frame(schedule)) .bt_bootstrap_abort("Supply a comparison schedule as a data frame.")
  columns <- if (all(c("A_id", "B_id") %in% names(schedule))) c("A_id", "B_id") else c("object1", "object2")
  if (!all(columns %in% names(schedule))) .bt_bootstrap_abort("Schedule needs `A_id`/`B_id` or `object1`/`object2`.")
  out <- data.frame(object1 = as.character(schedule[[columns[1L]]]),
                    object2 = as.character(schedule[[columns[2L]]]), stringsAsFactors = FALSE)
  if (!nrow(out) || anyNA(out) || any(!out$object1 %in% ids) || any(!out$object2 %in% ids) ||
      any(out$object1 == out$object2)) {
    .bt_bootstrap_abort("Schedule must contain non-self comparisons between generating item IDs.")
  }
  .bt_check_connected(out, ids)
  out
}

.bt_bootstrap_estimator <- function(object, estimator, args) {
  if (!is.list(args) || (length(args) &&
      (is.null(names(args)) || anyNA(names(args)) || any(!nzchar(names(args))) || anyDuplicated(names(args))))) {
    .bt_bootstrap_abort("`estimator_args` must be a uniquely named list.")
  }
  known <- inherits(object, "pairwiseLLM_bt_alpha") || inherits(object, "pairwiseLLM_bt_firth")
  if (known) {
    original <- object$provenance$supplied_arguments
    if ((!is.null(estimator) && !identical(estimator, object$engine)) ||
        (length(args) && !identical(args, original))) {
      .bt_bootstrap_abort("Use the fitted object's estimator and settings unchanged, or supply named theta explicitly.")
    }
    estimator <- object$engine
    args <- original
  }
  if (!is.function(estimator) &&
      (!is.character(estimator) || length(estimator) != 1L || is.na(estimator) ||
       !estimator %in% c("alpha", "brglm2"))) {
    .bt_bootstrap_abort("Supply estimator `alpha`, `brglm2`, or a function of `bt_data` and `item_ids`.")
  }
  reserved <- if (is.function(estimator)) c("bt_data", "item_ids") else c("bt_data", "engine", "verbose")
  if (any(names(args) %in% reserved)) .bt_bootstrap_abort("`estimator_args` contains reserved arguments.")
  if (identical(estimator, "alpha")) {
    alpha <- args$alpha
    if (!.bt_real_vector(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha < 0) {
      .bt_bootstrap_abort("The alpha estimator requires an explicit nonnegative `estimator_args$alpha`.")
    }
    .bt_alpha_control(args[setdiff(names(args), "alpha")], FALSE)
  }
  if (identical(estimator, "brglm2")) {
    if (!.require_ns("brglm2")) {
      .bt_bootstrap_abort("The requested brglm2 estimator requires optional package 'brglm2'.")
    }
    .bt_firth_control(args, FALSE)
  }
  list(estimator = estimator, args = args)
}

.bt_bootstrap_refit <- function(data, ids, spec) {
  if (is.function(spec$estimator)) {
    fit <- do.call(spec$estimator, c(list(bt_data = data, item_ids = ids), spec$args))
  } else {
    fit <- do.call(fit_bt_model, c(list(bt_data = data, engine = spec$estimator, verbose = FALSE), spec$args))
  }
  .bt_bootstrap_theta(fit, ids)
}

.bt_bootstrap_clock <- function() as.POSIXct("2000-01-01", tz = "UTC")

.bt_bootstrap_initial_state <- function(state, ids) {
  if (!inherits(state, "adaptive_state") || !setequal(state$item_ids, ids) ||
      !identical(state$controller$run_mode, "within_set")) {
    .bt_bootstrap_abort("`initial_state` must be an ordinary within-set adaptive state for the generating items.")
  }
  empty <- nrow(state$step_log) == 0L && nrow(state$history_pairs) == 0L &&
    nrow(state$round_log) == 0L && length(state$item_log) == 0L && is.null(state$btl_fit) &&
    is.null(state$stop_metrics) && !isTRUE(state$meta$stop_decision) &&
    is.na(state$meta$stop_boundary_step_id) && identical(state$warm_start_idx, 1L) &&
    identical(state$warm_start_done, FALSE)
  if (!isTRUE(empty)) {
    .bt_bootstrap_abort("`initial_state` must be pristine, before any attempted comparisons or refits.")
  }
  .adaptive_pairing_strategy(state)
  .warm_start_adaptive_validate(state)
  .adaptive_reservoir_validate_state(state)
  initial_seed <- .bt_bootstrap_integer(state$meta$initialization_seed %||% state$meta$seed,
    "initialization seed", -.Machine$integer.max)
  # Rebuild evidence-bearing fields independently; clearing logs cannot turn a
  # fitted state into a valid outcome-independent initialization.
  fresh <- new_adaptive_state(state$items, now_fn = .bt_bootstrap_clock)
  fresh <- .warm_start_adaptive_init(fresh, prior = state$predictive_prior,
    mode = state$meta$warm_start_mode %||% "cold")
  fields <- c("trueskill_state", "history_state", "item_step_log", "item_index", "n_items")
  refit_fields <- setdiff(names(fresh$refit_meta), "link_refit_local_memo_env")
  expected_round <- .adaptive_new_round_state(state$item_ids, controller = state$controller)
  if (!identical(state[fields], fresh[fields]) ||
      !identical(state$refit_meta[refit_fields], fresh$refit_meta[refit_fields]) ||
      !identical(state$round, expected_round) || isTRUE(state$controller$global_identified)) {
    .bt_bootstrap_abort(paste0("`initial_state` contains updated scores, histories, or scheduling state; ",
      "recreate it before outcomes."))
  }
  pairs <- state$warm_start_pairs
  if (!is.data.frame(pairs) || !identical(names(pairs), c("i_id", "j_id")) || nrow(pairs) != length(ids) - 1L) {
    .bt_bootstrap_abort("Initialization must contain a connected N - 1 comparison structure.")
  }
  .bt_bootstrap_pairs(data.frame(object1 = pairs$i_id, object2 = pairs$j_id), ids)
  # Copy mutable state environments and remove persistence and wall-clock time.
  # Retain item metadata: custom scheduling fitters may depend on it.
  state <- unserialize(serialize(state, NULL))
  state$meta$initialization_seed <- initial_seed
  state$meta$now_fn <- .bt_bootstrap_clock
  state$config$session_dir <- NULL
  state$config$persist_item_log <- FALSE
  if (.adaptive_reservoir_active(state)) {
    edges <- state$replay_reservoir$edges
    edges$Y <- rep(0L, nrow(edges))
    state <- .adaptive_reservoir_bind(state, make_adaptive_replay_reservoir(edges, state$item_ids))
  }
  state
}
