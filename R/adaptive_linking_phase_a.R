# -------------------------------------------------------------------------
# Adaptive linking Phase A artifact helpers.
# -------------------------------------------------------------------------

.adaptive_phase_a_empty_state <- function(set_ids = integer()) {
  set_ids <- as.integer(sort(unique(set_ids)))
  tibble::tibble(
    set_id = set_ids,
    source = rep(NA_character_, length(set_ids)),
    status = rep(NA_character_, length(set_ids)),
    validation_message = rep(NA_character_, length(set_ids)),
    artifact_path = rep(NA_character_, length(set_ids))
  )
}

.adaptive_phase_a_status_tbl <- function(state) {
  phase_a <- state$linking$phase_a %||% list()
  tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
}

.adaptive_phase_a_ready_sets <- function(state) {
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  ready_sets <- integer()
  if (nrow(status_tbl) > 0L) {
    ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
    ready_sets <- ready_sets[!is.na(ready_sets)]
  }
  as.integer(sort(unique(ready_sets)))
}

.adaptive_phase_a_pending_run_sets <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  if (!as.character(controller$run_mode %||% "within_set") %in% c("link_one_spoke", "link_multi_spoke")) {
    return(integer())
  }
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  if (nrow(status_tbl) < 1L) {
    return(integer())
  }
  keep <- !is.na(status_tbl$set_id) &
    status_tbl$source %in% "run" &
    status_tbl$status != "ready"
  as.integer(status_tbl$set_id[keep])
}

.adaptive_phase_a_ready_spokes <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  if (nrow(status_tbl) < 1L) {
    return(integer())
  }
  hub_id <- as.integer(controller$hub_id %||% 1L)
  ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
  if (!hub_id %in% ready_sets) {
    return(integer())
  }
  spokes <- setdiff(unique(as.integer(state$items$set_id)), hub_id)
  as.integer(sort(intersect(spokes, ready_sets)))
}

.adaptive_link_phase_context <- function(state, controller = NULL) {
  controller <- controller %||% .adaptive_controller_resolve(state)
  mode <- as.character(controller$run_mode %||% "within_set")
  if (!mode %in% c("link_one_spoke", "link_multi_spoke")) {
    return(list(
      phase = "phase_a",
      pending_run_sets = integer(),
      ready_spokes = integer(),
      active_phase_a_set = NA_integer_
    ))
  }

  pending_run_sets <- .adaptive_phase_a_pending_run_sets(state, controller = controller)
  ready_spokes <- .adaptive_phase_a_ready_spokes(state, controller = controller)
  pending_run_sets <- as.integer(pending_run_sets[!is.na(pending_run_sets)])
  active_set <- if (length(pending_run_sets) > 0L) {
    as.integer(sort(unique(pending_run_sets))[[1L]])
  } else {
    NA_integer_
  }
  status_tbl <- .adaptive_phase_a_status_tbl(state)
  phase <- if (length(pending_run_sets) > 0L) {
    "phase_a"
  } else if (length(ready_spokes) > 0L) {
    "phase_b"
  } else {
    "phase_a"
  }
  list(
    phase = as.character(phase),
    pending_run_sets = as.integer(pending_run_sets),
    ready_spokes = as.integer(ready_spokes),
    active_phase_a_set = as.integer(active_set)
  )
}

.adaptive_phase_a_hash_object <- function(x) {
  tmp <- tempfile("phase_a_hash_", fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(x, tmp)
  as.character(unname(tools::md5sum(tmp)))
}

.adaptive_phase_a_required_config_hash <- function(state, set_id) {
  controller <- .adaptive_controller_resolve(state)
  fit <- state$btl_fit %||% list()
  payload <- list(
    set_id = as.integer(set_id),
    judge_param_mode = as.character(controller$judge_param_mode %||% NA_character_),
    model_variant = as.character(fit$model_variant %||% "btl_e_b")
  )
  .adaptive_phase_a_hash_object(payload)
}

.adaptive_phase_a_run_stop_passed <- function(artifact, controller) {
  diagnostics <- artifact$diagnostics %||% list()
  diagnostics_pass <- isTRUE(diagnostics$diagnostics_pass %||% FALSE)
  reliability <- .adaptive_phase_a_extract_reliability(artifact)
  reliability_min <- as.double(controller$phase_a_required_reliability_min %||% 0.80)
  n_pairs_committed <- as.integer(artifact$n_pairs_committed %||% 0L)

  isTRUE(diagnostics_pass) &&
    is.finite(reliability) &&
    reliability >= reliability_min &&
    n_pairs_committed > 0L
}

.adaptive_phase_a_extract_set_draws <- function(state, set_id) {
  draws <- state$btl_fit$btl_posterior_draws %||% NULL
  if (!is.matrix(draws) || !is.numeric(draws) || nrow(draws) < 1L || ncol(draws) < 1L) {
    return(NULL)
  }

  ids <- as.character(state$items$item_id[state$items$set_id == as.integer(set_id)])
  if (length(ids) == 0L) {
    return(NULL)
  }

  if (is.null(colnames(draws))) {
    all_ids <- as.character(state$item_ids)
    if (ncol(draws) != length(all_ids)) {
      return(NULL)
    }
    colnames(draws) <- all_ids
  }
  if (!all(ids %in% colnames(draws))) {
    return(NULL)
  }

  draws <- draws[, ids, drop = FALSE]
  .pairwiseLLM_sanitize_draws_matrix(draws, name = "btl_posterior_draws")
}

.adaptive_phase_a_build_artifact <- function(state, set_id) {
  set_id <- as.integer(set_id)
  items_set <- state$items[state$items$set_id == set_id, , drop = FALSE]
  if (nrow(items_set) == 0L) {
    rlang::abort(paste0("No items found for set_id ", set_id, "."))
  }

  ids <- as.character(items_set$item_id)
  global_ids <- as.character(items_set$global_item_id)

  latest_item_log <- NULL
  if (is.list(state$item_log) && length(state$item_log) > 0L) {
    latest_item_log <- tibble::as_tibble(state$item_log[[length(state$item_log)]])
  }

  theta_mean <- NULL
  theta_sd <- NULL
  rank_mu_raw <- NULL

  if (!is.null(latest_item_log) && nrow(latest_item_log) > 0L &&
    all(c("item_id", "theta_raw_eap", "theta_sd") %in% names(latest_item_log))) {
    idx <- match(ids, as.character(latest_item_log$item_id))
    if (all(!is.na(idx))) {
      theta_mean <- as.double(latest_item_log$theta_raw_eap[idx])
      theta_sd <- as.double(latest_item_log$theta_sd[idx])
      if ("rank_global_eap" %in% names(latest_item_log)) {
        rank_mu_raw <- as.double(latest_item_log$rank_global_eap[idx])
      }
    }
  }

  draws <- .adaptive_phase_a_extract_set_draws(state, set_id = set_id)
  if ((is.null(theta_mean) || is.null(theta_sd)) && !is.null(draws)) {
    theta_mean <- as.double(colMeans(draws))
    theta_sd <- as.double(apply(draws, 2, stats::sd))
  }

  if (is.null(theta_mean) || is.null(theta_sd)) {
    rlang::abort(paste0(
      "Within-set summaries are unavailable for set_id ",
      set_id,
      "; run additional within-set comparisons/refits before linking."
    ))
  }

  if (is.null(rank_mu_raw)) {
    rank_mu_raw <- as.double(rank(-theta_mean, ties.method = "average"))
  }

  set_map <- stats::setNames(as.integer(state$items$set_id), as.character(state$items$item_id))
  history <- .adaptive_history_tbl(state)
  n_pairs_committed <- 0L
  if (nrow(history) > 0L) {
    a_set <- set_map[as.character(history$A_id)]
    b_set <- set_map[as.character(history$B_id)]
    n_pairs_committed <- as.integer(sum(a_set == set_id & b_set == set_id, na.rm = TRUE))
  }

  controller <- .adaptive_controller_resolve(state)
  round_log <- tibble::as_tibble(state$round_log %||% tibble::tibble())
  diagnostics_pass <- if (nrow(round_log) > 0L && "diagnostics_pass" %in% names(round_log)) {
    as.logical(round_log$diagnostics_pass[[nrow(round_log)]])
  } else {
    NA
  }
  ts_rank <- if (nrow(round_log) > 0L && "ts_btl_rank_spearman" %in% names(round_log)) {
    as.double(round_log$ts_btl_rank_spearman[[nrow(round_log)]])
  } else {
    NA_real_
  }
  reliability <- if (!is.null(draws)) {
    as.double(compute_reliability_EAP(draws))
  } else {
    NA_real_
  }

  fit <- state$btl_fit %||% list()
  fit_model_id <- as.character(fit$model_variant %||% "btl_e_b")
  fit_config_hash <- .adaptive_phase_a_required_config_hash(state, set_id = set_id)

  list(
    set_id = set_id,
    fit_model_id = fit_model_id,
    fit_config_hash = fit_config_hash,
    n_items = as.integer(length(ids)),
    n_pairs_committed = as.integer(n_pairs_committed),
    items = tibble::tibble(
      item_id = as.character(ids),
      global_item_id = as.character(global_ids),
      theta_raw_mean = as.double(theta_mean),
      theta_raw_sd = as.double(theta_sd),
      rank_mu_raw = as.double(rank_mu_raw)
    ),
    diagnostics = list(
      reliability_EAP_within = reliability,
      ts_btl_rank_spearman = ts_rank,
      diagnostics_pass = diagnostics_pass
    ),
    posterior_draws = draws,
    judge_param_mode = as.character(controller$judge_param_mode %||% NA_character_),
    created_at = Sys.time()
  )
}

.adaptive_phase_a_read_import_artifact <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    if (!file.exists(x)) {
      rlang::abort(paste0("Phase A artifact path does not exist: ", x))
    }
    return(readRDS(x))
  }
  if (is.list(x)) {
    return(x)
  }
  rlang::abort("Imported Phase A artifact entries must be a list or .rds path.")
}

.adaptive_phase_a_extract_reliability <- function(artifact) {
  diagnostics <- artifact$diagnostics %||% list()
  as.double(diagnostics$reliability_EAP_within %||% artifact$reliability_EAP_within %||% NA_real_)
}

.adaptive_phase_a_validate_imported_artifact <- function(artifact, state, set_id, controller) {
  if (!is.list(artifact)) {
    rlang::abort("Imported Phase A artifact must be a list.")
  }

  set_id <- as.integer(set_id)
  artifact_set <- as.integer(artifact$set_id %||% NA_integer_)
  if (is.na(artifact_set) || !identical(artifact_set, set_id)) {
    rlang::abort(paste0("Phase A artifact set_id mismatch for set ", set_id, "."))
  }

  fit_model_id <- as.character(artifact$fit_model_id %||% NA_character_)
  allowed_model_ids <- as.character(controller$phase_a_compatible_model_ids %||% "btl_e_b")
  if (!is.character(fit_model_id) || length(fit_model_id) != 1L || is.na(fit_model_id) ||
    !fit_model_id %in% allowed_model_ids) {
    rlang::abort(paste0(
      "Phase A artifact likelihood/model incompatibility for set ",
      set_id,
      "."
    ))
  }

  fit_config_hash <- as.character(artifact$fit_config_hash %||% NA_character_)
  required_hash <- .adaptive_phase_a_required_config_hash(state, set_id = set_id)
  compatible_hashes <- as.character(controller$phase_a_compatible_config_hashes %||% character())
  if (is.na(fit_config_hash) || fit_config_hash == "") {
    rlang::abort(paste0("Phase A artifact missing fit_config_hash for set ", set_id, "."))
  }
  if (!identical(fit_config_hash, required_hash) && !fit_config_hash %in% compatible_hashes) {
    rlang::abort(paste0(
      "Phase A artifact config hash incompatibility for set ",
      set_id,
      ": artifact hash `",
      fit_config_hash,
      "` did not match required hash `",
      required_hash,
      "` and was not found in `adaptive_config$phase_a_compatible_config_hashes`."
    ))
  }

  items_tbl <- tibble::as_tibble(artifact$items %||% tibble::tibble())
  required_cols <- c("global_item_id", "theta_raw_mean", "theta_raw_sd")
  missing_cols <- setdiff(required_cols, names(items_tbl))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0(
      "Phase A artifact completeness failure for set ",
      set_id,
      ": missing ",
      paste(missing_cols, collapse = ", "),
      "."
    ))
  }

  if (!is.character(items_tbl$global_item_id) ||
    any(is.na(items_tbl$global_item_id) | items_tbl$global_item_id == "")) {
    rlang::abort(paste0("Phase A artifact item identity failure for set ", set_id, "."))
  }
  if (anyDuplicated(items_tbl$global_item_id)) {
    rlang::abort(paste0("Phase A artifact item identity duplicates for set ", set_id, "."))
  }

  state_items <- tibble::as_tibble(state$items)
  state_set_items <- state_items[state_items$set_id == set_id, c("item_id", "global_item_id"), drop = FALSE]
  expected_global <- as.character(state_set_items$global_item_id)
  if (!setequal(expected_global, as.character(items_tbl$global_item_id))) {
    rlang::abort(paste0("Phase A artifact global_item_id mapping mismatch for set ", set_id, "."))
  }

  by_global <- match(expected_global, as.character(items_tbl$global_item_id))
  theta_mean <- as.double(items_tbl$theta_raw_mean[by_global])
  theta_sd <- as.double(items_tbl$theta_raw_sd[by_global])
  if (any(is.na(theta_mean)) || any(is.na(theta_sd))) {
    rlang::abort(paste0("Phase A artifact completeness failure for set ", set_id, "."))
  }

  if ("item_id" %in% names(items_tbl)) {
    imported_item_ids <- as.character(items_tbl$item_id[by_global])
    if (any(is.na(imported_item_ids)) || !identical(imported_item_ids, as.character(state_set_items$item_id))) {
      rlang::abort(paste0("Phase A artifact item_id mapping mismatch for set ", set_id, "."))
    }
  }

  reliability <- .adaptive_phase_a_extract_reliability(artifact)
  reliability_min <- as.double(controller$phase_a_required_reliability_min %||% 0.80)
  quality_gate_accepted <- isTRUE(artifact$quality_gate_accepted %||% FALSE)
  if ((!is.finite(reliability) || is.na(reliability)) && !quality_gate_accepted) {
    rlang::abort(paste0("Phase A artifact missing reliability_EAP_within for set ", set_id, "."))
  }
  if (is.finite(reliability) && reliability < reliability_min && !quality_gate_accepted) {
    rlang::abort(paste0(
      "Phase A artifact reliability gate failed for set ",
      set_id,
      ": reliability_EAP_within below threshold."
    ))
  }

  invisible(TRUE)
}

.adaptive_phase_a_collect_import_map <- function(controller) {
  raw <- controller$phase_a_artifacts %||% list()
  if (is.null(raw)) {
    raw <- list()
  }
  if (!is.list(raw)) {
    rlang::abort("`adaptive_config$phase_a_artifacts` must be a named list.")
  }

  out <- list()
  nms <- names(raw)
  if (is.null(nms)) {
    nms <- rep("", length(raw))
  }
  for (idx in seq_along(raw)) {
    entry <- .adaptive_phase_a_read_import_artifact(raw[[idx]])
    set_key <- nms[[idx]]
    if (is.na(set_key) || set_key == "") {
      set_key <- as.character(as.integer(entry$set_id %||% NA_integer_))
    }
    set_id <- suppressWarnings(as.integer(set_key))
    if (is.na(set_id)) {
      rlang::abort("Unable to resolve set_id for an imported Phase A artifact.")
    }
    out[[as.character(set_id)]] <- entry
  }

  out
}

.adaptive_phase_a_resolve_set_sources <- function(controller, set_ids, import_map) {
  set_ids <- as.integer(sort(unique(set_ids)))
  mode <- as.character(controller$phase_a_mode %||% "run")
  source <- stats::setNames(rep("run", length(set_ids)), as.character(set_ids))

  if (identical(mode, "import")) {
    source[] <- "import"
  }

  if (identical(mode, "mixed")) {
    import_sets <- intersect(names(source), names(import_map))
    source[import_sets] <- "import"
  }

  explicit <- controller$phase_a_set_source %||% NULL
  if (!is.null(explicit)) {
    if (length(explicit) == 0L) {
      return(source)
    }
    if (!is.character(explicit) || is.null(names(explicit)) || any(names(explicit) == "")) {
      rlang::abort("`adaptive_config$phase_a_set_source` must be a named character vector.")
    }
    bad <- setdiff(unique(as.character(explicit)), c("run", "import"))
    if (length(bad) > 0L) {
      rlang::abort("`adaptive_config$phase_a_set_source` values must be `run` or `import`.")
    }
    overlap <- intersect(names(source), names(explicit))
    source[overlap] <- explicit[overlap]
  }

  source
}

.adaptive_phase_a_prepare <- function(state) {
  out <- state
  controller <- .adaptive_controller_resolve(out)
  set_ids <- as.integer(sort(unique(out$items$set_id)))
  persisted_status_tbl <- tibble::as_tibble(out$linking$phase_a$set_status %||% tibble::tibble())
  status_cols <- c("set_id", "source", "status", "validation_message")
  if (!all(status_cols %in% names(persisted_status_tbl))) {
    persisted_status_tbl <- tibble::tibble()
  }
  persisted_map <- list()
  persisted_raw <- out$linking$phase_a$artifacts %||% list()
  if (is.list(persisted_raw) && length(persisted_raw) > 0L) {
    for (nm in names(persisted_raw)) {
      art <- persisted_raw[[nm]]
      set_id <- as.integer(art$set_id %||% suppressWarnings(as.integer(nm)))
      if (!is.na(set_id)) {
        persisted_map[[as.character(set_id)]] <- art
      }
    }
  }

  import_map <- .adaptive_phase_a_collect_import_map(controller)
  for (set_key in names(persisted_map)) {
    if (is.null(import_map[[set_key]])) {
      import_map[[set_key]] <- persisted_map[[set_key]]
    }
  }
  sources <- .adaptive_phase_a_resolve_set_sources(controller, set_ids = set_ids, import_map = import_map)
  policy <- as.character(controller$phase_a_import_failure_policy %||% "fail_fast")

  statuses <- .adaptive_phase_a_empty_state(set_ids = set_ids)
  artifacts <- list()

  for (idx in seq_along(set_ids)) {
    set_id <- as.integer(set_ids[[idx]])
    set_key <- as.character(set_id)
    source <- as.character(sources[[set_key]] %||% "run")
    persisted_row <- persisted_status_tbl[persisted_status_tbl$set_id == set_id, , drop = FALSE]
    prior_status <- if (nrow(persisted_row) > 0L) {
      as.character(persisted_row$status[[1L]] %||% NA_character_)
    } else {
      NA_character_
    }

    status <- "pending_finalization"
    message <- NA_character_
    persisted <- persisted_map[[set_key]] %||% NULL

    if (!is.null(persisted)) {
      persisted_ok <- tryCatch(
        {
          .adaptive_phase_a_validate_imported_artifact(
            persisted,
            out,
            set_id = set_id,
            controller = controller
          )
          TRUE
        },
        error = function(e) {
          message <<- paste0("persisted_invalid: ", conditionMessage(e))
          FALSE
        }
      )
      if (isTRUE(persisted_ok)) {
        artifacts[[set_key]] <- persisted
        if (identical(source, "run") && identical(prior_status, "pending_finalization")) {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        } else {
          status <- "ready"
          message <- "persisted"
        }
      }
    }

    if (identical(source, "import") && !identical(status, "ready")) {
      artifact <- import_map[[set_key]] %||% NULL
      if (is.null(artifact)) {
        status <- "failed"
        message <- "configured for import but no artifact was provided"
      } else {
        validated <- tryCatch(
          {
            .adaptive_phase_a_validate_imported_artifact(artifact, out, set_id = set_id, controller = controller)
            TRUE
          },
          error = function(e) {
            message <<- conditionMessage(e)
            FALSE
          }
        )

        if (isTRUE(validated)) {
          artifacts[[set_key]] <- artifact
          status <- "ready"
          message <- "imported"
        } else if (identical(policy, "fallback_to_run")) {
          source <- "run"
          message <- paste0("import_failed_fallback_to_run: ", message)
        } else {
          status <- "failed"
        }
      }
    }

    if (identical(source, "run") && !identical(status, "ready")) {
      built <- tryCatch(
        {
          .adaptive_phase_a_build_artifact(out, set_id = set_id)
        },
        error = function(e) {
          message <<- conditionMessage(e)
          NULL
        }
      )

      if (is.null(built)) {
        if (is.character(message) &&
          grepl("Within-set summaries are unavailable", message, fixed = TRUE)) {
          status <- "pending_finalization"
          message <- "pending_finalization: awaiting_within_set_finalization"
        } else {
          status <- "failed"
        }
      } else {
        artifacts[[set_key]] <- built
        prior_pairs <- as.integer(persisted$n_pairs_committed %||% NA_integer_)
        built_pairs <- as.integer(built$n_pairs_committed %||% NA_integer_)
        hold_pending <- identical(prior_status, "pending_finalization") &&
          is.finite(prior_pairs) &&
          is.finite(built_pairs) &&
          built_pairs <= prior_pairs
        if (isTRUE(hold_pending)) {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        } else if (isTRUE(.adaptive_phase_a_run_stop_passed(built, controller = controller))) {
          status <- "ready"
          message <- "built_in_run"
        } else {
          status <- "pending_finalization"
          message <- "pending_finalization: within-set stop criteria not yet met"
        }
      }
    }

    statuses$source[[idx]] <- source
    statuses$status[[idx]] <- status
    statuses$validation_message[[idx]] <- message
  }

  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  ready_for_phase_b <- isTRUE(all(statuses$status == "ready"))
  ready_spokes <- integer()
  active_phase_a_set <- NA_integer_
  pending_run <- integer()
  if (isTRUE(is_link_mode)) {
    status_tbl <- tibble::as_tibble(statuses)
    hub_id <- as.integer(controller$hub_id %||% 1L)
    ready_sets <- as.integer(status_tbl$set_id[status_tbl$status == "ready"])
    spokes <- setdiff(unique(as.integer(out$items$set_id)), hub_id)
    ready_spokes <- if (hub_id %in% ready_sets) {
      as.integer(sort(intersect(spokes, ready_sets)))
    } else {
      integer()
    }
    pending_run <- as.integer(status_tbl$set_id[status_tbl$source == "run" & status_tbl$status != "ready"])
    if (length(pending_run) > 0L) {
      active_phase_a_set <- as.integer(sort(pending_run)[[1L]])
    }
  }
  phase <- if (isTRUE(is_link_mode) && length(pending_run) == 0L &&
    (isTRUE(ready_for_phase_b) || length(ready_spokes) > 0L)) {
    "phase_b"
  } else {
    "phase_a"
  }
  prior_phase <- as.character((out$linking$phase_a %||% list())$phase %||% "phase_a")
  prior_phase_b_start <- as.integer((out$linking$phase_a %||% list())$phase_b_started_at_step %||% NA_integer_)
  phase_b_start <- prior_phase_b_start
  if (!identical(prior_phase, "phase_b") && identical(phase, "phase_b") && !is.finite(phase_b_start)) {
    phase_b_start <- as.integer(nrow(out$step_log %||% tibble::tibble()) + 1L)
  }

  out$linking <- out$linking %||% list()
  out$linking$phase_a <- list(
    set_status = statuses,
    artifacts = artifacts,
    ready_for_phase_b = ready_for_phase_b,
    phase = phase,
    ready_spokes = as.integer(ready_spokes),
    active_phase_a_set = as.integer(active_phase_a_set),
    phase_b_started_at_step = as.integer(phase_b_start)
  )

  out
}

.adaptive_phase_a_gate_or_abort <- function(state) {
  controller <- .adaptive_controller_resolve(state)
  run_mode <- as.character(controller$run_mode %||% "within_set")
  is_link_mode <- run_mode %in% c("link_one_spoke", "link_multi_spoke")
  if (!isTRUE(is_link_mode)) {
    return(invisible(state))
  }

  phase_a <- state$linking$phase_a %||% list()
  status_tbl <- tibble::as_tibble(phase_a$set_status %||% tibble::tibble())
  if (nrow(status_tbl) > 0L && any(status_tbl$status %in% "failed")) {
    blocked <- status_tbl[status_tbl$status %in% "failed", , drop = FALSE]
    blocked_msg <- paste0(
      "set ",
      blocked$set_id,
      " [",
      blocked$source,
      "]: ",
      blocked$validation_message
    )
    blocked_msg <- paste(blocked_msg, collapse = "; ")
    rlang::abort(paste0(
      "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
      blocked_msg
    ))
  }

  phase_ctx <- .adaptive_link_phase_context(state, controller = controller)
  if (length(phase_ctx$pending_run_sets) > 0L) {
    return(invisible(state))
  }
  if (identical(phase_ctx$phase, "phase_b")) {
    if (length(phase_ctx$ready_spokes) < 1L) {
      rlang::abort(
        paste0(
          "Phase metadata and routing mode disagree: phase marked phase_b but no ready spokes are available."
        )
      )
    }
    required_sets <- as.integer(unique(c(as.integer(controller$hub_id %||% 1L), phase_ctx$ready_spokes)))
    artifacts <- phase_a$artifacts %||% list()
    missing_sets <- required_sets[!as.character(required_sets) %in% names(artifacts)]
    if (length(missing_sets) > 0L) {
      rlang::abort(
        paste0(
          "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
          "missing artifacts for set_id: ",
          paste(missing_sets, collapse = ", "),
          "."
        )
      )
    }
    for (set_id in required_sets) {
      artifact <- artifacts[[as.character(set_id)]] %||% NULL
      if (is.null(artifact)) {
        rlang::abort(
          paste0(
            "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
            "missing artifact for set_id: ",
            as.integer(set_id),
            "."
          )
        )
      }
      .adaptive_phase_a_validate_imported_artifact(
        artifact = artifact,
        state = state,
        set_id = as.integer(set_id),
        controller = controller
      )
    }
    return(invisible(state))
  }

  blocked <- status_tbl
  if (nrow(blocked) > 0L) {
    blocked <- blocked[blocked$status != "ready", , drop = FALSE]
  }
  if (nrow(blocked) == 0L) {
    blocked_msg <- "phase_a_artifacts_missing"
  } else {
    blocked_msg <- paste0(
      "set ",
      blocked$set_id,
      " [",
      blocked$source,
      "]: ",
      blocked$validation_message
    )
    blocked_msg <- paste(blocked_msg, collapse = "; ")
  }

  rlang::abort(paste0(
    "Phase B linking cannot start until valid Phase A artifacts exist for hub and spoke sets. ",
    blocked_msg
  ))
}

.adaptive_phase_a_artifact_filename <- function(set_id) {
  paste0("set_", formatC(as.integer(set_id), width = 4, flag = "0"), ".rds")
}

.adaptive_write_phase_a_artifacts <- function(artifacts, artifact_dir) {
  if (is.null(artifacts) || !is.list(artifacts) || length(artifacts) == 0L) {
    return(invisible(NULL))
  }
  dir.create(artifact_dir, recursive = TRUE, showWarnings = FALSE)

  for (name in names(artifacts)) {
    art <- artifacts[[name]]
    set_id <- as.integer(art$set_id %||% suppressWarnings(as.integer(name)))
    if (is.na(set_id)) {
      next
    }
    path <- file.path(artifact_dir, .adaptive_phase_a_artifact_filename(set_id))
    write_log(art, path)
  }

  invisible(NULL)
}

.adaptive_read_phase_a_artifacts <- function(artifact_dir) {
  if (!dir.exists(artifact_dir)) {
    return(list())
  }
  files <- list.files(artifact_dir, pattern = "^set_\\d+\\.rds$", full.names = TRUE)
  if (length(files) == 0L) {
    return(list())
  }

  out <- list()
  for (path in files) {
    art <- read_log(path)
    set_id <- as.integer(art$set_id %||% NA_integer_)
    if (!is.na(set_id)) {
      out[[as.character(set_id)]] <- art
    }
  }

  out
}
