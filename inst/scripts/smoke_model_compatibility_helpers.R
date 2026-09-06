validate_smoke_matrix <- function(smoke_matrix) {
  required <- c(
    "test_id", "backend", "provider", "model_id", "mode", "endpoint",
    "request_profile", "reasoning_mode", "env_var", "catalog_status",
    "catalog_checked_on", "catalog_url"
  )
  missing <- setdiff(required, names(smoke_matrix))
  if (length(missing) > 0L) {
    stop("Smoke matrix is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(smoke_matrix$test_id)) {
    stop("Smoke matrix `test_id` values must be unique.", call. = FALSE)
  }
  if (!all(smoke_matrix$mode %in% c("live", "batch"))) {
    stop("Smoke matrix modes must be live or batch.", call. = FALSE)
  }
  if (!all(smoke_matrix$backend %in%
    c("openai", "anthropic", "gemini", "vertex", "together"))) {
    stop("Smoke matrix contains an unknown backend.", call. = FALSE)
  }
  invisible(smoke_matrix)
}

new_batch_state <- function(status = "pending", remote_id = NA_character_,
                            remote_status = NA_character_, error = NA_character_) {
  list(
    status = status,
    remote_id = remote_id,
    remote_status = remote_status,
    status_code = NA_integer_,
    parsed_winner = FALSE,
    error = error
  )
}

orchestrate_smoke_batches <- function(
  rows,
  states,
  submit_one,
  poll_one,
  collect_one,
  persist = function(states) invisible(NULL),
  pace = function() invisible(NULL),
  sleep = Sys.sleep,
  now = Sys.time,
  poll_interval_seconds = 10,
  timeout_seconds = 3600
) {
  ids <- rows$test_id
  if (is.null(names(states)) || !setequal(names(states), ids)) {
    stop("Batch states must be named by every selected `test_id`.", call. = FALSE)
  }

  for (id in ids) {
    if (!identical(states[[id]]$status, "pending")) next
    row <- rows[rows$test_id == id, , drop = FALSE]
    submitted <- tryCatch(submit_one(row), error = identity)
    if (inherits(submitted, "error")) {
      states[[id]]$status <- "failed-submission"
      states[[id]]$error <- conditionMessage(submitted)
    } else {
      states[[id]]$status <- "submitted"
      states[[id]]$remote_id <- submitted$remote_id
      states[[id]]$remote_status <- submitted$remote_status
    }
    persist(states)
    pace()
  }

  for (id in ids) {
    if (identical(states[[id]]$status, "timed-out") &&
      !is.na(states[[id]]$remote_id) && nzchar(states[[id]]$remote_id)) {
      states[[id]]$status <- "running"
    }
  }

  started <- now()
  repeat {
    active <- ids[vapply(states[ids], function(x) {
      x$status %in% c("submitted", "running", "poll-error")
    }, logical(1))]
    if (length(active) == 0L) break

    for (id in active) {
      row <- rows[rows$test_id == id, , drop = FALSE]
      polled <- tryCatch(poll_one(row, states[[id]]), error = identity)
      if (inherits(polled, "error")) {
        states[[id]]$status <- "poll-error"
        states[[id]]$error <- conditionMessage(polled)
      } else {
        states[[id]]$remote_status <- polled$remote_status
        states[[id]]$error <- NA_character_
        if (isTRUE(polled$terminal)) {
          states[[id]]$status <- if (isTRUE(polled$successful)) {
            "remote-completed"
          } else {
            "failed-remote"
          }
          if (!isTRUE(polled$successful)) states[[id]]$error <- polled$error
        } else {
          states[[id]]$status <- "running"
        }
      }
      persist(states)
      pace()
    }

    remaining <- ids[vapply(states[ids], function(x) {
      x$status %in% c("submitted", "running", "poll-error")
    }, logical(1))]
    if (length(remaining) == 0L) break
    elapsed <- as.numeric(difftime(now(), started, units = "secs"))
    if (!is.infinite(timeout_seconds) && elapsed >= timeout_seconds) {
      for (id in remaining) states[[id]]$status <- "timed-out"
      persist(states)
      break
    }
    sleep(poll_interval_seconds)
  }

  completed <- ids[vapply(states[ids], function(x) {
    identical(x$status, "remote-completed")
  }, logical(1))]
  for (id in completed) {
    row <- rows[rows$test_id == id, , drop = FALSE]
    collected <- tryCatch(collect_one(row, states[[id]]), error = identity)
    if (inherits(collected, "error")) {
      states[[id]]$status <- "failed-download"
      states[[id]]$error <- conditionMessage(collected)
    } else {
      states[[id]]$status <- collected$status
      states[[id]]$status_code <- collected$status_code
      states[[id]]$parsed_winner <- collected$parsed_winner
      states[[id]]$error <- collected$error
    }
    persist(states)
    pace()
  }

  states
}
