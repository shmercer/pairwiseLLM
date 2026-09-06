validate_promotion_inputs <- function(results, smoke_matrix, allow_incomplete = FALSE) {
  required_results <- c(
    "test_id", "backend", "provider", "model_id", "mode", "endpoint",
    "request_profile", "reasoning_mode", "env_var", "package_version", "test_date",
    "status", "catalog_status", "catalog_checked_on", "catalog_url"
  )
  missing_results <- setdiff(required_results, names(results))
  if (length(missing_results) > 0L) {
    stop(
      "Smoke results are missing columns: ", paste(missing_results, collapse = ", "),
      call. = FALSE
    )
  }
  if (anyDuplicated(results$test_id)) {
    stop("Smoke results must contain one row per `test_id`.", call. = FALSE)
  }
  if (!setequal(results$test_id, smoke_matrix$test_id)) {
    stop("Smoke results must cover the complete current smoke matrix.", call. = FALSE)
  }

  matched <- smoke_matrix[match(results$test_id, smoke_matrix$test_id), , drop = FALSE]
  identity_columns <- c(
    "backend", "provider", "model_id", "mode", "endpoint", "request_profile",
    "reasoning_mode", "env_var", "catalog_status", "catalog_checked_on", "catalog_url"
  )
  mismatched <- identity_columns[vapply(identity_columns, function(column) {
    !identical(as.character(results[[column]]), as.character(matched[[column]]))
  }, logical(1))]
  if (length(mismatched) > 0L) {
    stop(
      "Smoke results do not match the current matrix columns: ",
      paste(mismatched, collapse = ", "), call. = FALSE
    )
  }

  test_dates <- unique(results$test_date)
  if (length(test_dates) != 1L || !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", test_dates)) {
    stop("Smoke results must contain one ISO test date.", call. = FALSE)
  }
  package_versions <- unique(results$package_version)
  if (length(package_versions) != 1L ||
    !grepl("^[0-9]+([.][0-9]+)+$", package_versions)) {
    stop("Smoke results must contain one dotted-numeric package version.", call. = FALSE)
  }

  incomplete_statuses <- c(
    "pending", "submitted", "running", "poll-error", "timed-out",
    "remote-completed", "skipped-no-key"
  )
  incomplete <- results$status %in% incomplete_statuses
  if (any(incomplete) && !allow_incomplete) {
    counts <- sort(table(results$status[incomplete]), decreasing = TRUE)
    detail <- paste(names(counts), as.integer(counts), sep = "=", collapse = ", ")
    stop(
      "Smoke run is incomplete (", detail, "). Resume it before promotion or use ",
      "--allow-incomplete=true to archive a partial run.", call. = FALSE
    )
  }
  invisible(results)
}

mode_result_summary <- function(rows, mode) {
  selected <- rows$status[rows$mode == mode]
  if (length(selected) == 0L) return("not implemented")
  paste(sort(unique(selected)), collapse = "/")
}

build_compatibility_registry <- function(results, existing_registry = NULL) {
  config_columns <- c("backend", "provider", "model_id", "endpoint", "reasoning_mode")
  configs <- unique(results[config_columns])
  generated <- vector("list", nrow(configs))

  for (i in seq_len(nrow(configs))) {
    config <- configs[i, , drop = FALSE]
    matches <- rep(TRUE, nrow(results))
    for (column in config_columns) {
      matches <- matches & as.character(results[[column]]) == as.character(config[[column]])
    }
    rows <- results[matches, , drop = FALSE]
    live_tested <- any(rows$mode == "live" & rows$status == "passed")
    batch_tested <- any(rows$mode == "batch" & rows$status == "passed")
    tested <- live_tested || batch_tested
    catalog_status <- paste(sort(unique(rows$catalog_status)), collapse = "/")
    notes <- paste0(
      "Compatibility smoke run: live ", mode_result_summary(rows, "live"),
      "; batch ", mode_result_summary(rows, "batch"),
      ". Provider catalog status at check: ", catalog_status, "."
    )
    generated[[i]] <- data.frame(
      backend = config$backend,
      provider = config$provider,
      model_id = config$model_id,
      endpoint = config$endpoint,
      live_tested = live_tested,
      batch_tested = batch_tested,
      reasoning_mode = config$reasoning_mode,
      package_version = unique(rows$package_version)[[1L]],
      test_date = unique(rows$test_date)[[1L]],
      status = if (tested) "tested-current" else "unverified",
      notes = notes,
      official_catalog_url = unique(rows$catalog_url)[[1L]],
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }

  registry <- do.call(rbind, generated)
  if (!is.null(existing_registry) && nrow(existing_registry) > 0L) {
    historical <- existing_registry[
      existing_registry$status %in% c("tested-legacy", "retired"), , drop = FALSE
    ]
    registry <- rbind(registry, historical[names(registry)])
  }
  rownames(registry) <- NULL
  registry
}

update_dated_evidence_references <- function(paths, test_date) {
  paths <- paths[file.exists(paths)]
  for (path in paths) {
    contents <- readLines(path, warn = FALSE)
    contents <- gsub(
      "model_smoke_results_[0-9]{4}-[0-9]{2}-[0-9]{2}[.]csv",
      paste0("model_smoke_results_", test_date, ".csv"), contents
    )
    contents <- gsub(
      "model_batch_smoke_results_[0-9]{4}-[0-9]{2}-[0-9]{2}[.]csv",
      paste0("model_batch_smoke_results_", test_date, ".csv"), contents
    )
    writeLines(contents, path, useBytes = TRUE)
  }
  invisible(paths)
}
