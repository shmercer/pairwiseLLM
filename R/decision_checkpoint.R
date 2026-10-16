# Immutable, single-writer checkpoints. A renamed record is the commit boundary.
.decision_checkpoint_error <- function(message = "Invalid or corrupt decision checkpoint; preserve the saved run and inspect it before retrying.") {
  rlang::abort(message, class = "pairwiseLLM_decision_checkpoint_error")
}

.decision_record_write <- function(value, path) {
  bytes <- serialize(value, NULL, version = 3)
  tmp <- tempfile(".decision-tmp-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    con <- file(tmp, "wb")
    tryCatch({
      saveRDS(list(sha256 = paste0(openssl::sha256(bytes)), data = bytes), con, version = 3)
      flush(con)
    }, finally = close(con))
    if (file.exists(path) || !file.rename(tmp, path)) .decision_checkpoint_error()
  }, error = function(e) .decision_checkpoint_error("Decision checkpoint write failed; execution stopped before further submissions."))
  paste0(openssl::sha256(bytes))
}

.decision_record_read <- function(path) {
  tryCatch({
    envelope <- readRDS(path)
    if (!is.list(envelope) || !is.raw(envelope$data) ||
        !identical(envelope$sha256, paste0(openssl::sha256(envelope$data)))) {
      .decision_checkpoint_error()
    }
    list(value = unserialize(envelope$data), hash = envelope$sha256)
  }, error = function(e) .decision_checkpoint_error())
}

.decision_checkpoint_lock <- function(save_path, timeout) {
  .decision_live_require("filelock", "Saved decision execution")
  parent <- dirname(save_path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) .decision_checkpoint_error()
  lock <- filelock::lock(paste0(save_path, ".lock"), exclusive = TRUE, timeout = timeout)
  if (is.null(lock)) {
    .decision_checkpoint_error("Another session holds the decision checkpoint lock; bounded lock wait expired. No requests were submitted.")
  }
  lock
}

.decision_state_new <- function() {
  list(status = "pending", attempts = list(), ambiguous_reissues = 0L,
       next_at = 0, row = NULL)
}

.decision_checkpoint_open <- function(save_path, manifest) {
  journal <- new.env(parent = emptyenv())
  journal$path <- if (is.null(save_path)) NULL else paste0(save_path, ".checkpoint")
  journal$sequence <- 0L
  journal$hash <- ""
  journal$cooldown <- 0
  journal$states <- new.env(hash = TRUE, parent = emptyenv())
  for (uid in names(manifest$workset)) assign(uid, .decision_state_new(), journal$states)
  if (is.null(save_path)) return(journal)
  path <- journal$path
  header <- file.path(path, "manifest.rds")
  if (!file.exists(header)) {
    if (file.exists(save_path) || (dir.exists(path) &&
        length(list.files(path, pattern = "\\.rds$")))) {
      .decision_checkpoint_error("Saved results lack a supported decision journal; use a separate save_path. CSV alone cannot establish safe completion.")
    }
    if (!dir.exists(path) && !dir.create(path)) .decision_checkpoint_error()
    journal$hash <- .decision_record_write(manifest, header)
  } else {
    saved <- .decision_record_read(header)
    if (!identical(saved$value, manifest)) {
      .decision_checkpoint_error("Decision workset, scientific identity, wire controls or persistent attempt budget differs; use the original configuration or a separate save_path.")
    }
    journal$hash <- saved$hash
  }
  files <- sort(list.files(path, pattern = "^[0-9]{12}\\.rds$", full.names = TRUE))
  other <- setdiff(list.files(path, pattern = "\\.rds$"), c("manifest.rds", basename(files)))
  if (length(other)) .decision_checkpoint_error()
  for (file in files) {
    record <- .decision_record_read(file)
    event <- record$value
    sequence <- journal$sequence + 1L
    invalid <- tryCatch({
      !identical(basename(file), sprintf("%012d.rds", sequence)) ||
        !identical(event$version, 1L) || !identical(event$sequence, sequence) ||
        !identical(event$previous, journal$hash) ||
        !is.character(event$uid) || length(event$uid) != 1L ||
        !event$uid %in% names(manifest$workset) ||
        !is.list(event$state) || !event$state$status %in%
        c("pending", "inflight", "retry", "completed", "failed", "ambiguous") ||
        !is.list(event$state$attempts) ||
        length(event$state$attempts) > manifest$max_attempts ||
        !event$state$ambiguous_reissues %in% 0:1 ||
        !is.finite(event$cooldown)
    }, error = function(e) TRUE)
    if (!identical(invalid, FALSE)) .decision_checkpoint_error()
    assign(event$uid, event$state, journal$states)
    journal$sequence <- sequence
    journal$hash <- record$hash
    journal$cooldown <- event$cooldown
  }
  journal
}

# Never persist request bodies, pair text columns, or credentials. Raw response
# capture is separately opt-in, including response-validation failures.
.decision_checkpoint_strip <- function(state, include_raw) {
  strip <- function(row) {
    if (!is.null(row) && !include_raw) row$raw_response <- NULL
    row
  }
  state$row <- strip(state$row)
  state$attempts <- lapply(state$attempts, function(attempt) {
    attempt$row <- strip(attempt$row)
    attempt
  })
  state
}

.decision_checkpoint_commit <- function(journal, uid, state, include_raw) {
  sequence <- journal$sequence + 1L
  if (!is.null(journal$path)) {
    event <- list(version = 1L, sequence = sequence, previous = journal$hash,
                  uid = uid, state = .decision_checkpoint_strip(state, include_raw),
                  cooldown = journal$cooldown)
    journal$hash <- .decision_record_write(event, file.path(journal$path, sprintf("%012d.rds", sequence)))
  }
  journal$sequence <- sequence
  assign(uid, state, journal$states)
  invisible(NULL)
}

.decision_csv_fields <- function() {
  c("ID1", "ID2", "pair_uid", "custom_id", "better_sample", "better_id", "winner_pos",
    "model", "prompt_tokens", "completion_tokens", "total_tokens",
    "prompt_cached_tokens", "prompt_cache_write_tokens", "reasoning_tokens",
    names(.decision_column_defaults(0L)), "object_type", "status_code", "error_message",
    "request_controls_json", "scientific_payload_sha256", "wire_body_sha256",
    "attempt_id", "attempt_count", "ambiguous_reissues", "possible_duplicate_charge",
    "elapsed_seconds", "received_at")
}

.decision_csv_write <- function(rows, path, append = FALSE) {
  if (is.null(path)) return(invisible(NULL))
  fields <- .decision_csv_fields()
  values <- stats::setNames(rep(list(character(length(rows))), length(fields)), fields)
  for (j in seq_along(fields)) {
    values[[j]] <- vapply(rows, function(row) {
      value <- row[[fields[j]]]
      if (is.null(value) || length(value) != 1L || is.na(value)) return(NA_character_)
      if (inherits(value, "POSIXct")) return(format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"))
      if (is.numeric(value)) return(sprintf("%.17g", value))
      as.character(value)
    }, character(1L))
  }
  frame <- as.data.frame(values, stringsAsFactors = FALSE)
  tryCatch(utils::write.table(frame, file = path, append = append, col.names = !append,
                             row.names = FALSE, sep = ",", quote = TRUE, na = "",
                             qmethod = "double", fileEncoding = "UTF-8"),
           error = function(e) .decision_checkpoint_error("Decision CSV export failed; committed RDS evidence is preserved. Resume to rebuild the export."))
  invisible(NULL)
}

.decision_csv_rebuild <- function(journal, uids, save_path) {
  if (is.null(save_path)) return(invisible(NULL))
  rows <- lapply(uids, function(uid) {
    state <- get(uid, journal$states)
    if (state$status == "completed") state$row else NULL
  })
  rows <- Filter(Negate(is.null), rows)
  tmp <- tempfile(".decision-export-", tmpdir = dirname(save_path))
  on.exit(unlink(tmp), add = TRUE)
  .decision_csv_write(rows, tmp)
  # CSV is a derived export: a crash during replacement is repaired from RDS.
  if (file.exists(save_path)) unlink(save_path)
  if (!file.rename(tmp, save_path)) .decision_checkpoint_error()
  invisible(NULL)
}
