#' Live decision execution, persistence and recovery contracts
#'
#' Collect and resume OpenAI Decisions and TypeSafe Jev comparisons through
#' [submit_llm_pairs()], [submit_openai_pairs_live()] or
#' [submit_typesafe_pairs_live()]. Each pair occurrence is one live request;
#' these endpoints do not use provider-managed Batch jobs.
#'
#' @section Execution controls:
#' The bulk wrappers accept `execution_control`, a uniquely named list. Unknown
#' names, invalid scalars and unsupported provider options fail before HTTP.
#' `parallel = FALSE` or `workers = 1` selects sequential execution.
#' `parallel = TRUE, workers > 1` uses optional \pkg{promises} and \pkg{later}
#' through httr2's experimental public asynchronous API. Missing packages cause
#' an informative error before requests; no packages are installed automatically.
#' Workers bound simultaneous in-flight requests, not R processes. One private
#' event loop and curl pool belong to each session and are cleaned up on exit.
#' The coordinator explicitly implements retries and admission; asynchronous
#' httr2 retry/throttle policies are not assumed to apply.
#'
#' Supported controls and defaults are:
#' \describe{
#'   \item{`requests_per_minute = NULL`}{Optional positive integer maximum
#'     transmission reservations in a rolling 60-second window.}
#'   \item{`tokens_per_minute = NULL`}{Optional positive integer maximum
#'     estimated token reservations in that window. Requires `token_estimates`.}
#'   \item{`token_estimates = NULL`}{Positive integer scalar or vector in input
#'     row order, including an output allowance. A reservation must fit the token
#'     limit. No tokenizer or missing provider usage is guessed. This bounds
#'     reservations, not actual usage when estimates are too small.}
#'   \item{`max_attempts = 3L`}{Persistent total send-reservation budget per
#'     occurrence, including initial, transient and ambiguous-recovery sends.
#'     It is fixed in the saved manifest; restarting does not replenish it.}
#'   \item{`timeout_seconds = 120`}{Positive finite timeout per transmission.}
#'   \item{`retry_ambiguous = TRUE`}{Allow at most one automatic reissue of an
#'     interrupted occurrence across all restarts, also within `max_attempts`.
#'     `FALSE` leaves ambiguous work unresolved. Toggling it never replenishes
#'     the consumed allowance.}
#'   \item{`lock_timeout_ms = 10000`}{Nonnegative finite milliseconds to wait
#'     for an exclusive saved-run lock before failing without provider calls.}
#' }
#' Rate limits are caller configuration, not assumed provider quotas. Recent
#' reservations, retry deadlines and shared provider cooldowns survive restart.
#' Limits apply to this saved run; other sessions sharing an account can consume
#' the same provider quota. Limits and timeout may change on resume without
#' changing scientific identity; prior reservations remain counted.
#'
#' @section Retries and ambiguous requests:
#' Bulk OpenAI retries HTTP 408, 409, 429 and all 5xx statuses; TypeSafe retries
#' 408, 429 and all 5xx, including 529. Recognized OpenAI insufficient-quota or
#' billing-limit errors are terminal. Timeouts receive bounded transient retries;
#' other transport failures and permanent HTTP errors remain unresolved.
#' Exponential delays start at 0.5 seconds, double up to 30 seconds and add
#' uniform jitter of up to 0.25 seconds. Valid `Retry-After` seconds or HTTP dates
#' are a minimum delay, with shared cooldowns for 429, 503 and 529. Invalid headers
#' use backoff. Single-pair retry defaults are unchanged.
#'
#' A send reservation is committed before transmission. An interruption before
#' transmission may therefore conservatively consume an attempt. Reservations
#' without a committed response have an unknown provider outcome. Recovery first
#' checks local committed evidence. The currently supported endpoint contracts
#' provide no documented remote result lookup/idempotency mechanism: reconciliation
#' is recorded as `unsupported`, without speculative retrieval requests.
#' One automatic ambiguous reissue is permitted by default, across all restarts.
#' Exhaustion leaves the occurrence in `failed_pairs` for explicit operator action;
#' an operator can deliberately start a separate run containing only unresolved
#' source pairs. Preserve the old journal rather than editing it or resetting
#' budgets. Timeout retries can also incur duplicate charges. Neither locking nor
#' checkpoints establish exactly-once provider execution.
#'
#' Malformed/refused decisions, incomplete probabilities, invalid confidence and
#' choice/probability inconsistencies are recorded without automatic retry and
#' never become completed evidence. Only normalized valid results enter modeling.
#'
#' @section Request identity:
#' All four input ID/text columns must be nonmissing character columns.
#' Supplied `pair_uid` values must be unique and nonempty. Otherwise deterministic
#' scientific-fingerprint IDs plus occurrence suffixes distinguish repeats and
#' reversed presentations. Supply explicit IDs when scheduled occurrences need
#' independent labels. IDs are never matched by row position alone.
#'
#' The saved workset must have the same occurrence membership, scientific payload
#' and provider wire controls. Matching occurrences can be reordered. Adding or
#' removing occurrences, or changing texts, orientation, trait, instructions,
#' choices, model/provider/endpoint or OpenAI cache controls requires a separate
#' save path. Omitted decision prompts select the exact `decision_cj_v1` wording;
#' custom instructions follow [decision_request]. Generative templates fail.
#'
#' Every current request is rebuilt and validated even when its result is already
#' saved. The shared scientific validator checks body, ordered evidence hashes,
#' approved question core, provenance and fingerprint. Serialization and attached
#' raw UTF-8 bytes are checked before every transmission, including retries.
#' `scientific_payload_sha256` hashes the serialized scientific body;
#' `wire_body_sha256` hashes the exact transmitted JSON body, including supported
#' cache controls. These digests exclude authentication headers and timeout.
#' Digests detect identity/integrity mismatches, not malicious modification with
#' recomputed checksums. Requested and provider-reported resolved models remain
#' separate. Moving aliases may change weights; saved results retain the originally
#' reported resolution, and no availability or alias-resolution calls are made.
#'
#' @section Persistence and output:
#' A decision `save_path` names a CSV export and requires optional \pkg{filelock}.
#' The dedicated `<save_path>.lock` is locked before checkpoints are read and
#' held through all asynchronous calls, retries and writes. The OS lock releases
#' on exit/process termination; its file is retained. Only one writer is admitted.
#'
#' `<save_path>.checkpoint/` is the authoritative lossless RDS journal. Its immutable
#' checksummed manifest and sequenced records are written through same-directory
#' temporary files, flushed/closed, then renamed to new final names. Each response
#' is validated and committed immediately, independently of other in-flight work.
#' Completed committed occurrences are never automatically submitted again.
#' Uncommitted temporary files are ignored; corrupt committed files, missing
#' interior records and unsupported schemas stop execution before HTTP. CSV alone
#' cannot establish completion and is not imported as a resume journal.
#'
#' CSV contains valid results with stable scalar columns, including all probability
#' and provenance columns. Numeric values use 17 significant digits to round-trip
#' R doubles with a correctly rounded decimal reader. Some readers (including
#' base numeric conversion on macOS ARM in qualification) can change the last
#' bit. For exact numeric CSV recovery, read columns as character and decode
#' probability cells with `jsonlite::parse_json()`; use RDS for exact native
#' recovery. Read IDs as character to preserve leading zeros. A missing or torn
#' CSV export is rebuilt from RDS on resume. Journal envelopes are RDS lists with
#' a SHA-256 and serialized raw `data`; `unserialize(readRDS(record)$data)` exposes
#' the versioned event and its state/attempt records. Treat these as trusted local
#' files, not a public cross-version interchange format.
#'
#' The returned list retains `results`, `failed_pairs` and `failed_attempts`.
#' Results preserve [decision_results] fields, exact parsed R double probabilities,
#' full `probabilities_json`, independent confidence, usage and model provenance.
#' Missing usage/resolution stays missing; no price or cost estimate is fabricated.
#' Additional diagnostics include payload digests, controls, local `attempt_id`,
#' `attempt_count`, `ambiguous_reissues`, `possible_duplicate_charge`,
#' `elapsed_seconds` since admission, and `recovery_status`. Failed attempts retain
#' unique local IDs, reason (`initial`, `transient_retry`, `ambiguous_recovery`),
#' timestamps, available original/new provider request IDs, provenance, and
#' reconciliation status. Provider IDs unknown at interruption remain missing.
#' Successful outcomes and failed attempts remain separate; history is not
#' re-added on every restart. Probabilities never become BT/adaptive weights,
#' fractional wins or extra comparisons.
#'
#' No durability is promised without `save_path`. Saved-run guarantees concern
#' process interruption on a filesystem supporting OS locks and atomic rename;
#' they are not power-loss/fsync guarantees. Network/cloud-synchronized storage
#' and cross-machine locking require filesystem guarantees beyond this package.
#'
#' @section Privacy:
#' Samples go to the selected provider. Normal messages and scalar checkpoints
#' omit API keys and sample text. Provenance retains complete custom instructions
#' and trait definitions, which can themselves be sensitive. CSV excludes input
#' text columns and arbitrary caller metadata. `include_raw = TRUE` explicitly
#' retains parsed provider responses in RDS and may contain sensitive content;
#' `FALSE` omits them from new persisted records, including failures. Existing
#' opted-in records are not erased. In-memory failed-response diagnostics follow
#' the existing parser contract and can retain raw responses. Increasing raw
#' retention on resume cannot recreate responses previously omitted and never
#' triggers resubmission. In-memory failure objects may contain raw responses
#' even with `include_raw = FALSE`. Explicitly saving or serializing the complete
#' returned object (for example with `saveRDS()`) can persist sensitive text.
#' Successful normalized results can also retain source text columns. The
#' default journal/export privacy policy does not sanitize caller-created stores.
#' Keep journals and returned diagnostics private.
#'
#' @examples
#' \dontrun{
#' # Requires provider credentials; incurs usage charges.
#' pairs <- data.frame(ID1 = c("A", "B"), ID2 = c("B", "A"),
#'                     text1 = c("Synthetic A.", "Synthetic B."),
#'                     text2 = c("Synthetic B.", "Synthetic A."),
#'                     pair_uid = c("forward", "reverse"))
#' result <- submit_llm_pairs(
#'   pairs, model = "jev-1.13.0", backend = "typesafe",
#'   trait_name = "Organization", trait_description = "Logical sequencing.",
#'   parallel = TRUE, workers = 4, save_path = "decisions.csv",
#'   execution_control = list(requests_per_minute = 60,
#'                            retry_ambiguous = FALSE)
#' )
#' # Repeat the same call to resume; preserve its .checkpoint directory.
#' result$failed_pairs
#' result$failed_attempts
#' }
#'
#' @seealso [submit_llm_pairs()], [decision_request], [decision_results]
#' @name decision_bulk
NULL

# Coordinator owns scheduling and durable budgets; transports perform one send.
.decision_optional_available <- function(package) requireNamespace(package, quietly = TRUE)

.decision_live_require <- function(package, purpose) {
  if (!.decision_optional_available(package)) {
    rlang::abort(paste0(purpose, " requires optional package '", package,
                        "'; install.packages(\"", package, "\") before retrying."))
  }
}

.decision_execution_control <- function(control, n) {
  defaults <- list(requests_per_minute = NULL, tokens_per_minute = NULL,
                   token_estimates = NULL, max_attempts = 3L,
                   timeout_seconds = 120, retry_ambiguous = TRUE, lock_timeout_ms = 10000)
  if (!is.list(control) || (length(control) &&
      (is.null(names(control)) || anyNA(names(control)) || any(!nzchar(names(control))) ||
       anyDuplicated(names(control)) || length(setdiff(names(control), names(defaults)))))) {
    rlang::abort("`execution_control` must be a uniquely named list of documented decision controls.")
  }
  for (name in names(control)) defaults[name] <- control[name]
  positive <- function(x, whole = FALSE) {
    is.numeric(x) && length(x) == 1L && is.null(dim(x)) &&
      is.finite(x) && x > 0 && (!whole || (x == floor(x) && x <= .Machine$integer.max))
  }
  for (name in c("requests_per_minute", "tokens_per_minute")) {
    if (!is.null(defaults[[name]]) && !positive(defaults[[name]], TRUE)) {
      rlang::abort(paste0("`", name, "` must be NULL or a positive integer."))
    }
  }
  if (!positive(defaults$max_attempts, TRUE) || !positive(defaults$timeout_seconds) ||
      !is.numeric(defaults$lock_timeout_ms) || length(defaults$lock_timeout_ms) != 1L ||
      !is.finite(defaults$lock_timeout_ms) || defaults$lock_timeout_ms < 0) {
    rlang::abort("Invalid decision attempt budget, timeout_seconds or lock_timeout_ms.")
  }
  .decision_live_flag(defaults$retry_ambiguous, "retry_ambiguous")
  estimates <- defaults$token_estimates
  if (!is.null(defaults$tokens_per_minute) && is.null(estimates)) {
    rlang::abort("Token rate limiting requires explicit `token_estimates`, including an output allowance.")
  }
  if (!is.null(estimates) &&
      (!is.numeric(estimates) || !is.null(dim(estimates)) ||
       !length(estimates) %in% unique(c(1L, n)) || any(!is.finite(estimates)) ||
       any(estimates <= 0 | estimates != floor(estimates)) ||
       (!is.null(defaults$tokens_per_minute) && any(estimates > defaults$tokens_per_minute)))) {
    rlang::abort("`token_estimates` must be positive integer reservations, one or per input row, within tokens_per_minute.")
  }
  defaults$token_estimates <- if (is.null(estimates)) rep(0, n) else rep(estimates, length.out = n)
  defaults$max_attempts <- as.integer(defaults$max_attempts)
  defaults
}

.decision_live_now <- function() as.numeric(Sys.time())
.decision_live_wait <- function(seconds) Sys.sleep(min(1, max(0, seconds)))
.decision_async_wait <- function(seconds) later::run_now(timeoutSecs = min(1, max(0, seconds)))
.decision_retry_delay <- function(attempt) min(30, 0.5 * 2^(attempt - 1L)) + stats::runif(1L, 0, 0.25)

# Small boundary around httr2's experimental public API; no retry or throttling
# policies are assumed to apply to this transport.
.decision_async_submit <- function(req, spec, pool, success, failure) {
  .decision_live_validate(spec, req)
  promise <- httr2::req_perform_promise(req, pool = pool, verbosity = 0)
  promises::then(promise, onFulfilled = success, onRejected = failure)
}

.decision_sync_submit <- function(req, spec) {
  .decision_live_validate(spec, req)
  .pairwiseLLM_req_perform(req, verbosity = 0)
}

.decision_spec_identity <- function(spec) {
  list(pair_uid = spec$request$pair_uid,
       request_fingerprint = spec$request$request_fingerprint,
       scientific_payload_sha256 = .decision_sha256(.decision_json(spec$request$body)),
       wire_body_sha256 = .decision_sha256(rawToChar(spec$payload)),
       request_controls_json = .decision_json(spec$controls))
}

.decision_transport_row <- function(spec, message, status = NA_integer_, request_id = NULL) {
  row <- .parse_decision_response(NULL, spec$request, provider_request_id = request_id)
  row$raw_response <- NULL
  row$decision_error_code <- if (grepl("timed out", message, fixed = TRUE)) "timeout" else "http_error"
  row$decision_error_detail <- message
  row$object_type <- "decision"
  row$status_code <- status
  row$error_message <- message
  row$thoughts <- row$content <- NA_character_
  row$request_controls_json <- .decision_json(spec$controls)
  row
}

.decision_is_timeout <- function(error) {
  if (inherits(error, c("httr2_timeout", "curl_error_timeout", "curl_error_operation_timedout"))) return(TRUE)
  if (is.list(error$parent)) return(.decision_is_timeout(error$parent))
  FALSE
}

.decision_bulk_transient <- function(status, provider, resp = NULL) {
  if (identical(status, 429L) && !is.null(resp) && provider == "openai") {
    code <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE)$error$code,
                     error = function(e) NULL)
    if (is.character(code) && length(code) == 1L &&
        code %in% c("insufficient_quota", "billing_hard_limit_reached")) return(FALSE)
  }
  status %in% c(408L, 429L, 500:599, if (provider == "openai") 409L)
}

# Only admission reservations in the recent window are needed in memory.
.decision_admit_at <- function(ledger, now, tokens, control) {
  at <- now
  if (!is.null(control$requests_per_minute) && nrow(ledger) >= control$requests_per_minute) {
    at <- max(at, ledger$at[nrow(ledger) - control$requests_per_minute + 1L] + 60)
  }
  if (!is.null(control$tokens_per_minute) && sum(ledger$tokens) + tokens > control$tokens_per_minute) {
    needed <- sum(ledger$tokens) + tokens - control$tokens_per_minute
    at <- max(at, ledger$at[which(cumsum(ledger$tokens) >= needed)[1L]] + 60)
  }
  at
}

.submit_decision_pairs <- function(pairs, model, trait_name, trait_description,
                                    prompt_template, provider, api_key, verbose,
                                    status_every, progress, include_raw, save_path,
                                    parallel, workers, execution_control, dots) {
  pairs <- tibble::as_tibble(pairs)
  required <- c("ID1", "text1", "ID2", "text2")
  if (!all(required %in% names(pairs))) rlang::abort("`pairs` must contain ID1, text1, ID2, text2.")
  for (name in required) {
    if (!is.character(pairs[[name]]) || anyNA(pairs[[name]])) {
      rlang::abort("Decision pair IDs and texts must be nonmissing character columns.")
    }
  }
  for (name in c("parallel", "verbose", "progress", "include_raw")) .decision_live_flag(get(name), name)
  if (!is.numeric(workers) || length(workers) != 1L || !is.finite(workers) ||
      workers < 1 || workers != floor(workers) || workers > .Machine$integer.max ||
      !is.numeric(status_every) || length(status_every) != 1L || !is.finite(status_every) ||
      status_every < 1 || status_every != floor(status_every)) {
    rlang::abort("`workers` and `status_every` must be positive integers.")
  }
  if (!is.null(save_path) && !.decision_string(save_path)) rlang::abort("`save_path` must be NULL or a nonempty path.")
  if ("pair_uid" %in% names(dots)) rlang::abort("Supply bulk occurrence IDs in `pairs$pair_uid`, not `...`.")
  n <- nrow(pairs)
  control <- .decision_execution_control(execution_control, n)
  concurrent <- parallel && workers > 1L
  if (concurrent) {
    .decision_live_require("promises", "Concurrent decision execution")
    .decision_live_require("later", "Concurrent decision execution")
  }
  make_spec <- function(i, uid) {
    .decision_live_spec(pairs$ID1[i], pairs$text1[i], pairs$ID2[i], pairs$text2[i],
                        model, trait_name, trait_description, prompt_template,
                        provider, include_raw, c(dots, list(pair_uid = uid)))
  }
  provided <- "pair_uid" %in% names(pairs)
  if (provided && (!is.character(pairs$pair_uid) || anyNA(pairs$pair_uid) ||
      any(!nzchar(trimws(pairs$pair_uid))) || anyDuplicated(pairs$pair_uid))) {
    rlang::abort("Decision execution requires unique nonempty `pairs$pair_uid` values.")
  }
  identities <- vector("list", n)
  occurrences <- new.env(hash = TRUE, parent = emptyenv())
  uids <- character(n)
  # Validate all work, including completed work, before opening credentials/HTTP.
  # Retain descriptors only; sample-bearing payloads are bounded by concurrency.
  for (i in seq_len(n)) {
    spec <- make_spec(i, if (provided) pairs$pair_uid[i] else "preflight")
    if (!provided) {
      key <- spec$request$request_fingerprint
      ordinal <- if (exists(key, occurrences, inherits = FALSE)) get(key, occurrences) + 1L else 1L
      assign(key, ordinal, occurrences)
      spec$request$pair_uid <- paste0("decision-", sub("decision-v1-sha256-", "", key, fixed = TRUE), "-", ordinal)
    }
    uids[i] <- spec$request$pair_uid
    identities[[i]] <- .decision_spec_identity(spec)
  }
  if (!n) {
    # Validate provider options/model even for an empty workset, without IO.
    .decision_live_spec("empty-A", "", "empty-B", "", model, trait_name,
                        trait_description, prompt_template, provider, include_raw, dots)
  }
  pairs$pair_uid <- uids
  names(identities) <- uids
  manifest <- list(version = 1L, provider = provider, model = model,
                   trait_name = trait_name, trait_description = trait_description,
                   decision_template = dots$decision_template,
                   workset = identities[order(uids)], max_attempts = control$max_attempts)
  lock <- if (!is.null(save_path)) .decision_checkpoint_lock(save_path, control$lock_timeout_ms) else NULL
  if (!is.null(lock)) on.exit(filelock::unlock(lock), add = TRUE)
  journal <- .decision_checkpoint_open(save_path, manifest)
  for (i in seq_len(n)) {
    state <- get(uids[i], journal$states)
    rows <- c(list(state$row), lapply(state$attempts, `[[`, "row"))
    for (row in Filter(Negate(is.null), rows)) {
      if (!all(vapply(names(identities[[i]]), function(name) identical(row[[name]][1L], identities[[i]][[name]]), logical(1L)))) {
        .decision_checkpoint_error()
      }
    }
    if (state$status == "completed") {
      checked <- .decision_prepare_results(state$row, pairs[i, ], provider, model)
      if (nrow(checked) != 1L || !is.na(checked$decision_error_code)) .decision_checkpoint_error()
    }
  }
  .decision_csv_rebuild(journal, uids, save_path)
  limited <- !is.null(control$requests_per_minute) || !is.null(control$tokens_per_minute)
  reservations <- list()
  for (i in seq_len(n)) {
    state <- get(uids[i], journal$states)
    for (attempt in state$attempts) {
      if (limited) reservations[[length(reservations) + 1L]] <- data.frame(at = attempt$started_at, tokens = attempt$estimated_tokens)
    }
    if (state$status == "inflight") {
      spec <- make_spec(i, uids[i])
      k <- length(state$attempts)
      state$status <- "ambiguous"
      state$attempts[[k]]$outcome <- "ambiguous"
      state$attempts[[k]]$reconciliation_status <- "unsupported"
      state$row <- .decision_transport_row(spec, "Interrupted in-flight request; provider outcome is unknown.")
      for (name in names(identities[[i]])) state$row[[name]] <- identities[[i]][[name]]
      state$row$attempt_id <- state$attempts[[k]]$id
      state$row$attempt_count <- as.integer(k)
      state$row$ambiguous_reissues <- state$ambiguous_reissues
      state$row$possible_duplicate_charge <- TRUE
      state$row$elapsed_seconds <- NA_real_
      state$row$received_at <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
      state$attempts[[k]]$row <- state$row
      .decision_checkpoint_commit(journal, uids[i], state, include_raw)
    }
  }
  ledger <- if (length(reservations)) do.call(rbind, reservations) else data.frame(at = numeric(), tokens = numeric())
  ledger <- ledger[order(ledger$at), , drop = FALSE]
  pending <- integer()
  delayed <- data.frame(i = integer(), at = numeric())
  for (i in seq_len(n)) {
    state <- get(uids[i], journal$states)
    eligible <- length(state$attempts) < control$max_attempts
    if (state$status == "ambiguous") eligible <- eligible && control$retry_ambiguous && state$ambiguous_reissues < 1L
    if (eligible && state$status %in% c("pending", "ambiguous")) pending <- c(pending, i)
    if (eligible && state$status == "retry") delayed <- rbind(delayed, data.frame(i = i, at = state$next_at))
  }
  cursor <- 1L
  finished <- 0L
  active <- 0L
  fatal <- NULL
  alive <- TRUE
  promises_active <- new.env(hash = TRUE, parent = emptyenv())
  pool <- NULL
  pb <- if (progress && n) utils::txtProgressBar(min = 0, max = n, style = 3) else NULL
  if (!is.null(pb)) on.exit(close(pb), add = TRUE)
  finish <- function(i, spec, response = NULL, error = NULL, attempt_id = NULL) {
    if (!alive) return(invisible(NULL))
    uid <- uids[i]
    state <- get(uid, journal$states)
    k <- length(state$attempts)
    if (!is.null(attempt_id) && (state$status != "inflight" ||
        !identical(state$attempts[[k]]$id, attempt_id))) return(invisible(NULL))
    now <- .decision_live_now()
    retry <- FALSE
    delay <- 0
    status <- if (is.null(response)) NA_integer_ else httr2::resp_status(response)
    if (!is.null(error)) {
      timeout <- .decision_is_timeout(error)
      row <- .decision_transport_row(spec, if (timeout) "Decision request timed out." else "Decision transport failed.")
      outcome <- if (timeout) "timeout" else "http_error"
      retry <- timeout
    } else if (status >= 400L) {
      row <- .decision_transport_row(spec, paste0("HTTP ", status), status,
                                     httr2::resp_header(response, "x-request-id"))
      if (include_raw) row$raw_response <- list(.decision_live_body(response, provider))
      outcome <- "http_error"
      retry <- .decision_bulk_transient(status, provider, response)
      if (retry) delay <- .retry_after_seconds(response, as.POSIXct(now, origin = "1970-01-01", tz = "UTC"))
    } else {
      row <- .decision_live_response(response, spec, include_raw)
      row <- .decision_prepare_results(row, pairs[i, ], provider, model)
      outcome <- if (is.na(row$decision_error_code)) "valid" else "decision_invalid"
    }
    if (retry) {
      delay <- max(if (is.finite(delay)) delay else 0, .decision_retry_delay(k))
      if (!is.na(status) && status %in% c(429L, 503L, 529L)) journal$cooldown <- max(journal$cooldown, now + delay)
    }
    for (name in names(identities[[i]])) row[[name]] <- identities[[i]][[name]]
    row$attempt_id <- state$attempts[[k]]$id
    row$attempt_count <- as.integer(k)
    row$ambiguous_reissues <- state$ambiguous_reissues
    previous <- vapply(state$attempts, function(a) a$outcome %||% "", character(1L))
    row$possible_duplicate_charge <- any(previous %in% c("ambiguous", "timeout")) || outcome == "timeout"
    row$elapsed_seconds <- max(0, now - state$attempts[[k]]$started_at)
    row$received_at <- as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
    state$row <- row
    state$attempts[[k]]$outcome <- outcome
    state$attempts[[k]]$completed_at <- now
    state$attempts[[k]]$row <- row
    state$status <- if (outcome == "valid") "completed" else if (retry && k < control$max_attempts) "retry" else "failed"
    state$next_at <- now + delay
    .decision_checkpoint_commit(journal, uid, state, include_raw)
    if (state$status == "completed") .decision_csv_write(list(row), save_path, append = TRUE)
    if (state$status == "retry") delayed <<- rbind(delayed, data.frame(i = i, at = state$next_at))
    active <<- active - 1L
    if (exists(uid, promises_active, inherits = FALSE)) rm(list = uid, envir = promises_active)
    finished <<- finished + 1L
    if (!is.null(pb)) utils::setTxtProgressBar(pb, min(n, finished))
    if (verbose && finished %% status_every == 0L) message(sprintf("Decision attempts committed: %d; active: %d.", finished, active))
    invisible(NULL)
  }
  run <- function() {
    if (concurrent) pool <<- curl::new_pool(total_con = workers, host_con = workers, max_streams = workers)
    on.exit({
      alive <<- FALSE
      if (!is.null(pool)) for (handle in curl::multi_list(pool)) curl::multi_cancel(handle)
    }, add = TRUE)
    repeat {
      if (!is.null(fatal)) stop(fatal)
      now <- .decision_live_now()
      ledger <<- ledger[ledger$at > now - 60, , drop = FALSE]
      due <- which(delayed$at <= now)
      i <- if (length(due)) delayed$i[due[which.min(delayed$at[due])]] else {
        if (cursor <= length(pending)) pending[cursor] else NA_integer_
      }
      wait <- if (nrow(delayed)) max(0, min(delayed$at) - now) else Inf
      if (!is.na(i) && active < if (concurrent) workers else 1L) {
        admitted <- max(journal$cooldown, .decision_admit_at(ledger, now, control$token_estimates[i], control))
        wait <- max(0, admitted - now)
        if (wait <= 0) {
          if (length(due)) delayed <<- delayed[delayed$i != i, , drop = FALSE] else cursor <<- cursor + 1L
          spec <- make_spec(i, uids[i])
          if (!identical(.decision_spec_identity(spec), identities[[i]])) .decision_request_error()
          req <- .decision_live_http(spec, api_key) |>
            httr2::req_timeout(control$timeout_seconds) |>
            httr2::req_error(is_error = function(resp) FALSE)
          state <- get(uids[i], journal$states)
          reason <- if (state$status == "ambiguous") "ambiguous_recovery" else if (length(state$attempts)) "transient_retry" else "initial"
          if (reason == "ambiguous_recovery") state$ambiguous_reissues <- state$ambiguous_reissues + 1L
          k <- length(state$attempts) + 1L
          state$attempts[[k]] <- list(id = paste0(uids[i], "/attempt-", k), reason = reason,
                                     started_at = now, estimated_tokens = control$token_estimates[i],
                                     timeout_seconds = control$timeout_seconds, outcome = NULL,
                                     reconciliation_status = if (reason == "ambiguous_recovery") "unsupported" else NA_character_)
          state$status <- "inflight"
          .decision_checkpoint_commit(journal, uids[i], state, include_raw)
          if (limited) ledger <<- rbind(ledger, data.frame(at = now, tokens = control$token_estimates[i]))
          active <<- active + 1L
          if (concurrent) {
            local({
              index <- i; current <- spec; reserved <- state$attempts[[k]]$id
              safe <- function(response = NULL, error = NULL) {
                tryCatch(finish(index, current, response, error, attempt_id = reserved), error = function(e) { fatal <<- e })
                NULL
              }
              promise <- .decision_async_submit(req, spec, pool,
                                                success = function(resp) safe(response = resp),
                                                failure = function(err) safe(error = err))
              assign(uids[i], promise, promises_active)
            })
          } else {
            response <- tryCatch(.decision_sync_submit(req, spec), error = function(e) e)
            if (inherits(response, "pairwiseLLM_decision_request_error")) stop(response)
            if (inherits(response, "error")) finish(i, spec, error = response) else finish(i, spec, response)
          }
          next
        }
      }
      if (!active && cursor > length(pending) && !nrow(delayed)) break
      if (active >= if (concurrent) workers else 1L) wait <- 1
      if (concurrent) .decision_async_wait(if (active) min(wait, 1) else wait) else .decision_live_wait(wait)
    }
  }
  if (concurrent) later::with_temp_loop(run()) else run()
  .decision_bulk_output(journal, pairs, provider, model, include_raw, make_spec)
}

.decision_bulk_output <- function(journal, pairs, provider, model, include_raw, make_spec) {
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    state <- get(pairs$pair_uid[i], journal$states)
    row <- state$row
    if (is.null(row)) row <- .decision_transport_row(make_spec(i, pairs$pair_uid[i]), "No completed provider response.")
    row$recovery_status <- state$status
    row
  })
  if (length(rows)) raw <- dplyr::bind_rows(rows) else {
    raw <- tibble::as_tibble(c(list(ID1 = character(), ID2 = character(), pair_uid = character(),
                                   custom_id = character(), better_id = character(), model = character()),
                              .decision_column_defaults(0L)))
  }
  normalized <- .normalize_llm_results(raw, pairs, provider, model, include_raw)
  extra <- c("recovery_status", "attempt_id", "attempt_count", "ambiguous_reissues",
             "possible_duplicate_charge", "elapsed_seconds", "scientific_payload_sha256",
             "wire_body_sha256", "request_controls_json")
  for (name in intersect(extra, names(raw))) {
    normalized$failed_pairs[[name]] <- raw[[name]][match(normalized$failed_pairs$pair_uid, raw$pair_uid)]
  }
  if (nrow(normalized$results)) {
    normalized$results$received_at <- raw$received_at[match(normalized$results$pair_uid, raw$pair_uid)]
  }
  failures <- list()
  for (i in seq_len(nrow(pairs))) {
    state <- get(pairs$pair_uid[i], journal$states)
    for (attempt in state$attempts) {
      if (identical(attempt$outcome, "valid") || is.null(attempt$row)) next
      failure <- .normalize_llm_results(attempt$row, pairs[i, ], provider, model, include_raw)$failed_attempts
      for (name in intersect(extra, names(attempt$row))) failure[[name]] <- attempt$row[[name]]
      failure$attempt_id <- attempt$id
      failure$attempt_reason <- attempt$reason
      failure$reconciliation_status <- attempt$reconciliation_status
      failure$attempted_at <- as.POSIXct(attempt$started_at, origin = "1970-01-01", tz = "UTC")
      failures[[length(failures) + 1L]] <- failure
    }
  }
  normalized$failed_attempts <- if (length(failures)) dplyr::bind_rows(failures) else normalized$failed_attempts[0, ]
  normalized[c("results", "failed_pairs", "failed_attempts")]
}
