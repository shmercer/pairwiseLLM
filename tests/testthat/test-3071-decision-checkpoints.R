test_that("saved decisions round-trip doubles, identifiers and raw capture without resubmission", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  for (provider in c("openai", "typesafe")) {
    td <- withr::local_tempdir()
    path <- file.path(td, "decisions.csv")
    calls <- 0L
    probabilities <- c(.12345678901234566, .8765432109876543)
    response <- decision_333_response(provider, p = probabilities)
    local_mocked_bindings(.decision_sync_submit = function(req, spec) {
      calls <<- calls + 1L
      decision_bulk_response(provider, body = response)
    }, .package = "pairwiseLLM")
    pairs <- decision_bulk_pairs(2)
    pairs$ID1 <- c("001", "002")
    args <- decision_bulk_args(provider, pairs = pairs, save_path = path, include_raw = TRUE)
    out <- do.call(submit_llm_pairs, args)
    csv <- utils::read.csv(path, colClasses = "character", check.names = FALSE)
    expect_identical(csv$ID1, pairs$ID1)
    expect_identical(unname(vapply(csv$p_sample_1, jsonlite::parse_json, numeric(1L))), rep(probabilities[1], 2))
    expect_identical(unname(vapply(csv$p_sample_2, jsonlite::parse_json, numeric(1L))), rep(probabilities[2], 2))
    expect_false(any(c("text1", "text2", "raw_response") %in% names(csv)))
    events <- decision_bulk_events(path)
    last <- events[[length(events)]]$state
    expect_identical(last$row$p_sample_1, probabilities[1])
    expect_identical(last$row$raw_response[[1]], response)
    again <- do.call(submit_llm_pairs, args)
    expect_identical(calls, 2L)
    expect_identical(again, out)
    expect_equal(length(decision_bulk_events(path)), 4L)
    # The journal is authoritative even when the CSV is missing or torn.
    writeLines('"torn export', path)
    expect_identical(do.call(submit_llm_pairs, args), out)
    unlink(path)
    expect_identical(do.call(submit_llm_pairs, args), out)
    expect_identical(calls, 2L)
    expect_equal(nrow(utils::read.csv(path)), 2L)
    expect_true(file.exists(paste0(path, ".lock")))
  }
})

test_that("mixed failures stay unresolved, history is unique and raw persistence is opt-in", {
  skip_if_not_installed("filelock")
  clock <- decision_bulk_clock()
  td <- withr::local_tempdir()
  path <- file.path(td, "mixed.csv")
  counts <- integer(3)
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    i <- match(spec$request$pair_uid, decision_bulk_pairs(3)$pair_uid)
    counts[i] <<- counts[i] + 1L
    if (i == 1L && counts[i] == 1L) return(decision_bulk_response(status = 503L))
    if (i == 3L) return(decision_bulk_response(body = decision_333_response(p = c(.3, .3))))
    decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(3), save_path = path)
  out <- do.call(submit_llm_pairs, args)
  expect_equal(nrow(out$results), 2L)
  expect_equal(nrow(out$failed_attempts), 2L)
  expect_equal(nrow(out$failed_pairs), 1L)
  expect_identical(out$failed_attempts$attempt_reason, c("initial", "initial"))
  expect_identical(length(unique(out$failed_attempts$attempt_id)), 2L)
  expect_equal(nrow(utils::read.csv(path)), 2L)
  again <- do.call(submit_llm_pairs, args)
  expect_identical(counts, c(2L, 1L, 1L))
  expect_identical(again$results, out$results)
  expect_identical(again$failed_attempts$attempt_id, out$failed_attempts$attempt_id)
  for (event in decision_bulk_events(path)) {
    expect_false("raw_response" %in% names(event$state$row))
    expect_false(any(c("text1", "text2") %in% names(event$state$row)))
    for (attempt in event$state$attempts) expect_false("raw_response" %in% names(attempt$row))
  }
  expect_silent(pairwiseLLM:::validate_failed_attempts_tbl(out$failed_attempts))
  expect_silent(pairwiseLLM:::validate_results_tbl(out$results))
})

test_that("scientific changes and independently configured wire controls fail before HTTP", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir()
  path <- file.path(td, "identity.csv")
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L
    decision_bulk_response("typesafe")
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args("typesafe", save_path = path)
  baseline <- do.call(submit_llm_pairs, args)
  changes <- list(list(model = "jev-9.8.7"), list(trait_description = "Changed construct"),
                  list(decision_template = "Assess {TRAIT_NAME}: {TRAIT_DESCRIPTION}"))
  for (changeset in changes) {
    different <- args; different[names(changeset)] <- changeset
    expect_error(do.call(submit_llm_pairs, different), class = "pairwiseLLM_decision_checkpoint_error")
  }
  for (field in c("text1", "text2", "ID1", "ID2", "pair_uid")) {
    different <- args; different$pairs[[field]][1] <- "changed"
    expect_error(do.call(submit_llm_pairs, different), class = "pairwiseLLM_decision_checkpoint_error")
  }
  different <- args
  different$pairs[1, c("ID1", "text1", "ID2", "text2")] <- args$pairs[1, c("ID2", "text2", "ID1", "text1")]
  expect_error(do.call(submit_llm_pairs, different), class = "pairwiseLLM_decision_checkpoint_error")
  different <- decision_bulk_args(save_path = path)
  expect_error(do.call(submit_llm_pairs, different), class = "pairwiseLLM_decision_checkpoint_error")
  expect_identical(calls, 4L)
  # Reordering explicit occurrences is safe, with output in the caller's order.
  reordered <- args; reordered$pairs <- reordered$pairs[4:1, ]
  expect_identical(do.call(submit_llm_pairs, reordered)$results$pair_uid, rev(baseline$results$pair_uid))
  path2 <- file.path(td, "openai.csv")
  local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_response(), .package = "pairwiseLLM")
  do.call(submit_llm_pairs, decision_bulk_args(save_path = path2))
  expect_error(do.call(submit_llm_pairs, decision_bulk_args(save_path = path2,
                 prompt_cache_options = list(mode = "explicit"))), class = "pairwiseLLM_decision_checkpoint_error")
})

test_that("corrupt checkpoints fail closed and temporary records do not replace committed evidence", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir()
  path <- file.path(td, "corrupt.csv")
  calls <- 0L
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; decision_bulk_response()
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path)
  do.call(submit_llm_pairs, args)
  writeLines("uncommitted", file.path(paste0(path, ".checkpoint"), ".decision-tmp-torn"))
  expect_equal(nrow(do.call(submit_llm_pairs, args)$results), 1L)
  expect_identical(calls, 1L)
  committed <- file.path(paste0(path, ".checkpoint"), "000000000002.rds")
  bytes <- readBin(committed, "raw", n = file.info(committed)$size)
  writeBin(charToRaw("torn committed record"), committed)
  expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_checkpoint_error")
  writeBin(bytes, committed)
  # Checksum corruption and deletion of an interior record are also terminal.
  envelope <- readRDS(committed); envelope$data[1] <- as.raw(0)
  saveRDS(envelope, committed)
  expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_checkpoint_error")
  writeBin(bytes, committed)
  unlink(file.path(paste0(path, ".checkpoint"), "000000000001.rds"))
  expect_error(do.call(submit_llm_pairs, args), class = "pairwiseLLM_decision_checkpoint_error")
  expect_identical(calls, 1L)
  legacy <- file.path(td, "legacy.csv")
  utils::write.csv(data.frame(custom_id = "LIVE_A_vs_B"), legacy, row.names = FALSE)
  args$save_path <- legacy
  expect_error(do.call(submit_llm_pairs, args), "CSV alone")
})

test_that("CSV and RDS retain zero, one, exact ties and very small probabilities", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  td <- withr::local_tempdir(); path <- file.path(td, "boundaries.csv")
  values <- list(c(0, 1), c(.5, .5), c(1, 1e-100))
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    p <- values[[match(spec$request$pair_uid, decision_bulk_pairs(3)$pair_uid)]]
    decision_bulk_response(body = decision_333_response(p = p, choice = if (p[1] >= p[2]) "SAMPLE_1" else "SAMPLE_2"))
  }, .package = "pairwiseLLM")
  args <- decision_bulk_args(pairs = decision_bulk_pairs(3), save_path = path)
  out <- do.call(submit_llm_pairs, args)
  csv <- utils::read.csv(path, colClasses = "character")
  expect_identical(unname(vapply(csv$p_sample_1, jsonlite::parse_json, numeric(1L))), c(0, .5, 1))
  expect_identical(unname(vapply(csv$p_sample_2, jsonlite::parse_json, numeric(1L))), c(1, .5, 1e-100))
  expect_identical(do.call(submit_llm_pairs, args)$results$p_sample_2, out$results$p_sample_2)
  expect_identical(build_bt_data(out$results)$result, c(0, 1, 1))
})

test_that("opted-in HTTP error bodies persist privately and quota failures are terminal", {
  skip_if_not_installed("filelock")
  td <- withr::local_tempdir(); path <- file.path(td, "quota.csv")
  calls <- 0L
  body <- list(error = list(code = "insufficient_quota", message = "synthetic sensitive diagnostic"))
  local_mocked_bindings(.decision_sync_submit = function(req, spec) {
    calls <<- calls + 1L; decision_bulk_response(status = 429L, body = body)
  }, .package = "pairwiseLLM")
  out <- do.call(submit_llm_pairs, decision_bulk_args(pairs = decision_bulk_pairs(1), save_path = path, include_raw = TRUE))
  expect_identical(calls, 1L)
  expect_identical(out$failed_attempts$raw_response[[1]], body)
  expect_identical(tail(decision_bulk_events(path), 1)[[1]]$state$row$raw_response[[1]], body)
  expect_identical(out$failed_attempts$error_detail, "HTTP 429")
})
