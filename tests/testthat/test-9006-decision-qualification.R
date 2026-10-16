# Release gates that connect existing decision contracts across public boundaries.
test_that("all bulk scientific mutations fail before fresh or resumed transmissions", {
  skip_if_not_installed("filelock")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  decision_bulk_clock()
  build <- pairwiseLLM:::.build_decision_request
  for (provider in c("openai", "typesafe")) {
    td <- withr::local_tempdir()
    calls <- 0L
    local_mocked_bindings(.decision_sync_submit = function(req, spec) {
      calls <<- calls + 1L
      decision_bulk_response(provider)
    }, .decision_async_submit = function(...) {
      calls <<- calls + 1L
      stop("Unexpected asynchronous transmission")
    }, .package = "pairwiseLLM")
    args <- decision_bulk_args(provider, pairs = decision_bulk_pairs(2),
                               save_path = file.path(td, "completed.csv"))
    do.call(submit_llm_pairs, args)
    expect_identical(calls, 2L)
    calls <- 0L
    mutations <- if (provider == "openai") decision_334_mutations() else typesafe_335_mutations()
    mutations <- mutations[!names(mutations) %in% c("fingerprint", "provenance")]
    for (mutate in mutations) {
      for (resume in c(FALSE, TRUE)) {
        for (async in c(FALSE, TRUE)) {
          candidate <- args
          if (!resume) candidate$save_path <- file.path(td, "fresh.csv")
          candidate$parallel <- async
          candidate$workers <- 2L
          with_mocked_bindings({
            expect_error(do.call(submit_llm_pairs, candidate),
                         class = "pairwiseLLM_decision_request_error")
          }, .build_decision_request = function(...) {
            original <- build(...)
            # A valid first occurrence must not be sent before a bad second one.
            if (original$pair_uid != "occurrence-2") return(original)
            changed <- mutate(original)
            expect_identical(changed$provenance, original$provenance)
            expect_identical(changed$request_fingerprint, original$request_fingerprint)
            changed
          }, .package = "pairwiseLLM")
          expect_identical(calls, 0L)
          expect_false(dir.exists(file.path(td, "fresh.csv.checkpoint")))
        }
      }
    }
    expect_equal(nrow(do.call(submit_llm_pairs, args)$results), 2L)
    expect_identical(calls, 0L)
  }
})

test_that("prepared bulk wire JSON, retries and restored results preserve the same science", {
  skip_if_not_installed("filelock")
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  clock <- decision_bulk_clock()
  local_mocked_bindings(.decision_async_wait = function(seconds) {
    later::run_now(timeoutSecs = 0)
    clock$now <- clock$now + min(1, max(0, seconds))
  }, .package = "pairwiseLLM")
  prepare_handle <- getFromNamespace("req_handle", "httr2")
  for (provider in c("openai", "typesafe")) {
    for (async in c(FALSE, TRUE)) {
      td <- withr::local_tempdir()
      pairs <- decision_333_pairs(list(decision_333_request(provider)))
      pairs <- dplyr::bind_rows(pairs, pairs[, c("ID2", "text2", "ID1", "text1", "pair_uid")])
      # Assign reversed values, rather than relying on column names or sorting.
      pairs[2, c("ID1", "text1", "ID2", "text2")] <- pairs[1, c("ID2", "text2", "ID1", "text1")]
      pairs$pair_uid <- c("primary", "reversed")
      args <- decision_bulk_args(provider, pairs = pairs, parallel = async, workers = 2L,
                                 save_path = file.path(td, "wire.csv"),
                                 trait_description = "Logical sequencing and connections.\nExclude spelling and grammar.",
                                 decision_template = "Assess {TRAIT_NAME}.\n{TRAIT_DESCRIPTION}")
      if (provider == "openai") args$prompt_cache_options <- list(mode = "explicit", ttl = "30m")
      captured <- list()
      response <- function() {
        decision_bulk_response(provider, status = if (length(captured) == 1L) 503L else 200L)
      }
      local_mocked_bindings(req_perform1 = function(req, req_prep, ...) {
        captured[[length(captured) + 1L]] <<- req_prep
        response()
      }, req_handle = if (async) function(req) {
        captured[[length(captured) + 1L]] <<- req
        prepare_handle(req)
      } else prepare_handle, .package = "httr2")
      local_mocked_bindings(multi_add = function(handle, done, fail, pool, data) {
        done(list(method = "POST", url = captured[[length(captured)]]$url,
                  status_code = if (length(captured) == 1L) 503L else 200L,
                  headers = charToRaw("HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"),
                  content = charToRaw(pairwiseLLM:::.decision_json(decision_333_response(provider))),
                  times = numeric()))
        invisible(handle)
      }, .package = "curl")
      out <- do.call(submit_llm_pairs, args)
      expect_equal(nrow(out$results), 2L)
      expect_length(captured, 3L)
      expected <- decision_333_fixture(paste0(provider, "-request.json"))
      expected$questions[[1]]$instructions <- paste0("Assess Organization.\n", args$trait_description)
      if (provider == "openai") expected$prompt_cache_options <- list(mode = "explicit", ttl = "30m")
      field <- if (provider == "openai") "input" else "state"
      bodies <- lapply(captured, function(req) req$options$postfields)
      for (i in seq_along(captured)) {
        req <- captured[[i]]
        wire <- jsonlite::parse_json(rawToChar(bodies[[i]]))
        reverse <- identical(jsonlite::parse_json(wire[[field]])$SAMPLE_1, pairs$text1[2])
        wanted <- expected
        if (reverse) {
          wanted[[field]] <- as.character(jsonlite::toJSON(
            list(SAMPLE_1 = pairs$text1[2], SAMPLE_2 = pairs$text2[2]), auto_unbox = TRUE))
        }
        expect_identical(wire, wanted)
        expect_identical(req$url, if (provider == "openai") "https://api.openai.com/v1/decisions" else
          "https://api.typesafe.ai/v1/systemone")
        expect_true(is.raw(bodies[[i]]))
      }
      # Both orientations occur; a retry repeats the failed request's exact bytes.
      expect_equal(length(unique(vapply(bodies, rawToChar, character(1L)))), 2L)
      expect_equal(sum(vapply(bodies, identical, logical(1L), bodies[[1]])), 2L)
      restored <- do.call(submit_llm_pairs, args)
      expect_length(captured, 3L)
      expect_identical(restored, out)
      expect_identical(build_bt_data(restored$results)$result, c(0, 0))
      expect_identical(restored$results$better_id, pairs$ID2)
    }
  }
})

test_that("decision privacy boundaries cover successful, invalid and HTTP-error responses", {
  skip_if_not_installed("filelock")
  decision_bulk_clock()
  key <- "PRIVATE-CREDENTIAL-337"
  sample <- "PRIVATE-WRITING-337"
  for (provider in c("openai", "typesafe")) {
    for (kind in c("success", "invalid", "http")) {
      td <- withr::local_tempdir()
      response <- decision_333_response(provider, p = if (kind == "invalid") c(.1, .1) else c(.3, .7))
      response$private_echo <- paste(key, sample)
      local_mocked_bindings(.decision_sync_submit = function(req, spec) {
        decision_bulk_response(provider, status = if (kind == "http") 401L else 200L, body = response)
      }, .package = "pairwiseLLM")
      pairs <- decision_bulk_pairs(1)
      pairs$text1 <- sample
      args <- decision_bulk_args(provider, pairs = pairs, api_key = key,
                                 save_path = file.path(td, "private.csv"))
      messages <- character()
      printed <- capture.output(out <- withCallingHandlers(do.call(submit_llm_pairs, args),
        message = function(cnd) { messages <<- c(messages, conditionMessage(cnd)); invokeRestart("muffleMessage") },
        warning = function(cnd) { messages <<- c(messages, conditionMessage(cnd)); invokeRestart("muffleWarning") }))
      for (secret in c(key, sample)) {
        expect_false(any(grepl(secret, c(messages, printed), fixed = TRUE)))
        csv <- readLines(args$save_path, warn = FALSE)
        expect_false(any(grepl(secret, csv, fixed = TRUE)))
        records <- list.files(paste0(args$save_path, ".checkpoint"), pattern = "[.]rds$", full.names = TRUE)
        for (record in records) {
          value <- pairwiseLLM:::.decision_record_read(record)$value
          expect_false(any(grepl(secret, capture.output(dput(value)), fixed = TRUE)))
        }
        for (name in names(out)) {
          table <- out[[name]]
          # Successful normalized rows preserve source text columns by contract;
          # input data are not scalar failure diagnostics or ordinary CSV exports.
          if (name == "results") table <- table[, setdiff(names(table), c("text1", "text2"))]
          scalars <- table[!vapply(table, is.list, logical(1L))]
          expect_false(any(grepl(secret, unlist(scalars, use.names = FALSE), fixed = TRUE)))
        }
      }
      if (kind == "success") {
        expect_false("raw_response" %in% names(out$results))
      } else if (kind == "invalid") {
        expect_identical(out$failed_attempts$raw_response[[1]], response)
        saved_object <- file.path(td, "caller-saved-object.rds")
        saveRDS(out, saved_object)
        expect_identical(readRDS(saved_object)$failed_attempts$raw_response[[1]], response)
      }
      args$include_raw <- TRUE
      args$save_path <- file.path(td, "opted-in.csv")
      private <- do.call(submit_llm_pairs, args)
      states <- decision_bulk_events(args$save_path)
      expect_true(any(vapply(states, function(event) {
        identical(event$state$row$raw_response[[1]], response)
      }, logical(1L))))
      expect_false(any(grepl(sample, readLines(args$save_path), fixed = TRUE)))
    }
    # A thrown transport error is also sanitized, rather than exposing its text.
    local_mocked_bindings(.decision_sync_submit = function(...) stop(paste(key, sample)), .package = "pairwiseLLM")
    failed <- do.call(submit_llm_pairs, decision_bulk_args(provider, pairs = decision_bulk_pairs(1)))
    expect_false(any(grepl(paste(key, sample), failed$failed_attempts$error_detail, fixed = TRUE)))
  }
})

test_that("probability evidence survives collection and does not alter adaptive binary ingest", {
  withr::local_seed(337L)
  for (provider in c("openai", "typesafe")) {
    local_mocked_bindings(.decision_sync_submit = function(req, spec) decision_bulk_response(provider),
                          .package = "pairwiseLLM")
    out <- do.call(submit_llm_pairs, decision_bulk_args(provider, pairs = decision_bulk_pairs(1)))
    original <- out$results
    changed <- original
    changed$p_sample_1 <- .01
    changed$p_sample_2 <- changed$p_chosen <- .99
    expect_identical(build_bt_data(original), build_bt_data(changed))
    # The existing custom-judge interface consumes Y, independent of extra fields.
    run <- function(row) {
      judge <- function(A, B, state, ...) {
        c(list(is_valid = TRUE, Y = as.integer(row$winner_pos == 1L)),
          as.list(row[, c("p_sample_1", "p_sample_2", "p_chosen", "decision_confidence")]))
      }
      state <- adaptive_rank_start(make_test_items(4), seed = 337L)
      withr::with_seed(337L, adaptive_rank_run_live(state, judge, n_steps = 3L, progress = "none"))
    }
    first <- run(original)
    second <- run(changed)
    expect_identical(first$history_pairs, second$history_pairs)
    expect_identical(first$trueskill_state, second$trueskill_state)
    expect_identical(adaptive_results_history(first)$result, rep(0, 3))
  }
})
