test_that("decision normalization preserves reversed identities and repeated occurrences", {
  for (provider in c("openai", "typesafe")) {
    forward <- decision_333_request(provider)
    p <- forward$provenance
    evidence <- jsonlite::fromJSON(forward$body$input %||% forward$body$state)
    reverse <- decision_333_request(provider, ID1 = p$ID2, text1 = evidence$SAMPLE_2,
                                    ID2 = p$ID1, text2 = evidence$SAMPLE_1, pair_uid = "reverse-333")
    repeat_pair <- decision_333_request(provider, pair_uid = "repeated-forward-333")
    requests <- list(forward, reverse, repeat_pair)
    pairs <- decision_333_pairs(requests)
    raw <- dplyr::bind_rows(
      pairwiseLLM:::.parse_decision_response(decision_333_response(provider), forward, "provider-forward"),
      pairwiseLLM:::.parse_decision_response(decision_333_response(provider, c(.7, .3), "SAMPLE_1"), reverse, "provider-reverse"),
      pairwiseLLM:::.parse_decision_response(decision_333_response(provider, c(1, 0), "SAMPLE_1"), repeat_pair)
    )
    # Responses arrive out of order. Alignment uses real UIDs, never row index.
    out <- pairwiseLLM:::.normalize_llm_results(raw[c(3, 1, 2), ], pairs, provider, forward$body$model)
    expect_identical(out$alignment, "custom_id")
    expect_identical(out$results$pair_uid, pairs$pair_uid)
    expect_identical(out$results$better_id, c("B:02", "B:02", "A_with_vs_delimiter"))
    expect_identical(out$results$winner_pos, c(2L, 1L, 1L))
    expect_identical(out$results$p_sample_1, c(.3, .7, 1))
    expect_identical(out$results$p_sample_2, c(.7, .3, 0))
    expect_identical(out$results$provider_request_id, c("provider-forward", "provider-reverse", NA_character_))
    expect_silent(pairwiseLLM:::validate_results_tbl(out$results))
    expect_identical(build_bt_data(out$results)$result, c(0, 1, 1))
    expect_identical(build_bt_data(out$results), build_bt_data(raw))
    expect_false("raw_response" %in% names(out$results))
    expect_equal(nrow(out$failed_attempts), 0L)
    expect_false(identical(forward$request_fingerprint, reverse$request_fingerprint))
  }
})

test_that("invalid decisions with binary winners are excluded before the success filter", {
  for (provider in c("openai", "typesafe")) {
    request <- decision_333_request(provider)
    pairs <- decision_333_pairs(list(request))
    cases <- list(
      list(response = decision_333_response(provider, c(.8, .2)), code = "choice_probability_mismatch"),
      list(response = decision_333_response(provider, c(.4, .4)), code = "invalid_probabilities"),
      list(response = decision_333_response(provider, confidence = NULL), code = "invalid_confidence")
    )
    for (case in cases) {
      raw <- pairwiseLLM:::.parse_decision_response(case$response, request, "provider-request")
      expect_identical(raw$better_id, "B:02")
      out <- pairwiseLLM:::.normalize_llm_results(raw, pairs, provider, request$body$model)
      expect_equal(nrow(out$results), 0L)
      expect_identical(out$failed_attempts$error_code, case$code)
      expect_identical(out$failed_pairs$error_code, case$code)
      for (failed in list(out$failed_pairs, out$failed_attempts)) {
        expect_identical(failed$better_sample, "SAMPLE_2")
        expect_identical(failed$better_id, "B:02")
        expect_identical(failed$trait_name, "Organization")
        expect_identical(failed$ordered_key, "A_with_vs_delimiter:B:02")
        expect_identical(failed$backend, provider)
        expect_identical(failed$p_sample_1, raw$p_sample_1)
        expect_identical(failed$p_sample_2, raw$p_sample_2)
        expect_identical(failed$probabilities_json, raw$probabilities_json)
        expect_identical(failed$request_fingerprint, request$request_fingerprint)
        expect_identical(failed$provider_request_id, "provider-request")
        expect_identical(failed$raw_response[[1]], case$response)
        expect_false("text1" %in% names(failed))
      }
      expect_silent(pairwiseLLM:::validate_failed_attempts_tbl(out$failed_attempts))
      expect_equal(nrow(build_bt_data(out$results)), 0L)
    }
    refusal <- if (provider == "openai") {
      list(answers = list(list(name = "writing_trait", type = "refusal")))
    } else {
      list(answers = list(writing_trait = list(type = "score", score = 1)))
    }
    out <- pairwiseLLM:::.normalize_llm_results(
      pairwiseLLM:::.parse_decision_response(refusal, request), pairs, provider, request$body$model
    )
    expect_identical(out$failed_attempts$error_code,
                     if (provider == "openai") "decision_refusal" else "malformed_decision")
  }
})

test_that("missing, altered or inconsistent probability fields cannot bypass validation", {
  request <- decision_333_request()
  pairs <- decision_333_pairs(list(request))
  raw <- pairwiseLLM:::.parse_decision_response(decision_333_response(), request)
  changes <- list(list(p_sample_1 = NULL), list(p_sample_2 = .2), list(p_chosen = .5),
                  list(decision_confidence = NA_real_), list(better_sample = "OTHER"),
                  list(probabilities_json = '{"SAMPLE_1":0.1,"SAMPLE_2":0.1}'),
                  list(probabilities_json = NA_character_))
  for (change in changes) {
    altered <- raw
    for (name in names(change)) altered[[name]] <- change[[name]]
    out <- pairwiseLLM:::.normalize_llm_results(altered, pairs, "openai", request$body$model)
    expect_equal(nrow(out$results), 0L)
    expect_equal(nrow(out$failed_attempts), 1L)
  }
})

test_that("decision identity never falls back to row order or overwrites a supplied UID", {
  request <- decision_333_request()
  pairs <- decision_333_pairs(list(request))
  raw <- pairwiseLLM:::.parse_decision_response(decision_333_response(), request)
  for (uids in list(NULL, NA_character_, "")) {
    broken <- pairs
    broken$pair_uid <- uids
    expect_error(pairwiseLLM:::.normalize_llm_results(raw, broken, "openai", request$body$model), "pair_uid")
  }
  expect_error(pairwiseLLM:::.normalize_llm_results(raw, dplyr::bind_rows(pairs, pairs), "openai", request$body$model), "unique")
  for (uids in list(NULL, NA_character_, "unknown-pair")) {
    broken <- raw
    broken$pair_uid <- uids
    expect_error(pairwiseLLM:::.normalize_llm_results(broken, pairs, "openai", request$body$model), "actual.*pair_uid")
  }
  expect_error(pairwiseLLM:::.normalize_llm_results(dplyr::bind_rows(raw, raw), pairs, "openai", request$body$model), "unique actual")
  # Repeated IDs across different pairs are legitimate, self-pairs are not.
  expect_error(decision_333_request(ID1 = "B:02"), "self-pairs")
  for (change in list(list(ID1 = "wrong"), list(ID2 = "wrong"), list(better_id = "A_with_vs_delimiter"),
                      list(custom_id = "provider-id"), list(request_fingerprint = "changed"),
                      list(endpoint = "/v1/systemone"), list(trait_name = "Ideas"),
                      list(request_provenance_json = "{}"))) {
    broken <- raw
    for (name in names(change)) broken[[name]] <- change[[name]]
    out <- pairwiseLLM:::.normalize_llm_results(broken, pairs, "openai", request$body$model)
    expect_equal(nrow(out$results), 0L)
    expect_true(out$failed_attempts$error_code %in% c("decision_identity_mismatch", "malformed_decision"))
    expect_identical(out$failed_attempts$pair_uid, request$pair_uid)
  }
  for (column in c("text1", "text2")) {
    for (text in list("different synthetic text", NA_character_, NULL)) {
      changed <- pairs
      changed[[column]] <- text
      out <- pairwiseLLM:::.normalize_llm_results(raw, changed, "openai", request$body$model)
      expect_identical(out$failed_attempts$error_code, "decision_identity_mismatch")
    }
  }
  for (args in list(list(backend = "typesafe", model = request$body$model),
                    list(backend = "openai", model = "other-model"))) {
    out <- do.call(pairwiseLLM:::.normalize_llm_results, c(list(raw = raw, pairs = pairs), args))
    expect_identical(out$failed_attempts$error_code, "decision_identity_mismatch")
  }
  # custom_id can be omitted: actual pair_uid remains the authoritative identity.
  raw$custom_id <- NULL
  out <- pairwiseLLM:::.normalize_llm_results(raw, pairs, "openai", request$body$model)
  expect_identical(out$results$pair_uid, request$pair_uid)
})

test_that("mixed and historical results retain binary semantics and missing probabilities", {
  request <- decision_333_request()
  pair <- decision_333_pairs(list(request))
  old_pair <- tibble::tibble(ID1 = "X", text1 = "x", ID2 = "Y", text2 = "y", pair_uid = "old")
  old <- tibble::tibble(ID1 = "X", ID2 = "Y", custom_id = "old", better_sample = "SAMPLE_1", better_id = "X")
  decision <- pairwiseLLM:::.parse_decision_response(decision_333_response(), request, include_raw = TRUE)
  out <- pairwiseLLM:::.normalize_llm_results(dplyr::bind_rows(decision, old), dplyr::bind_rows(pair, old_pair), "openai", request$body$model)
  expect_identical(out$results$better_id, c("B:02", "X"))
  expect_true(all(is.na(out$results[2L, c("p_sample_1", "p_sample_2", "p_chosen", "decision_confidence") ])))
  expect_identical(build_bt_data(out$results)$result, c(0, 1))
  expect_false("raw_response" %in% names(out$results))
  kept <- pairwiseLLM:::.normalize_llm_results(decision, pair, "openai", request$body$model, include_raw = TRUE)
  expect_identical(kept$results$raw_response, decision$raw_response)
  historical <- pairwiseLLM:::.normalize_llm_results(old, old_pair, "openai", "legacy-model")
  expect_identical(historical$results$better_id, "X")
  expect_identical(historical$results$winner_pos, 1L)
  expect_true(all(is.na(historical$results[c("p_sample_1", "p_sample_2", "p_chosen", "decision_confidence") ])))
  # Zero-row schemas are also bindable, with no invented probabilities.
  empty <- pairwiseLLM:::.normalize_llm_results(old[0, ], old_pair[0, ], "openai", "legacy-model")
  expect_type(empty$results$p_sample_1, "double")
})

test_that("mixed failed rows retain raw evidence only for decisions when opted out", {
  request <- decision_333_request()
  pairs <- decision_333_pairs(list(request))
  legacy_pair <- tibble::tibble(ID1 = "X", text1 = "x", ID2 = "Y", text2 = "y", pair_uid = "legacy-failure")
  legacy <- tibble::tibble(ID1 = "X", ID2 = "Y", custom_id = "legacy-failure", better_id = NA_character_,
                           raw_response = list(list(content = "SYNTHETIC_LEGACY_PRIVATE")))
  response <- decision_333_response(p = c(.8, .2))
  failed <- pairwiseLLM:::.parse_decision_response(response, request)
  out <- pairwiseLLM:::.normalize_llm_results(dplyr::bind_rows(failed, legacy),
                                            dplyr::bind_rows(pairs, legacy_pair), "openai", request$body$model)
  for (table in list(out$failed_pairs, out$failed_attempts)) {
    expect_identical(table$raw_response[[1]], response)
    expect_null(table$raw_response[[2]])
    expect_false(any(grepl("SYNTHETIC_LEGACY_PRIVATE", table$error_detail, fixed = TRUE)))
  }
  kept <- pairwiseLLM:::.normalize_llm_results(dplyr::bind_rows(failed, legacy),
                                             dplyr::bind_rows(pairs, legacy_pair), "openai", request$body$model,
                                             include_raw = TRUE)
  expect_identical(kept$failed_attempts$raw_response[[2]], legacy$raw_response[[1]])
})
