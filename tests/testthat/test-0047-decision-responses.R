test_that("provider fixtures preserve selections, complete distributions, models and usage", {
  for (provider in c("openai", "typesafe")) {
    request <- decision_333_request(provider)
    response <- decision_333_fixture(paste0(provider, "-response.json"))
    row <- pairwiseLLM:::.parse_decision_response(response, request, "transport-id")
    expect_identical(row$better_sample, "SAMPLE_2")
    expect_identical(row$better_id, "B:02")
    expect_identical(row$winner_pos, 2L)
    expect_identical(c(row$p_sample_1, row$p_sample_2, row$p_chosen), c(.3, .7, .7))
    expect_identical(row$decision_confidence, .2)
    expect_identical(as.numeric(unlist(jsonlite::fromJSON(row$probabilities_json))), c(.3, .7))
    expect_identical(row$provider_request_id, "transport-id")
    expect_identical(row$pair_uid, request$pair_uid)
    expect_identical(row$requested_model, request$body$model)
    expect_identical(row$resolved_model, response$model)
    expect_identical(row$prompt_tokens, 12)
    expect_identical(row$completion_tokens, if (provider == "openai") 0 else 3)
    expect_identical(row$total_tokens, if (provider == "openai") 12 else NA_real_)
    expect_true(is.na(row$decision_error_code))
    expect_false("raw_response" %in% names(row))
    json <- pairwiseLLM:::.decision_json(response)
    expect_identical(pairwiseLLM:::.parse_decision_response(json, request), row |> dplyr::mutate(provider_request_id = NA_character_))
    kept <- pairwiseLLM:::.parse_decision_response(response, request, include_raw = TRUE)
    expect_identical(kept$raw_response[[1]], response)
  }
  request <- decision_333_request("typesafe", model = "explicit-alias")
  response <- decision_333_response("typesafe")
  row <- pairwiseLLM:::.parse_decision_response(response, request)
  expect_identical(row$requested_model, "explicit-alias")
  expect_identical(row$resolved_model, "jev-1.13.0")
  response$model <- response$usage <- NULL
  row <- pairwiseLLM:::.parse_decision_response(response, request)
  expect_identical(row$model, "explicit-alias")
  expect_true(is.na(row$resolved_model))
  expect_true(all(is.na(row[c("prompt_tokens", "completion_tokens", "total_tokens")])))
})

test_that("boundary and precise probabilities remain exact, with explicit tie winners", {
  cases <- list(list(p = c(0, 1), choice = "SAMPLE_2"),
                list(p = c(1, 0), choice = "SAMPLE_1"),
                list(p = c(.5, .5), choice = "SAMPLE_1"),
                list(p = c(.5, .5), choice = "SAMPLE_2"),
                list(p = c(.500000001, .499999999), choice = "SAMPLE_2"),
                list(p = c(.1234567890123456, .8765432109876544), choice = "SAMPLE_2"),
                list(p = c(1e-30, 1), choice = "SAMPLE_2"),
                list(p = c(.3, .7000005), choice = "SAMPLE_2"))
  for (provider in c("openai", "typesafe")) {
    for (confidence in c(0, 1)) {
      row <- pairwiseLLM:::.parse_decision_response(
        decision_333_response(provider, confidence = confidence), decision_333_request(provider)
      )
      expect_identical(row$decision_confidence, confidence)
      expect_true(is.na(row$decision_error_code))
    }
    for (case in cases) {
      response <- do.call(decision_333_response, c(list(provider = provider), case))
      row <- pairwiseLLM:::.parse_decision_response(response, decision_333_request(provider))
      expect_true(is.na(row$decision_error_code))
      expect_identical(c(row$p_sample_1, row$p_sample_2), case$p)
      expect_identical(row$better_sample, case$choice)
      expect_identical(as.numeric(unlist(jsonlite::fromJSON(row$probabilities_json))), case$p)
    }
  }
})

test_that("lower-probability selections fail with original evidence and no substitution", {
  for (provider in c("openai", "typesafe")) {
    response <- decision_333_response(provider, c(.8, .2))
    row <- pairwiseLLM:::.parse_decision_response(response, decision_333_request(provider))
    expect_identical(row$decision_error_code, "choice_probability_mismatch")
    expect_match(row$decision_error_detail, "do not automatically retry")
    expect_identical(row$better_sample, "SAMPLE_2")
    expect_identical(row$better_id, "B:02")
    expect_identical(c(row$p_sample_1, row$p_sample_2, row$p_chosen), c(.8, .2, .2))
    expect_identical(row$raw_response[[1]], response)
    response <- decision_333_response(provider, c(.50000002, .49999998))
    expect_identical(pairwiseLLM:::.parse_decision_response(response, decision_333_request(provider))$decision_error_code,
                     "choice_probability_mismatch")
  }
})

test_that("malformed distributions, choices and confidence never become valid decisions", {
  for (provider in c("openai", "typesafe")) {
    request <- decision_333_request(provider)
    for (p in list(c(.2, .2), c(-.1, 1.1), c(NA, 1), c(Inf, 0), c(NaN, 0), c(.3, .700002))) {
      row <- pairwiseLLM:::.parse_decision_response(decision_333_response(provider, p), request)
      expect_identical(row$decision_error_code, "invalid_probabilities")
    }
    for (choice in list(NULL, NA_character_, "", "OTHER", TRUE, c("SAMPLE_1", "SAMPLE_2"))) {
      row <- pairwiseLLM:::.parse_decision_response(decision_333_response(provider, choice = choice), request)
      expect_identical(row$decision_error_code, "invalid_choice")
    }
    for (confidence in list(NULL, NA_real_, NaN, Inf, -.01, 1.01, TRUE, "0.2", c(.1, .2), list(.2))) {
      row <- pairwiseLLM:::.parse_decision_response(decision_333_response(provider, confidence = confidence), request)
      expect_identical(row$decision_error_code, "invalid_confidence")
      expect_true("raw_response" %in% names(row))
    }
    containers <- if (provider == "openai") {
      list(NULL, .5, list(list(value = "SAMPLE_1", probability = 1)),
           list(list(value = "SAMPLE_1", probability = .5), list(value = "SAMPLE_1", probability = .5)),
           list(list(value = "SAMPLE_1", probability = .5), list(value = "OTHER", probability = .5)),
           list(list(value = TRUE, probability = .5), list(value = "SAMPLE_2", probability = .5)),
           list(list(value = "SAMPLE_1", probability = "0.5"), list(value = "SAMPLE_2", probability = .5)),
           list(SAMPLE_1 = .5, SAMPLE_2 = .5), list(list(value = "SAMPLE_1", probability = .3), list(value = "SAMPLE_2", probability = .7), list(value = "OTHER", probability = 0)))
    } else {
      list(NULL, .5, list(SAMPLE_1 = 1), list(SAMPLE_1 = .5, SAMPLE_1 = .5),
           list(SAMPLE_1 = .5, OTHER = .5), list(.5, .5),
           list(SAMPLE_1 = "0.5", SAMPLE_2 = .5), list(SAMPLE_1 = .3, SAMPLE_2 = .7, OTHER = 0))
    }
    for (container in containers) {
      response <- decision_333_response(provider)
      response$answers[[1L]]$probabilities <- container
      row <- pairwiseLLM:::.parse_decision_response(response, request)
      expect_identical(row$decision_error_code, "invalid_probabilities")
      expect_identical(row$raw_response[[1]], response)
    }
  }
})

test_that("refusals and unexpected response structures have distinct failure reasons", {
  for (provider in c("openai", "typesafe")) {
    request <- decision_333_request(provider)
    for (type in c("refusal", "score", "predicate", "unknown")) {
      response <- decision_333_response(provider)
      response$answers[[1L]] <- if (provider == "openai") list(name = "writing_trait", type = type) else list(type = type)
      row <- pairwiseLLM:::.parse_decision_response(response, request)
      expect_identical(row$decision_error_code,
                       if (provider == "openai" && type == "refusal") "decision_refusal" else "malformed_decision")
      expect_true(is.na(row$better_id))
    }
    for (response in list(NULL, list(), "not JSON: SYNTHETIC_PRIVATE", "[]", c("{}", "{}"),
                          list(answers = 1), list(answers = list(list(list(type = "choice")))),
                          list(answers = list(wrong = list(type = "choice"))),
                          list(model = 1), list(usage = 1), list(usage = list(input_tokens = -1)),
                          list(error = list(message = "SYNTHETIC_PRIVATE")),
                          list(answers = list(writing_trait = list(), writing_trait = list())))) {
      row <- pairwiseLLM:::.parse_decision_response(response, request)
      expect_identical(row$decision_error_code, "malformed_decision")
      expect_false(grepl("SYNTHETIC_PRIVATE", row$decision_error_detail, fixed = TRUE))
    }
    response <- decision_333_response(provider)
    response$answers <- c(response$answers, response$answers)
    expect_identical(pairwiseLLM:::.parse_decision_response(response, request)$decision_error_code, "malformed_decision")
  }
  response <- decision_333_response()
  response$answers[[1]]$name <- NULL
  expect_identical(pairwiseLLM:::.parse_decision_response(response, decision_333_request())$decision_error_code, "malformed_decision")
})

test_that("parser rejects invalid construction controls", {
  request <- decision_333_request()
  response <- decision_333_response()
  for (invalid in list(NULL, list(), utils::modifyList(request, list(request_fingerprint = "changed")))) {
    expect_error(pairwiseLLM:::.parse_decision_response(response, invalid), "unmodified")
  }
  for (invalid in list(NULL, NA, 1, c(TRUE, FALSE), matrix(TRUE))) {
    expect_error(pairwiseLLM:::.parse_decision_response(response, request, include_raw = invalid), "include_raw")
  }
  for (invalid in list(NA_character_, "", 1)) {
    expect_error(pairwiseLLM:::.parse_decision_response(response, request, provider_request_id = invalid), "provider_request_id")
  }
})
