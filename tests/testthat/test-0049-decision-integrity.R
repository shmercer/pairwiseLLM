test_that("scientific body validation accepts both existing provider representations", {
  for (provider in c("openai", "typesafe")) {
    for (template in list(NULL, "Judge {TRAIT_NAME}.\n{TRAIT_DESCRIPTION}")) {
      r <- decision_333_request(provider, decision_template = template,
                                 text1 = "\u00e9\r\n\"SAMPLE_2\":literal", text2 = "")
      before <- serialize(r, NULL)
      expect_invisible(pairwiseLLM:::.validate_decision_request(r))
      expect_identical(serialize(r, NULL), before)
      expect_invisible(pairwiseLLM:::.validate_decision_request(
        r, jsonlite::parse_json(pairwiseLLM:::.decision_json(r$body))))
    }
  }
})

test_that("body mutations cannot borrow unchanged provenance or fingerprint", {
  original <- decision_333_request()
  for (mutate in decision_334_mutations()) {
    r <- mutate(original)
    expect_identical(r$provenance, original$provenance)
    expect_identical(r$request_fingerprint, original$request_fingerprint)
    err <- tryCatch(pairwiseLLM:::.validate_decision_request(r), error = identity)
    expect_s3_class(err, "pairwiseLLM_decision_request_error")
    expect_false(grepl("Synthetic|Changed|Altered", conditionMessage(err)))
    expect_false(identical(r$body, original$body))
  }
  for (evidence in list("not JSON", "https://example.com/never-read", "{}", "[]",
                         '{"SAMPLE_1":"a","SAMPLE_1":"b"}',
                         '{"SAMPLE_1":"a","SAMPLE_2":"b","extra":"c"}',
                         '{"SAMPLE_1":1,"SAMPLE_2":"b"}')) {
    r <- original
    r$body$input <- evidence
    expect_error(pairwiseLLM:::.validate_decision_request(r), class = "pairwiseLLM_decision_request_error")
  }
})

test_that("internally inconsistent provenance fails even with a freshly computed fingerprint", {
  original <- decision_333_request()
  # IDs have no separate body field: a consistent new ID + fingerprint is a
  # different valid request, not an authenticated alteration of the old one.
  for (name in setdiff(names(original$provenance), c("ID1", "ID2"))) {
    r <- original
    r$provenance[[name]] <- if (name == "choices") rev(r$provenance[[name]]) else "altered"
    r$request_fingerprint <- pairwiseLLM:::.decision_fingerprint(r$provenance)
    expect_error(pairwiseLLM:::.validate_decision_request(r), class = "pairwiseLLM_decision_request_error")
  }
  for (invalid in list(NULL, list(), utils::modifyList(original, list(pair_uid = "")),
                       utils::modifyList(original, list(request_fingerprint = "changed")))) {
    expect_error(pairwiseLLM:::.validate_decision_request(invalid), class = "pairwiseLLM_decision_request_error")
  }
  r <- original
  r$provenance$unexpected <- "extra"
  r$request_fingerprint <- pairwiseLLM:::.decision_fingerprint(r$provenance)
  expect_error(pairwiseLLM:::.validate_decision_request(r), class = "pairwiseLLM_decision_request_error")
})

test_that("the shared validator also rejects altered TypeSafe scientific content", {
  original <- decision_333_request("typesafe")
  mutations <- list(
    function(r) { r$body$state <- "{}"; r },
    function(r) { names(r$body$questions) <- "wrong"; r },
    function(r) { r$body$questions[[1]]$criteria <- rev(r$body$questions[[1]]$criteria); r },
    function(r) { r$body$questions[[1]]$criteria$SAMPLE_1 <- "changed"; r },
    function(r) { r$body$extra <- "changed"; r }
  )
  for (mutate in mutations) {
    expect_error(pairwiseLLM:::.validate_decision_request(mutate(original)),
                 class = "pairwiseLLM_decision_request_error")
  }
})
