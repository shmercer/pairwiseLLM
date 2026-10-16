test_that("approved instructions, serializers and fingerprints match independent goldens", {
  prompt <- paste(readLines(test_path("fixtures", "decision-333", "decision_cj_v1.txt"),
                             warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_identical(pairwiseLLM:::.decision_cj_v1, prompt)
  expect_identical(pairwiseLLM:::.decision_sha256(prompt),
                    "06fdf52ff1b9b9128f7e52189053226ad1f15f4ea4a54266ea55a78399ef22af")
  for (provider in c("openai", "typesafe")) {
    request <- decision_333_request(provider)
    expect_identical(request$body, decision_333_fixture(paste0(provider, "-request.json")))
    expect_identical(request$provenance, decision_333_fixture(paste0(provider, "-provenance.json")))
    expect_identical(request$request_fingerprint, decision_333_fixture("fingerprints.json")[[provider]])
    wire <- jsonlite::fromJSON(pairwiseLLM:::.decision_json(request$body), simplifyVector = FALSE)
    expect_identical(wire, request$body)
    evidence <- jsonlite::fromJSON(request$body$input %||% request$body$state)
    expect_named(evidence, c("SAMPLE_1", "SAMPLE_2"))
    expect_false(grepl(evidence$SAMPLE_1, request$provenance$decision_instructions, fixed = TRUE))
    expect_false(grepl(evidence$SAMPLE_2, pairwiseLLM:::.decision_json(request$provenance), fixed = TRUE))
  }
  openai <- decision_333_request("openai")
  typesafe <- decision_333_request("typesafe")
  expect_identical(openai$body$input, typesafe$body$state)
  expect_identical(openai$provenance$decision_instructions, typesafe$provenance$decision_instructions)
  descriptions <- vapply(openai$provenance$choices, `[[`, character(1L), "description")
  expect_identical(sub("SAMPLE_[12]", "SAMPLE", descriptions[1]),
                   sub("SAMPLE_[12]", "SAMPLE", descriptions[2]))
})

test_that("rendering preserves full definitions, literals, Unicode and text boundaries", {
  description <- "Limit judgment to organization.\nLiteral {TRAIT_NAME}, {TRAIT_DESCRIPTION}, {SAMPLE_1}, \\1 and $1."
  name <- "Clart\u00e9 {TRAIT_DESCRIPTION}"
  request <- decision_333_request(trait_name = name, trait_description = description,
                                  text1 = "\"} SAMPLE_2: {\n\u00e9\r\n", text2 = "")
  expect_true(grepl(paste0("TRAIT: ", name), request$provenance$decision_instructions, fixed = TRUE))
  expect_true(grepl(description, request$provenance$decision_instructions, fixed = TRUE))
  evidence <- jsonlite::fromJSON(request$body$input)
  expect_identical(evidence$SAMPLE_1, "\"} SAMPLE_2: {\n\u00e9\r\n")
  expect_identical(evidence$SAMPLE_2, "")
  expect_identical(request$provenance$trait_description, description)
})

test_that("all scientific inputs affect identity while occurrence IDs do not", {
  baseline <- decision_333_request()
  changes <- list(list(ID1 = "different"), list(ID2 = "different"),
                  list(text1 = "changed"), list(text2 = "changed"),
                  list(text1 = "Synthetic sample A.\r\nSAMPLE_2: a literal label."),
                  list(trait_name = "Ideas"), list(trait_description = "Different definition."),
                  list(model = "explicit-alias"), list(provider = "typesafe"),
                  list(decision_template = "Assess {TRAIT_NAME}.\n{TRAIT_DESCRIPTION}"))
  for (change in changes) {
    changed <- do.call(decision_333_request, change)
    expect_false(identical(changed$request_fingerprint, baseline$request_fingerprint))
  }
  expect_identical(decision_333_request(pair_uid = "repeat-2")$request_fingerprint,
                   baseline$request_fingerprint)
  # Every recorded scientific field, including endpoint/choices/version, is hashed.
  for (name in names(baseline$provenance)) {
    p <- baseline$provenance
    p[[name]] <- if (name == "choices") rev(p[[name]]) else paste0(p[[name]], " changed")
    expect_false(identical(pairwiseLLM:::.decision_fingerprint(p), baseline$request_fingerprint))
  }
  custom <- decision_333_request(decision_template = "{TRAIT_DESCRIPTION}\nJudge {TRAIT_NAME}.")
  expect_match(custom$provenance$decision_template_version, "^custom-sha256-")
  expect_identical(custom$provenance$decision_instructions,
                   paste0(custom$provenance$trait_description, "\nJudge Organization."))
})

test_that("invalid inputs and generative templates fail before any provider work", {
  for (name in c("ID1", "ID2", "pair_uid", "trait_name", "trait_description", "model")) {
    for (value in list(NA_character_, "", " ", character(), c("A", "B"), 1, matrix("x"))) {
      expect_error(do.call(decision_333_request, stats::setNames(list(value), name)), "scalar")
    }
  }
  for (name in c("text1", "text2")) {
    for (value in list(NULL, NA_character_, 1, c("a", "b"))) {
      expect_error(do.call(decision_333_request, stats::setNames(list(value), name)), "text1.*text2")
    }
  }
  expect_error(decision_333_request(ID2 = "A_with_vs_delimiter"), "self-pairs")
  expect_error(decision_333_request(provider = "other"), "arg")
  expect_error(decision_333_request(prompt_template = set_prompt_template()), "Generative")
  for (template in list(NA_character_, 1, "missing placeholders", set_prompt_template(),
                        "{TRAIT_NAME} {TRAIT_DESCRIPTION} <BETTER_SAMPLE>",
                        "{TRAIT_NAME} {TRAIT_DESCRIPTION} {SAMPLE_2}")) {
    expect_error(decision_333_request(decision_template = template), "decision_template")
  }
})

test_that("historical templates and generative prompt bytes remain unchanged", {
  path <- system.file("templates", "default.txt", package = "pairwiseLLM")
  # Match historical readLines rendering, including on CRLF checkouts.
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_identical(pairwiseLLM:::.decision_sha256(text),
                   "44eacfee581b6a0451d992f97fe3d1c28691e02a38913d19690abecaa5cc57cb")
  before <- set_prompt_template()
  legacy <- build_prompt(before, "Organization", "Logical order.", "alpha", "beta")
  decision_333_request()
  expect_identical(set_prompt_template(), before)
  expect_identical(get_prompt_template("default"), before)
  expect_identical(build_prompt(before, "Organization", "Logical order.", "alpha", "beta"), legacy)
  expect_false("decision_cj_v1" %in% list_prompt_templates())
})

test_that("both generative endpoints keep their frozen XML request and winner", {
  response <- list(model = "gpt-4.1", object = "chat.completion",
                   choices = list(list(message = list(content = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>"))),
                   output = list(list(type = "message", content = list(list(text = "<BETTER_SAMPLE>SAMPLE_2</BETTER_SAMPLE>")))))
  captured <- NULL
  local_mocked_bindings(
    .openai_api_key = function(...) "synthetic-key",
    .openai_req_body_json = function(req, body) { captured <<- body; req },
    .openai_req_perform = function(req) structure(list(), class = "synthetic_response"),
    .openai_resp_body_json = function(...) response,
    .openai_resp_status = function(...) 200L,
    .package = "pairwiseLLM"
  )
  for (endpoint in c("chat.completions", "responses")) {
    decision_333_request()
    row <- openai_compare_pair_live("A", "alpha", "B", "beta", "gpt-4.1",
                                    "Organization", "Logical order.", endpoint = endpoint)
    text <- if (endpoint == "chat.completions") captured$messages[[1]]$content else captured$input
    expect_identical(pairwiseLLM:::.decision_sha256(text),
                     "f9cea0213306c7cf4217a15962595e8eaaa08870f63bc240140c6d89d6c38626")
    expect_identical(row$better_sample, "SAMPLE_2")
    expect_identical(row$better_id, "B")
  }
})
