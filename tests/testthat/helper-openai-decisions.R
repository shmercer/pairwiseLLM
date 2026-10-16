# Synthetic, credential-free fixtures for the live transport boundary.
decision_334_args <- function(...) {
  args <- list(
    ID1 = "A_with_vs_delimiter", text1 = "Synthetic sample A.\nSAMPLE_2: a literal label.",
    ID2 = "B:02", text2 = "Synthetic sample B says \"hello\".",
    model = "gpt-6-luna", trait_name = "Organization",
    trait_description = "Logical sequencing and connections.\nExclude spelling and grammar.",
    endpoint = "decisions", api_key = "synthetic-fixture-key", pair_uid = "live-pair-334"
  )
  utils::modifyList(args, list(...), keep.null = TRUE)
}

decision_334_response <- function(body = decision_333_response(), status = 200L) {
  httr2::response(
    status_code = status, url = "https://api.openai.com/v1/decisions", method = "POST",
    headers = list(`Content-Type` = "application/json", `x-request-id` = "provider-334"),
    body = charToRaw(if (is.character(body)) body else pairwiseLLM:::.decision_json(body))
  )
}

decision_334_mutations <- function() {
  list(
    model = function(r) { r$body$model <- "changed-model"; r },
    text = function(r) { r$body$input <- sub("Synthetic sample A", "Altered sample A", r$body$input, fixed = TRUE); r },
    sample_order = function(r) {
      x <- jsonlite::parse_json(r$body$input)
      r$body$input <- pairwiseLLM:::.decision_json(rev(x)); r
    },
    sample_assignment = function(r) {
      x <- jsonlite::parse_json(r$body$input)
      r$body$input <- pairwiseLLM:::.decision_json(list(SAMPLE_1 = x$SAMPLE_2, SAMPLE_2 = x$SAMPLE_1)); r
    },
    instructions = function(r) { r$body$questions[[1]]$instructions <- "Changed instructions"; r },
    question_name = function(r) { r$body$questions[[1]]$name <- "another_trait"; r },
    question_type = function(r) { r$body$questions[[1]]$type <- "predicate"; r },
    choice_order = function(r) { r$body$questions[[1]]$choices <- rev(r$body$questions[[1]]$choices); r },
    choice_description = function(r) { r$body$questions[[1]]$choices[[1]]$description <- "Changed description"; r },
    choice_label = function(r) { r$body$questions[[1]]$choices[[1]]$value <- "OTHER"; r },
    added_field = function(r) { r$body$instructions <- "extra scientific content"; r },
    missing_field = function(r) { r$body$model <- NULL; r },
    duplicate_field = function(r) { r$body <- c(r$body, list(model = r$body$model)); r },
    extra_question = function(r) { r$body$questions <- rep(r$body$questions, 2L); r }
  )
}
