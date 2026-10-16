# Synthetic single-pair transport fixtures; never contacts TypeSafe.
typesafe_335_args <- function(...) {
  args <- decision_334_args(model = "jev-1.13.0", pair_uid = "live-pair-335")
  args$endpoint <- NULL
  utils::modifyList(args, list(...), keep.null = TRUE)
}

typesafe_335_response <- function(body = decision_333_response("typesafe"), status = 200L) {
  httr2::response(
    status_code = status, url = "https://api.typesafe.ai/v1/systemone", method = "POST",
    headers = list(`Content-Type` = "application/json", `x-request-id` = "provider-335"),
    body = charToRaw(if (is.character(body)) body else pairwiseLLM:::.decision_json(body))
  )
}

typesafe_335_mutations <- function() {
  list(
    model = function(r) { r$body$model <- "jev-9.9.9"; r },
    text = function(r) { r$body$state <- sub("Synthetic sample A", "Altered sample A", r$body$state, fixed = TRUE); r },
    sample_order = function(r) {
      r$body$state <- pairwiseLLM:::.decision_json(rev(jsonlite::parse_json(r$body$state))); r
    },
    sample_assignment = function(r) {
      x <- jsonlite::parse_json(r$body$state)
      r$body$state <- pairwiseLLM:::.decision_json(list(SAMPLE_1 = x$SAMPLE_2, SAMPLE_2 = x$SAMPLE_1)); r
    },
    state_type = function(r) { r$body$state <- jsonlite::parse_json(r$body$state); r },
    instructions = function(r) { r$body$questions[[1]]$instructions <- "Changed instructions"; r },
    question_name = function(r) { names(r$body$questions) <- "another_trait"; r },
    question_type = function(r) { r$body$questions[[1]]$type <- "noul"; r },
    question_array = function(r) { r$body$questions <- unname(r$body$questions); r },
    criteria_order = function(r) { r$body$questions[[1]]$criteria <- rev(r$body$questions[[1]]$criteria); r },
    criteria_description = function(r) { r$body$questions[[1]]$criteria$SAMPLE_1 <- "Changed description"; r },
    criteria_label = function(r) { names(r$body$questions[[1]]$criteria)[1] <- "OTHER"; r },
    criteria_extra = function(r) { r$body$questions[[1]]$criteria$OTHER <- "Another option"; r },
    added_field = function(r) { r$body$instructions <- "extra scientific content"; r },
    missing_field = function(r) { r$body$model <- NULL; r },
    duplicate_field = function(r) { r$body <- c(r$body, list(model = r$body$model)); r },
    extra_question = function(r) { r$body$questions$another_trait <- r$body$questions[[1]]; r },
    fingerprint = function(r) { r$request_fingerprint <- "altered"; r },
    provenance = function(r) { r$provenance$trait_description <- "Altered trait"; r }
  )
}
