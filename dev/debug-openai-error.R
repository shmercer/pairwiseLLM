# dev/debug-openai-error.R
#
# Minimal script to show the EXACT OpenAI error message for /v1/responses.
# Uses httr2's req_error(is_error = ...) to avoid throwing on HTTP 400,
# so we can always inspect the real response object.

library(httr2)
library(jsonlite)

api_key <- Sys.getenv("OPENAI_API_KEY")
if (!nzchar(api_key)) {
  stop("OPENAI_API_KEY is not set", call. = FALSE)
}

url <- "https://api.openai.com/v1/responses"

body <- list(
  model = "gpt-5.1",   # <-- change this if you want to test another model
  input = "Say hello.",
  reasoning = list(
    effort  = "low",
    summary = "auto"
  )
)

cat("\n=== Request Body ===\n")
cat(prettify(toJSON(body, auto_unbox = TRUE)), "\n\n")

req <- request(url) |>
  req_auth_bearer_token(api_key) |>
  req_body_json(body) |>
  # IMPORTANT: do not throw on HTTP errors, so we can inspect the body
  req_error(is_error = function(resp) FALSE)

resp <- req_perform(req)

cat("=== HTTP Status ===\n")
print(resp_status(resp))

cat("\n=== Raw Response Body ===\n")
raw_txt <- resp_body_string(resp, encoding = "UTF-8")
cat(raw_txt, "\n")

parsed <- tryCatch(fromJSON(raw_txt), error = function(e) NULL)

if (!is.null(parsed)) {
  cat("\n=== Parsed Response Body ===\n")
  print(parsed)

  if (!is.null(parsed$error)) {
    cat("\n=== Parsed Error Message ===\n")
    print(parsed$error$message)
    cat("\nError type: ", parsed$error$type, "\n", sep = "")
  }
} else {
  cat("\n[NOTE] Could not parse response as JSON.\n")
}

cat("\nDone.\n")
