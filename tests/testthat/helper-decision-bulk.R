# Offline bulk fixtures: transports are replaced inside each test.
decision_bulk_pairs <- function(n = 4L) {
  tibble::tibble(ID1 = rep("A", n), text1 = rep("Synthetic sample A.", n),
                 ID2 = rep("B", n), text2 = rep("Synthetic sample B.", n),
                 pair_uid = paste0("occurrence-", seq_len(n)))
}

decision_bulk_args <- function(provider = "openai", ...) {
  args <- list(pairs = decision_bulk_pairs(), model = if (provider == "openai") "gpt-6-luna" else "jev-1.13.0",
               trait_name = "Organization", trait_description = "Logical sequencing.",
               backend = provider, api_key = "synthetic-bulk-key", verbose = FALSE, progress = FALSE)
  if (provider == "openai") args$endpoint <- "decisions"
  extra <- list(...)
  args[names(extra)] <- extra
  args
}

decision_bulk_response <- function(provider = "openai", status = 200L,
                                    body = decision_333_response(provider), headers = list()) {
  httr2::response(status_code = status, method = "POST", body = charToRaw(pairwiseLLM:::.decision_json(body)),
                  headers = c(list(`Content-Type` = "application/json", `x-request-id` = "synthetic-provider-id"), headers))
}

# Clock and sleep are replaceable boundaries; no real delay is needed for quotas.
decision_bulk_clock <- function(env = parent.frame()) {
  clock <- new.env(parent = emptyenv())
  clock$now <- 1000
  testthat::local_mocked_bindings(
    .decision_live_now = function() clock$now,
    .decision_live_wait = function(seconds) { clock$now <- clock$now + seconds },
    .decision_retry_delay = function(attempt) 0.5 * 2^(attempt - 1L),
    .package = "pairwiseLLM", .env = env
  )
  clock
}

decision_bulk_interrupt <- function() {
  stop(structure(list(message = "Synthetic process interruption", call = NULL),
                 class = c("interrupt", "condition")))
}

decision_bulk_events <- function(path) {
  files <- sort(list.files(paste0(path, ".checkpoint"), pattern = "^[0-9]{12}\\.rds$", full.names = TRUE))
  lapply(files, function(file) pairwiseLLM:::.decision_record_read(file)$value)
}
