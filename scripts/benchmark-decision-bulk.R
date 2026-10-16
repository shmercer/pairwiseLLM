#!/usr/bin/env Rscript
# Provider-free client overhead benchmark; no HTTP or process workers.
# Usage: Rscript scripts/benchmark-decision-bulk.R [n=1000] [workers=8] [save=false]
# Run in a fresh process under /usr/bin/time -v for peak RSS. Times do not
# estimate provider latency/throughput and are never used as test assertions.
main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  n <- if (length(args)) as.integer(args[1]) else 1000L
  workers <- if (length(args) > 1L) as.integer(args[2]) else 8L
  saved <- length(args) > 2L && identical(args[3], "true")
  stopifnot(is.finite(n), n > 0, is.finite(workers), workers > 0)
  pkgload::load_all(quiet = TRUE)
  pairs <- data.frame(ID1 = rep("A", n), ID2 = rep("B", n),
                      text1 = rep("Synthetic A.", n), text2 = rep("Synthetic B.", n),
                      pair_uid = paste0("benchmark-", seq_len(n)))
  body <- pairwiseLLM:::.decision_json(list(
    model = "jev-1.13.0", answers = list(writing_trait = list(
      type = "choice", choice = "SAMPLE_2", confidence = .2,
      probabilities = list(SAMPLE_1 = .3, SAMPLE_2 = .7))),
    usage = list(input_tokens = 32, output_tokens = 0)))
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    httr2::response(status_code = 200L, body = charToRaw(body))
  })
  folder <- tempfile("decision-benchmark-"); dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  path <- if (saved) file.path(folder, "results.csv") else NULL
  execute <- function() submit_llm_pairs(
    pairs, model = "jev-1.13.0", backend = "typesafe", trait_name = "Organization",
    trait_description = "Logical sequencing.", api_key = "synthetic-benchmark-key",
    verbose = FALSE, progress = FALSE, parallel = workers > 1L, workers = workers,
    save_path = path)
  elapsed <- system.time(out <- execute())[["elapsed"]]
  stopifnot(nrow(out$results) == n, calls == n)
  resume <- if (saved) system.time(again <- execute())[["elapsed"]] else NA_real_
  stopifnot(calls == n)
  files <- list.files(folder, recursive = TRUE, full.names = TRUE)
  print(data.frame(n = n, workers = workers, saved = saved, elapsed_seconds = elapsed,
                   client_pairs_per_second = n / elapsed, resume_seconds = resume,
                   journal_and_csv_bytes = sum(file.info(files)$size)))
}
main()
