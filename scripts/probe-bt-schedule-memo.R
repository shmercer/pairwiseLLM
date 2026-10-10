#!/usr/bin/env Rscript
# Diagnostic observation only, with the same 11 arguments as the cell runner.
# Record exact inputs to the runtime memo hasher without changing its return.
# This is not a timing cell and is never installed into any package library.
library(pairwiseLLM)
probe_arguments <- commandArgs(TRUE)
stopifnot(length(probe_arguments) == 11L, as.integer(probe_arguments[[7L]]) == 1L)
memo_contexts_325 <- new.env(parent = emptyenv())
original_memo_hash_325 <- pairwiseLLM:::.adaptive_runtime_hash_object
memo_observer_325 <- local({
  original <- original_memo_hash_325
  contexts <- memo_contexts_325
  function(x) {
    value <- original(x)
    if (is.list(x) && all(c("set_id", "requested_source", "required_config_hash") %in% names(x))) {
      assign(value, x, envir = contexts)
    }
    value
  }
})
utils::assignInNamespace(".adaptive_runtime_hash_object", memo_observer_325, ns = "pairwiseLLM")
source("scripts/benchmark-bt-schedule-cell.R")
utils::assignInNamespace(".adaptive_runtime_hash_object", original_memo_hash_325, ns = "pairwiseLLM")
saveRDS(as.list(memo_contexts_325, all.names = TRUE), file.path(probe_arguments[[1L]], "memo-contexts.rds"))
