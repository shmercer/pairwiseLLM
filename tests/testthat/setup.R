# Ensure E2E helpers are available even if helper loading is altered elsewhere.
source(testthat::test_path("helper-e2e.R"))

if (!"package:pairwiseLLM" %in% search()) {
  suppressWarnings(suppressMessages(library(pairwiseLLM)))
}

pll_ns <- asNamespace("pairwiseLLM")
exported_names <- getNamespaceExports("pairwiseLLM")
internal_names <- setdiff(ls(envir = pll_ns, all.names = TRUE), exported_names)

for (fn_name in internal_names) {
  obj <- get(fn_name, envir = pll_ns)
  if (is.function(obj) && !exists(fn_name, envir = .GlobalEnv, inherits = FALSE)) {
    assign(fn_name, obj, envir = .GlobalEnv)
  }
}

for (fn_name in exported_names) {
  obj <- get(fn_name, envir = pll_ns)
  if (is.function(obj) && !exists(fn_name, envir = .GlobalEnv, inherits = FALSE)) {
    assign(fn_name, obj, envir = .GlobalEnv)
  }
}
