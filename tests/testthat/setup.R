# Ensure helpers are available even if helper loading is altered elsewhere.
source(testthat::test_path("helper-fit-contract.R"))
source(testthat::test_path("helper-fixtures.R"))

if (!"pairwiseLLM" %in% loadedNamespaces()) {
  library(pairwiseLLM)
}

pkg_root <- normalizePath(testthat::test_path("..", ".."), winslash = "/", mustWork = TRUE)
ns_path <- normalizePath(getNamespaceInfo("pairwiseLLM", "path"), winslash = "/", mustWork = TRUE)
if (!identical(pkg_root, ns_path)) {
  rlang::abort(paste0(
    "Tests must run against the local package namespace. Loaded path: `",
    ns_path,
    "`, expected: `",
    pkg_root,
    "`."
  ))
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

skip_if_no_psock <- function() {
  ok <- tryCatch(
    {
      con <- base::serverSocket(0L)
      on.exit(base::close(con), add = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    testthat::skip("PSOCK server sockets unavailable in this environment.")
  }
}
