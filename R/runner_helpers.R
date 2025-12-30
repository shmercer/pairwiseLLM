# Internal helpers for runner orchestration

# Coerce judge output into a tibble.
# Supported shapes:
#   * a data.frame/tibble
#   * a list with a `results` data.frame (as returned by submit_llm_pairs())
.coerce_judge_output <- function(x) {
  if (inherits(x, "data.frame")) {
    return(tibble::as_tibble(x))
  }

  if (is.list(x) && "results" %in% names(x) && inherits(x$results, "data.frame")) {
    return(tibble::as_tibble(x$results))
  }

  stop(
    "`judge_fun` must return a tibble/data.frame, or a list with a `results` element (data.frame).",
    call. = FALSE
  )
}

# Remove runner-reserved argument names from a `...` list that will be forwarded to fit_fun.
.clean_fit_dots <- function(dots,
                            reserved = c(
                              "verbose",
                              "engine",
                              "return_diagnostics",
                              "include_residuals",
                              "fit_verbose"
                            )) {
  if (!is.list(dots)) {
    return(list())
  }
  for (nm in reserved) if (nm %in% names(dots)) dots[[nm]] <- NULL
  dots
}
