.call_user_fun <- function(fun, args) {
  # Call a user-supplied function but only pass arguments it can accept.
  # This makes it easier to provide lightweight mock functions in examples/tests.
  if (!is.function(fun)) {
    stop("`fun` must be a function.", call. = FALSE)
  }
  if (!is.list(args)) {
    stop("`args` must be a list.", call. = FALSE)
  }

  fmls <- tryCatch(formals(fun), error = function(e) NULL)
  if (is.null(fmls)) {
    return(do.call(fun, args))
  }

  fml_names <- names(fmls)
  if (is.null(fml_names) || "..." %in% fml_names) {
    return(do.call(fun, args))
  }

  nms <- names(args)
  if (is.null(nms)) {
    return(do.call(fun, args))
  }

  unnamed_idx <- which(is.na(nms) | nms == "")
  unnamed <- if (length(unnamed_idx)) args[unnamed_idx] else list()
  named_args <- if (length(unnamed_idx)) args[-unnamed_idx] else args

  keep <- names(named_args) %in% fml_names
  named_args <- named_args[keep]

  do.call(fun, c(unnamed, named_args))
}
