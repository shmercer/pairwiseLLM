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
  # Some non-primitive functions can still yield NULL formals (e.g., certain
  # wrapped/compiled functions). For closures we treat NULL formals as
  # "no arguments" to preserve the helper's intent of being permissive when
  # callers pass extra args.
  if (is.null(fmls)) {
    if (identical(typeof(fun), "closure")) {
      return(do.call(fun, list()))
    }
    return(do.call(fun, args))
  }

  # `formals()` can return an empty pairlist() for zero-argument functions.
  # In that case, drop all supplied args (named or unnamed) to avoid
  # unintended "unused argument" errors.
  if (length(fmls) == 0L) {
    return(do.call(fun, list()))
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
