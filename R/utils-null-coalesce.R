#' Internal helper: x %||% y
#'
#' Returns `x` if it is not NULL and not an empty string; otherwise returns `y`.
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  if (is.character(x) && length(x) == 1L && identical(x, "")) {
    return(y)
  }
  x
}
