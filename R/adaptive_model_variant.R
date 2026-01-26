# -------------------------------------------------------------------------
# Adaptive v3 model variants.
# -------------------------------------------------------------------------

#' Normalize adaptive model variant
#'
#' @keywords internal
#' @noRd
normalize_model_variant <- function(model_variant) {
  allowed <- c("btl", "btl_e", "btl_b", "btl_e_b")
  if (!is.character(model_variant) || length(model_variant) != 1L || is.na(model_variant)) {
    rlang::abort("`model_variant` must be a length-1 character value.")
  }
  if (!model_variant %in% allowed) {
    rlang::abort(paste0(
      "`model_variant` must be one of: ",
      paste(allowed, collapse = ", "),
      "."
    ))
  }
  model_variant
}

#' @keywords internal
#' @noRd
model_has_e <- function(model_variant) {
  model_variant %in% c("btl_e", "btl_e_b")
}

#' @keywords internal
#' @noRd
model_has_b <- function(model_variant) {
  model_variant %in% c("btl_b", "btl_e_b")
}
