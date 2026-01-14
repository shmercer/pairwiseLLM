# -------------------------------------------------------------------------
# Internal helper: stable custom_id construction
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.pairwiseLLM_make_custom_id <- function(ID1, ID2, pair_uid = NULL) {
  ID1 <- as.character(ID1)
  ID2 <- as.character(ID2)

  legacy <- sprintf("LIVE_%s_vs_%s", ID1, ID2)

  if (is.null(pair_uid)) {
    return(legacy)
  }

  out <- as.character(pair_uid)
  missing <- is.na(out) | !nzchar(out)
  if (any(missing)) {
    out[missing] <- legacy[missing]
  }
  out
}
