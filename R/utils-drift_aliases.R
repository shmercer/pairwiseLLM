.bt_add_drift_aliases <- function(m) {
  if (is.null(m) || !is.data.frame(m) || nrow(m) == 0L) {
    return(m)
  }

  # Make sure core_n exists
  if (!("core_n" %in% names(m))) m$core_n <- NA_integer_

  suffixes <- c(
    "n",
    "flip_applied",
    "mean_abs_shift",
    "max_abs_shift",
    "mean_signed_shift",
    "p90_abs_shift",
    "p95_abs_shift",
    "theta_cor",
    "theta_spearman"
  )

  for (sfx in suffixes) {
    core_nm <- paste0("core_", sfx)
    link_nm <- paste0("linking_", sfx)

    if ((core_nm %in% names(m)) && !(link_nm %in% names(m))) {
      m[[link_nm]] <- m[[core_nm]]
    }
    if ((link_nm %in% names(m)) && !(core_nm %in% names(m))) {
      m[[core_nm]] <- m[[link_nm]]
    }
  }

  m
}
