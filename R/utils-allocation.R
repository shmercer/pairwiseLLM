.bt_frac_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop("`", name, "` must be a single finite numeric value.", call. = FALSE)
  }
  as.numeric(x)
}

.bt_apply_allocation_fun <- function(allocation_fun,
                                     state,
                                     within_batch_frac,
                                     core_audit_frac) {
  if (is.null(allocation_fun)) {
    return(list(within_batch_frac = within_batch_frac, core_audit_frac = core_audit_frac))
  }
  if (!is.function(allocation_fun)) {
    stop("`allocation_fun` must be a function or NULL.", call. = FALSE)
  }

  out <- allocation_fun(state)
  if (is.null(out)) {
    return(list(within_batch_frac = within_batch_frac, core_audit_frac = core_audit_frac))
  }
  if (!is.list(out)) {
    stop("`allocation_fun` must return NULL or a list.", call. = FALSE)
  }

  if (!is.null(out$within_batch_frac)) {
    within_batch_frac <- .bt_frac_scalar(out$within_batch_frac, "within_batch_frac")
  }
  if (!is.null(out$core_audit_frac)) {
    core_audit_frac <- .bt_frac_scalar(out$core_audit_frac, "core_audit_frac")
  }

  # Clamp to [0, 1] and preserve the select_core_link_pairs() constraint
  within_batch_frac <- max(0, min(1, within_batch_frac))
  core_audit_frac <- max(0, min(1, core_audit_frac))
  if (within_batch_frac + core_audit_frac > 1) {
    within_batch_frac <- max(0, 1 - core_audit_frac)
  }

  list(within_batch_frac = within_batch_frac, core_audit_frac = core_audit_frac)
}
