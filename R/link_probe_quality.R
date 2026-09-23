# Shared held-out probe metric arithmetic; thresholds are unchanged.
.link_probe_quality <- function(p, y, hub_items, spoke_items, hub_bins = integer(),
                                spoke_bins = integer(), controller = list()) {
  controller <- controller %||% list()
  min_required <- max(1L, as.integer(controller$probe_edges_min_for_stop %||% 80L))
  unique_hub_min <- as.integer(ceiling(as.double(controller$probe_unique_hub_min_frac %||% 0.60) * min_required))
  unique_spoke_min <- as.integer(ceiling(as.double(controller$probe_unique_spoke_min_frac %||% 0.75) * min_required))
  rank_bins <- max(1L, as.integer(controller$probe_rank_bins %||% 10L))
  rank_bins_hub_min <- min(
    rank_bins,
    min_required,
    max(1L, as.integer(controller$probe_rank_bins_hub_min %||% 8L))
  )
  rank_bins_spoke_min <- min(
    rank_bins,
    min_required,
    max(1L, as.integer(controller$probe_rank_bins_spoke_min %||% 8L))
  )
  out <- list(
    probe_near_boundary_frac = NA_real_,
    probe_near_boundary_min_frac_used = as.double(controller$probe_near_boundary_min_frac %||% 0.35),
    probe_near_boundary_pass = FALSE,
    probe_extreme_frac = NA_real_,
    probe_extreme_max_frac_used = as.double(controller$probe_extreme_max_frac %||% 0.30),
    probe_extreme_frac_pass = FALSE,
    probe_midrange_frac = NA_real_,
    probe_midrange_min_frac_used = as.double(controller$probe_midrange_min_frac %||% 0.60),
    probe_midrange_pass = FALSE,
    probe_unique_hub_items = 0L,
    probe_unique_hub_min_used = as.integer(unique_hub_min),
    probe_unique_hub_pass = FALSE,
    probe_unique_spoke_items = 0L,
    probe_unique_spoke_min_used = as.integer(unique_spoke_min),
    probe_unique_spoke_pass = FALSE,
    probe_rank_bins_hub_covered = 0L,
    probe_rank_bins_hub_min_used = as.integer(rank_bins_hub_min),
    probe_rank_bins_hub_pass = FALSE,
    probe_rank_bins_spoke_covered = 0L,
    probe_rank_bins_spoke_min_used = as.integer(rank_bins_spoke_min),
    probe_rank_bins_spoke_pass = FALSE,
    probe_brier_near_boundary = NA_real_,
    probe_brier_near_boundary_max_used = as.double(controller$probe_brier_near_boundary_max %||% 0.20),
    probe_brier_near_boundary_pass = FALSE,
    probe_ece = NA_real_,
    probe_ece_max_used = as.double(controller$probe_ece_max %||% 0.10),
    probe_ece_pass = FALSE,
    probe_quality_pass = FALSE,
    probe_quality_blocker_codes = "probe_quality_unavailable"
  )
  if (!length(p)) return(out)
  near <- p >= as.double(controller$probe_near_boundary_low %||% 0.35) &
    p <= as.double(controller$probe_near_boundary_high %||% 0.65)
  extreme <- p < as.double(controller$probe_extreme_low %||% 0.15) |
    p > as.double(controller$probe_extreme_high %||% 0.85)
  midrange <- p >= as.double(controller$probe_midrange_low %||% 0.20) &
    p <= as.double(controller$probe_midrange_high %||% 0.80)
  out$probe_near_boundary_frac <- mean(near)
  out$probe_extreme_frac <- mean(extreme)
  out$probe_midrange_frac <- mean(midrange)
  out$probe_unique_hub_items <- length(unique(as.character(hub_items)))
  out$probe_unique_spoke_items <- length(unique(as.character(spoke_items)))
  out$probe_rank_bins_hub_covered <- length(hub_bins)
  out$probe_rank_bins_spoke_covered <- length(spoke_bins)
  out$probe_brier_near_boundary <- if (any(near)) mean((y[near] - p[near])^2) else NA_real_
  out$probe_ece <- .adaptive_link_probe_calibration_ece(p, y, n_bins = 5L)
  out$probe_near_boundary_pass <- out$probe_near_boundary_frac >= out$probe_near_boundary_min_frac_used
  out$probe_extreme_frac_pass <- out$probe_extreme_frac <= out$probe_extreme_max_frac_used
  out$probe_midrange_pass <- out$probe_midrange_frac >= out$probe_midrange_min_frac_used
  out$probe_unique_hub_pass <- out$probe_unique_hub_items >= out$probe_unique_hub_min_used
  out$probe_unique_spoke_pass <- out$probe_unique_spoke_items >= out$probe_unique_spoke_min_used
  out$probe_rank_bins_hub_pass <- out$probe_rank_bins_hub_covered >= out$probe_rank_bins_hub_min_used
  out$probe_rank_bins_spoke_pass <- out$probe_rank_bins_spoke_covered >= out$probe_rank_bins_spoke_min_used
  out$probe_brier_near_boundary_pass <- is.finite(out$probe_brier_near_boundary) &&
    out$probe_brier_near_boundary <= out$probe_brier_near_boundary_max_used
  out$probe_ece_pass <- is.finite(out$probe_ece) && out$probe_ece <= out$probe_ece_max_used
  passes <- c(
    probe_near_boundary = out$probe_near_boundary_pass,
    probe_extreme_frac = out$probe_extreme_frac_pass,
    probe_midrange = out$probe_midrange_pass,
    probe_unique_hub = out$probe_unique_hub_pass,
    probe_unique_spoke = out$probe_unique_spoke_pass,
    probe_rank_bins_hub = out$probe_rank_bins_hub_pass,
    probe_rank_bins_spoke = out$probe_rank_bins_spoke_pass,
    probe_brier_near_boundary = out$probe_brier_near_boundary_pass,
    probe_ece = out$probe_ece_pass
  )
  blockers <- names(passes)[!as.logical(passes)]
  out$probe_quality_pass <- length(blockers) < 1L
  out$probe_quality_blocker_codes <- if (out$probe_quality_pass) "none" else paste(blockers, collapse = ",")
  out
}
