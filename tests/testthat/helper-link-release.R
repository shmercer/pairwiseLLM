# Shared release fixtures: synthetic data only, independent of provider state.
link_release_args <- function(estimator) {
  args <- link_e3_args()
  args$estimator <- estimator
  if (estimator == "gaussian_posterior_bridge") args$phase_a <- link_e2_args()$phase_a
  if (estimator == "fixed_shape_offset") {
    args$phase_a <- lapply(link_e2_args()$phase_a, function(x) list(points = colMeans(x$draws)))
  }
  args
}

link_release_ids <- c("fixed_shape_offset", "gaussian_posterior_bridge", "joint_offset")

link_release_reflect <- function(args, presentation = FALSE) {
  flip <- function(x) {
    if (presentation) {
      old <- x
      x$A_set <- old$B_set
      x$A_item <- old$B_item
      x$B_set <- old$A_set
      x$B_item <- old$A_item
    }
    x$y_A <- 1L - x$y_A
    x
  }
  args$judge$beta <- -args$judge$beta
  args$cross <- flip(args$cross)
  for (k in c("hub", "spoke")) {
    x <- args$phase_a[[k]]
    if (!is.null(x$observations)) x$observations <- flip(x$observations)
    if (!presentation) {
      if (!is.null(x$points)) x$points <- -x$points
      if (!is.null(x$draws)) x$draws <- -x$draws
    }
    args$phase_a[[k]] <- x
  }
  args
}
