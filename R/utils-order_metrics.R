.bt_order_metrics <- function(metrics) {
  if (is.null(metrics)) {
    return(metrics)
  }
  if (!inherits(metrics, "data.frame")) {
    return(metrics)
  }

  first <- c("batch_index", "round_index", "stage", "stop", "stop_reason")
  first <- first[first %in% names(metrics)]
  rest <- setdiff(names(metrics), first)
  metrics[, c(first, sort(rest)), drop = FALSE]
}
