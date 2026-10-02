# Public synthetic fixtures only. Counts retain BOTH presentation directions.
lapse_case <- function(n = 4L, beta = 0.3, epsilon = 0.2, graph = "complete", seed = NULL,
                       total = 2000) {
  edges <- if (graph == "complete") {
    t(utils::combn(n, 2L))
  } else {
    path <- cbind(seq_len(n - 1L), 2:n)
    if (graph == "tree") path else unique(rbind(path, c(1, n), c(1, floor(n / 2) + 1), c(2, n)))
  }
  edges <- rbind(edges, edges[, 2:1, drop = FALSE])
  pairs <- data.frame(object1 = letters[edges[, 1L]], object2 = letters[edges[, 2L]])
  skeleton <- pairs[rep(seq_len(nrow(pairs)), each = 2L), , drop = FALSE]
  skeleton$result <- rep(c(1, 0), nrow(pairs))
  kernel <- pairwiseLLM:::.bt_lapse_design(skeleton)
  truth <- seq(-2, 2, length.out = n)
  truth <- truth - mean(truth)
  counts <- kernel$counts
  p <- (1 - epsilon) * plogis(truth[counts$first] - truth[counts$second] + beta) + epsilon / 2
  if (is.null(seed)) {
    wins <- total * p
  } else {
    withr::local_seed(seed)
    wins <- rbinom(length(p), total, p)
  }
  kernel$counts$wins <- wins
  kernel$counts$losses <- total - wins
  list(kernel = kernel, theta = truth, beta = beta, epsilon = epsilon, skeleton = skeleton)
}

lapse_fit_case <- function(case = lapse_case(), control = list()) {
  pairwiseLLM:::.bt_lapse_fit(case$kernel, pairwiseLLM:::.bt_lapse_control(list(control = control), FALSE))
}

lapse_binary_data <- function(case) {
  counts <- case$kernel$counts
  rows <- rep(seq_len(nrow(counts)), counts$wins + counts$losses)
  data.frame(object1 = case$kernel$ids[counts$first[rows]], object2 = case$kernel$ids[counts$second[rows]],
    result = unlist(Map(function(w, l) c(rep(1, w), rep(0, l)), counts$wins, counts$losses)))
}

lapse_error <- function(expr) tryCatch(expr, pairwiseLLM_bt_lapse_error = identity)
