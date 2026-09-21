# Only iteration-limit failures at an unambiguous exact-path boundary are recoverable.
.warm_start_path_count <- function(requested, returned, jerr) {
  if (!.warm_start_number(jerr) || jerr != floor(jerr) ||
      jerr > 0 || jerr <= -10000 || (!is.null(requested) && -jerr > length(requested)) ||
      (is.null(requested) && jerr != 0)) {
    rlang::abort("Elastic-net path did not converge.")
  }
  # glmnet::getcoef represents a failure before any solution with lambda = Inf.
  empty <- jerr == -1 && identical(as.numeric(returned), Inf)
  count <- if (empty) 0L else length(returned)
  if (!is.numeric(returned) || !is.null(dim(returned)) ||
      (!empty && (any(!is.finite(returned)) || any(returned < 0) || any(diff(returned) >= 0))) ||
      (jerr == 0 && count == 0L) ||
      (jerr < 0 && count != -jerr - 1L) ||
      (!is.null(requested) && (count > length(requested) ||
        (jerr == 0 && count != length(requested)) ||
        (!empty && !isTRUE(all.equal(as.numeric(returned),
          as.numeric(requested[seq_len(count)]), tolerance = 1e-12)))))) {
    rlang::abort("Elastic-net path did not return a converged prefix at every requested lambda before failure.")
  }
  count
}

.warm_start_candidate_summary <- function(lambda, loss, sizes, eligible, rule) {
  indices <- which(eligible)
  cvm <- cvsd <- rep(NA_real_, length(lambda))
  choice <- list(index_min = NA_integer_, index_1se = NA_integer_, index = NA_integer_,
    lambda_min = NA_real_, lambda_1se = NA_real_)
  if (length(indices)) {
    errors <- .warm_start_loss_summary(loss[, indices, drop = FALSE], sizes)
    cvm[indices] <- errors$cvm
    cvsd[indices] <- errors$cvsd
    choice <- .warm_start_lambda_choice(lambda[indices], errors$cvm, errors$cvsd, rule)
    for (name in c("index_min", "index_1se", "index")) choice[[name]] <- indices[choice[[name]]]
  }
  c(list(cvm = cvm, cvsd = cvsd), choice)
}

.warm_start_candidate_record <- function(lambda, fold_lambda, fold_jerr, index) {
  counts <- vapply(seq_along(fold_lambda), function(fold) {
    .warm_start_path_count(lambda, fold_lambda[[fold]], fold_jerr[fold])
  }, integer(1))
  mask <- outer(counts, seq_along(lambda), `>=`)
  eligible <- colSums(mask) == nrow(mask)
  list(requested_lambda = lambda, fold_lambda = fold_lambda, fold_jerr = fold_jerr,
    fold_converged_count = counts, fold_eligible = mask, eligible = eligible,
    invalid_tail_count = sum(!eligible), alpha_eligible = any(eligible),
    selected_smallest_eligible = if (any(eligible)) isTRUE(index == max(which(eligible))) else NA)
}

.validate_warm_start_candidate_record <- function(record, trace, k) {
  invalid <- function() rlang::abort("Invalid warm-start candidate-validity contract.")
  if (!is.list(record) || !identical(record$requested_lambda, trace$lambda) ||
      !is.list(record$fold_lambda) || length(record$fold_lambda) != k ||
      !is.numeric(record$fold_jerr) || length(record$fold_jerr) != k ||
      !is.null(dim(record$fold_jerr))) invalid()
  expected <- .warm_start_candidate_record(trace$lambda, record$fold_lambda,
    record$fold_jerr, trace$index)
  if (!identical(record, expected) || !is.numeric(trace$fold_mse) ||
      any(!is.finite(trace$fold_mse[expected$fold_eligible])) ||
      any(trace$fold_mse[expected$fold_eligible] < 0) ||
      !identical(unname(trace$fold_mse[!expected$fold_eligible]),
        rep(NA_real_, sum(!expected$fold_eligible)))) invalid()
  expected$eligible
}
