# Training-local helpers accept numeric matrices, not raw feature tables.
.warm_start_number <- function(x, lower = -Inf, upper = Inf) {
  is.numeric(x) && !is.object(x) && is.null(dim(x)) && length(x) == 1L &&
    is.finite(x) && x >= lower && x <= upper
}

.warm_start_matrix <- function(x, missing = TRUE) {
  if (!is.matrix(x) || !is.numeric(x) || nrow(x) < 1L || ncol(x) < 1L ||
      is.null(colnames(x)) || anyNA(colnames(x)) || any(!nzchar(colnames(x))) ||
      anyDuplicated(colnames(x)) || any(is.infinite(x)) || (!missing && anyNA(x))) {
    rlang::abort("Predictors must be a nonempty numeric matrix with unique names and valid values.")
  }
  invisible(x)
}

.warm_start_preprocess_control <- function(missing_threshold, unique_threshold, frequency_ratio) {
  if (!.warm_start_number(missing_threshold, 0, 1) ||
      !.warm_start_number(unique_threshold, 0, 1) ||
      !.warm_start_number(frequency_ratio, 1)) {
    rlang::abort("Preprocessing requires missing/unique thresholds in [0, 1] and frequency ratio >= 1.")
  }
  list(missing_threshold = missing_threshold, unique_threshold = unique_threshold,
    frequency_ratio = frequency_ratio)
}

.warm_start_preprocess_fit <- function(x, missing_threshold = 0.20,
                                       unique_threshold = 0.10, frequency_ratio = 19) {
  .warm_start_matrix(x)
  if (nrow(x) < 2L) rlang::abort("Preprocessing requires at least two training rows.")
  control <- .warm_start_preprocess_control(missing_threshold, unique_threshold, frequency_ratio)
  missing_fraction <- colMeans(is.na(x))
  reasons <- stats::setNames(rep(NA_character_, ncol(x)), colnames(x))
  reasons[missing_fraction == 1] <- "all_missing"
  reasons[is.na(reasons) & missing_fraction > missing_threshold] <- "missingness"
  candidates <- names(reasons)[is.na(reasons)]
  medians <- centers <- scales <- stats::setNames(numeric(), character())
  for (name in candidates) {
    value <- x[, name]
    median <- stats::median(value, na.rm = TRUE)
    value[is.na(value)] <- median
    counts <- sort(table(value), decreasing = TRUE)
    if (length(counts) == 1L) {
      reasons[name] <- "constant"
    } else if (length(counts) / length(value) <= unique_threshold &&
               counts[1] / counts[2] > frequency_ratio) {
      reasons[name] <- "near_zero_variance"
    } else {
      medians[name] <- median
      centers[name] <- mean(value)
      scales[name] <- stats::sd(value)
    }
  }
  if (!length(medians)) {
    rlang::abort(paste0("No predictors survive preprocessing: ",
      paste(paste(names(reasons), reasons, sep = "="), collapse = ", "), "."))
  }
  out <- list(features = colnames(x), retained = names(medians), removed = reasons[!is.na(reasons)],
    missing_fraction = missing_fraction, medians = medians, centers = centers, scales = scales,
    control = control, sd_convention = "sample", n_training = nrow(x))
  .validate_warm_start_preprocess(out)
  out
}

.warm_start_names <- function(x) {
  is.character(x) && is.null(dim(x)) && length(x) > 0L && !anyNA(x) &&
    all(nzchar(x)) && !anyDuplicated(x)
}

.warm_start_named_numeric <- function(x, names) {
  is.numeric(x) && !is.object(x) && is.null(dim(x)) && identical(names(x), names) &&
    all(is.finite(x))
}

.validate_warm_start_preprocess <- function(x) {
  invalid <- function() rlang::abort("Invalid warm-start preprocessing contract.")
  if (!is.list(x) || !.warm_start_names(x$features) || !.warm_start_names(x$retained) ||
      !identical(x$retained, x$features[x$features %in% x$retained]) ||
      !is.character(x$removed) || is.null(names(x$removed)) ||
      !identical(names(x$removed), setdiff(x$features, x$retained)) || anyNA(x$removed) ||
      any(!x$removed %in% c("all_missing", "missingness", "constant", "near_zero_variance")) ||
      !.warm_start_named_numeric(x$missing_fraction, x$features) ||
      any(x$missing_fraction < 0 | x$missing_fraction > 1) ||
      !identical(x$sd_convention, "sample") || !.warm_start_number(x$n_training, 2) ||
      x$n_training != floor(x$n_training)) invalid()
  for (field in c("medians", "centers", "scales")) {
    if (!.warm_start_named_numeric(x[[field]], x$retained)) invalid()
  }
  if (any(x$scales <= 0) || !is.list(x$control)) invalid()
  .warm_start_preprocess_control(x$control$missing_threshold, x$control$unique_threshold,
    x$control$frequency_ratio)
  invisible(x)
}

.warm_start_preprocess_apply <- function(x, preprocessing) {
  .validate_warm_start_preprocess(preprocessing)
  .warm_start_matrix(x)
  if (!all(preprocessing$features %in% colnames(x))) {
    rlang::abort("Missing original predictor columns when applying preprocessing.")
  }
  out <- x[, preprocessing$retained, drop = FALSE]
  for (name in preprocessing$retained) {
    out[is.na(out[, name]), name] <- preprocessing$medians[name]
    out[, name] <- (out[, name] - preprocessing$centers[name]) / preprocessing$scales[name]
  }
  .warm_start_matrix(out, missing = FALSE)
  out
}

.warm_start_outcome_values <- function(theta) {
  if (!is.numeric(theta) || is.object(theta) || !is.null(dim(theta)) || !length(theta) ||
      any(!is.finite(theta))) {
    rlang::abort("`theta` must be a nonempty finite numeric vector.")
  }
  invisible(theta)
}

.validate_warm_start_outcome <- function(x) {
  if (!is.list(x) || !identical(x$definition, "within_task_z") ||
      !identical(x$sd_convention, "sample") || !.warm_start_number(x$mean) ||
      !.warm_start_number(x$sd) || x$sd <= 0) {
    rlang::abort("Outcome scaling requires a finite mean and positive sample SD on within_task_z.")
  }
  invisible(x)
}

.warm_start_outcome_fit <- function(theta) {
  .warm_start_outcome_values(theta)
  out <- list(definition = "within_task_z", mean = mean(theta), sd = stats::sd(theta),
    sd_convention = "sample")
  .validate_warm_start_outcome(out)
  out
}

.warm_start_outcome_apply <- function(theta, outcome) {
  .warm_start_outcome_values(theta)
  .validate_warm_start_outcome(outcome)
  z <- (theta - outcome$mean) / outcome$sd
  if (any(!is.finite(z))) rlang::abort("Outcome scaling produced nonfinite values.")
  z
}
