#' Extract frozen writing features for warm-start prediction
#'
#' Warm-start prediction uses text features to predict relative writing quality
#' before collecting pairwise comparisons. This function extracts the frozen
#' English writing features; it does not train a model or estimate BTL scores.
#'
#' @param ids Unique, nonmissing character or finite numeric item IDs. IDs are
#'   returned as character strings; blank IDs are not allowed.
#' @param texts A nonempty character vector of the same length as `ids`, without
#'   missing values. Empty strings are allowed. Text is never trimmed or normalized.
#' @param schema The frozen schema identifier, currently `"writing_features_v1"`.
#' @param python Optional path to an existing Python interpreter. With `NULL`, use
#'   an existing environment selected through reticulate. Automatic environment
#'   creation is disabled. Conflicting interpreter selections require correction
#'   or a fresh R session; this function never switches an initialized interpreter.
#'
#' @return A tibble with character `item_id` and the 20 numeric features in schema
#'   order, in the requested ID order. The `warm_start_schema` attribute records
#'   the schema identifier. Document-level undefined values remain `NA`.
#'
#' @details
#' Extraction requires optional reticulate and the audited Python 3.12.3 stack.
#' See [warm_start_python_status()] and the installed setup instructions located by
#' `system.file("python", "README.md", package = "pairwiseLLM")`.
#' Software and resources must be installed explicitly before extraction. Package
#' loading, schema inspection, and later prediction from precomputed features do
#' not require Python. The tested environment is Linux x86_64; Windows and macOS
#' have not been validated. Features are not evidence of predictive validity.
#'
#' The default English spaCy model, resource contents and package versions are
#' checked before extraction. Missing resources fail without downloads. Entropy
#' is divided by all spaCy tokens, including punctuation and whitespace, rather
#' than the filtered `n_tokens` feature. Undefined values and valid zeros follow
#' [warm_start_feature_schema()]. Zero-vector coherence preserves upstream values
#' and warnings. No feature is imputed or replaced with zero.
#'
#' @examples
#' \dontrun{
#' # Explicitly provision the documented environment first.
#' extract_warm_start_features(c("a", "b"), c("A short text.", "Another text."),
#'   python = "/path/to/venv/bin/python")
#' }
#' @export
extract_warm_start_features <- function(ids, texts, schema = "writing_features_v1",
                                        python = NULL) {
  definition <- warm_start_feature_schema(schema)
  ids <- .warm_start_ids(ids)
  if (!is.character(texts) || !is.null(dim(texts)) || length(texts) != length(ids) ||
      anyNA(texts)) {
    rlang::abort("`texts` must be a nonmissing character vector with one text per ID.")
  }
  result <- .warm_start_python_request(list(
    operation = "extract", schema = schema, ids = as.list(ids), texts = as.list(texts)
  ), python)
  out <- .warm_start_decode_features(result)
  .validate_warm_start_features(out, ids, schema, definition)
}

.warm_start_ids <- function(ids) {
  if ((!is.character(ids) && !is.numeric(ids)) || is.object(ids) ||
      !is.null(dim(ids)) || length(ids) == 0L || anyNA(ids) ||
      (is.numeric(ids) && any(!is.finite(ids)))) {
    rlang::abort("IDs must be a nonempty character or finite numeric vector without missing values.")
  }
  ids <- as.character(ids)
  if (any(!nzchar(trimws(ids))) || anyDuplicated(ids)) {
    rlang::abort("IDs must be unique and nonblank.")
  }
  ids
}

.warm_start_decode_features <- function(result) {
  invalid <- function() rlang::abort("Malformed Python feature response: invalid columns or rows.")
  columns <- result$columns
  if (!is.list(columns) || !length(columns) ||
      any(!vapply(columns, function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1)))) {
    invalid()
  }
  columns <- unlist(columns, use.names = FALSE)
  if (anyDuplicated(columns) || !"item_id" %in% columns || !is.list(result$rows) ||
      !length(result$rows)) {
    invalid()
  }
  if (any(!vapply(result$rows, function(x) is.list(x) && length(x) == length(columns), logical(1)))) {
    invalid()
  }
  retained <- which(columns %in% c("item_id", warm_start_feature_schema()$feature))
  out <- lapply(retained, function(i) {
    values <- lapply(result$rows, `[[`, i)
    if (columns[i] == "item_id") {
      if (any(!vapply(values, function(x) is.character(x) && length(x) == 1L, logical(1)))) invalid()
      return(unlist(values, use.names = FALSE))
    }
    if (any(!vapply(values, function(x) is.null(x) || (is.numeric(x) && length(x) == 1L), logical(1)))) {
      invalid()
    }
    vapply(values, function(x) if (is.null(x)) NA_real_ else as.double(x), numeric(1))
  })
  names(out) <- columns[retained]
  out <- tibble::as_tibble(out)
  attr(out, "warm_start_schema") <- result$schema
  out
}

# Shared with later precomputed-feature prediction; no Python calls here.
.validate_warm_start_features <- function(features, ids, schema = "writing_features_v1",
                                          definition = warm_start_feature_schema(schema)) {
  ids <- .warm_start_ids(ids)
  if (!is.data.frame(features) || anyDuplicated(names(features))) {
    rlang::abort("Features must be a data frame with unique column names.")
  }
  if (!identical(attr(features, "warm_start_schema"), schema)) {
    rlang::abort("Feature schema mismatch: supply features with matching `warm_start_schema` metadata.")
  }
  required <- c("item_id", definition$feature)
  missing <- setdiff(required, names(features))
  if (length(missing)) {
    rlang::abort(paste("Missing required feature columns:", paste(missing, collapse = ", ")))
  }
  returned_ids <- .warm_start_ids(features$item_id)
  if (nrow(features) != length(ids) || !setequal(returned_ids, ids)) {
    rlang::abort("Feature IDs must match requested IDs exactly, with one row per ID.")
  }
  out <- tibble::as_tibble(features[match(ids, returned_ids), required, drop = FALSE])
  out$item_id <- ids
  for (name in definition$feature) {
    value <- out[[name]]
    if (!is.numeric(value) || is.object(value) || !is.null(dim(value)) || any(is.infinite(value))) {
      rlang::abort(paste0("Feature '", name, "' must be numeric, finite or missing."))
    }
  }
  if (anyNA(out$n_tokens) || any(out$n_tokens < 0 | out$n_tokens != floor(out$n_tokens))) {
    rlang::abort("Feature 'n_tokens' must contain nonmissing nonnegative whole counts.")
  }
  if (anyNA(out$dale_chall_readability_score)) {
    rlang::abort("Feature 'dale_chall_readability_score' must not be missing.")
  }
  for (name in c("proportion_unique_tokens", "token_length_mean", "token_length_std")) {
    if (any(is.na(out[[name]]) != (out$n_tokens == 0))) {
      rlang::abort(paste0("Feature '", name, "' has missingness inconsistent with n_tokens."))
    }
  }
  attr(out, "warm_start_schema") <- schema
  out
}
