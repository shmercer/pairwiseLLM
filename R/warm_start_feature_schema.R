#' Inspect the frozen warm-start writing feature schema
#'
#' Warm-start prediction uses text features to predict relative writing quality
#' before collecting pairwise comparisons. This function lists the frozen
#' predictor definitions; it does not extract features or require Python.
#'
#' @param schema A single schema identifier. Currently only
#'   `"writing_features_v1"` is supported.
#'
#' @return A tibble with one row per feature in fixed predictor order. Columns:
#'   * `schema`, `position`, `feature`: version, integer order, and canonical name.
#'   * `source_package`, `source_version`, `component`, `upstream_field`:
#'     audited upstream mapping.
#'   * `family`, `type`, `unit`, `interpretation`: meaning of the feature.
#'   * `configuration`, `definition`, `requirements`, `missing_behavior`:
#'     fixed calculation settings, formula, capabilities, and undefined values.
#'   * `source_url`: upstream implementation reference.
#'
#' @details
#' Version 1 describes English writing using TextDescriptives and a supplementary
#' textstat readability measure. The schema records definitions, not evidence
#' of predictive validity for any particular writing population.
#'
#' `upstream_entropy_per_token` divides TextDescriptives' entropy by the number
#' of all spaCy tokens, including punctuation and whitespace tokens. This is an
#' average of upstream probability-weighted contributions, not normalized
#' document Shannon entropy or language-model cross-entropy. Its denominator
#' differs from the filtered `n_tokens` feature. Zero tokens give a missing value.
#'
#' Definitions, settings, membership, and ordering are frozen. Changes require
#' a new schema identifier rather than silently modifying version 1. Optional
#' Python software is needed only for extraction, not schema inspection
#' or prediction from precomputed features.
#'
#' @examples
#' features <- warm_start_feature_schema()
#' features[c("position", "feature", "family")]
#' @export
warm_start_feature_schema <- function(schema = "writing_features_v1") {
  if (!is.character(schema) || length(schema) != 1L || is.na(schema) || !nzchar(schema)) {
    rlang::abort("`schema` must be one nonmissing, nonempty character string.")
  }
  if (!identical(schema, "writing_features_v1")) {
    rlang::abort(paste0("Unknown feature schema '", schema, "'. Use 'writing_features_v1'."))
  }
  path <- system.file("warm-start", "feature-schema-writing-v1.csv", package = "pairwiseLLM")
  .read_warm_start_feature_schema(path, schema)
}

.read_warm_start_feature_schema <- function(path, schema) {
  if (!nzchar(path) || !file.exists(path)) {
    rlang::abort("The installed feature schema is missing. Reinstall pairwiseLLM.")
  }
  invalid <- function() {
    rlang::abort("The installed feature schema is corrupt or incompatible. Reinstall pairwiseLLM.")
  }
  out <- tryCatch(
    utils::read.csv(path, colClasses = "character", check.names = FALSE, na.strings = ""),
    error = function(e) invalid(),
    warning = function(w) invalid()
  )
  columns <- c(
    "schema", "position", "feature", "source_package", "source_version", "component",
    "upstream_field", "family", "type", "unit", "interpretation", "configuration",
    "definition", "requirements", "missing_behavior", "source_url"
  )
  if (!identical(names(out), columns) || nrow(out) == 0L || anyNA(out) ||
      any(!nzchar(trimws(as.matrix(out))))) {
    invalid()
  }
  if (any(out$schema != schema) ||
      !identical(out$position, as.character(seq_len(nrow(out)))) ||
      anyDuplicated(out$feature) ||
      anyDuplicated(out[c("source_package", "component", "upstream_field")]) ||
      any(!grepl("^[a-z][a-z0-9_]*$", out$feature)) ||
      any(!out$type %in% c("integer", "double"))) {
    invalid()
  }
  out$position <- as.integer(out$position)
  tibble::as_tibble(out)
}
