test_that("writing_features_v1 is ordered, explicit, and independent of Python", {
  withr::local_envvar(c(RETICULATE_PYTHON = "/nonexistent/python"))
  schema <- warm_start_feature_schema()
  expected <- c(
    "n_tokens", "proportion_unique_tokens", "token_length_mean", "token_length_std",
    "sentence_length_mean", "sentence_length_std", "pos_prop_noun", "pos_prop_verb",
    "pos_prop_adj", "pos_prop_adv", "pos_prop_pron", "pos_prop_adp", "pos_prop_cconj",
    "pos_prop_sconj", "dependency_distance_mean", "dependency_distance_std",
    "prop_adjacent_dependency_relation_mean", "upstream_entropy_per_token",
    "first_order_coherence", "dale_chall_readability_score"
  )
  expect_s3_class(schema, "tbl_df")
  expect_identical(schema$feature, expected)
  upstream <- expected
  upstream[7:14] <- paste0(
    "pos_prop_", c("NOUN", "VERB", "ADJ", "ADV", "PRON", "ADP", "CCONJ", "SCONJ")
  )
  upstream[18] <- "entropy"
  expect_identical(schema$upstream_field, upstream)
  expect_identical(schema$position, seq_along(expected))
  expect_identical(unique(schema$schema), "writing_features_v1")
  expect_identical(schema, warm_start_feature_schema("writing_features_v1"))
  expect_false(anyNA(schema))
  expect_true(all(nzchar(trimws(as.matrix(schema)))))
  expect_identical(anyDuplicated(schema$feature), 0L)
  expect_identical(anyDuplicated(schema[c("source_package", "component", "upstream_field")]), 0L)
  expect_identical(schema$upstream_field[7:14], paste0(
    "pos_prop_", c("NOUN", "VERB", "ADJ", "ADV", "PRON", "ADP", "CCONJ", "SCONJ")
  ))
  expect_identical(schema$upstream_field[18], "entropy")
  expect_identical(schema$source_package, c(rep("textdescriptives", 19), "textstat"))
  expect_identical(schema$source_version, c(rep("2.8.4", 19), "0.7.13"))
  expect_match(schema$definition[18], "len\\(doc\\)")
  expect_match(schema$missing_behavior[18], "zero tokens")
})

test_that("schema lookup rejects malformed and unsupported identifiers", {
  for (value in list(NULL, character(), c("a", "b"), NA_character_, "", 1, TRUE, NA)) {
    expect_error(warm_start_feature_schema(value), "one nonmissing, nonempty character string")
  }
  for (value in c("writing_features_v2", "../writing_features_v1", " writing_features_v1")) {
    expect_error(warm_start_feature_schema(value), "Unknown feature schema")
  }
})

test_that("missing and corrupt installed artifacts give actionable failures", {
  read_schema <- pairwiseLLM:::.read_warm_start_feature_schema
  for (path in c("", tempfile())) {
    expect_error(read_schema(path, "writing_features_v1"), "missing.*Reinstall")
  }
  schema <- as.data.frame(warm_start_feature_schema())
  path <- tempfile(fileext = ".csv")
  withr::defer(unlink(path))
  check_bad <- function(x) {
    utils::write.csv(x, path, row.names = FALSE, na = "")
    expect_error(read_schema(path, "writing_features_v1"), "corrupt or incompatible.*Reinstall")
  }
  check_bad(schema[FALSE, ])
  check_bad(schema[-1])
  check_bad(schema[c(2, 1, 3:20), ])
  for (column in names(schema)) {
    bad <- schema
    bad[[column]][1] <- NA
    check_bad(bad)
    bad[[column]][1] <- " "
    check_bad(bad)
  }
  for (change in list(
    list(column = "schema", value = "v2"),
    list(column = "position", value = "01"),
    list(column = "feature", value = "Invalid name"),
    list(column = "feature", value = schema$feature[2]),
    list(column = "type", value = "logical")
  )) {
    bad <- schema
    bad[[change$column]][1] <- change$value
    check_bad(bad)
  }
  bad <- schema
  bad[2, c("component", "upstream_field")] <- bad[1, c("component", "upstream_field")]
  check_bad(bad)
  writeLines('"unterminated', path)
  expect_error(read_schema(path, "writing_features_v1"), "corrupt or incompatible")
  writeLines(c("a,b", "1,2,3", "4,5,6,7"), path)
  expect_error(read_schema(path, "writing_features_v1"), "corrupt or incompatible")
})
