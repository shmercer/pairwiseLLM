warm_v2_fixture <- function() {
  jsonlite::read_json(test_path("fixtures", "warm-start-features-v2", "golden.json"))
}

test_that("v2 schema is a frozen additive inventory with an exact byte identity", {
  withr::local_envvar(RETICULATE_PYTHON = "/nonexistent/python")
  v1 <- warm_start_feature_schema()
  v2 <- warm_start_feature_schema("writing_features_v2")
  additions <- c(
    "n_characters", "token_length_median", "sentence_length_median",
    "syllables_per_token_mean", "syllables_per_token_median", "syllables_per_token_std",
    "pos_prop_aux", "pos_prop_det", "pos_prop_part", "prop_adjacent_dependency_relation_std",
    "second_order_coherence", "gunning_fog", "lix", "textstat_char_count",
    "textstat_letter_count", "textstat_lexicon_count", "textstat_miniword_count",
    "textstat_syllable_count", "textstat_sentence_count", "textstat_polysyllabcount",
    "textstat_linsear_write_formula", "textstat_difficult_words", "textstat_gunning_fog",
    "textstat_spache_readability", "textstat_long_word_count", "textstat_monosyllabcount"
  )
  expect_identical(v2$feature, c(v1$feature, additions))
  expect_identical(v2$position, 1:46)
  expect_identical(v2[1:20, -1], v1[, -1])
  expect_identical(unique(v2$schema), "writing_features_v2")
  expect_false(anyNA(v2))
  checksum <- digest::digest(file = system.file("warm-start", "feature-schema-writing-v2.csv",
    package = "pairwiseLLM"), algo = "sha256")
  expect_identical(checksum, "d9f271abae50eeac6d06a9ab7304309932174d54193ba8fb26348e9e886b2492")
  manifest <- jsonlite::read_json(system.file("python", "schema-writing-v2.json", package = "pairwiseLLM"))
  expect_identical(manifest$schema_sha256, checksum)
  expect_identical(warm_v2_fixture()$schema_sha256, checksum)
})

test_that("v2 extraction decodes every field, aligns IDs and preserves upstream missingness", {
  fixture <- warm_v2_fixture()
  result <- fixture[c("schema", "columns", "rows")]
  ids <- vapply(result$rows, `[[`, character(1), 1L)
  local_mocked_bindings(.warm_start_python_request = function(request, python) {
    expect_identical(request$schema, "writing_features_v2")
    expect_identical(request$texts, fixture$texts)
    result
  }, .package = "pairwiseLLM")
  out <- extract_warm_start_features(ids, unlist(fixture$texts), "writing_features_v2")
  expect_identical(names(out), unlist(fixture$columns))
  expect_identical(out$item_id, ids)
  expect_identical(attr(out, "warm_start_schema"), "writing_features_v2")
  expect_identical(out, .warm_start_decode_features(result))
  shuffled <- out[rev(seq_len(nrow(out))), ]
  shuffled$ignored <- "extra"
  expect_identical(.validate_warm_start_features(shuffled, ids, "writing_features_v2"), out)
  expect_equal(out$n_characters[1], 0)
  expect_true(all(is.na(out$gunning_fog[out$n_tokens == 0])))
  expect_true(all(is.na(out$syllables_per_token_mean[out$n_tokens == 0])))
  expect_true(all(is.na(out$second_order_coherence[1:5])))
  expect_true(all(is.finite(out$textstat_linsear_write_formula)))
  expect_true(any(out$textstat_linsear_write_formula < 0))
  result$columns <- c(result$columns, list("upstream_text"))
  result$rows <- lapply(result$rows, function(row) c(row, list("ignored")))
  expect_identical(.warm_start_decode_features(result), out)
  legacy <- jsonlite::read_json(test_path("fixtures", "warm-start-features", "golden.json"))
  old <- .warm_start_decode_features(legacy)
  for (name in names(old)[-1]) expect_equal(out[[name]][seq_len(nrow(old))], old[[name]], tolerance = 1e-7)
})

test_that("v2 response mismatches and invalid added values fail before fitting", {
  fixture <- warm_v2_fixture()
  out <- .warm_start_decode_features(fixture)
  validate <- function(x) .validate_warm_start_features(x, out$item_id, "writing_features_v2")
  for (name in warm_start_feature_schema("writing_features_v2")$feature[-(1:20)]) {
    expect_error(validate(out[setdiff(names(out), name)]), "Missing required")
    for (value in list("bad", TRUE, Inf)) {
      bad <- out
      bad[[name]] <- value
      expect_error(validate(bad), "numeric, finite or missing")
    }
  }
  for (name in c("n_characters", "textstat_char_count", "textstat_difficult_words")) {
    for (value in c(NA_real_, -1, 1.2)) {
      bad <- out
      bad[[name]][1] <- value
      expect_error(validate(bad), "nonmissing nonnegative whole counts")
    }
  }
  for (name in c("textstat_linsear_write_formula", "textstat_gunning_fog", "textstat_spache_readability")) {
    bad <- out
    bad[[name]][1] <- NA_real_
    expect_error(validate(bad), "must not be missing")
  }
  for (name in c("token_length_median", "syllables_per_token_mean",
    "syllables_per_token_median", "syllables_per_token_std", "gunning_fog", "lix")) {
    bad <- out
    bad[[name]][1] <- 0
    expect_error(validate(bad), "missingness inconsistent")
    bad <- out
    bad[[name]][2] <- NA_real_
    expect_error(validate(bad), "missingness inconsistent")
  }
  bad <- fixture
  bad$rows[[1]][[22]] <- "not numeric"
  expect_error(.warm_start_decode_features(bad), "Malformed")
  bad <- out
  attr(bad, "warm_start_schema") <- "writing_features_v1"
  expect_error(validate(bad), "schema mismatch")
  fixture$schema <- "writing_features_v1"
  local_mocked_bindings(.warm_start_python_request = function(...) fixture, .package = "pairwiseLLM")
  expect_error(extract_warm_start_features(out$item_id, unlist(fixture$texts), "writing_features_v2"),
    "schema mismatch")
})

test_that("v2 format-3 fitting reuses schema-independent plans and deploys reduced artifacts", {
  skip_if_not_installed("glmnet")
  withr::local_seed(25903)
  f <- warm_phase2_fixture()
  v1 <- warm_phase2_fit(f)
  x <- f$x
  definition <- warm_start_feature_schema("writing_features_v2")
  for (name in setdiff(definition$feature, names(x))) {
    x[[name]] <- if (definition$type[definition$feature == name] == "integer") {
      sample(1:25, nrow(x), replace = TRUE)
    } else {
      stats::runif(nrow(x))
    }
  }
  x$second_order_coherence <- NA_real_
  x$sentence_length_median[1:2] <- NA_real_
  attr(x, "warm_start_schema") <- "writing_features_v2"
  model <- fit_warm_start_model(x$item_id, f$theta, "phase2", features = x,
    schema = "writing_features_v2", alpha_grid = c(0, 1), cv_plan = f$plan)
  expect_identical(model$cv_plan, v1$cv_plan)
  expect_identical(model$cv_identity, v1$cv_identity)
  expect_identical(model$features, definition$feature)
  expect_identical(model$validation$predictions$fold, v1$validation$predictions$fold)
  expect_false("second_order_coherence" %in% model$preprocessing$retained)
  expect_identical(unname(model$preprocessing$removed["second_order_coherence"]), "all_missing")
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  expect_identical(reduced$format_version, 3L)
  expect_identical(reduced$schema, "writing_features_v2")
  root <- withr::local_tempdir()
  withr::local_envvar(R_USER_DATA_DIR = root)
  local_mocked_bindings(.warm_start_require_glmnet = function() stop("unexpected fitting"),
    .warm_start_python_request = function(...) stop("unexpected Python"), .package = "pairwiseLLM")
  for (artifact in list(model, reduced)) {
    path <- file.path(root, "v2.rds")
    save_warm_start_model(artifact, path, overwrite = TRUE)
    expect_identical(load_warm_start_model(path), artifact)
    expect_identical(predict(load_warm_start_model(path), x), predict(model, x))
    expect_identical(warm_start_coefficients(artifact), warm_start_coefficients(model))
    prior <- make_warm_start_prior(predict(artifact, x))
    expect_equal(prior$prior_mean, prior$scores - mean(prior$scores))
    expect_true(all(prior$prior_sd == 0.5))
  }
  register_warm_start_model(reduced, "writing-v2")
  expect_identical(list_warm_start_models("user")$schema, "writing_features_v2")
  expect_identical(load_warm_start_model(name = "writing-v2", source = "user"), reduced)
  expect_error(predict(model, f$x), "schema mismatch")
  expect_error(ensemble_warm_start_models(v1, model), "schema")
})

test_that("real pinned v2 Python extraction agrees with independent upstream evidence", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", unset = "")
  skip_if(!nzchar(python), "Set PAIRWISELLM_TEST_PYTHON to an explicitly provisioned interpreter")
  skip_if_not_installed("reticulate", "1.41")
  status <- suppressWarnings(warm_start_python_status("writing_features_v2", python))
  expect_true(status$available, info = paste(status$problems, collapse = "; "))
  fixture <- warm_v2_fixture()
  expected <- .warm_start_decode_features(fixture)
  actual <- suppressWarnings(extract_warm_start_features(expected$item_id, unlist(fixture$texts),
    "writing_features_v2", python))
  expect_identical(names(actual), names(expected))
  for (name in names(expected)[-1]) {
    expect_identical(is.na(actual[[name]]), is.na(expected[[name]]))
    finite <- !is.na(expected[[name]])
    expect_true(all(abs(actual[[name]][finite] - expected[[name]][finite]) <=
      1e-10 + 1e-7 * abs(expected[[name]][finite])), info = name)
  }
  repeated <- suppressWarnings(extract_warm_start_features(expected$item_id, unlist(fixture$texts),
    "writing_features_v2", python))
  expect_identical(repeated, actual)
})
