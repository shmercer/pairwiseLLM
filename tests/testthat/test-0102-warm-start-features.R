feature_fixture <- function() {
  jsonlite::read_json(test_path("fixtures", "warm-start-features", "golden.json"))
}

feature_result <- function() {
  x <- feature_fixture()
  x[c("schema", "columns", "rows")]
}

feature_table <- function() {
  pairwiseLLM:::.warm_start_decode_features(feature_result())
}

test_that("extraction preserves the canonical fixture contract without Python", {
  fixture <- feature_fixture()
  result <- feature_result()
  ids <- vapply(result$rows, `[[`, character(1), 1L)
  local_mocked_bindings(.warm_start_python_request = function(request, python) {
    expect_identical(request$ids, as.list(ids))
    expect_identical(request$texts, fixture$texts)
    result
  })
  out <- extract_warm_start_features(ids, unlist(fixture$texts))
  expect_s3_class(out, "tbl_df")
  expect_identical(names(out), unlist(fixture$columns))
  expect_identical(out$item_id, ids)
  expect_identical(attr(out, "warm_start_schema"), fixture$schema)
  expect_identical(out, feature_table())
  expect_equal(out$n_tokens[1:3], c(0, 1, 0))
  expect_true(is.na(out$upstream_entropy_per_token[1]))
  expect_true(is.finite(out$upstream_entropy_per_token[3]))
})

test_that("invalid inputs fail before crossing the Python boundary", {
  local_mocked_bindings(.warm_start_python_request = function(...) stop("Unexpected Python call"))
  bad_ids <- list(NULL, character(), NA_character_, NA_real_, TRUE, Inf, NaN,
    list("a"), factor("a"), matrix("a"), c("a", "a"), c("a", " "), "")
  for (ids in bad_ids) expect_error(extract_warm_start_features(ids, "text"), "IDs")
  for (texts in list(NULL, NA_character_, 1, list("text"), matrix("text"), c("a", "b"))) {
    expect_error(extract_warm_start_features("a", texts), "texts")
  }
  expect_error(extract_warm_start_features("a", "", "wrong"), "Unknown feature schema")
  expect_identical(pairwiseLLM:::.warm_start_ids(c(1, 2)), c("1", "2"))
})

test_that("feature validator aligns exact ID sets and ignores extra columns", {
  out <- feature_table()
  ids <- out$item_id
  validate <- pairwiseLLM:::.validate_warm_start_features
  shuffled <- out[rev(seq_len(nrow(out))), ]
  shuffled$extra <- "ignored"
  expect_identical(validate(shuffled, ids), out)
  for (bad in list(out[-1, ], rbind(out, out[1, ]))) {
    expect_error(validate(bad, ids), "IDs")
  }
  bad <- out
  bad$item_id[1] <- "extra"
  expect_error(validate(bad, ids), "IDs")
  bad$item_id[1] <- NA_character_
  expect_error(validate(bad, ids), "IDs")
  bad <- out
  bad$item_id[1] <- bad$item_id[2]
  expect_error(validate(bad, ids), "unique")
  expect_error(validate(list(), ids), "data frame")
  bad <- out
  names(bad)[2] <- "item_id"
  expect_error(validate(bad, ids), "unique column")
  expect_error(validate(out[-2], ids), "Missing required.*n_tokens")
  for (schema in list(NULL, "v2", c("writing_features_v1", "writing_features_v1"))) {
    bad <- out
    attr(bad, "warm_start_schema") <- schema
    expect_error(validate(bad, ids), "schema mismatch")
  }
})

test_that("feature validator rejects invalid values without imputing missingness", {
  out <- feature_table()
  validate <- function(x) pairwiseLLM:::.validate_warm_start_features(x, out$item_id)
  for (value in list("1", TRUE, Inf, -Inf, as.Date("2020-01-01"), matrix(1, nrow(out)))) {
    bad <- out
    bad$dependency_distance_mean <- value
    expect_error(validate(bad), "numeric, finite or missing")
  }
  for (value in c(NA_real_, -1, 1.5)) {
    bad <- out
    bad$n_tokens[1] <- value
    expect_error(validate(bad), "whole counts")
  }
  bad <- out
  bad$dale_chall_readability_score[1] <- NA_real_
  expect_error(validate(bad), "must not be missing")
  for (name in c("proportion_unique_tokens", "token_length_mean", "token_length_std")) {
    bad <- out
    bad[[name]][1] <- 0
    expect_error(validate(bad), "missingness inconsistent")
    bad <- out
    bad[[name]][2] <- NA_real_
    expect_error(validate(bad), "missingness inconsistent")
  }
  expect_identical(validate(out), out)
})

test_that("decoder rejects malformed rows, IDs, types and duplicate columns", {
  decode <- pairwiseLLM:::.warm_start_decode_features
  result <- feature_result()
  for (columns in list(NULL, list(), "a", list(1), list(NA_character_), list("a", "a"), list("a"))) {
    bad <- result
    bad$columns <- columns
    expect_error(decode(bad), "Malformed")
  }
  for (rows in list(NULL, list(), list(1), list(list("a")), list(rep(list(1), 21)))) {
    bad <- result
    bad$rows <- rows
    expect_error(decode(bad), "Malformed")
  }
  for (value in list("1", TRUE, list(1), c(1, 2))) {
    bad <- result
    bad$rows[[1]][[2]] <- value
    expect_error(decode(bad), "Malformed")
  }
})

test_that("real Python integration agrees with golden outputs and repeats", {
  python <- Sys.getenv("PAIRWISELLM_TEST_PYTHON", unset = "")
  skip_if(!nzchar(python), "Set PAIRWISELLM_TEST_PYTHON to an explicitly provisioned interpreter")
  skip_if_not_installed("reticulate", "1.41")
  status <- suppressWarnings(warm_start_python_status(python = python))
  skip_if(!status$available, paste(status$problems, collapse = "; "))
  fixture <- feature_fixture()
  ids <- vapply(fixture$rows, `[[`, character(1), 1L)
  texts <- unlist(fixture$texts)
  actual <- suppressWarnings(extract_warm_start_features(ids, texts, python = python))
  expected <- feature_table()
  expect_identical(names(actual), names(expected))
  expect_identical(actual$item_id, ids)
  for (name in names(expected)[-1]) {
    expect_identical(is.na(actual[[name]]), is.na(expected[[name]]))
    finite <- !is.na(expected[[name]])
    difference <- abs(actual[[name]][finite] - expected[[name]][finite])
    bound <- 1e-10 + 1e-7 * abs(expected[[name]][finite])
    expect_true(all(difference <= bound), info = name)
  }
  repeated <- suppressWarnings(extract_warm_start_features(ids, texts, python = python))
  expect_equal(repeated, actual, tolerance = 1e-10)
  single <- suppressWarnings(extract_warm_start_features(1, "", python = python))
  expect_identical(single$item_id, "1")
  expect_equal(single$n_tokens, 0)
})


test_that("decoder ignores noncanonical upstream fields before numeric validation", {
  result <- feature_result()
  result$columns <- c(result$columns, list("upstream_text"))
  result$rows <- lapply(result$rows, function(row) c(row, list("ignored")))
  expect_identical(pairwiseLLM:::.warm_start_decode_features(result), feature_table())
})
