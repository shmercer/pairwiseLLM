testthat::test_that(".openai_request normalizes malformed httr2 request objects", {
  # Avoid touching the real environment.
  withr::local_envvar(OPENAI_API_KEY = "sk-test")

  # Case 1: httr2::request returns a non-list.
  testthat::local_mocked_bindings(
    `request` = function(url) "not_a_request",
    `req_auth_bearer_token` = function(req, token) req,
    .package = "pairwiseLLM"
  )

  req1 <- pairwiseLLM:::.openai_request("/files")
  testthat::expect_s3_class(req1, "httr2_request")
  testthat::expect_true(is.list(req1))
  testthat::expect_true(is.character(req1$url))
  testthat::expect_true(is.list(req1$headers))
  testthat::expect_true(!is.null(req1$headers$Authorization))

  # Case 2: existing malformed headers/body get normalized.
  testthat::local_mocked_bindings(
    `request` = function(url) {
      list(
        url = url,
        method = NULL,
        headers = structure(list("x"), .Names = NULL),
        body = "bad",
        options = "bad"
      )
    },
    `req_auth_bearer_token` = function(req, token) req,
    .package = "pairwiseLLM"
  )

  req2 <- pairwiseLLM:::.openai_request("/batches")
  testthat::expect_s3_class(req2, "httr2_request")
  testthat::expect_equal(req2$method, "GET")
  testthat::expect_true(is.list(req2$headers))
  testthat::expect_true(is.list(req2$options))
  testthat::expect_null(req2$body)
})

testthat::test_that(".openai_req_body_json defends against malformed request bodies", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test")

  # Mock httr2::req_body_json to return a non-list, triggering normalization.
  testthat::local_mocked_bindings(
    `req_body_json` = function(req, body, ...) "not_a_request",
    .package = "pairwiseLLM"
  )

  req <- structure(list(url = "u", body = list(type = NA)), class = "httr2_request")
  out <- pairwiseLLM:::.openai_req_body_json(req, list(a = 1))
  testthat::expect_s3_class(out, "httr2_request")
  testthat::expect_true(is.list(out))
})
