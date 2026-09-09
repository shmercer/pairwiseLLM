test_that("status reports unavailable and available environments without throwing", {
  local_mocked_bindings(.warm_start_python_request = function(...) {
    rlang::abort("missing test dependency")
  })
  status <- warm_start_python_status()
  expect_false(status$available)
  expect_match(status$problems, "missing test dependency")
  expect_identical(status$expected$python, "3.12.3")
  local_mocked_bindings(.warm_start_python_request = function(...) {
    list(python = "test-python", observed = list(python = "3.12.3"))
  })
  status <- warm_start_python_status()
  expect_true(status$available)
  expect_identical(status$python, "test-python")
  expect_identical(status$problems, character())
  expect_identical(status$observed$python, "3.12.3")
  for (python in list(NA_character_, "", character(), c("a", "b"), 1)) {
    expect_error(warm_start_python_status(python = python), "interpreter path")
  }
  expect_error(warm_start_python_status(schema = "v2"), "Unknown feature schema")
  local_mocked_bindings(.warm_start_python_artifact = function(...) "missing")
  expect_error(warm_start_python_status(), "metadata is missing or corrupt")
})

test_that("status handles unreadable installed Python metadata", {
  local_mocked_bindings(.warm_start_python_artifact = function(...) "")
  expect_error(warm_start_python_status(), "metadata is missing or corrupt")
})

test_that("bridge decodes structured results and restores environment settings", {
  request <- pairwiseLLM:::.warm_start_python_request
  withr::local_envvar(c(RETICULATE_USE_MANAGED_VENV = "yes", RETICULATE_AUTOCONFIGURE = NA))
  local_mocked_bindings(.warm_start_python_module = function(python) {
    expect_identical(Sys.getenv("RETICULATE_USE_MANAGED_VENV"), "no")
    expect_identical(Sys.getenv("RETICULATE_AUTOCONFIGURE"), "FALSE")
    list(request_json = function(payload) {
      expect_identical(jsonlite::fromJSON(payload)$operation, "status")
      '{"ok":true,"result":{"python":"test"},"warnings":["test warning"]}'
    })
  })
  expect_warning(result <- request(list(operation = "status"), NULL), "test warning")
  expect_identical(result$python, "test")
  expect_identical(Sys.getenv("RETICULATE_USE_MANAGED_VENV"), "yes")
  expect_identical(Sys.getenv("RETICULATE_AUTOCONFIGURE", unset = NA_character_), NA_character_)
})

test_that("bridge rejects malformed responses and produces concise errors", {
  request <- pairwiseLLM:::.warm_start_python_request
  cases <- list(
    "bad JSON" = "expected a JSON",
    "[]" = "missing success status",
    '{"ok":null}' = "missing success status",
    '{"ok":"true"}' = "missing success status",
    '{"ok":false}' = "missing error message",
    '{"ok":false,"error":42}' = "missing error message",
    '{"ok":false,"error":"missing CMUdict"}' = "missing CMUdict",
    '{"ok":true}' = "missing result"
  )
  for (response in names(cases)) {
    local_mocked_bindings(.warm_start_python_module = function(...) {
      list(request_json = function(...) response)
    })
    expect_error(request(list(), NULL), cases[[response]])
  }
  local_mocked_bindings(.warm_start_python_module = function(...) stop("setup error"))
  expect_error(request(list(), NULL), "setup failed: setup error")
  local_mocked_bindings(.warm_start_python_module = function(...) {
    list(request_json = function(...) stop("private Python traceback"))
  })
  expect_error(request(list(), NULL), "Python extraction failed")
})

test_that("missing reticulate gives an actionable optional dependency error", {
  local_mocked_bindings(.warm_start_has_reticulate = function(...) FALSE)
  expect_error(pairwiseLLM:::.warm_start_python_module(NULL), "optional R package 'reticulate'")
})

test_that("interpreter selection detects conflicts without initializing Python", {
  skip_if_not_installed("reticulate", "1.41")
  module <- pairwiseLLM:::.warm_start_python_module
  withr::local_envvar(c(RETICULATE_PYTHON = "managed"))
  expect_error(module(NULL), "existing Python interpreter")
  Sys.setenv(RETICULATE_PYTHON = "/configured/python")
  expect_error(module("/other/python"), "conflicts with RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON")
  local_mocked_bindings(
    py_available = function(...) TRUE,
    py_config = function(...) list(python = "/active/python"),
    import_from_path = function(...) "mock module",
    .package = "reticulate"
  )
  expect_error(module("/other/python"), "Restart R")
  expect_identical(module("/active/python"), "mock module")
  expect_identical(module(NULL), "mock module")
  local_mocked_bindings(.warm_start_python_artifact = function(...) "missing")
  expect_error(module(NULL), "extractor is missing")
})

test_that("uninitialized interpreter selection uses only existing Python", {
  skip_if_not_installed("reticulate", "1.41")
  module <- pairwiseLLM:::.warm_start_python_module
  withr::local_envvar(c(RETICULATE_PYTHON = NA))
  python <- tempfile()
  file.create(python)
  withr::defer(unlink(python))
  local_mocked_bindings(
    py_available = function(...) FALSE,
    py_discover_config = function(...) list(python = python),
    use_python = function(python, required) expect_true(required),
    import_from_path = function(...) "mock module",
    .package = "reticulate"
  )
  expect_identical(module(NULL), "mock module")
  expect_identical(module(python), "mock module")
  expect_error(module("/nonexistent/python"), "No existing Python")
  expect_error(module(tempdir()), "No existing Python")
  local_mocked_bindings(py_discover_config = function(...) NULL, .package = "reticulate")
  expect_error(module(NULL), "No existing Python")
})


test_that("corrupt Python metadata is an actionable installation error", {
  path <- tempfile()
  writeLines("not JSON", path)
  withr::defer(unlink(path))
  local_mocked_bindings(.warm_start_python_artifact = function(...) path)
  expect_error(warm_start_python_status(), "metadata is missing or corrupt")
})
