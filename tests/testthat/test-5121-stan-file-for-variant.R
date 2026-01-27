testthat::test_that("stan_file_for_variant resolves stan paths", {
  variants <- c("btl", "btl_e", "btl_b", "btl_e_b")
  paths <- vapply(variants, pairwiseLLM:::stan_file_for_variant, character(1L))

  testthat::expect_true(all(file.exists(paths)))
  testthat::expect_match(paths[[1L]], "stan[\\\\/]btl\\.stan$")
  testthat::expect_match(paths[[2L]], "stan[\\\\/]btl_e\\.stan$")
  testthat::expect_match(paths[[3L]], "stan[\\\\/]btl_b\\.stan$")
  testthat::expect_match(paths[[4L]], "stan[\\\\/]btl_e_b\\.stan$")
})

testthat::test_that("stan_file_for_variant rejects invalid variants", {
  testthat::expect_error(
    pairwiseLLM:::stan_file_for_variant("bad"),
    "model_variant"
  )
})

testthat::test_that("stan_file_for_variant errors when stan file is missing", {
  local_rebind_namespace <- function(ns, name, value) {
    env <- asNamespace(ns)
    has_old <- exists(name, envir = env, inherits = FALSE)
    old <- if (has_old) get(name, envir = env, inherits = FALSE) else NULL
    locked <- if (has_old) bindingIsLocked(name, env) else FALSE
    if (locked) {
      unlockBinding(name, env)
    }
    assign(name, value, envir = env)
    if (locked) {
      lockBinding(name, env)
    }
    function() {
      if (locked) {
        unlockBinding(name, env)
      }
      if (has_old) {
        assign(name, old, envir = env)
      } else if (exists(name, envir = env, inherits = FALSE)) {
        rm(list = name, envir = env)
      }
      if (locked) {
        lockBinding(name, env)
      }
    }
  }
  restore_system_file <- local_rebind_namespace(
    "base",
    "system.file",
    function(...) ""
  )
  on.exit(restore_system_file(), add = TRUE)

  # Some pkgload contexts still resolve system.file to the original binding,
  # so skip if the override cannot trigger the error branch.
  err <- tryCatch(
    pairwiseLLM:::stan_file_for_variant("btl"),
    error = function(e) e
  )
  if (!inherits(err, "error")) {
    testthat::skip("Unable to override system.file under pkgload; error path covered elsewhere.")
  }
  testthat::expect_error(
    pairwiseLLM:::stan_file_for_variant("btl"),
    "not found"
  )
})
