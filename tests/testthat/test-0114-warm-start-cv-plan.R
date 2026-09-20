test_that("plans preserve exact legacy fold draw order and serialize portably", {
  legacy <- readRDS(test_path("fixtures", "warm-start-legacy", "baseline-1.5.1.rds"))
  withr::local_seed(259, .rng_kind = legacy$rng_kind[1],
    .rng_normal_kind = legacy$rng_kind[2], .rng_sample_kind = legacy$rng_kind[3])
  before <- .Random.seed
  for (case in legacy$cases) {
    input <- case$input
    plan <- do.call(make_warm_start_cv_plan, input[intersect(names(input),
      c("ids", "theta", "task_id", "seed", "outer_folds", "inner_folds"))])
    expect_identical(unname(plan$outer_foldid), case$model$validation$predictions$fold)
    expect_identical(unname(plan$full_inner_foldid), case$model$tuning$foldid)
    for (i in seq_along(plan$outer_inner_foldid)) {
      expect_identical(unname(plan$outer_inner_foldid[[i]]), case$model$validation$folds[[i]]$tuning$foldid)
      expect_identical(names(plan$outer_inner_foldid[[i]]), case$model$validation$folds[[i]]$train_ids)
    }
    expect_identical(.Random.seed, before)
    path <- file.path(withr::local_tempdir(), "plan.rds")
    saveRDS(plan, path)
    expect_identical(readRDS(path), plan)
    expect_identical(.validate_warm_start_cv_plan(readRDS(path)), plan)
  }
})

test_that("plans preserve nondefault RNG kinds and an absent caller seed", {
  withr::local_seed(87, .rng_kind = "L'Ecuyer-CMRG", .rng_normal_kind = "Ahrens-Dieter")
  before <- .Random.seed
  kind <- RNGkind()
  first <- make_warm_start_cv_plan(1:20, rep(1:10, 2), "ties")
  expect_identical(RNGkind(), kind)
  expect_identical(first$rng_kind, kind)
  expect_identical(.Random.seed, before)
  expect_identical(make_warm_start_cv_plan(1:20, rep(1:10, 2), "ties"), first)
  rm(".Random.seed", envir = .GlobalEnv)
  make_warm_start_cv_plan(1:20, 1:20, "absent")
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  expect_identical(RNGkind(), kind)
})

test_that("plan identities reject task, order, outcome and structural corruption", {
  f <- warm_phase2_fixture()
  p <- f$plan
  expect_error(.validate_warm_start_cv_plan(p, rev(p$ids)), "identity")
  expect_error(.validate_warm_start_cv_plan(p, theta = rev(p$theta)), "identity")
  expect_error(.validate_warm_start_cv_plan(p, task_id = "another"), "identity")
  for (field in names(p)) {
    bad <- p
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_cv_plan(bad), info = field)
  }
  for (change in list(
    function(p) {
      p$theta[1] <- p$theta[1] + 1
      p
    },
    function(p) {
      p$outer_foldid[1] <- 99L
      warm_phase2_rehash(p)
    },
    function(p) {
      names(p$full_inner_foldid) <- rev(p$ids)
      warm_phase2_rehash(p)
    },
    function(p) {
      p$outer_inner_foldid[[1]][1] <- NA_integer_
      warm_phase2_rehash(p)
    },
    function(p) {
      p$rng_kind[1] <- "invalid"
      warm_phase2_rehash(p)
    },
    function(p) {
      p$theta[] <- 1
      warm_phase2_rehash(p)
    },
    function(p) {
      p$extra <- TRUE
      p
    })) {
    expect_error(.validate_warm_start_cv_plan(change(p)))
  }
  expect_error(make_warm_start_cv_plan(1:5, 1:5, "small"), "Requested folds")
  expect_error(make_warm_start_cv_plan(1:20, 1:19, "length"), "one theta")
  expect_error(make_warm_start_cv_plan(1:20, 1:20, "seed", seed = -1), "seed")
})

test_that("plan and argument mismatches fail before extraction or engine calls", {
  f <- warm_phase2_fixture()
  local_mocked_bindings(extract_warm_start_features = function(...) stop("extraction boundary"),
    .warm_start_require_glmnet = function() stop("engine boundary"), .package = "pairwiseLLM")
  for (field in c("seed", "outer_folds", "inner_folds")) {
    for (value in list(2L, NA, "bad", NULL)) {
      expect_error(do.call(warm_phase2_fit, c(list(f = f), stats::setNames(list(value), field))), "conflicts")
    }
  }
  changed <- f
  changed$theta[1] <- changed$theta[1] + 1
  expect_error(warm_phase2_fit(changed), "identity")
  expect_error(warm_phase2_fit(f), "engine boundary")
  expect_error(warm_phase2_fit(f, engine_control = list(alpha = 1)), "engine_control")
  expect_error(warm_phase2_fit(f, engine_control = 1), "engine_control")
  expect_error(warm_phase2_fit(f, engine = "pls"), "glmnet-only")
  expect_error(warm_phase2_fit(f, engine = "svr_rbf"), "glmnet-only")
})

test_that("supplied plans defer omitted settings and insulate folds from engine RNG", {
  skip_if_not_installed("glmnet")
  f <- warm_phase2_fixture()
  withr::local_seed(7)
  before <- .Random.seed
  first <- warm_phase2_fit(f)
  expect_identical(.Random.seed, before)
  expect_identical(first$cv_plan, f$plan)
  expect_identical(first$format_version, 3L)
  expect_identical(first$audit_status, "full")
  expect_identical(first, warm_phase2_fit(f, seed = 259, outer_folds = 5, inner_folds = 5,
    engine = "glmnet", engine_control = list()))
  original <- .warm_start_glmnet_path
  local_mocked_bindings(.warm_start_glmnet_path = function(...) {
    stats::runif(17)
    original(...)

    }, .warm_start_folds = function(...) stop("must not regenerate supplied folds"), .package = "pairwiseLLM")
  second <- warm_phase2_fit(f)
  expect_identical(second, first)
  expect_identical(.Random.seed, before)
})

test_that("format 3 rejects payload, compatibility view and CV evidence corruption", {
  skip_if_not_installed("glmnet")
  model <- warm_phase2_fit()
  expect_identical(.validate_warm_start_model(model), model)
  for (field in names(model)) {
    bad <- model
    bad[field] <- list(NULL)
    expect_error(.validate_warm_start_model(bad), info = field)
  }
  for (change in list(
    function(m) {
      m$engine_payload$type <- "unknown"
      m
    },
    function(m) {
      m$engine_payload$intercept <- 1
      m
    },
    function(m) {
      m$engine_payload$coefficients <- rev(m$coefficients)
      m
    },
    function(m) {
      m$training$engine <- "pls"
      m
    },
    function(m) {
      m$training$hyperparameters$alpha <- 0.2
      m
    },
    function(m) {
      m$audit_status <- "unknown"
      m
    },
    function(m) {
      m$cv_identity$digest <- "bad"
      m
    },
    function(m) {
      m$cv_identity$outcome_digest <- strrep("a", 32)
      m
    },
    function(m) {
      m$cv_identity$seed <- 2L
      m
    },
    function(m) {
      m$validation$folds[[1]]$engine_payload$intercept <- 1
      m
    },
    function(m) {
      m$cv_plan$theta[1] <- m$cv_plan$theta[1] + 1
      m$cv_plan <- warm_phase2_rehash(m$cv_plan)
      m$cv_identity <- .warm_start_cv_identity(m$cv_plan)
      m
    },
    function(m) {
      m$cv_plan$full_inner_foldid <- rev(m$cv_plan$full_inner_foldid)
      names(m$cv_plan$full_inner_foldid) <- m$cv_plan$ids
      m$cv_plan <- warm_phase2_rehash(m$cv_plan)
      m$cv_identity <- .warm_start_cv_identity(m$cv_plan)
      m
    })) {
    expect_error(.validate_warm_start_model(change(model)))
  }
})


test_that("R 4.7 binomial RNG provenance is portable without changing partitions", {
  original <- base::RNGkind()[1:3]
  withr::local_seed(37)
  before <- .Random.seed
  local_mocked_bindings(.warm_start_rng_kind = function(...) original, .package = "pairwiseLLM")
  legacy <- make_warm_start_cv_plan(1:20, 1:20, "rng-version")
  expect_identical(legacy$rng_kind, original)
  for (binomial in c("Buggy BTPE", "BTPE")) {
    reported <- c(original, binomial)
    current <- with_mocked_bindings(
      make_warm_start_cv_plan(1:20, 1:20, "rng-version"),
      .warm_start_rng_kind = function(...) reported, .package = "pairwiseLLM")
    expect_identical(current$rng_kind, reported)
    expect_identical(current$outer_foldid, legacy$outer_foldid)
    expect_identical(current$outer_inner_foldid, legacy$outer_inner_foldid)
    expect_identical(current$full_inner_foldid, legacy$full_inner_foldid)
    expect_identical(.validate_warm_start_cv_plan(current), current)
    expect_false(identical(current$digest, legacy$digest))
    expect_identical(.Random.seed, before)
  }
  expect_error(.warm_start_plan_rng(c(original, "invalid")), "RNG provenance")
  expect_error(.warm_start_plan_rng(c(original, "BTPE", "extra")), "RNG provenance")
  expect_error(.warm_start_plan_rng(original[1:2]), "RNG provenance")
  skip_if_not_installed("glmnet")
  f <- warm_phase2_fixture()
  model <- with_mocked_bindings({
    f$plan <- make_warm_start_cv_plan(f$x$item_id, f$theta, "phase2", seed = 259L)
    warm_phase2_fit(f)
  }, .warm_start_rng_kind = function(...) c(original, "BTPE"), .package = "pairwiseLLM")
  expect_identical(model$cv_identity$rng_kind, c(original, "BTPE"))
  reduced <- prepare_warm_start_model(model, omit_audit = TRUE)
  expect_identical(reduced$cv_identity, model$cv_identity)
  expect_identical(predict(reduced, f$x), predict(model, f$x))
})
