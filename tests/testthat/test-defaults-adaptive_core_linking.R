testthat::test_that("bt_run_adaptive_core_linking default embeddings_metric is cosine", {
  fml <- formals(bt_run_adaptive_core_linking)$embeddings_metric
  testthat::expect_true(is.language(fml) || is.call(fml))
  testthat::expect_true(grepl('^c\\("cosine", "euclidean"\\)$', paste(deparse(fml), collapse = "")))
})
