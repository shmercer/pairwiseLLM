compute_batch_sizes <- pairwiseLLM:::compute_batch_sizes

test_that("compute_batch_sizes handles small N", {
  sizes <- compute_batch_sizes(50)
  expect_equal(sizes$BATCH1, 500L)
  expect_equal(sizes$BATCH2, 200L)
  expect_equal(sizes$BATCH3, 100L)
  expect_equal(sizes$CW, 25L)
})

test_that("compute_batch_sizes handles large N", {
  sizes <- compute_batch_sizes(10000)
  expect_equal(sizes$BATCH1, 5000L)
  expect_equal(sizes$BATCH2, 2000L)
  expect_equal(sizes$BATCH3, 500L)
  expect_equal(sizes$CW, 5000L)
})
