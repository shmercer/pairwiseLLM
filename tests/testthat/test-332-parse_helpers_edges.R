testthat::test_that(".extract_better_sample handles tag parsing and fallbacks robustly", {
  # invalid input
  testthat::expect_true(is.na(pairwiseLLM:::.extract_better_sample(NA_character_)))
  testthat::expect_true(is.na(pairwiseLLM:::.extract_better_sample("")))

  # tag-wrapped with whitespace/case variations
  txt1 <- "<BETTER_SAMPLE> sample 1 </BETTER_SAMPLE>"
  testthat::expect_identical(pairwiseLLM:::.extract_better_sample(txt1), "SAMPLE_1")

  txt2 <- "<BETTER_SAMPLE>\nSAMPLE-2\n</BETTER_SAMPLE>"
  testthat::expect_identical(pairwiseLLM:::.extract_better_sample(txt2), "SAMPLE_2")

  # custom tags that include regex metacharacters should be escaped properly
  txt3 <- "<B+>SAMPLE_1</B+>"
  testthat::expect_identical(
    pairwiseLLM:::.extract_better_sample(txt3, tag_prefix = "<B+>", tag_suffix = "</B+>"),
    "SAMPLE_1"
  )

  # fallback: bare token without tags
  txt4 <- "Winner: SAMPLE 2"
  testthat::expect_identical(pairwiseLLM:::.extract_better_sample(txt4), "SAMPLE_2")

  # no match returns NA
  txt5 <- "Winner: neither"
  testthat::expect_true(is.na(pairwiseLLM:::.extract_better_sample(txt5)))
})
