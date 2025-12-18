Sys.setenv(
  OPENAI_API_KEY = "",
  ANTHROPIC_API_KEY = "",
  GEMINI_API_KEY = "",
  TOGETHER_API_KEY = "",
  CI = "true",
  NOT_CRAN = "false"
)

options(
  # Don’t accidentally use an http proxy that gives you internet
  internet.info = FALSE
)

# Run checks
devtools::check()
