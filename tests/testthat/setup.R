# testthat setup
#
# This file is always sourced by testthat (Edition 3) before running any tests.
# We define common helpers here so they are available even when running tests
# via tooling that does not automatically source helper-* files.

options(
  # Don't show cli progress bars during tests unless explicitly enabled.
  cli.progress_show_after = Inf
)

if (!exists("quietly", mode = "function", inherits = TRUE)) {
  quietly <- function(expr) {
    # Capture stdout + messages while preserving the expression's return value.
    # (We intentionally do NOT silence warnings.)
    out_path <- tempfile("testthat_quiet_out_")
    msg_path <- tempfile("testthat_quiet_msg_")

    out_con <- file(out_path, open = "wt")
    msg_con <- file(msg_path, open = "wt")

    on.exit({
      try(sink(type = "message"), silent = TRUE)
      try(sink(), silent = TRUE)
      try(close(msg_con), silent = TRUE)
      try(close(out_con), silent = TRUE)
      try(unlink(c(out_path, msg_path)), silent = TRUE)
    }, add = TRUE)

    sink(out_con)
    sink(msg_con, type = "message")

    eval.parent(substitute(expr))
  }
}
