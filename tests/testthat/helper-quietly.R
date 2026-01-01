# Helpers for keeping tests quiet and fast.
#
# Use `quietly(expr)` when a call would otherwise emit lots of messages or
# progress output during `devtools::test()`.

quietly <- function(expr) {
  # Capture stdout + messages while preserving the expression's return value.
  # (We intentionally do NOT silence warnings.)
  out_path <- tempfile("testthat_quiet_out_")
  msg_path <- tempfile("testthat_quiet_msg_")

  out_con <- file(out_path, open = "wt")
  msg_con <- file(msg_path, open = "wt")

  on.exit({
    # Unwind sinks if they were opened.
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

