#' Create a judge function backed by live LLM pair submission
#'
#' @description
#' Creates a function suitable for use as a judge in adaptive or iterative
#' workflows. The returned function calls [submit_llm_pairs()] and returns only
#' the `results` tibble.
#'
#' For checkpointed live runs that append to `save_path`, set
#' `return_mode = "new"` (the default) to avoid double-counting historical rows
#' in iterative workflows.
#'
#' @param backend Character scalar. Backend name (e.g., `"openai"`, `"anthropic"`,
#'   `"gemini"`, `"together"`, `"ollama"`).
#' @param model Character scalar. Model identifier passed through to
#'   [submit_llm_pairs()].
#' @param save_path Optional path for checkpointed results. Passed through to
#'   [submit_llm_pairs()].
#' @param ... Additional arguments forwarded to [submit_llm_pairs()].
#' @param return_mode Character scalar. One of `"new"` or `"all"`. Passed through
#'   to [submit_llm_pairs()]. Defaults to `"new"` because this helper is intended
#'   for iterative/adaptive use.
#'
#' @return A function with signature `function(pairs)` that returns a tibble of
#'   pairwise results.
#'
#' @examples
#' judge_fun <- make_llm_judge_fun(
#'   backend = "openai",
#'   model = "gpt-4.1",
#'   save_path = tempfile(fileext = ".csv")
#' )
#'
#' # In adaptive workflows, use return_mode = "new" (default) to avoid
#' # double-counting historical checkpoint rows.
#' \dontrun{
#' pairs <- tibble::tibble(
#'   ID1 = c("a", "b"),
#'   text1 = c("Text A", "Text B"),
#'   ID2 = c("c", "d"),
#'   text2 = c("Text C", "Text D")
#' )
#' res <- judge_fun(pairs)
#' }
#'
#' @export
make_llm_judge_fun <- function(
  backend,
  model,
  save_path = NULL,
  ...,
  return_mode = c("new", "all")
) {
  return_mode <- match.arg(return_mode)

  # Freeze all `...` values at construction time.
  # Without this, values passed via symbols (e.g., temperature = temp) remain
  # lazy promises and can change across adaptive rounds if the caller mutates
  # those symbols.
  dots <- rlang::list2(...)
  if (length(dots) > 0L) {
    for (i in seq_along(dots)) {
      force(dots[[i]])
    }
  }

  force(backend)
  force(model)
  force(save_path)
  force(return_mode)

  function(pairs) {
    out <- rlang::exec(
      submit_llm_pairs,
      pairs = pairs,
      backend = backend,
      model = model,
      save_path = save_path,
      return_mode = return_mode,
      !!!dots
    )

    out$results
  }
}
