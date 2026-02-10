#' Estimate LLM token usage and cost for a set of pairwise comparisons
#'
#' Estimate total token usage and cost for running a large set of pairwise
#' comparisons by:
#' \itemize{
#'   \item running a small pilot on \code{n_test} pairs (live calls) to observe
#'     \code{prompt_tokens} and \code{completion_tokens}, and
#'   \item using the pilot to calibrate a prompt-bytes-to-input-token model for
#'     the remaining pairs, and
#'   \item prorating output tokens for the remaining pairs from the pilot
#'     distribution.
#' }
#'
#' The estimator does not require a provider tokenizer.
#' Input tokens are estimated from the byte length of the fully constructed
#' prompt and calibrated on the pilot's observed \code{prompt_tokens}.
#'
#' @param pairs Tibble or data frame with at least columns \code{ID1},
#'   \code{text1}, \code{ID2}, \code{text2}. Typically created by
#'   \code{\link{make_pairs}}, \code{\link{sample_pairs}}, and
#'   \code{\link{randomize_pair_order}}.
#' @param model Model name to use for the pilot run (and for the target job).
#' @param trait_name Short label for the trait (for example "Overall Quality").
#' @param trait_description Full-text description of the trait or rubric.
#' @param prompt_template Prompt template string, typically from
#'   \code{\link{set_prompt_template}}.
#' @param backend Backend for the pilot run; one of \code{"openai"},
#'   \code{"anthropic"}, \code{"gemini"}, or \code{"together"}.
#' @param endpoint OpenAI endpoint; one of \code{"chat.completions"} or
#'   \code{"responses"}. Ignored for other backends.
#' @param mode Target execution mode for the full job; one of \code{"live"} or
#'   \code{"batch"}. The pilot is always run live. If \code{mode = "batch"},
#'   \code{batch_discount} is applied to the estimated cost for the remaining
#'   (non-pilot) pairs.
#' @param n_test Number of pilot pairs to run live. Defaults to 25 or fewer if
#'   fewer pairs are supplied.
#' @param test_strategy Strategy for selecting pilot pairs:
#'   \code{"stratified_prompt_bytes"} (default), \code{"random"}, or
#'   \code{"first"}.
#' @param seed Optional integer seed used for pilot sampling when
#'   \code{test_strategy} is not \code{"first"}.
#' @param cost_per_million_input Cost per one million input tokens (prompt
#'   tokens), in your currency of choice.
#' @param cost_per_million_output Cost per one million output tokens
#'   (completion tokens). Reasoning/thinking tokens are treated as output.
#' @param batch_discount Numeric scalar multiplier applied to the estimated cost
#'   for the remaining pairs when \code{mode = "batch"}. For example, if batch
#'   pricing is 50 percent of live pricing, use \code{batch_discount = 0.5}.
#' @param budget_quantile Quantile used for the "budget" output-token estimate
#'   for remaining pairs. Defaults to \code{0.9} (p90).
#' @param return_test_results Logical; if \code{TRUE}, include pilot results in
#'   the returned object so you can reuse them and avoid paying twice.
#' @param return_remaining_pairs Logical; if \code{TRUE}, include the remaining
#'   pairs (excluding pilot pairs) in the returned object.
#' @param ... Additional arguments forwarded to \code{\link{submit_llm_pairs}}
#'   for the pilot run (for example \code{api_key}, \code{reasoning},
#'   \code{include_thoughts}, \code{max_tokens}, etc.).
#'
#' @return An object of class \code{"pairwiseLLM_cost_estimate"}, a list with:
#' \describe{
#'   \item{summary}{A one-row tibble with expected and budget token and cost
#'     estimates (and pilot usage).}
#'   \item{calibration}{A list describing the input-token calibration
#'     (coefficients and fit diagnostics).}
#'   \item{test_pairs}{The pilot pair subset.}
#'   \item{pilot}{Pilot results (when \code{return_test_results = TRUE}).}
#'   \item{remaining_pairs}{Remaining pairs (when
#'     \code{return_remaining_pairs = TRUE}).}
#' }
#'
#' @examples
#' \dontrun{
#' # Requires an API key and internet access.
#' data("example_writing_samples", package = "pairwiseLLM")
#'
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 50, seed = 123)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' est <- estimate_llm_pairs_cost(
#'   pairs = pairs,
#'   backend = "openai",
#'   model = "gpt-4.1",
#'   endpoint = "chat.completions",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   mode = "batch",
#'   batch_discount = 0.5,
#'   n_test = 10,
#'   cost_per_million_input = 0.15,
#'   cost_per_million_output = 0.60
#' )
#'
#' est
#' est$summary
#'
#' # Reuse pilot results and run only remaining pairs:
#' remaining <- est$remaining_pairs
#' }
#' @export
estimate_llm_pairs_cost <- function(
    pairs,
    model,
    trait_name,
    trait_description,
    prompt_template = set_prompt_template(),
    backend = c("openai", "anthropic", "gemini", "together"),
    endpoint = c("chat.completions", "responses"),
    mode = c("live", "batch"),
    n_test = 25,
    test_strategy = c("stratified_prompt_bytes", "random", "first"),
    seed = NULL,
    cost_per_million_input,
    cost_per_million_output,
    batch_discount = 1,
    budget_quantile = 0.9,
    return_test_results = TRUE,
    return_remaining_pairs = TRUE,
    ...
) {
  if (identical(backend, "ollama")) {
    rlang::abort("`backend = \"ollama\"` is not supported for cost estimation (local models have no token cost).")
  }

  backend <- match.arg(backend)
  mode <- match.arg(mode)
  test_strategy <- match.arg(test_strategy)

  pairs <- tibble::as_tibble(pairs)
  required_cols <- c("ID1", "text1", "ID2", "text2")
  missing_cols <- setdiff(required_cols, names(pairs))
  if (length(missing_cols) > 0L) {
    rlang::abort(paste0(
      "`pairs` must contain columns: ",
      paste(required_cols, collapse = ", ")
    ))
  }

  if (!is.character(model) || length(model) != 1L) {
    rlang::abort("`model` must be a single character.")
  }
  if (!is.character(trait_name) || length(trait_name) != 1L) {
    rlang::abort("`trait_name` must be a single character.")
  }
  if (!is.character(trait_description) || length(trait_description) != 1L) {
    rlang::abort("`trait_description` must be a single character.")
  }

  endpoint <- match.arg(endpoint)
  if (!is.numeric(cost_per_million_input) || length(cost_per_million_input) != 1L || is.na(cost_per_million_input)) {
    rlang::abort("`cost_per_million_input` must be a single numeric value.")
  }
  if (!is.numeric(cost_per_million_output) || length(cost_per_million_output) != 1L || is.na(cost_per_million_output)) {
    rlang::abort("`cost_per_million_output` must be a single numeric value.")
  }
  if (!is.numeric(batch_discount) || length(batch_discount) != 1L || is.na(batch_discount) || batch_discount <= 0) {
    rlang::abort("`batch_discount` must be a single numeric value > 0.")
  }
  if (!is.numeric(budget_quantile) || length(budget_quantile) != 1L ||
      is.na(budget_quantile) || budget_quantile <= 0 || budget_quantile >= 1) {
    rlang::abort("`budget_quantile` must be a single numeric value in (0, 1).")
  }

  n_total <- nrow(pairs)
  if (n_total == 0L) {
    rlang::abort("`pairs` has 0 rows.")
  }

  n_test <- as.integer(n_test)
  if (is.na(n_test) || n_test < 0L) rlang::abort("`n_test` must be a non-negative integer.")
  if (n_test > n_total) n_test <- n_total

  # ------------------------------------------------------------------
  # Hidden testing hook: allow injecting a submitter via ... without
  # namespace mocking. Not documented; safe no-op for end users.
  # ------------------------------------------------------------------
  dots <- list(...)
  submit_fun <- dots$.submit_fun
  if (is.null(submit_fun)) submit_fun <- submit_llm_pairs
  dots$.submit_fun <- NULL

  # ------------------------------------------------------------------
  # Prompt-bytes proxy for all pairs (offline)
  # ------------------------------------------------------------------
  prompt_bytes_all <- `.prompt_bytes_for_pairs`(
    template   = prompt_template,
    trait_name = trait_name,
    trait_desc = trait_description,
    text1      = as.character(pairs$text1),
    text2      = as.character(pairs$text2)
  )

  # ------------------------------------------------------------------
  # Select pilot indices
  # ------------------------------------------------------------------
  if (n_test == 0L) {
    test_idx <- integer(0)
  } else if (test_strategy == "first") {
    test_idx <- seq_len(n_test)
  } else {
    test_idx <- .pairwiseLLM_with_seed(seed, function() {
      if (test_strategy == "random") {
        return(sort(sample.int(n_total, size = n_test, replace = FALSE)))
      }

      # stratified_prompt_bytes
      if (n_total == 1L) {
        return(1L)
      }

      probs <- seq(0, 1, length.out = 6L)
      qs <- stats::quantile(prompt_bytes_all, probs = probs, na.rm = TRUE, type = 7)
      # Make breaks strictly increasing (defensive)
      brks <- unique(as.numeric(qs))
      if (length(brks) < 2L) {
        return(sort(sample.int(n_total, size = n_test, replace = FALSE)))
      }

      # Ensure the last break captures max
      brks[length(brks)] <- max(prompt_bytes_all, na.rm = TRUE) + 1
      strata <- cut(prompt_bytes_all, breaks = brks, include.lowest = TRUE, right = FALSE)
      by_stratum <- split(seq_len(n_total), strata)

      # Allocate approximately evenly across strata
      k <- length(by_stratum)
      base <- n_test %/% k
      rem  <- n_test %% k
      alloc <- rep(base, k)
      if (rem > 0) alloc[seq_len(rem)] <- alloc[seq_len(rem)] + 1L

      idx <- integer(0)
      for (j in seq_along(by_stratum)) {
        pool <- by_stratum[[j]]
        if (length(pool) == 0L || alloc[j] == 0L) next
        take <- min(length(pool), alloc[j])
        idx <- c(idx, sample(pool, size = take, replace = FALSE))
      }

      # If we didn't get enough (tiny strata), top up randomly from remaining
      idx <- unique(idx)
      if (length(idx) < n_test) {
        remaining <- setdiff(seq_len(n_total), idx)
        need <- n_test - length(idx)
        if (need > 0 && length(remaining) > 0) {
          idx <- c(idx, sample(remaining, size = min(need, length(remaining)), replace = FALSE))
        }
      }
      sort(idx)
    })
  }

  test_pairs <- pairs[test_idx, , drop = FALSE]
  remaining_idx <- setdiff(seq_len(n_total), test_idx)
  remaining_pairs <- pairs[remaining_idx, , drop = FALSE]

  # ------------------------------------------------------------------
  # Run pilot (live) and normalize pilot results shape
  # ------------------------------------------------------------------
  pilot <- NULL
  pilot_results <- NULL
  pilot_failed_attempts <- NULL
  # Backwards-compatible alias used in older tests / callers.
  pilot_failed_pairs <- NULL

  if (nrow(test_pairs) > 0L) {
    pilot <- do.call(
      submit_fun,
      c(
        list(
          pairs             = test_pairs,
          model             = model,
          trait_name        = trait_name,
          trait_description = trait_description,
          prompt_template   = prompt_template,
          backend           = backend,
          endpoint          = endpoint
        ),
        dots
      )
    )

    raw_input <- pilot
    if (is.list(pilot) && !inherits(pilot, "data.frame") && !is.null(pilot$results)) {
      raw_input <- pilot
    }

    normalized <- .normalize_llm_results(
      raw = raw_input,
      pairs = test_pairs,
      backend = backend,
      model = model,
      include_raw = FALSE
    )
    pilot_results <- normalized$results
    pilot_failed_attempts <- normalized$failed_attempts
    pilot_failed_pairs <- pilot_failed_attempts
  } else {
    pilot_results <- tibble::tibble()
    pilot_failed_pairs <- tibble::tibble()
  }

  # ------------------------------------------------------------------
  # Extract usable pilot rows for calibration and output stats
  # ------------------------------------------------------------------
  if (!("prompt_tokens" %in% names(pilot_results))) pilot_results$prompt_tokens <- NA_integer_
  if (!("completion_tokens" %in% names(pilot_results))) pilot_results$completion_tokens <- NA_integer_
  if (!("status_code" %in% names(pilot_results))) pilot_results$status_code <- NA_integer_

  if (is.null(pilot_failed_attempts)) pilot_failed_attempts <- tibble::tibble()
  pilot_attempts_all <- dplyr::bind_rows(pilot_results, pilot_failed_attempts)

  if (!("prompt_tokens" %in% names(pilot_attempts_all))) pilot_attempts_all$prompt_tokens <- NA_integer_
  if (!("completion_tokens" %in% names(pilot_attempts_all))) pilot_attempts_all$completion_tokens <- NA_integer_
  if (!("status_code" %in% names(pilot_attempts_all))) pilot_attempts_all$status_code <- NA_integer_

  usable_in_all <- which(
    !is.na(pilot_attempts_all$prompt_tokens) &
      (is.na(pilot_attempts_all$status_code) | pilot_attempts_all$status_code == 200L)
  )

  usable_out_all <- which(
    !is.na(pilot_attempts_all$completion_tokens) &
      (is.na(pilot_attempts_all$status_code) | pilot_attempts_all$status_code == 200L)
  )

  pilot_prompt_tokens_sum <- if (length(usable_in_all) > 0L) {
    sum(as.numeric(pilot_attempts_all$prompt_tokens[usable_in_all]), na.rm = TRUE)
  } else {
    0
  }

  pilot_completion_tokens_sum <- if (length(usable_out_all) > 0L) {
    sum(as.numeric(pilot_attempts_all$completion_tokens[usable_out_all]), na.rm = TRUE)
  } else {
    0
  }

  # Align pilot bytes with pilot rows (best-effort):
  # If pilot_results has custom_id/ID columns and order changed, we keep a simple positional match.
  pilot_bytes <- prompt_bytes_all[test_idx]
  if (length(pilot_bytes) != nrow(pilot_results)) {
    # fallback: recycle safely (should be rare)
    pilot_bytes <- rep(pilot_bytes, length.out = nrow(pilot_results))
  }

  usable_in <- which(
    !is.na(pilot_results$prompt_tokens) &
      (is.na(pilot_results$status_code) | pilot_results$status_code == 200L) &
      !is.na(pilot_bytes)
  )

  usable_out <- which(
    !is.na(pilot_results$completion_tokens) &
      (is.na(pilot_results$status_code) | pilot_results$status_code == 200L)
  )

  # ------------------------------------------------------------------
  # Calibrate prompt_tokens ~ prompt_bytes
  # ------------------------------------------------------------------
  intercept <- NA_real_
  slope <- NA_real_
  r2 <- NA_real_
  rmse <- NA_real_
  n_used <- length(usable_in)

  if (n_used >= 2L) {
    df_cal <- data.frame(
      prompt_tokens = as.numeric(pilot_results$prompt_tokens[usable_in]),
      prompt_bytes  = as.numeric(pilot_bytes[usable_in])
    )
    fit <- stats::lm(prompt_tokens ~ prompt_bytes, data = df_cal)
    co <- stats::coef(fit)
    intercept <- unname(co[1])
    slope <- unname(co[2])
    preds <- stats::predict(fit, newdata = df_cal)
    rmse <- sqrt(mean((preds - df_cal$prompt_tokens)^2))
    r2 <- summary(fit)$r.squared
  } else if (n_used == 1L) {
    # One point: assume intercept 0
    intercept <- 0
    slope <- as.numeric(pilot_results$prompt_tokens[usable_in]) / as.numeric(pilot_bytes[usable_in])
    r2 <- NA_real_
    rmse <- NA_real_
  } else {
    # No pilot tokens: fallback to a coarse bytes->tokens rule (bytes/4)
    intercept <- 0
    slope <- 1 / 4
    r2 <- NA_real_
    rmse <- NA_real_
  }

  # ------------------------------------------------------------------
  # Estimate remaining tokens
  # ------------------------------------------------------------------
  bytes_remaining <- prompt_bytes_all[remaining_idx]
  est_remaining_prompt_tokens <- if (length(bytes_remaining) > 0L) {
    pmax(0, as.numeric(round(intercept + slope * as.numeric(bytes_remaining))))
  } else {
    numeric(0)
  }
  est_remaining_prompt_tokens_sum <- sum(est_remaining_prompt_tokens, na.rm = TRUE)

  # Output tokens per remaining pair: expected (mean) and budget (quantile)
  out_samples <- as.numeric(pilot_results$completion_tokens[usable_out])
  out_mean <- if (length(out_samples) > 0L) mean(out_samples, na.rm = TRUE) else NA_real_
  out_q <- if (length(out_samples) > 0L) {
    stats::quantile(
      out_samples,
      probs = budget_quantile,
      na.rm = TRUE,
      names = FALSE,
      type = 7
    )
  } else {
    NA_real_
  }

  n_remaining <- length(remaining_idx)
  est_remaining_completion_expected <- if (!is.na(out_mean)) out_mean * n_remaining else NA_real_
  est_remaining_completion_budget <- if (!is.na(out_q)) out_q * n_remaining else NA_real_

  # ------------------------------------------------------------------
  # Cost calculation (expected and budget)
  # Pilot is always live; discount applies only to remaining when mode=batch.
  # ------------------------------------------------------------------
  total_prompt_tokens <- pilot_prompt_tokens_sum + est_remaining_prompt_tokens_sum
  total_completion_expected <- pilot_completion_tokens_sum + est_remaining_completion_expected
  total_completion_budget <- pilot_completion_tokens_sum + est_remaining_completion_budget

  pilot_cost_input <- (pilot_prompt_tokens_sum / 1e6) * cost_per_million_input
  pilot_cost_output <- (pilot_completion_tokens_sum / 1e6) * cost_per_million_output

  remaining_cost_input <- (est_remaining_prompt_tokens_sum / 1e6) * cost_per_million_input
  remaining_cost_output_expected <- (est_remaining_completion_expected / 1e6) * cost_per_million_output
  remaining_cost_output_budget <- (est_remaining_completion_budget / 1e6) * cost_per_million_output

  discount_mult <- if (identical(mode, "batch")) batch_discount else 1

  expected_cost_total <- pilot_cost_input + pilot_cost_output +
    discount_mult * (remaining_cost_input + remaining_cost_output_expected)

  budget_cost_total <- pilot_cost_input + pilot_cost_output +
    discount_mult * (remaining_cost_input + remaining_cost_output_budget)

  expected_cost_input <- pilot_cost_input + discount_mult * remaining_cost_input
  expected_cost_output <- pilot_cost_output + discount_mult * remaining_cost_output_expected

  budget_cost_input <- pilot_cost_input + discount_mult * remaining_cost_input
  budget_cost_output <- pilot_cost_output + discount_mult * remaining_cost_output_budget

  summary <- tibble::tibble(
    backend = backend,
    mode = mode,
    model = model,
    endpoint = if (identical(backend, "openai")) endpoint else NA_character_,
    n_total = n_total,
    n_test = length(test_idx),
    n_remaining = n_remaining,
    cost_per_million_input = as.numeric(cost_per_million_input),
    cost_per_million_output = as.numeric(cost_per_million_output),
    batch_discount = as.numeric(batch_discount),
    budget_quantile = as.numeric(budget_quantile),
    pilot_prompt_tokens = as.numeric(pilot_prompt_tokens_sum),
    pilot_completion_tokens = as.numeric(pilot_completion_tokens_sum),
    est_remaining_prompt_tokens = as.numeric(est_remaining_prompt_tokens_sum),
    est_remaining_completion_tokens_expected = as.numeric(est_remaining_completion_expected),
    est_remaining_completion_tokens_budget = as.numeric(est_remaining_completion_budget),
    expected_total_prompt_tokens = as.numeric(total_prompt_tokens),
    expected_total_completion_tokens = as.numeric(total_completion_expected),
    budget_total_completion_tokens = as.numeric(total_completion_budget),
    expected_cost_input = as.numeric(expected_cost_input),
    expected_cost_output = as.numeric(expected_cost_output),
    expected_cost_total = as.numeric(expected_cost_total),
    budget_cost_input = as.numeric(budget_cost_input),
    budget_cost_output = as.numeric(budget_cost_output),
    budget_cost_total = as.numeric(budget_cost_total)
  )

  calibration <- list(
    method = "lm(prompt_tokens ~ prompt_bytes)",
    coefficients = c(intercept = intercept, slope = slope),
    n_used = n_used,
    r_squared = r2,
    rmse = rmse
  )

  out <- list(
    summary = summary,
    calibration = calibration,
    test_pairs = test_pairs
  )

  if (isTRUE(return_test_results)) out$pilot <- pilot
  if (isTRUE(return_remaining_pairs)) out$remaining_pairs <- remaining_pairs
  if (!is.null(pilot_failed_pairs)) out$pilot_failed_pairs <- pilot_failed_pairs

  class(out) <- "pairwiseLLM_cost_estimate"
  out
}

#' Print a pairwiseLLM cost estimate
#'
#' Prints a compact, human-readable summary of an object returned by
#' \code{\link{estimate_llm_pairs_cost}}. The print method reports the backend,
#' model, pilot/remaining pair counts, estimated token totals, and both the
#' expected and budget cost estimates.
#'
#' @param x An object of class \code{"pairwiseLLM_cost_estimate"}, typically
#'   returned by \code{\link{estimate_llm_pairs_cost}}.
#' @param ... Unused. Included for method compatibility.
#'
#' @return \code{x}, invisibly.
#'
#' @examples
#' \dontrun{
#' data("example_writing_samples", package = "pairwiseLLM")
#' pairs <- example_writing_samples |>
#'   make_pairs() |>
#'   sample_pairs(n_pairs = 50, seed = 123)
#'
#' td <- trait_description("overall_quality")
#' tmpl <- set_prompt_template()
#'
#' est <- estimate_llm_pairs_cost(
#'   pairs = pairs,
#'   backend = "openai",
#'   model = "gpt-4.1",
#'   endpoint = "chat.completions",
#'   trait_name = td$name,
#'   trait_description = td$description,
#'   prompt_template = tmpl,
#'   mode = "batch",
#'   batch_discount = 0.5,
#'   n_test = 10,
#'   cost_per_million_input = 0.15,
#'   cost_per_million_output = 0.60
#' )
#'
#' est
#' }
#'
#' @export
print.pairwiseLLM_cost_estimate <- function(x, ...) {
  s <- x$summary

  # Defensive: handle missing/empty summary
  if (is.null(s) || nrow(s) == 0) {
    cat("<pairwiseLLM_cost_estimate>\n")
    cat("No summary available.\n")
    return(invisible(x))
  }

  cat("<pairwiseLLM_cost_estimate>\n")
  cat("Backend:", s$backend, "\n")
  cat("Model:", s$model, "\n")
  if (!is.na(s$endpoint)) cat("Endpoint:", s$endpoint, "\n")

  cat(
    "Pairs: ", s$n_total, " total (",
    s$n_test, " pilot, ",
    s$n_remaining, " remaining)\n",
    sep = ""
  )

  if (!is.na(s$batch_discount) && identical(s$mode, "batch")) {
    cat("Batch discount:", s$batch_discount, "\n")
  }

  cat(
    "Estimated prompt tokens:              ",
    format(round(s$expected_total_prompt_tokens), big.mark = ","),
    "\n", sep = ""
  )
  cat(
    "Estimated completion tokens (expected): ",
    format(round(s$expected_total_completion_tokens), big.mark = ","),
    "\n", sep = ""
  )
  cat(
    "Estimated completion tokens (budget):   ",
    format(round(s$budget_total_completion_tokens), big.mark = ","),
    "\n", sep = ""
  )

  cat("Expected cost:", format(s$expected_cost_total, digits = 6), "\n")
  cat("Budget cost:  ", format(s$budget_cost_total, digits = 6), "\n", sep = "")

  invisible(x)
}

# ---- Internal helpers ----

.prompt_bytes_for_pairs <- function(template, trait_name, trait_desc, text1, text2) {
  tmpl <- template
  tmpl <- gsub("{TRAIT_NAME}", trait_name, tmpl, fixed = TRUE)
  tmpl <- gsub("{TRAIT_DESCRIPTION}", trait_desc, tmpl, fixed = TRUE)

  n_s1 <- .count_fixed_occurrences(tmpl, "{SAMPLE_1}")
  n_s2 <- .count_fixed_occurrences(tmpl, "{SAMPLE_2}")

  base_bytes <- nchar(tmpl, type = "bytes") -
    n_s1 * nchar("{SAMPLE_1}", type = "bytes") -
    n_s2 * nchar("{SAMPLE_2}", type = "bytes")

  # Ensure character vectors
  text1 <- as.character(text1)
  text2 <- as.character(text2)

  base_bytes +
    n_s1 * nchar(text1, type = "bytes") +
    n_s2 * nchar(text2, type = "bytes")
}

.count_fixed_occurrences <- function(x, pattern) {
  m <- gregexpr(pattern, x, fixed = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) 0L else length(m)
}

.calibrate_prompt_tokens <- function(pilot_bytes, pilot_prompt_tokens) {
  pilot_bytes <- as.numeric(pilot_bytes)
  pilot_prompt_tokens <- as.numeric(pilot_prompt_tokens)

  n <- length(pilot_prompt_tokens)
  if (n == 0L) {
    return(list(
      method = "fallback_bytes_per_token",
      coefficients = c(intercept = 0, slope = 1 / 4),
      n_used = 0L,
      rmse = NA_real_,
      r_squared = NA_real_
    ))
  }

  if (n == 1L || length(unique(pilot_bytes)) == 1L) {
    slope <- if (pilot_bytes[1] > 0) pilot_prompt_tokens[1] / pilot_bytes[1] else 0
    return(list(
      method = "single_point_ratio",
      coefficients = c(intercept = 0, slope = slope),
      n_used = 1L,
      rmse = NA_real_,
      r_squared = NA_real_
    ))
  }

  df <- data.frame(prompt_tokens = pilot_prompt_tokens, prompt_bytes = pilot_bytes)
  fit <- stats::lm(prompt_tokens ~ prompt_bytes, data = df)
  co <- stats::coef(fit)
  pred <- stats::predict(fit, newdata = df)
  rmse <- sqrt(mean((pred - pilot_prompt_tokens)^2))
  r2 <- summary(fit)$r.squared

  list(
    method = "lm",
    coefficients = c(intercept = unname(co[1]), slope = unname(co[2])),
    n_used = n,
    rmse = rmse,
    r_squared = r2
  )
}

.predict_prompt_tokens <- function(calib, bytes_vec) {
  a <- calib$coefficients[["intercept"]]
  b <- calib$coefficients[["slope"]]
  pred <- a + b * as.numeric(bytes_vec)
  pred <- round(pred)
  pred[pred < 0] <- 0
  pred
}
