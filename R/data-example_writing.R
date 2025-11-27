#' Example dataset of writing samples
#'
#' A small set of ten writing samples on the topic
#' "Why is writing assessment difficult?", intended for use in
#' examples and tests involving pairing and LLM-based comparisons.
#' The samples vary in quality, approximately from very weak to very
#' strong, and a simple numeric quality score is included to support
#' simulated comparison outcomes.
#'
#' @format A tibble with 10 rows and 3 variables:
#' \describe{
#'   \item{ID}{Character ID for each sample (e.g., \code{"S01"}).}
#'   \item{text}{Character string with the writing sample.}
#'   \item{quality_score}{Integer from 1 to 10 indicating the
#'     intended relative quality of the sample (higher = better).}
#' }
#'
#' @usage data("example_writing_samples")
#'
#' @examples
#' data("example_writing_samples")
#' example_writing_samples
#'
#' @docType data
#' @keywords datasets
#' @name example_writing_samples
NULL

#' Example dataset of paired comparisons for writing samples
#'
#' A complete set of unordered paired comparison outcomes for the
#' ten samples in \code{\link{example_writing_samples}}. For each
#' pair of IDs, the \code{better_id} field indicates which sample
#' is assumed to be better, based on the \code{quality_score} in
#' \code{example_writing_samples}.
#'
#' This dataset is useful for demonstrating functions that process
#' paired comparisons (e.g., building Bradley-Terry data and
#' fitting \code{\link[sirt]{btm}} models) without requiring any
#' calls to an LLM.
#'
#' @format A tibble with 45 rows and 3 variables:
#' \describe{
#'   \item{ID1}{Character ID of the first sample in the pair.}
#'   \item{ID2}{Character ID of the second sample in the pair.}
#'   \item{better_id}{Character ID of the sample judged better in
#'     this pair (either \code{ID1} or \code{ID2}).}
#' }
#'
#' @usage data("example_writing_pairs")
#'
#' @examples
#' data("example_writing_pairs")
#' head(example_writing_pairs)
#'
#' @docType data
#' @keywords datasets
#' @name example_writing_pairs
NULL

#' Example OpenAI Batch output (JSONL lines)
#'
#' A small character vector containing three example lines from an
#' OpenAI Batch API output file in JSONL format. Each element is a
#' single JSON object representing the result for one batch request.
#'
#' The structure follows the current Batch API output schema, with
#' fields such as \code{id}, \code{custom_id}, and a nested
#' \code{response} object containing \code{status_code},
#' \code{request_id}, and a \code{body} that resembles a regular
#' chat completion response. One line illustrates a successful
#' comparison where \code{<BETTER_SAMPLE>SAMPLE_1</BETTER_SAMPLE>}
#' is returned, one illustrates a case where SAMPLE_2 is preferred,
#' and one illustrates an error case with a non-200 status.
#'
#' This dataset is designed for use in examples and tests of batch
#' output parsing functions. Typical usage is to write the lines to
#' a temporary file and then read/parse them as a JSONL batch file.
#'
#' @format A character vector of length 3, where each element is a
#' single JSON line (JSONL).
#'
#' @usage data("example_openai_batch_output")
#'
#' @examples
#' data("example_openai_batch_output")
#'
#' # Inspect the first line
#' cat(example_openai_batch_output[1], "\n")
#'
#' # Write to a temporary .jsonl file for parsing
#' tmp <- tempfile(fileext = ".jsonl")
#' writeLines(example_openai_batch_output, con = tmp)
#' tmp
#'
#' @docType data
#' @keywords datasets
#' @name example_openai_batch_output
NULL
