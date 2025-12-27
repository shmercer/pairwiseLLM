#' Compute text embeddings via Python (reticulate)
#'
#' Computes a numeric embedding matrix for a set of texts using a Python backend
#' accessed through \pkg{reticulate}. This is intended as a convenience helper
#' for workflows that select a linking core set using embeddings (see
#' \code{\link{select_core_set}}).
#'
#' The default engine uses the Python package \pkg{sentence-transformers} and
#' the \code{SentenceTransformer} API.
#'
#' @param x A character vector of texts, or a data.frame/tibble with a \code{text}
#'   column.
#' @param ids Optional character vector of IDs for the rows. If \code{x} is a
#'   data.frame/tibble with an \code{ID} column and \code{ids} is \code{NULL},
#'   \code{x$ID} is used.
#' @param engine Embedding engine. Currently only \code{"sentence_transformers"}
#'   is supported.
#' @param model Model name/path passed to \code{SentenceTransformer}.
#' @param batch_size Integer batch size passed to \code{encode}.
#' @param normalize Logical; if \code{TRUE}, request normalized embeddings from
#'   the Python backend when supported.
#' @param device Optional device string passed to \code{SentenceTransformer},
#'   e.g. \code{"cpu"} or \code{"cuda"}. If \code{NULL}, the Python package
#'   selects a default.
#' @param show_progress Logical; show a progress bar during encoding when the
#'   backend supports it.
#' @param ... Additional keyword arguments forwarded to the Python
#'   \code{encode()} method.
#'
#' @return A numeric matrix with one row per input text and one column per
#'   embedding dimension. If \code{ids} are available, they are set as
#'   \code{rownames()}.
#'
#' @examples
#' \dontrun{
#' # Requires reticulate + Python package sentence-transformers.
#' texts <- c("hello world", "goodbye")
#' emb <- compute_text_embeddings(texts)
#' dim(emb)
#' }
#'
#' @export
compute_text_embeddings <- function(x,
                                   ids = NULL,
                                   engine = c("sentence_transformers"),
                                   model = "all-MiniLM-L6-v2",
                                   batch_size = 32L,
                                   normalize = FALSE,
                                   device = NULL,
                                   show_progress = interactive(),
                                   ...) {
  engine <- match.arg(engine)

  if (is.data.frame(x)) {
    x <- tibble::as_tibble(x)
    if (!"text" %in% names(x)) stop("`x` must have a column named 'text'.", call. = FALSE)
    if (is.null(ids) && "ID" %in% names(x)) ids <- as.character(x$ID)
    x <- as.character(x$text)
  }

  if (!is.character(x)) stop("`x` must be a character vector of texts.", call. = FALSE)
  if (length(x) < 1L) stop("`x` must contain at least 1 text.", call. = FALSE)
  if (anyNA(x)) stop("`x` must not contain missing values.", call. = FALSE)

  if (!is.null(ids)) {
    if (!is.character(ids) || length(ids) != length(x) || anyNA(ids) || any(ids == "")) {
      stop("`ids` must be a non-missing character vector with length equal to `x`.", call. = FALSE)
    }
  }

  if (!is.numeric(batch_size) || length(batch_size) != 1L || is.na(batch_size) || batch_size < 1) {
    stop("`batch_size` must be a single integer >= 1.", call. = FALSE)
  }
  batch_size <- as.integer(batch_size)

  if (!is.logical(normalize) || length(normalize) != 1L || is.na(normalize)) {
    stop("`normalize` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(show_progress) || length(show_progress) != 1L || is.na(show_progress)) {
    stop("`show_progress` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(device) && (!is.character(device) || length(device) != 1L || is.na(device))) {
    stop("`device` must be NULL or a single character string.", call. = FALSE)
  }

  if (!.has_reticulate()) {
    stop("The 'reticulate' package is required to compute embeddings. Install it and try again.", call. = FALSE)
  }

  if (identical(engine, "sentence_transformers")) {
    if (!.rt_py_module_available("sentence_transformers")) {
      stop(
        "Python module 'sentence_transformers' is not available. ",
        "Install it in your reticulate Python environment (e.g., pip install sentence-transformers).",
        call. = FALSE
      )
    }

    st <- .rt_import("sentence_transformers")
    mdl <- if (is.null(device)) {
      st$SentenceTransformer(model)
    } else {
      st$SentenceTransformer(model, device = device)
    }

    enc <- tryCatch(
      mdl$encode(
        x,
        batch_size = batch_size,
        show_progress_bar = isTRUE(show_progress),
        convert_to_numpy = TRUE,
        normalize_embeddings = isTRUE(normalize),
        ...
      ),
      error = function(e) {
        stop("Embedding computation failed: ", conditionMessage(e), call. = FALSE)
      }
    )

    mat <- .rt_py_to_r(enc)
    mat <- as.matrix(mat)
    storage.mode(mat) <- "double"
    if (!is.null(ids)) rownames(mat) <- ids
    return(mat)
  }

  stop("Unsupported `engine`.", call. = FALSE)
}

.has_reticulate <- function() {
  requireNamespace("reticulate", quietly = TRUE)
}

.rt_py_module_available <- function(module) {
  reticulate::py_module_available(module)
}

.rt_import <- function(module) {
  reticulate::import(module, delay_load = TRUE)
}

.rt_py_to_r <- function(x) {
  reticulate::py_to_r(x)
}
