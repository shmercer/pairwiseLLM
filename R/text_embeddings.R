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
#' @param cache_dir Optional directory to cache computed embeddings as \code{.rds}
#'   files. If \code{NULL} (default), caching is disabled.
#' @param use_cache Logical; when \code{TRUE} and \code{cache_dir} is provided,
#'   reuse cached embeddings when available. When \code{FALSE}, embeddings are
#'   computed fresh (and the cache is not read or written).
#' @param overwrite Logical; when \code{TRUE} and \code{cache_dir} is provided,
#'   overwrite any existing cache entry for the current inputs.
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
#' emb <- compute_text_embeddings(texts, cache_dir = tempdir())
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
                                    cache_dir = NULL,
                                    use_cache = TRUE,
                                    overwrite = FALSE,
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

  # Convenience: allow cache_dir to be configured via environment variable.
  # This is useful for project-level configuration (for example, a local
  # `.Renviron`) so users don't need to pass `cache_dir` explicitly everywhere.
  if (is.null(cache_dir)) {
    env_cache <- trimws(Sys.getenv("PAIRWISELLM_EMBEDDINGS_CACHE_DIR", unset = ""))
    if (nzchar(env_cache)) {
      cache_dir <- env_cache
    }
  }

  if (!is.null(cache_dir) && (!is.character(cache_dir) || length(cache_dir) != 1L || is.na(cache_dir))) {
    stop("`cache_dir` must be NULL or a single directory path.", call. = FALSE)
  }
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    stop("`use_cache` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }

  dots <- list(...)

  compute_fn <- function(texts) {
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

      args <- c(
        list(
          texts,
          batch_size = batch_size,
          show_progress_bar = isTRUE(show_progress),
          convert_to_numpy = TRUE,
          normalize_embeddings = isTRUE(normalize)
        ),
        dots
      )

      enc <- tryCatch(
        do.call(mdl$encode, args),
        error = function(e) {
          stop("Embedding computation failed: ", conditionMessage(e), call. = FALSE)
        }
      )

      mat <- .rt_py_to_r(enc)
      mat <- as.matrix(mat)
      storage.mode(mat) <- "double"
      return(mat)
    }

    stop("Unsupported `engine`.", call. = FALSE)
  }

  mat <- .compute_text_embeddings_cached(
    texts = x,
    ids = ids,
    engine = engine,
    model = model,
    batch_size = batch_size,
    normalize = normalize,
    device = device,
    show_progress = show_progress,
    dots = dots,
    cache_dir = cache_dir,
    use_cache = use_cache,
    overwrite = overwrite,
    compute_fn = compute_fn
  )

  if (!is.null(ids)) rownames(mat) <- ids
  mat
}

# Internal helper that wraps optional caching around a compute function.
.compute_text_embeddings_cached <- function(texts,
                                            ids = NULL,
                                            engine,
                                            model,
                                            batch_size,
                                            normalize,
                                            device,
                                            show_progress,
                                            dots = list(),
                                            cache_dir = NULL,
                                            use_cache = TRUE,
                                            overwrite = FALSE,
                                            compute_fn) {
  if (!is.function(compute_fn)) stop("Internal error: `compute_fn` must be a function.", call. = FALSE)

  # Try cache first (before requiring Python/reticulate).
  cache_path <- NULL
  if (!is.null(cache_dir) && isTRUE(use_cache)) {
    key <- .embeddings_cache_key(
      texts = texts,
      engine = engine,
      model = model,
      batch_size = batch_size,
      normalize = normalize,
      device = device,
      show_progress = show_progress,
      dots = dots
    )
    cache_path <- .embeddings_cache_path(cache_dir, key)

    if (file.exists(cache_path) && !isTRUE(overwrite)) {
      cached <- .read_embeddings_cache(cache_path)
      if (!is.null(cached)) {
        mat <- cached$embeddings
        if (is.matrix(mat) && nrow(mat) == length(texts)) {
          storage.mode(mat) <- "double"
          return(mat)
        }
        # If the cache is malformed, fall through to recompute.
      }
    }
  }

  # Compute fresh.
  mat <- compute_fn(texts)
  mat <- as.matrix(mat)
  storage.mode(mat) <- "double"
  if (nrow(mat) != length(texts)) {
    stop("Embedding computation returned the wrong number of rows.", call. = FALSE)
  }

  # Write cache if enabled.
  if (!is.null(cache_path) && isTRUE(use_cache)) {
    .write_embeddings_cache(
      cache_path,
      embeddings = mat,
      meta = list(
        n = length(texts),
        engine = engine,
        model = model,
        batch_size = batch_size,
        normalize = normalize,
        device = device,
        show_progress = show_progress,
        dots = dots
      )
    )
  }

  mat
}

.embeddings_cache_key <- function(texts,
                                  engine,
                                  model,
                                  batch_size,
                                  normalize,
                                  device,
                                  show_progress,
                                  dots = list()) {
  # Deterministic key without extra dependencies.
  obj <- list(
    texts = texts,
    engine = engine,
    model = model,
    batch_size = batch_size,
    normalize = normalize,
    device = device,
    show_progress = show_progress,
    dots = dots
  )
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(obj, tf, version = 2)
  unname(as.character(tools::md5sum(tf)))
}

.embeddings_cache_path <- function(cache_dir, key) {
  # Ensure directory exists (best-effort).
  if (!dir.exists(cache_dir)) {
    ok <- dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop("Could not create cache directory: ", cache_dir, call. = FALSE)
    }
  }
  file.path(cache_dir, paste0("embeddings_", key, ".rds"))
}

.read_embeddings_cache <- function(path) {
  out <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(out)) {
    return(NULL)
  }
  if (is.list(out) && !is.null(out$embeddings)) {
    return(out)
  }
  if (is.matrix(out)) {
    return(list(embeddings = out, meta = NULL))
  }
  NULL
}

.write_embeddings_cache <- function(path = NULL,
                                    embeddings,
                                    meta = NULL,
                                    cache_path = NULL,
                                    row_key = NULL,
                                    ...) {
  # Back-compat for older arg name:
  if (is.null(path) && !is.null(cache_path)) path <- cache_path
  if (is.null(path) || !nzchar(path)) {
    stop("`path` must be provided.", call. = FALSE)
  }

  dir <- dirname(path)

  # If the "dir" exists as a file, bail out (prevents gzfile warnings on Windows)
  if (file.exists(dir) && !dir.exists(dir)) {
    warning("Failed to write embeddings cache: cache directory exists as a file: ", dir, call. = FALSE)
    return(invisible(FALSE))
  }

  if (!dir.exists(dir)) {
    ok <- dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      warning("Failed to write embeddings cache: could not create directory: ", dir, call. = FALSE)
      return(invisible(FALSE))
    }
  }

  ok <- tryCatch(
    {
      suppressWarnings(saveRDS(list(embeddings = embeddings, meta = meta), path, version = 2))
      TRUE
    },
    error = function(e) {
      warning("Failed to write embeddings cache: ", conditionMessage(e), call. = FALSE)
      FALSE
    }
  )

  invisible(ok)
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
