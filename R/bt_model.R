# ------------------------------------------------------------------------------
# Internal wrappers
# ------------------------------------------------------------------------------
# These small helpers serve two purposes:
#  1) They keep `fit_bt_model()` readable by centralising namespace calls.
#  2) They make hard-to-test branches testable *without* heavy-handed stubbing
#     (e.g., mocking `base::requireNamespace()` or `sirt::btm()` directly).


.require_ns <- function(pkg, quietly = TRUE) {
  base::requireNamespace(pkg, quietly = quietly)
}

.sirt_btm <- function(...) {
  sirt::btm(...)
}

#' Build Bradley-Terry comparison data from pairwise results
#'
#' This function converts pairwise comparison results into the
#' three-column format commonly used for Bradley-Terry models:
#' the first two columns contain object labels and the third
#' column contains the comparison result (1 for a win of the
#' first object, 0 for a win of the second).
#'
#' It assumes that the input contains columns \code{ID1},
#' \code{ID2}, and \code{better_id}, where \code{better_id}
#' is the ID of the better sample. Rows where \code{better_id}
#' does not match either \code{ID1} or \code{ID2} (including
#' \code{NA}) are excluded.
#'
#' Optionally, a judge/rater column can be carried through for
#' multi-judge modeling (e.g., different models/backends). When
#' \code{judge_col} is provided, the output includes a fourth
#' column \code{judge} (character), suitable for passing into
#' \code{sirt::btm()}.
#'
#' @param results A data frame or tibble with columns \code{ID1},
#'   \code{ID2}, and \code{better_id}.
#' @param judge_col Optional character column name in \code{results}
#'   identifying the judge/rater (for example, a model name). If
#'   provided, the output includes a \code{judge} column.
#'
#' @return A tibble with columns:
#'   \itemize{
#'     \item \code{object1}: ID from \code{ID1}
#'     \item \code{object2}: ID from \code{ID2}
#'     \item \code{result}: numeric value, 1 if \code{better_id == ID1},
#'       0 if \code{better_id == ID2}
#'     \item \code{judge}: (optional) judge/rater identifier, only when
#'       \code{judge_col} is provided
#'   }
#'   Rows with invalid or missing \code{better_id} are dropped.
#'
#' @examples
#' results <- tibble::tibble(
#'   ID1       = c("S1", "S1", "S2"),
#'   ID2       = c("S2", "S3", "S3"),
#'   better_id = c("S1", "S3", "S2")
#' )
#'
#' bt_data <- build_bt_data(results)
#' bt_data
#'
#' # Carry through judge/model labels
#' results2 <- tibble::tibble(
#'   ID1       = c("S1", "S1", "S2"),
#'   ID2       = c("S2", "S3", "S3"),
#'   better_id = c("S1", "S3", "S2"),
#'   model     = c("gpt-4o", "gpt-4o", "o1")
#' )
#'
#' bt_data2 <- build_bt_data(results2, judge_col = "model")
#' bt_data2
#'
#' # Using the example writing pairs
#' data("example_writing_pairs")
#' bt_ex <- build_bt_data(example_writing_pairs)
#' head(bt_ex)
#'
#' @export
build_bt_data <- function(results, judge_col = NULL) {
  results <- tibble::as_tibble(results)

  required_cols <- c("ID1", "ID2", "better_id")
  if (!all(required_cols %in% names(results))) {
    stop(
      "`results` must contain columns: ",
      paste(required_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.null(judge_col)) {
    if (!is.character(judge_col) || length(judge_col) != 1L) {
      stop("`judge_col` must be a single character column name.", call. = FALSE)
    }
    if (!judge_col %in% names(results)) {
      stop("`judge_col` must name a column in `results`.", call. = FALSE)
    }
  }

  # Ensure character IDs (avoid factors / labelled types)
  results <- dplyr::mutate(
    results,
    ID1 = as.character(.data$ID1),
    ID2 = as.character(.data$ID2),
    better_id = as.character(.data$better_id)
  )

  if (!is.null(judge_col)) {
    results <- dplyr::mutate(
      results,
      judge = as.character(.data[[judge_col]])
    )
  }

  out <- dplyr::mutate(
    results,
    result = dplyr::case_when(
      .data$better_id == .data$ID1 ~ 1L,
      .data$better_id == .data$ID2 ~ 0L,
      TRUE ~ NA_integer_
    )
  )

  # Drop rows where better_id doesn't match either member of the dyad
  out <- dplyr::filter(out, !is.na(.data$result))

  if (is.null(judge_col)) {
    out <- dplyr::transmute(
      out,
      object1 = .data$ID1,
      object2 = .data$ID2,
      result  = as.numeric(.data$result) # sirt::btm is happiest with numeric 0/1
    )
  } else {
    out <- dplyr::transmute(
      out,
      object1 = .data$ID1,
      object2 = .data$ID2,
      result  = as.numeric(.data$result),
      judge   = .data$judge
    )
  }

  tibble::as_tibble(out)
}

#' Fit a Bradley–Terry model with sirt and fallback to BradleyTerry2
#'
#' This function fits a Bradley–Terry paired-comparison model to data
#' prepared by \code{\link{build_bt_data}}. It supports two modeling
#' engines:
#' \itemize{
#'   \item \pkg{sirt}: \code{\link[sirt]{btm}} — the preferred engine, which
#'         produces ability estimates, standard errors, and MLE reliability.
#'   \item \pkg{BradleyTerry2}: \code{\link[BradleyTerry2]{BTm}} — used as a
#'         fallback if \pkg{sirt} is unavailable or fails; computes ability
#'         estimates and standard errors, but not reliability.
#' }
#'
#' When \code{engine = "auto"} (the default), the function attempts
#' \pkg{sirt} first and automatically falls back to \pkg{BradleyTerry2}
#' only if necessary. In all cases, the output format is standardized, so
#' downstream code can rely on consistent fields.
#'
#' @details
#' The input \code{bt_data} must contain exactly three columns:
#' \enumerate{
#'   \item object1: character ID for the first item in the pair
#'   \item object2: character ID for the second item
#'   \item result: numeric indicator (1 = object1 wins, 0 = object2 wins)
#' }
#'
#' Ability estimates (\code{theta}) represent latent "writing quality"
#' parameters on a log-odds scale. Standard errors are included for both
#' modeling engines. MLE reliability is only available from \pkg{sirt}.
#'
#' @param bt_data A data frame or tibble with exactly three columns:
#'   two character ID columns and one numeric \code{result} column
#'   equal to 0 or 1. Usually produced by \code{\link{build_bt_data}}.
#' @param engine Character string specifying the modeling engine. One of:
#'   \code{"auto"} (default), \code{"sirt"}, or \code{"BradleyTerry2"}.
#' @param verbose Logical. If \code{TRUE} (default), show engine output (iterations,
#'   warnings). If \code{FALSE}, suppress noisy output to keep
#'   examples and reports clean.
#' @param ... Additional arguments passed through to \code{sirt::btm()}
#'   or \code{BradleyTerry2::BTm()}.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{engine}{The engine actually used ("sirt" or "BradleyTerry2").}
#'   \item{fit}{The fitted model object.}
#'   \item{theta}{
#'     A tibble with columns:
#'     \itemize{
#'       \item \code{ID}: object identifier
#'       \item \code{theta}: estimated ability parameter
#'       \item \code{se}: standard error of \code{theta}
#'     }
#'   }
#'   \item{reliability}{
#'       MLE reliability (sirt engine only). \code{NA} for
#'       \pkg{BradleyTerry2} models.
#'   }
#' }
#'
#' @examples
#' # Example using built-in comparison data
#' data("example_writing_pairs")
#' bt <- build_bt_data(example_writing_pairs)
#'
#' fit1 <- fit_bt_model(bt, engine = "sirt")
#' fit2 <- fit_bt_model(bt, engine = "BradleyTerry2")
#'
#' @import tibble
#' @import dplyr
#' @importFrom stats aggregate
#' @export
fit_bt_model <- function(bt_data,
                         engine = c("auto", "sirt", "BradleyTerry2"),
                         verbose = TRUE,
                         ...) {
  bt_data <- as.data.frame(bt_data)
  if (ncol(bt_data) != 3L) {
    stop("`bt_data` must have exactly three columns.", call. = FALSE)
  }

  engine <- match.arg(engine)

  # --------------------------
  # sirt helper
  # --------------------------
  fit_sirt <- function(dat, verbose, ...) {
    if (!.require_ns("sirt", quietly = TRUE)) {
      stop(
        "Package 'sirt' must be installed to use engine = \"sirt\".\n",
        "Install it with: install.packages(\"sirt\")",
        call. = FALSE
      )
    }

    # sirt::btm often prints iteration progress. Capture when verbose = FALSE.
    run_btm <- function() .sirt_btm(dat, ...)

    fit <- if (isTRUE(verbose)) {
      run_btm()
    } else {
      suppressWarnings({
        tmp <- utils::capture.output(
          fit0 <- run_btm(),
          type = "output"
        )
        invisible(tmp)
        fit0
      })
    }

    effects <- fit$effects
    if (is.null(effects)) {
      stop("sirt::btm output missing `effects`.", call. = FALSE)
    }

    if (!all(c("individual", "theta", "se.theta") %in% names(effects))) {
      stop(
        "sirt::btm$effects does not contain expected columns ",
        "`individual`, `theta`, `se.theta`.",
        call. = FALSE
      )
    }

    theta <- tibble::tibble(
      ID    = effects$individual,
      theta = effects$theta,
      se    = effects$se.theta
    )

    list(
      engine      = "sirt",
      fit         = fit,
      theta       = theta,
      reliability = fit$mle.rel
    )
  }

  # --------------------------
  # BradleyTerry2 helper
  # --------------------------
  fit_bt2 <- function(dat, verbose, ...) {
    if (!.require_ns("BradleyTerry2", quietly = TRUE)) {
      stop(
        "Package 'BradleyTerry2' must be installed to use engine = \"BradleyTerry2\".\n",
        "Install it with: install.packages(\"BradleyTerry2\")",
        call. = FALSE
      )
    }

    dat <- as.data.frame(dat)
    names(dat)[1:3] <- c("object1", "object2", "result")

    # Aggregate wins for object1 vs object2
    wins1 <- stats::aggregate(I(result == 1) ~ object1 + object2, data = dat, sum)
    wins0 <- stats::aggregate(I(result == 0) ~ object1 + object2, data = dat, sum)

    agg <- merge(wins1, wins0, by = c("object1", "object2"), all = TRUE)
    agg[is.na(agg)] <- 0
    names(agg)[3:4] <- c("win1", "win2")

    # Force both player factors to share identical levels
    players <- sort(unique(c(agg$object1, agg$object2)))
    agg$object1 <- factor(agg$object1, levels = players)
    agg$object2 <- factor(agg$object2, levels = players)

    # Fit; optionally suppress warnings when verbose = FALSE (keeps examples clean)
    fit <- if (isTRUE(verbose)) {
      BradleyTerry2::BTm(
        outcome = cbind(agg$win1, agg$win2),
        player1 = agg$object1,
        player2 = agg$object2,
        data    = agg,
        ...
      )
    } else {
      suppressWarnings(
        BradleyTerry2::BTm(
          outcome = cbind(agg$win1, agg$win2),
          player1 = agg$object1,
          player2 = agg$object2,
          data    = agg,
          ...
        )
      )
    }

    abil <- BradleyTerry2::BTabilities(fit)

    theta <- tibble::tibble(
      ID    = rownames(abil),
      theta = abil[, 1],
      se    = abil[, 2]
    )

    list(
      engine      = "BradleyTerry2",
      fit         = fit,
      theta       = theta,
      reliability = NA_real_
    )
  }

  # --------------------------
  # Dispatch
  # --------------------------
  if (engine == "sirt") {
    return(fit_sirt(bt_data, verbose = verbose, ...))
  }

  if (engine == "BradleyTerry2") {
    return(fit_bt2(bt_data, verbose = verbose, ...))
  }

  res_sirt <- tryCatch(
    fit_sirt(bt_data, verbose = verbose, ...),
    error = function(e) e
  )
  if (!inherits(res_sirt, "error")) {
    return(res_sirt)
  }

  res_bt2 <- tryCatch(
    fit_bt2(bt_data, verbose = verbose, ...),
    error = function(e) e
  )
  if (!inherits(res_bt2, "error")) {
    return(res_bt2)
  }

  stop(
    "Both sirt and BradleyTerry2 failed:\n",
    "sirt error: ", conditionMessage(res_sirt), "\n",
    "BradleyTerry2 error: ", conditionMessage(res_bt2),
    call. = FALSE
  )
}
