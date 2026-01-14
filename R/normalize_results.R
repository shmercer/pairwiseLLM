# -------------------------------------------------------------------------
# Internal helper: normalize LLM results across backends
# -------------------------------------------------------------------------

#' @keywords internal
#' @noRd
.normalize_llm_results <- function(
    raw,
    pairs,
    backend,
    model,
    include_raw = FALSE
) {
  pairs <- tibble::as_tibble(pairs)

  # Backwards/alternate schema support (additive-only): accept A/B naming.
  if (!"ID1" %in% names(pairs) && "A_id" %in% names(pairs)) {
    pairs$ID1 <- as.character(pairs$A_id)
  }
  if (!"ID2" %in% names(pairs) && "B_id" %in% names(pairs)) {
    pairs$ID2 <- as.character(pairs$B_id)
  }
  if (!"text1" %in% names(pairs) && "A_text" %in% names(pairs)) {
    pairs$text1 <- pairs$A_text
  }
  if (!"text2" %in% names(pairs) && "B_text" %in% names(pairs)) {
    pairs$text2 <- pairs$B_text
  }

  required_id_cols <- c("ID1", "ID2")
  missing_id_cols <- setdiff(required_id_cols, names(pairs))
  if (length(missing_id_cols) > 0L) {
    rlang::abort(paste0(
      "`pairs` must contain columns: ",
      paste(required_id_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_id_cols, collapse = ", ")
    ))
  }
  if (!"text1" %in% names(pairs)) pairs$text1 <- NA_character_
  if (!"text2" %in% names(pairs)) pairs$text2 <- NA_character_

  backend <- as.character(backend)
  model <- as.character(model)

  raw_results <- raw
  raw_failed <- NULL
  if (is.list(raw) && !inherits(raw, "data.frame")) {
    if (!is.null(raw$results)) raw_results <- raw$results
    if (!is.null(raw$failed_attempts)) {
      raw_failed <- raw$failed_attempts
    } else if (!is.null(raw$failed_pairs)) {
      raw_failed <- raw$failed_pairs
    }
  }

  raw_tbl <- tibble::as_tibble(raw_results)

  # Strip any canonical columns if an upstream helper already added them.
  canonical_cols <- c(
    "A_id", "B_id", "unordered_key", "ordered_key", "pair_uid",
    "unordered_occurrence_index", "ordered_occurrence_index",
    "winner_pos", "backend", "received_at"
  )
  raw_tbl <- raw_tbl |>
    dplyr::select(-dplyr::any_of(canonical_cols))

  if (!include_raw && "raw_response" %in% names(raw_tbl)) {
    raw_tbl$raw_response <- NULL
  }

  # Support legacy LIVE_<ID1>_vs_<ID2> ids by parsing when explicit IDs missing.
  parse_custom_id <- function(custom_id) {
    if (is.na(custom_id) || is.null(custom_id) || !nzchar(custom_id)) {
      return(list(ID1 = NA_character_, ID2 = NA_character_))
    }
    parts <- strsplit(custom_id, "_vs_", fixed = TRUE)[[1L]]
    if (length(parts) != 2L) {
      return(list(ID1 = NA_character_, ID2 = NA_character_))
    }
    left <- parts[1L]
    right <- parts[2L]
    m <- regexpr("_[^_]*$", left)
    if (m[1L] > 0L) {
      id1 <- substring(left, m[1L] + 1L)
    } else {
      id1 <- left
    }
    list(ID1 = id1, ID2 = right)
  }

  if (!all(c("ID1", "ID2") %in% names(raw_tbl)) && "custom_id" %in% names(raw_tbl)) {
    parsed_ids <- lapply(raw_tbl$custom_id, parse_custom_id)
    raw_tbl$ID1 <- vapply(parsed_ids, `[[`, character(1L), "ID1")
    raw_tbl$ID2 <- vapply(parsed_ids, `[[`, character(1L), "ID2")
  }

  pairs_keyed <- pairs |>
    dplyr::mutate(
      A_id = as.character(.data$ID1),
      B_id = as.character(.data$ID2)
    )

  unordered_key <- vapply(
    seq_len(nrow(pairs_keyed)),
    function(i) paste(sort(c(pairs_keyed$A_id[i], pairs_keyed$B_id[i])), collapse = ":"),
    character(1L)
  )

  pairs_keyed <- pairs_keyed |>
    dplyr::mutate(
      unordered_key = unordered_key,
      ordered_key = paste(.data$A_id, .data$B_id, sep = ":")
    ) |>
    dplyr::group_by(.data$unordered_key) |>
    dplyr::mutate(unordered_occurrence_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$ordered_key) |>
    dplyr::mutate(ordered_occurrence_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      pair_uid = paste0(.data$unordered_key, "#", .data$unordered_occurrence_index),
      pair_uid_input = if ("pair_uid" %in% names(pairs)) as.character(pairs$pair_uid) else NA_character_
    )

  # Prepare a deduped raw table for alignment, while recording duplicates as attempts.
  extra_attempts_tbl <- tibble::tibble()

  join_mode <- NULL
  aligned <- NULL

  if ("pair_uid" %in% names(pairs) && "custom_id" %in% names(raw_tbl) &&
      any(raw_tbl$custom_id %in% pairs$pair_uid)) {
    join_mode <- "custom_id"

    raw_tbl <- raw_tbl |>
      dplyr::mutate(custom_id = as.character(.data$custom_id)) |>
      dplyr::group_by(.data$custom_id) |>
      dplyr::mutate(.raw_occurrence = dplyr::row_number(), .raw_n = dplyr::n()) |>
      dplyr::ungroup()

    # Treat duplicate custom_id rows as non-observed attempts (keep last for observed alignment).
    dups <- raw_tbl |>
      dplyr::filter(.data$.raw_n > 1L & .data$.raw_occurrence < .data$.raw_n)

    if (nrow(dups) > 0L) {
      extra_attempts_tbl <- dups |>
        dplyr::transmute(
          custom_id = .data$custom_id,
          error_code = "http_error",
          error_detail = "Duplicate result row for custom_id; treated as non-observed attempt",
          attempted_at = Sys.time()
        )
    }

    raw_dedup <- raw_tbl |>
      dplyr::group_by(.data$custom_id) |>
      dplyr::slice_tail(n = 1L) |>
      dplyr::ungroup() |>
      dplyr::mutate(.matched = TRUE) |>
      dplyr::select(-dplyr::any_of(c(".raw_occurrence", ".raw_n")))

    aligned <- dplyr::left_join(
      pairs_keyed,
      raw_dedup,
      by = c("pair_uid_input" = "custom_id")
    ) |>
      dplyr::mutate(custom_id = .data$pair_uid_input)

  } else if (all(c("ID1", "ID2") %in% names(raw_tbl))) {
    join_mode <- "id"

    raw_tbl <- raw_tbl |>
      dplyr::mutate(
        ID1 = as.character(.data$ID1),
        ID2 = as.character(.data$ID2),
        ordered_key = paste(.data$ID1, .data$ID2, sep = ":")
      ) |>
      dplyr::group_by(.data$ordered_key) |>
      dplyr::mutate(ordered_occurrence_index = dplyr::row_number(), .raw_n = dplyr::n()) |>
      dplyr::ungroup()

    dups <- raw_tbl |>
      dplyr::filter(.data$.raw_n > 1L)

    if (nrow(dups) > 0L) {
      # Keep last occurrence per ordered_key for alignment; record earlier as failed attempts.
      extra_attempts_tbl <- dups |>
        dplyr::group_by(.data$ordered_key) |>
        dplyr::slice_head(n = dplyr::n() - 1L) |>
        dplyr::ungroup() |>
        dplyr::transmute(
          ID1 = .data$ID1,
          ID2 = .data$ID2,
          ordered_key = .data$ordered_key,
          ordered_occurrence_index = .data$ordered_occurrence_index,
          error_code = "http_error",
          error_detail = "Duplicate result row for ordered pair; treated as non-observed attempt",
          attempted_at = Sys.time()
        )
    }

    raw_dedup <- raw_tbl |>
      dplyr::group_by(.data$ordered_key) |>
      dplyr::slice_tail(n = 1L) |>
      dplyr::ungroup() |>
      dplyr::mutate(.matched = TRUE) |>
      dplyr::select(-dplyr::any_of(c(".raw_n")))

    aligned <- dplyr::left_join(
      pairs_keyed,
      raw_dedup,
      by = c("ordered_key", "ordered_occurrence_index")
    )

  } else if (nrow(raw_tbl) == nrow(pairs_keyed)) {
    join_mode <- "row_order"
    raw_trimmed <- raw_tbl |>
      dplyr::select(-dplyr::any_of(c("ID1", "ID2"))) |>
      dplyr::mutate(.matched = TRUE)
    aligned <- dplyr::bind_cols(pairs_keyed, raw_trimmed)
  } else {
    rlang::abort("Unable to align `raw` results with `pairs`.")
  }

  if (any(is.na(aligned$.matched))) {
    rlang::abort("`raw` results could not be fully aligned with `pairs`.")
  }
  aligned$.matched <- NULL

  if ("pair_uid" %in% names(pairs)) {
    aligned$custom_id <- aligned$pair_uid_input
  }

  if (!"better_id" %in% names(aligned)) aligned$better_id <- NA_character_

  if ("better_sample" %in% names(aligned)) {
    mapped <- ifelse(
      aligned$better_sample == "SAMPLE_1",
      aligned$A_id,
      ifelse(aligned$better_sample == "SAMPLE_2", aligned$B_id, NA_character_)
    )
    aligned$better_id <- ifelse(is.na(aligned$better_id), mapped, aligned$better_id)
  }

  aligned$backend <- backend

  if (!"model" %in% names(aligned)) aligned$model <- NA_character_
  aligned$model <- as.character(aligned$model)
  aligned$model <- ifelse(is.na(aligned$model), model, aligned$model)

  determine_error_code <- function(better_id, a_id, b_id, status_code, error_message, result_type) {
    if (!is.na(better_id) && better_id %in% c(a_id, b_id)) return(NA_character_)

    if (!is.na(error_message) && grepl("timeout|timed out", error_message, ignore.case = TRUE)) {
      return("timeout")
    }
    if (!is.na(error_message) && grepl("parse", error_message, ignore.case = TRUE)) {
      return("parse_error")
    }
    if (!is.na(status_code) && status_code >= 400L) return("http_error")
    if (!is.na(result_type) && result_type %in% c("errored", "failed", "canceled", "expired")) {
      return("http_error")
    }
    if (is.na(better_id)) return("backend_missing_fields")
    "invalid_winner"
  }

  status_code <- if ("status_code" %in% names(aligned)) aligned$status_code else NA_integer_
  error_message <- if ("error_message" %in% names(aligned)) as.character(aligned$error_message) else NA_character_
  result_type <- if ("result_type" %in% names(aligned)) as.character(aligned$result_type) else NA_character_

  error_code <- mapply(
    determine_error_code,
    aligned$better_id,
    aligned$A_id,
    aligned$B_id,
    status_code,
    error_message,
    result_type,
    USE.NAMES = FALSE
  )

  is_valid <- is.na(error_code)

  aligned$winner_pos <- ifelse(
    aligned$better_id == aligned$A_id,
    1L,
    ifelse(aligned$better_id == aligned$B_id, 2L, NA_integer_)
  )

  timestamp <- Sys.time()

  results_tbl <- tibble::as_tibble(aligned[is_valid, , drop = FALSE])
  results_tbl$received_at <- timestamp

  # Failed pairs are the scheduled pairs whose outcome is not observed.
  failed_tbl <- aligned[!is_valid, , drop = FALSE]
  failed_tbl$error_code <- error_code[!is_valid]
  failed_tbl$error_detail <- error_message[!is_valid]

  failed_tbl$error_detail <- ifelse(
    failed_tbl$error_code == "invalid_winner",
    paste0("Invalid winner: ", failed_tbl$better_id),
    failed_tbl$error_detail
  )

  failed_tbl$error_detail <- ifelse(
    is.na(failed_tbl$error_detail) & !is.na(status_code[!is_valid]),
    paste0("HTTP ", status_code[!is_valid]),
    failed_tbl$error_detail
  )

  failed_tbl$attempted_at <- timestamp

  failed_pairs_tbl <- tibble::as_tibble(failed_tbl) |>
    dplyr::select(
      dplyr::any_of(c(
        "ID1", "ID2", "A_id", "B_id", "unordered_key", "ordered_key",
        "pair_uid", "unordered_occurrence_index", "ordered_occurrence_index",
        "custom_id", "backend", "model", "status_code", "error_message",
        "error_code", "error_detail", "attempted_at"
      ))
    )

  failed_attempts_tbl <- tibble::tibble(
    A_id = failed_tbl$A_id,
    B_id = failed_tbl$B_id,
    unordered_key = failed_tbl$unordered_key,
    ordered_key = failed_tbl$ordered_key,
    backend = failed_tbl$backend,
    model = failed_tbl$model,
    error_code = failed_tbl$error_code,
    error_detail = failed_tbl$error_detail,
    attempted_at = failed_tbl$attempted_at
  )

  # Add duplicate-result rows as non-observed attempts.
  if (nrow(extra_attempts_tbl) > 0L) {
    if ("custom_id" %in% names(extra_attempts_tbl)) {
      extra_joined <- dplyr::left_join(
        pairs_keyed,
        extra_attempts_tbl,
        by = c("pair_uid_input" = "custom_id")
      )
    } else {
      extra_joined <- dplyr::left_join(
        pairs_keyed,
        extra_attempts_tbl,
        by = c("ordered_key", "ordered_occurrence_index")
      )
    }

    extra_failed <- tibble::tibble(
      A_id = extra_joined$A_id,
      B_id = extra_joined$B_id,
      unordered_key = extra_joined$unordered_key,
      ordered_key = extra_joined$ordered_key,
      backend = backend,
      model = model,
      error_code = extra_joined$error_code,
      error_detail = extra_joined$error_detail,
      attempted_at = extra_joined$attempted_at
    )

    failed_attempts_tbl <- dplyr::bind_rows(failed_attempts_tbl, extra_failed)
  }

  # Add backend-provided failed rows, if any.
  if (!is.null(raw_failed)) {
    raw_failed_tbl <- tibble::as_tibble(raw_failed)
    if (nrow(raw_failed_tbl) > 0L) {
      raw_failed_tbl <- raw_failed_tbl |>
        dplyr::mutate(.matched = TRUE)

      failed_joined <- NULL
      if ("pair_uid" %in% names(pairs) && "custom_id" %in% names(raw_failed_tbl)) {
        failed_joined <- dplyr::left_join(
          pairs_keyed,
          raw_failed_tbl,
          by = c("pair_uid_input" = "custom_id")
        )
      } else if (all(c("ID1", "ID2") %in% names(raw_failed_tbl))) {
        raw_failed_tbl <- raw_failed_tbl |>
          dplyr::mutate(
            ID1 = as.character(.data$ID1),
            ID2 = as.character(.data$ID2),
            ordered_key = paste(.data$ID1, .data$ID2, sep = ":")
          ) |>
          dplyr::group_by(.data$ordered_key) |>
          dplyr::mutate(ordered_occurrence_index = dplyr::row_number()) |>
          dplyr::ungroup()

        failed_joined <- dplyr::left_join(
          pairs_keyed,
          raw_failed_tbl,
          by = c("ordered_key", "ordered_occurrence_index")
        )
      }

      if (!is.null(failed_joined)) {
        extra_error_message <- if ("error_message" %in% names(failed_joined)) {
          as.character(failed_joined$error_message)
        } else {
          NA_character_
        }

        extra_code <- ifelse(
          is.na(extra_error_message),
          "http_error",
          ifelse(
            grepl("timeout|timed out", extra_error_message, ignore.case = TRUE),
            "timeout",
            "http_error"
          )
        )

        extra_failed <- tibble::tibble(
          A_id = failed_joined$A_id,
          B_id = failed_joined$B_id,
          unordered_key = failed_joined$unordered_key,
          ordered_key = failed_joined$ordered_key,
          backend = backend,
          model = model,
          error_code = extra_code,
          error_detail = extra_error_message,
          attempted_at = timestamp
        )

        failed_attempts_tbl <- dplyr::bind_rows(failed_attempts_tbl, extra_failed)
      }
    }
  }

  # Add retry failures recorded on the response object.
  if ("retry_failures" %in% names(aligned)) {
    retry_rows <- vector("list", nrow(aligned))
    for (i in seq_len(nrow(aligned))) {
      failures <- aligned$retry_failures[[i]]
      if (is.null(failures) || length(failures) == 0L) next

      failure_tbl <- tibble::as_tibble(failures)
      if (!"error_code" %in% names(failure_tbl)) failure_tbl$error_code <- "http_error"
      if (!"error_detail" %in% names(failure_tbl)) failure_tbl$error_detail <- NA_character_
      if (!"attempted_at" %in% names(failure_tbl)) failure_tbl$attempted_at <- timestamp

      retry_rows[[i]] <- tibble::tibble(
        A_id = aligned$A_id[i],
        B_id = aligned$B_id[i],
        unordered_key = aligned$unordered_key[i],
        ordered_key = aligned$ordered_key[i],
        backend = aligned$backend[i],
        model = aligned$model[i],
        error_code = failure_tbl$error_code,
        error_detail = failure_tbl$error_detail,
        attempted_at = failure_tbl$attempted_at
      )
    }

    retry_rows <- Filter(Negate(is.null), retry_rows)
    if (length(retry_rows) > 0L) {
      failed_attempts_tbl <- dplyr::bind_rows(
        failed_attempts_tbl,
        dplyr::bind_rows(retry_rows)
      )
    }
  }

  results_tbl <- results_tbl |>
    dplyr::select(-dplyr::any_of(c("pair_uid_input", "retry_failures")))

  list(
    results = tibble::as_tibble(results_tbl),
    failed_pairs = tibble::as_tibble(failed_pairs_tbl),
    failed_attempts = tibble::as_tibble(failed_attempts_tbl),
    alignment = join_mode
  )
}
