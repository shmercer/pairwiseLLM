# -------------------------------------------------------------------------
# Benchmark metric extractors (report-only).
# -------------------------------------------------------------------------

.adaptive_link_mix_metrics <- function(step_log) {
  log <- tibble::as_tibble(step_log)
  if (nrow(log) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      pair_type = character(),
      committed = integer(),
      proportion = double()
    ))
  }

  staged <- log[
    log$round_stage %in% .adaptive_stage_order() &
      !is.na(log$pair_id) &
      !is.na(log$round_id),
    ,
    drop = FALSE
  ]
  if (nrow(staged) == 0L) {
    return(tibble::tibble(
      round_id = integer(),
      round_stage = character(),
      pair_type = character(),
      committed = integer(),
      proportion = double()
    ))
  }

  counts <- staged |>
    dplyr::count(.data$round_id, .data$round_stage, .data$pair_type, name = "committed")
  totals <- staged |>
    dplyr::count(.data$round_id, .data$round_stage, name = "stage_total")
  out <- counts |>
    dplyr::left_join(totals, by = c("round_id", "round_stage")) |>
    dplyr::mutate(proportion = as.double(.data$committed / .data$stage_total)) |>
    dplyr::select("round_id", "round_stage", "pair_type", "committed", "proportion")

  tibble::as_tibble(out)
}

.adaptive_fallback_distribution <- function(step_log) {
  log <- tibble::as_tibble(step_log)
  if (nrow(log) == 0L) {
    return(tibble::tibble(
      fallback_used = character(),
      attempts = integer(),
      share = double()
    ))
  }
  counts <- log |>
    dplyr::count(.data$fallback_used, name = "attempts")
  denom <- sum(counts$attempts)
  counts$share <- as.double(counts$attempts / denom)
  tibble::as_tibble(counts)
}

.adaptive_benchmark_metrics <- function(state) {
  if (!inherits(state, "adaptive_state")) {
    rlang::abort("`state` must be an adaptive_state object.")
  }

  step_log <- adaptive_step_log(state)
  round_log <- adaptive_round_log(state)
  link_mix <- .adaptive_link_mix_metrics(step_log)
  fallback <- .adaptive_fallback_distribution(step_log)
  quota <- .adaptive_stage_quota_summary(step_log)

  top_level <- tibble::tibble(
    metric_group = "run",
    round_id = NA_integer_,
    round_stage = NA_character_,
    metric = c("steps_attempted", "steps_committed", "refit_rows"),
    value = c(
      as.double(nrow(step_log)),
      as.double(sum(!is.na(step_log$pair_id))),
      as.double(nrow(round_log))
    ),
    report_only = TRUE
  )

  link_rows <- if (nrow(link_mix) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    link_mix |>
      dplyr::transmute(
        metric_group = "link_mix",
        round_id = as.integer(.data$round_id),
        round_stage = as.character(.data$round_stage),
        metric = paste0("pair_type:", .data$pair_type, ":proportion"),
        value = as.double(.data$proportion),
        report_only = TRUE
      )
  }

  fallback_rows <- if (nrow(fallback) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    fallback |>
      dplyr::transmute(
        metric_group = "fallback",
        round_id = NA_integer_,
        round_stage = NA_character_,
        metric = paste0("fallback:", .data$fallback_used, ":share"),
        value = as.double(.data$share),
        report_only = TRUE
      )
  }

  quota_rows <- if (nrow(quota) == 0L) {
    tibble::tibble(
      metric_group = character(),
      round_id = integer(),
      round_stage = character(),
      metric = character(),
      value = double(),
      report_only = logical()
    )
  } else {
    quota |>
      dplyr::transmute(
        metric_group = "quota",
        round_id = as.integer(.data$round_id),
        round_stage = as.character(.data$round_stage),
        metric = "stage_shortfall",
        value = as.double(.data$shortfall),
        report_only = TRUE
      )
  }

  dplyr::bind_rows(top_level, link_rows, fallback_rows, quota_rows)
}
