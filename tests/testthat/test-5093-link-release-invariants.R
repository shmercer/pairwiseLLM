test_that("the release matrix reconciles invariance, repeatability and ordered provenance", {
  for (id in link_release_ids) {
    args <- link_release_args(id)
    input <- do.call(prepare_link_input, args)
    fit <- fit_link(input)
    expect_true(fit$diagnostics$fit_valid, info = id)
    again <- fit_link(input)
    expect_identical(again$items, fit$items)
    expect_identical(again$offset, fit$offset)
    expect_identical(again$prediction, fit$prediction)
    expect_identical(fit$provenance$hashes, input$hashes)
    expect_identical(fit$provenance$counts, input$counts)
    expect_identical(input$counts$cross, nrow(args$cross))
    expect_identical(input$counts$phase_a_hub,
      if (id == "joint_offset") nrow(args$phase_a$hub$observations) else 0L)

    reordered <- args
    for (k in c("hub", "spoke")) reordered[[k]]$items <- args[[k]]$items[3:1, , drop = FALSE]
    expect_identical(do.call(prepare_link_input, reordered), input)
    reordered$cross <- args$cross[rev(seq_len(nrow(args$cross))), ]
    if (id == "joint_offset") for (k in c("hub", "spoke")) {
      x <- args$phase_a[[k]]$observations
      reordered$phase_a[[k]]$observations <- x[rev(seq_len(nrow(x))), ]
    }
    permuted <- do.call(prepare_link_input, reordered)
    expect_false(identical(permuted$hashes$cross, input$hashes$cross))
    expect_equal(fit_link(permuted)$items, fit$items, tolerance = 1e-6)
    expect_error(fit_link(permuted, previous = fit), class = "pairwiseLLM_link_contract_error")

    renamed <- args
    ids <- c(a = "z", b = "x", c = "y")
    rename_rows <- function(x) {
      x$A_item <- unname(ids[x$A_item])
      x$B_item <- unname(ids[x$B_item])
      x
    }
    renamed$cross <- rename_rows(args$cross)
    for (k in c("hub", "spoke")) {
      renamed[[k]]$items$item_id <- unname(ids[args[[k]]$items$item_id])
      x <- args$phase_a[[k]]
      if (!is.null(x$observations)) x$observations <- rename_rows(x$observations)
      if (!is.null(x$points)) names(x$points) <- unname(ids[names(x$points)])
      if (!is.null(x$draws)) colnames(x$draws) <- unname(ids[colnames(x$draws)])
      renamed$phase_a[[k]] <- x
    }
    relabeled <- fit_link(do.call(prepare_link_input, renamed))
    expect_equal(relabeled$items$theta_link_mean[c(3, 1, 2, 6, 4, 5)],
      fit$items$theta_link_mean, tolerance = 1e-6)
    expect_equal(relabeled$offset, fit$offset, tolerance = 1e-6)
    for (presentation in c(FALSE, TRUE)) {
      reflected <- fit_link(do.call(prepare_link_input,
        link_release_reflect(args, presentation)))
      sign <- if (presentation) 1 else -1
      expect_equal(reflected$items$theta_link_mean, sign * fit$items$theta_link_mean, tolerance = 1e-6)
      expect_equal(reflected$offset$delta_mean, sign * fit$offset$delta_mean, tolerance = 1e-6)
      expect_equal(reflected$items$theta_link_sd, fit$items$theta_link_sd, tolerance = 1e-6)
    }
  }
})

test_that("all estimators retain repeated judgments and reject reused identities", {
  for (id in link_release_ids) {
    args <- link_release_args(id)
    base <- fit_link(do.call(prepare_link_input, args))
    repeats <- args$cross
    repeats$observation_id <- paste0("repeat-", repeats$observation_id)
    args$cross <- rbind(args$cross, repeats)
    input <- do.call(prepare_link_input, args)
    repeated <- fit_link(input)
    expect_true(repeated$diagnostics$fit_valid)
    expect_identical(input$counts$cross, 2L * base$provenance$counts$cross)
    expect_false(isTRUE(all.equal(repeated$offset$delta_sd, base$offset$delta_sd)))
    args$cross$observation_id[nrow(args$cross)] <- args$cross$observation_id[1]
    expect_error(do.call(prepare_link_input, args), "unique")
  }
})

test_that("held-out final and probe outcomes cannot affect an explicit fit", {
  for (id in link_release_ids) {
    args <- link_release_args(id)
    # Source partitions are external: only the active table is passed to fitting.
    source <- list(active = args$cross[1:6, ], probe = args$cross[7:9, ], final = args$cross[10:12, ])
    args$cross <- source$active
    input <- do.call(prepare_link_input, args)
    base <- fit_link(input)
    source$probe$y_A <- 1L - source$probe$y_A
    source$final$y_A <- 1L - source$final$y_A
    expect_identical(do.call(prepare_link_input, args), input)
    expect_identical(fit_link(input)$items, base$items)
    expect_identical(input$cross$observation_id, source$active$observation_id)
    expect_false(any(c(source$probe$observation_id, source$final$observation_id) %in% input$cross$observation_id))
    expect_error(predict_link(base, source$final), "fields")
    expect_length(predict_link(base, source$final[, -6]), 3L)
    args$cross$partition <- "active"
    expect_error(do.call(prepare_link_input, args), "fields")
  }
})
