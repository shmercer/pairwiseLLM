# Adapt historical routing fixtures to the common contract. This constructs
# synthetic fixed shapes only; no legacy Phase B posterior is migrated.
add_test_link_results <- function(state) {
  hub_id <- as.character(state$controller$hub_id %||% 1L)
  identities <- split(state$items, as.character(state$items$set_id))
  identity <- function(key) list(set_id = key,
    items = as.data.frame(identities[[key]][, c("item_id", "global_item_id")]))
  points <- function(key) {
    rows <- state$linking$phase_a$artifacts[[key]]$items
    ids <- rows[["item_id"]]
    if (is.null(ids)) ids <- state$items$item_id[match(rows$global_item_id, state$items$global_item_id)]
    stats::setNames(as.double(rows$theta_raw_mean), as.character(ids))
  }
  results <- lapply(setdiff(names(identities), hub_id), function(key) {
    empty <- data.frame(observation_id = character(), A_set = character(), A_item = character(),
      B_set = character(), B_item = character(), y_A = integer())
    input <- prepare_link_input("fixed_shape_offset", identity(hub_id), identity(key),
      list(hub = list(points = points(hub_id)), spoke = list(points = points(key))), empty,
      list(beta = .2, epsilon = .1, model_variant = "btl_e_b", link = "logit", source = "synthetic"))
    fit_link(input)
  })
  names(results) <- setdiff(names(identities), hub_id)
  state$linking$estimator <- list(accepted_state_by_spoke = results)
  state
}
