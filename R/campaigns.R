#' DG Campaigns
#'
#' Returns information about all campaigns in an account. Information includes
#' title, description, urls, actions, etc.
#'
#' @return a tibble of the org's Do Gooder campaigns
#' @export
dg_campaigns <- function() {
  dg_api(endpoint = "campaigns", out_class = "campaigns", clean_response = TRUE)
}
