#' DG Campaigns
#'
#' Returns information about all campaigns in an account. Information includes title, description, urls, actions, etc.
#'
#' @return
#' @export
#'
#' @examples
dg_campaigns <- function(){
  dg_api(endpoint="campaigns", out_class="campaigns", clean_response=TRUE)
}
