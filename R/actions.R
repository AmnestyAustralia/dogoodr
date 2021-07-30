#' DG Actions
#'
#' Returns a feed the latest action log entries for the entire account. It includes the campaign, action, and form data submitted by the supporter.
#'
#' @param since time object representing the earliest action to be returned
#' @param campaign_id vector of campaign id integers to be returned
#' @param starting_with should the return list be arranged to start with the earliest or latest action
#' @param process_pagination if TRUE function will make several requests to download each page of actions, or should it only return the first 100
#'
#' @return tibble with nested columns for campaign, action, form_data
#' @export
#'
#' @examples
#' minimum_time <- lubridate::now() - lubridate::days(5)
#' dg_actions(since=minimum_time, campaign_id=5618, starting_with="latest", process_pagination=FALSE)
dg_actions <- function(since=this_morning(), campaign_id=NULL, starting_with=c("earliest", "latest"), process_pagination=FALSE){
  request_params <-
    list(since = encode_time_param(since),
         campaign = campaign_id,
         ordering = parse_starting_with(match.arg(starting_with))) %>%
    purrr::compact()
  dg_api(endpoint="action-log-feed",
         out_class="actionfeed",
         clean_response=TRUE,
         query_param = request_params,
         process_pagination=process_pagination)
}

encode_time_param <- function(tm){
  if(any(c("POSIXct", "POSIXt") %in% class(tm))){
    return(format(tm, "%Y-%m-%dT%H:%M:%S"))
  } else {
    stop("'since' value must be a time object")
  }
}

parse_starting_with <- function(selected_arg){
  if(selected_arg == "earliest"){
    return("created")
  } else {
    return("-created")
  }
}

first_moment <- function(day) day + lubridate::seconds(1)
this_morning <- function() first_moment(lubridate::today())
days_ago <- function(n) first_moment(lubridate::today()-lubridate::days(n))
yesterday <- function() days_ago(1)
weeks_ago <- function(n) first_moment(lubridate::today()-lubridate::weeks(n))
