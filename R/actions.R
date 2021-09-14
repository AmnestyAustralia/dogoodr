#' DG Actions
#'
#' Returns a feed the latest action log entries for the entire account. It
#' includes the campaign, action, and form data submitted by the supporter.
#'
#' Useful convenience functions for 'since' argument are documented with the
#' [first_moment()] function.
#'
#' @param since time object representing the earliest action to be returned
#' @param campaign_id vector of campaign id integers to be returned
#' @param starting_with should the return list be arranged to start with the
#'   earliest or latest action
#' @param process_pagination if TRUE function will make several requests to
#'   download each page of actions, or should it only return the first 100
#'
#' @return tibble with nested columns for campaign, action, form_data
#' @export
#'
#' @examples
#' minimum_time <- lubridate::now() - lubridate::days(5)
#'
#' dg_actions(
#'   since = minimum_time,
#'   campaign_id = 5618,
#'   starting_with = "latest",
#'   process_pagination = FALSE
#' )
dg_actions <- function(since = this_morning(),
                       campaign_id = NULL,
                       starting_with = c("earliest", "latest"),
                       process_pagination = FALSE) {

  request_params <-
    list(
      since = encode_time_param(since),
      campaign = campaign_id,
      ordering = parse_starting_with(match.arg(starting_with))
    ) %>%
    purrr::compact()

  dg_api(
    endpoint = "action-log-feed",
    out_class = "actionfeed",
    clean_response = TRUE,
    query_param = request_params,
    process_pagination = process_pagination
  )
}

encode_time_param <- function(tm) {
  if (any(c("POSIXct", "POSIXt") %in% class(tm))) {
    return(format(tm, "%Y-%m-%dT%H:%M:%S"))
  } else {
    stop("'since' value must be a time object")
  }
}

parse_starting_with <- function(selected_arg) {
  if (selected_arg == "earliest") {
    return("created")
  } else {
    return("-created")
  }
}

#' Convenience functions dg_actions "since" argument
#'
#' @param day a date value
#' @param n  an integer for the number of days or weeks previous to this moment
#'
#' @return a datetime object
#' @export
#' @importFrom lubridate seconds today days weeks ymd_hms
first_moment <- function(day) {
  day + lubridate::seconds(1)
}
#' @describeIn first_moment returns first moment of the current day
#' @export
this_morning <- function() {
  first_moment(lubridate::today())
}
#' @describeIn first_moment returns first moment n days ago
#' @export
days_ago <- function(n) {
  first_moment(lubridate::today() - lubridate::days(n))
}
#' @describeIn first_moment returns first moment yesterday
#' @export
yesterday <- function() {
  days_ago(1)
}
#' @describeIn first_moment returns first moment n weeks ago
#' @export
weeks_ago <- function(n) {
  first_moment(lubridate::today() - lubridate::weeks(n))
}
#' @describeIn first_moment returns first moment of the year 2000
#' @export
ever <- function() {
  lubridate::ymd_hms("2000-01-01T00:00:01")
}
