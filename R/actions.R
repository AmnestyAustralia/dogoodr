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
#' @param process_pagination should additional pages of data be retrieved?
#' @param page_size the number of records to retrieve in each request
#' @param max_requests the maximum number of requests to perform when retrieving
#'   additional pages of data
#'
#' @return tibble with nested columns for campaign, action, form_data
#' @export
#'
#' @examples
#' minimum_time <- lubridate::now() - lubridate::days(5)
#'
#' \dontrun{
#' dg_actions(
#'   since = minimum_time,
#'   campaign_id = 5618,
#'   starting_with = "latest",
#'   process_pagination = FALSE
#' )
#' }
dg_actions <- function(since = this_morning(),
                       campaign_id = NULL,
                       starting_with = c("earliest", "latest"),
                       process_pagination = FALSE,
                       page_size = 1000,
                       max_requests = Inf) {

  request_params <-
    purrr::compact(
      list(
        since = encode_time_param(since),
        campaign = campaign_id,
        ordering = parse_starting_with(match.arg(starting_with))
      )
    )

  dg_api(
    endpoint = "action-log-feed",
    out_class = "actionfeed",
    clean_response = TRUE,
    query_param = request_params,
    process_pagination = process_pagination,
    page_size = page_size,
    max_requests = max_requests
  )
}

encode_time_param <- function(tm,
                              arg = rlang::caller_arg(tm),
                              call = rlang::caller_env()) {
  if (any(c("POSIXct", "POSIXt") %in% class(tm))) {
    return(format(tm, "%Y-%m-%dT%H:%M:%S"))
  } else if (is.null(tm)) {
    return(NULL)
  } else {
    cli_abort(
      "{.arg {arg}} must be a time object, not {.cls {class(tm)}}.",
      call = call
    )
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
