#' Make a DG API Request
#'
#' Lower level function used by functions with dg_ prefix.
#'
#' @param endpoint name of endpoint
#' @param out_class class to use for the parse_response request
#' @param clean_response whether to run parse_response on the request
#' @param query_param URL parameters to send as part of the request
#' @param process_pagination if there are multiple response pages, should
#'   whether to send multiple requests
#' @param max_requests stops early if it exceeds this number
#' @param silent whether to print request details to the console
#' @return a response list
#' @export
#' @examples
#' \dontrun{
#' dg_api(
#'   endpoint = "action-log-feed",
#'   out_class = "actionfeed",
#'   query_param = list(campaign_ids = 5618),
#'   clean_response = TRUE,
#'   process_pagination = TRUE
#' )
#' }
dg_api <- function(endpoint,
                   out_class,
                   clean_response = FALSE,
                   query_param = list(),
                   process_pagination = TRUE,
                   max_requests = Inf,
                   silent = FALSE) {

  path <- paste0("/api/", endpoint)
  url <- httr::modify_url(
    paste0("http://", dogooder_subdomain(), ".good.do"),
    path = path,
    query = query_param
  )

  out <- list()
  page <- 1

  cli::cli_progress_step("Performing initial dogoodr request...")

  while(TRUE) {
    resp <- httr::GET(
      url,
      httr::add_headers(Authorization = dogooder_token())
    )

    # Check HTTP Status
    httr::stop_for_status(resp)

    # Check response is in JSON
    if (httr::http_type(resp) != "application/json") {
      cli_abort("Do Gooder API did not return json.")
    }

    parsed_response <- response_parse(response = resp, out_class = out_class)
    if (!is.null(parsed_response$error)) {
      cli_abort(
        c(
          "Do Gooder API error {.val {parsed_response$error$code}}.",
          "x" = "{parsed_response$error$info}"
        )
      )
    }

    if (clean_response) {
      out <- dplyr::bind_rows(out, parse_response(parsed_response))
    } else {
      out <- c(out, list(parsed_response))
    }
    if (!is.null(parsed_response$`next`)) {
      if (process_pagination) {
        if (page == 1) {
          cli::cli_progress_done()
          total_requests <- ceiling(parsed_response$count / 100)
          cli::cli_alert_info("Initial request returned 100 of {parsed_response$count} total records.")
          if (total_requests > max_requests) {
            cli::cli_alert_warning (
              "Requesting all pages would exceed {.arg max_requests}. {max_requests} of {total_requests} page{?s} will be retrieved."
            )
            total_requests <- max_requests
          }
          # cli::cli_alert_info("Performing {total_requests - 1} additional request{?s}.")
          # cli::cli_progress_step(
          #   paste(
          #     "Request {page + 1} / {total_requests}, records {((100 * page) + 1)}-{min(100 * (page + 1), parsed_response$count)} / {parsed_response$count}"
          #   ),
          #   msg_done = "Request returned {parsed_response$count} total records."
          # )
          cli::cli_progress_bar(
            "Requesting additional pages",
            total = total_requests - 1,
            type = "tasks",
            format_done = "{.alert-success {cli::pb_name}| {cli::pb_current}/{cli::pb_total} {.timestamp {cli::pb_elapsed}}}",
            format_failed = "{.alert-danger {cli::pb_name}| {cli::pb_current}/{cli::pb_total} {.timestamp {cli::pb_elapsed}}}",
            clear = FALSE
          )
        } else {
          cli::cli_progress_update()
        }

        if (page >= max_requests) {
          cli::cli_progress_done()
          cli::cli_alert_success("Retrieved {nrow(out)} record{?s}.")
          break()
        }

        page <- page + 1
        url <- curl::curl_unescape(parsed_response$`next`)
      } else {
        cli::cli_alert_info("Initial request returned 100 of {parsed_response$count} total records.")
        cli::cli_alert_warning(
          "{.arg process_pagination} is set to {.code FALSE}, remaining records will not be retrieved."
        )
        cli::cli_alert_success("Retrieved {nrow(out)} record{?s}.")

        break
      }
    } else {
      cli::cli_progress_done()
      cli::cli_alert_success("Retrieved {nrow(out)} record{?s}.")
      break
    }
  }

  out
}

#' Parse http response json to a list
#'
#' @param response http response in json format
#' @param out_class class to assign the response list
#' @return response list
response_parse <- function(response, out_class) {
  response_text <- httr::content(x = response, as = "text", encoding = "UTF-8")
  parsed_text <- jsonlite::fromJSON(txt = response_text, simplifyVector = FALSE)
  class(parsed_text) <- out_class
  parsed_text
}

#' Parse a response list into a rectangle
#'
#' @param x a response list
#' @return a tibble based off the out_class of a response list
parse_response <- function(x) {
  UseMethod("parse_response", x)
}

#' Parse response list from actionfeed
#'
#' @param x response list
#' @return tibble
parse_response.actionfeed <- function(x) {
  out <- stack_results(x)
  out$created <- lubridate::ymd_hms(out$created)
  out
}

parse_response.campaigns <- function(x) {
  stack_results(x)
}

#' Flat list values
#'
#' Returns list items within a level that aren't lists
#'
#' @param x a list
#' @return list items without sublists
flat_values <- function(x) {
  x[purrr::map_int(x, purrr::vec_depth) < 2]
}

shallow_listed_values <- function(x) x[purrr::map_int(x, purrr::vec_depth) == 2]
deeply_listed_values <- function(x) x[purrr::map_int(x, purrr::vec_depth) > 2]

#' Stacks several lists into tibble rows
#'
#' Requires a list of lists as an input. Tries to turn sublists into lists as
#' well.
#'
#' @param l a list where each item is a list, which will become a row
#' @return a tibble
stack_lists <- function(l) {
  df.flat <- suppressWarnings(
   dplyr::bind_rows(list(flat_values(l)))
  )

  df.sub_dfs <-
    purrr::map(
      shallow_listed_values(l),
      ~ list(dplyr::bind_rows(list(.x)))
    )

  tibble::as_tibble(c(df.flat, df.sub_dfs))
}

#' Adds Do Gooder metadata to a response rectangle
#'
#' Meta includes count, next, previous, which are provided as top level "flat"
#' values in the response json.
#'
#' @param x response list
#' @return rectangle with count, next, previous values stored as attributes
stack_results <- function(x) {
  x.results <- x$results
  x.meta <- flat_values(x)
  x.df <- purrr::map_dfr(x.results, stack_lists)
  x.df.w_meta <- rlang::exec(structure, !!!c(list(.Data = x.df), x.meta))
  x.df.w_meta
}
