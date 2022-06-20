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
#' @importFrom httr modify_url add_headers GET stop_for_status http_type
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

  polite_message(
    "Performing dogoodr request...",
    silent = silent
  )

  while(TRUE) {
    resp <- httr::GET(
      url,
      httr::add_headers(Authorization = dogooder_token())
    )

    # Check HTTP Status
    httr::stop_for_status(resp)

    # Check response is in JSON
    if (httr::http_type(resp) != "application/json") {
      stop("API did not return json", call. = FALSE)
    }

    parsed_response <- response_parse(response = resp, out_class = out_class)
    if (!is.null(parsed_response$error)) {
      stop(
        "The API returned an error: ",
        parsed_response$error$code, " - ", parsed_response$error$info,
        call. = FALSE
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
          polite_message(
            "Request returned first 100 of {parsed_response$count} total records. Performing {ceiling(parsed_response$count / 100) - 1} additional requests.",
            silent = silent
          )
        }

        if (page >= max_requests) {
          polite_message(
            "Reached {max_requests} max requests. Returning {nrow(out)} records retrieved so far.",
            silent = silent
          )
          break()
        }

        polite_message(
          "{as.character(Sys.time())}: request {page + 1} / {ceiling(parsed_response$count / 100)}, records {((100 * page) + 1)}-{min(100 * (page + 1), parsed_response$count)} / {parsed_response$count}",
          silent = silent
        )

        page <- page + 1
        url <- curl::curl_unescape(parsed_response$`next`)
      } else {
        warning(
          "Request returned first 100 of ", parsed_response$count, " total records.\n",
          "`process_pagination` is set to `FALSE`, so remaining records will not be loaded.",
          call. = FALSE
        )
        break
      }
    } else {
      polite_message(
        "Request returned {parsed_response$count} total records.",
        silent = silent
      )
      break
    }
  }

  out
}

#' Extract a URLs arguments for new requests
#'
#' @param url character item with arguments embedded in it
#' @return a list of arguments
url_args <- function(url) {
  arg.chrs <-
    stringr::str_extract(url, "(?<=/\\?).*?$") %>%
    stringr::str_split("&") %>%
    magrittr::extract2(1) %>%
    stringr::str_split("=")

  arg.list <-
    purrr::map(arg.chrs, ~ .x[[2]]) %>%
    rlang::set_names(nm = purrr::map_chr(arg.chrs, ~ .x[[1]]))

  arg.list
}

#' Parse http response json to a list
#'
#' @param response http response in json format
#' @param out_class class to assign the response list
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
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
#' @importFrom dplyr mutate
#' @importFrom lubridate ymd_hms
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
#' @importFrom purrr vec_depth
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
#' @importFrom purrr map
#' @importFrom tibble as_tibble
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

#' Adds dogooder metadata to a response rectangle
#'
#' Meta includes count, next, previous, which are provided as top level "flat"
#' values in the response json.
#'
#' @param x response list
#' @return rectangle with count, next, previous values stored as attributes
#' @importFrom purrr map_dfr
#' @importFrom rlang exec
stack_results <- function(x) {
  x.results <- x$results
  x.meta <- flat_values(x)
  x.df <- purrr::map_dfr(x.results, stack_lists)
  x.df.w_meta <- rlang::exec(structure, !!!c(list(.Data = x.df), x.meta))
  x.df.w_meta
}
