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
#' @return a response list
#' @export
#' @importFrom httr modify_url add_headers GET stop_for_status http_type
#' @examples
#' dg_api(
#'   endpoint = "action-log-feed",
#'   out_class = "actionfeed",
#'   query_param = list(campaign_ids = 5618),
#'   clean_response = TRUE,
#'   process_pagination = TRUE
#' )
dg_api <- function(endpoint,
                   out_class,
                   clean_response = FALSE,
                   query_param = list(),
                   process_pagination = FALSE) {

  path <- glue("/api/{endpoint}")
  url <- httr::modify_url(glue("http://{dogooder_subdomain()}.good.do"), path = path)
  resp <- httr::GET(
    url,
    query = query_param,
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
    parsed_response <- parse_response(parsed_response)
  }

  if (clean_response & process_pagination) {
    parsed_response <-
      process_pages(parsed_response,
                    endpoint = endpoint,
                    out_class = out_class)
  }

  parsed_response
}

#' Process remotely paginated dogooder data
#'
#' @param parsed_response previous response to pull the 'next' value from
#' @param max_requests stops early if it exceeds this number
#' @param silent whether to print request details to the console
#' @param ... other parameters for the next request
#' @importFrom dplyr bind_rows
#' @importFrom rlang dots_list exec
#' @return parsed responses binded together
process_pages <- function(parsed_response, max_requests = NULL, silent = FALSE, ...) {

  next_page_url <- attr(parsed_response, "next")
  results_count <- attr(parsed_response, "count")

  if (!is.null(next_page_url)) {
    query_params <- url_args(next_page_url)
    query_params[["since"]] <- stringr::str_replace_all(query_params$since, "%3A", ":")

    if (query_params$page == 2)
      polite_message(
        "Request returned first 100 of {results_count} total records.\nProcessing remaining records with {ceiling(results_count/100)-1} additional requests.\n",
        silent = silent
      )
    query_params.chr <- paste0(names(query_params), "=", query_params, collapse = "&")
    polite_message("Getting /?{query_params.chr}", silent = silent)

    request_arg_list <- c(
      list(query_param = query_params, clean_response = TRUE),
      rlang::dots_list(...)
    )
    parsed_response.next_page <- rlang::exec(dg_api, !!!request_arg_list)

    attr(parsed_response, "next") <- NULL
    attr(parsed_response, "count") <- NULL

    merged_responses <- dplyr::bind_rows(parsed_response, parsed_response.next_page)
    attr(merged_responses, "next") <- attr(parsed_response.next_page, "next")
    attr(merged_responses, "count") <- attr(parsed_response.next_page, "count")

    if (!is.null(max_requests)) {
      if (query_params$page == max_requests) {
        polite_message(
          "Max requests reached, returning data retrieved so far.\n",
          silent = silent
        )
        return(merged_responses)
      }
    } else {
      next_page_arg_list <-
        c(
          list(parsed_response = merged_responses, max_requests = max_requests),
          rlang::dots_list(...)
        )
      rlang::exec(process_pages, !!!next_page_arg_list)
    }
  } else {
    polite_message("Pagination processing completed.\n", silent = silent)
    return(parsed_response)
  }
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
  stack_results(x) %>%
    dplyr::mutate(created = lubridate::ymd_hms(created))
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
#' @importFrom data.table rbindlist
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @return a tibble
stack_lists <- function(l) {
  df.flat <- suppressWarnings(
    data.table::rbindlist(list(flat_values(l)), use.names = TRUE, fill = TRUE)
  )

  df.sub_dfs <-
    purrr::map(
      shallow_listed_values(l),
      ~ list(data.table::rbindlist(list(.x), use.names = TRUE, fill = TRUE))
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
