dg_api <- function(endpoint, out_class, clean_response = FALSE, query_param = list(), process_pagination=FALSE, ...){
  path <- glue("/api/{endpoint}")
  url <- httr::modify_url(glue("http://{dogooder_subdomain()}.good.do"), path = path)
  resp <- httr::GET(url, query = query_param, httr::add_headers(Authorization = dogooder_token()))
  # Check HTTP Status
  httr::stop_for_status(resp)
  # Check response is in JSON
  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  parsed_response <- response_parse(response = resp, out_class = out_class)
  if(!is.null(parsed_response$error)){
    stop(glue("The API returned an error: {parsed_response$error$code} - {parsed_response$error$info}"))
  }
  if(clean_response){
    parsed_response <- parse_response(parsed_response)
  }
  if(clean_response & process_pagination){
    parsed_response <- process_pages(parsed_response, endpoint=endpoint, out_class=out_class)
  }
  return(parsed_response)
}

process_pages <- function(parsed_response, max_requests=NULL, silent=FALSE, ...){
  next_page_url <- attr(parsed_response, "next")
  results_count <- attr(parsed_response, "count")
  if(!is.null(next_page_url)){
    query_params <- url_args(next_page_url)
    if(query_params$page==2){
      polite_message("Request returned first 100 of {results_count} total records.\nProcessing remaining records with {ceiling(results_count/100)-1} additional requests.\n", silent=silent)
    }
    query_params.chr <- paste0(names(query_params), '=', query_params, collapse="&")
    polite_message("Getting /?{query_params.chr}", silent=silent)
    request_arg_list <- c(list(query_param = query_params, clean_response=TRUE), rlang::dots_list(...))
    parsed_response.next_page <-rlang::exec(dg_api, !!!request_arg_list)
    attr(parsed_response, "next") <- NULL
    attr(parsed_response, "count") <- NULL
    merged_responses <- dplyr::bind_rows(parsed_response, parsed_response.next_page)
    attr(merged_responses, "next") <- attr(parsed_response.next_page, "next")
    attr(merged_responses, "count") <- attr(parsed_response.next_page, "count")
    if(!is.null(max_requests)){
      if(query_params$page==max_requests){
        polite_message("Max requests reached, returning data retrieved so far.\n", silent=silent)
        return(merged_responses)
      }
    } else {
      next_page_arg_list <- c(list(parsed_response = merged_responses, max_requests=max_requests), rlang::dots_list(...))
      rlang::exec(process_pages, !!!next_page_arg_list)
    }
  } else {
    polite_message("Pagination processing completed.\n", silent=silent)
    return(parsed_response)
  }
}

url_args <- function(url){
  arg.chrs <-
    stringr::str_extract(url, "(?<=/\\?).*?$") %>%
    stringr::str_split("&") %>%
    magrittr::extract2(1) %>%
    stringr::str_split("=")
  arg.list <-
    purrr::map(arg.chrs, ~.x[[2]]) %>%
    rlang::set_names(nm=purrr::map_chr(arg.chrs, ~.x[[1]]))
  arg.list
}

response_parse <- function(response, out_class){
  response_text <- httr::content(x=response, as="text", encoding="UTF-8")
  parsed_text <- jsonlite::fromJSON(txt=response_text, simplifyVector = FALSE)
  class(parsed_text) <- out_class
  return(parsed_text)
}

parse_response <- function(x) UseMethod("parse_response", x)

parse_response.actionfeed <- function(x){
  stack_results(x) %>%
    dplyr::mutate(created = lubridate::ymd_hms(created))
}

parse_response.campaigns <- function(x){
  stack_results(x)
}

# Returns list items within a level that aren't lists
flat_values <- function(x) x[purrr::map_int(x, purrr::vec_depth) < 2]
shallow_listed_values <- function(x) x[purrr::map_int(x, purrr::vec_depth) == 2]
deeply_listed_values <- function(x) x[purrr::map_int(x, purrr::vec_depth) > 2]

stack_lists <- function(l){
  df.flat <- suppressWarnings(data.table::rbindlist(list(flat_values(l)), use.names=TRUE, fill=TRUE))
  df.sub_dfs <- purrr::map(shallow_listed_values(l), ~list(data.table::rbindlist(list(.x), use.names=TRUE, fill=TRUE)))
  tibble::as_tibble(c(df.flat, df.sub_dfs))
}

stack_results <- function(x){
  x.results <- x$results
  x.meta <- flat_values(x) # count, next, previous stored on top level as "flat" values
  x.df <- purrr::map_dfr(x.results, stack_lists)
  x.df.w_meta <- rlang::exec(rlang::set_attrs, !!!c(list(.x=x.df), x.meta))
  x.df.w_meta
}
