#' Unnest actions/campaigns data
#'
#' Used to turn columns containing tables (eg. campaign, action, form_data), each of which may have different dimensions, into wider tables with new columns using names with the format prior_colname.colname_of_nested_table eg. action turns into action.id and action.title
#'
#' @param x table with nested columns
#'
#' @return wider table
#' @export
#'
#' @examples
#'
dg_unnest <- function(x){
  nested_col_lgl <- purrr::map_lgl(x, is.list)
  nested_cols <- names(nested_col_lgl[nested_col_lgl==TRUE])
  contains_values <- purrr::map_lgl(dplyr::select(x, nested_cols), function(nested_col) {
    any(purrr::map_lgl(nested_col, ~!is.null(.x)))
  })
  nested_cols.to_unnest <- nested_cols[contains_values]
  if(length(nested_cols.to_unnest)==0){
    message("No columns appear nested!")
    return(x)
  } else {
    x.unnested <- tidyr::unnest(x, cols=all_of(nested_cols.to_unnest), names_sep=".")
    x.unnested <- tidy_unnested_names(x.unnested)
    return(x.unnested)
  }
}

tidy_unnested_names <- function(x){
  new_names <-
    stringr::str_split(names(x), "\\.") %>%
    purrr::map(~snakecase::to_snake_case(.x)) %>%
    purrr::map_chr(~paste0(.x, collapse="."))
  new_names_incremented <-
    tibble(new_colnames=new_names, i=1) %>%
    dplyr::group_by(new_colnames) %>%
    dplyr::mutate(name_increment = cumsum(i)) %>%
    dplyr::ungroup() %>%
    mutate(name_increment = ifelse(name_increment==1, "", paste0("_", name_increment))) %>%
    mutate(new_colnames = paste0(new_colnames, name_increment)) %>%
    pull(new_colnames)
  names(x) <- new_names_incremented
  return(x)
}
