#' Unnest actions/campaigns data
#'
#' Used to turn columns containing tables (eg. campaign, action, form_data),
#' each of which may have different dimensions, into wider tables with new
#' columns using names with the format prior_colname.colname_of_nested_table eg.
#' action turns into action.id and action.title
#'
#' @param x table with nested columns
#'
#' @return wider table
#' @export
dg_unnest <- function(x) {
  nested_col_lgl <- purrr::map_lgl(x, is.list)
  nested_cols <- names(nested_col_lgl[nested_col_lgl == TRUE])

  contains_values <- purrr::map_lgl(
    dplyr::select(x, nested_cols),
    function(nested_col) {
      any(purrr::map_lgl(nested_col, ~ !is.null(.x)))
    }
  )
  nested_cols.to_unnest <- nested_cols[contains_values]

  if (length(nested_cols.to_unnest) == 0) {
    message("No columns appear nested!")
    return(x)
  } else {
    x.unnested <- tidyr::unnest(x, cols = dplyr::all_of(nested_cols.to_unnest), names_sep = ".")
    return(x.unnested)
  }
}
