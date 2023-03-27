


utils::globalVariables('.SD')

#' @title aggregate data
#' @param df_input \code{data.table} data
#' @param names_vars_by \code{character} col name to group by
#' @param names_vars_num \code{numeric} cols names numerical
#' @export
#' @importFrom data.table copy
agreg_df <- function(df_input, names_vars_by, names_vars_num){
  assertthat::assert_that(inherits(df_input, "data.table"))

  if(!nrow(df_input)>0)
    stop("put data with more than 0 row")

  df_calcul <- copy(df_input)

  # calcul
  df_calcul[, lapply(.SD, sum),
            by= names_vars_by,
            .SDcols= names_vars_num]
}





