#' Calculate percentage in a numeric vector
#' @param df is a dataframe object
#' @return numeric value expressed in percents
#' @export

rows_all_missing <- function(df) {
  as.vector(which(rowSums(is.na(df)) == nrow(df)))
}

