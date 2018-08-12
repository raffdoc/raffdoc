#' Calculate percentage in a numeric vector
#' @param df is a dataframe object
#' @return numeric value expressed in percents
#' @export

cols_all_missing <- function(df) {
        as.vector(which(colSums(is.na(df)) == nrow(df)))
}