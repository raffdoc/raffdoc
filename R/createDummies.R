#' Create dummy variables for each level of a categorical variable
#' @param x is a character vector with the name or names of the columnes in dataframe to convert to dumy values
#' @param df is a data.frame with new dummy variables.
#' @param keepNAs is logical vector for keeping or removing NA values.
#' @return returns data.frame wiht dummy variables.
#' @export
createDummies <- function(x, df, keepNAs = TRUE) {
        for (i in seq(1, length(unique(df[, x])))) {
                if(keepNAs) {
                        df[, paste(x,".", i, sep = "")] <- ifelse(df[, x] != i, 0, 1)
                } else {
                        df[, paste(x,".", i, sep = "")] <- ifelse(df[, x] != i | is.na(df[, x]) , 0, 1)     
                }
        }
        df
}
