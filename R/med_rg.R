#' median and range for latex documents.
#' @param x numeric vector for mean and range calculation
#' @return returns mean and range (0.25 percentile - 0.75 percentile) of the vector for latex envirment.
#' @export 
m.rg <- function (x, na.rm = TRUE) {
        if (na.rm) 
                x <- x[!is.na(x)]
        #n <- length(x)
        #if (n == 0) 
        #return(c(median = NA, range = NA);
        #median(x);range(x)
        return(paste0(format(median(x))," ","(","range, ",format(min(x))," ","$-$ ",format(max(x)),")"))
}