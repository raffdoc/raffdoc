#' mean and standard deviation for latex documents.
#' @param x numeric vector for mean and range calculation
#' @param na.rm logical value for removing or not the NA values.
#' @return returns mean and standard deviation for latex envirment.
#' @export 
m.sd <- function (x, na.rm = TRUE) {
        if (na.rm) 
                x <- x[!is.na(x)]
        n <- length(x)
        if (n == 0) 
                return(c(mean = NA, sd = NA))
        xbar <- sum(x)/n
        sd <- sqrt(sum((x - xbar)^2)/(n - 1))
        c(mean = xbar, sem = sd)
        return(paste(formatC(xbar,format = "f" ,digits = 1),"$\\pm$",formatC(sd, format = "f" ,digits = 1)))
}
