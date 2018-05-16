#' confidence intervals for latex documents.
#' @param x numeric vector for mean and range calculation
#' @param na.rm logical value for removing or not the NA values.
#' @return returns mean and standard deviation for latex envirment.
#' @export 
m.ci <- function (x, na.rm = TRUE) {
        if (na.rm) 
                x <- x[!is.na(x)]
        n <- length(x)
        if (n == 0) 
                return(c(median = NA, CI = NA))
        require(stats)
        return(paste0(formatC(median(x), format = "f" ,digits = 1)," ","(","95% CI: ",formatC(quantile(x, probs = 0.025),format = "f" ,digits = 1),", ",formatC(quantile(x, probs = 0.975), format = "f" ,digits = 1),")"))
}
