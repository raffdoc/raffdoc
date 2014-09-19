#' histogram wiht base plotting system with normal curve 
#' addopted from http://www.statmethods.net/graphs/density.html
#' @param x numeric vector wiht values to plot
#' @param breaks count of breaks in the histogram.
#' @return histogram wiht base plotting sistem
#' @export
hist_wnc <- function(x, breaks = 24) {
        h <- hist(x, breaks = breaks, col = "lightblue",border = "white")
        xfit <- seq(min(x), max(x), length = 40)
        yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
        yfit <- yfit * diff(h$mids[1:2]) * length(x)
        lines(xfit, yfit, lwd = 2,col= "red")
}