#' thresholded p-value for ploting purposes
#' @param pval object 
#' @return returns pvalue in base ploting system environment.
#' @export 
pval_plot  <-  function(pval) {
        threshold = 0.01
        return(ifelse(pval < threshold, paste("p < ", sprintf("%.2f", threshold), sep=""),
                      ifelse(pval > 0.1, paste("p = ",round(pval, 2), sep=""),
                             paste("p = ", round(pval, 2), sep=""))))
}