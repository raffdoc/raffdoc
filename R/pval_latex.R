#' Thresholded p-value 0.01 for latex implementation
#' @param pval object
#' @return returns pvalue in latex environment.
#' @export 
pval_latex  <-  function(pval) {
        threshold = 0.01
        return(ifelse(pval < threshold, paste("\\textit{p $<$ }", 
                                              sprintf("%.2f", threshold), sep=""),
                      ifelse(pval > 0.1, paste("\\textit{p = }",round(pval, 2), sep=""),
                             paste("\\textit{p = }", round(pval, 2), sep=""))))
}