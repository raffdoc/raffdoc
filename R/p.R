#' Calculate percentage in a numeric vector
#' @param x is a numeric vector
#' @param level is a level of the fator ocharacter variable
#' @param na.rm is remouval of NA values and set defaoul to TRUE
#' @return numeric value expressed in percents
#' @export
p <- function(x, level=1,f = FALSE,  na.rm=TRUE,...){
        suppressPackageStartupMessages(require(mosaic))
        if (na.rm) 
                x <- x[!is.na(x)]
        if(f==FALSE){
                return(paste0(format(mosaic::perc(~x,format = "count", success=level))," ","(",format(mosaic::perc(~x, success=level)),"%",")")) 
        }
        else{
                return(paste0(format(mosaic::perc(~x, success=level)),"%"," ","(",format(mosaic::perc(~x, format = "count", success=level)),")"))
                }
}