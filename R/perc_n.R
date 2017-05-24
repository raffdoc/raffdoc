#' Calculate percentage in a numeric vector
#' @param x is a numeric vector
#' @param na.rm is remouval of NA values and set defaoul to TRUE
#' @return numeric value expressed in percents
#' @export
perc_n <- function(x,na.rm=TRUE,ref=1,...){
        require(mosaic)
        if (na.rm) 
                x <- x[!is.na(x)]
        x <- relevel(as.factor(x),ref=ref)
        return(paste0(mosaic::count(~x,format = "count")," ","(",format(mosaic::perc(~x)),"%",")")) 
}