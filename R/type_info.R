#' median and range for latex documents.
#' @param x object in wich we want to know the properties
#' @return returns list of propertes in the object
#' @export 
type_info <- function(x){
        c(
                class = class(x),
                typeof = typeof(x),
                mode = mode(x),
                storage.mode = storage.mode(x)
        )
}