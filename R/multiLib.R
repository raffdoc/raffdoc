#' transform code below into the multiLibrary function 
#' @param libraries character vector with library name or names to load.
#' @return lodes libraries into curnt session of R
#' @export
multiLibrary <- function (libraries) {
        lapply(libraries, library, character.only=T)
}