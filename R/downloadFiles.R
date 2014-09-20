#' Donwnload files adopted from Machine Learning Class
#' @param dataURL url of the data to be downloaded
#' @param destF destination file to be saved in csv format.
#' @return returns localy stored file, if files exists it will show message "Data alreade donwloaded".
#' @export


downloadFiles<-function(
        dataURL="", destF="t.csv"
){
        if(!file.exists(destF)){
                download.file(dataURL, destF, method="curl")
        }else{
                message("Data already downloaded.")
        }
}