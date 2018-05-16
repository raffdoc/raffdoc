#' Estimating the mean and standard deviation from the median and the range
#' From statmd blog 
#' @param a min of the data
#' @param b max of the data
#' @param m median of the data
#' @param n sample of size 
#' @export



msmr<-function(a,m,b,n){
        mn<-(a+2*m+b)/4+(a-2*m+b)/(4*n)
        s=sqrt((a*a+m*m+b*b+(n-3)*((a+m)^2+(m+b)^2)/8-n*mn*mn)/(n-1))
        c(mn,s)
}