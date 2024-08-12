#' progresively Censored Data
#' @export
#' @name pcdata
#' @param m vector of quantiles.
#' @param r vector of censoring scheme.
#' @param qdistr quantile function for specifided distribution.
#' @param par vector of  distribution parameters.
#' @description
#' random generations under progressive censoring.
#' @return \code{pcdata} gives the progressive censored data.
#' @details
#' The details will be given.
#' @references  Balakrishnan, N., & Aggarwala, R. (2000).
#' *Progressive censoring: theory, methods, and applications*
#' Springer Science & Business Media.
#' @examples
#' pcdata(10,rep(1,10),"qweibull",c(2,3))
pcdata<-function(m,r,qdistr="qunif",par=c(0,1))
{
  if(any(m!=length(r))) {stop("dimension of r must be equal to m")}
  x<-NULL
  y<-NULL
  n<-m+sum(r)
  x[1]<-stats::rexp(1)/n
  for(i1 in 2:m){
    x[i1]<-x[i1-1]+stats::rexp(1)/(n-sum(r[1:(i1-1)])-i1+1)
  }
  u<-1-exp(-x)
  f<-paste0("y<-",qdistr,"(u,par)")
  eval(parse(text = f))
  return(y)
}
