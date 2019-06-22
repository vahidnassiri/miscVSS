

#' combines estimated from different vertical splitted sub-samples using appropriate combination rules
#' @param imo.param.est a matrix containing estimated parameters in each sub-sample as its rows
#' @param imo.param.cov a list contaning the covariance matrix estimated within each sub-sample.
#' @return the combined estimate vector and covariance matrix.
#' @author Vahid Nassiri
#' @export
combine.imo <- function(imo.param.est,imo.param.cov){
  # Input variables:
  # imo.param: a matrix containing estimated parameters in each imputation
  # as its rows
  # imo.param.cov: a list contaning the covariance matrix estimated within 
  # each imputation.
  M=length(imo.param.cov)
  if (M>1){
    param.est=apply(imo.param.est,2,mean)
    est.diff=scale(imo.param.est , center = TRUE, scale = FALSE)
    Sample.cov.param1=NULL
    for (i in 1:M){
      Sample.cov.param1[[i]]=(est.diff[i,])%*%t(est.diff[i,])
    }
    sample.cov.param1=Reduce('+',Sample.cov.param1)/ (M-1)
    Factor=(M+1)/M
    sample.cov.param = Factor* sample.cov.param1
    mean.cov.param=Reduce(`+`, imo.param.cov)/M
    param.cov=mean.cov.param- sample.cov.param
  }
  if (M==1){
    param.est=imo.param.est
    param.cov=imo.param.cov
    sample.cov.param1=NULL
    mean.cov.param=NULL
  }
  
  return(list(param.est=param.est,param.cov=param.cov,between.cov=sample.cov.param1,within.cov=mean.cov.param))
}
