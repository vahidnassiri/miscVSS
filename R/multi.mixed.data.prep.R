# TODO: Add comment
# 
# Author: Vahid Nassiri
###############################################################################


#' prepares a multivariate dataset
#' in a format suitable for joint modelling approach.
#' @param data could be the outcome of gen.multi.lin.mixed
#'       it should have the responses as columns, followed by
#'     repeated measurements id and cluster id.
#' @param num.var number of responses
#' @return the prepared dataset 
#' 
#' @author Vahid Nassiri
#' @export
multi.mixed.data.prep <- function(data,num.var){
	# This function 
	# Input:  
	# data: which could be the outcome of gen.multi.lin.mixed
	#       it should have the responses as columns, followed by
	#       repeated measurements id and cluster id.
	# num.var: number of responses
	repeated_id=data[,num.var+1]
	cluster_id=data[,num.var+2]
	resp=data[,1:num.var]
	total.length=dim(resp)[1]*dim(resp)[2]
	resp.ind=matrix(0,total.length,num.var)
	resp.idx=matrix(1:total.length,dim(resp)[1],num.var)
	resp.all=c(resp)
	resp.ind[resp.idx[,1],1]=1
	id_repeated= rep(repeated_id,num.var)
	id_cluster= rep(cluster_id,num.var)
	outcome_ind=rep(1,dim(resp)[1])
	nam.resp=rep(0,num.var)
	nam.resp[1]=paste('y',1,sep='')
	for (i in 2:num.var){
		resp.ind[resp.idx[,i],i]=1
		outcome_ind=c(outcome_ind, rep(i,dim(resp)[1]))
		nam.resp[i]=paste('y',i,sep='')
	}
	colnames(resp.ind)=nam.resp
	final.data=cbind(resp.all,resp.ind,id_cluster,id_repeated,outcome_ind)
	return(final.data)
}
