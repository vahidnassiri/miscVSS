# TODO: Add comment
# 
# Author: Vahid Nassiri
###############################################################################

#' takes all sub-samples based on pairwise approach, i.e., 
#' all possible pairs of outcomes. It can be used in any package 
#' to fit the models.
#' @param data could be the outcome of multi.mixed.data.prep or any dataset with the same format
#' @param num.var number of response variables
#' @return the prepared dataset
#' 
#' @author Vahid Nassiri
#' @export
multi.mixed.pairs.prep <- function (data,num.var){
	# This function takes all sub-samples based on
	# pairwise approach, i.e., all possible pairs 
	# of outcomes. It can be used in any package
	# to fit the models.
	# Input variables:
	# data: could be the outcome of multi.mixed.data.prep 
	#       or any dataset with the same format
	# num.var: number of response variables
	pair.id=combn(1:num.var,2)
	num.pairs=dim(pair.id)[2]
	resp.ind=data[,2:(num.var+1)]
	# first pairs
	pair.id.tmp=pair.id[,1]
	data.sub=data[resp.ind[,pair.id.tmp[1]]==1 | resp.ind[,pair.id.tmp[2]]==1,]
	pair.idx=rep(1,dim(data.sub)[1])
	for (i in 2:num.pairs){
		pair.id.tmp=pair.id[,i]
		data.sub.tmp=data[resp.ind[,pair.id.tmp[1]]==1 | resp.ind[,pair.id.tmp[2]]==1,]
		data.sub=rbind(data.sub,data.sub.tmp)
		pair.idx.tmp=rep(i,dim(data.sub.tmp)[1])
		pair.idx=c(pair.idx,pair.idx.tmp)
	}
	paired.data=cbind(data.sub,pair.idx)
	return(paired.data)
}

