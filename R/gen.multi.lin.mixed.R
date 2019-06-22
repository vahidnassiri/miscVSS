# TODO: Add comment
# 
# Author: Vahid Nassiri
###############################################################################


#' generates data from a random intercept linear mixed models 
#' with only intercepts: \eqn{y_{rij} = \beta_r + b_{ri} + \epsilon_{rij}}
#' @param sigma2 error variance
#' @param D random intercepts covariance matrix
#' @param mu fixed effects values, the same length as num.var
#' @param num.clust sample size 
#' @param clust.size cluster size 
#' @param num.var number of response variables
#' @param seed a seed
#' @return prepared dataset
#' 
#' @author Vahid Nassiri
#' @export
gen.multi.lin.mixed <- function(sigma2, D, mu, num.clust, clust.size, num.var, seed=123){
	# This function 
	# input of the function: 
	#sigma2: error variance
	#D: random intercepts covariance matrix
	#mu: fixed effects values, the same length as num.var
	#num.clust: sample size 
	#clust.size: cluster size 
	#num.var: number of response variables
	################################################################### 
	# Constructing the within and between cluster variance matrices
	# and computing their Cholesky decomposition
	s.within=sigma2*diag(clust.size) 
	s.between=D
	Chol_dec.w=t(chol(s.within))
	Chol_dec.b=t(chol(s.between))
	# Generating a correlated Gaussian random vector
	b1=rnorm(num.var)
	b.between=Chol_dec.b%*%b1
	b.add=matrix(rep(b.between,clust.size),clust.size,num.var,byrow = TRUE)
	x.within0=matrix(0,clust.size,num.var)
	for (j in 1:num.var){
		x1=rnorm(clust.size)
		x.within0[,j]=(Chol_dec.w%*%x1) 
	}
	# Adding the mean to the generated random vector
	mu.add=matrix(rep(mu,clust.size),clust.size,num.var,byrow=TRUE)
	data.final1=(x.within0+b.add)+mu.add
	# Preparing extra variables indicating time variable and subject index
	repeated_id=1:clust.size
	cluster_id=rep(1,clust.size)
	data.final=cbind(data.final1,repeated_id,cluster_id)
	# Now having first subject generated, it will be repeapted to generate
	# num.clust samples.
	for (i in 2:num.clust){
		b1=rnorm(num.var)
		b.between=Chol_dec.b%*%b1
		b.add=matrix(rep(b.between,clust.size),,num.var,byrow = TRUE)
		x.within0=matrix(0,clust.size,num.var)
		for (j in 1:num.var){
			x1=rnorm(clust.size)
			x.within0[,j]=(Chol_dec.w%*%x1) 
		}
		subj.final0=(x.within0+b.add)+mu.add
		repeated_id=1:clust.size
		cluster_id=rep(i,clust.size)
		subj.final=cbind(subj.final0,repeated_id,cluster_id)
		data.final=rbind(data.final,subj.final)
	}
	return(data.final)
}
