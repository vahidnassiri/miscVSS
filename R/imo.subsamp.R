
#' take random sub-samples from each cluster using random vertical splitting with acceleration
#' @param data the input dataset
#' @param num.outp number of sub-samples
#' @param sub.size size of each sub-sample
#' @return the sub-sampled dataset
#' @author Vahid Nassiri
#' @export
imo.subsamp <-function(data,num.outp,sub.size){
  n_s=sub.size
  data.subset.final=matrix(0,1,dim(data)[2]+1)
  colnames(data.subset.final)=c(colnames(data),'iteration')
  # Data shaould have the family ID as the firt column
  cluster_id=data[,1]
  # Defining the unique cluster sizes and split the data based on that
  freq.cluster=table(cluster_id)
  cluster.size=rep(freq.cluster,freq.cluster)
  unique.cluster.size=unique(cluster.size)
  for (R in 1:num.outp){
    #print(R)
    #flush.console()
    sel.subset.final=data[which(cluster.size<=sub.size),]
    unique.cluster.size.rest=unique.cluster.size[unique.cluster.size> sub.size]
    for (i in 1:length(unique.cluster.size.rest)){
      clust.size=unique.cluster.size.rest[i]
      data.sub=data[cluster.size==clust.size,]
      clust.num=dim(data.sub)[1]/clust.size
      idx.rand=matrix(rnorm(clust.num*clust.size),clust.size,clust.num)
      idx.order=apply(idx.rand,2,order)
      idx.sel1=idx.order[1:n_s,]
      idx.sel2=matrix(rep(seq(0,(clust.num*clust.size)-clust.size,clust.size),n_s),n_s,clust.num,byrow=T)
      idx.sel=c(idx.sel1+idx.sel2)
      sel.sub=data.sub[idx.sel,]
      sel.subset.final=rbind(sel.subset.final,sel.sub)
    }
    add.column=rep(R,dim(sel.subset.final)[1])
    data.subset.final1=cbind(sel.subset.final,add.column)
    colnames(data.subset.final1)=colnames(data.subset.final)
    data.subset.final=rbind(data.subset.final,data.subset.final1)
  }
  return(data.subset=data.subset.final[-1,])
}
