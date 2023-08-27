Test___MeanDiff___Multi.Reponses___MANOVA___Nonparametric = function(group_var,
                                                                     responses,
                                                                     simplify=FALSE){
  #=============================================================================
  # define a function
  #=============================================================================
  install_packages = function(packages, load=TRUE) {
    # load : load the packages after installation?

    for(pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
        install.packages(pkg)
      }

      if(load){
        library(pkg, character.only = TRUE)
      }
    }
  }





  #=============================================================================
  # packages
  #=============================================================================
  install_packages(c("lattice", "Matrix"), TRUE)









  #=============================================================================
  # Mult-KW
  #=============================================================================
  ### sort data by group ###
  o = order(group)
  group = group[o] %>% unname

  y<-as.matrix(y[o,])
  n<-length(group)
  p<-dim(y)[2]
  if(dim(y)[1] != n){
    return("number of oberservations not equal to length of group")
  }

  groupls<-unique(group)
  g<-length(groupls) #number of groups#
  groupind<-sapply(groupls,"==",group) #group indicator#
  ni<-colSums(groupind) #num of subj of each group#
  r<-apply(y,2,rank) #corresponding rank variable#
  ### calculation of statistic ###
  r.ik<-t(groupind)%*%r*(1/ni) #gxp, mean rank of kth variate in ith group#
  m<- (n+1)/2 #expected value of rik#
  u.ik<-t(r.ik-m)
  U<-as.vector(u.ik)
  V<-1/(n-1)*t(r-m)%*%(r-m) #pooled within-group cov matrix
  Vstar<-bdiag(lapply(1/ni,"*",V))
  W2<-as.numeric(t(U)%*%solve(Vstar)%*%U)
  ### return stat and p-value ###
  returnlist<-list(statistic=W2,d.f.=p*(g-1),
                   p.value=pchisq(W2,p*(g-1),lower.tail=F))
  if (simplify==TRUE) return (W2)
  else return (returnlist)









}


#
# ##########################################
# ######## 2 MKW with missing values #######
# ##########################################
# mkw.m<-function(group,y,r,weight){
#   ### count missng patterns ###
#   p<-dim(y)[2]
#   r.order<-r
#   y.order<-y
#   g.order<-group
#   for (i in 1:p){
#     oo<-order(r.order[,i])
#     y.order<-y.order[oo,]
#     g.order<-g.order[oo]
#     r.order<-r.order[oo,]
#   }
#   J<-nrow(unique(r.order,MARGIN=1)) #number of missing patterns
#   D<-data.frame(r.order)
#   n<-length(group)
#   ones<-rep(1,n)
#   mc<-aggregate(ones,by=as.list(D),FUN=sum) #counts of each missing pattern
#   mi<-mc$x
#   pi<-p-rowSums(mc[,1:p])
#   ### get W^2_j ###
#   W2<-rep(0,J)
#   W2.c<-0
#   i.st<-1
#   for (j in 1:J){
#     i.end<-i.st+mi[j]-1
#     gg<-g.order[i.st:i.end]
#     yy<-y.order[i.st:i.end,]
#     ii<-mc[j,1:p]==F
#     if (sum(as.numeric(ii))>0){
#       yy1<-as.matrix(yy[,ii])
#       if (mi[j]>pi[j]) W2[j]<-multkw(gg,yy1,simplify=T) ##### if mi[j]>p needs to dig more
#     }
#     if (prod(as.numeric(ii))==1) W2.c<-W2[j]
#     i.st<-i.end+1
#   }
#   if (weight=="prop") tj<-mi/sum(mi) else tj<-1/J
#   W2<-sum(tj*W2)
#   nu<-(W2)^2/sum((tj*W2)^2/pi/(g-1))
#   return(list(W2.m=W2,nu=nu,W2.c=W2.c))
# }
#
# ########################################
# ###### 3. monte carlo permutation ######
# ########################################
# multkw.perm<-function(nmc,group,y,r,weight){
#   ### count missng patterns ###
#   p<-dim(y)[2]
#   r.order<-r
#   y.order<-y
#   g.order<-group
#   for (i in 1:p){
#     oo<-order(r.order[,i])
#     y.order<-y.order[oo,]
#     g.order<-g.order[oo]
#     r.order<-r.order[oo,]
#   }
#   J<-nrow(unique(r.order,MARGIN=1)) #number of missing patterns
#   D<-data.frame(r.order)
#   n<-length(group)
#   ones<-rep(1,n)
#   mc<-aggregate(ones,by=as.list(D),FUN=sum) #counts of each missing pattern
#   mi<-mc$x
#   W2.m.perm<-rep(0,nmc)
#   W2.c.perm<-rep(0,nmc)
#   stats0<-mkw.m(group,y,r,weight)
#   W2.m<-stats0$W2.m
#   W2.c<-stats0$W2.c
#   nu<-stats0$nu
#   for (i in 1:nmc){
#     i.st<-1
#     group.perm<-rep(0,n)
#     group.perm<-sample(group,size=n)
#     stats<-mkw.m(group.perm,y,r,weight)
#     W2.m.perm[i]<-stats$W2.m
#     W2.c.perm[i]<-stats$W2.c
#   }
#   p.mkw.m.perm<-sum(W2.m<W2.m.perm)/nmc
#   p.mkw.m.chi2<-pchisq(W2.m,nu,lower.tail=FALSE)
#   p.mkw.c.perm<-sum(W2.c<W2.c.perm)/nmc
#   p.mkw.c.chi2<-pchisq(W2.c,p*(g-1),lower.tail=FALSE)
#   return(list(W2.m=W2.m,p.mkw.m.perm=p.mkw.m.perm,p.mkw.m.chi2=p.mkw.m.chi2,
#               p.mkw.c.perm=p.mkw.c.perm,p.mkw.c.chi2=p.mkw.c.chi2))
# }

## 예시
# group = data$예측분류
# y = data %>% select(pqi, score, ipc_cnt)
# result = multkw(group,y,simplify=FALSE)
# print(result)
#
# ## 결과
# $statistic
# [1] 1481.013
#
# $d.f.
# [1] 15
#
# $p.value
# [1] 6.106133e-307
