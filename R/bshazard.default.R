bshazard.default <-
function(formula,data = parent.frame(),nbin=NULL, nk=31,degree=1,l0=10,lambda=NULL,phi=NULL,
                             alpha=0.05, err=0.0001,verbose=T) {
         mf<-model.frame(formula=formula,data=data)
        covs<-  model.matrix(attr(mf,"terms"),data=mf)
        y<-model.response(mf)
  est<-bshazard_fit(mf,covs,nbin, nk,degree=degree,l0,lambda,phi,alpha, err,verbose)       
  est$call<-match.call()
  class(est)<-"bshazard"
  est
}
