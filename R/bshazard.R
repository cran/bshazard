bshazard <-
function(formula,data = parent.frame(),nbin=NULL, nk=31,degree=1,l0=10,lambda=NULL,phi=NULL,
                             alpha=0.05, err=0.0001,verbose=T) UseMethod("bshazard")
