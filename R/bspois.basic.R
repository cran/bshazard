bspois.basic <-
function(x,y,offset=0,v= NULL, degree=1,nk=31,lambda=10, phi=1)
  {
  #if with covariates
  x<-as.matrix(x)
  ncov<-ncol(x)-1
  
  # Design matrix
  Z = bs.design(x[,1],nk,degree)
  
  X = as.matrix(x[,-1]) #linear covariate  (centered)    
  
  
  # second difference
  D = diag(ncol(Z))
  for (i in 1:2) D = diff(D)
  Rinv = t(D) %*% D
  
  # starting
  m0 = mean(y*exp(-offset))
  #add fixed effect
  beta = as.matrix(rep(c(0.0000),ncov))
    
  if (is.null(v)) v = rep(log(m0), ncol(Z))
  
  for (i in 1:25){
    hmu = offset+ c(Z %*% v)+ c(X %*% beta) #added F.E.
    mu = exp(hmu)
    W = mu
    Y = hmu + (y-mu)/mu
  
    # update R.E.
    ZWZ = t(Z*W)%*% Z
    ZWZRi = solve(ZWZ+ lambda*Rinv)
    v =  ZWZRi %*% t(Z*W)%*%(Y-offset - X %*% beta)
          #inv(Z'WZ+lambda*Rinv)((ZW)'(Y-offset-Xbeta)
    # update F.E.
    if (ncov>0) beta = solve(t(X*W) %*% X) %*% (t(X*W) %*% (Y - offset- Z %*% v))
          #inv(X'WX)(X'W(Y-Zv))  
   }
    hmu = offset + c(Z %*% v)   
    mu = exp(hmu)
  df = sum(ZWZRi * ZWZ) # = tr(ZWZRi %*% t(ZWZ)) = tr(ZWZRi %*% ZWZ)
  tr = -sum(log(eigen(ZWZRi)$val))
  dH= apply((Z %*% ZWZRi)*Z, 1, sum)  ## diag(Z %*% ZWZRi %*% t(Z))
  if (ncov==0) beta = NULL
  return(list(fit=mu, linear.predictor=hmu,beta = beta, v=v, df=df, dH=dH, tr=tr,phi=phi))
  }
