bspois <-
function(x,y,offset=0,nk=31,degree=1,lambda=NULL,phi=NULL,
                   alpha=0.05, err=0.0001,verbose=T,l0=l0)
  {
  
  # if lambda is provided - but phi is not:  phi is assumed 1
  if(!is.null(lambda)){ 
    if(is.null(phi)) phi=1
    fit= bspois.basic(x,y,offset,nk=nk,degree=degree,lambda=lambda,phi=phi)
    dev = 2*(log(dpois(y,y))- log(dpois(y,fit$fit)))
    # if (verbose){cat('phi=',phi)} 

  }
  
  
  # .................................  if lambda is NULL, but phi is given
  if(is.null(lambda) & !is.null(phi))
  {  
   nx = length(x)
   if (verbose){cat('Iterations: relative error in sv2-hat =',err,'\n')}
  
  # start
  lambda=l0; oldval = 2*lambda
  while (abs(oldval-lambda)/lambda > err & lambda< 1/err){
   oldval = lambda
   fit = bspois.basic(x,y,offset,nk=nk,degree=degree, lambda=lambda, phi=phi)
   df = fit$df
   tr = fit$tr 
   v = fit$v; vRv = sum(diff(diff(v))^2)
   dev = 2*(log(dpois(y,y))- log(dpois(y,fit$fit)))
   sv2 = vRv/(df-2)
    lambda= phi/sv2; lambda   
 
  if (verbose){cat('sv2=',sv2,'  df=',df,'  lambda=',lambda,'\n')}
  } # end while
  } # end if lambda=NULL
  
  
  # ........................   if both lambda and phi are to be computed
  if(is.null(lambda) & is.null(phi)){   
  nx = length(x)
  if (verbose){cat('Iterations: relative error in phi-hat =',err,'\n')}
  
  # start
  phi=1; oldphi=2*phi
   lambda=10 ; oldval = 2*lambda
 while ( (abs(oldphi-phi)/phi > err | abs(oldval-lambda)/lambda > err) & lambda< 1/err ){  #
   oldval = lambda
   oldphi = phi 
   fit = bspois.basic(x,y, offset,nk=nk,degree=degree, lambda=lambda, phi=phi)
   df = fit$df
   tr = fit$tr 
   v = fit$v; vRv = sum(diff(diff(v))^2)
   dev = 2*(log(dpois(y,y))- log(dpois(y,fit$fit)))
    phi = var( (y-fit$fit)/sqrt(fit$fit), na.rm=T)
   
   sv2 = vRv/(df-2)
    lambda= phi/sv2; lambda       #phi/sv2; 
    if (verbose){cat('phi=',phi, '  sv2=',sv2,'  df=',df,'  lambda=',lambda,'\n')}
   } # end while
  } # end if lambda=NULL
  
  # 100(1-alpha) CI for the fit
  up = fit$lin + qnorm(1-alpha/2)*sqrt(fit$dH); up = exp(up)
  lo = fit$lin - qnorm(1-alpha/2)*sqrt(fit$dH); lo = exp(lo)
  sv2=1/lambda  # solo nel 1 caso ?
  
  return(list(fit=fit$fit, linear.pred = fit$lin, 
         beta=fit$beta,v=fit$v, phi=phi, sv2=sv2, df=fit$df, 
         dH=fit$dH, lower.ci = lo, upper.ci = up, deviance= sum(dev)))
  
  }
