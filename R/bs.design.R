bs.design <-
function(x,nk,degree){
    xl= min(x)-0.00001
    xr = max(x)+0.00001
    dx = (xr-xl)/(nk-1)
    knots = seq(xl- degree*dx,xr+degree*dx,by=dx )
    bs = spline.des(knots,x,degree+1)$design
    return(bs)
  }
