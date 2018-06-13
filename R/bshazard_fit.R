bshazard_fit <-
function(mf,covs,nbin=NULL, nk=31,degree=1,l0=10,lambda=NULL,phi=NULL,
                   alpha=0.05, err=0.0001,verbose=T)       
  {  # UseMethod("bshz")
      #   mf<-model.frame(formula=formula,data=data)
      #    covs<-  model.matrix(attr(mf,"terms"),data=mf)
          names.cov<-make.names(dimnames(covs)[2][[1]][-1])
          ncov<-(ncol(covs)-1) #number of covariate in model
       #checks of input
         
       if (attr(mf[[1]], "type") %in% c("interval","interval2")) print("Interval censored data not considered")
         nobs<- nrow(mf[[1]])
       #  save the Surv object and prepare it to use it in Lexis macro
            surv<- mf[[1]]   
            if (ncol(surv)==2) {entry_<-rep(0,length(surv[,"time"])); exit_<-surv[,"time"]}
            if (ncol(surv)==3) {entry_<-surv[,"start"]; exit_<-surv[,"stop"]       }
            dataf<-data.frame(entry_=entry_,exit_=exit_,status=surv[,"status"],covs)
      #  compute the means of each covariate and center the covariates for estimation
            medie<-colMeans(dataf)[names.cov]  
          if (length(names.cov)>0)  dataf[,paste(names.cov,"_c",sep="")]<- dataf[,names.cov] -  matrix(medie,nrow(covs),ncol(covs)-1,byrow=T)
     
     ############# if not covariates & not pre bin ###########################
      if (length(names.cov)==0 & is.null(nbin))  {
                      fit = survfit(Surv(dataf$entry_,dataf$exit_,dataf$status)~1)
                      dt<-diff(c(0,fit$time))
                      X = fit$time
                      y = fit$n.event
                      PY = fit$n.risk* dt   # person-years at risk
                      rate = y/PY
       input<-data.frame(X=X,n.event=y,person.time=PY,raw.hazard=rate)
       input<-input[input$X>0 & input$person.time>0,]
       X<-input$X ;       y<-input$n.event    ;    PY<-input$person.time
            }     else { ###    if covariates
     ############# with covariates or prebin ###########################
     
               #  Lexis object                                                          
                  lex<-Lexis(entry=list(per=entry_),exit=list(per=exit_),exit.status = status,data=dataf)
               #  breaks to split the data according to min,max and number of bins
              
                   if (!is.null(nbin)) xx<- seq(min(entry_),max( exit_),len=(nbin+1)) else {
                    # if nbin is not specified split the data at each time at exit in the data
                   xx<- sort(unique(dataf$exit)) # [dataf$status==1] 
                    }
               #  split according to bins/times  
                   splitted<-splitLexis(lex, breaks=c(min(entry_),xx,max( exit_)),time.scale="per") 
                   splitted$per.mid<-timeBand(splitted, time.scale="per", type="middle") #take the middle of each bins as ref time
                
                # Aggregate data 
                if (length(names.cov)>0) {  # if account for covariates
                  ysum_<-aggregate(x=as.numeric(status(splitted,at="exit")==1),by=splitted[,c("per.mid",paste(names.cov,"_c",sep=""))], FUN=sum)
                  PY_  <-aggregate(x=splitted$lex.dur,                         by=splitted[,c("per.mid",paste(names.cov))], FUN=sum) 
                  } else   {               #  if no covariates 
                  ysum_<-aggregate(x=as.numeric(status(splitted,at="exit")==1),by=list(splitted[,"per.mid"]), FUN=sum)
                  PY_  <-aggregate(x=splitted$lex.dur,                         by=list(splitted[,"per.mid"]), FUN=sum)
                  }       
                  
                  ysum<-ysum_[,ncol(ysum_)]  #last column is the sum of events in each bin
                  PY<-PY_[,ncol(PY_)]        #last column is the sum of person time in each bin
                  X<-as.matrix(ysum_[,-ncol(ysum_)])   #time and  CENTERED covariates values  (if present)
                 # Order data by time 
                   ord= order(X[,1])
                   X<-X[ord,];y<-ysum[ord];PY<-PY[ord]
                   rate<-y/PY
                  input = data.frame(X,n.event=y,person.time=PY,raw.hazard=rate)
                }
       fit =  bspois(X,y,offset = log(PY),  nk=nk, degree=degree,lambda=lambda,phi=phi,
                         alpha=alpha, err=err,verbose=verbose,l0=l0)
    #output 
      #1.raw.data      
          raw.data<-input
          names(raw.data)[1] <- "time"
         
          if (ncol(covs)>1) {   #if there are covariates attach the value of covaiates
          raw.data[,paste(names.cov)]<-raw.data[,paste(names.cov,"_c",sep="")]+matrix(medie,nrow=nrow(raw.data),ncol=ncol(covs)-1,byrow=T)
          col.d<-seq(2,2+ncol(covs)-2,1)
          raw.data<-raw.data[,-col.d]
          }
      #2. fitted smoothing and parameters
        hazard=fit$fit/PY; lower.ci=fit$low/PY;   upper.ci=fit$upper/PY 
        dH<-fit$dH
         times<-as.matrix(X)[,1]
   
      if (length(names.cov)>0)  {
          cc<-medie
              #d.unique<-unique(data.frame(times,hazard,lower.ci,upper.ci,dH))
              times.unique<-times[!duplicated(times)];
              hazard.unique<-hazard[!duplicated(times)]
              lower.ci.unique<-lower.ci[!duplicated(times)]
              upper.ci.unique<-upper.ci[!duplicated(times)]
              dH.unique<-dH[!duplicated(times)]
               }   else       {
                  times.unique<-times  ;hazard.unique<-hazard;  lower.ci.unique<-lower.ci  ;upper.ci.unique<-upper.ci ;dH.unique<-dH
                  cc<-NULL}
#   return(list(raw.data=raw.data,
#   time=times.unique,cov.value=cc,hazard=hazard.unique, lower.ci=lower.ci.unique, upper.ci=upper.ci.unique
#                ,dH=dH.unique,fit=fit$fit,  beta=fit$beta, sv2=fit$sv2,phi=fit$phi, df=fit$df
       
   return(list(raw.data=raw.data,  n=nobs,
   time=times.unique,cov.value=cc,hazard=hazard.unique, lower.ci=lower.ci.unique, upper.ci=upper.ci.unique
                ,dH=dH.unique,fitted.values=fit$fit,coefficients=fit$beta, sv2=fit$sv2,phi=fit$phi, df=fit$df
           )) 
  }
