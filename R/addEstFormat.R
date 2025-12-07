addEstFormat <- function(pars,rse.cov,source.ci="cov"){

    CVlnorm <- function(omega){
        sqrt(exp(omega)-1)
    }
    invlogit <- function(x) 1/(1+exp(-x))


### transformed values for reporting
### do not transform se or rse
    cols.trans <- intersect(cc(est,CI.l,CI.u,CI.l.boot,CI.u.boot),colnames(pars))
    pars[trans%in%cc(log,logTrans),(cols.trans):=lapply(.SD,exp),.SDcols=cols.trans]
    pars[trans%in%cc(logit),(cols.trans):=lapply(.SD,invlogit),.SDcols=cols.trans]

    
    if(rse.cov){
        if("rse"%in%colnames(pars)){
            pars[,tab.rse:=percent(rse,accuracy=.1)]
            pars[FIX==1,tab.rse:="-"]
        } else {
            warning("rse.cov is TRUE but rse is missing. Ignoring.")
            pars[,tab.rse:="-"]
        }
    }

    
### TODO tab.rse is using rse, i.e. relying on covariance step. Move to print function? 
    ## pars[par.type=="THETA",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[par.type=="THETA",tab.est:=signif(est,3)]
    if(rse.cov) pars[,tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]
    ## pars[par.type=="THETA"&FIX==1,tab.est:=sprintf("%s (fixed)",signif(est,3))]
    pars[FIX!=0,tab.est:=sprintf("%s (fixed)",signif(est,3))]
    pars[par.type=="OMEGA"&trans=="lognormal"&FIX==0,tab.CV:=percent(CVlnorm(est),acc=1)]
    ## pars[par.type=="OMEGA"&trans=="lognormal"&FIX==0,tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]
    pars[par.type=="OMEGA"&trans=="lognormal"&FIX==0,tab.est:=sprintf("%s [%s]",signif(est,3),tab.CV)]

    ## for this, the associated theta est has to be used to calc CV
    ## pars[panel=="OMEGAdiag"&trans=="normal",tab.CV:=percent(sd/est,acc=1)]
    ## pars[panel=="OMEGAdiag"&trans=="normal",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]
    pars[par.type=="OMEGA"&trans=="normal"&FIX==0,tab.est:=sprintf("%s [%s]",signif(est,3),tab.CV)]
    if(rse.cov) pars[par.type=="OMEGA"&trans=="normal"&FIX==0,tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]
    ##    pars[panel=="OMEGAdiag"&is.na(label),tab.lab:=paste("BSV",symbol)]

    
    ## pars[panel=="OMEGAcorr"&FIX==0,tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.corr,tab.rse)]
    pars[panel=="OMEGAcorr"&FIX==0,tab.est:=sprintf("%s [%s]",signif(est,3),tab.corr)]
    if(rse.cov) pars[panel=="OMEGAcorr"&FIX==0,tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]


    ## residual error terms
    ## pars[par.type=="THETA"&trans=="addErr",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[par.type=="THETA"&trans=="addErr",tab.est:=sprintf("%s",signif(est,3))]
    if(rse.cov) pars[par.type=="THETA"&trans=="propErr",tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]
    
    ## pars[par.type=="SIGMA"&FIX==0,tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[par.type=="SIGMA"&FIX==0,tab.est:=sprintf("%s",signif(est,3))]
    if(rse.cov) pars[par.type=="SIGMA"&FIX==0,tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]

                                        # pars[par.type=="SIGMA"&trans=="propErr"&FIX==0,
                                        #      tab.est:=sprintf("%s [%s] (%s)",est,percent(sqrt(est),acc=1.1),tab.rse)]
    pars[par.type=="SIGMA"&trans=="propErr"&FIX==0,
         tab.est:=sprintf("%s [%s]",est,percent(sqrt(est),acc=1.1))]
    if(rse.cov) pars[par.type=="SIGMA"&trans=="propErr"&FIX==0,
                     tab.est:=sprintf("%s (%s)",tab.est,tab.rse)]


### confidence intervals
    if(all(cc(CI.l,CI.u)%in%colnames(pars))){
        pars[,tab.CI:=sprintf("[%s,%s]",signif(CI.l,2),signif(CI.u,2))]
        pars[FIX==1,tab.CI:="-"]
    } else {
        pars[,tab.CI:="-"]
    }

    if(all(cc(CI.l.boot,CI.u.boot)%in%colnames(pars))){
        pars[,tab.CI.boot:=sprintf("[%s,%s]",signif(CI.l.boot,2),signif(CI.u.boot,2))]
        pars[FIX==1,tab.CI.boot:="-"]
    } else {
        pars[,tab.CI.boot:="-"]
    }


    ## Latex versions of columns for report tables
    pars[,tab.est.ltx:=latexify(tab.est,doublebackslash = FALSE)]

    return(pars)

}
