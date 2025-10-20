##' Create parameter table data.frame using `NMreadExt()` and
##' `NMreadParsText()`
##'
##' Automated parameter table creation of Nonmem models. Currently,
##' requires a covariance step.
##' 
##' @param file.lst Control stream. If possible, it is recommended to
##'     use output control stream. You can also use input control
##'     stream.
##' @param args.ParsText List of arguments to be passes to
##'     `NMreadParsText()`.
##' @param dt.labels Optional table of labels to overwrite results
##'     from `NMreadParsText()`. Useful to correct issues on a model
##'     you don´t want to rerun.
##' @param by.labels If `dt.labels` provided, names of columns to
##'     merge by.
##' @param drop.symbol Symbol values to not include. To be specific,
##'     rows with these values in the symbol column will be omitted in
##'     the generated table.
##' @details
##'
##' ### Guideline for panel column:
##'    ## THETA: struct (default), cov, RV
##'    ## OMEGA: IIV/BSV (default), IOV/BOV
##'    ## OMEGA off diag: (handled automatically)
##'    ## SIGMA: (handled automatically)
##'
##' ### trans column:
##'    ## THETA: none (default), log, logit, addErr/SD, propErr/CV
##'    ## OMEGA: lognormal (default), normal
##'    ## OMEGA: off diag: (handled automatically)
##'    ## SIGMA: Not implemented.
##' 
##' trans is used for the following:
##'
##' - Transformation of model parameters (say THETA(1)) values and CI
##' to the scale of the interpretable parameter, (say CL).
##'
##' Theta's addErr and propErr gives formatting as residual standard
##' deviations for additive and proportional error terms,
##' respectively. On OMEGA's, the lognormal distribut is for
##' CL=EXP(THETA +ETA) , normal is for additive effects:
##' PAR=EXP(THETA) + ETA.
##' 
##'
##' - Formatting of CV 
##' trans="normal" or "none": CV=se/est
##' trans="lognormal" : CV=sqrt(exp(OMEGA[i,i])-1)
##' 
##' 
##' Inverse of log (inverse is exp) and logit (NMcalc::invlogit) will
##' be applied to parameter values and confidence intervals. SE and
##' RSE are calculated before the backtransformation and must be
##' interpreted on the scale of the estimated THETA, not the scale of
##' the transformed parameter (e.g. Clerance when CL=EXP(THETA+...).
##'
##' SIGMA's are sorted and grouped as residual error terms, but
##' specific formatting of additive, proportional or exponential is
##' missing. Estimates reported are of the variances and
##' correlations.
##'
##'
##' \strong{Off-diagonals in OMEGA and SIGMA}
##' 
##' These should be fully automated. They are automatically
##' identified, and labels are auto-generated based on labels of the
##' associated diagonal elements.
##'
##' Off-diagonal estimates are shown as correlations. But
##' SE is standard error of the variances or
##' cov-variances. For off-diagonal elements, the scale of the
##' estimate (correlation) and the scale of the associated uncertainty
##' (covariance) are different.
##'
##' 
##' @import data.table
##' @import NMdata
##' @importFrom scales percent
##' @importFrom stats cov2cor
##' @importFrom NMcalc CVlnorm invlogit
##' @export
##' @seealso formatParameterTable

### should also take arg to include fixed parameters. Maybe default
### should be estimated and non-zero?

createParameterTable <- function(file.lst,args.ParsText=NULL,dt.labels=NULL,by.labels="symbol",drop.symbol){
    
### If NMcalc version < 0.0.3 we need to define CVlnorm
    CVlnorm <- function(omega){
        sqrt(exp(omega)-1)
    }

    if(missing(drop.symbol)) drop.symbol <- NULL

    
### this example requires NMdata 0.1.5 (a little more code is needed for 0.1.4)

### parameters are assumed to be concistently labeled in $THETA,
### $OMEGA and $SIGMA sections

### Off-diagonal elements are automatically identified by NMreadParsText().
    
    pars <- NMreadExt(file=file.lst,as.fun="data.table")
    
    labs <- do.call(NMreadParsText,
                    as.list(c(args.ParsText,file=file.lst,as.fun="data.table"))
                    )
    
    ## we need a pars with anything found in labels, plus estimated omega and sigma off-diags
    pars.found <- merge(pars,labs[,.(parameter)],by="parameter",all.y=TRUE)
    pars <- pars[parameter%in%pars.found[,parameter]|(FIX==0 & i!=j)]
    
    ## if(!is.null(drop.symbol)){
    ##     if(!"symbol"%in%colnames(labs)){
    ##         warning("drop.symbol uses but symbol is not found in parameter labels. drop.symbol not used.")
    ##         drop.symbol <- NULL
    ##     }
    ##     if(!is.null(drop.symbol)){
    ##         labs <- labs[!grepl(pattern=drop.symbol,symbol)]
    ##     }
    ## }
    
    ## pars <- mergeCheck(labs[,!(c("i","j","par.type"))],pars,by=cc(model,parameter),all.x=T,quiet=T)
    pars <- mergeCheck(pars,labs[,!(c("i","j","par.type"))],by=cc(model,parameter),all.x=T,quiet=T)
    
##### group parameters

#### guideline todo: How to define sigmas?
    
### Guideline for panel column:
    ## THETA: struct (default), cov, RV
    ## OMEGA: IIV/BSV (default), IOV/BOV
    ## OMEGA off diag: (handled automatically)

### trans:
    ## THETA: none (default), log, logit, addErr/SD, propErr/CV
    ## OMEGA: lognormal (default), normal
    ## OMEGA: off diag: (handled automatically)
    
    if(!"panel"%in%colnames(pars)) pars[,panel:=NA_character_]
    pars[is.na(panel)&par.type=="THETA",panel:="struct"]
    ## pars[par.type=="THETA"&trans%in%cc(addErr,propErr),parGRP:="resVar"]
    ## pars[par.type=="THETA"&symbol%in%cc(ADDP,ADDM,PROPP,PROPM),parGRP:="resVar"]
    
    pars[is.na(panel)&par.type=="OMEGA"&i==j,panel:="IIV"]
    pars[par.type=="OMEGA"&panel=="BSV",panel:="IIV"]
    pars[par.type=="OMEGA"&panel=="BOV",panel:="IOV"]
    pars[is.na(panel)&par.type=="OMEGA"&i!=j,panel:="OMEGAcorr"]

    pars[is.na(panel)&par.type=="SIGMA"&i==j,panel:="resvar"]
    pars[is.na(panel)&par.type=="SIGMA"&i!=j,panel:="SIGMAcorr"]

    pars[tolower(panel)%in%cc(rv,resvar),panel:="resvar"]

### TODO: do we want to generate a factor with the observed values, guessing an order?
    ## pars[,panel:=factor(parGRP,levels=cc(theta,OMEGAdiag,OMEGAcorr,resVar))]

    
    ## pars[,.N,by=.(parGRP)]

    ##  colnames(pars)

    ## if(!"panel"%in%colnames(pars))
    if(!is.null(dt.labels)){
        ## cols.drop <- setdiff(colnames(dt.labels),by.labels)
        ## cols.drop <- intersect(cols.drop,colnames(pars))
        ## pars <- mergeCheck(pars[,!(cols.drop),with=F],dt.labels,by=by.labels,all.x=T,quiet=T)
        
        pars0 <- copy(pars)
        pars0[,row:=.I]
        pars.new <- mergeCheck(dt.labels,pars0,by=by.labels,all.x=T,quiet=T,common.cols="drop.y")
        
        pars <- rbind(
            pars0[!row%in%pars.new[,row]]
           ,
            pars.new
        )
        setorder(pars,row)
        pars[,row:=NULL]
    }


    
#### requires a covariance step
    if("se"%in%colnames(pars)){
        pars[,rse:=abs(se/est)]
        pars[,CI.l:=est-1.96*se]
        pars[,CI.u:=est+1.96*se]
        pars[FIX==1,(cc(rse,CI.l,CI.u)):=NA]
    }

    ## est.orig is the estimate on the scale used in Nonmem
    pars[,est.orig:=est]
    ## back transform parameters estimated on a transformed scale
    ##pars[,.N,.(par.type,trans)]

    
    
    ## correlation between omegas
    Sigma <- dt2mat(pars[par.type=="OMEGA"])
    ## Sigma <- NMdata::dt2mat(pars[par.type=="OMEGA"])
    ## Sigma[is.na(Sigma),Sigma:=0]
    mat.cor <- cov2cor(Sigma)
    dt.cor <- as.data.table(mat.cor)
    dt.cor[,i:=.I]
    dt.cor <- melt(dt.cor,id.vars="i",variable.name="j")
    dt.cor[,j:=as.numeric(j)]
    dt.cor <- dt.cor[j<=i]
    ## pars <- mergeCheck(pars,dt.cor[,.(par.type="OMEGA",i,j,tab.corr=round(value))],by=cc(par.type,i,j),all.x=TRUE)
    pars <- mergeCheck(pars,dt.cor[,.(par.type="OMEGA",i,j,corr=value)],by=cc(par.type,i,j),all.x=TRUE,quiet=T)


    
### these use parGRP to identify the groups
    if(!"label"%in%colnames(pars)){
        pars[,label:=""]
    }
    if(!"unit"%in%colnames(pars)){
        pars[,unit:=""]
    }
    pars[par.type=="THETA"&panel!="cov",tab.lab:=paste(label,symbol,sep=", ")]

    pars[grepl("^ *-* *$",unit),unit:=NA]

    pars[panel=="BSV"&is.na(label),label:=paste("BSV",symbol)]
    pars[panel=="BOV"&is.na(label),label:=paste("BOV",symbol)]

###### patching/standardizing trans
    if(!"trans"%in%colnames(pars)){
        pars[,trans:=NA_character_]
    }

    pars[par.type=="THETA"&is.na(trans),trans:="none"]
    pars[par.type=="THETA"&trans=="logTrans",trans:="log"]
    pars[par.type=="OMEGA"&i==j&is.na(trans),trans:="lognormal"]
    pars[par.type=="OMEGA"&trans=="lognormalOm",trans:="lognormal"]

    pars[par.type=="THETA"&tolower(trans)=="SD",trans:="addErr"]
    pars[par.type=="THETA"&tolower(trans)=="CV",trans:="propErr"]
    
    pars[par.type=="OMEGA"&trans=="normal",label:=paste(label,"(additive)")]
    ## label done

    
    ## tab.lab is the formatted string (not latex) to show in a table
    pars[!is.na(unit),tab.lab:=sprintf("%s (%s)",label,unit)]
    pars[is.na(unit),tab.lab:=sprintf("%s",label)]
    
    if("rse"%in%colnames(pars)){
        pars[,tab.rse:=percent(rse,accuracy=.1)]
        pars[FIX==1,tab.rse:="-"]
    } else {
        pars[,tab.rse:="-"]
    }

    pars[panel=="OMEGAcorr",tab.corr:=percent(corr,accuracy=1)]
    pars[panel=="SIGMAcorr",tab.corr:=percent(corr,accuracy=1)]

### transformed values for reporting
### do not transform se or rse
    cols.trans <- intersect(cc(est,CI.l,CI.u),colnames(pars))
    pars[trans%in%cc(log,logTrans),(cols.trans):=lapply(.SD,exp),.SDcols=cols.trans]
    pars[trans%in%cc(logit),(cols.trans):=lapply(.SD,invlogit),.SDcols=cols.trans]

    pars[par.type=="THETA",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[par.type=="THETA"&FIX==1,tab.est:=sprintf("%s (fixed)",signif(est,3))]
    pars[par.type=="OMEGA"&trans=="lognormal",tab.CV:=percent(CVlnorm(est),acc=1)]
    pars[par.type=="OMEGA"&trans=="lognormal",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]

    ## for this, the associated theta est has to be used to calc CV
    ## pars[panel=="OMEGAdiag"&trans=="normal",tab.CV:=percent(sd/est,acc=1)]
    ## pars[panel=="OMEGAdiag"&trans=="normal",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]
    pars[par.type=="OMEGA"&trans=="normal",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    ##    pars[panel=="OMEGAdiag"&is.na(label),tab.lab:=paste("BSV",symbol)]

    
    pars[panel=="OMEGAcorr",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.corr,tab.rse)]


    if(all(cc(CI.l,CI.u)%in%colnames(pars))){
        pars[,CI:=sprintf("[%s,%s]",signif(CI.l,2),signif(CI.u,2))]
        pars[FIX==1,CI:="-"]
    } else {
        pars[,CI:="-"]
    }
    ## residual error terms
    pars[par.type=="THETA"&trans=="addErr",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[par.type=="THETA"&trans=="propErr",tab.est:=sprintf("%s (%s)",percent(est,acc=1.1),tab.rse)]
    
    pars[par.type=="SIGMA",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]


    ## parameter - label
    pars[,par.label:=sprintf("%s - %s", par.name,label)]

    ## Latex versions of columns for report tables
    pars[,tab.est.ltx:=latexify(tab.est,doublebackslash = FALSE)]
    pars[,tab.lab.ltx:=latexify(tab.lab,doublebackslash = FALSE)]
    pars[par.type=="THETA",parameter.ltx:=paste0("$\\theta_{",i,"}$")]
    pars[par.type=="OMEGA",parameter.ltx:=paste0("$\\Omega_{",i,",",j,"}$")]
    pars[par.type=="SIGMA",parameter.ltx:=paste0("$\\sigma_{",i,",",j,"}$")]

    if(!is.null(drop.symbol)){
        if(!"symbol"%in%colnames(pars)){
            warning("drop.symbol uses but symbol is not found in parameter labels. drop.symbol not used.")
            drop.symbol <- NULL
        }
        if(!is.null(drop.symbol)){
            pars <- pars[!grepl(pattern=drop.symbol,symbol)]
        }
    }
    
    pars[]
}


