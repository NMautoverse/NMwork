
##' @param file Control stream. If possible, it is recommended to use output control stream.

### should also take arg to include fixed parameters. Maybe default
### should be estimated and non-zero?

createParameterTable <- function(file,args.ParsText,dt.labels=NULL,by.labels){
    
### If NMcalc version < 0.0.3 we need to define CVlnorm
    CVlnorm <- function(omega){
        sqrt(exp(omega)-1)
    }

    
### this example requires NMdata 0.1.5 (a little more code is needed for 0.1.4)

### parameters are assumed to be concistently labeled in $THETA, $OMEGA and $SIGMA sections

### For off-diagonal elements to be identified in NMreadParsText, you must:
    ## Include a counter in $OMEGA.
    ## Use a - to delimit the row and column number, say 1-2 to denote covariance between OMEGA 1 and 2
    ## Call the counter "num" when reading the labels.
    
#### requires a covariance step
    pars <- NMreadExt(file=file)
    labs <- do.call(NMreadParsText,c(args.ParsText,file=file))
    
    ## subset whatever should be in the parameter table. In this example, we skip FIXed parameters.
    pars <- mergeCheck(labs[,!(c("i","j","par.type"))],pars,by=cc(model,parameter),all.x=T,quiet=T)
    
##### group parameters
    pars[par.type=="THETA",parGRP:="theta"]
    pars[par.type=="THETA"&trans%in%cc(addErr,propErr),parGRP:="resVar"]
    pars[par.type=="THETA"&symbol%in%cc(ADDP,ADDM,PROPP,PROPM),parGRP:="resVar"]

    pars[par.type=="OMEGA"&i==j,parGRP:="OMEGAdiag"]
    pars[par.type=="OMEGA"&i!=j,parGRP:="OMEGAcorr"]
    pars[,parGRP:=factor(parGRP,levels=cc(theta,OMEGAdiag,OMEGAcorr,resVar))]

    pars[par.type=="SIGMA",parGRP:="resVar"]
    
    pars[,.N,by=.(parGRP)]

   ##  colnames(pars)

    ## if(!"panel"%in%colnames(pars))
    if(!is.null(dt.labels)){
        ## cols.drop <- setdiff(colnames(dt.labels),by.labels)
        ## cols.drop <- intersect(cols.drop,colnames(pars))
        ## pars <- mergeCheck(pars[,!(cols.drop),with=F],dt.labels,by=by.labels,all.x=T,quiet=T)
        
        pars <- mergeCheck(pars,dt.labels,by=by.labels,all.x=T,quiet=T,common.cols="drop.x")
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
    pars[,.N,.(par.type,trans)]
    
### do not transform se or rse
    cols.trans <- intersect(cc(est,CI.l,CI.u),colnames(pars))
    pars[trans%in%cc(log,logTrans),(cols.trans):=lapply(.SD,exp),.SDcols=cols.trans]
    
    
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
    if(!"parGRP"%in%colnames(pars)){
        pars[,parGRP:=""]
    }
    if(!"panel"%in%colnames(pars)){
        pars[,panel:=""]
    }
    if(!"label"%in%colnames(pars)){
        pars[,label:=""]
    }
    if(!"unit"%in%colnames(pars)){
        pars[,unit:=""]
    }
    pars[parGRP=="theta"&panel!="cov",tab.lab:=paste(label,symbol,sep=", ")]

    pars[grepl("^ *-* *$",unit),unit:=NA]

    pars[parGRP=="OMEGAdiag"&is.na(label),label:=paste("BSV",symbol)]
    pars[parGRP=="OMEGAdiag"&trans=="normalOm",label:=paste(label,"(additive)")]
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

    
    pars[parGRP=="OMEGAdiag"&is.na(trans),trans:="lognormalOm"]
    pars[parGRP=="OMEGAcorr",tab.corr:=percent(corr,accuracy=1)]
    pars[parGRP=="theta",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
    pars[parGRP=="theta"&FIX==1,tab.est:=sprintf("%s (fixed)",signif(est,3))]
    pars[parGRP=="OMEGAdiag" &trans=="lognormalOm",tab.CV:=percent(CVlnorm(est),acc=1)]
    pars[parGRP=="OMEGAdiag"&trans=="lognormalOm",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]
    ## for this, the associated theta est has to be used to calc CV
    ## pars[parGRP=="OMEGAdiag"&trans=="normalOm",tab.CV:=percent(sd/est,acc=1)]
    ## pars[parGRP=="OMEGAdiag"&trans=="normalOm",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.CV,tab.rse)]
    pars[parGRP=="OMEGAdiag"&trans=="normalOm",tab.est:=sprintf("%s (%s)",signif(est,3),tab.rse)]
##    pars[parGRP=="OMEGAdiag"&is.na(label),tab.lab:=paste("BSV",symbol)]

    
    pars[parGRP=="OMEGAcorr",tab.est:=sprintf("%s [%s] (%s)",signif(est,3),tab.corr,tab.rse)]
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

    
    ## Latex versions of columns for report tables
    pars[,tab.est.ltx:=latexify(tab.est,doublebackslash = FALSE)]
    pars[,tab.lab.ltx:=latexify(tab.lab,doublebackslash = FALSE)]
    pars[par.type=="THETA",parameter.ltx:=paste0("$\\theta_{",i,"}$")]
    pars[par.type=="OMEGA",parameter.ltx:=paste0("$\\Omega_{",i,",",j,"}$")]
    pars[par.type=="SIGMA",parameter.ltx:=paste0("$\\sigma_{",i,",",j,"}$")]
    
    formatParTable(pars)
}

##' @details A support function for createParameterTable().
##' @keywords internal
formatParTable <- function(pars){
    
    pars[trans%in%c("log","logTrans"),parameter.ltx:=sub("\\$ *$","\\{\\}\\^\\*\\$",parameter.ltx)]
    ## shrinkage is not included in the parameter table for now.

#### It would be good to be able to save this one
    ## NMstamp(pars,script=label.script,model=model$label)
    ## saveRDS(pars,fnOut)
    pars.full <- copy(pars)
    
    ## subset whatever should be in the parameter table. In this example, we skip FIXed parameters.
    pars <- pars[symbol!="not used"]
    pars <- pars[FIX==0|symbol%in%cc(F1TAB,AMAX,AC50,KENZ,V2PWT,V3PWT,V2MWT,V3MWT)]

    pars.write <- pars[,.(Parameter=par.name,
                          Symbol=symbol,
                          Label=tab.lab,
                          Estimate=tab.est,
                          "95% CI"=CI,
                          Transformation=trans
                          )]
    pars.write[is.na(Label),Label:=""]
    
    ft.write <- flextable(pars.write) |> autofit()

### smaller table for presentation
    pars.present <- pars.write[,.(Symbol,Label,"Estimate [CV] (RSE)"=Estimate,`95% CI`)]
    ft.present <- flextable(pars.present) |> autofit()
    
    list(partab_full=pars.full,
         partab_detail=ft.write,
         partab_present=ft.present)

}
