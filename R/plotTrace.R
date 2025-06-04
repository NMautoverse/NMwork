## 2025-01-17. changed so it doesn't rely on a path to a parameter table. But now it wants a pars object containing parameters and symbol. Would be good if the name of column used for symbol could be specified.

## cutoffs for separation of type.iter should be added

##' @import data.table
##' @import ggplot2
##' @export

plotTrace <- function(file.mod,pars=NULL,label.by="parameter",col.label=NULL){

    ## model <- modelPaths(file.mod)
    
    ## file.pars.rds <- sprintf("diagnostics/models/%s/partab_full_%s.rds",model$run,model$runno)
    ## pars <- readRDS(file.pars.rds)

    iters <- NMreadExt(file=file.mod,return="iterations",tableno=1,as.fun="data.table")
    
    
    ## ISAMPLE for NONMEM/SAEM setting options. 
    #TODO: this will fail if ISAMPLE preprocessing + burn-in iterations goes below 200 
    #TODO: a better way would be to check if the negative iterations re-start: iters[,ISAMPLE:=ifelse(any(lead(ITERATION)<ITERATION), 1, 0)
    iters[, REC := .I]
    # check for isample activity
    if (nrow(iters[lead(ITERATION) < ITERATION & ITERATION < 0]) > 0) {
        isample.end = iters[(lead(ITERATION) < ITERATION & ITERATION < 0)]
        isample.start = dplyr::slice_head(iters, n = 1, by = parameter)
        isample.remove = dplyr::inner_join(isample.start[, .(parameter, istart = REC)], 
                                           isample.end[, .(parameter, iend =  REC)])
        isample.remove.reduce = purrr::map(.x = isample.remove$parameter, function(.x)
            tibble(
                parameter = .x,
                REC = isample.remove[parameter == .x]$istart:isample.remove[parameter == .x]$iend
            )) %>% purrr::list_rbind() %>% as.data.table()
        # remove ISAMPLE iteration rows from iters.
        iters = dplyr::anti_join(iters, isample.remove.reduce)
    }
    # iters <- iters[ITERATION<(-200)|ITERATION>0]
    iters[,ITERPLOT:=ITERATION]
    iters[ITERATION<0,ITERPLOT:=ITERATION-max(ITERATION)-1]
    iters[ITERATION<0,type.iter:="Burn-in"]
    iters[ITERATION>=0,type.iter:="SAEM"]


    ## if(is.null(pars)){
    ##     iters.plot <- iters
    ##     iters.plot[,symbol2:=parameter]
    ## } else {
    ##     iters <- mergeCheck(iters,pars[,.(parameter,symbol)],by="parameter",all.x=T,quiet=T)
    ##     iters[,symbol2:=paste(par.name,symbol)]
    ##     ## not exactly sure about this step
    ##     iters.plot <- iters[!is.na(symbol)&table.step=="SAEM"]
    ## }
    
    iters <- mergeLabel(iters,tab.label=pars,by=label.by,col.label=col.label)

    
    if("FIX" %in% colnames(iters)) {
        iters <- iters[FIX!=1]
    } else {
        ## selecting parameters with more than one value - i.e. not fixed
        iters <- merge(iters[,uniqueN(value),by=parameter][V1>1,.(parameter)],iters,all.x=T)
    }
    
    p1 <- ggplot(iters,
                 aes(ITERPLOT,value,colour=type.iter))+
        geom_line()+
        facet_wrap(~label, scales="free")+
        labs(x="Iteration",y="",colour="")

    return(list(parTrace=p1))
}
