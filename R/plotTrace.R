## 2025-01-17. changed so it doesn't rely on a path to a parameter table. But now it wants a pars object containing parameters and symbol. Would be good if the name of column used for symbol could be specified.

## cutoffs for separation of type.iter should be added

##' Plot parameter estimation iterations
##'
##' @param file.mod NONMEM model file, must be completed run
##' @param pars a parameter table to map NONMEM parameters to their labels
##' @param label.by a column in the parameter table which will be used for merging the parameter table and the parameter labels in the .ext file, as read by `NMdata::NMreadExt()`. usually this column is 'parameter' and has values such as 'THETA1', 'THETA2', .... , 'OMEGA(1,1)', 'OMEGA(2,2)', ... 'SIGMA(1,1)',  etc.
##' @param col.label the column in the parameter table which we will use for labeling the plot panels.
##' @param ... Passed to `NMdata::NMreadExt()`. 
##' @import data.table
##' @import ggplot2
##' @importFrom NMdata NMreadExt
##' @importFrom dplyr slice_head inner_join anti_join arrange pull lead
##' @importFrom tibble tibble
##' @importFrom purrr map list_rbind
##' @export

plotTrace <- function(file.mod,pars=NULL,label.by="parameter",col.label=NULL,...){

    ## model <- modelPaths(file.mod)
    
    ## file.pars.rds <- sprintf("diagnostics/models/%s/partab_full_%s.rds",model$run,model$runno)
    ## pars <- readRDS(file.pars.rds)

    iters <- NMreadExt(file=file.mod,return="iterations",as.fun="data.table",...)
    
    
    ## ISAMPLE for NONMEM/SAEM setting options. 
    #TODO: this will fail if ISAMPLE preprocessing + burn-in iterations goes below 200 
    #TODO: a better way would be to check if the negative iterations re-start: iters[,ISAMPLE:=ifelse(any(lead(ITERATION)<ITERATION), 1, 0)
    iters[, REC := .I]
    # check for isample activity
    if (nrow(iters[dplyr::lead(ITERATION) < ITERATION & ITERATION < 0]) > 0) {
        isample.end = iters[(dplyr::lead(ITERATION) < ITERATION & ITERATION < 0)]
        isample.start = slice_head(iters, n = 1, by = parameter)
        isample.remove = inner_join(isample.start[, .(parameter, istart = REC)], 
                                           isample.end[, .(parameter, iend =  REC)])
        isample.remove.reduce = map(.x = isample.remove$parameter, function(.x)
            tibble(
                parameter = .x,
                REC = isample.remove[parameter == .x]$istart:isample.remove[parameter == .x]$iend
            )) %>% list_rbind() %>% as.data.table()
        # remove ISAMPLE iteration rows from iters.
        iters = anti_join(iters, isample.remove.reduce)
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
    
    #order by par.type
    iters[, par.type := factor(par.type, levels = c("THETA", "OMEGA", "SIGMA"))]
    iters[, label := factor(label, levels = arrange(iters,par.type,i) %>% pull(label) %>% unique())]
    
    p1 <- ggplot(iters,
                 aes(ITERPLOT,value,colour=type.iter))+
        geom_line()+
        facet_wrap(~label, scales="free")+
        labs(x="Iteration",y="",colour="")

        return(list(data=iters,plot=p1))
    ##return(iters)
}

