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
  
  if(FALSE) {
    
    ## ISAMPLE for NONMEM/SAEM setting options. 
    #TODO: this will fail if ISAMPLE preprocessing + burn-in iterations goes below 200 
    #TODO: a better way would be to check if the negative iterations re-start: iters[,ISAMPLE:=ifelse(any(lead(ITERATION)<ITERATION), 1, 0)

    iters[, REC := .I]

    # check for isample activity: a negative ITERATION followed by a lower one
    ## marks the end of an ISAMPLE block
    isample.end <- iters[shift(ITERATION, type = "lead") < ITERATION & ITERATION < 0,
                         .(parameter, iend = REC)]

    if (nrow(isample.end) > 0) {
      ## first row per parameter
      isample.start <- iters[, .(istart = REC[1]), by = parameter]
      isample.remove <- isample.start[isample.end, on = "parameter", nomatch = NULL]
      ## expand each start/end pair into the full range of rows
      isample.remove.reduce <- isample.remove[, .(REC = istart:iend),
                                              by = .(parameter, istart, iend)]
      ## remove ISAMPLE iteration rows from iters
      iters <- iters[!isample.remove.reduce, on = c("parameter", "REC")]
    }
  }

  
  ## ISAMPLE (NONMEM/SAEM): negative iterations restarting (next value lower)
  ## marks the end of an ISAMPLE pre-processing block. Drop all rows up to and
  ## including the last such restart within each parameter. No threshold on
  ## the number of iterations is needed.
  iters[, isample.end := ITERATION < 0 & shift(ITERATION, type = "lead") < ITERATION, by = label.by]
  iters[is.na(isample.end), isample.end := FALSE]
  iters <- iters[iters[, rev(cumsum(rev(isample.end))) == 0, by = label.by]$V1]
  iters[, isample.end := NULL]


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
  ## iters <- mergeLabel(iters,tab.label=pars,by=label.by,col.label="par.name")

  
  if("FIX" %in% colnames(iters)) {
    iters <- iters[is.na(FIX)|FIX!=1]
  } else {
    ## selecting parameters with more than one value - i.e. not fixed
    iters <- merge(iters[,uniqueN(value),by=parameter][V1>1,.(parameter)],iters,all.x=T)
  }
  
  
  #order by par.type
  ## iters[, par.type := factor(par.type, levels = c("THETA", "OMEGA", "SIGMA"))]
  ## iters[, label := factor(label, levels = arrange(iters,par.type,i) %>% pull(label) %>% unique())]
  ##   pars[,label := reorder2(get(col.label),match.order,i,j)]

  iters[, match.order := match(par.type, c("THETA", "OMEGA", "SIGMA", "OBJ"))]
  iters[,(col.label) := reorder2(get(col.label),match.order,i,j)]
  iters[,match.order:=NULL]

  
  p1 <- ggplot(iters,
               aes(ITERPLOT,value,colour=type.iter))+
    geom_line()+
    facet_wrap(col.label, scales="free")+
    labs(x="Iteration",y="",colour="")

  return(list(data=iters,plot=p1))
  ##return(iters)
}

