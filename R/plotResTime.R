# Plot CWRES/IWRES vs time (after dose, after first dose) 
# 2025-04-07: do we want to include summarizeBy var?

##' @import data.table
##' @import ggplot2
##' @import NMdata
##' @export
##' 
##' 
plotResTime = function(
  .data=NULL,
  .file.mod = NULL,
  .ResCol = "CWRES",
  .TimeCol = "AFRLT",
  .SummarizeBy = NULL # "NFRLT" 
) {
  # checks that we have a dataset
  if(!is.null(.data)){
    data = .data
  }
  if(is.null(.data) && !is.null(.file.mod)) {
    data = NMscanData(file = .file.mod, as.fun = "data.table")
    # filter to observation rows
    data = data[EVID==0]
  }
  if(is.null(.data) && is.null(.file.mod)){
    stop("Must either provide dataset (`.data`) or path to completed NONMEM model control stream (`.file.mod`)")
  }
  
  # check for variables we need:
  if(is.null(data[[.ResCol]])){
    stop(paste0("`.ResCol`=",.ResCol, " is required and not present in the dataset."))
    if(!is.numeric(data[[.ResCol]])){
      stop(paste0("`.ResCol`=",.ResCol, " is required to be numeric."))
    }
  }
  if(is.null(data[[.TimeCol]])){
    stop(paste0("`.TimeCol`=",.TimeCol, " is required and not present in the dataset."))
    if(!is.numeric(data[[.TimeCol]])){
      stop(paste0("`.TimeCol`=",.TimeCol, " is required to be numeric."))
    }
  }
  # check for correct data type:
  
  
  # if we want to summarize by a variable:
  if(!is.null(.SummarizeBy) && !is.null(data[[.SummarizeBy]])) {
    # data[, `:=` (q5 = quantile(!!sym(.ResCol), probs=0.05),
    #              q50 = quantile(!!sym(.ResCol), probs=0.50),
    #              q95 = quantile(!!sym(.ResCol), probs=0.95)), 
    #      by = .SummarizeBy]
    sumdata = dplyr::summarise(data,
      q10 = quantile(!!sym(.ResCol), probs=0.10),
      q50 = quantile(!!sym(.ResCol), probs=0.50),
      q90 = quantile(!!sym(.ResCol), probs=0.90), 
         .by = c(!!sym(.SummarizeBy)))
    # return(sumdata)
  }
  
  p = 
    ggplot(data, aes(x=!!sym(.TimeCol), y=!!sym(.ResCol)))+
    geom_point(shape = 1, size = 3) + 
    geom_hline(yintercept = 0, color = "blue", linewidth = 1, linetype = 2) +
    coord_cartesian(ylim = c(-1.01*max(abs(data[[.ResCol]])), +1.01*max(abs(data[[.ResCol]])))) +
  { if(!is.null(.SummarizeBy)&&!is.null(sumdata[["q50"]])){
    geom_pointrange(data = sumdata, aes(x = !!sym(.SummarizeBy), ymin = q10, ymax = q90, y = q50), 
    color = "red", size = 0.2)
  } } +
    theme_bw()
  
  return(p)
  
}

