# Plot CWRES/IWRES vs time (after dose, after first dose) 
# 2025-04-07: do we want to include summarizeBy var?


##' @import data.table
##' @import ggplot2
##' @import NMdata
##' @export
##' 
##' @param .data dataset object to use for plotting. Must have all columns
##'   required for plotting
##' @param .file.mod the path to a completed NONMEM model which will be passed
##'   to `NMdata::NMscanData()` to read in the dataset
##' @param .ResCol the residuals column name (i.e. "RES", "CWRES", "NPDE",
##'   etc.). Will be plotted as the y-axis.
##' @param .TimeCol the time column name (i.e. "TIME", "AFRLT", "APRLT", "TAFD",
##'   etc.). Will be plotted as the x-axis.
##' @param .NomtimeCol nominal time column name. When this is non-NULL, this
##'   column will be used for grouping observations and plotting a median + 90%
##'   confidence interval for each nominal time point. Helpful when many
##'   observations occur at the same time point and are hard to interpret.
##' @param .AddSmooth TRUE/FALSE. If TRUE, will add a smooth line using method="loess"

plotResTime = function(
  .data=NULL,
  .file.mod = NULL,
  .ResCol = "CWRES",
  .TimeCol = "AFRLT",
  .NomtimeCol = NULL, # "NFRLT"
  .AddSmooth = FALSE
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
  } else if(!is.null(data[[.ResCol]]) && !is.numeric(data[[.ResCol]])){
    stop(paste0("`.ResCol`=",.ResCol, " is required to be numeric."))
  }
  if(is.null(data[[.TimeCol]])){
    stop(paste0("`.TimeCol`=",.TimeCol, " is required and not present in the dataset."))
  } else if(!is.null(data[[.TimeCol]]) && !is.numeric(data[[.TimeCol]])){
    stop(paste0("`.TimeCol`=",.TimeCol, " is required to be numeric."))
  }
  if(!is.null(.NomtimeCol) && is.null(data[[.NomtimeCol]])) {   
    stop(paste0("`.NomtimeCol`=",.NomtimeCol, " is required and not present in the dataset."))
  } else if(!is.null(.NomtimeCol) && !is.null(data[[.NomtimeCol]]) && !is.numeric(data[[.NomtimeCol]]) ){
    stop(paste0("`.NomtimeCol`=",.NomtimeCol, " is required to be numeric."))
  }

  # if we want to summarize by a variable to plot a pointrange/boxplot
  # this is usually nominal time.
  if(!is.null(.NomtimeCol)) {
    sumdata = tidytable::summarise(data,
      q10 = quantile(!!sym(.ResCol), probs=0.10),
      q50 = quantile(!!sym(.ResCol), probs=0.50),
      q90 = quantile(!!sym(.ResCol), probs=0.90),
         .by = c(!!sym(.NomtimeCol)))
  }
  
  p = 
    ggplot(data, aes(x=!!sym(.TimeCol), y=!!sym(.ResCol)))+
    geom_point(shape = 1, size = 3) + 
    geom_hline(yintercept = 0, color = "blue", linewidth = 1, linetype = 2) +
    coord_cartesian(ylim = c(-1.01*max(abs(data[[.ResCol]])), +1.01*max(abs(data[[.ResCol]])))) +
    { 
      if(!is.null(.NomtimeCol) && !is.null(sumdata)) {
        geom_pointrange(data = sumdata,
                        aes(x = !!sym(.NomtimeCol), ymin = q10, ymax = q90, y = q50),
                        color = "red", size = 0.2)
      } 
    } +
    { 
      if (.AddSmooth) geom_smooth(method = "loess", se = FALSE, linewidth = 1, col = "red") 
    } +
    theme_bw()
  return(p)
}

