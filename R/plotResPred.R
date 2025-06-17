# Plot CWRES/IWRES vs population predictions (PRED) 
 # 2025-04-08: adding 'tidytable' package to imports -- this package is a
# tidyverse interface to data.table, it seeks to provide the performance
# advantages of data.table but with the readability of tidyverse

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
##' @param .PredCol the population prediction column name (i.e. "PRED"). Will be plotted as the x-axis.
##' @param .AddSmooth TRUE/FALSE. If TRUE, will add a smooth line using method="loess"

plotResPred = function(
  .data=NULL,
  .file.mod = NULL,
  .ResCol = "CWRES",
  .PredCol = "PRED",
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
  if(is.null(data[[.PredCol]])){
    stop(paste0("`.PredCol`=",.PredCol, " is required and not present in the dataset."))
  } else if(!is.null(data[[.PredCol]]) && !is.numeric(data[[.PredCol]])){
    stop(paste0("`.PredCol`=",.PredCol, " is required to be numeric."))
  }

  p = 
    ggplot(data, aes(x=!!sym(.PredCol), y=!!sym(.ResCol)))+
    geom_point(shape = 1, size = 3) + 
    geom_hline(yintercept = 0, color = "blue", linewidth = 1, linetype = 2) +
    coord_cartesian(ylim = c(-1.01*max(abs(data[[.ResCol]])), +1.01*max(abs(data[[.ResCol]])))) +
    { 
      if (.AddSmooth) geom_smooth(method = "loess", se = FALSE, linewidth = 1, col = "red") 
    } +
    theme_bw()
  return(p)
}

