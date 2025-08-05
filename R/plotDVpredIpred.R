# Plot DV vs PRED or DV vs IPRED 
# 2025-04-07: option to pass in dataset or model file. If large dataset, may
# only want to read it once to save time.

##' Plot DV vs PRED or IPRED
##' @import data.table
##' @import ggplot2
##' @importFrom NMdata NMscanData
##' @importFrom scales trans_format math_format
##' @export
##' @param .data dataset object to use for plotting. Must have all columns
##'   required for plotting
##' @param .file.mod the path to a completed NONMEM model which will be passed
##'   to `NMdata::NMscanData()` to read in the dataset
##' @param .dvcol NONMEM dependent variable column name (i.e. "DV" ). Will be
##'   plotted as the y-axis.
##' @param .predcol the population prediction column name (i.e. "PRED"). Will be
##'   plotted as the x-axis.
##' @param .AddSmooth TRUE/FALSE. If TRUE, will add a smooth line using
##'   method="loess"
##' @param .log TRUE/FALSE. If TRUE, will plot data on log-scale for both x and
##'   y axes


plotDVpredIpred <- function(
  .data=NULL,
  .file.mod = NULL,
  .dvcol = "DV",
  .predcol = "PRED",
  .AddSmooth = TRUE,
  .log = FALSE
  ){ 
  
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
  if(is.null(data[[.dvcol]])){
    stop(paste0("`.dvcol`=",.dvcol, " is required and not present in the dataset."))
  }
  if(is.null(data[[.predcol]])){
    stop(paste0("`.predcol`=",.predcol, " is required and not present in the dataset."))
  }
  
  lowest = min(c(data[[.dvcol]],data[[.predcol]]))
  highest =  max(c(data[[.dvcol]], data[[.predcol]]))
  low.lim = ifelse(lowest>0, lowest*0.9, lowest*1.1)
  high.lim = ifelse(highest>0, highest*1.1, highest*0.9)
  lims = c(low.lim, high.lim)
  
  if(.log & any(c(lowest,highest)<=0)) {
    stop(paste0("`.log` cannot equal TRUE if there are data <= 0"))
  }

  p = ggplot(data, aes(x = !!sym(.predcol), y = !!sym(.dvcol))) +
    geom_point(shape = 1, size = 3) +
    geom_abline( intercept = 0, slope = 1, color = "blue", linewidth = 1, linetype = 2) +
    { 
      if (.AddSmooth) geom_smooth( aes(group = NULL), method = "loess", color = "red", se = FALSE, linewidth = 1 ) 
    } +
    # geom_smooth( method = "loess", color = "red", aes(group = NULL), se = FALSE, linewidth = 1) +
    theme_bw()

  if (.log) {
    # https://stackoverflow.com/questions/30179442/plotting-minor-breaks-on-a-log-scale-with-ggplot
    log10_minor_break = function (...){
      function(x) {
        minx         = floor(min(log10(x), na.rm=T))-1;
        maxx         = ceiling(max(log10(x), na.rm=T))+1;
        n_major      = length(seq(minx, maxx, by=1));
        major_breaks = seq(minx, maxx, by=1)
        minor_breaks =
          rep(log10(seq(1, 9, by=1)), times = n_major)+
          rep(major_breaks, each = 9)
        return(10^(minor_breaks))
      }
    }
    # lims = c(min(c(data[[.dvcol]],data[[.predcol]])), 1.1 * max(c(data[[.dvcol]], data[[.predcol]])))
    p = p +
      coord_cartesian(xlim = lims, ylim = lims) +
      scale_x_log10(minor_breaks = log10_minor_break(),
                    labels = scales::trans_format('log10', scales::math_format(10 ^ .x))) +
      scale_y_log10(minor_breaks = log10_minor_break(),
                    labels = scales::trans_format('log10', scales::math_format(10 ^ .x))) +
      annotation_logticks(
        sides = "lb",
        short = unit(0.02, "in"),
        mid = unit(0.04, "in"),
        long = unit(0.05, "in"),
        linewidth = 0.25
      )
  } else {
    # lims = c(1.1*min(c(data[[.dvcol]],data[[.predcol]])), 1.1 * max(c(data[[.dvcol]], data[[.predcol]])))
    p = p + coord_cartesian(xlim = lims, ylim = lims)
  }  
  return(p)
}
  