# Plot DV vs PRED or DV vs IPRED 
# 2025-04-07: option to pass in dataset or model file. If large dataset, may
# only want to read it once to save time.

##' @import data.table
##' @import ggplot2
##' @import NMdata
##' @export

plotDVpredIpred <- function(
  .data=NULL,
  .file.mod = NULL,
  .dvcol = "DV",
  .predcol = "PRED",
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

  p = ggplot(data, aes(x = !!sym(.predcol), y = !!sym(.dvcol))) +
    geom_point(shape = 1, size = 3) +
    geom_abline( intercept = 0, slope = 1, color = "blue", linewidth = 1, linetype = 2) +
    geom_smooth( method = "loess", color = "red", aes(group = NULL), se = FALSE, linewidth = 1) +
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
    lims = c(min(c(data[[.dvcol]],data[[.predcol]])), 1.1 * max(c(data[[.dvcol]], data[[.predcol]])))
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
    lims = c(0, 1.1 * max(c(data[[.dvcol]], data[[.predcol]])))
    p = p + coord_cartesian(xlim = lims, ylim = lims)
  }  
  return(p)
}
  