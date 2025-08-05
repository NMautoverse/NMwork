
##' Plot Inidividual profiles observed (DV) and predicted (PRED and IPRED) 
##' @param .data dataset to use for plotting
##' @param .timecol  column to use for x-axis (TIME)
##' @param .dvcol  column to use for y-axis for observations (DV)
##' @param .predcol  column to use for y-axis for population predictions (PRED)
##' @param .ipredcol  column to use for y-axis for individual predictions (PRED)
##' @param .log  should we use a logged y-axis?
##' 
##' @import data.table
##' @import ggplot2
##' @import magrittr
##' @importFrom dplyr filter
##' @importFrom NMdata NMscanData
##' @export

plotIndivProfilesDvPredIpred <- function(
  .data=NULL,
  .timecol = "TIME",
  .dvcol = "DV",
  .predcol = "PRED",
  .ipredcol = "IPRED",
  .log = FALSE
) {
  if(!is.null(.data)){
    data = .data
  }

  if(is.null(.data)){
    stop("Must provide dataset (`.data`)")
  }
  
  # check for variables we need:
  if(is.null(data[[.dvcol]])){
    stop(paste0("`.dvcol`=",.dvcol, " is required and not present in the dataset."))
  }
  if(is.null(data[[.predcol]])){
    stop(paste0("`.predcol`=",.predcol, " is required and not present in the dataset."))
  }
  if(is.null(data[[.ipredcol]])){
    stop(paste0("`.ipredcol`=",.ipredcol, " is required and not present in the dataset."))
  }
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
  
  # filter to observation rows
  data_obs = data %>% dplyr::filter(EVID==0)
  data_dos = data %>% dplyr::filter(EVID==1)
  
  p = 
    ggplot() +
    geom_point(data = data_obs, aes(x=!!sym(.timecol), !!sym(.dvcol), color="Observations"), shape=1, size=2) +
    geom_line(data = data_obs,aes(x=!!sym(.timecol), y=!!sym(.ipredcol), color="Individual prediction")) +
    geom_line(data = data_obs,aes(x=!!sym(.timecol), y=!!sym(.predcol), color="Population prediction")) +
    scale_colour_manual(name="", values=c(`Population prediction`="blue",
                                          `Individual prediction`="red",
                                          `Observations`="black"))+
    geom_rug(data = data_dos,aes(x=!!sym(.timecol)))
  if (.log) {
    p = p +
      scale_y_log10(minor_breaks = log10_minor_break()) +
      annotation_logticks(
        sides = "l",
        short = unit(0.02, "in"),
        mid = unit(0.04, "in"),
        long = unit(0.05, "in"),
        linewidth = 0.25
      )
  }

  return(p)
}
