# EtaPairsScatMat.R


##' Plots Eta pairs scatterplot matrix with lower triangle being a scatterplot,
##' diagonal being a density plot, and upper triangle being text of the
##' correlation and p-value.  This function takes (1) the time-constant etas
##' dataset, and (2) the output of NMwork:::EtaPairsUpperTriangle(), (3) the
##' output of NMwork:::EtaPairsScatMat() (and NMwork::EtaPairsLowerTriangle), to
##' plot a full eta pairs matrix plot. Taken with minimal modifications from
##' GGally::ggscatmat().
##' @import magrittr
##' @import ggplot2
##' @export
##' 
PlotEtaPairs <- 
  function(data, columns = 1:ncol(data), color = NULL, alpha = 1, shape = 1, size=0.7, corMethod = "pearson") {
    # data =etas_time_constant
    # columns = which(colnames(etas_time_constant) %in% names(iiv.etas))
    # color = NULL
    # alpha = 1
    # shape = 1
    # size=0.7
    # corMethod = "pearson"
    ## if 'color' is not a factor, mold it into one
    if (!is.null(color)) {
      if (is.null(data[[color]])) {
        stop(paste0("Non-existent column <", color, "> requested"))
      }
      data[[color]] <- as.factor(data[[color]])
    }
    ## do we really need this next line?
    # data <- GGally:::upgrade_scatmat_data(data)
    data.choose <- data[columns]
    dn <- data.choose[sapply(data.choose, is.numeric)]
    
    if (ncol(dn) == 0) {
      stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
    }
    if (ncol(dn) < 2) {
      stop("Not enough numeric variables to make a scatter plot matrix")
    }
    
    a <- EtaPairsUpperTriangle(data, columns = columns, color = color, corMethod = corMethod)
    if (is.null(color)) {
      plot <- EtaPairsScatMat(data, columns = columns, alpha = alpha, shape=shape, size=size) +
        geom_text(data = a, aes(label = !!as.name("r")), colour = "black")
    } else {
      plot <- EtaPairsScatMat(data, columns = columns, color = color, alpha = alpha, shape=shape, size=size) +
        geom_text(data = a, aes(label = !!as.name("r"), color = !!as.name("colorcolumn"))) + labs(color = color)
    }
    is.factor.or.character <- function(x) {
      is.factor(x) | is.character(x)
    }
    factor <- data.choose[sapply(data.choose, is.factor.or.character)]
    if (ncol(factor) == 0) {
      return(plot)
    } else {
      warning("Factor variables are omitted in plot")
      return(plot)
    }
  }
