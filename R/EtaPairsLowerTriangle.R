#EtaPairsLowerTriangle.R

##' Gets pairwise data between all columns of data, then provide organizing
##' columns to allow the data to be plotted in a pairwise way in a matrix plot.
##' This function returns the dataset in a format that will be directly used to
##' plot the scatterplot in the lower triangle of the pairs plot. . The output
##' of this function gets sent to NMwork:::EtaPairsScatMat.R (slightly modified
##' version of GGally::scatmat), and then to NMwork::PlotEtaPairs(). Taken with
##' minimal modifications from GGally:::lowertriangle().
##' @import magrittr
##' @import ggplot2
##' @export
##' 

EtaPairsLowerTriangle <-
  function(data, columns = 1:ncol(data), color = NULL) {
    # data = etas_time_constant
    # columns = which(colnames(etas_time_constant) %in% names(iiv.etas))
    
    data <- GGally:::upgrade_scatmat_data(data)
    data.choose <- data[columns]
    dn <- data.choose[sapply(data.choose, is.numeric)]
    factor <- data[sapply(data, is.factor)]
    p <- ncol(dn)
    q <- nrow(dn)
    newdata <- as.data.frame(matrix(NA, nrow = q * p * p, ncol = 6 + ncol(factor)), stringsAsFactors = FALSE)
    newdata[5:6] <- as.data.frame(matrix("", nrow = q * p * p, ncol = 2), stringsAsFactors = FALSE)
    
    r <- 1
    for (i in 1:p) {
      for (j in 1:p) {
        newdata[r:(r + q - 1), 1:6] <- cbind(dn[[i]], dn[[j]], i, j, colnames(dn)[i], colnames(dn)[j])
        r <- r + q
      }
    }
    
    if (ncol(newdata) > 6) {
      newdata[7:ncol(newdata)] <- factor
    }
    colnames(newdata) <- c("xvalue", "yvalue", "xslot", "yslot", "xlab", "ylab", colnames(factor))
    
    rp <- data.frame(newdata)
    
    
    rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
    rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))
    rp$xslot <- suppressWarnings(as.numeric(as.character(rp$xslot)))
    rp$yslot <- suppressWarnings(as.numeric(as.character(rp$yslot)))
    rp$xlab <- factor(rp$xlab, levels = unique(rp$xlab))
    rp$ylab <- factor(rp$ylab, levels = unique(rp$ylab))
    
    rp[[2]][rp[[3]] >= rp[[4]]] <- NA
    rp[[1]][rp[[3]] > rp[[4]]] <- NA
    
    if (is.null(color)) {
      rp.new <- rp[1:6]
    } else {
      colorcolumn <- rp[[which(colnames(rp) == color)]]
      rp.new <- cbind(rp[1:6], colorcolumn)
    }
    return(rp.new)
  }