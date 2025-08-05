# EtaPairsUpperTriangle.R

##' Gets pairwise data between all columns of data, then calculates correlation and p value of correlation to plot in upper triangle of eta pairs plot..
##' This function returns the dataset in a format that will be directly used to
##' plot the correlation statistics in the upper triangle of the pairs plot. The output
##' of this function gets sent to NMwork::PlotEtaPairs(). This function is taken with
##' minimal modifications from GGally:::uppertriangle().
##' @import magrittr
##' @import ggplot2
##' @export
##' 

EtaPairsUpperTriangle = 
  function(data, columns = 1:ncol(data), color = NULL, corMethod = "pearson") {
    # data = etas_time_constant
    # columns = which(colnames(etas_time_constant) %in% names(iiv.etas))
    # color=NULL
    data <- GGally:::upgrade_scatmat_data(data)
    data.choose <- data[columns]
    # why do  we need to check this again?
    dn <- data.choose[sapply(data.choose, is.numeric)]
    factor <- data[sapply(data, is.factor)]
    p <- ncol(dn)
    newdata <- NULL
    for (i in 1:p) {
      for (j in 1:p) {
        newdata <- rbind(
          newdata,
          cbind(
            dn[, i], dn[, j], i, j, colnames(dn)[i], colnames(dn)[j],
            min(dn[, i],na.rm = T) + 0.5 * (max(dn[, i],na.rm = T) - min(dn[, i],na.rm = T)),
            min(dn[, j],na.rm = T) + 0.5 * (max(dn[, j],na.rm = T) - min(dn[, j],na.rm = T))
            , factor
          )
        )
      }
    }
    colnames(newdata) <- c(
      "xvalue", "yvalue",
      "xslot", "yslot",
      "xlab", "ylab",
      "xcenter", "ycenter",
      colnames(factor)
    )
    
    rp <- data.frame(newdata, stringsAsFactors = TRUE)
    rp$xvalue <- suppressWarnings(as.numeric(as.character(rp$xvalue)))
    rp$yvalue <- suppressWarnings(as.numeric(as.character(rp$yvalue)))
    rp[[2]][rp[[3]] <= rp[[4]]] <- NA
    rp[[1]][rp[[3]] < rp[[4]]] <- NA
    
    if (is.null(color)) {
      rp.new <- rp[1:8]
    } else {
      colorcolumn <- rp[[which(colnames(rp) == color)]]
      rp.new <- cbind(rp[1:8], colorcolumn)
    }
    b <- rp.new[!is.na(rp.new$xvalue) & !is.na(rp.new$yvalue), ]
    b$xlab <- factor(b$xlab, levels = unique(b$xlab))
    b$ylab <- factor(b$ylab, levels = unique(b$ylab))
    if (is.null(color)) {
      data.cor <- b %>%
        # dplyr::group_by(xlab, ylab) %>%
        dplyr::summarise(
          rest = cor.test(xvalue, yvalue,
                       use = "pairwise.complete.obs",
                       method = "pearson"
          )$estimate,
          rpval = cor.test(xvalue, yvalue,
                          use = "pairwise.complete.obs",
                          method = "pearson"
          )$p.value,
          xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
          yvalue = min(yvalue) + 0.5 * (max(yvalue) - min(yvalue)),
          .by=c(xlab,ylab)
        ) %>% 
        dplyr::mutate(
          r = paste0("r=",round(rest ,2), "\np=", round(rpval,2))
        ) %>% 
        dplyr::select(-rest, -rpval)
      # if (identical(corMethod, "rsquare")) {
      #   data.cor$r <- data.cor$r^2
      # }
      # data.cor$r <- paste(round(data.cor$r, digits = 2))
      
      return(data.cor)
    } else {
      c <- b
      data.cor1 <- c %>%
        dplyr::summarise(
          rest = cor.test(xvalue, yvalue,
                          use = "pairwise.complete.obs",
                          method = "pearson"
          )$estimate,
          rpval = cor.test(xvalue, yvalue,
                           use = "pairwise.complete.obs",
                           method = "pearson"
          )$p.value,
          xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
          yvalue = min(yvalue) + 0.5 * (max(yvalue) - min(yvalue)),
          .by=c(xlab,ylab,colorcolumn)
        ) %>% 
        dplyr::mutate(
          r = paste0("r=",round(rest ,2), "\np=", round(rpval,2))
        ) %>% 
        dplyr::select(-rest, -rpval)
      # if (identical(corMethod, "rsquare")) {
      #   data.cor1$r <- data.cor1$r^2
      # }
      # data.cor1$r <- paste(round(data.cor1$r, digits = 2))
      
      
      n <- nrow(data.frame(unique(b$colorcolumn)))
      position <- b %>%
        dplyr::group_by(xlab, ylab) %>%
        dplyr::summarise(
          xvalue = min(xvalue) + 0.5 * (max(xvalue) - min(xvalue)),
          ymin = min(yvalue),
          ymax = max(yvalue),
          range = max(yvalue) - min(yvalue)
        )
      
      df <- data.frame()
      for (i in 1:nrow(position)) {
        for (j in 1:n) {
          row <- position[i, ]
          df <- rbind(df, cbind(row[, 3], (row[, 4] + row[, 6] * j / (n + 1))))
        }
      }
      data.cor <- cbind(data.cor1, df)
      colnames(data.cor) <- c("xlab", "ylab", "colorcolumn", "r", "xvalue", "yvalue")
      return(data.cor)
    }
  }
