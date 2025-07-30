# EtaPairsScatMat.R


##' Eta pairs scatterplot matrix lower triangle and diagonal elements.  This
##' function takes (1) the time-constant etas dataset, and (2) the output of
##' NMwork:::EtaPairsLowerTriangle() to create the lower triangle and diagonal
##' of the eta pairs plot. The output of this function gets sent to
##' NMwork::PlotEtaPairs().
##' Taken with minimal modifications from GGally::scatmat().
##' @import magrittr
##' @import ggplot2
##' @export
EtaPairsScatMat <- 
  function(data, columns = 1:ncol(data), color = NULL, alpha = 0.6, shape=1, size=1) {
    # data = etas_time_constant
    # columns = which(colnames(etas_time_constant) %in% names(iiv.etas))
    # color=NULL
    # alpha=1
    # shape=1
    # size=0.5
    # data <- upgrade_scatmat_data(data)
    data.choose <- data[columns]
    dn <- data.choose[sapply(data.choose, is.numeric)]
    if (ncol(dn) == 0) {
      stop("All of your variables are factors. Need numeric variables to make scatterplot matrix.")
    }
    
    ltdata.new <- EtaPairsLowerTriangle(data, columns = columns, color = color)
    ## set up the plot
    r <- ggplot(
      ltdata.new,
      mapping = aes(x = !!as.name("xvalue"), y = !!as.name("yvalue"))
    ) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size=7)
      ) +
      facet_grid(ylab ~ xlab, scales = "free") +
      theme(aspect.ratio = 1, panel.grid.minor = element_blank())
    if (is.null(color)) {
      ## b/w version
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(
          xlab = names(dn)[i], ylab = names(dn)[i],
          x = dn[, i], stringsAsFactors = TRUE
        )
      }))
      for (m in 1:ncol(dn)) {
        j <- subset(densities, xlab == names(dn)[m])
        r <- r + stat_density(
          aes(
            x = !!as.name("x"),
            y = after_stat(scaled) * diff(range(!!as.name("x"))) + min(!!as.name("x")) # nolint
          ),
          data = j, position = "identity", geom = "line", color = "black"
        )
      }
      ## add b/w points
      r <-
        r + geom_point(
          alpha = alpha,
          na.rm = TRUE,
          shape = shape,
          size = size
        ) +
        ## add line
        geom_smooth(method = "lm",
                    formula = y ~ x,
                    color = "red")
      return(r)
    } else {
      ## do the colored version
      densities <- do.call("rbind", lapply(1:ncol(dn), function(i) {
        data.frame(
          xlab = names(dn)[i], ylab = names(dn)[i],
          x = dn[, i], colorcolumn = data[, which(colnames(data) == color)],
          stringsAsFactors = TRUE
        )
      }))
      for (m in 1:ncol(dn)) {
        j <- subset(densities, xlab == names(dn)[m])
        r <- r +
          # r is the facet grid plot
          stat_density(
            aes(
              x = !!as.name("x"),
              y = after_stat(scaled) * diff(range(!!as.name("x"))) + min(!!as.name("x")),
              colour = !!as.name("colorcolumn")
            ),
            data = j,
            position = "identity",
            geom = "line"
          )
      }
      ## add color points
      r <- r +
        geom_point(
          data = ltdata.new,
          aes(colour = !!as.name("colorcolumn")),
          alpha = alpha,
          shape = shape, 
          size = size,
          na.rm = TRUE
        ) +
        ## add line
        geom_smooth(method = "lm",
                    formula = y ~ x,
                    color = "red")
      return(r)
    }
  }