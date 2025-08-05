##' Plot correlation matrix heatmap from completed NONMEM run This function
##' differs from `plotEstCorr` in that it will plot a full matrix with diagonals
##' representing standard error (rather than half matrix with no diagonals).
##' Requires successful covariance step in NONMEM.
##' @param file.lst NONMEM model output file, must be completed run
##' @param pars a parameter table to map NONMEM parameters to their labels
##' @param merge.pars.ext.by a common column in the parameter table and .ext file which will be used for merging the parameter table and the parameter labels in the .ext file, as read by `NMdata::NMreadExt()`. usually this column is 'parameter' and has values such as 'THETA1', 'THETA2', .... , 'OMEGA(1,1)', 'OMEGA(2,2)', ... 'SIGMA(1,1)',  etc.
##' @param col.label the column in the parameter table which we will use for labeling the plot panels.
##' @param ... Passed to `NMdata::NMreadExt()`.  
##' @import ggplot2
##' @import magrittr
##' @importFrom NMdata fnExtension NMreadCov mat2dt NMreadExt mergeCheck cc
##' @importFrom stringr str_detect str_trim str_split str_extract
##' @importFrom dplyr filter pull transmute mutate select
##' @importFrom tidyr pivot_longer
##' @importFrom forcats fct_relevel
##' @importFrom purrr map2 map
##' @importFrom tibble tibble as_tibble
##' @importFrom pals parula
##' @export
plotCorrelationHeatmap <- function(
  file.lst,
  pars = NULL,
  merge.pars.ext.by = "parameter",
  col.label = NULL, 
  tableno = "max",
  ...
) {
  # file.lst = "models/pk/036/036.lst"
  # 
  # partab <- createParameterTable(
  #   file.lst,
  #   args.ParsText = list(
  #     format = "%init;%symbol;%trans;%idx;%panel;%label;%unit",
  #     format.omega = "%init;%symbol;%trans;%idx;%panel;%label;%unit",
  #     format.sigma = "%init;%symbol;%trans;%idx;%panel;%label;%unit"
  #   )
  # ) %>%
  #   dplyr::mutate(par.label.symbol = paste0(par.name, " - ", symbol))
  # 
  
  # col.label = "par.label.symbol"
  # merge.pars.ext.by = "parameter"
  # pars = NULL
  # tableno = "max"

  # can only plot one matrix/table at a time, so if returned more than one, take the first element of the list
  cormat = read_cor_mat(file.lst = file.lst, tableno = tableno)
  ext = NMdata::NMreadExt(file.lst, return = "pars", tableno=tableno)
    # read_ext_tables(file.lst = file.lst, tableno = tableno)[[1]]
  
  non_fixed = ext %>% dplyr::filter(FIX==0) %>% dplyr::pull(parameter)
  cordf = 
    cbind(tibble::tibble(PARAM1 = rownames(cormat)), tibble::as_tibble(cormat)) %>% 
    tidyr::pivot_longer(-PARAM1) %>% 
    dplyr::transmute(PARAM1,PARAM2=name,value) %>% 
    dplyr::filter(PARAM1 %in% non_fixed, PARAM2 %in% non_fixed)
  
  if (!is.null(pars) & !is.null(col.label)) {
    pars = 
      pars %>% 
      dplyr::transmute(symbol =  get(col.label), PARAM1 = parameter, PARAM2 = parameter, i, j, par.type)

  } else {
    pars = ext %>% 
      dplyr::filter(FIX == 0) %>%
      dplyr::transmute(symbol = par.name, PARAM1 = parameter, PARAM2 = parameter, i, j, par.type)
  }
  
  cordf2 <-
    dplyr::inner_join(cordf, pars %>% dplyr::select(-PARAM2), by = "PARAM1") %>%
    dplyr::rename(symbol1 = symbol) %>%
    dplyr::left_join(pars %>% dplyr::select(-c(PARAM1, i, j, par.type)), by = "PARAM2") %>%
    dplyr::rename(symbol2 = symbol) %>%
    dplyr::mutate(par.type = forcats::fct_relevel(par.type, "THETA", "OMEGA","SIGMA")) %>% 
    dplyr::arrange(par.type, i, j) 
  
  # parameter order:
  symbol.ord <- pars$symbol
  
  plotdat <-
    cordf2 %>%
    dplyr::mutate(value = ifelse(PARAM1==PARAM2, NA_real_, value)) %>%
    dplyr::mutate(value = abs(value)) %>%
    dplyr::mutate(discrete = cut(value, breaks = c(0, 0.3, 0.6, 0.8, 0.9, 0.95, 1))) %>%
    dplyr::mutate(
      symbol1 = factor(symbol1, levels = symbol.ord),
      symbol2 = factor(symbol2, levels = symbol.ord)
    )
  
  plot <-
    ggplot2::ggplot(data = plotdat, aes(x = symbol1, y = symbol2, fill = discrete)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_manual(
      values = pals::parula(n = length(levels(plotdat$discrete))),
      breaks = levels(plotdat$discrete),
      name = "Correlation\n(Absolute value)"
    ) +
    ggplot2::theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.title = element_blank(),
      legend.position = "top"
    ) +
    ggplot2::guides(fill = guide_legend(ncol = 2))
  return(plot)
}
