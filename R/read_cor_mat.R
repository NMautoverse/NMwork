##' read list of correlation matrices from .cor file. takes the path to the .cor
##' file as an argument. output is a list with length equal to the number of
##' tables output in the .cor file. Use: when more than one table is listed in
##' .cor file and you want all of them
##' @param file.lst NONMEM model output file, must be completed run
##' @param tableno which correlation matrix to output in case there are
##'   multiple. tableno must be either one of the character strings min, max,
##'   all or an integer greater than zero
##' @import magrittr
##' @importFrom readr read_lines
##' @importFrom NMdata fnExtension
##' @importFrom stringr str_detect str_trim str_split str_extract
##' @importFrom dplyr lead mutate across everything
##' @importFrom purrr map2 map
##' @importFrom tibble tibble
##' 
##' @export
read_cor_mat <-  function(
  file.lst,
  tableno = "max"
) {
  
  file_cor = NMdata::fnExtension(file.lst, ".cor")
  if(!file.exists(file_cor)) {
    stop(paste0("Error: File ",file_cor," does not exist."))
  }
  # since we often have two table files for SAEM and IMP, make matrices for each run separately. optionally return only the final cormat. 
  ext <- readr::read_lines(file_cor)
  split_pattern <- "TABLE NO.*"
  indices <- which(stringr::str_detect(ext, split_pattern))
  tab_starts <- indices + 1
  tab_ends <- c(dplyr::lead(indices) %>% .[!is.na(.)] - 1, length(ext))
  tabs <- purrr::map2(.x = tab_starts, .y = tab_ends, .f = function(.x, .y) {
    ext[.x:.y] %>%
      tibble::tibble(.) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_trim(.x))) %>%
      purrr::map(., ~ stringr::str_split(.x, pattern = "[[:whitespace:]]{1,}")) %>%
      unlist(recursive = F) %>%
      do.call(rbind, .)
  })
  tabs <- purrr::map(tabs, function(.x) {
    dims <- .x[1,-1]
    mat <- .x[-1, -1]
    mat = apply(mat, 1, as.numeric)
    dimnames(mat) = list(dims, dims)
    return(mat)
  })
  
  extnames <- ext[indices] %>% stringr::str_extract(., pattern = "TABLE NO\\.[[:whitespace:]]{1,}\\d+\\:[([[:alnum:]]|[[:space:]]|\\-)]*\\:")
  
  names(tabs) <- extnames
  
  if(is.numeric(tableno)){tabs = tabs[[tableno]]}
  if(!is.null(tableno)&tableno=="max"){tabs = tabs[[length(tabs)]]}
  if(!is.null(tableno)&tableno=="min"){tabs = tabs[[1]]}
  if(!is.null(tableno)&tableno=="all"){tabs = tabs}
  
  return(tabs)
}



