##' Create a pdf or print to console the output of PsN 'sumo' summary of model diagnostics
##'
##' @param file.lst Control stream. If possible, it is recommended to
##'     use output control stream. You can also use input control
##'     stream.
##' @param dir.diag location to save pdf file of summary output
##' @param within.rmd if this command will be run in an rmd document, don't save pdf just print the output and put it in an Rmd chunk
##' @details This is additional details
##' 
##'
 
##' @import data.table
##' @import NMdata
##' @importFrom rmarkdown render
##' @export
sumoPrint <- function(file.lst,dir.diag=dirname(file.lst), within.rmd = TRUE){
  require(rmarkdown)
  file.lst <- fnExtension(file.lst,"lst")
  
  sumotemp <- tempfile()
  system(sprintf("sumo %s > %s",file.lst,sumotemp))
  mytext <- readLines(sumotemp) 
  if(within.rmd==FALSE){
    mytext <- c("```",mytext,"```")
    cat(mytext, sep="  \n", file = file.path(dir.diag,"sumores.Rmd"))
    suppressMessages(render(file.path(dir.diag,"sumores.Rmd"), output_format=pdf_document(),output_file=file.path(dir.diag,"sumores.pdf")))
  } else {
    cat(mytext, sep="  \n")
  }
  NULL
}
