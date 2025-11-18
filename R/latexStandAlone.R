##' @import NMdata
##' @import NMsim
##' @importFrom tinytex latexmk

### todo: usepackage must support arguments. How about package:arg1,arg2 becomes \usepackage[arg1,arg2]{package}?
## when done, move geometry to printParameterTable

latexStandAlone <- function(lines.ltx,file.pdf,usepackage=NULL){


    ### convert say "geometry:margin=1in" to "\\usepackage[margin=1in]{geometry}"
    dt.usepkg <- data.table(string=gsub(" ","",usepackage))
    dt.usepkg[!grepl(":",string),pkg:=string]
    dt.usepkg[grepl(":",string),pkg:=sub(":.*$","",string)]
    dt.usepkg[grepl(":",string),args:=sub("^[^:]*:","",string)]
    dt.usepkg[,row:=.I]
    strings.usepkg <- dt.usepkg[,fifelse(is.na(args),
                                         sprintf("\\usepackage{%s}",pkg),
                                         sprintf("\\usepackage[%s]{%s}",args,pkg),
                                         ),by=row][,V1]
    
    lines.ltx <- c("\\documentclass{article}",
                   ## "\\usepackage[margin=1in]{geometry}",
                   ## paste0("\\usepackage{",usepackage,"}")
                   strings.usepkg
                  ,
                   "\\begin{document}",
                   lines.ltx,
                   "\\end{document}")
    
    file.tex <- fnExtension(file.pdf,"tex")
    NMsim:::writeTextFile(lines.ltx,file=file.tex)

    ## if(!exists("latexmk")) tinytex::install_tinytex()

    latexmk(file.tex)
    
}
