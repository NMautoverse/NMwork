##' Create a convenient set of file paths and other info related to a
##' model.
##' @param file path to control stream, or almost any other model file
##' @param as.dt Return model information in a data.table?
##' @param must.exist Make sure the provided paths match files?
##'     Default is not to but it can be a good first check in a
##'     script.
##' @param simplify If only one model supplied, and `as.dt=FALSE`,
##'     should the format be simplified to a list of model elements,
##'     rather than a list of (model) lists of elements?
##' @import data.table
##' @import NMdata
##' @export

modelPaths <- function(file,as.dt=FALSE,must.exist=FALSE,simplify=TRUE){
    
    if(must.exist && any(!file.exists(file))) stop("Not all models exist, and `must.exist=TRUE`")
    
    all <- list(mod=fnExtension(file,"mod")
               ,lst=fnExtension(file,"lst")
               ,run=basename(file) |> fnExtension("")
                )
    
    all <- within(all,{
        label = sub(" */data/","",lst)
        runno=as.numeric(gsub("[^0-9]+","",run))
    })
### add name from Description field in .mod
    funName <- function(mod){
        lines.mod <- readLines(mod,warn=FALSE)
        line.desc <- lines.mod[grepl("^;+ *2. *Description:",lines.mod)]
        line.desc <- sub("^;+ *2. *Description: *","",line.desc)
        sub(":.*$","",line.desc)
        if(length(line.desc)==0) line.desc <- ""
        line.desc
    }
    all$name <- lapply(all$mod,funName) |> unlist()
    
    if(!is.null(names(file))) all$name <- names(file)
    
    refName <- function(mod){
        
        lines.mod <- readLines(mod,warn=FALSE)
        line.desc <- lines.mod[grepl("^;+ *1. *Based on *:",lines.mod)]
        line.desc <- sub("^;+ *1. *Based on *: *","",line.desc)
        line.desc <- sub(":.*$","",line.desc)
        if(length(line.desc)==0) line.desc <- ""
        line.desc
    }
    all$ref <- lapply(all$mod,refName) |> unlist()


    all <- as.data.table(all)
    if(as.dt){

        return(all)
    }

    
    all <- lapply(split(all,by="name"),as.list)
    if(simplify && length(all)==1){
        all <- all[[1]]
    }
    all
}
