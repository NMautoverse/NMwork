##' Create a convenient set of file paths and other info related to a model.
##' @param file path to control stream, or almost any other model file
##' @param as.dt Return model information in a data.table?
##' @param simplify If only one model supplied, and `as.dt=FALSE`,
##'     should the format be simplified to a list of model elements,
##'     rather than a list of (model) lists of elements?
##' @import data.table

modelPaths <- function(file,as.dt=FALSE,simplify=TRUE){
     
    
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
