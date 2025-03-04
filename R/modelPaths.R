##' @param mod path to control stream

modelPaths <- function(mod,as.dt=FALSE,simplify=TRUE){
    

    all <- list(mod=fnExtension(mod,"mod")
               ,lst=fnExtension(mod,"lst")
               ,run=basename(mod) |> fnExtension("")
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

    if(!is.null(names(mod))) all$name <- names(mod)
    
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
