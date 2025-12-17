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

modelPaths <- function(file,as.dt=TRUE,must.exist=FALSE,simplify=TRUE){


    if(must.exist){
        files.missing <- file[!file.exists(file)]
        if(length(files.missing)){
            stop("Not all models exist, and `must.exist=TRUE`:\n",paste(files.missing,collapse=",\n"))
        }
    }

    ### this is what NMdata functions call 'model', not 'run'

    ### TODO inherit NMdataConf()$modelname and other file naming variables

    all <- data.table(mod=fnExtension(file,"mod")
                      ## ,lst=fnExtension(file,"lst")
                      ## should be called model and not run to align with NMdata functions
                     ,model=basename(file) |> fnExtension("")
                      )[,
                        ## for backward-compat, keep run but don't use. To be deprecated.
                        run:=model]



    ## If a bbr model, paths are to version in subfolder.
    modelIsBbr <- function(file.mod){
        res <- data.table(
            structure="main"
           ,mod=file.mod
        )
        
        res[,dir.parent :=  dirname(file.mod)]
        res[,dirname.parent := basename(dir.parent)]
        res[,name.model := basename(file) |> fnExtension("")]
        res[dirname.parent==name.model,structure := "bbr"]
        res[dirname.parent==name.model,mod := mod]
        ## if(dirname.parent==name.model){
        ##     ## file.mod is inside a bbr subfolder
        ##     res$structure <- "bbr"
        ##     res$mod <- file.mod
        ## }
        ## dirs.found <- list.files(path=dirname(file.mod),pattern=name.model)
        res[,dir.bbr.sub := file.path(dirname(mod),name.model)]
        res[,bbr.dir.found := dir.exists(dir.bbr.sub) ]
        res[bbr.dir.found==TRUE,structure := TRUE]
        res[bbr.dir.found==TRUE,mod := file.path(dir.bbr.sub,basename(file.mod))]
        ##bbr.dir.found <- dir.exists(dir.bbr.sub) 
        ## if(length(bbr.dir.found)){
        ## res$structure <- "bbr"
        ## res$mod <- file.path(dir.bbr.sub,basename(file.mod))
        ## }
        res[,.(structure,mod)]
    }

    all[,(c("structure","mod")):=modelIsBbr(mod)]

    ## lst
    all[,lst := fnExtension(mod,"lst")]


    all[
       ,label := sub(" */data/","",lst) ][
       ,runno := as.numeric(gsub("[^0-9]+","",run))]

    ### I think this is just description. I don't understand why it's been called name.
    ### add name from Description field in .mod
    funName <- function(mod){
        lines.mod <- readLines(mod,warn=FALSE)
        line.desc <- lines.mod[grepl("^;+ *2. *Description:",lines.mod)]
        line.desc <- sub("^;+ *2. *Description: *","",line.desc)
        ## TODO test. Is assignment forgotten?
        sub(":.*$","",line.desc)
        if(length(line.desc)==0) line.desc <- ""
        paste(line.desc,collapse="\n")
    }
    ## all$name <- lapply(all$mod,funName) |> unlist()
    all[,description:=funName(mod),by=1:nrow(all)]
    
    
    refName <- function(mod){
        
        lines.mod <- readLines(mod,warn=FALSE)
        line.desc <- lines.mod[grepl("^;+ *1. *Based on *:",lines.mod)]
        line.desc <- sub("^;+ *1. *Based on *: *","",line.desc)
        line.desc <- sub(":.*$","",line.desc)
        if(length(line.desc)==0) line.desc <- ""
        line.desc
    }
    ##    all$ref <- lapply(all$mod,refName) |> unlist()
    all[,ref := refName(mod),by=1:nrow(all)]


    ##    all <- as.data.table(all)
    if(as.dt){

        return(all[])
    }

    ## When is this better?

    col.name <- "mod"
    all <- lapply(split(all,by=col.name),as.list)
    if(simplify && length(all)==1){
        all <- all[[1]]
    }
    all
}

