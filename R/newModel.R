
##' Create new control stream based on an existing model
##' @param overwrite If newfile exists, overwrite it?
##' 

### NMwriteInits is from NMsim 0.1.9 or NMdata 0.2.0
## newModel() should make it into NMdata at some point. But it uses
## the NMwritePreamble which is a pirana-specific function.

newModel <- function(file.mod,newfile,update=TRUE,values,
                     ## preamble arguments
                     description=NULL,based.on,author=NULL,write.file=TRUE
,overwrite=FALSE){

    if(missing(values)) values <- NULL

    newmod <- NMwriteInits(file.mod,update=update,values=values)

    
    
### update table file names
    newmod <- NMupdateFn(lines=newmod,section="TABLE",
                         model=basename(newfile),
                         fnext=".tab",add.section.text=NULL,
                         par.file="FILE",
                         text.section=NULL)
        
### update .msf
    newmod <- NMupdateFn(lines=newmod,section="EST",
                         model=basename(newfile),
                         fnext=".msf",add.section.text=NULL,
                         par.file="MSFO",
                         text.section=NULL)


### update pirana description sections
    ## NMwritePreamble()
    if(missing(based.on)) based.on <- NULL
    ## better if based.on is a relative path from the new file
    if(is.null(based.on)) based.on <- file.mod
    newmod <- NMwritePreamble(lines=newmod,description=description,based.on=based.on,author=author,write.file=FALSE)


### modify.model

##### write output
    if(!is.null(newfile)) {
        if(!file.exists(newfile) || overwrite){
            writeTextFile(newmod,file=newfile)
        } else {
            message("Model not overwritten")
        }
### in this case, we return the path. Ready for NMexec(res)
        message(paste("returning path to newfile:",newfile))
        return(invisible(newfile))
    }
    message("returning new control stream text")
    return(invisible(newModel))
}


#### Section start: Function to update file name and optionally section contents ####

### issue:     if(exists("add.var.table")) - add.var.table is defined outside the function

## In $EST, "FILE" is "MSFO"

##' @param add.section.text Addditional text to insert right after $SECTION. It can be additional TABLE variables.
##' @param par.file The Nonmem parameter that secifies te file. In $TABLE, this is FILE. In $EST it's probably MSFO.

NMupdateFn <- function(lines,section,model,fnext,add.section.text,par.file,text.section){
### Arguments to replace: FILE, .tab, text.table

    if(missing(text.section)) text.section <- NULL
    run.sim <- fnExtension(basename(model),ext="")
    
    fn.tab.base <- paste0(par.file,"=",run.sim,fnext)
    ## lines.mod <- readLines(model)

    dollar.section <- section
    dollar.section <- paste0("$",substr(dollar.section,1,3))    
    dollar.section.new <- dollar.section
    if(dollar.section.new=="EST") dollar.section.new <- "ESTIMATION"
    if(dollar.section.new=="SIM") dollar.section.new <- "SIMULATION"

    
    lines.section <- NMreadSection(lines=lines,section=section,as.one=FALSE,simplify=FALSE)
    
    if(is.null(text.section)){
        ## replace output table name
        if(length(lines.section)==0){
            stop(sprintf("No %s statements found in control stream.",section))
        } else if(length(lines.section)<2){
            ## notice, this must capture zero and 1.
            lines.section.new <- list(gsub(paste0(par.file," *= *[^ ]+"),replacement=fn.tab.base,lines.section[[1]]))
        } else {
            ## I don't remember the reason for the concern this may fail. It looks OK? I think it was supposed to be a check if text.section was a list, so the user is trying to create multiple tables. I don't know if that would work.
            ## message("Number of output tables is >1. Trying, but retrieving results may fail.")
            lines.section.new <- lapply(seq_along(lines.section),function(n){
                fn.tab <- fnAppend(fn.tab.base,n)
                gsub(paste0(par.file," *= *[^ ]+"),replacement=fn.tab,lines.section[[n]])
            })
        }
    } else {
        lines.section.new <- list(paste(dollar.section.new,text.section,fn.tab.base))
    }
    lines.section.new <- paste(unlist(lines.section.new),collapse="\n")
    if(!is.null(add.section.text)){
        lines.section.new <- gsub(paste0("\\",dollar.section,"[A-Z]*"),paste(dollar.section.new,add.section.text),lines.section.new,fixed=FALSE)
    }
    
    ## replace old section with the updated one
    NMdata:::NMwriteSectionOne(lines=lines,section=section,newlines=lines.section.new)

    
}
### Section end: Function to update file name and optionally section contents
