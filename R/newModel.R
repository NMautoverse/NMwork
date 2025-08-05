
##' Create new control stream based on an existing model
##' 
##' @param file.mod The path to the control stream to use as the
##'     starting point.
##' @param newfile The new control stream file path.
##' @param update Update initial values with final paremeter estimates
##'     of `file.mod`.
##' @param values Specify specific initial values (passed to
##'     NMwriteInits).
##' @param description A Pirana style description field before the
##'     control stream code.
##' @param based.on A Pirana style field before the control stream
##'     code.
##' @param author A Pirana style field before the control stream code.
##' @param write.file Write to newfile? Default is TRUE. See
##'     `overwrite` too.
##' @param overwrite If newfile exists, overwrite it?
##' @param modify List of modification to do to control stream. Passed
##'     to NMsim:::modifyModel().
##' @details Pirana fields are only applied if existing in `file.mod`.
##' @examples
##' ## Reference Base model is run11. You are trying two differnt new
##' ## models run21 and run22. update=TRUE to use final parameter
##' ## estimates as new inits.
##' newmod <- newModel(file.mod="run11.mod",newfile="run21.mod",update=TRUE)
##' ## manual edits of run21.mod. Execute run21.mod
##' ## NMexec(newmod)
##' 
##' # add run22 based on the code in run21 because we need most of the
##' # same edits, so file.mod=run21. But you are testing run22 against
##' # run11, so based.on="run11". Don't update the initial values
##' # based on run21. We want them to be identical to run21, so use
##' # update=FALSE.
##' newmod <- newmodel(file.mod="run21.mod",newfile="run22.mod",based.on="run11.mod",update=FALSE)

### NMwriteInits is from NMsim 0.1.9 or NMdata 0.2.0
## newModel() should make it into NMdata at some point. But it uses
## the NMwritePreamble which is a pirana-specific function.

newModel <- function(file.mod,newfile,update=TRUE,values,
                     ## preamble arguments
                     description=NULL,based.on,author=NULL,write.file=TRUE
                    ,overwrite=FALSE,
                     modify=NULL){

    if(missing(values)) values <- NULL

    
    newmod <- NMwriteInits(file.mod,update=update,values=values)
    
    ## newmod is a list
    ## str(newmod)

### update table file names
    newmod <- NMupdateFn(lines=newmod[[1]],section="TABLE",
                         model=basename(newfile),
                         fnext=".tab",add.section.text=NULL,
                         par.file="FILE",
                         text.section=NULL)

    ## str(newmod)
    
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
    
    newmod <- NMsim:::modifyModel(modify=modify,list.ctl=list(newmod))[[1]]

    ## data file
    ## NMreplaceDataFile()
    
    
##### write output
    if(write.file) {
        if(!file.exists(newfile) || overwrite){
            NMsim:::writeTextFile(newmod,file=newfile)
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
    
    sec <- substr(toupper(section),1,3)
    
    ## what to look for 
    dollar.section <- paste0("$",sec)
    ## what to print
    dollar.section.new <- dollar.section
    if(dollar.section.new=="EST") dollar.section.new <- "ESTIMATION"
    if(dollar.section.new=="SIM") dollar.section.new <- "SIMULATION"

    
    
    
### is NMreadSection returning a list?    
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
    lines <- NMsim:::NMwriteSectionOne(lines=lines,section=section,newlines=lines.section.new,quiet=TRUE)

    lines
    
}
### Section end: Function to update file name and optionally section contents
