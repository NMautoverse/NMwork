
##' Create new control stream based on an existing model
##' 
##' @param newfile The new control stream file path.
##' @param file.mod The path to the control stream to use as the
##'     starting point.
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
##' @import NMsim
##' @export


### NMwriteInits is from NMsim 0.1.9 or NMdata 0.2.0
## newModel() should make it into NMdata or NMsim at some point. But
## it uses the NMwritePreamble which is a pirana-specific function.

newModel <- function(newfile,file.mod,update=TRUE,values,
                     ## preamble arguments
                     description=NULL,
                     based.on,author=NULL,write.file=TRUE
                    ,overwrite=FALSE,
                     modify=NULL,
                     inits=NULL,
                     filters=NULL){

    if(missing(values)) values <- NULL

    
    newmod <- NMwriteInits(file.mod,update=update,values=values)
    if(is.list(newmod)) newmod <- newmod[[1]]
    
######### Inits
## if(FALSE){
    if(!is.null(inits)){
        
        ## newmod <- do.call(NMwriteInits,args=c(list(file.mod=newfile,lines=newmod),inits))
        
        newmod <- do.call(
            NMwriteInits
           ,
            ## args=
            c(list(file.mod=file.mod),inits)
        )
        
    }
##}
    
    
    ## newmod is a list
    ## str(newmod)
    
### update table file names
    newmod <- NMupdateFn(lines=newmod,section="TABLE",
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


######### Filters 
    ## if(!is.null(filters)){
    ##     if(is.function(filters)){
    ##     }
    ##     newmod <- NMwriteFilters(filters,lines=newmod)
    ## }

    
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


