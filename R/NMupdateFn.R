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
