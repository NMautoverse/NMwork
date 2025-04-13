##' Run script on models using Rscript, optionally using sge
##' @param script The actual command-line command you want to run on
##'     the grid (including all arguments)
##' @param file.mod First argument sent to script. If length>1, the
##'     script is run on all the elements.
##' @param ... Additional arguments to script.
##' @param sge Send to cluster using `qsub`?
##' @param wait Only used if sge is `FALSE`.
##' @param nc Only used if sge is `TRUE`. The number of cores you want
##'     the script to run on the grid with.
##' @param stdout_file Only used if sge is `TRUE`. The file where the
##'     job output will be printed.
##' @param stderr_file Only used if sge is `TRUE`. The file where the
##'     job errror output will be printed.

NMscript <- function(
                     script,
                     file.mod,
                     ...,
                     sge=FALSE,
                     wait=FALSE,
                     nc = 4,
                     stdout_file,
                     stderr_file
                     ) {


    if(missing(stdout_file)) stdout_file <- NULL
    if(missing(stderr_file)) stderr_file <- NULL
    
                                        #.cmd = glue::glue("Rscript {.script} {.file_mod}")


##### probably apply around model instead of running Rscript on all of model
    ## cmds <- Rscript(script,model,execute=FALSE)
    ## lapply(cmds,function(cmd){
    ## }

    lapply(file.mod,function(mod){


        ## cmd <- Rscript(script,model=mod,execute=FALSE)[[1]]
        cmd <- paste("Rscript",script,mod, unlist(list(...)))
        if(sge){
            outfile.def.base <- file.path(dirname(mod),"qsub_debug",
                                          paste(fnExtension(basename(script),ext=""),fnExtension(basename(mod),ext=""),sep="_"))
            if(is.null(stdout_file)){
                stdout_file <- fnExtension(outfile.def.base,"out")
                if(!dir.exists(dirname(stdout_file))) dir.create(dirname(stdout_file))
            }
            if(is.null(stderr_file)){
                stderr_file <- fnExtension(outfile.def.base,"err")
                if(!dir.exists(dirname(stderr_file))) dir.create(dirname(stderr_file))
            }

            
            ## concatenating first two chars of script name and the model name
            ## to get something recognizable in qstat job table.
            jobname <- paste(substr(fnExtension(basename(script),ext=""),1,2),fnExtension(basename(mod),ext=""),sep="")
            ## qsub does not allow a jobname to start in a numeric
            if(grepl("^[0-9]",jobname)) {
                jobname <- paste0("NM",jobname)
            }

            
            qsub_cmd = glue::glue('echo "{cmd}" | qsub -cwd -V -N {jobname} -o {stdout_file} -e {stderr_file} -pe orte {nc}')
            message(paste0("qsub cmd:\n", qsub_cmd))
            
            system(qsub_cmd,intern = FALSE,wait = TRUE)

            return(invisible(qsub_cmd))
        } else {
            message(paste0("cmd:\n", cmd))
            system(cmd,wait=wait)
            return(invisible(cmd))
        }
    })
}
