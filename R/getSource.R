##' Get and source an R script.
##' 
##' Get file from remote library and inport it into project if it does
##' not exist. Then source it.
##'
##' @param file File name of wanted file
##' @param dir.central Folder path of wanted file
##' @param dir.local Where to put the file if imported, and where to look for
##'     already imported files. Default is getwd().
##' @param overwrite Owerwrite previously imported file?
##' @param source.directly Enables direct sourcing of the central file copy. This
##'     bypasses the whole concept of the function but it is useful when
##'     developing while using a function. Especially if your debugger in the
##'     editor is linking to a file that you will edit while debugging. It gives
##'     a warning because it is not recommended in final code.
##' @param quiet Disables printning. Mainly used in testing.
##' @family FileSystem
##' @return None. Sources the specified file into the global environment.
##' @export

getSource <- function(file,dir.central=NULL,dir.local,overwrite=FALSE,source.directly=FALSE,quiet=T){

    filePathSimple <- NMdata:::filePathSimple
    
### Getting the paths right
    if(is.null(dir.central)){
        dir.central <- dirname(file)
        file <- basename(file)
    }
    if(missing(dir.local)) dir.local <- getwd()
    dir.local <- filePathSimple(dir.local)
    dir.central <- filePathSimple(dir.central)

    ## It should be checked that destination folder exists before doing anything.
    ## if(!dir.exists(dir.local)) {stop("Destination directory (dir.local) must exist.")}

    ## Check that source directory is different from destination directory
    if(filePathSimple(dir.central)==filePathSimple(dir.local)){
        stop("source and destination directories are identical. Makes no sense.")
    }

    
    dt.files <- data.table(file=file)[
       ,org := file.path(dir.central,file)][
       ,dest := file.path(dir.local,file)][
       ,row:=.I][
       ,do.source:="dest"]

    
    dt.files[
       ,org.exists:=file.exists(org)][
       ,dest.exists:=file.exists(dest)]
    


    if(source.directly){
        ##source(org)
        dt.files[org.exists==FALSE,do.source:={
            if(.N>0) message("Source files not found and will be ignored:",paste(file,collapse=", "))
            ""
        }]

        dt.files[org.exists==TRUE,do.source:={
                                        # source(org,echo=FALSE)
            "org"
        },by=row]

        
        message("Central file(s) was/were sourced directly. Only allowed for debugging. Switch off and run once with overwrite=TRUE if you want to update. Use this option for debugging only.")
        dt.files[do.source=="org",if(.N>0) {
                                      source(org,echo=FALSE)
                                      NULL
                                  },by=row]

        return(invisible(dt.files))
    }
    

    dt.files[ !org.exists==TRUE & dest.exists==TRUE,do.source:={
        ## File exists locally, but not external. Load local
        ##source(file.path(dest),echo=FALSE)
        message("File not found at dir.central. Local version sourced.")
        "dest"
    },by=row]

    
    dt.files[ !org.exists==TRUE & !file.exists(dest),do.source:={
        if (.N>0 & quiet == FALSE){
            message("No original and no local version have been found. Skipping",file)
        }
        ""
    },by=row]


    dt.files[ file.exists(org) &
              (overwrite | !file.exists(dest)),do.source:={
                  ## Copying the latest version of the file
                  if (quiet == FALSE){message("Copying ",file)}
                  dir.create(dir.local,recursive=TRUE,showWarnings=FALSE)
                  file.copy(from=org,
                            to=dest,overwrite=TRUE)
                  lines.script <- readLines(org,warn=FALSE)
                  lines.script <- c(sprintf("## Copied from %s",org),
                                    sprintf("## on %s using NMwork::getSource().",Sys.Date()),
                                    "",lines.script)
                  NMsim:::writeTextFile(lines=lines.script,file=dest)
                  "dest"
              },by=row]

    ## source
    
    dt.files[do.source=="org",if(.N>0) {
                                  source(org,echo=FALSE)
                                  NULL
                              },by=row]
    dt.files[do.source=="dest",if(.N>0) {
                                   source(dest,echo=FALSE)
                                   NULL}
            ,by=row]

    invisible(dt.files)
}

