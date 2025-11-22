##' find model paths winin a directory
##'
##' Finds models created by NMsim, BBR, and PSN.
##'
##' @param methods Prioritized vector of model types to search
##'     for. "main" means psn and nmsim because they copy results to
##'     main dir.
##'
##' @details "main": any lst in main dir "bbr": any lst generated this
##'     way: look for x.mod files, look for x/ and for x/x.lst. The
##'     x.lst is the result. But a x.mod is used to compare to results
##'     from "main". When x.lst (derived from x.mod) matches a lst
##'     file from main, the order of methods is used to prioritize.

### Need more use cases before exporting

findModels <- function(dir,pattern=".*.lst$",methods=c("main","bbr"),recursive=FALSE,ext.mod.bbr=".mod"){
### variables named files., lsts contain files known to
### exist. Those named path are unknown to matched existing files.

    findModsMain <- function(dir,recursive,pattern){
        paths <- data.table(
            file.lst.main=list.files(
                path=dir,pattern=pattern,recursive=recursive,full.names=TRUE
            ))
        paths[,file.lst:=file.lst.main]
        paths[,method:="main"]
        ##        paths[,path.mod:=]
        paths <- paths[!is.na(file.lst)]
        paths
    }
    
    ## find bbr
    findModsBbr <- function(dir,recursive,ext.mod.bbr){

        pattern.bbr <- paste0(".*",ext.mod.bbr)
        bbr <- data.table(mod.main=list.files(
                              path=dir,pattern=pattern.bbr,
                              recursive=recursive,full.names=TRUE))
## dropping findings of .mod files in main with no associated bbr dir.
        bbr[,dir:=fnExtension(mod.main,"")]
        bbr <- bbr[dir.exists(dir)]
        
        bbr[dir.exists(dir),path.lst.bbr:=file.path(dir, fnExtension(basename(dir),"lst"))]
        bbr[!is.na(path.lst.bbr)&file.exists(path.lst.bbr),
            file.lst.bbr:=path.lst.bbr]
        
        bbr[,file.lst:=file.lst.bbr]
        ## looking for lst in main dir to match against models found in main
        bbr[,path.mod.main:=fnExtension(mod.main,"lst")]
        bbr[file.exists(path.mod.main),file.mod.main:=path.mod.main]

        bbr <- bbr[!is.na(file.lst)]
        bbr[,method:="bbr"]
    }

    all <- NULL
    if("main"%in%methods) {
        main <- findModsMain(dir=dir,pattern=pattern,recursive=recursive)
        all <- rbind(all,main,fill=TRUE)
    }
    if("bbr"%in%methods) {
        bbr <- findModsBbr(dir=dir,ext.mod.bbr=ext.mod.bbr,recursive=recursive)
        all <- rbind(all,bbr,fill=TRUE)
    }
    if(is.null(all)) return(NULL)
    ## very quickly, prioritizing according to order in methods
    all <- all[order(file.lst.main,match(method,methods))][!duplicated(file.lst.main)]
    
    all[,file.lst]
}
