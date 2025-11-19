##' Add parameter labels from a parameter table
##' @param pars parameters table
##' @param tab.label table with a column for labels
##' @param by column to merge pars and tab.label
##' @param col.label column in `tab.label` to use for labelling
##' @param suffix not sure
##' @import data.table
##' @importFrom NMdata mergeCheck
##' @details This is internally used in plotEstCor and plotTrace. Not
##'     sure it should its worth exporting.


mergeLabel <- function(pars,tab.label=NULL,by="parameter",col.label="parameter",suffix=NULL){

    if(is.null(col.label)) col.label="parameter"
    col.newlabel <- "label"
    
    if(!is.null(tab.label)){
        
        if(!is.null(suffix)){
            
            tab.label <- copy(tab.label)
            cols.rename <- unique(c(by,col.label))
            setnames(tab.label,cols.rename,paste(cols.rename,suffix,sep="."))
            by <- paste(by,suffix,sep=".")
            col.label <- paste(col.label,suffix,sep=".")
            col.newlabel <- paste(col.newlabel,suffix,sep=".")
        }
        
        pars <- mergeCheck(
            pars
           ,
            tab.label[,c(by,col.label),with=FALSE]
           ,
            by=by,
            all.x=T,quiet=T,
            as.fun="data.table")
    }


    
### handling objv. May not be the best place to do this? Maybe this should also be handled in createParameterTable to get both symbol and label. Here, default values can be put in if not already available.
    if("parameter"%in%colnames(pars)){
        pars[parameter%in%c("OBJ","SAEMOBJ"),(col.label):="Obj Function Value"]
    }
    
    ## if these columns are present, sort by them. I am not sure if this would be better handled elsewhere. Maybe in createParameterTable but I think making label a factor could create issues - so maybe it should be a different column.
    if(all(c("par.type","i","j")%in%colnames(pars))){

### Check: match looks up values in first against order provided in second
        ## match(c("THETA", "OMEGA","THETA"), c("THETA", "OMEGA", "SIGMA", "OBJ"))
        ## match(c("THETA", "OMEGA", "SIGMA", "OBJ"),c("THETA", "OMEGA","THETA"))

        pars[, match.order := match(par.type, c("THETA", "OMEGA", "SIGMA", "OBJ"))]
        pars[, label := reorder(get(col.label), frank(pars, match.order, i, j))]
        pars[,match.order:=NULL]

    }

    pars[,(col.newlabel):=get(col.label)]
    
    pars
    
}
