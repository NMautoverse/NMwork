mergeLabel <- function(pars,tab.label=NULL,by="parameter",col.label="parameter"){

    if(is.null(col.label)) col.label="parameter"
    
    if(!is.null(tab.label)){
        pars <- mergeCheck(pars,tab.label[,c(by,col.label),with=FALSE],
                           by=by,
                           all.x=T,quiet=T,
                           as.fun="data.table")
    }



    ### handling objv. May not be the best place to do this? Maybe this should also be handled in createParameterTable to get both symbol and label. Here, default values can be put in if not already available.
    pars[parameter%in%c("OBJ","SAEMOBJ"),(col.label):="Obj Function Value"]

    pars[,label:=get(col.label)]
    
    ## if these columns are present, sort by them. I am not sure if this would be better handled elsewhere. Maybe in createParameterTable but I think making label a factor could create issues - so maybe it should be a different column.
    if(all(c("par.type","i","j")%in%colnames(pars))){
        pars[, match.order := match(par.type, c("THETA", "OMEGA", "SIGMA", "OBJ"))]
        pars[, label := reorder(label, frank(pars, match.order, i, j))]
        pars[,match.order:=NULL]

    }
    
    pars
    
}
