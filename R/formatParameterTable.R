##' @param drop symbol names to drop
##' @details A simple printing function for createParameterTable(). It creates three objects.
##' \itemize{
##' \item partab_full: a df with all columns for selected parameters.
##' \item partab_detail: a flextable with selected columns for selected parameters.
##' \item partab_present: a flextable with fewer selected columns for selected parameters.
##' }
##'
##' Notes: I suggest dropping the asterisk on parameter.ltx and
##' instead include a nicely formated transformation column in
##' createParameterTable(). It could be included in a parameter table
##' only if non-standard transformations are used.
##'
##' formatParameterTable is a bad name. This function selects rows and
##' columns in three different levels of detail and returns those
##' three objects. Formatting should happen in
##' `createParameterTable()`.
##'
##' @import flextable
##' @keywords internal
##'

formatParameterTable <- function(pars,include.fix="ifNotZero",include,drop){

    if(missing(include)) include <- NULL
    if(missing(drop)) drop <- NULL
    

### Include * at transformed variables
    pars[trans%in%c("log","logTrans"),parameter.ltx:=sub("\\$ *$","\\{\\}\\^\\*\\$",parameter.ltx)]
    ## shrinkage is not included in the parameter table for now.

#### It would be good to be able to save this one
    ## NMstamp(pars,script=label.script,model=model$label)
    ## saveRDS(pars,fnOut)
    pars.full <- copy(pars)
    
    ## subset whatever should be in the parameter table. In this example, we skip FIXed parameters.
    ## pars <- pars[symbol!="not used"]
    if( is.logical(include.fix) && isFALSE(include.fix) ){
        pars <- pars[FIX==0|symbol%in%include]
    }
    
    if( !is.logical(include.fix) && tolower(include.fix)=="ifnotzero" ){
        pars <- pars[FIX==0|est!=0|symbol%in%include]
    }

    if( !is.null(drop) ){
        pars <- pars[!symbol%in%drop]
    }

    
    ## pars <- pars[]
    ## pars <- pars[FIX==0|symbol%in%cc(F1TAB,AMAX,AC50,KENZ,V2PWT,V3PWT,V2MWT,V3MWT)]    
    pars.detail <- pars[,.(Parameter=par.name,
                          ## Paramtype=panel,
                          Symbol=symbol,
                          Label=tab.lab,
                          Estimate=tab.est,
                          "95% CI"=CI,
                          Transformation=trans
                          )]
    pars.detail[is.na(Label),Label:=""]
    
    ft.detail <- flextable(pars.detail) |> autofit()

### smaller table for presentation
    pars.present <- pars.detail[,.(Symbol,Label,"Estimate [CV] (RSE)"=Estimate,`95% CI`)]
    ft.present <- flextable(pars.present) |> autofit()
    
    list(partab_full=pars.full,
         partab_detail=ft.detail,
         partab_present=ft.present)

}
