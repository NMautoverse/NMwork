devtools::load_all("~/wdirs/NMwork")

file.mod <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr133.mod"

model <- modelPaths(file.mod)

### look up formatting of parameter sections
NMreadSection(model$lst,section="THETA")
NMreadSection(model$lst,section="OMEGA")
NMreadSection(model$lst,section="SIGMA")

## library(stringi)
partab <- createParameterTable(model$lst,
                               args.ParsText=list(
                                   format="%init;%idx:%symbol;%label;%trans",
                                   format.sigma="%init;%label"))

## creates three objects containing subsets of rows and columns -
## different degrees of detail. The subsetting of rows defines what
## parameters are being reported. The row subsetting can be controlled
## using convenient arguments.
tabs.write <- formatParameterTable(partab)

if(FALSE){
    tabs.write$partab_detail
    tabs.write$partab_full
    tabs.write$partab_full$label
    tabs.write$partab_full$par.label
}

tabs.write$partab_full[,.(parameter,symbol,label,par.label)]

### trace plot
plotTrace(model$mod,pars=tabs.write$partab_full,label.by="parameter",col.label="par.label")
## if the parameter table was not generated
plotTrace(model$mod)

### covariance plot
plotEstCor(model$mod,pars=tabs.write$partab_full,label.by="parameter",col.label="par.label")
plotEstCor(model$mod)
plotEstCor(model$mod,col.label="par.label")

## BSV plot


