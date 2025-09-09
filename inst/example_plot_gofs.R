library(NMdata)
library(data.table)
library(scales)
# source("~/wdirs/NMwork/R/modelPaths.R")
# source("~/wdirs/NMwork/R/latexify.R")
getwd()
file.lst <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr132.lst"
model <- modelPaths(file.lst)

## generate parameter table

source("~/wdirs/NMwork/R/createParameterTable.R")

NMreadSection(model$mod,section="THETA")
NMreadSection(model$lst,section="THETA")
NMreadSection(model$mod,section=c("OMEGA"))
NMreadSection(model$mod,section=c("SIGMA"))
pars <- createParameterTable(file.lst=model$mod,args.ParsText=list(
                                                    format="%init;%idx:%symbol;%label;%trans",
                                                    format.sigma="%init%symbol-%label"
                                                    ))

source("~/wdirs/NMwork/R/plotEstCor.R")

plotEstCorr(model$lst,pars=pars)


