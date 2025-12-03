# load_all()
## options(warn=0)
## test footnotes at log and logit transformations
file.mod <- "testData/nonmem/xgxr134.mod"
pars <- createParameterTable(file.mod)
printParameterTable(pars)

pars[,.N,by=.(par.type,trans)]



## log and logit
pars[par.name=="THETA(1)",trans:="logit"]
pars[par.name=="THETA(1)",trans]
pars[,.N,by=.(par.type,trans)]

printParameterTable(pars)


## no transformations
pars[,trans:="none"]
pars[par.name=="THETA(1)",trans]
pars[,.N,by=.(par.type,trans)]
printParameterTable(pars)
