load_all~("~/wdirs/NMwork")

file.mod <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr133.mod"

model <- modelPaths(file.mod)

createParameterTable(model$lst)
