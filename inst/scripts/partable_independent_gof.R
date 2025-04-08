devtools::load_all("~/wdirs/NMwork")

file.mod <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr003.mod"

model <- modelPaths(file.mod)


## DV vs. PRED
p1 = plotDVpredIpred(.file.mod = file.mod,.dvcol = "DV",.predcol = "PRED")

p2 = plotDVpredIpred(.file.mod = file.mod, .dvcol = "DV", .predcol = "PRED", .log = TRUE)

lapply(list(p1,p2), function(.x) .x+labs(x="Population predictions (ng/mL)",y="Observations (ng/mL)")) |> patchwork::wrap_plots()

## DV vs. IPRED
plotDVpredIpred(.file.mod = file.mod,.dvcol = "DV",.predcol = "IPRED")

plotDVpredIpred(.file.mod = file.mod, .dvcol = "DV", .predcol = "IPRED", .log = TRUE)



## NPDE/CWRES vs. TIME
plotResTime(.file.mod = file.mod, .ResCol = "RES", .TimeCol = "TIME")
plotResTime(.file.mod = file.mod, .ResCol = "RES", .TimeCol = "TIME", .SummarizeBy = "NOMTIME")

## NPDE/CWRES vs. TAPD

## NPDE/CWRES vs. PRED



# Plots above, but stratified by another variable. 

