devtools::load_all("~/wdirs/NMwork")

file.mod <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr003.mod"
file.mod <- "~/wdirs/NMdata/inst/examples/nonmem/xgxr018.mod"

model <- modelPaths(file.mod)

data = NMscanData(file = file.mod) |> mutate(across(c(TIME,AFRLT,APRLT,NFRLT,NPRLT,RES,WRES,CWRES,NPDE,IWRES), ~ as.numeric(.x)))
data.parent = filter(data, EVID==0, DVID=="SUZ") 
data.metab = filter(data, EVID==0, DVID=="M6-SUZ") 

## DV vs. PRED -------
p1 = plotDVpredIpred(.file.mod = file.mod,.dvcol = "DV",.predcol = "PRED")

p2 = plotDVpredIpred(.file.mod = file.mod, .dvcol = "DV", .predcol = "PRED", .log = TRUE)

lapply(list(p1,p2), function(.x) .x+labs(x="Population predictions (ng/mL)",y="Observations (ng/mL)")) |> patchwork::wrap_plots()

## DV vs. IPRED  -------
plotDVpredIpred(.file.mod = file.mod,.dvcol = "DV",.predcol = "IPRED")

plotDVpredIpred(.file.mod = file.mod, .dvcol = "DV", .predcol = "IPRED", .log = TRUE)

## Residuals vs. TIME  -------
plotResTime(.file.mod = file.mod, .ResCol = "CWRES", .TimeCol = "TIME")
# just a scatter plot
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "AFRLT")
# add a smooth line
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "AFRLT", .AddSmooth = T)
# add pointrange summarizing by nominal time
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "AFRLT", .NomtimeCol = "NFRLT")
# pointrange + smoother --> shows how the smoother can overweight sparse observations even when they are a small part of the data.
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "AFRLT", .NomtimeCol = "NFRLT", .AddSmooth = T)

## Residuals vs. TAPD -------
# scatter alone
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "APRLT")
# with pointrange.
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "APRLT", .NomtimeCol =  "NPRLT")
# with smoother
plotResTime(.data = data.parent, .ResCol = "CWRES", .TimeCol = "APRLT", .NomtimeCol =  "NPRLT", .AddSmooth = T)


## Residuals vs. PRED
plotResPred(.data = data.parent, .ResCol = "CWRES", .PredCol = "PRED", .AddSmooth = T)
plotResPred(.data = data.parent, .ResCol = "NPDE", .PredCol = "PRED", .AddSmooth = T)


# Plots above, but stratified by another variable. 
split_plots_by = c("STUDYID")
distinct(data.parent, all_of(split_plots_by))

dvpred_parent_study = map(.x = split(data.parent,by = "STUDYID"), function(.x) plotDVpredIpred(.data = .x,.dvcol="DV",.predcol = "PRED")+geom_point(aes(color=DOSE1),shape=1,size=3))



strats = c("")
# DV vs. PRED
