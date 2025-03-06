################################################################################
## Project     : 
## Study       : 
##
## Description : Template for generation of VPCs. Simulations obtained
## using >= NMsim 0.2.0.
##
## Author      : Philip Delff
## Date        : 2025-03-02
################################################################################



#### Section start: Initialization ####
dir.main = "/data/prod_vx973_phase1_analysis/trunk/analysis/SAD_PopPK"
setwd(dir.main)
source(".Rprofile")



##libraries
library(data.table)
library(devtools)
## devtools::load_all("/data/sandbox/trunk/analysis/NMsim/wdirs/NMdata")
## NMdata 0.1.9 needed
devtools::load_all("~/wdirs/NMdata")
## library(NMdata)
devtools::load_all("~/wdirs/NMsim")
## library(NMsim)
library(tidyvpc)
library(patchwork)
library(tracee)


NMdataConf(as.fun="data.table"
          ,col.row="REC"
          ,quiet=FALSE
          ,col.nomtime="NTAFD"
          ,col.flagn="EXCLF"
          ,col.flagc="EXCLFCOM"
          ,path.nonmem = "/opt/NONMEM/nm75/run/nmfe75"
          ,dir.sims="simtmp/vpcs"
          ,dir.res="simres/vpcs"
           ## ,allow.unknown = TRUE
           )

## library(tidyverse)
## options(tibble.print_max = 50, tibble.print_min = 10, tibble.n_extra = 0, tibble.width = Inf, tibble.min_title_chars = 15)


                                        # functions
## source("/data/prod_cqp_code_library/trunk/DiseaseAreas/Pain/AnalysisTemplates/functions/plot_vpc.R")
## source("~/wdirs/pmxtricks/R/getSource.R")
dir.funs.pain <- "/data/prod_cqp_code_library/trunk/DiseaseAreas/Pain/AnalysisTemplates/functions"
if(!file.exists("functions/getSource.R")){
    file.copy(file.path(dir.funs.pain,"getSource.R"),"functions/")
}
source("functions/getSource.R")
getSource(file="CQP_misc.R", dir.central=dir.funs.pain, dir.local="functions")
getSource(file="modelPaths.R", dir.central=dir.funs.pain, dir.local="functions")


script  <-  scriptLabel(dir.main,"scripts/vpc_tidyvpc.R")


### Section end: Initialization


#### Section start: Settings ####

## Get the run arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args)>0){
    file.mod <- args[1]
} else {
    file.mod <- "models/run017.mod"
}

model <- modelPaths(file.mod,as.dt=T)


#### I don't think we should rename these columns. What exactly is this used for? Can we apply the labels to plots rather than renaming the columns? 
## strat.vars = 
##     c(##"Study"="STUDYID",
##         "DEVPART",
##         "First Dose Amount (mg) (categorical)"= "DOSE1", 
##         "Analyte" = "DVID")


## we need to think more about how we are doing this. Seems like the user will want to define stratification variables together with splitting (like DVID which makes no sense to mix) and facetting (like regimens). And we need to support multiple of those combinations.

NMscanInput(model$mod) |>
    colnames()

## strat.raw <- list(split=c(params$col.dvid,"DEVPART"), facet="DOSESS")
strat.raw <- list(split=c(params$col.dvid,"DEVPART"), facet="DOSE1")
strat.predcorr=list(split=c(params$col.dvid))



## set the parameters of the vpc_template Rmd file
params <- list(
    ##dataset = minimal_data,
    file.mod = file.mod,
    ## philip: isnt this just dir.dest?
    ##dir_output = fnExtension(fnAppend(file.mod,"VPC"),""),
    ## run_script = scriptLabel,
    ## rmd_template = templateLabel,
    include_blq = TRUE,
    subproblems = 1,
    nsims = 500,
    sge=TRUE,
    nc = 18,
    reuse.results=FALSE,
    unitDV = "ng/mL",
    ## not used
    ## compound_name = c("SUZ","M6-SUZ"),
    col.dvid = "DVID",
    ##covLookup = strat.vars, # used for creating plot filenames by their short name (i.e. "DOSE1_100mg" instead of "First Dose Amount (mg)_100mg" )
    ## redundant
    ## stratify_by = names(strat.vars),
    col.bin = "NFRLT" # the column used for binning the vpc stats, usually nominal time after first dose
### Philip: I don't think these are used
    ## include_code = FALSE,
    ## include_plots = TRUE
    ##   run_mrggsave = TRUE # if TRUE: will save all plots and tables in a report-ready format (more or less, may require some tweaking of sizes in some cases)

)

## dir.dest is where the output files will be saved (i.e. html report and
## report-ready figures if requested)
dir.dest = file.path(dirname(model$mod), paste0(model$run,"_VPC"))


### Section end: Settings




#### Section start: Run the simulations #### 


## MANUAL STEP: 
## if we also want the data used in NONMEM (as above) PLUS the BLQ values which
## were not used in NONMEM:


filters <- NULL
if(params$include_blq){
    
    filters <- NMreadFilters(file=params$file.mod)
    filters[cond=="EXCLF.NE.0",cond:="EXCLF.GT.10"]
}


if(F){


    ## check that all variables listed in strat.vars are columns in dataset:

    ### If we want to do this, we need to read input data set and check column names.

    covs.miss = setdiff(strat.vars,colnames(modeling_dataset)) 
    if(length(covs.miss)){
        stop(sprintf("Columns not found:\n %s", paste(covs.miss,collapse=" ,")))
    }
}


path.res <- NMsim(
    file.mod = model$mod,
    ## data = data.inp,
    filters=filters,
    table.vars = cc(PRED, IPRED, Y),
    carry.out=c("TIME","EVID","DV","LLOQ","BLQ",
                strat.vars,params$col.dvid,
                params$col.bin),
    name.sim = fnExtension(basename(script),""),
    file.res = file.path("simres/vpcs",paste0(model$run,"_vpc2.rds")),
    subproblems = params$subproblems,
    nsims = params$nsims,
    sge = params$sge,
    reuse.results=params$reuse.results,
    nc=params$nc,
    seed.R=43
    ## ,sizes=list(PD=200)
)


### it would be more robust to generate the rds path before running NMsim and specifying it in NMsim() as well as in NMreadSim(). That ensures NMsim() and NMreadSim() are refering to the same results.

##NOTE: Note from earlier versions of NMsim. need >16GB RAM for this - the dataset is 22 million rows. Using carry.out should improve this but it will still be heavy.

## seems like R is taking 7GB of ram for 

simres <- NMreadSim(file.res,wait = TRUE)
simres = simres[EVID==0]




###  Section end: Run the simulations



#### Section start: summarise the data and plot  ####

message("summarise the data and plot")

if(!dir.exists(dir.dest)){dir.create(dir.dest)}
file.res <- function(stem) file.path(dir.dest,fnAppend(stem,model$run))

data.obs <- simres[NMREP==first(NMREP)&model.sim==first(model.sim)] 

### calculate stats on all data and all stratifications

### TODO: should the consoring part be included here if include_blq is
### FALSE?




### How do we handle log version and a 24h version? I kept some code further down.



### todo: Summarize how many obs are dropped because of missing bin
stats.raw.split.facet <- 
    observed(data.obs, x = TIME, y = DV) |>
    simulated(simres, x = TIME, y = Y) |>
    censoring(blq = as.logical(BLQ), lloq = LLOQ) |>
    stratify(reformulate(unlist(strat.raw))) |>
    binning(bin = !!sym(params$col.bin)) |>
    ## binning(bin = "ntile", nbins = 8, xbin = "xmean") |>
    vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95)

stats.predcorr.split <- 
    observed(data.obs, x = TIME, y = DV) |>
    simulated(simres, x = TIME, y = Y) |>
    censoring(blq = as.logical(BLQ), lloq = LLOQ) |>
    stratify(reformulate(unlist(strat.predcorr))) |>
    ## binning(bin = !!sym(params$col.bin)) |>
    binning(bin = "ntile", nbins = 8, xbin = "xmean") |>
    predcorrect(pred=PRED)|>
    vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95)

stats.predcorr.all <- 
    observed(data.obs, x = TIME, y = DV) |>
    simulated(simres, x = TIME, y = Y) |>
    censoring(blq = as.logical(BLQ), lloq = LLOQ) |>
    stratify(reformulate(cc(DVID,DEVPART))) |>
    ## binning(bin = !!sym(params$col.bin)) |>
    binning(bin = "ntile", nbins = 12, xbin = "xmean") |>
    predcorrect(pred=PRED)|>
    vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95)

saveRDS(stats.raw.split.facet,file=file.res("vpcstats_raw_split_facet.rds"))
saveRDS(stats.predcorr.split,file=file.res("vpcstats_predcorr_split.rds"))
saveRDS(stats.predcorr.all,file=file.res("vpcstats_predcorr_all.rds"))

### plot all of the data


plotCombos <- function(stats,strat,predcorr,params,ymax=NULL){

    combos <- unique(stats$strat)
    split.combos <- split(combos,by=strat$split)


    plots <- lapply(split.combos,ymax=ymax,FUN=function(combo,ymax){
        sub = list()
        sub$obs = merge.data.table(x=stats$obs, y=combo, by = colnames(combo), all = FALSE)
        sub$stats = merge.data.table(x=stats$stats, y=combo, by = colnames(combo), all = FALSE)
        sub$.stratbin = merge.data.table(x=stats$.stratbin, y=combo, by = colnames(combo), all = FALSE)
        sub$strat = merge.data.table(x=stats$strat, y=combo, by = colnames(combo), all = FALSE)
        
        p <- plot_vpc(.vpcdata = sub, .predcorr=predcorr,.y_axis_lab = "Concentration (ng/mL)")
        if(length(strat$facet)){
            p <- p +
                facet_wrap(strat$facet, scales="free",ncol=2)
        }
        if(!is.null(ymax)) p <- p+lims(y=c(NA,ymax))
        return(p)
    })

}

plots.raw <- plotCombos(stats=stats.raw.split.facet,strat=strat.raw,params=params,predcorr=FALSE)

plots.raw[[1]]

plots.predcorr <- plotCombos(stats=stats.predcorr.split,strat=strat.predcorr,params=params,predcorr=TRUE)

plots.predcorr.all <- plotCombos(stats=stats.predcorr.all,
                                 strat=list(split="DVID",facet="DEVPART"),
                                 params=params,
                                 predcorr=TRUE,
                                 ymax=4000)

plots.predcorr.all[[1]]

plots <- list(raw=plots.raw,
              predcorr=plots.predcorr,
              predcorr_all=plots.predcorr.all
              )

###  Section end: summarise the data and plot 


#### Section start: write figs ####
message("write figures")


ggwrite(plots.raw,file=file.res("vpcs_raw.pdf"),onefile=TRUE,canvas="A4",
        script=script)

ggwrite(plots.raw,file=file.res("vpcs_raw.png"),onefile=TRUE,canvas="A4",useNames=TRUE,
        script=script)


ggwrite(plots.predcorr,file=file.res("vpcs_predcorr.pdf"),onefile=TRUE,canvas="A4",
        script=script)

ggwrite(plots.predcorr,file=file.res("vpcs_predcorr.png"),onefile=TRUE,canvas="A4",useNames=TRUE,
        script=script)


ggwrite(plots.predcorr.all,file=file.res("vpcs_predcorr_all.pdf"),
        onefile=TRUE,canvas="A4",
        script=script)

ggwrite(plots.predcorr.all,file=file.res("vpcs_predcorr_all.png"),
        onefile=TRUE,canvas="A4",useNames=TRUE,
        script=script)


###### something like below should probably be included
if(F){
    ## get chunks of 8 plots and put them in a grid on a single patchwork page:
    tib = tibble(indexes = 1:length(all.plots),
                 groups = 1 + (indexes %/% 9))

    chunked.plots = map(.x=unique(tib$groups), function(.x) {
        p = patchwork::wrap_plots(all.plots[tib %>% filter(groups==.x) %>% pull(indexes)],ncol = 2,
                                  guides="collect",axis_titles = "collect",axes = "collect")
        return(p)
    })

    chunked.plots.24h = map(chunked.plots, function(.x) .x & coord_cartesian(xlim=c(0,24)))

    chunked.plots.log = map(chunked.plots, function(.x) .x & scale_y_log10() & annotation_logticks(sides = "l"))

    chunked.plots.log.24h = map(chunked.plots.log, function(.x) .x & coord_cartesian(xlim=c(0,24)))


    ggwrite(chunked.plots,file=file.res("vpcs_lin_all.pdf"),onefile=TRUE,canvas="A4",
            script=script)
    ggwrite(chunked.plots,file=file.res("vpcs_lin_all.png"),canvas="A4",useNames=FALSE,
            script=script)

    ggwrite(chunked.plots.24h,file=file.res("vpcs_lin_24h.pdf"),onefile=TRUE,canvas="A4",
            script=script)
    ggwrite(chunked.plots.24h,file=file.res("vpcs_lin_24h.png"),canvas="A4",useNames=FALSE,
            script=script)

    ggwrite(chunked.plots.log,file=file.res("vpcs_log_all.pdf"),onefile=TRUE,canvas="A4",
            script=script)
    ggwrite(chunked.plots.log,file=file.res("vpcs_log_all.png"),canvas="A4",useNames=FALSE,
            script=script)

    ggwrite(chunked.plots.log.24h,file=file.res("vpcs_log_24h.pdf"),onefile=TRUE,canvas="A4",
            script=script)
    ggwrite(chunked.plots.log.24h,file=file.res("vpcs_log_24h.png"),onefile=TRUE,useNames=FALSE,
            script=script)
}
###  Section end: write figs

