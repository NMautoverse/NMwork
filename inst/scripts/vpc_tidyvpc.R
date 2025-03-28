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
dir.main = "/data/prod_vx548_lsr_phase2_analysis/trunk/analysis/PK_review"
setwd(dir.main)
## source(".Rprofile")



##libraries
library(data.table)
library(devtools)
## devtools::load_all("/data/sandbox/trunk/analysis/NMsim/wdirs/NMdata")
## NMdata 0.1.9 needed
## devtools::load_all("~/wdirs/NMdata")
library(NMdata)
## NMsim 0.2.0 needed
## devtools::load_all("~/wdirs/NMsim")
library(NMsim)
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

library(ggplot2)


## functions
dir.funs.pain <- "/data/prod_cqp_code_library/trunk/DiseaseAreas/Pain/AnalysisTemplates/functions"
if(!file.exists("functions/getSource.R")){
    file.copy(file.path(dir.funs.pain,"getSource.R"),"functions/")
}
source("functions/getSource.R")
getSource(file="CQP_misc.R", dir.central=dir.funs.pain, dir.local="functions")
getSource(file="modelPaths.R", dir.central=dir.funs.pain, dir.local="functions")
getSource(file="plot_vpc.R", dir.central=dir.funs.pain, dir.local="functions")

script  <-  scriptLabel(dir.main,"scripts/vpc_tidyvpc.R")


### Section end: Initialization


#### Section start: Settings ####

## Get the run arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args)>0){
    file.mod <- args[1]
} else {
    ## file.mod <- "models/run017.mod"
    file.mod <- "models/12746.mod"
}

model <- modelPaths(file.mod,as.dt=T)


## just to know what variables are available
if(F){
    NMscanInput(model$mod) |>
        colnames()
}

## strat.raw <- list(split=c(params$col.dvid,"DEVPART"), facet="DOSESS")
## strat.raw <- list(split=c(params$col.dvid,"DEVPART"), facet="DOSE1")
## strat.predcorr=list(split=c(params$col.dvid))



## set the parameters of the vpc_template Rmd file
params <- list(
    ##dataset = minimal_data,
    file.mod = file.mod,
    ## philip: isnt this just dir.dest?
    ##dir_output = fnExtension(fnAppend(file.mod,"VPC"),""),
    ## run_script = scriptLabel,
    ## rmd_template = templateLabel,
    include_blq = TRUE,
    subproblems = 5,
    nsims = 100,
    sge=TRUE,
    nc = 18,
    reuse.results=TRUE,
#### Postprocessing
    ##covLookup = strat.vars, # used for creating plot filenames by their short name (i.e. "DOSE1_100mg" instead of "First Dose Amount (mg)_100mg" )
    ## redundant
    ## stratify_by = names(strat.vars),
    col.bin = "NFRLT", # the column used for binning the vpc stats, usually nominal time after first dose
#### Plotting
    ## unitDV = "ng/mL",
    lab.y="Concentration (ng/mL)",
    ## not used
    ## compound_name = c("SUZ","M6-SUZ"),
    col.dvid = "DVID"
)

## dir.dest is where the output files will be saved (i.e. html report and
## report-ready figures if requested)
dir.dest <- file.path(dirname(model$mod), paste0(model$run,"_VPC"))



plot.layouts <- list(
    ##  raw_byDevpart=list(
    ##      split="DVID",
    ##      facet=c("DEVPART"),
    ##      predcorr=FALSE)
    ## ,
    raw_byDose=list(
        split=c("DVID","DEVPART"),
        facet=c("DOSESS"),
        predcorr=FALSE)
   ,
    raw_Dose=list(
        split=c("DVID","DEVPART","DOSESS"),
        facet=NULL,
        predcorr=FALSE)
   ,
    pcorr_all=list(
        split="DVID",
        facet=NULL,
        predcorr=TRUE)
   ,
    pcorr=list(
        split=c("DVID","DEVPART"),
        facet=NULL,
        predcorr=TRUE)
)

plot.versions <- list(
    trans=list(
        lin=identity,
        log=function(p) p + scale_y_log10()
    ),
    subset=list(
        "all"=identity,
        "24h"=function(p) p + coord_cartesian(xlim=c(0,24))
    )
)


### Section end: Settings

##### User must ensure the BLQs are handled this way in NONMEM
filters <- NULL
if(params$include_blq){
    
    filters <- NMsim:::NMreadFilters(file=params$file.mod)
    ## filters[cond=="EXCLF.NE.0",cond:="EXCLF.GT.10"]
    filters[cond=="FLAG.NE.0",cond:="FLAG.GT.10"]
}





#### Section start: Run the simulations #### 
strat.vars <- unique(unlist(lapply(plot.layouts,function(x)c(x$split,x$facet))))

############ This is a usefult check. It should be re-enabled somewhere.
if(F){
    ## check that all variables listed in strat.vars are columns in dataset:

    cnames.data <- NMscanInput(model$mod) |>
        colnames()

### If we want to do this, we need to read input data set and check column names.
    
    carry.out <- c("TIME","EVID","DV","LLOQ","BLQ",
                   strat.vars,params$col.dvid,
                   params$col.bin)
    
    covs.miss = setdiff(strat.vars,cnames.data) 
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


simres <- NMreadSim(path.res,wait = TRUE)
simres <- simres[EVID==0]

## simres[,DVID:=PCTESTCD]
## simres[,DVID:="VX-973"]
## simres[,NFRLT:=TIME]

### deriving obs from first sim. I think this should be sufficient for in-sample VPCs
data.obs <- simres[NMREP==first(NMREP)&model.sim==first(model.sim)] 

###  Section end: Run the simulations



#### Section start: summarise the data and plot  ####

message("Summarise the data and plot")

########## automated but pretty ugly

### generate a dt with all the plots. Include all necessary
### stratification variables as list or structured formula string.

plot.layouts2 <- lapply(1:length(plot.layouts),
                        function(n) {
                            x <- plot.layouts[[n]]
                            x$name.plot <- names(plot.layouts)[n]
                            x$split <- paste(sort(unique(x$split)),collapse=" ")
                            x
                        })

dt.plots <- rbindlist(lapply(plot.layouts2,as.data.table),fill=TRUE)
paste.no.na <- function(x,...) {
    paste(x[!is.na(x)],...)
}
dt.plots[,rowplot:=.I]
dt.plots[,strat:=paste.no.na(c(split,facet),collapse=" "),by=rowplot]
dt.plots

##plot.layouts[,.GRP,by=.(c())]
plot.layouts3 <- lapply(1:length(plot.layouts),
                        function(n) {
                            x <- plot.layouts[[n]]
                            x$name.plot <- names(plot.layouts)[n]
                            ## x$split <- paste(sort(unique(x$split)),collapse=" ")
                            x$var <- c(x$split,x$facet)
                            x$type.var <- c(rep("split",length(x$split)),
                                            rep("facet",length(x$facet)))
                            x$split <- NULL
                            x$facet <- NULL
                            x
                        })
plot.layouts3

dt.vars <- rbindlist(
    lapply(plot.layouts3,as.data.table)
   ,fill=TRUE)
dt.vars

## strat.vars <- dt.vars[,unique(var)]

### for each stratification, run stats, then generate plots. Name
### plots based on plot.layouts and plot.versions.
vars.combs <- dt.vars[,.(var=unique(var)),by=.(name.plot,predcorr)]
dt.plots.strat <- vars.combs[,.(stratvars=paste(sort(unique(var)),collapse=":")),by=c("name.plot","predcorr")]
dt.plots.strat[,NSTRAT:=.GRP,by=.(stratvars,predcorr)]
dt.plots.strat

dt.plot.vars <- merge(dt.vars,dt.plots.strat,by=c("name.plot","predcorr"))

list.plot.strat <- split(dt.plot.vars,by="NSTRAT")




### plotting function
plotStats <- function(stats,dt.plots,ymax=NULL){
    
    split.dtplots <- split(dt.plots,by=c("name.plot"))
    

    plots <- lapply(1:length(split.dtplots),ymax=ymax,FUN=function(n,ymax){
        
        dtplot <- split.dtplots[[n]]
        predcorr <- dtplot[,unique(predcorr)]
        split <- strsplit(dtplot$split,split=" ")[[1]]
        
        stats.obs.split <- split(stats$obs,by=split)
        stats.stats.split <- split(stats$stats,by=split)
        stats.stratbin.split <- split(stats$.stratbin,by=split)
        stats.strat.split <- split(stats$strat,by=split)
        pls <- lapply(1:length(stats.obs.split),function(n){
            name.split <- names(stats.obs.split)[[n]]
            sub <- list(obs=stats.obs.split[[name.split]],
                        stats=stats.stats.split[[name.split]],
                        .stratbin=stats.stratbin.split[[name.split]],
                        strat=stats.strat.split[[name.split]]
                        )
            
            p <- plot_vpc(.vpcdata = sub, .predcorr=predcorr,.y_axis_lab = params$lab.y)+
                labs(subtitle=name.split)

            
            if(length(dtplot$facet)&&!is.na(unique(dtplot$facet))){
                p <- p +
                    facet_wrap(unique(dtplot$facet), scales="free",ncol=2)
            }
            if(!is.null(ymax)) p <- p+lims(y=c(NA,ymax))
            return(p)
        })
        ## split predcorr facet
        names(pls) <- names(stats.obs.split)
        pls
    })
    
    plots
}



### calculate stats on all data and all stratifications

### if we want to save stats we need the output dir
if(!dir.exists(dir.dest)){dir.create(dir.dest)}
file.res <- function(stem) file.path(dir.dest,fnAppend(stem,model$run))

### TODO: For binning on NFRLT or similar, summarize how many obs are
### dropped because of missing bin?

pls <- list()
for(Nstrat in 1:length(list.plot.strat)){
    
    dt.strat <- list.plot.strat[[Nstrat]]

    vars.strat <- dt.strat[,strsplit(unique(stratvars),split=":")[[1]]]
    pcorr <- dt.strat[,unique(predcorr)]


### TODO: should the consoring part be included here if include_blq is
### FALSE?

    stats <- 
        observed(data.obs, x = TIME, y = DV) |>
        simulated(simres, x = TIME, y = Y) |>
        censoring(blq = as.logical(BLQ), lloq = LLOQ) |>
        stratify(reformulate(unlist(vars.strat))) 
    
    if(pcorr){
#### TODO: the predcorr binning is hard-coded
        stats <- stats |>
            binning(bin = "ntile", nbins = 8, xbin = "xmean") |>
            predcorrect(pred=PRED)
    } else {
        stats <- stats |>
            binning(bin = !!sym(params$col.bin)) 
    }
    
    stats  <- stats |>
        vpcstats(qpred = c(0.05, 0.5, 0.95), conf.level = 0.95)

    ## save stats
    ## saveRDS()
    
#### Generate plots
    
    plots.this <- dt.plots[name.plot%in%dt.strat$name.plot]

    
    p1 <- plotStats(stats,plots.this,ymax=NULL)

    ## Bug: names should go in first
    ##browser()

    names(p1) <- 
        plots.this[,paste0(name.plot,
                           "_",gsub(" ","_",split),
                           ifelse(is.na(facet),"",paste0("_facet_",paste(facet,collapse="_")))),
                   by=1:nrow(plots.this)][,V1]
    
    pls[[Nstrat]] <- p1
    
    ## names(pls)[Nstrat] <- 
    ##     plots.this[,paste0(name.plot,
    ##                       "_",gsub(" ","_",split),
    ##                       ifelse(is.na(facet),"",paste0("_facet_",paste(facet,collapse="_")))),
    ##                       by=1:nrow(plots.this)][,V1]

    ##    names(pls)[Nstrat] <- names(p1)
    ##names.pls <- names(pls)
    ##names.pls[Nstrat] <- paste(dt.plots[,paste(split,predcorr)])
    ## names(plots) <- paste(dtplot[,paste(split,predcorr)])
    
}
## pls.backup <- pls
if(F){
    pls <- pls.backup
}
if(F){
    ## Exploration
    length(pls)
    lapply(pls,length)
    pls[[2]]
    pls[[3]]
    patchwork::wrap_plots(pls[[4]])
    lapply(pls[4],class)
    pls[4]

    names(pls)
    lapply(pls,names)
}

### collapse list into one list of plots - keeping names from both layers
pls2 <- lapply(1:length(pls),function(n){
    x <- pls[[n]]
    name.main <- names(pls)[n]
    names(x) <- paste(name.main,names(x))
    x
})
pls3 <- do.call(c,pls2)
## names(pls3)

### How do we handle log version and a 24h version? I kept some code further down.

dt.vers <- expand.grid(lapply(plot.versions,names)) |> setDT()

######## dt.vers2 is a way to generate name suffixes for the plot versions
dt.vers2 <- copy(dt.vers)
dt.vers2[,(names(dt.vers2)):=lapply(1:length(dt.vers2),function(x)paste(colnames(dt.vers2)[x],dt.vers2[[x]],sep="_") )]
##dt.vers2[,(names(dt.vers2)):=lapply(.SD,function(col,colname) paste(colname,col,sep="_"),colname=names(dt.vers2))]


all.plots.0 <- lapply(1:length(pls3),function(np){
    
    p0 <- pls3[[np]]
    prows <- list()
    ## apply all versions to the plot
    ## plist <- lapply(plot.versions,function(pver)lapply(pver,function(pverfun)pverfun(p)))
    for(nr in 1:nrow(dt.vers)){
        p <- p0
        for(nc in 1:ncol(dt.vers)) {
            p <- plot.versions[[colnames(dt.vers)[nc]]][[as.matrix(dt.vers)[nr,nc]]](p)
        }
        prows[nr] <- list(p)
        ##prows[nr] <- p
    }

    length(prows)
    
    ## browser()
    
    names(prows) <- do.call(function(...)paste(...,sep="_"),dt.vers2)
    prows
})

names(all.plots.0) <- names(pls3)

eliminateLevel <- function(list){
    all.els <- lapply(1:length(list),function(nx){
        el.list <- list[[nx]]
        if(is.null(el.list)) return(el.list)
        ##browser()    
        names(el.list) <- paste(names(list)[nx],names(el.list),sep="_")
        el.list
    })
    list2 <- do.call(c,all.els)
    list2
}

all.plots.1 <- eliminateLevel(all.plots.0)
all.plots <- eliminateLevel(all.plots.1)

if(F){
    all.plots.1 <- lapply(1:length(all.plots.0),function(nx){
        el.x <- all.plots.0[[nx]]
        ##browser()    
        names(el.x) <- paste(names(all.plots.0)[nx],names(el.x),sep="_")
        el.x
    })
    all.plots <- do.call(c,all.plots.1)


    all.plots.2 <- lapply(1:length(all.plots),function(n){
        x <- all.plots[[n]]
        if(is.null(x)) return(x)
        name.main <- names(all.plots)[n]
        names(x) <- paste(name.main,names(x))
        x
    })
    all.plots <- do.call(c,all.plots.2)
}

names(all.plots) <- gsub(".","_",NMsim:::cleanStrings(names(all.plots)),fixed=TRUE)


names(all.plots)
##lapply(all.plots,names)

ggwrite(all.plots,file=file.path(dir.dest,"vpc.png"),script=script,useNames=T,time=model$lab,canvas="a4")
ggwrite(all.plots,file=file.path(dir.dest,"vpcs_allplots.pdf"),script=script,time=model$lab,onefile=TRUE)

