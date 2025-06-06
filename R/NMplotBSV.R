##' Generate distribution plots of between-occasion variability terms
##' from Nonmem
##' @param data A dataset - will be converted to data.frame so
##'     data.table is OK.
##' @param file.mod If used, parameter names that ETA's are associated
##'     with will be derived by looking at \code{$PRED} or \code{$PK}
##'     in the control stream. Essentially, it looks for where the
##'     ETA's are found and look for a parameter name to the left of a
##'     `=` sign. Alternatively, you can hard-code the ETA-parameter
##'     relationship using \code{regex.eta}.
##' @param regex.eta A regular expression defining the naming of the
##'     ETA's of interest. See `file.mod` too.
##' @param parameters Character vector of model parameters to
##'     include. This will drop ETA's that are not associated with a
##'     parameter in this vector.
##' @param col.id The name of the id column in data. Default is ID
##'     like Nonmem. This is not fully working if col.id is different
##'     from `ID`.
##' @param covs.num Names of columns containing numerical covariates
##'     to plot the random effects against.
##' @param covs.char Names of columns containing categorical
##'     covariates to plot the random effects against.
##' @param fun.file If saving plots, this function can be used to
##'     translate the file names. The inputs given to the function
##'     argument are "iov_pairs.png" and "iov_covs_n.png".
##' @param save Save the generated plots?
##' @param script If saving the plots, a stamp to add. See ggstamp.
##' @param return.data If TRUE, the identified ETA's together with
##'     subject id and covariates will be returned in both wide and
##'     long format. If FALSE, you just get the plots.
##' @import ggplot2
##' @import data.table
##' @import stats
##' @import NMdata
##' @importFrom GGally ggpairs
##' @family Plotting
##' @export

NMplotBSV <- function(data,regex.eta,names.eta=NULL,parameters=NULL,col.id="ID",covs.num,covs.char,save=FALSE,show=TRUE,return.data=FALSE,title=NULL,file.mod,structure="flat",use.phi,auto.map,keep.zeros=FALSE,keep.zeros.pairs=FALSE,...){

    
#### Section start: dummy variables, only not to get NOTE's in pacakge checks ####

    value  <- NULL
    ..density..  <- NULL
    predicted <- NULL
    val.cov <- NULL

### Section end: dummy variables, only not to get NOTE's in pacakge checks
    

    if(missing(covs.num)) covs.num <- NULL
    if(missing(covs.char)) covs.char <- NULL
    if(missing(file.mod)) file.mod <- NULL
    if(missing(auto.map)||is.null(auto.map)){
        auto.map <- !is.null(file.mod)
    }
    if(missing(use.phi)) use.phi <- NULL
    if(is.null(use.phi)){
        use.phi <- !is.null(file.mod)
    }
    if(!is.null(names.eta)){
        cols.missing <- setdiff(cc(i,label),colnames(names.eta))
        if(length(cols.missing)>0){
            stop("If names.eta is provided, it must be a data.frame including columns called `i` and `label`.")
        }
    }

### will be used in ggpairs
    points.and.smooth <- function(data, mapping, method="lm", ...){
        if(nrow(data)==0) return(ggplot(data.table(a=0,b=0)))
        p <- ggplot(data = data, mapping = mapping) + 
            geom_point() + 
            geom_smooth(method=method, formula=y~x, ...)
        p
    }

    
    data <- copy(as.data.table(data))
    setnames(data,col.id,"ID")
    col.id <- "ID"
    
    pkpars <- findCovs(data,by=col.id,as.fun="data.table")

    
### extract eta values from phi file or output tables
    if(use.phi){

        file.phi <- fnExtension(file.mod,"phi")
        if(file.exists(file.phi)){
            dt.phi <- NMreadPhi(file.phi,as.fun="data.table")
            dt.etas <- dt.phi[par.type=="ETA" ]
            if(nrow(dt.etas)==0) {
                ## this will happen for muref models where .phi does
                ## not contain etas
                use.phi <- FALSE
            }
        }
    }
    if(!use.phi) {
        
        ## if(missing(regex.eta)) regex.eta <- "^ETA[1-9]$|^ET[A]{0,1}[1-9][0-9]$"
        if(missing(regex.eta)) regex.eta <- "^ETA[1-9][0-9]*$"

        names.etas <-
            names(pkpars)[
                grepl(regex.eta,names(pkpars))
            ]
        
        ## if specified to be a covariate, drop the eta
        names.etas <- setdiff(names.etas,c(covs.num,covs.char))
        
        names.etas <- setdiff(names.etas,col.id)
        dt.etas.tab <- pkpars[,c(col.id,names.etas),with=FALSE]
        dt.etas <- melt(dt.etas.tab,id.vars=col.id,variable.name="parameter")
        
        dt.etas[,i:=as.numeric(gsub("[^0-9]","",parameter))]
        ## TODO: This was ETAs from output tables. check if all etas
        ## are found. Preferably based on OMEGA size in ext. If not,
        ## if i is consecutive. However preferably, the subsetting is
        ## done first.
    }

### reduce to needed etas
    dt.etas <- dt.etas[get(col.id) %in% data[,get(col.id)]]
    ## only the ones that vary
    dt.etas[,eta.var:=any(value!=0),by=.(parameter)]
    dt.etas.var <- dt.etas[eta.var>0]

### check that dt.etas.var is unique in IDxETA.
    if(any(duplicated(dt.etas.var[,.(ID,i)]))){
        
        stop("ID and ETA indexes are not unique. Are IDs disjoint in input data? This is not supported by NMplotBSV and may be a bug in the input data set.")

    }
    
### get eta labels
    if(auto.map){
        if(is.null(names.eta)){
            if(!file.exists(file.mod)){
                stop("Either provide `file.mod` with `auto.map=TRUE` or `names.eta` as a data.frame associating ETAs and parameters.")
            }
            names.eta <- NMrelateOne(file.mod,par.type="omega",as.fun="data.table")
            ##names.eta <- identifyEtas(file.mod)
            cat("the following eta relationships found:\n")
            print(names.eta)
        }
    } 
    
    ## merge etas and labels
    if(!is.null(names.eta)){
        dt.etas.var <- mergeCheck(dt.etas.var,names.eta[,.(i,label)],by="i",all.x=TRUE,quiet=TRUE)
    } else {
        dt.etas.var[,label:=parameter]
    }

### this should be a more general subset. A list with one or two
### of i and label. Like subset=list(i=c(1,2,3),label=cc(CL))
    if(!is.null(parameters)){
        dt.etas.var <- dt.etas.var[label%in%parameters]
    }
    
##### derive covariates
    if(!all(covs.num%in%names(pkpars))){
        covs.num.drop <- setdiff(covs.num,colnames(pkpars))
        warning(paste0("The following numerical parameters were not found:\n",paste(covs.num.drop,collapse=", ")))
        covs.num <- setdiff(covs.num,covs.num.drop)
        ## use only covariates that vary
        covs.num <- colnames(findVars(pkpars[,covs.num,with=F]))
    }
    if(!all(covs.char%in%names(pkpars))){
        covs.char.drop <- setdiff(covs.char,colnames(pkpars))
        warning(paste0("The following covariates were not found:\n",paste(covs.char.drop,collapse=", ")))
        covs.char <- setdiff(covs.char,covs.char.drop)
        ## use only covariates that vary
        if(length(covs.char)){
            covs.char <- colnames(findVars(pkpars[,covs.char,with=F]))
        }
    }
    ## add covariates to etas
    if(pkpars[,is.character(get(col.id))]){
        dt.etas.var[,(col.id):=as.character(get(col.id))]
    }
    ## dt.etas.var <- mergeCheck(dt.etas.var,pkpars[,c(col.id,covs.num,covs.char),with=FALSE],by=col.id)
    dt.etas.var <- mergeCheck(dt.etas.var,pkpars[,c(col.id,setdiff(colnames(pkpars),colnames(dt.etas.var))),with=FALSE],by=col.id,quiet=TRUE)

    ## only keep non-0 ETAs (should be an option)
    etas.l <- dt.etas.var
    if(!keep.zeros){
        etas.l <- etas.l[value!=0]
    }
    
    

    ## make a wide version for ggpairs
    ## names.etas.var <- setdiff(colnames(etas.w),"ID")

    names.etas.var <- as.character(etas.l[,unique(label)])
    if(!length(names.etas.var)){        
        message("No BSV random effects found in parameter table.")
        return(invisible(NULL))
    }

    
    all.output <- list()
    
    
### ggpairs
    etas.l.pairs <- copy(dt.etas.var)
    if(!keep.zeros.pairs){
        etas.l.pairs <- etas.l.pairs[value!=0]
    }
    names.etas.pairs <- as.character(etas.l.pairs[,unique(label)])
    etas.w <- dcast(etas.l.pairs,ID~label,value.var="value")
    iiv.pairs <- ggpairs(etas.w,columns=names.etas.pairs,lower=list(continuous=points.and.smooth),title=title)

    all.output[["iiv.pairs"]]  <- iiv.pairs

    
#### this is the histograms of non-zeros and with gaussian approximations
    dat <- etas.l
    dat[,param:=label]
    grid <- with(dat, seq(min(value), max(value), length = 100))

    normaldens <-
        dat[,list(
            predicted=grid,
            density=dnorm(grid, mean(value), sd(value))
        ),
        by="param"]
    
    gh2 <- ggplot(data = dat,aes(x = value)) +
        geom_vline(xintercept=0,colour="grey",linetype="dashed")+
        geom_histogram(aes(y = ..density..)) + 
        geom_line(data = normaldens, aes(x = predicted, y = density), colour = "red",size=1)+
        facet_wrap(~param,scales="free")+
        labs(title=title)


    all.output[["hists.etas"]]  <- gh2
    
    
    plot.qq <- ggplot(dat,aes(sample=value))+
        geom_hline(yintercept=0,colour="grey",linetype="dashed")+
        geom_vline(xintercept=0,colour="grey",linetype="dashed")+
        geom_qq_line(colour=2,size=1.5)+
        geom_qq()+
        ## the theroretical identity line
        geom_abline(slope=1,intercept=0,linetype=2)+
        facet_wrap(~param)+
        labs(x="Theoretical",y="Observed",title=title)
    all.output[["qq.bsv"]]  <- plot.qq
    
    ## IIV random effects vs covariates
    if(!is.null(covs.num)){        
        
        etas.l2.n <- etas.l[,c(col.id,"param","value",covs.num),with=FALSE]

        ## etas.covs.n <- melt.data.table(etas.l2.n,variable.name="cov",value.name="val.cov",measure.vars=setdiff(colnames(etas.l2.n),c("ID","param","value")))
        etas.covs.n <- melt.data.table(etas.l,variable.name="cov",value.name="val.cov",measure.vars=covs.num)
        
### I think this one removes covariates that are etas. Not really
### necessary and if anything, it should be done much earlier.
        ## data.plot <- etas.covs.n[!grepl(regex.eta,cov)]
        
        p.iiv.covsn <- lapply(split(etas.covs.n,by="cov"),
                              function(dt){ggplot(dt,aes(val.cov,value))+
                                               geom_point(shape=1, size=1, alpha=0.2)+
                                               geom_smooth(method="lm", color="red",formula=y~x)+
                                               facet_wrap(~param,scales="free")+
                                               labs(title=title,x=dt[,unique(cov)],y="Eta")+
                                               aes_string(...)
                              })

        if(structure=="flat"){
            all.output <- c(all.output,
                            setNames(p.iiv.covsn,paste0("iiv_covsn_",names(p.iiv.covsn)))
                            )
        } else {
            all.output[["iiv.covsn"]] <- p.iiv.covsn
        }
        
    }
    if(length(covs.char)>0){
        
        ## etas.l2.c <- etas.l[,c(col.id,"param","value",covs.char),with=F]
        ## DT <- data.table(etas.l2.c)
        ## DT2 <- melt(DT,measure.vars=covs.char,id.vars=c("ID","param","value"),value.name="val.cov",value.factor=T)
        DT2 <- melt(etas.l,measure.vars=covs.char,value.name="val.cov",value.factor=T)
        sets <- split(DT2,by="variable")
        p.iiv.covsc <- lapply(sets,function(dat){
            if(is.numeric(dat[,val.cov])) dat[,val.cov:=factor(val.cov)]
            ggplot(dat,aes(val.cov,value))+
                geom_boxplot(outlier.shape=NA,colour="blue")+
                geom_hline(yintercept=0,linetype=2) +
                geom_jitter(height=0,width=.4,alpha=.5)+
                facet_wrap(~param,scales="free_y")+
                labs(title=title,x=dat[,unique(variable)],y="Eta")+
                aes_string(...)
        })
        if(structure=="flat"){
            all.output <- c(all.output,
                            setNames(p.iiv.covsc,paste0("iiv_covsc_",names(p.iiv.covsc)))
                            )
        } else {
            all.output[["iiv.covsc"]] <- p.iiv.covsc
        }
    }

    ## if there are no plots to return, we return NULL (instead of a list of length 0).
    if(length(all.output)==0&&!return.data){return(NULL)}
    if(return.data){
        ## all.output[["etas"]] <- etas.w
        all.output[["etas"]] <- etas.l
    }

    all.output
}
