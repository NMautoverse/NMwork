##' Plot correlation matrix heatmap from completed NONMEM run
##' @param file.lst `.lst` file from completed NONMEM run
##' @param pars a table of parameter names and labels to use for renaming parameters to descriptive labels
##' @import ggplot2
##' @importFrom NMdata fnExtension NMreadCov mat2dt NMreadExt mergeCheck cc
##' @importFrom flextable flextable autofit
##' @export

plotEstCor <- function(file.lst,pars=NULL,label.by="parameter",col.label=NULL){

    # file.lst = "models/pk/028/028.lst"
    # pars = partab.plots
    # col.label = "par.label.symbol"
    ## file.lst <- "models/run372.lst"
    model.lab <- basename(file.lst) |> fnExtension("")


    ## source("functions/addOmegaCorr.R")
    file.cor <- fnExtension(file.lst,"cor")
    if(!file.exists(file.cor)) stop("correlation estimate (.cor file) not found.")
    cor <- NMreadCov(file.cor,auto.ext=FALSE)
    
    
    dt.cor <- mat2dt(cor,as.fun="data.table")
    ## add FIX from .ext
    ext <- NMreadExt(file.lst,auto.ext=TRUE,return="pars",as.fun="data.table")
    dt.cor <- mergeCheck(dt.cor,ext[,.(parameter,FIX.i=FIX,par.type.i=par.type,i.i=i,j.i=j)],by.x="parameter.i",by.y="parameter",quiet=TRUE)
    dt.cor <- mergeCheck(dt.cor,ext[,.(parameter,FIX.j=FIX,par.type.j=par.type,i.j=i,j.j=j)],by.x="parameter.j",by.y="parameter",quiet=TRUE)

    ## Add parameter labels from pars    
    if(!is.null(pars)){
        ## can mergeLabel be applied

        dt.cor <- mergeLabel(pars=dt.cor,
                             tab.label=pars,
                             by="parameter",
                             col.label=col.label,
                             suffix="i"
                             )

        dt.cor <- mergeLabel(pars=dt.cor,
                             tab.label=pars,
                             by="parameter",
                             col.label=col.label,
                             suffix="j"
                             )
        
        ## pars <- pars[par.type%in%cc(THETA,OMEGA,SIGMA)]
        ## pars[,par.type:=factor(par.type,levels=cc(THETA,OMEGA,SIGMA))]
        ## pars[,symbol:=reorder(symbol,frank(pars,par.type,i,j))]
        ## pars[,symbol2:=paste(par.name,symbol)]
        ## pars[,symbol2:=reorder(symbol2,frank(pars,par.type,i,j))]
        
        ## ## add fix info and labels
        ## dt.cor <- mergeCheck(
        ##     dt.cor
        ##    ,
        ##     pars[,.(parameter,i.i=i,j.i=j,par.name.i=par.name,par.type.i=par.type,FIX.i=FIX,symbol.i=symbol,label.i=label,symbol2.i=symbol2,i.in.ext=1)]
        ##    ,
        ##     by.x="parameter.i"
        ##    ,by.y="parameter"
        ##    ,all.x=TRUE
        ##    ,quiet=FALSE
        ## )

        ## dt.cor <- mergeCheck(
        ##     dt.cor
        ##    ,
        ##     pars[,.(parameter,i.j=i,j.j=j,par.name.j=par.name,par.type.j=par.type,FIX.j=FIX,symbol.j=symbol,label.j=label,symbol2.j=symbol2,j.in.ext=1)],
        ##     by.x="parameter.j"
        ##    ,by.y="parameter"
        ##    ,all.x=TRUE
        ##    ,quiet=FALSE
        ## )
        ##dt.cor <- dt.cor[!is.na(i.in.ext) & !is.na(j.in.ext)]
        dt.cor <- dt.cor[!is.na(get(paste0("parameter",".i"))) &
                         !is.na(get(paste0("parameter",".j")))
                         ]
    } else {
        dt.cor[,(paste(col.label,"i",sep=".")):=parameter.i]
        dt.cor[,(paste(col.label,"j",sep=".")):=parameter.j]
        dt.cor[,label.i := (paste(col.label,"i",sep="."))]
        dt.cor[,label.j := (paste(col.label,"j",sep="."))]
        ## dt.cor[,symbol.j:=parameter.j]
        ## dt.cor[,symbol2.i:=parameter.i]
        ## dt.cor[,symbol2.j:=parameter.j]
    }
    
    

    ## avoid fixed
    dt.cor <- dt.cor[FIX.i==0&FIX.j==0]
    dt.cor <- dt.cor[i!=j]
    

    if(F){
        ### Bug in this section
        
        dt.cor[,par.type.i:=factor(par.type.i,levels=cc(THETA,OMEGA,SIGMA))]
        dt.cor[,par.type.j:=factor(par.type.j,levels=cc(THETA,OMEGA,SIGMA))]
        ## recode row and column according to ordering
        ## match.i = c(1,2,3) for par.type.i = c("THETA","OMEGA","SIGMA")
        dt.cor[,match.i:=match(as.character(par.type.i),c("THETA","OMEGA","SIGMA"))]
        dt.cor[,match.j:=match(as.character(par.type.j),c("THETA","OMEGA","SIGMA"))]
        
        dt.cor[,(paste(col.label,"i",sep=".")):=.GRP,keyby=.(match.i,i.i,j.i)]
        dt.cor[,(paste(col.label,"j",sep=".")):=.GRP,keyby=.(match.j,i.j,j.j)]
        ## dt.cor[,label.i:=reorder(label.i,counter.i)]
        ## dt.cor[,counter.j:=.GRP,keyby=.(par.type.j,i.j,j.j)]
        
        
        ## dt.cor[,counter.i:=.GRP,keyby=.(match(as.character(par.type.i),c("THETA","OMEGA","SIGMA")),i.i,i.j)]
        dt.cor[,counter.i:=.GRP,keyby=.(match.i,j.i,j.i)]
        #TODO: there is no label.i if we don't provide param table.
        dt.cor[,label.i:=reorder(label.i,counter.i)]
        
        dt.cor[,counter.j:=.GRP,keyby=.(match.j,i.j,j.j)]
        dt.cor[,label.j:=reorder(label.j,counter.j)]
    }

    if(F){
        setorder(dt.cor,par.type.i,i.i)
        dt.cor[,row:=.GRP,by=.(par.type.i,i.i)]
        setorder(dt.cor,par.type.j,j.j)
        dt.cor[,col:=.GRP,by=.(par.type.j,j.j)]
        dt.cor[,col:=.I]

        setorder(dt.cor,par.type.i,i.i,j.j)
        
        dt.cor[,symbol2.i:=reorder(symbol2.i,row)]
        dt.cor[,symbol2.j:=reorder(symbol2.j,col)]
    }
    
    
    ## use fixed thresholds for correlation colors
    dt.cor[,value.bin:=cut(abs(value),c(0,.25,.5,.7,.8,.95,1))]




    ## Repeat upper and lower
    ## dt.cor.2 <- rbind()
    
    ## avoid diagonal

    ## levels(dt.cor$symbol.j) <- rev(levels(dt.cor$symbol.j))
    # dt.cor[,label.j:=factor(label.j,levels=rev(levels(dt.cor$label.j)))]
    dt.cor[,label.i := factor(label.i, levels = pars[[col.label]])]
    dt.cor[,label.j := factor(label.j, levels = pars[[col.label]])]
    
    p.corr <- ggplot(dt.cor,aes(label.i,label.j,fill=value.bin))+
        geom_tile()+
        coord_fixed()+
        labs(x="",y="",fill="Correlation")

    loadres <- requireNamespace("ggpubr",quietly=TRUE)
    if(loadres) {
        p.corr <- p.corr+rotate_x_text()
    } else {
        message("Install ggpubr to include rotation of x axis tick labels.")
    }
### todo table with all correlations > 0.8 ordered by correlation

    dt.cor[,value.abs:=abs(value)]
    tab1 <- dt.cor[value.abs>=.8][
        order(-value.abs)][
       ,.(label.i,label.j,value)]

    ft1 <- flextable(tab1) |> autofit()
    
    list(plot=p.corr,corr_large=ft1)
    
}
