plotEstCorr <- function(file.lst,pars=NULL){

    library(ggpubr)
    library(NMdata)
    library(flextable)
    
    ## file.lst <- "models/run372.lst"
    model.lab <- basename(file.lst) |> fnExtension("")


    ## source("functions/addOmegaCorr.R")
    cor <- fnExtension(file.lst,"cor") |> NMreadCov(auto.ext=FALSE)
    ##fnExtension(file.lst,"cor") |> NMreadCov()
    

    
    dt.cor <- mat2dt(cor,as.fun="data.table")

    if(!is.null(pars)){
        pars <- pars[par.type%in%cc(THETA,OMEGA,SIGMA)]
        pars[,par.type:=factor(par.type,levels=cc(THETA,OMEGA,SIGMA))]
        pars[,symbol:=reorder(symbol,frank(pars,par.type,i,j))]
        pars[,symbol2:=paste(par.name,symbol)]
        pars[,symbol2:=reorder(symbol2,frank(pars,par.type,i,j))]
        
        ## add fix info and labels
        dt.cor <- mergeCheck(
            dt.cor
           ,
            pars[,.(parameter,i.i=i,j.i=j,par.name.i=par.name,par.type.i=par.type,FIX.i=FIX,symbol.i=symbol,label.i=label,symbol2.i=symbol2,i.in.ext=1)]
           ,
            by.x="parameter.i"
           ,by.y="parameter"
           ,all.x=TRUE
           ,quiet=FALSE
        )

        dt.cor <- mergeCheck(
            dt.cor
           ,
            pars[,.(parameter,i.j=i,j.j=j,par.name.j=par.name,par.type.j=par.type,FIX.j=FIX,symbol.j=symbol,label.j=label,symbol2.j=symbol2,j.in.ext=1)],
            by.x="parameter.j"
           ,by.y="parameter"
           ,all.x=TRUE
           ,quiet=FALSE
        )
        dt.cor <- dt.cor[!is.na(i.in.ext) & !is.na(j.in.ext)]
    } else {
        dt.cor[,symbol.i:=parameter.i]
        dt.cor[,symbol.j:=parameter.j]
        dt.cor[,symbol2.i:=parameter.i]
        dt.cor[,symbol2.j:=parameter.j]
    }
    
    

    ## avoid fixed
    dt.cor <- dt.cor[FIX.i==0&FIX.j==0]
    dt.cor <- dt.cor[i!=j]

    if(T){
        dt.cor[,par.type.i:=factor(par.type.i,levels=cc(THETA,OMEGA,SIGMA))]
        dt.cor[,par.type.j:=factor(par.type.j,levels=cc(THETA,OMEGA,SIGMA))]
        ## recode row and column according to ordering
        dt.cor[,counter.i:=.GRP,by=.(match(as.character(par.type.i),c("THETA","OMEGA","SIGMA")),i.i,j.i)]
        dt.cor[,symbol2.i:=reorder(symbol2.i,counter.i)]
        ## dt.cor[,counter.j:=.GRP,keyby=.(par.type.j,i.j,j.j)]
        dt.cor[,counter.j:=.GRP,by=.(match(as.character(par.type.j),c("THETA","OMEGA","SIGMA")),i.j,j.j)]
        dt.cor[,symbol2.j:=reorder(symbol2.j,counter.j)]
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
    dt.cor[,symbol2.j:=factor(symbol2.j,levels=rev(levels(dt.cor$symbol2.j)))]
    p.corr <- ggplot(dt.cor,aes(symbol2.i,symbol2.j,fill=value.bin))+
        geom_tile()+
        rotate_x_text()+
        coord_fixed()+
        labs(x="",y="",fill="Correlation")

### todo table with all correlations > 0.8 ordered by correlation

    dt.cor[,value.abs:=abs(value)]
    tab1 <- dt.cor[value.abs>=.8][
        order(-value.abs)][
       ,.(par.name.i,label.i,par.name.j,label.j,value)]

    ft1 <- flextable(tab1) |> autofit()
    
    list(plot_est_corr=p.corr,tab_est_corr_large=ft1)
    
}
