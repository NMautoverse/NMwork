library(data.table)
library(NMdata)
library(NMsim)

## Get the setup ready for NMsim
path.candidates <- c(## metworx
    "/opt/NONMEM/nm75/run/nmfe75"
    ## custom linux
   ,"/opt/nonmem/nm751/run/nmfe75"
    ## Ahmed
   ,"c:/nm75g64/run/nmfe75.bat"
)
NMdataConf(path.nonmem = NMsim:::prioritizePaths(path.candidates)) ## path to whichever NONM

NMdataConf(##path.nonmem = "/opt/NONMEM/nm75/run/nmfe75"
           ## path.nonmem = "/opt/nonmem/nm751/run/nmfe75"
          ## ,dir.sims="~/NMsim_vignette/simtmp/simtmp-intro"
          dir.sims="simtmp-intro"
          ,dir.res="simres-intro"
           )


NMexec("~/wdirs/NMwork/inst/nonmem/xgxr134.mod",sge=FALSE)
