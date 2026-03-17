if(F){
library(devtools)
unloadNamespace("NMwork")
unloadNamespace("NMsim")
unloadNamespace("NMdata")

load_all("~/wdirs/NMdata")
load_all("~/wdirs/NMwork")

res <- newModel(newfile="testOutput/newmodel_01.mod",
         file.mod="testData/nonmem/xgxr134.mod",
         values=list("THETA(1)"=list(init=1)),
         write.file=FALSE
         )


res

}

