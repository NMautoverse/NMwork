 ## if(F){

  library(devtools)
  unloadNamespace("NMwork")
  unloadNamespace("NMsim")
  unloadNamespace("NMdata")

  load_all("~/wdirs/NMdata")
  load_all("~/wdirs/NMsim")
  load_all("~/wdirs/NMwork")

test_that("theta init and fix",{
  
    fileRef <- "testReference/newModel_01.rds"

  res0 <- newModel(newfile="testOutput/newModel_01.mod",
                  file.mod="testData/nonmem/xgxr134.mod",
                  values=list("THETA(1)"=list(init=1,fix=1)),
                  write.file=FALSE
                  )


  res <- NMreadSection( lines=res0,section="theta")

  expect_snapshot(res,fileRef)  
})



## 

test_that("fix=TRUE",{
  
  res0 <- newModel(newfile="testOutput/newModel_01.mod",
                  file.mod="testData/nonmem/xgxr134.mod",
                  values=list("THETA(1)"=list(init=1,fix=TRUE)),
                  write.file=FALSE
                  )


  res <- NMreadSection( lines=res0,section="theta")[1]
  
})

### inits using NMwriteInits() value arg
values=list( "theta(2)"=list(init=1),FIX=1)
