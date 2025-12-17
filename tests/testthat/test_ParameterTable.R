## context("Parameter tables")


## load_all()
test_that("basic",{
    ## options(warn=0)
    ## test footnotes at log and logit transformations
    file.mod <- "testData/nonmem/xgxr134.mod"
    pars <- createParameterTable(file.mod)
    res1 <- printParameterTable(pars)

    expect_snapshot_value(res1,style="serialize")

    pars[,.N,by=.(par.type,trans)]



    ## log and logit
    pars[par.name=="THETA(1)",trans:="logit"]
    pars[par.name=="THETA(1)",trans]
    pars[,.N,by=.(par.type,trans)]

    res2 <- printParameterTable(pars)

    expect_snapshot_value(res2,style="serialize")

    ## no transformations
    pars[,trans:="none"]
    pars[par.name=="THETA(1)",trans]
    pars[,.N,by=.(par.type,trans)]
    res3 <- printParameterTable(pars)

    expect_snapshot_value(res3,style="serialize")
})



