test_that("kable",{

    file.mod <- "testData/nonmem/xgxr134.mod"
    pars <- createParameterTable(file.mod)

    
    res1 <- printParameterTable(pars,engine="kable",format="tex")

    res1

    res2 <- printParameterTable(pars,engine="kable",format="testOutput/myfile.tex")
    res2

})

test_that("pmtables",{


    file.mod <- "testData/nonmem/xgxr134.mod"
    pars <- createParameterTable(file.mod)

    
    res1 <- printParameterTable(pars,engine="pmtables",format="tex",label.pmtables="tab:test")

    
    res2 <- printParameterTable(pars,engine="pmtables",format="testOutput/myfile_pmtables.tex",label.pmtables="tab:test")

    res2

    res2 <- printParameterTable(pars,engine="pmtables",format="pdf",label.pmtables="tab:test")



})
