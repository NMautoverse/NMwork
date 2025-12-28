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
    pars <- createParameterTable(file.mod,script="/data/home/philipde/wdirs/NMwork/tests/testthat/test_printParameterTable.R")

    
    res1 <- printParameterTable(pars,engine="pmtables",format="tex",label.pmtables="tab:test")
    res1
    
    res2 <- printParameterTable(pars,engine="pmtables",format="testOutput/myfile_pmtables.tex",label.pmtables="tab:test")

    res2

    res3 <- printParameterTable(pars,engine="pmtables",format="pdf",label.pmtables="tab:test")
    res3
    fs::file_show(res3)

    printParameterTable(pars,engine="pmtables",format="pdf",label.pmtables="tab:test")
    

    res4 <- printParameterTable(pars,engine="pmtables",format="testOutput/pmtables_01.pdf",label.pmtables="tab:test")
    res4
    file_show(res4)


    res5 <- printParameterTable(pars,engine="pmtables",format="testOutput/pmtables_01.pdf",label.pmtables="tab:test",script="myscript.R")
    file_show(res5)
})
