# context("getSource")

test_that("Basic",{

    fileRef <- "testReference/getSource_01.rds"
    outfile <- "testOutput/hello_world.R"
    unlink(outfile,force=TRUE)

    ## dtr <- getSource(file=c("xgxr134.mod","xgxr134.ext"),"testData/nonmem",dir.local="testOutput")
    dtr <- getSource(file=c("hello_world.R"),"testData/scripts/",dir.local="testOutput")

    res <- readLines(outfile)
    res <- sub("on ....-..-.. using","",res)
    # expect_equal_to_reference(res,fileRef)
    expect_snapshot_value(res,style="serialize")
    ## testthat::test_file()
    ##    snapshot_accept(res)
    ##snapshot_accept('getSource')
})
