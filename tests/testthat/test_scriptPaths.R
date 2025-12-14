## context("test_scriptPaths")


test_that("Basic",{
    fileRef <- "testReference/scriptPaths_01.rds"

    res <- scriptPaths("myscript.R")

    # expect_equal_to_reference(res,fileRef)
    expect_snapshot_value(res,style="serialize")

   })


