# Create new control stream based on an existing model

Create new control stream based on an existing model

## Usage

``` r
newModel(
  newfile,
  file.mod,
  update = TRUE,
  values,
  description = NULL,
  based.on,
  author = NULL,
  write.file = TRUE,
  overwrite = FALSE,
  modify = NULL,
  inits = NULL,
  filters = NULL
)
```

## Arguments

- newfile:

  The new control stream file path.

- file.mod:

  The path to the control stream to use as the starting point.

- update:

  Update initial values with final paremeter estimates of \`file.mod\`.

- values:

  Specify specific initial values (passed to NMwriteInits).

- description:

  A Pirana style description field before the control stream code.

- based.on:

  A Pirana style field before the control stream code.

- author:

  A Pirana style field before the control stream code.

- write.file:

  Write to newfile? Default is TRUE. See \`overwrite\` too.

- overwrite:

  If newfile exists, overwrite it?

- modify:

  List of modification to do to control stream. Passed to
  NMsim:::modifyModel().

## Details

Pirana fields are only applied if existing in \`file.mod\`.

## Examples

``` r
## Reference Base model is run11. You are trying two differnt new
## models run21 and run22. update=TRUE to use final parameter
## estimates as new inits.
newmod <- newModel(file.mod="run11.mod",newfile="run21.mod",update=TRUE)
#> Error in NMdata:::getLines(file = file.mod, lines = lines, simplify = FALSE): When using the file argument, file has to point to an existing file.
## manual edits of run21.mod. Execute run21.mod
## NMexec(newmod)

# add run22 based on the code in run21 because we need most of the
# same edits, so file.mod=run21. But you are testing run22 against
# run11, so based.on="run11". Don't update the initial values
# based on run21. We want them to be identical to run21, so use
# update=FALSE.
newmod <- newmodel(file.mod="run21.mod",newfile="run22.mod",based.on="run11.mod",update=FALSE)
#> Error in newmodel(file.mod = "run21.mod", newfile = "run22.mod", based.on = "run11.mod",     update = FALSE): could not find function "newmodel"
```
