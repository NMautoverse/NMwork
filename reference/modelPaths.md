# Create a convenient set of file paths and other info related to a model.

Create a convenient set of file paths and other info related to a model.

## Usage

``` r
modelPaths(
  file,
  as.dt = FALSE,
  col.name = "mod",
  must.exist = FALSE,
  simplify = TRUE
)
```

## Arguments

- file:

  path to control stream, or almost any other model file

- as.dt:

  Return model information in a data.table?

- must.exist:

  Make sure the provided paths match files? Default is not to but it can
  be a good first check in a script.

- simplify:

  If only one model supplied, and \`as.dt=FALSE\`, should the format be
  simplified to a list of model elements, rather than a list of (model)
  lists of elements?
