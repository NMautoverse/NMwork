# Plot DV vs PRED or IPRED

Plot DV vs PRED or IPRED

## Usage

``` r
plotDVpredIpred(
  .data = NULL,
  .file.mod = NULL,
  .dvcol = "DV",
  .predcol = "PRED",
  .AddSmooth = TRUE,
  .log = FALSE
)
```

## Arguments

- .data:

  dataset object to use for plotting. Must have all columns required for
  plotting

- .file.mod:

  the path to a completed NONMEM model which will be passed to
  \`NMdata::NMscanData()\` to read in the dataset

- .dvcol:

  NONMEM dependent variable column name (i.e. "DV" ). Will be plotted as
  the y-axis.

- .predcol:

  the population prediction column name (i.e. "PRED"). Will be plotted
  as the x-axis.

- .AddSmooth:

  TRUE/FALSE. If TRUE, will add a smooth line using method="loess"

- .log:

  TRUE/FALSE. If TRUE, will plot data on log-scale for both x and y axes
