# Plot Inidividual profiles observed (DV) and predicted (PRED and IPRED)

Plot Inidividual profiles observed (DV) and predicted (PRED and IPRED)

## Usage

``` r
plotIndivProfilesDvPredIpred(
  .data = NULL,
  .timecol = "TIME",
  .dvcol = "DV",
  .predcol = "PRED",
  .ipredcol = "IPRED",
  .log = FALSE
)
```

## Arguments

- .data:

  dataset to use for plotting

- .timecol:

  column to use for x-axis (TIME)

- .dvcol:

  column to use for y-axis for observations (DV)

- .predcol:

  column to use for y-axis for population predictions (PRED)

- .ipredcol:

  column to use for y-axis for individual predictions (PRED)

- .log:

  should we use a logged y-axis?
