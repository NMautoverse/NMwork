# Plot CWRES/IWRES vs population predictions (PRED)

Plot CWRES/IWRES vs population predictions (PRED)

## Usage

``` r
plotResPred(
  .data = NULL,
  .file.mod = NULL,
  .ResCol = "CWRES",
  .PredCol = "PRED",
  .AddSmooth = FALSE
)
```

## Arguments

- .data:

  dataset object to use for plotting. Must have all columns required for
  plotting

- .file.mod:

  the path to a completed NONMEM model which will be passed to
  \`NMdata::NMscanData()\` to read in the dataset

- .ResCol:

  the residuals column name (i.e. "RES", "CWRES", "NPDE", etc.). Will be
  plotted as the y-axis.

- .PredCol:

  the population prediction column name (i.e. "PRED"). Will be plotted
  as the x-axis.

- .AddSmooth:

  TRUE/FALSE. If TRUE, will add a smooth line using method="loess"
