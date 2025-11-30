# Plot CWRES/IWRES vs time (after dose, after first dose)

Plot CWRES/IWRES vs time (after dose, after first dose)

## Usage

``` r
plotResTime(
  .data = NULL,
  .file.mod = NULL,
  .ResCol = "CWRES",
  .TimeCol = "AFRLT",
  .NomtimeCol = NULL,
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

- .TimeCol:

  the time column name (i.e. "TIME", "AFRLT", "APRLT", "TAFD", etc.).
  Will be plotted as the x-axis.

- .NomtimeCol:

  nominal time column name. When this is non-NULL, this column will be
  used for grouping observations and plotting a median + 90 confidence
  interval for each nominal time point. Helpful when many observations
  occur at the same time point and are hard to interpret.

- .AddSmooth:

  TRUE/FALSE. If TRUE, will add a smooth line using method="loess"
