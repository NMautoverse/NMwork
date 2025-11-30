# Plot correlation matrix heatmap from completed NONMEM run This function differs from \`plotEstCorr\` in that it will plot a full matrix with diagonals representing standard error (rather than half matrix with no diagonals). Requires successful covariance step in NONMEM.

Plot correlation matrix heatmap from completed NONMEM run This function
differs from \`plotEstCorr\` in that it will plot a full matrix with
diagonals representing standard error (rather than half matrix with no
diagonals). Requires successful covariance step in NONMEM.

## Usage

``` r
plotCorrelationHeatmap(
  file.lst,
  pars = NULL,
  merge.pars.ext.by = "parameter",
  col.label = NULL,
  tableno = "max",
  ...
)
```

## Arguments

- file.lst:

  NONMEM model output file, must be completed run

- pars:

  a parameter table to map NONMEM parameters to their labels

- merge.pars.ext.by:

  a common column in the parameter table and .ext file which will be
  used for merging the parameter table and the parameter labels in the
  .ext file, as read by \`NMdata::NMreadExt()\`. usually this column is
  'parameter' and has values such as 'THETA1', 'THETA2', .... ,
  'OMEGA(1,1)', 'OMEGA(2,2)', ... 'SIGMA(1,1)', etc.

- col.label:

  the column in the parameter table which we will use for labeling the
  plot panels.

- ...:

  Passed to \`NMdata::NMreadExt()\`.
