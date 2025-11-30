# Plot parameter estimation iterations

Plot parameter estimation iterations

## Usage

``` r
plotTrace(file.mod, pars = NULL, label.by = "parameter", col.label = NULL, ...)
```

## Arguments

- file.mod:

  NONMEM model file, must be completed run

- pars:

  a parameter table to map NONMEM parameters to their labels

- label.by:

  a column in the parameter table which will be used for merging the
  parameter table and the parameter labels in the .ext file, as read by
  \`NMdata::NMreadExt()\`. usually this column is 'parameter' and has
  values such as 'THETA1', 'THETA2', .... , 'OMEGA(1,1)', 'OMEGA(2,2)',
  ... 'SIGMA(1,1)', etc.

- col.label:

  the column in the parameter table which we will use for labeling the
  plot panels.

- ...:

  Passed to \`NMdata::NMreadExt()\`.
