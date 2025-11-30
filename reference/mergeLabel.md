# Add parameter labels from a parameter table

Add parameter labels from a parameter table

## Usage

``` r
mergeLabel(
  pars,
  tab.label = NULL,
  by = "parameter",
  col.label = "parameter",
  suffix = NULL
)
```

## Arguments

- pars:

  parameters table

- tab.label:

  table with a column for labels

- by:

  column to merge pars and tab.label

- col.label:

  column in \`tab.label\` to use for labelling

- suffix:

  not sure

## Details

This is internally used in plotEstCor and plotTrace. Not sure it should
its worth exporting.
