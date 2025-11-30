# Overwrite values in one data.frame if they are available in another.

Repair x with y

## Usage

``` r
mergeCoal(x, y, by, cols.coal, as.fun)
```

## Arguments

- x:

  The initial data.frame

- y:

  A data.frame to prioritize overc \`x\`.

- by:

  Columns to merge by

- cols.coal:

  Columns to overwrite values from \`y\` if available.

## Details

Non-na values in y will be used o overwrite columns in x at the rows
matched using \`by\` columns.

Merges must be done using the same "by" columns for all rows. If rows
needs to be merged using varying by columns, the merges must be done
sequentially.
