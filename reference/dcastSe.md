# A standard-evaluation interface to \`data.table::dcast()\`

A standard-evaluation interface to \`data.table::dcast()\`

## Usage

``` r
dcastSe(data, l, r, ...)
```

## Arguments

- data:

  data set to transpose (widen)

- l:

  left-hand side variables as character vector. Result will be
  long/vertical in these variables.

- r:

  left-hand side variables as character vector. Result will be wide in
  these variables.

- ...:

  Additional arguments passed to \`data.table::dcast()\`.
