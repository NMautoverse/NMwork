# Update the pirana-style comments in top of a Nonmem control stream

Update the pirana-style comments in top of a Nonmem control stream

## Usage

``` r
NMwritePreamble(
  file.mod,
  lines,
  description = NULL,
  based.on = NULL,
  author = NULL,
  write.file = TRUE
)
```

## Arguments

- file.mod:

  The control stream to edit

- description:

  The desription to put in the preamble comments. If a function is
  provided, te function will be run on the old description, and the
  result will be used.

- based.on:

  A control stream that the model was based on (will be a comment in
  preamble). Functions not supported - todo.

- author:

  Name of author to credit in preamble. Functions not supported - todo.

- write.file:

  Write to file? If not, resulting control stream will be returned to
  user as lines, and nothing else done.

## Value

lines (character) for new control stream
