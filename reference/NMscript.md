# Run script on models using Rscript, optionally using sge

Run script on models using Rscript, optionally using sge

## Usage

``` r
NMscript(
  script,
  file.mod,
  ...,
  sge = FALSE,
  wait = FALSE,
  nc = 4,
  stdout_file,
  stderr_file
)
```

## Arguments

- script:

  The actual command-line command you want to run on the grid (including
  all arguments)

- file.mod:

  First argument sent to script. If length\>1, the script is run on all
  the elements.

- ...:

  Additional arguments to script.

- sge:

  Send to cluster using \`qsub\`?

- wait:

  Only used if sge is \`FALSE\`.

- nc:

  Only used if sge is \`TRUE\`. The number of cores you want the script
  to run on the grid with.

- stdout_file:

  Only used if sge is \`TRUE\`. The file where the job output will be
  printed.

- stderr_file:

  Only used if sge is \`TRUE\`. The file where the job errror output
  will be printed.
