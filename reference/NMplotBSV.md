# Generate distribution plots of between-occasion variability terms from Nonmem

Generate distribution plots of between-occasion variability terms from
Nonmem

## Usage

``` r
NMplotBSV(
  data,
  regex.eta,
  names.eta = NULL,
  parameters = NULL,
  col.id = "ID",
  covs.num,
  covs.char,
  save = FALSE,
  show = TRUE,
  return.data = FALSE,
  title = NULL,
  file.mod,
  structure = "flat",
  use.phi,
  auto.map,
  keep.zeros = FALSE,
  keep.zeros.pairs = FALSE,
  ...
)
```

## Arguments

- data:

  A dataset - will be converted to data.frame so data.table is OK.

- regex.eta:

  A regular expression defining the naming of the ETA's of interest. See
  \`file.mod\` too.

- parameters:

  Character vector of model parameters to include. This will drop ETA's
  that are not associated with a parameter in this vector.

- col.id:

  The name of the id column in data. Default is ID like Nonmem. This is
  not fully working if col.id is different from \`ID\`.

- covs.num:

  Names of columns containing numerical covariates to plot the random
  effects against.

- covs.char:

  Names of columns containing categorical covariates to plot the random
  effects against.

- save:

  Save the generated plots?

- return.data:

  If TRUE, the identified ETA's together with subject id and covariates
  will be returned in both wide and long format. If FALSE, you just get
  the plots.

- file.mod:

  If used, parameter names that ETA's are associated with will be
  derived by looking at `$PRED` or `$PK` in the control stream.
  Essentially, it looks for where the ETA's are found and look for a
  parameter name to the left of a \`=\` sign. Alternatively, you can
  hard-code the ETA-parameter relationship using `regex.eta`.

- fun.file:

  If saving plots, this function can be used to translate the file
  names. The inputs given to the function argument are "iov_pairs.png"
  and "iov_covs_n.png".

- script:

  If saving the plots, a stamp to add. See ggstamp.
