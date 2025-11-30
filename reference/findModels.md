# find model paths winin a directory

Finds models created by NMsim, BBR, and PSN.

## Usage

``` r
findModels(
  dir,
  pattern = ".*.lst$",
  methods = c("main", "bbr"),
  recursive = FALSE,
  ext.mod.bbr = ".mod"
)
```

## Arguments

- methods:

  Prioritized vector of model types to search for. "main" means psn and
  nmsim because they copy results to main dir.

## Details

"main": any lst in main dir "bbr": any lst generated this way: look for
x.mod files, look for x/ and for x/x.lst. The x.lst is the result. But a
x.mod is used to compare to results from "main". When x.lst (derived
from x.mod) matches a lst file from main, the order of methods is used
to prioritize.
