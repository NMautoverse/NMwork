# Eta pairs scatterplot matrix lower triangle and diagonal elements. This function takes (1) the time-constant etas dataset, and (2) the output of NMwork:::EtaPairsLowerTriangle() to create the lower triangle and diagonal of the eta pairs plot. The output of this function gets sent to NMwork::PlotEtaPairs(). Taken with minimal modifications from GGally::scatmat().

Eta pairs scatterplot matrix lower triangle and diagonal elements. This
function takes (1) the time-constant etas dataset, and (2) the output of
NMwork:::EtaPairsLowerTriangle() to create the lower triangle and
diagonal of the eta pairs plot. The output of this function gets sent to
NMwork::PlotEtaPairs(). Taken with minimal modifications from
GGally::scatmat().

## Usage

``` r
EtaPairsScatMat(
  data,
  columns = 1:ncol(data),
  color = NULL,
  alpha = 0.6,
  shape = 1,
  size = 1
)
```
