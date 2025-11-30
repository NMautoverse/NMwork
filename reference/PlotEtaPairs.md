# Plots Eta pairs scatterplot matrix with lower triangle being a scatterplot, diagonal being a density plot, and upper triangle being text of the correlation and p-value. This function takes (1) the time-constant etas dataset, and (2) the output of NMwork:::EtaPairsUpperTriangle(), (3) the output of NMwork:::EtaPairsScatMat() (and NMwork::EtaPairsLowerTriangle), to plot a full eta pairs matrix plot. Taken with minimal modifications from GGally::ggscatmat().

Plots Eta pairs scatterplot matrix with lower triangle being a
scatterplot, diagonal being a density plot, and upper triangle being
text of the correlation and p-value. This function takes (1) the
time-constant etas dataset, and (2) the output of
NMwork:::EtaPairsUpperTriangle(), (3) the output of
NMwork:::EtaPairsScatMat() (and NMwork::EtaPairsLowerTriangle), to plot
a full eta pairs matrix plot. Taken with minimal modifications from
GGally::ggscatmat().

## Usage

``` r
PlotEtaPairs(
  data,
  columns = 1:ncol(data),
  color = NULL,
  alpha = 1,
  shape = 1,
  size = 0.7,
  corMethod = "pearson"
)
```
