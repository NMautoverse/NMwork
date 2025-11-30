# Gets pairwise data between all columns of data, then calculates correlation and p value of correlation to plot in upper triangle of eta pairs plot.. This function returns the dataset in a format that will be directly used to plot the correlation statistics in the upper triangle of the pairs plot. The output of this function gets sent to NMwork::PlotEtaPairs(). This function is taken with minimal modifications from GGally:::uppertriangle().

Gets pairwise data between all columns of data, then calculates
correlation and p value of correlation to plot in upper triangle of eta
pairs plot.. This function returns the dataset in a format that will be
directly used to plot the correlation statistics in the upper triangle
of the pairs plot. The output of this function gets sent to
NMwork::PlotEtaPairs(). This function is taken with minimal
modifications from GGally:::uppertriangle().

## Usage

``` r
EtaPairsUpperTriangle(
  data,
  columns = 1:ncol(data),
  color = NULL,
  corMethod = "pearson"
)
```
