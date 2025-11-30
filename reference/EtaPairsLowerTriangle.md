# Gets pairwise data between all columns of data, then provide organizing columns to allow the data to be plotted in a pairwise way in a matrix plot. This function returns the dataset in a format that will be directly used to plot the scatterplot in the lower triangle of the pairs plot. . The output of this function gets sent to NMwork:::EtaPairsScatMat.R (slightly modified version of GGally::scatmat), and then to NMwork::PlotEtaPairs(). Taken with minimal modifications from GGally:::lowertriangle().

Gets pairwise data between all columns of data, then provide organizing
columns to allow the data to be plotted in a pairwise way in a matrix
plot. This function returns the dataset in a format that will be
directly used to plot the scatterplot in the lower triangle of the pairs
plot. . The output of this function gets sent to
NMwork:::EtaPairsScatMat.R (slightly modified version of
GGally::scatmat), and then to NMwork::PlotEtaPairs(). Taken with minimal
modifications from GGally:::lowertriangle().

## Usage

``` r
EtaPairsLowerTriangle(data, columns = 1:ncol(data), color = NULL)
```
