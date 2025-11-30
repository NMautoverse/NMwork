# read list of correlation matrices from .cor file. takes the path to the .cor file as an argument. output is a list with length equal to the number of tables output in the .cor file. Use: when more than one table is listed in .cor file and you want all of them

read list of correlation matrices from .cor file. takes the path to the
.cor file as an argument. output is a list with length equal to the
number of tables output in the .cor file. Use: when more than one table
is listed in .cor file and you want all of them

## Usage

``` r
read_cor_mat(file.lst, tableno = "max")
```

## Arguments

- file.lst:

  NONMEM model output file, must be completed run

- tableno:

  which correlation matrix to output in case there are multiple. tableno
  must be either one of the character strings min, max, all or an
  integer greater than zero
