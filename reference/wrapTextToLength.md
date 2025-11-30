# Does a script fit on a single page?

Take script path and test if it is too long to fit on a typical page. If
it is too long, wrap it onto multiple lines. This is more involved than
one may think as shown here.

## Usage

``` r
wrapTextToLength(
  text =
    "Source code: path/to/some/file/thatIsVery/Very/Veryvery/looooooooonnnnng/scripts/testing.R",
  page_width = 6.26,
  font_size = 7,
  font_family = "sans"
)
```

## Arguments

- text:

  string to be wrapped

- page_width:

- font_size:

- font_family:
