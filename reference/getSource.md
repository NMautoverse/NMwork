# Get and source an R script.

Get file from remote library and inport it into project if it does not
exist. Then source it.

## Usage

``` r
getSource(
  file,
  dir.central = NULL,
  dir.local,
  overwrite = FALSE,
  source.directly = FALSE,
  silent = F
)
```

## Arguments

- file:

  File name of wanted file

- dir.central:

  Folder path of wanted file

- dir.local:

  Where to put the file if imported, and where to look for already
  imported files. Default is getwd().

- overwrite:

  Owerwrite previously imported file?

- source.directly:

  Enables direct sourcing of the central file copy. This bypasses the
  whole concept of the function but it is useful when developing while
  using a function. Especially if your debugger in the editor is linking
  to a file that you will edit while debugging. It gives a warning
  because it is not recommended in final code.

- silent:

  Disables printning. Mainly used in testing.

## Value

None. Sources the specified file into the global environment.
