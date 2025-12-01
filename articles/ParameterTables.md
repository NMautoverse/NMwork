# NMwork - Create Nonmem Parameter Tables

## Objectives

- Create parameter tables, using annotation of control stream parameter
  sections

- Print parameter tables to various formats (R console, jpg, word, ppt,
  pdf, include in Rmd document)

- Modify subsets of information provided in parameter section
  annotations

- Use flexible tools to generate information when parameter sections are
  less diligently annotated

## Introduction

A table of model parameter estimates is some of the most basic model
information. Nevertheless, it is often cumbersome to get a decently
annotated table of parameter estimates. In Nonmem control streams, the
definition of parameters such as ’s, ’s and ’s is handled in `$THETA`,
`$OMEGA` and `$SIGMA` while the definition of variables such as `CL` or
`KIN` is handled elsewhere, typically in `$PK`, `$PRED` or `$ERROR`.
This allows for flexibility in model definition but it also leaves some
book keeping to the user for interpretation of the parameter estimates.
One can choose to avoid interpretation of parameters by outputting the
variables themselves in `$TABLE`. That way, they can simulate out the
(often) more easily interpretable variable values without relying on
translation between parameters and variables. However, by interpreting
the parameter values (preferably including appropriate transformation to
physiological variables), one can make direct use of other properties
estimated by Nonmem such as parameter precision as estimated in the
\$COVARIANCE step. This vignette presents a flexible and easy-to-use
framework for handling this bookkeeping, with the benefit of automated
generation of annotated parameter tables.

Let’s break the generation of parameter tables into four steps.

1.  Annotation of control streams (down to the user)

2.  Compilation of annotations, parameter estimates and precision, if
    present (tools available in `NMdata`)

3.  Formatting of parameter table information
    ([`NMwork::createParameterTable`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md))

4.  Printing of parameter table information
    ([`NMwork::printParameterTable`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md))

Provided that step 1. is done informatively, step 2 is fully wrapped
into step 3. This means, you can end up with as simple a workflow as
this

[TABLE]

Model: xgxr134

``` r
file.mod <- system.file("nonmem/xgxr134.mod",package="NMwork")
partab <- createParameterTable(file.mod)
printParameterTable(partab,format="html") 
```

In the rest of this document, we will see what all this did, how you can
get it to work, and how you can fill in the information if this was not
done during model development (the annotation in step 1 above).

While
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
and
[`printParameterTable()`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md)
are offered by `NMwork`, the underlying functionality is largely
provided by `NMdata`. For the best experience, make sure to keep NMdata
up to date from CRAN.

### When to Use This

The methods shwon in this document are intended to automate generation
of publication-ready parameter estimate tables. Because of the
informative value of such tables and because they are easy to generate
(especially with a few simple habits of how to document parameter
interpretation in control streams), the recommendation is to use them
during model development as well as for reporting.

## Creating the Parameter Table (`createParameterTable()`)

### Formatting Features

[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
uses the following columns to format the parameter table. Hence, large
parts of this document describes techniques to fill in the columns of
interest to
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md).
These are (all characters):

- `symbol`: Typically, the variable name associated with the parameter,
  e.g. “CL”.
- `label`: The parameter label, e.g. “Clearance”.
- `trans`: Parameter transformation (character strings).
- `unit`: The parameter unit, e.g. “L/h”.
- `panel`: Grouping variable. Can be an intermediary short-hand code
  like “struct” or “iiv”.

[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
will use those columns for formatting, if available. Sensible default
values are being used where possible. If no `symbol` column is
available, the parameter name (i.e. `THETA(1)`) will be used. The
default parameter description is “label, symbol (unit)” (unit) If
`label` and/or `unit` are missing, the resulting parameter description
will just omit them.

### Example 1: Annotation of Control Stream Parameter Sections

This approach is the simplest to show because it is what we already did
in the introduction. All the preparations are done in the control
stream, and `createParamaterTable()` can do the rest of the work.

Take a look at the parameter sections of the control stream of the model
we just read for the parameter table above.

    ;; format: %idx: %symbol ; %label [%unit] ; %trans
    $THETA  (.1)             ; 1 : TVKA ; Absorption rate [1/h] ; log
    $THETA  (3)             ; 2 : TVV2 ;  Central volume [L] ; log
    $THETA  (1)             ; 3 : TVCL ; Clearance [L/h] ; log
    $THETA  (4)             ; 4 : TVV3 ; Peripheral volume [L] ; log
    $THETA  (-1)             ; 5 : TVQ ; Intercomparmental clearance [L/h] ; log
    $THETA .1              ; 6 : AGECL ; Age effect on clearance []; log
    $THETA .1              ; 7 : WEIGHTCL ; Body-weight effect on clearance []; log
    $THETA .1              ; 8 : MALECL ; Male effect on clearance []; log
    $OMEGA 0 FIX ; 1 : KA 
    $OMEGA 0.1   ; 2 : V2 
    $OMEGA 0.1   ; 3 : CL 
    $OMEGA 0 FIX ; 4 : V3 
    $OMEGA 0 FIX ; 5 : Q  
    ;; format.sigma: %symbol - %label ; %trans
    $SIGMA 0.1    ; SigP - Prop err ; propErr
    $SIGMA 0 FIX  ; SigA - Add err ; addErr

Each `$THETA` definition is annotated with descriptive information such
as the variable “symbol” (TVKA), label (Absorption rate), unit (1/h),
and transformation (log). I know I skipped the `idx` column too, but
that is intentional. While such a counter can be very useful during
model development,
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
does not need it.

Notice a few things about the format - The format of the annotations is
up to the user as long as it is consistent within parmeter types
(i.e. all `$THETA`s annotations must be consistent). - Delimiters can
vary. between idx and symbol, a colon (:) is used, between symbol and
label it a semicolon, and the unit is in brackets. The latter acrually
means the at the delimitor between label and unit is a left bracket
(\[), while the delimitor between unit and trans is a composite of a
right bracket and a semicolon (\];). Spaces surrounding delimiters are
dropped, so “\] ;”, “\] ;” and “\];” all mean the same. By the way,
tabulator characters are treated like spaces. - As a note of how the
parameters have been documented, the modeler has left a comment string
starting with “format:” where the column names in the desired table have
been labeled with %-signs. In fact, that’s what
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
used to learn how to generate the table. - The `$OMEGA` annotations only
contain `idx` and `symbol`. In this case all the `ETA`s are used for
log-normal distributed between-subject variability so a diligent
annotation of these parameters are redundant. Since the formatting of
`idx` and `symbol` (and of course the delimiter between them) are
consistent with the `$THETA` annotations. The missing fields, `label`,
`unit` and `trans` will be filled with `NA`s. - The `$SIGMA` parameters
are annotated differently. The annotation columns are `symbol`, `label`,
and `trans`, but the delimiters are different, and `unit` is not
included. - The annotation scheme used for `$SIGMA`s is documented in a
comment starting with `format.sigma:`.
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
finds this automatically.

[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
collects

``` r
head(partab,2)
```

[TABLE]

### Example: Specify Formats

If you have a control stream with consistent annotation but without the
“`format`” lines to document the annotation format in the control
stream, you can include that format in `createParameterTables()` using
the `args.ParsText` argument.

``` r
createParameterTable(file.lst=file.mod,args.ParsText=list(format="%idx - %symbol - %unit",format.omega="%idx-%symbol"))
```

The reason for the argument name `args.ParsText` is that that all you
provide will be passed to
[`NMdata::NMreadParsText()`](https://nmautoverse.github.io/NMdata/reference/NMreadParsText.html)
which is what
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
uses to process the control stream. If formats are passed here, they
overrule what may be defined in the control stream.

### Example: Specify Selected Annotation Values

What we have seen so far relies on the modeler to diligently annotate
all parameters, and all text in the parameter table is coming directly
from the control stream. However, it may be desired to edit some of the
information.
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
has the arguments `df.repair` and `by.repair` to do this.
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
uses
[`NMdata::mergeCoal()`](https://nmautoverse.github.io/NMdata/reference/mergeCoal.html)
to do this. The basic idea is that non-`NA` values in `df.repair` are
inserted into (overwriting) the parameter table by matching the columns
provided in `by.repair`. `by.repair` is like `by` in merges, but
[`mergeCoal()`](https://nmautoverse.github.io/NMwork/reference/mergeCoal.md)
prioritizes `df.repair` over the parameter annotations in the control
stream.

``` r
df.repair <- data.frame(symbol="WEIGHTCL",label="Bodyweight effect on clearance")
partab <- createParameterTable(file.mod,df.repair=df.repair,by.repair="symbol") |>
    printParameterTable(format="html")
```

`by.repair` can be a character vector of any length. But the main
limitation of this approach is that you can only use one `by.repair`
vector. This means you need one variable (say `symbol`) to be
consistently read from the control stream. You could merge by
`parameter` (like `THETA1`, `OMEGA(1,1)` etc.) but that breaks the
benefit of linking parameters to variables in the control stream.

### Example: Construct Annotations First

Sometimes you may need to edit the table in more detail that what
`df.repair` and `by.repair` allow for.
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
offers the argument `df.labs` for the user to completely format the
labels prior to the table generation. This allows for say multiple steps
of `mergeCoal` or other manual editing, or whatever automated techniques
the user may have.

``` r
dt.labs <- NMreadParsText(file.mod)
df.repair <- data.frame(symbol="WEIGHTCL",label="Bodyweight effect on clearance")
dt.labs <- NMwork:::mergeCoal(dt.labs,df.repair,by="symbol",as.fun="data.table")
dt.labs[par.type=="OMEGA",label:=paste("IIV:",label)]
createParameterTable(file.mod,df.labs=dt.labs) |>
    printParameterTable(format="html")
```

[TABLE]

Model: xgxr134

A function to consider for automated detection of relationship between
parameters and Nonmem variables (like what is called `symbol` above) is
[`NMdata::NMrelate()`](https://nmautoverse.github.io/NMdata/reference/NMrelate.html).

``` r
NMrelate(file.mod)
```

| model   | par.name   | par.type |   i |   j | nrep.LHS | nrep.par | LHS      | label          | code                 | parameter  |
|:--------|:-----------|:---------|----:|----:|---------:|---------:|:---------|:---------------|:---------------------|:-----------|
| xgxr134 | THETA(1)   | THETA    |   1 |  NA |        1 |        1 | LTVKA    | LTVKA          | LTVKA=THETA(1)       | THETA1     |
| xgxr134 | THETA(2)   | THETA    |   2 |  NA |        1 |        1 | LTVV2    | LTVV2          | LTVV2=THETA(2)       | THETA2     |
| xgxr134 | THETA(3)   | THETA    |   3 |  NA |        1 |        1 | LTVCL    | LTVCL          | LTVCL=THETA(3)       | THETA3     |
| xgxr134 | THETA(4)   | THETA    |   4 |  NA |        1 |        1 | LTVV3    | LTVV3          | LTVV3=THETA(4)       | THETA4     |
| xgxr134 | THETA(5)   | THETA    |   5 |  NA |        1 |        1 | LTVQ     | LTVQ           | LTVQ=THETA(5)        | THETA5     |
| xgxr134 | THETA(6)   | THETA    |   6 |  NA |        1 |        1 | AGECL    | AGECL          | AGECL=THETA(6)       | THETA6     |
| xgxr134 | THETA(7)   | THETA    |   7 |  NA |        1 |        1 | WEIGHTCL | WEIGHTCL       | WEIGHTCL=THETA(7)    | THETA7     |
| xgxr134 | THETA(8)   | THETA    |   8 |  NA |        1 |        1 | MALECL   | MALECL         | MALECL=THETA(8)      | THETA8     |
| xgxr134 | OMEGA(1,1) | OMEGA    |   1 |   1 |        1 |        1 | KA       | KA             | KA=EXP(MU_1+ETA(1))  | OMEGA(1,1) |
| xgxr134 | OMEGA(2,2) | OMEGA    |   2 |   2 |        1 |        1 | V2       | V2             | V2=EXP(MU_2+ETA(2))  | OMEGA(2,2) |
| xgxr134 | OMEGA(3,3) | OMEGA    |   3 |   3 |        1 |        1 | CL       | CL             | CL=EXP(MU_3+ETA(3))  | OMEGA(3,3) |
| xgxr134 | OMEGA(4,4) | OMEGA    |   4 |   4 |        1 |        1 | V3       | V3             | V3=EXP(MU_4+ETA(4))  | OMEGA(4,4) |
| xgxr134 | OMEGA(5,5) | OMEGA    |   5 |   5 |        1 |        1 | Q        | Q              | Q=EXP(MU_5+ETA(5))   | OMEGA(5,5) |
| xgxr134 | SIGMA(1,1) | SIGMA    |   1 |   1 |        1 |        2 | SIGP     | SIGP           | SIGP=SIGMA(1,1)      | SIGMA(1,1) |
| xgxr134 | SIGMA(1,1) | SIGMA    |   1 |   1 |        2 |        2 | Y        | Y - SIGMA(1,1) | Y=F+F\*ERR(1)+ERR(2) | SIGMA(1,1) |
| xgxr134 | SIGMA(2,2) | SIGMA    |   2 |   2 |        1 |        2 | SIGA     | SIGA           | SIGA=SIGMA(2,2)      | SIGMA(2,2) |
| xgxr134 | SIGMA(2,2) | SIGMA    |   2 |   2 |        2 |        2 | Y        | Y - SIGMA(2,2) | Y=F+F\*ERR(1)+ERR(2) | SIGMA(2,2) |

`NMrelate()` looks at the control stream to identify variable
assignments, shown in the `code` column. The `label` column is an
attempt to identify the name of the variable the parameter is associated
with. This works very well with some limitations. For instance, `THETA`s
will normally not work with referencing because a `MU` will be defined
based on the parameter, which is no more informative than the parameter
name itself. Also, the control stream does not need to create named
variables at all and could use parameters like `THETA` and `ETA`
directly in `$DES`. On the other hand, by always creating named
variables, you could automate this step using `NMrelate()`.

For example, if the `$OMEGA`’s are not annotated, we can use `NMrelate`
to fill these in. The parameter sections in this model look like this:

    ;; format: %idx: %symbol ; %label [%unit] ; %trans
    $THETA  (.1)             ; 1 : TVKA ; Absorption rate [1/h] ; log
    $THETA  (3)             ; 2 : TVV2 ;  Central volume [L] ; log
    $THETA  (1)             ; 3 : TVCL ; Clearance [L/h] ; log
    $THETA  (4)             ; 4 : TVV3 ; Peripheral volume [L] ; log
    $THETA  (-1)             ; 5 : TVQ ; Intercomparmental clearance [L/h] ; log
    $THETA .1              ; 6 : AGECL ; Age effect on clearance []; log
    $THETA .1              ; 7 : WEIGHTCL ; Body-weight effect on clearance []; log
    $THETA .1              ; 8 : MALECL ; Male effect on clearance []; log
    $OMEGA 0 FIX 
    $OMEGA 0.1   
    $OMEGA 0.1   
    $OMEGA 0 FIX 
    $OMEGA 0 FIX 
    ;; format.sigma: %symbol - %label ; %trans
    $SIGMA 0.1    ; SigP - Prop err ; propErr
    $SIGMA 0 FIX  ; SigA - Add err ; addErr

``` r
df.labs.b <- NMreadParsText(file.mod.b)[par.type!="OMEGA"] |>
    rbind(setnames(NMrelate(file.mod.b)[par.type=="OMEGA"],"label","symbol"),fill=TRUE)
```

Now we filled in the `symbol` column for the `$OMEGA` parameters so
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
can label the parameters.

``` r
df.labs.b[,.(par.type,par.name,symbol,label,unit,trans)]
```

| par.type | par.name   | symbol   | label                           | unit | trans   |
|:---------|:-----------|:---------|:--------------------------------|:-----|:--------|
| THETA    | THETA(1)   | TVKA     | Absorption rate                 | 1/h  | log     |
| THETA    | THETA(2)   | TVV2     | Central volume                  | L    | log     |
| THETA    | THETA(3)   | TVCL     | Clearance                       | L/h  | log     |
| THETA    | THETA(4)   | TVV3     | Peripheral volume               | L    | log     |
| THETA    | THETA(5)   | TVQ      | Intercomparmental clearance     | L/h  | log     |
| THETA    | THETA(6)   | AGECL    | Age effect on clearance         |      | log     |
| THETA    | THETA(7)   | WEIGHTCL | Body-weight effect on clearance |      | log     |
| THETA    | THETA(8)   | MALECL   | Male effect on clearance        |      | log     |
| SIGMA    | SIGMA(1,1) | SigP     | Prop err                        | NA   | propErr |
| SIGMA    | SIGMA(2,2) | SigA     | Add err                         | NA   | addErr  |
| OMEGA    | OMEGA(1,1) | KA       | NA                              | NA   | NA      |
| OMEGA    | OMEGA(2,2) | V2       | NA                              | NA   | NA      |
| OMEGA    | OMEGA(3,3) | CL       | NA                              | NA   | NA      |
| OMEGA    | OMEGA(4,4) | V3       | NA                              | NA   | NA      |
| OMEGA    | OMEGA(5,5) | Q        | NA                              | NA   | NA      |

And the parameter table becomes

``` r
createParameterTable(file.lst=file.mod.b,df.labs=df.labs.b) |>
    printParameterTable(format="html")
```

[TABLE]

Model: xgxr134b

## Printing the Parameter Table (`printParameterTable()`)

[`printParameterTable()`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md)
prints the output from
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
using predefined templates. Currently, it has two templates, of which
the first is designed for display in the R console, i.e. intended for
interactive use. The other is intended for reporting with all
information explicitly expressed.

[`printParameterTable()`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md)
supports use of three “engines” to write tables: `kable`, `pmtables`,
and `flextable`. Switch between those using the `engine` argument. The
`format` argument specifies whether pdf, html

| engine          | format   | Description                                                                                                |
|:----------------|:---------|:-----------------------------------------------------------------------------------------------------------|
| kable, pmtables | latex    | latex code. Use in .tex/.Rmd or similar files.                                                             |
| kable, pmtables | file.pdf | A path to a standalone pdf file to be generated. Absolute paths may not be supported with pmtables engine. |
| kable, pmtables | pdf      | A standalone pdf file in a temporary location, intended for interactive use.                               |
| kable           | R        | A simplified format printed in the R console. For interactive use.                                         |
| kable           | html     | html code. To be used in html documents                                                                    |
| flextable       | NA       | NA                                                                                                         |

Currently, the `flextable` returns a flextable object which the user can
save to png, docx, pptx, etc.

### Example: Include in Rmd report

If `createParameterTables()` has not already been called during the
analysis, the code could be this simple. Notice, here we choose
`engine="pmtables"` to use pmtables. This is because pmtables works with
footnotes even for large tables spanning multiple pages. Use
`engine="kable"` to get kable in stead (or of course
`engine="flextable"` can be used too.

``` r
createParameterTable(file.mod) |> 
    printParameterTable(partab,format="latex", engine="pmtables") 
```

Depending on the model and the (missing) annotations of parameter
definitions, running
[`createParameterTable()`](https://nmautoverse.github.io/NMwork/reference/createParameterTable.md)
may take a little customization, as discussed above. Therefore, it may
be desired to load the result of that step, and then run
[`printParameterTable()`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md).
In that case

#### If Paths Have Changed

`file.mod`

### Subsetting Parameters to Print

Notice not all parameters are printed above. The argument `include.fix`
controls whether to include fixed parameters. The options are “notZero
(default, inclde only fixed variables different from zero), `TRUE`
(include fixed parameters) and `FALSE` (omit all fixed parameters).
Also, see additional arguments to control what parameters to include:
`include`, `include.pattern`, `drop`, `drop.pattern` in the manual.

### Title and Footnote Generation

[`printParameterTable()`](https://nmautoverse.github.io/NMwork/reference/printParameterTable.md)
by default inserts a title with the model name. This can be modified
through the `caption` argument. Footnotes are also included.

### meta data

`script`
