# reportingnames

function returning the reporting names of a vector

## Usage

``` r
reportingnames(
  x,
  from = "magpienames",
  to = "reportingnames",
  mapping = "reportingnames.csv"
)
```

## Arguments

- x:

  a vector of common magpie names (e.g. c("tece","trce"))

- from:

  magpienames for common magpie abbreviations, reportingnames for
  reverse translation

- to:

  reportingnames for official reporting names, reportincolors for
  typical colorcode of an item

- mapping:

  csv file in inst/extdata folder.

## Value

vector with reporting names or volors

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Patrick v. Jeetze

## Examples

``` r
  if (FALSE) { # \dontrun{
    reportingnames("tece")
    reportingnames("tece",to="reportingcolors")
  } # }
```
