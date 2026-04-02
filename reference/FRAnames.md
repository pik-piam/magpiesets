# FRA names

function returning the reporting names from FRA 2020 category

## Usage

``` r
FRAnames(x, from = "franames", to = "magpienames", mapping = "fra_names.csv")
```

## Arguments

- x:

  a vector of common FRA 2020 names (e.g. c("nat_reg","agb"))

- from:

  fra names for common magpie abbreviations, FRAnames for reverse
  translation

- to:

  Common names for official reporting names, reportincolors for typical
  colorcode of an item

- mapping:

  csv file in inst/extdata folder.

## Value

vector with reporting names or volors

## Author

Abhijeet Nishra

## Examples

``` r
  if (FALSE) { # \dontrun{
    FRAnames("nat_reg")
    FRAnames("tece",to="reportingcolors")
  } # }
  
```
