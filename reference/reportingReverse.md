# reportingReverse

reverses the reporting format with tree structure (e.g. "Demand",
"Demand\|Agriculture\|Food (Mt DM)") into a conventional magpie format

## Usage

``` r
reportingReverse(x)
```

## Arguments

- x:

  a MAgPIE which has the format of a report file (e.g.
  "Demand\|Agriculture\|Food (Mt DM)")

## Value

a MAgPIE object with higher dimensionailty

## Author

Benjamin Leon Bodirsky

## Examples

``` r
if (FALSE) { # \dontrun{
budget <- calcOutput("ValidDemand")
    reportingReverse(budget)
} # }
```
