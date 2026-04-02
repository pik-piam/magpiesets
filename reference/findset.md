# findset

function returning set item of MAgPIE sets

## Usage

``` r
findset(set, alias = FALSE, noset = "warning", version = NULL)
```

## Arguments

- set:

  a MAgPIE set (e.g. "kcr"), or a vector of sets.

- alias:

  if TRUE, set elements are extended by '\_alias'. Can be used to avoid
  doubling of dimnames.

- noset:

  if "original", a set that has no mapping is returned without changes;
  otherwhise a vector of length 0 is returned, plus a warning that the
  set does not exist.

- version:

  Switch between different version of the magpiesets library (use
  'versionset' to load version types itself)

## Value

MAgPIE set items

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Kristine Karstens

## Examples

``` r
  if (FALSE) { # \dontrun{
    findset("kcr")
  } # }
```
