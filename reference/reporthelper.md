# reporthelper

Aggregates MAgPIE products to standard reporting categories and changes
to reporting names. Automatically recognizes if only a reduced form of
"kall" is provided.

## Usage

``` r
reporthelper(
  x,
  dim = 3.1,
  level_zero_name = "All products",
  detail = TRUE,
  sort = FALSE,
  partly = FALSE,
  version = NULL
)
```

## Arguments

- x:

  Magpie object with data that shall be reported

- dim:

  Dimension in which magpie products ("tece" etc) can be found

- level_zero_name:

  the general reporting name of the Magpie object (e.g. "Agricultural
  Production")

- detail:

  if detail=F, the subcategories of groups are not reported (e.g.
  "soybean" within "oilcrops")

- sort:

  sort items in 3rd dimension alphabetically (TRUE or FALSE)

- partly:

  boolean or set name, that should be reported in detail, even if it is
  just partly provided within the gdx

- version:

  Switch between different version of the magpiesets library

## Value

MAgPIE object with aggregated and renamed items in 3rd dimension

## Author

Benjamin Leon Bodirsky, Florian Humpenoeder, Kristine Karstens

## Examples

``` r
if (FALSE) { # \dontrun{
x <- calories(level = "regglo", products = "kcr", attributes = "protein")
x <- reporthelper(x)
} # }
```
