# summationhelper

Modifies name of variables in a magpie object to add summation symbols.

## Usage

``` r
summationhelper(x, sep = "+", dim = 3.1, check = TRUE, excludeLevels = 0)
```

## Arguments

- x:

  MAgPIE object from validation calculations in "almost" ready format.
  for e.g. from x \<- calcOutput("ValidDemand","FAO")

- sep:

  summation symbol (+,++,-)

- dim:

  Dimension in which the modification, should take place

- check:

  Switch to turn off checking routine, if FALSE. Default is TRUE.

- excludeLevels:

  levels to be excluded from summation, e.g. 1 for Trade in
  Trade\|Net-Exports.

## Value

MAgPIE object

## Details

summationhelper should help to organize the subcategories into groups
for e.g. stackplots. Notice the following hints:

- Every name should just contain one summation symbol (mostly '+').

- The position of the symbol (counted in '\|' from left side) will
  determine the level.

- Every subitem containing the same summation symbol in the same level
  with the same supercategory name will be summed.

- Items without any summation symbol will ge ignored.

- Items with different summation symbols will be summed up separately.

- In most of the cases a summation symbol will be just placed before the
  last level (counted in '\|' from left side).

- It is helpful to think about which group of items should be stacked in
  a stackplot.

An example how a summation symbol placement could look like:

      Toplevel
      Toplevel|+|Item 1
      Toplevel|+|Item 2
      Toplevel|Item 2|+|Subitem 1
      Toplevel|Item 2|+|Subitem 1
      Toplevel|++|Item A
      Toplevel|++|Item B
      Toplevel|Item ?

## Author

Abhijeet Mishra, Kristine Karstens

## Examples

``` r
  if (FALSE) { # \dontrun{
    x <- calcOutput("LanduseInitialisation",aggregate = FALSE)
    getNames(x) <- paste0("Land Cover|", reportingnames(getNames(x))," (million ha)")   
    x <- summationhelper(x)
  } # }
```
