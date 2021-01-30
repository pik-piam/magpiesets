# MAgPIE sets for R

R package **magpiesets**, version **0.40.6**

[![CRAN status](https://www.r-pkg.org/badges/version/magpiesets)](https://cran.r-project.org/package=magpiesets) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1158588.svg)](https://doi.org/10.5281/zenodo.1158588)  [![R build status](https://github.com/pik-piam/magpiesets/workflows/check/badge.svg)](https://github.com/pik-piam/magpiesets/actions) [![codecov](https://codecov.io/gh/pik-piam/magpiesets/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/magpiesets)

## Purpose and Functionality

A library containing MAgPIE sets and other support functions.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("magpiesets")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("magpiesets") # Working with magpiesets
```

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **magpiesets** in publications use:

Bodirsky B, Humpenoeder F, Mishra A, Karstens K, Weindl I, Molina Bacca E, von Jeetze P, Dietrich J (2021). _magpiesets:
MAgPIE sets for R_. doi: 10.5281/zenodo.1158588 (URL: https://doi.org/10.5281/zenodo.1158588), R package version 0.40.6,
<URL: https://github.com/pik-piam/magpiesets>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {magpiesets: MAgPIE sets for R},
  author = {Benjamin Leon Bodirsky and Florian Humpenoeder and Abhijeet Mishra and Kristine Karstens and Isabelle Weindl and Edna {Molina Bacca} and Patrick {von Jeetze} and Jan Philipp Dietrich},
  year = {2021},
  note = {R package version 0.40.6},
  doi = {10.5281/zenodo.1158588},
  url = {https://github.com/pik-piam/magpiesets},
}
```

