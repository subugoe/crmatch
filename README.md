


# Match references to Crossref DOIs using the Crossref metadata search

[![Travis-CI Build Status](https://travis-ci.org/subugoe/crmatch.svg?branch=master)](https://travis-ci.org/subugoe/crmatch)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/subugoe/crmatch?branch=master&svg=true)](https://ci.appveyor.com/project/subugoe/crmatch)

crmatch provides an R interface to Crossref Metadata Search for fuzzy matching of free-form references to DOIs.

Link: <http://search.crossref.org/references>

## Installation

This package is only available via GitHub. To install, using devtools package

```r
devtools::install_github("subugoe/crmatch")
# load into R
library("crmatch")
```

## Usage

There is only one function to call:


```r
crmatch::cr_match(refs = c("Kleinbölting N, Huep G, Weisshaar B. Enhancing the GABI-Kat Arabidopsis thaliana T-DNA insertion mutant database by incorporating Araport11 annotation. Plant and Cell Physiology. 2017;58(1): e7.", "Tamir, Jonathan I. et al. “T-2 Shuffling: Sharp, Multicontrast, Volumetric Fast Spin-Echo Imaging.” Magnetic Resonance in Medicine 77.1 (2017): 180–195."))
#> # A tibble: 2 × 5
#>                                                                          text
#> *                                                                       <chr>
#> 1 Kleinbölting N, Huep G, Weisshaar B. Enhancing the GABI-Kat Arabidopsis tha
#> 2 Tamir, Jonathan I. et al. “T-2 Shuffling: Sharp, Multicontrast, Volumetric 
#> # ... with 4 more variables: match <lgl>, doi <chr>, coins <chr>,
#> #   score <dbl>
```


## Use rcrossref to make full use of Crossref APIs

This package complements the wonderful [rcrossref-package](https://github.com/ropensci/rcrossref)

## Meta

* Please [report any issues or bugs](https://github.com/subugoe/crmatch/issues).
* License: MIT
* Please note that this project is released with a [Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree to abide by its terms.
