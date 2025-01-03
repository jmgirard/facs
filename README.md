
<!-- README.md is generated from README.Rmd. Please edit that file -->

# protofacs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of protofacs is to provide tools for storing and manipulating
data from the Facial Action Coding System (FACS) 2002 edition.

## Installation

You can install the development version of protofacs from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jmgirard/protofacs")
```

## Example

``` r
library("protofacs")
# Clean up messy codes and set class
messy_codes <- c(" 12B+ 1 +L2", "T20 + 25")
clean_codes <- coding(messy_codes)
clean_codes
#> [1] "1+L2+12B" "T20+25"  
#> FACS Coding
```

``` r
# Which behaviors were coded for occurrence, intensity, and asymmetry?
my_scheme <- scheme(
  list(
    occurrence = c(1, 2, 4, 6, 9, 10, 12, 14, 20, 25),
    intensity  = c(6, 12),
    asymmetry  = c(2, 6, 10, 12, 14, 20)
  )
)
my_scheme
#> A scheme object from the {facs} package:
#>   Occurrence Coded = [1, 2, 4, 6, 9, 10, 12, 14, 20, 25]
#>   Intensity Coded =  [6, 12]
#>   Asymmetry Coded =  [2, 6, 10, 12, 14, 20]
```

``` r
# Extract occurrence matrix
occurrence(clean_codes, my_scheme)
#>   O1 O2 O4 O6 O9 O10 O12 O14 O20 O25
#> 1  1  1  0  0  0   0   1   0   0   0
#> 2  0  0  0  0  0   0   0   0   1   1

# Extract intensity matrix
intensity(clean_codes, my_scheme)
#>   I6 I12
#> 1  0   2
#> 2  0   0

# Extract asymmetry matrix
asymmetry(clean_codes, my_scheme)
#>   A2  A6 A10 A12 A14 A20
#> 1 "L" NA NA  "S" NA  NA 
#> 2 NA  NA NA  NA  NA  "T"
```
