
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
```

### Step 1: Define your coding scheme

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

### Step 2: Convert your coding from character

``` r
# Clean up messy codes and set class
messy_codes <- c(" 12B+ 1 +L2", "T20 + 25")
clean_codes <- coding(messy_codes)
clean_codes
#> [1] "1+L2+12B" "T20+25"  
#> FACS Coding
```

### Step 3: Generate coding matrixes

``` r
# Generate occurrence matrix
occurrence(clean_codes, my_scheme)
#>   O01 O02 O04 O06 O09 O10 O12 O14 O20 O25
#> 1   1   1   0   0   0   0   1   0   0   0
#> 2   0   0   0   0   0   0   0   0   1   1

# Generate intensity matrix
intensity(clean_codes, my_scheme)
#>   I06 I12
#> 1   0   2
#> 2   0   0

# Generate asymmetry matrix
asymmetry(clean_codes, my_scheme)
#>   A02 A06 A10 A12 A14 A20
#> 1 "L" NA  NA  "S" NA  NA 
#> 2 NA  NA  NA  NA  NA  "T"
```

### Step 4: Compute occurrence agreement

``` r
coder1 <- coding(c("1+4+9", "6+7+12+25"))
coder2 <- coding(c("1+4+10", "6+12+25"))
coder3 <- coding(c(NA, "7+12+25"))
ad <- agree_description(coder1, coder2, coder3, scheme = my_scheme)
print(ad)
#> [1] 0.7708333
summary(ad)
#> # Counts
#> Events = 2
#> Codes  = 10
#> Coders = 3
#> 
#> # Overall
#> 0.771
#> 
#> # Per Event
#>    E1    E2 
#> 0.667 0.875 
#> 
#> # Per Code
#> O01 O04 O06 O09 O10 O12 O25 
#> 1.0 1.0 0.5 0.0 0.0 1.0 1.0 
#> 
#> # Per Pair
#> C1_C2 C1_C3 C2_C3 
#> 0.833 0.800 0.800 
#> 
#> # Drop One
#> drop_C1 drop_C2 drop_C3 
#>   0.800   0.800   0.833
```
