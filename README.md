tirt
================
interpretation threshold values based on the IRT method

# Introduction

This $R$ package contains the `tirt` function to estimate interpretation
threshold values (with bootstrapped 95% CIs) of a multi-item
questionnaire based on the item response theory (IRT) method developed
by [Terluin et al[^1]](https://pubmed.ncbi.nlm.nih.gov/36780033/). The
package is developed to accompany the paper, “Using item response theory
to estimate interpretation threshold values for the Frailty Index (FI)
in community dwelling older adults.” Two example datasets are included
in the `tirt` package to help users better understand its functionality.

## Installation

Please install the `tirt` package from
[GitHub](https://github.com/yhpua/tirt) with:

``` r
install.packages("devtools")
devtools::install_github("yhpua/tirt")
```

## Example 1

We will use `tirt` to estimate the interpretation threshold of the
Frailty Index (FI). The built-in synthetic dataset, `fi_dat`, includes
all (binary and polytomous) items of the FI and a (binary) clinical
anchor item that is placed in the last column.

Note that setting the `B = 100` argument specifies 100[^2] bootstrap
resamples to be used when computing the 95%CIs for the IRT-based
thresholds.

``` r
library(mirt)
```

    ## Loading required package: stats4

    ## Loading required package: lattice

``` r
library(tirt)
data(fi_dat)
summary(fi_dat)
```

    ##    ffi_cancer      ffi_sts     ffi_heart_chf   ffi_medication    ffi_bmi      
    ##  Min.   :0.00   Min.   :0.00   Min.   :0.000   Min.   :0.00   Min.   :0.0000  
    ##  1st Qu.:0.00   1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.0000  
    ##  Median :0.00   Median :0.00   Median :0.000   Median :0.00   Median :0.5000  
    ##  Mean   :0.05   Mean   :0.24   Mean   :0.025   Mean   :0.01   Mean   :0.4725  
    ##  3rd Qu.:0.00   3rd Qu.:0.50   3rd Qu.:0.000   3rd Qu.:0.00   3rd Qu.:1.0000  
    ##  Max.   :1.00   Max.   :1.00   Max.   :1.000   Max.   :0.50   Max.   :1.0000  
    ##   ffi_heart_mi    ffi_finance    ffi_healthchanged   ffi_vision   
    ##  Min.   :0.000   Min.   :0.000   Min.   :0.00      Min.   :0.000  
    ##  1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.00      1st Qu.:0.000  
    ##  Median :0.000   Median :0.000   Median :0.00      Median :0.000  
    ##  Mean   :0.015   Mean   :0.105   Mean   :0.23      Mean   :0.175  
    ##  3rd Qu.:0.000   3rd Qu.:0.000   3rd Qu.:0.00      3rd Qu.:0.000  
    ##  Max.   :1.000   Max.   :1.000   Max.   :1.00      Max.   :1.000  
    ##     ffi_walk       ffi_bladder    ffi_cutdown       ffi_low     ffi_arthritis  
    ##  Min.   :0.0000   Min.   :0.00   Min.   :0.000   Min.   :0.00   Min.   :0.000  
    ##  1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.000   1st Qu.:0.00   1st Qu.:0.000  
    ##  Median :0.0000   Median :0.00   Median :0.000   Median :0.00   Median :0.000  
    ##  Mean   :0.0775   Mean   :0.03   Mean   :0.075   Mean   :0.03   Mean   :0.195  
    ##  3rd Qu.:0.0000   3rd Qu.:0.00   3rd Qu.:0.000   3rd Qu.:0.00   3rd Qu.:0.000  
    ##  Max.   :0.5000   Max.   :1.00   Max.   :1.000   Max.   :1.00   Max.   :1.000  
    ##   anchor_lsmob  
    ##  Min.   :0.000  
    ##  1st Qu.:0.000  
    ##  Median :0.000  
    ##  Mean   :0.085  
    ##  3rd Qu.:0.000  
    ##  Max.   :1.000

``` r
tirt(fi_dat, B = 100)  
```

    ## Warning in tirt(fi_dat, B = 100): ffi_sts has non-integer values

    ## Warning in tirt(fi_dat, B = 100): ffi_medication has non-integer values

    ## Warning in tirt(fi_dat, B = 100): ffi_bmi has non-integer values

    ## Warning in tirt(fi_dat, B = 100): ffi_finance has non-integer values

    ## Warning in tirt(fi_dat, B = 100): ffi_walk has non-integer values

    ## Warning in tirt(fi_dat, B = 100): dataset includes non-integer values

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_medication has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## $irt
    ## [1] 0.2820692
    ## 
    ## $boot
    ##           Lower Upper
    ## threshold 0.224 0.413

Suppose the `ffi_walk` item is collinear with the anchor condition. By
specifying `clvar = "ffi_walk"`, `tirt` will remove this item in the
initial estimation phase. Once `tirt` has estimated the FI threshold in
terms of the latent trait level, it will re-fit an IRT model on all FI
items (excluding the anchor item) and use the IRT parameters to find the
expected FI score that corresponds to the latent threshold.

``` r
tirt(fi_dat, clvar = "ffi_walk", B = 100)  
```

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): ffi_sts has non-integer
    ## values

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): ffi_medication has
    ## non-integer values

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): ffi_bmi has non-integer
    ## values

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): ffi_finance has
    ## non-integer values

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): ffi_walk has non-integer
    ## values

    ## Warning in tirt(fi_dat, clvar = "ffi_walk", B = 100): dataset includes
    ## non-integer values

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_medication has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_chf has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_bladder has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_medication has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_medication has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_medication has <= 1 unique value(s)

    ## Warning in yhcheck(mydat, 1): ffi_heart_mi has <= 1 unique value(s)

    ## $irt
    ## [1] 0.2609688
    ## 
    ## $boot
    ##           Lower Upper
    ## threshold 0.212  0.49

## Example 2

We will use `tirt` to estimate the IRT-based threshold of a multi-item
questionnaire. The built-in `qol_dat` dataset is the simulated dataset
described in Appendix Section 4 of the paper by [Terluin et
al](https://pubmed.ncbi.nlm.nih.gov/36780033/).

Note that setting the `rform` argument to `sum` returns the expected
(mean) questionnaire score. In Example 1, we set `rform` to `prop`
(default) which expresses the expected score as a proportion of the
number of items evaluated.

``` r
data(qol_dat)
tirt(qol_dat, B = 100, rform = "sum")
```

    ## $irt
    ## [1] 115.0028
    ## 
    ## $boot
    ##             Lower   Upper
    ## threshold 114.539 115.463

[^1]: Terluin et al Estimating meaningful thresholds for multi-item
    questionnaires using item response theory Qual Life Res. 2023
    Jun;32(6):1819-1830. doi: 10.1007/s11136-023-03355-8.

[^2]: Should have specified more (e.g., 1000) samples
