
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ISO8601.parser

<!-- badges: start -->

[![R-CMD-check](https://github.com/billdenney/ISO8601.parser/workflows/R-CMD-check/badge.svg)](https://github.com/billdenney/ISO8601.parser/actions)
<!-- badges: end -->

The goal of ISO8601.parser is to assist with parsing ISO8601 strings

## Installation

You can install the development version of ISO8601.parser from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("billdenney/ISO8601.parser")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ISO8601.parser)
parse_ISO8601("R8/P1Y2M3DT4H5M6.7S")
#> $repeating_interval
#> [1] TRUE
#> 
#> $repeating_interval_n
#> [1] "8"
#> 
#> $interval
#> [1] TRUE
#> 
#> $duration
#> $duration$duration
#> [1] TRUE
#> 
#> $duration$duration_yearnum
#> [1] "1"
#> 
#> $duration$duration_monthnum
#> [1] "2"
#> 
#> $duration$duration_daynum
#> [1] "3"
#> 
#> $duration$duration_hournum
#> [1] "4"
#> 
#> $duration$duration_minutenum
#> [1] "5"
#> 
#> $duration$duration_secondnum
#> [1] "6"
#> 
#> $duration$fraction
#> [1] "7"
parse_ISO8601("2020-11-01T12:34:56.567890")
#> $date
#> [1] TRUE
#> 
#> $year
#> [1] "2020"
#> 
#> $iso_8601_format
#> [1] "extended"
#> 
#> $month
#> [1] "11"
#> 
#> $mday
#> [1] "01"
#> 
#> $time
#> [1] TRUE
#> 
#> $hour
#> [1] "12"
#> 
#> $minute
#> [1] "34"
#> 
#> $second
#> [1] "56"
#> 
#> $fraction
#> [1] "567890"
```
