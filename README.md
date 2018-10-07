
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Pollen - tools for working with aerobiological data

[![Build
Status](https://travis-ci.org/Nowosad/pollen.png?branch=master)](https://travis-ci.org/Nowosad/pollen)
[![codecov](https://codecov.io/gh/Nowosad/pollen/branch/master/graph/badge.svg)](https://codecov.io/gh/Nowosad/pollen)
[![CRAN
version](http://www.r-pkg.org/badges/version/pollen)](https://cran.r-project.org/package=pollen)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/pollen)](https://cran.r-project.org/package=pollen)

`pollen` is a set of functions for working with aerobiological data. It
takes care of some of the most widely use aerobiological calculations,
such as determination of pollen season limits or replacement of outliers
in a pollen count data.

## Installation

Get the released version from CRAN:

``` r
install.packages("pollen")
```

Or the development version from github:

``` r
devtools::install_github("nowosad/pollen")
```

## Examples

``` r
library(pollen)
```

``` r
data("pollen_count")
head(pollen_count)
#>   site       date alder birch hazel
#> 1   Oz 2007-01-01     0     0     0
#> 2   Oz 2007-01-02     0     0     0
#> 3   Oz 2007-01-03     0     0     0
#> 4   Oz 2007-01-04     0     0     0
#> 5   Oz 2007-01-05     0     0     0
#> 6   Oz 2007-01-06     0     0     0
```

### Pollen season calculation

``` r
df <- subset(pollen_count, site == "Oz")
pollen_season(value = df$birch, date = df$date, method = "95")
#>    year      start        end
#> 1  2007 2007-03-31 2007-05-03
#> 2  2008 2008-04-19 2008-05-07
#> 3  2009 2009-04-09 2009-05-09
#> 4  2010 2010-04-14 2010-05-07
#> 5  2011 2011-04-20 2011-05-17
#> 6  2012 2012-04-09 2012-05-14
#> 7  2013 2013-04-09 2013-05-09
#> 8  2014 2014-04-08 2014-05-10
#> 9  2015 2015-04-08 2015-04-30
#> 10 2016 2016-04-06 2016-05-09
```

More examples could be found in [the first package
vignette](https://cdn.rawgit.com/Nowosad/pollen/master/vignettes/intro.html).

## Contributions

[Feel free to submit issues and enhancement
requests.](https://github.com/Nowosad/pollen/issues)
