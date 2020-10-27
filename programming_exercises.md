Programming Exercises
================
Your name

## Load necessary libraries

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(knitr)
```

## Compute the number of unique values in each column of `iris`

### Using a `for` loop

``` r
uniq_for <- function(a) {
  uniq_out <- array(dim = ncol(a))
  len_out <- vector("integer", length = ncol(a))
  for (i in seq_along(a)){
    uniq_out[i] <- unique(a[i])
    print(uniq_out)
    len_out[i] <- length(uniq_out[i])
  }
  return(len_out)
}

uniq_for(iris)
```

    ## [[1]]
    ##  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.4 4.8 4.3 5.8 5.7 5.2 5.5 4.5 5.3 7.0 6.4 6.9 6.5
    ## [20] 6.3 6.6 5.9 6.0 6.1 5.6 6.7 6.2 6.8 7.1 7.6 7.3 7.2 7.7 7.4 7.9
    ## 
    ## [[2]]
    ## [1] NA
    ## 
    ## [[3]]
    ## [1] NA
    ## 
    ## [[4]]
    ## [1] NA
    ## 
    ## [[5]]
    ## [1] NA
    ## 
    ## [[1]]
    ##  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.4 4.8 4.3 5.8 5.7 5.2 5.5 4.5 5.3 7.0 6.4 6.9 6.5
    ## [20] 6.3 6.6 5.9 6.0 6.1 5.6 6.7 6.2 6.8 7.1 7.6 7.3 7.2 7.7 7.4 7.9
    ## 
    ## [[2]]
    ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 2.9 3.7 4.0 4.4 3.8 3.3 4.1 4.2 2.3 2.8 2.4 2.7
    ## [20] 2.0 2.2 2.5 2.6
    ## 
    ## [[3]]
    ## [1] NA
    ## 
    ## [[4]]
    ## [1] NA
    ## 
    ## [[5]]
    ## [1] NA
    ## 
    ## [[1]]
    ##  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.4 4.8 4.3 5.8 5.7 5.2 5.5 4.5 5.3 7.0 6.4 6.9 6.5
    ## [20] 6.3 6.6 5.9 6.0 6.1 5.6 6.7 6.2 6.8 7.1 7.6 7.3 7.2 7.7 7.4 7.9
    ## 
    ## [[2]]
    ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 2.9 3.7 4.0 4.4 3.8 3.3 4.1 4.2 2.3 2.8 2.4 2.7
    ## [20] 2.0 2.2 2.5 2.6
    ## 
    ## [[3]]
    ##  [1] 1.4 1.3 1.5 1.7 1.6 1.1 1.2 1.0 1.9 4.7 4.5 4.9 4.0 4.6 3.3 3.9 3.5 4.2 3.6
    ## [20] 4.4 4.1 4.8 4.3 5.0 3.8 3.7 5.1 3.0 6.0 5.9 5.6 5.8 6.6 6.3 6.1 5.3 5.5 6.7
    ## [39] 6.9 5.7 6.4 5.4 5.2
    ## 
    ## [[4]]
    ## [1] NA
    ## 
    ## [[5]]
    ## [1] NA
    ## 
    ## [[1]]
    ##  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.4 4.8 4.3 5.8 5.7 5.2 5.5 4.5 5.3 7.0 6.4 6.9 6.5
    ## [20] 6.3 6.6 5.9 6.0 6.1 5.6 6.7 6.2 6.8 7.1 7.6 7.3 7.2 7.7 7.4 7.9
    ## 
    ## [[2]]
    ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 2.9 3.7 4.0 4.4 3.8 3.3 4.1 4.2 2.3 2.8 2.4 2.7
    ## [20] 2.0 2.2 2.5 2.6
    ## 
    ## [[3]]
    ##  [1] 1.4 1.3 1.5 1.7 1.6 1.1 1.2 1.0 1.9 4.7 4.5 4.9 4.0 4.6 3.3 3.9 3.5 4.2 3.6
    ## [20] 4.4 4.1 4.8 4.3 5.0 3.8 3.7 5.1 3.0 6.0 5.9 5.6 5.8 6.6 6.3 6.1 5.3 5.5 6.7
    ## [39] 6.9 5.7 6.4 5.4 5.2
    ## 
    ## [[4]]
    ##  [1] 0.2 0.4 0.3 0.1 0.5 0.6 1.4 1.5 1.3 1.6 1.0 1.1 1.8 1.2 1.7 2.5 1.9 2.1 2.2
    ## [20] 2.0 2.4 2.3
    ## 
    ## [[5]]
    ## [1] NA
    ## 
    ## [[1]]
    ##  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.4 4.8 4.3 5.8 5.7 5.2 5.5 4.5 5.3 7.0 6.4 6.9 6.5
    ## [20] 6.3 6.6 5.9 6.0 6.1 5.6 6.7 6.2 6.8 7.1 7.6 7.3 7.2 7.7 7.4 7.9
    ## 
    ## [[2]]
    ##  [1] 3.5 3.0 3.2 3.1 3.6 3.9 3.4 2.9 3.7 4.0 4.4 3.8 3.3 4.1 4.2 2.3 2.8 2.4 2.7
    ## [20] 2.0 2.2 2.5 2.6
    ## 
    ## [[3]]
    ##  [1] 1.4 1.3 1.5 1.7 1.6 1.1 1.2 1.0 1.9 4.7 4.5 4.9 4.0 4.6 3.3 3.9 3.5 4.2 3.6
    ## [20] 4.4 4.1 4.8 4.3 5.0 3.8 3.7 5.1 3.0 6.0 5.9 5.6 5.8 6.6 6.3 6.1 5.3 5.5 6.7
    ## [39] 6.9 5.7 6.4 5.4 5.2
    ## 
    ## [[4]]
    ##  [1] 0.2 0.4 0.3 0.1 0.5 0.6 1.4 1.5 1.3 1.6 1.0 1.1 1.8 1.2 1.7 2.5 1.9 2.1 2.2
    ## [20] 2.0 2.4 2.3
    ## 
    ## [[5]]
    ## [1] setosa     versicolor virginica 
    ## Levels: setosa versicolor virginica

    ## [1] 1 1 1 1 1

### Using a `map` function

``` r
#Generalized the task into a function.

uniq_map <- function(a) {
  uniq_val <- map(a, unique) #First, it creates a dataframe just with the unique values in each column
  count_uniq <- map_int(uniq_val, length) #Second, it creates a vector with the length of each column
  return(kable(count_uniq, col.names = "Unique Values", caption = "Count of Unique Values per Column")) #At last, it makes the visualization prettier with a decent table explaining values
}

uniq_map(iris) #Then, I just needed to call the specified dataframe "Iris" into function
```

|              | Unique Values |
| :----------- | ------------: |
| Sepal.Length |            35 |
| Sepal.Width  |            23 |
| Petal.Length |            43 |
| Petal.Width  |            22 |
| Species      |             3 |

Count of Unique Values per Column

## Calculate the square of each element in vector `x`

``` r
x <- seq(from = 30, to = 1)
x
```

    ##  [1] 30 29 28 27 26 25 24 23 22 21 20 19 18 17 16 15 14 13 12 11 10  9  8  7  6
    ## [26]  5  4  3  2  1

### Using a `for` loop

``` r
#Generalized this task on a function as well.

sqr_function <- function(a){
  output <- vector("integer", length(a)) #Assigned the size of the output for efficiency
  for (i in seq_along(a)){
    output[i] <- a[i] ^ 2 #Substitutes each item in the blank output for squared of homologous item within input vector
  }
  vis_result <- data.frame(a, output) #Creates dataframe comparing output and input side to side
  return(kable(vis_result, col.names = c("x", "x^2"))) #Makes visualization of this function easier through a table
}

sqr_function(x) #Called specified vector into function
```

|  x | x^2 |
| -: | --: |
| 30 | 900 |
| 29 | 841 |
| 28 | 784 |
| 27 | 729 |
| 26 | 676 |
| 25 | 625 |
| 24 | 576 |
| 23 | 529 |
| 22 | 484 |
| 21 | 441 |
| 20 | 400 |
| 19 | 361 |
| 18 | 324 |
| 17 | 289 |
| 16 | 256 |
| 15 | 225 |
| 14 | 196 |
| 13 | 169 |
| 12 | 144 |
| 11 | 121 |
| 10 | 100 |
|  9 |  81 |
|  8 |  64 |
|  7 |  49 |
|  6 |  36 |
|  5 |  25 |
|  4 |  16 |
|  3 |   9 |
|  2 |   4 |
|  1 |   1 |

### Using a `map` function

## Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem

## Session info

``` r
devtools::session_info()
```

    ## - Session info ---------------------------------------------------------------
    ##  setting  value                       
    ##  version  R version 4.0.2 (2020-06-22)
    ##  os       Windows 8.1 x64             
    ##  system   x86_64, mingw32             
    ##  ui       RTerm                       
    ##  language (EN)                        
    ##  collate  English_United States.1252  
    ##  ctype    English_United States.1252  
    ##  tz       America/Chicago             
    ##  date     2020-10-26                  
    ## 
    ## - Packages -------------------------------------------------------------------
    ##  package     * version date       lib source        
    ##  assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)
    ##  backports     1.1.10  2020-09-15 [1] CRAN (R 4.0.2)
    ##  blob          1.2.1   2020-01-20 [1] CRAN (R 4.0.2)
    ##  broom         0.7.0   2020-07-09 [1] CRAN (R 4.0.2)
    ##  callr         3.4.4   2020-09-07 [1] CRAN (R 4.0.2)
    ##  cellranger    1.1.0   2016-07-27 [1] CRAN (R 4.0.2)
    ##  cli           2.0.2   2020-02-28 [1] CRAN (R 4.0.2)
    ##  colorspace    1.4-1   2019-03-18 [1] CRAN (R 4.0.2)
    ##  crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)
    ##  DBI           1.1.0   2019-12-15 [1] CRAN (R 4.0.2)
    ##  dbplyr        1.4.4   2020-05-27 [1] CRAN (R 4.0.2)
    ##  desc          1.2.0   2018-05-01 [1] CRAN (R 4.0.2)
    ##  devtools      2.3.2   2020-09-18 [1] CRAN (R 4.0.2)
    ##  digest        0.6.25  2020-02-23 [1] CRAN (R 4.0.2)
    ##  dplyr       * 1.0.2   2020-08-18 [1] CRAN (R 4.0.2)
    ##  ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)
    ##  evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)
    ##  fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.2)
    ##  forcats     * 0.5.0   2020-03-01 [1] CRAN (R 4.0.2)
    ##  fs            1.5.0   2020-07-31 [1] CRAN (R 4.0.2)
    ##  generics      0.0.2   2018-11-29 [1] CRAN (R 4.0.2)
    ##  ggplot2     * 3.3.2   2020-06-19 [1] CRAN (R 4.0.2)
    ##  glue          1.4.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  gtable        0.3.0   2019-03-25 [1] CRAN (R 4.0.2)
    ##  haven         2.3.1   2020-06-01 [1] CRAN (R 4.0.2)
    ##  highr         0.8     2019-03-20 [1] CRAN (R 4.0.2)
    ##  hms           0.5.3   2020-01-08 [1] CRAN (R 4.0.2)
    ##  htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.2)
    ##  httr          1.4.2   2020-07-20 [1] CRAN (R 4.0.2)
    ##  jsonlite      1.7.1   2020-09-07 [1] CRAN (R 4.0.2)
    ##  knitr       * 1.30    2020-09-22 [1] CRAN (R 4.0.2)
    ##  lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.2)
    ##  lubridate     1.7.9   2020-06-08 [1] CRAN (R 4.0.2)
    ##  magrittr      1.5     2014-11-22 [1] CRAN (R 4.0.2)
    ##  memoise       1.1.0   2017-04-21 [1] CRAN (R 4.0.2)
    ##  modelr        0.1.8   2020-05-19 [1] CRAN (R 4.0.2)
    ##  munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)
    ##  pillar        1.4.6   2020-07-10 [1] CRAN (R 4.0.2)
    ##  pkgbuild      1.1.0   2020-07-13 [1] CRAN (R 4.0.2)
    ##  pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)
    ##  pkgload       1.1.0   2020-05-29 [1] CRAN (R 4.0.2)
    ##  prettyunits   1.1.1   2020-01-24 [1] CRAN (R 4.0.2)
    ##  processx      3.4.4   2020-09-03 [1] CRAN (R 4.0.2)
    ##  ps            1.3.4   2020-08-11 [1] CRAN (R 4.0.2)
    ##  purrr       * 0.3.4   2020-04-17 [1] CRAN (R 4.0.2)
    ##  R6            2.4.1   2019-11-12 [1] CRAN (R 4.0.2)
    ##  Rcpp          1.0.5   2020-07-06 [1] CRAN (R 4.0.2)
    ##  readr       * 1.3.1   2018-12-21 [1] CRAN (R 4.0.2)
    ##  readxl        1.3.1   2019-03-13 [1] CRAN (R 4.0.2)
    ##  remotes       2.2.0   2020-07-21 [1] CRAN (R 4.0.2)
    ##  reprex        0.3.0   2019-05-16 [1] CRAN (R 4.0.2)
    ##  rlang         0.4.7   2020-07-09 [1] CRAN (R 4.0.2)
    ##  rmarkdown     2.4     2020-09-30 [1] CRAN (R 4.0.2)
    ##  rprojroot     1.3-2   2018-01-03 [1] CRAN (R 4.0.2)
    ##  rstudioapi    0.11    2020-02-07 [1] CRAN (R 4.0.2)
    ##  rvest         0.3.6   2020-07-25 [1] CRAN (R 4.0.2)
    ##  scales        1.1.1   2020-05-11 [1] CRAN (R 4.0.2)
    ##  sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)
    ##  stringi       1.5.3   2020-09-09 [1] CRAN (R 4.0.2)
    ##  stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.2)
    ##  testthat      2.3.2   2020-03-02 [1] CRAN (R 4.0.2)
    ##  tibble      * 3.0.3   2020-07-10 [1] CRAN (R 4.0.2)
    ##  tidyr       * 1.1.2   2020-08-27 [1] CRAN (R 4.0.2)
    ##  tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)
    ##  tidyverse   * 1.3.0   2019-11-21 [1] CRAN (R 4.0.2)
    ##  usethis       1.6.3   2020-09-17 [1] CRAN (R 4.0.2)
    ##  vctrs         0.3.4   2020-08-29 [1] CRAN (R 4.0.2)
    ##  withr         2.3.0   2020-09-22 [1] CRAN (R 4.0.2)
    ##  xfun          0.17    2020-09-09 [1] CRAN (R 4.0.2)
    ##  xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.2)
    ##  yaml          2.2.1   2020-02-01 [1] CRAN (R 4.0.2)
    ## 
    ## [1] C:/Users/Gustavo/OneDrive - The University of Chicago/Documents/R/win-library/4.0
    ## [2] C:/Program Files/R/R-4.0.2/library
