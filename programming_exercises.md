Programming Exercises
================
Gustavo Arruda Franco

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
#Generalized the task into a function

uniq_for <- function(a) {
  len_out <- vector("double", length = ncol(a)) #Set up the size of an empty vector to optimize iteration
  for (i in seq_along(a)){
    uniq_out <- unique(a[i]) #Extract the unique values from each column of input
    len_out[i] <- count(uniq_out) #Count the values in each iteration of uniq_out and store it in the previously empty vector
  }
  return(len_out)
}

uniq_for(iris)
```

    ## [[1]]
    ## [1] 35
    ## 
    ## [[2]]
    ## [1] 23
    ## 
    ## [[3]]
    ## [1] 43
    ## 
    ## [[4]]
    ## [1] 22
    ## 
    ## [[5]]
    ## [1] 3

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

sqr_for <- function(a){
  output <- vector("integer", length(a)) #Assigned the size of the output for efficiency
  for (i in seq_along(a)){
    output[i] <- a[i] ^ 2 #Substitutes each item in the blank output for squared of homologous item within input vector
  }
  vis_result <- data.frame(a, output) #Creates dataframe comparing output and input side to side
  return(kable(vis_result, col.names = c("x", "x^2"))) #Makes visualization of this function easier through a table
}

sqr_for(x) #Called specified vector into function
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

``` r
sqr_function <- function(a){ #To make the map method work I need a pre-defined squaring function
  squared <- a ^ 2
}
sqr_map <- function(a){ #This function takes the previous one and applies it to a whole vector, creating a table for easy visualization in the end
  squared_vector <- map_dbl(a, sqr_function)
  sqr_vis <- data.frame(a, squared_vector)
  return(kable(sqr_vis, col.names = c("x", "x^2")))
}

sqr_map(x)
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

## Write a function to calculate length of sides in a right-triangle using the Pythagorean Theorem

``` r
pyth_thm <- function(a = NA, b = NA, c = NA){ #The three arguments are set to NA by default
  enough_cond <- is.na(a) + is.na(b) + is.na(c) #If any of the arguments is NA, they will add 1 to this count
  numeric_cond <- is.numeric(a) + is.numeric(b) + is.numeric(c) #If any of the arguments is numerical, they will add 1 to this count
  if (enough_cond == 2 | enough_cond == 0){ #If only one argument is passed to the function, then the NA count equals to 2; conversely, if three arguments are passed, the NA count equals to 0. In that case the function stops, hence the "enough" variable name.
    stop()
  }
  else {
    if (numeric_cond != 2){ #Given that the previous "if" statement limits this function to only two arguments, this particular "if" statement guarantees that it is necessary that the two arguments passed into this function are numerical.
      stop()
    }
    else{ #This conditionals within the function detects which of the three arguments is blank. Then they proceed to apply the Pythagorean formula to the remaining arguments and print the answer.
     if (is.na(c)){
       print(paste("C =", as.character(sqrt(a^2 + b^2))))
     }
     if (is.na(b)){
       print(paste("B =", as.character(sqrt(c^2 - a^2))))
     }
     if (is.na(a)){
       print(paste("B =", as.character(sqrt(c^2 - b^2))))
     }
    }
  }
}
```

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
    ##  date     2020-10-27                  
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
