Unsupervised Aggregattion of Rows in large Dataframe
================
Maximilian Nölscher
10 März 2020

Libraries
=========

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages -------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.3.0     v purrr   0.3.2
    ## v tibble  2.1.1     v dplyr   0.8.3
    ## v tidyr   1.0.0     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts ----------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Data Import
===========

``` r
data_dummy <- read_csv2('../../raw_data/data_dummy.csv')
```

    ## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.

    ## Parsed with column specification:
    ## cols(
    ##   proj_id = col_character(),
    ##   date = col_number(),
    ##   proj = col_character(),
    ##   column_A = col_character(),
    ##   column_B = col_character(),
    ##   column_C = col_character(),
    ##   column_D = col_character(),
    ##   column_F = col_number()
    ## )

Show the dataframe

``` r
data_dummy
```

    ## # A tibble: 7 x 8
    ##   proj_id     date proj  column_A column_B column_C column_D column_F
    ##   <chr>      <dbl> <chr> <chr>    <chr>    <chr>    <chr>       <dbl>
    ## 1 id_001  23041995 A     string_0 0.7      <NA>     <NA>           53
    ## 2 id_002  24041995 B     string_1 0.3      <NA>     0.8            NA
    ## 3 id_003  25041995 C     <NA>     <NA>     <NA>     <NA>           NA
    ## 4 id_004  26041995 D     string_2 <NA>     <NA>     <NA>           NA
    ## 5 id_005  27041995 F     string_3 <NA>     <NA>     <NA>           NA
    ## 6 id_002  24041995 C     string_4 <NA>     0.4      0.8            NA
    ## 7 id_001  23041995 D     <NA>     0.9      1.4      <NA>           NA

The goal is to merge duplicates of rows. The criterion for duplicates is \* same `proj_id` value \* same `date` value
