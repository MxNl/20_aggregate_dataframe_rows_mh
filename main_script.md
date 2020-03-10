Unsupervised Aggregattion of Rows in Large Dataframe
================
Maximilian Nölscher
10 März 2020

Libraries
=========

``` r
library(tidyverse)
```

Data Import
===========

``` r
data_dummy <- read_csv2('raw_data/data_dummy.csv')
```

Clean the column names

``` r
data_dummy <- data_dummy %>% 
  janitor::clean_names() %>% 
  mutate(date = lubridate::dmy(date)) %>% 
  mutate_at(c('column_b', 'column_c', 'column_d'), as.numeric)
```

Show the dataframe

``` r
data_dummy
```

    ## # A tibble: 9 x 8
    ##   proj_id date       proj     column_a column_b column_c column_d column_f
    ##   <chr>   <date>     <chr>    <chr>       <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 id_001  1995-04-23 Ampel    string_0      0.7     NA       NA         53
    ## 2 id_002  1995-04-24 Berta    string_1      0.3     NA        0.8       NA
    ## 3 id_003  1995-04-25 Chlor    <NA>         NA       NA       NA         NA
    ## 4 id_004  1995-04-26 Dora     string_2     NA       NA       NA         NA
    ## 5 id_005  1995-04-27 Farfalle string_3     NA       NA       NA         NA
    ## 6 id_002  1995-04-24 Chlor    string_4     NA        0.4      0.8       NA
    ## 7 id_001  1995-04-23 Dora     <NA>          0.9      1.4     NA         NA
    ## 8 id_001  1995-04-23 Berta    <NA>          7.5     10.1      3         NA
    ## 9 id_005  1995-04-23 Dora     string_3     NA       11.8     56.8       NA

Problem Definition
==================

The goal is to merge duplicates of rows. The criterion for duplicates is

-   same `proj_id` value
-   same `date` value

Find Duplicates
---------------

### Way 1 - Janitor

``` r
data_dummy %>% 
  janitor::get_dupes(proj_id, date)
```

    ## # A tibble: 5 x 9
    ##   proj_id date       dupe_count proj  column_a column_b column_c column_d
    ##   <chr>   <date>          <int> <chr> <chr>       <dbl>    <dbl>    <dbl>
    ## 1 id_001  1995-04-23          3 Ampel string_0      0.7     NA       NA  
    ## 2 id_001  1995-04-23          3 Dora  <NA>          0.9      1.4     NA  
    ## 3 id_001  1995-04-23          3 Berta <NA>          7.5     10.1      3  
    ## 4 id_002  1995-04-24          2 Berta string_1      0.3     NA        0.8
    ## 5 id_002  1995-04-24          2 Chlor string_4     NA        0.4      0.8
    ## # ... with 1 more variable: column_f <dbl>

Aggregation
===========

Method 1 - with prioritizing confidence
---------------------------------------

### `priority_key`

In the following steps we define a dataframe containing priorities to each unique project

``` r
priority_key <- data_dummy %>% 
  distinct(proj)

priority_key
```

    ## # A tibble: 5 x 1
    ##   proj    
    ##   <chr>   
    ## 1 Ampel   
    ## 2 Berta   
    ## 3 Chlor   
    ## 4 Dora    
    ## 5 Farfalle

Now we add values for the priorities

``` r
priority_key <- priority_key %>% 
  mutate(priority = c(5, 3, 1, 2, 4))
```

We join the 2 dataframes

``` r
data_dummy <- data_dummy %>% 
  left_join(priority_key)

data_dummy
```

    ## # A tibble: 9 x 9
    ##   proj_id date       proj  column_a column_b column_c column_d column_f
    ##   <chr>   <date>     <chr> <chr>       <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 id_001  1995-04-23 Ampel string_0      0.7     NA       NA         53
    ## 2 id_002  1995-04-24 Berta string_1      0.3     NA        0.8       NA
    ## 3 id_003  1995-04-25 Chlor <NA>         NA       NA       NA         NA
    ## 4 id_004  1995-04-26 Dora  string_2     NA       NA       NA         NA
    ## 5 id_005  1995-04-27 Farf~ string_3     NA       NA       NA         NA
    ## 6 id_002  1995-04-24 Chlor string_4     NA        0.4      0.8       NA
    ## 7 id_001  1995-04-23 Dora  <NA>          0.9      1.4     NA         NA
    ## 8 id_001  1995-04-23 Berta <NA>          7.5     10.1      3         NA
    ## 9 id_005  1995-04-23 Dora  string_3     NA       11.8     56.8       NA
    ## # ... with 1 more variable: priority <dbl>

### Aggregating the `data_dummy` dataframe

1.  We group the dataframe depending on our duplicate criteria `proj_id` and `date`
2.  we sort them by priority
3.  and simply chose the first non `NA` value in each group

``` r
data_dummy %>% 
  group_by(proj_id, date) %>% 
  arrange(priority) %>% 
  summarise_all(funs(first(na.omit(.))))
```

    ## # A tibble: 6 x 9
    ## # Groups:   proj_id [5]
    ##   proj_id date       proj  column_a column_b column_c column_d column_f
    ##   <chr>   <date>     <chr> <chr>       <dbl>    <dbl>    <dbl>    <dbl>
    ## 1 id_001  1995-04-23 Dora  string_0      0.9      1.4      3         53
    ## 2 id_002  1995-04-24 Chlor string_4      0.3      0.4      0.8       NA
    ## 3 id_003  1995-04-25 Chlor <NA>         NA       NA       NA         NA
    ## 4 id_004  1995-04-26 Dora  string_2     NA       NA       NA         NA
    ## 5 id_005  1995-04-23 Dora  string_3     NA       11.8     56.8       NA
    ## 6 id_005  1995-04-27 Farf~ string_3     NA       NA       NA         NA
    ## # ... with 1 more variable: priority <dbl>

This results in an aggregated dataframe, where cell values are chosen

1.  by prioritizing the source
2.  fill in the only availyble value in a column
