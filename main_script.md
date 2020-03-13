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
data_dummy %>% 
  knitr::kable("markdown")
```

| proj\_id | date       | proj     | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|
|:---------|:-----------|:---------|:----------|----------:|----------:|----------:|----------:|
| id\_001  | 1995-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|
| id\_002  | 1995-04-24 | Berta    | string\_1 |        0.3|           |       0.80|           |
| id\_003  | 1995-04-25 | Chlor    |           |           |           |           |           |
| id\_004  | 1995-04-26 | Dora     | string\_2 |           |           |           |           |
| id\_005  | 1995-04-27 | Farfalle | string\_3 |           |           |           |           |
| id\_002  | 1995-04-24 | Chlor    | string\_4 |           |        0.4|       0.95|           |
| id\_001  | 1995-04-23 | Dora     |           |        0.9|        1.4|           |           |
| id\_001  | 1995-04-23 | Berta    |           |        7.5|       10.1|       3.00|           |
| id\_005  | 1995-04-23 | Dora     | string\_3 |           |       11.8|      56.80|           |
| id\_001  | 1990-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|
| id\_001  | 1990-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|

Get unique combination
----------------------

``` r
data_dummy %>% 
  drop_na(column_b) %>% 
  janitor::get_dupes(proj_id) %>% 
  group_by(proj_id) %>% 
  drop_na(date) %>% 
  filter(date == min(date)) %>% 
  ungroup() %>%
  distinct(proj_id, column_b, .keep_all = TRUE) %>% 
  knitr::kable("markdown")
```

| proj\_id |  dupe\_count| date       | proj  | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|
|:---------|------------:|:-----------|:------|:----------|----------:|----------:|----------:|----------:|
| id\_001  |            5| 1990-04-23 | Ampel | string\_0 |        0.7|           |           |         53|

Problem Definition
==================

The goal is to merge duplicates of rows. The criterion for duplicates is

-   same `proj_id` value
-   same `date` value

Find Duplicates
---------------

### Way 1 - Janitor

``` r
data_dummy_duplicates <- data_dummy %>% 
  janitor::get_dupes(proj_id, date) %>% 
  select(-dupe_count)
```

Show the resulting dataframe

``` r
data_dummy_duplicates %>% 
  knitr::kable("markdown")
```

| proj\_id | date       | proj  | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|
|:---------|:-----------|:------|:----------|----------:|----------:|----------:|----------:|
| id\_001  | 1990-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1990-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1995-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1995-04-23 | Dora  |           |        0.9|        1.4|           |           |
| id\_001  | 1995-04-23 | Berta |           |        7.5|       10.1|       3.00|           |
| id\_002  | 1995-04-24 | Berta | string\_1 |        0.3|           |       0.80|           |
| id\_002  | 1995-04-24 | Chlor | string\_4 |           |        0.4|       0.95|           |

Aggregation
===========

Method 1 - with prioritizing confidence
---------------------------------------

### `priority_key`

In the following steps we define a dataframe containing priorities to each unique project

``` r
priority_key <- data_dummy %>% 
  distinct(proj)
```

Now we add values for the priorities

``` r
priority_key <- priority_key %>% 
  mutate(priority = c(5, 3, 1, 2, 4))
```

Show the resulting dataframe

``` r
priority_key %>% 
  knitr::kable("markdown")
```

| proj     |  priority|
|:---------|---------:|
| Ampel    |         5|
| Berta    |         3|
| Chlor    |         1|
| Dora     |         2|
| Farfalle |         4|

We join the 2 dataframes

``` r
data_dummy <- data_dummy %>% 
  left_join(priority_key, by = 'proj')
```

Show the resulting dataframe

``` r
data_dummy %>% 
  knitr::kable("markdown")
```

| proj\_id | date       | proj     | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|  priority|
|:---------|:-----------|:---------|:----------|----------:|----------:|----------:|----------:|---------:|
| id\_001  | 1995-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|         5|
| id\_002  | 1995-04-24 | Berta    | string\_1 |        0.3|           |       0.80|           |         3|
| id\_003  | 1995-04-25 | Chlor    |           |           |           |           |           |         1|
| id\_004  | 1995-04-26 | Dora     | string\_2 |           |           |           |           |         2|
| id\_005  | 1995-04-27 | Farfalle | string\_3 |           |           |           |           |         4|
| id\_002  | 1995-04-24 | Chlor    | string\_4 |           |        0.4|       0.95|           |         1|
| id\_001  | 1995-04-23 | Dora     |           |        0.9|        1.4|           |           |         2|
| id\_001  | 1995-04-23 | Berta    |           |        7.5|       10.1|       3.00|           |         3|
| id\_005  | 1995-04-23 | Dora     | string\_3 |           |       11.8|      56.80|           |         2|
| id\_001  | 1990-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|         5|
| id\_001  | 1990-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|         5|

### Aggregating the `data_dummy` dataframe

1.  We group the dataframe depending on our duplicate criteria `proj_id` and `date`
2.  we sort them by priority
3.  and simply chose the first non `NA` value in each group
4.  remove the column `priority`

``` r
data_dummy_aggregated <- data_dummy %>% 
  group_by(proj_id, date) %>% 
  arrange(priority) %>% 
  summarise_all(funs(first(na.omit(.)))) %>% 
  select(-priority)
```

This results in an aggregated dataframe, where cell values are chosen

1.  by prioritizing the source
2.  fill in the only availyble value in a column

Show the resulting dataframe. For comparison the dateframe containing the duplicates is displayed below.

``` r
data_dummy_aggregated %>% 
  list(., data_dummy_duplicates) %>% 
  knitr::kable("markdown")
```

| proj\_id | date       | proj     | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|
|:---------|:-----------|:---------|:----------|----------:|----------:|----------:|----------:|
| id\_001  | 1990-04-23 | Ampel    | string\_0 |        0.7|           |           |         53|
| id\_001  | 1995-04-23 | Dora     | string\_0 |        0.9|        1.4|       3.00|         53|
| id\_002  | 1995-04-24 | Chlor    | string\_4 |        0.3|        0.4|       0.95|           |
| id\_003  | 1995-04-25 | Chlor    |           |           |           |           |           |
| id\_004  | 1995-04-26 | Dora     | string\_2 |           |           |           |           |
| id\_005  | 1995-04-23 | Dora     | string\_3 |           |       11.8|      56.80|           |
| id\_005  | 1995-04-27 | Farfalle | string\_3 |           |           |           |           |

| proj\_id | date       | proj  | column\_a |  column\_b|  column\_c|  column\_d|  column\_f|
|:---------|:-----------|:------|:----------|----------:|----------:|----------:|----------:|
| id\_001  | 1990-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1990-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1995-04-23 | Ampel | string\_0 |        0.7|           |           |         53|
| id\_001  | 1995-04-23 | Dora  |           |        0.9|        1.4|           |           |
| id\_001  | 1995-04-23 | Berta |           |        7.5|       10.1|       3.00|           |
| id\_002  | 1995-04-24 | Berta | string\_1 |        0.3|           |       0.80|           |
| id\_002  | 1995-04-24 | Chlor | string\_4 |           |        0.4|       0.95|           |
