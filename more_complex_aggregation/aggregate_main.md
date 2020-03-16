Unsupervised Aggregattion of Rows in Large Dataframe 2
================
Maximilian Nölscher
16 März 2020

Presettings
===========

Load Packages
-------------

``` r
library(tidyverse)
```

Load Custom Functions
---------------------

``` r
purrr::map(
  list.files(
    path = './functions/',
    pattern = "\\.R$",
    full.names = TRUE,
    recursive = TRUE
  ),
  source
)
```

    ## [[1]]
    ## [[1]]$value
    ## function (df, priority_df, method_name) 
    ## {
    ##     if (method_name == "mean") {
    ##         columns_mean <- priority_df %>% filter(apply_function == 
    ##             method_name) %>% distinct(field) %>% pull(field)
    ##         data_dummy_aggregated <- df %>% group_by(id, project) %>% 
    ##             summarise_at(.vars = vars(one_of(columns_mean)), 
    ##                 .funs = mean, na.rm = TRUE)
    ##         return(data_dummy_aggregated)
    ##     }
    ##     else if (method_name == "max") {
    ##         columns_max <- priority_df %>% filter(apply_function == 
    ##             "max") %>% distinct(field) %>% pull(field)
    ##         data_dummy_aggregated <- df %>% group_by(id, project) %>% 
    ##             summarise_at(.vars = vars(one_of(columns_max)), .funs = max, 
    ##                 na.rm = TRUE)
    ##         return(data_dummy_aggregated)
    ##     }
    ##     else if (method_name == "max char") {
    ##         df <- df %>% mutate(alang_lenght = str_length(alang))
    ##         columns_max_char <- priority_df %>% filter(apply_function == 
    ##             "max char") %>% distinct(field) %>% pull(field)
    ##         data_dummy_aggregated <- df %>% group_by(id, project) %>% 
    ##             filter(alang_lenght == max(alang_lenght, na.rm = TRUE)) %>% 
    ##             slice(1) %>% select(id, project, one_of(columns_max_char))
    ##         return(data_dummy_aggregated)
    ##     }
    ##     else if (method_name == "first") {
    ##         columns_first <- priority_df %>% filter(apply_function == 
    ##             "first") %>% distinct(field) %>% pull(field)
    ##         priority_join <- priority_df %>% filter(field %in% columns_first) %>% 
    ##             pivot_wider(names_from = "field", values_from = "priority") %>% 
    ##             rename_at(vars(-c("project", "apply_function")), 
    ##                 str_c, "_prio")
    ##         data_dummy_aggregated <- df %>% left_join(priority_join, 
    ##             by = "project") %>% select(id, project, matches(str_c(columns_first, 
    ##             collapse = "|"))) %>% pivot_longer(cols = contains("prio")) %>% 
    ##             group_by(id, project, name) %>% arrange(value) %>% 
    ##             summarise_all(funs(first(na.omit(.)))) %>% pivot_wider(names_from = "name", 
    ##             values_from = "value") %>% select(-contains("prio"))
    ##         return(data_dummy_aggregated)
    ##     }
    ##     else if (method_name == "noclue") {
    ##         columns_noclue <- priority_df %>% filter(apply_function == 
    ##             "noclue") %>% distinct(field) %>% pull(field)
    ##         data_dummy_aggregated <- data_dummy %>% group_by(id, 
    ##             project) %>% summarise_all(funs(first(na.omit(.)))) %>% 
    ##             select(id, project, one_of(columns_noclue))
    ##         return(data_dummy_aggregated)
    ##     }
    ## }
    ## 
    ## [[1]]$visible
    ## [1] FALSE

Data Import
===========

Dataframe to Be Aggregated
--------------------------

``` r
data_dummy <- read_csv2('../raw_data/dummy2.csv') %>% 
  janitor::clean_names()
```

Show the dataframe

``` r
data_dummy %>% 
  knitr::kable("markdown")
```

|   id| project | alang    |  bmean|  cmax|   dx|   ey|    f|
|----:|:--------|:---------|------:|-----:|----:|----:|----:|
|    1| y       | kurz     |     10|     5|    3|   10|    2|
|    1| x       | kurz     |       |    10|     |    3|   10|
|    1| y       | langlang |     20|     1|   10|    2|    5|
|    2| y       | kurz     |     10|     5|    3|   10|     |
|    2| y       | kurz     |       |    10|     |    3|     |
|    2| x       | langlang |     20|     1|   10|    2|     |

Import Priority Dataframe
-------------------------

``` r
priority_key <- read_csv2('../raw_data/dummy_priority.csv') %>% 
  mutate(field = str_to_lower(field))
```

Show the dataframe

``` r
priority_key %>% 
  knitr::kable("markdown")
```

| field | project | apply\_function |  priority|
|:------|:--------|:----------------|---------:|
| alang | x       | max char        |         1|
| alang | y       | max char        |         1|
| bmean | x       | mean            |         1|
| bmean | y       | mean            |         1|
| cmax  | x       | max             |         1|
| cmax  | y       | max             |         1|
| dx    | x       | first           |         1|
| dx    | y       | first           |         2|
| ey    | x       | first           |         2|
| ey    | y       | first           |         1|
| f     | x       | noclue          |         1|
| f     | y       | noclue          |         1|

Processing
==========

Show all duplicates

``` r
data_dummy_duplicates <- data_dummy %>% 
  janitor::get_dupes(id, project) %>% 
  select(-dupe_count)
```

List all functions

``` r
method_list <- priority_key %>% 
  distinct(apply_function) %>% 
  pull(apply_function)
```

Apply each aggregation method to the defined subset of columns

``` r
df_list <- method_list %>% 
  map(~cus_fun_aggregate_columns_by_method(data_dummy, priority_key, .))
```

Join the individual dataframes into one

``` r
data_dummy_aggregated <- df_list %>% 
  reduce(inner_join, by = c('id', 'project'))
```

``` r
data_dummy_aggregated %>% 
  list(., 
       data_dummy_duplicates, 
       data_dummy) %>% 
  knitr::kable("markdown")
```

|   id| project | alang    |  bmean|  cmax|   dx|   ey|    f|
|----:|:--------|:---------|------:|-----:|----:|----:|----:|
|    1| x       | kurz     |       |    10|     |    3|   10|
|    1| y       | langlang |     15|     5|    3|   10|    2|
|    2| x       | langlang |     20|     1|   10|    2|     |
|    2| y       | kurz     |     10|    10|    3|   10|     |

|   id| project | alang    |  bmean|  cmax|   dx|   ey|    f|
|----:|:--------|:---------|------:|-----:|----:|----:|----:|
|    1| y       | kurz     |     10|     5|    3|   10|    2|
|    1| y       | langlang |     20|     1|   10|    2|    5|
|    2| y       | kurz     |     10|     5|    3|   10|     |
|    2| y       | kurz     |       |    10|     |    3|     |

|   id| project | alang    |  bmean|  cmax|   dx|   ey|    f|
|----:|:--------|:---------|------:|-----:|----:|----:|----:|
|    1| y       | kurz     |     10|     5|    3|   10|    2|
|    1| x       | kurz     |       |    10|     |    3|   10|
|    1| y       | langlang |     20|     1|   10|    2|    5|
|    2| y       | kurz     |     10|     5|    3|   10|     |
|    2| y       | kurz     |       |    10|     |    3|     |
|    2| x       | langlang |     20|     1|   10|    2|     |
