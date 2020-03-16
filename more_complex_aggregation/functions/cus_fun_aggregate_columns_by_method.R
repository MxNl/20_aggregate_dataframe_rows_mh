cus_fun_aggregate_columns_by_method <- function(df, priority_df, method_name, columns_to_group_by) {
  
  ############### Test ##########
  
  # df <- data_dummy
  # priority_df <- priority_key
  # method_name <- 'mean'
  # columns_to_group_by <- 'id'
  
  #############################
  
  if (method_name == "mean") {
    
    columns_mean <- priority_df %>%
      filter(apply_function == method_name) %>%
      distinct(field) %>%
      pull(field)
    
    data_dummy_aggregated <- df %>%
      group_by_at(vars(one_of(columns_to_group_by))) %>% 
      summarise_at(
        .vars = vars(one_of(columns_mean)),
        .funs = mean,
        na.rm = TRUE
      )
    
    return(data_dummy_aggregated)
    
  } else if (method_name == "max") {
    
    columns_max <- priority_df %>%
      filter(apply_function == "max") %>%
      distinct(field) %>%
      pull(field)
    
    data_dummy_aggregated <- df %>%
      group_by_at(vars(one_of(columns_to_group_by))) %>% 
      summarise_at(
        .vars = vars(one_of(columns_max)),
        .funs = max,
        na.rm = TRUE
      )
    
    return(data_dummy_aggregated)
    
  } else if (method_name == "max char") {
    
    df <- df %>%
      mutate(alang_lenght = str_length(alang))
    
    columns_max_char <- priority_df %>%
      filter(apply_function == "max char") %>%
      distinct(field) %>%
      pull(field)
    
    data_dummy_aggregated <- df %>%
      group_by_at(vars(one_of(columns_to_group_by))) %>% 
      filter(alang_lenght == max(alang_lenght, na.rm = TRUE)) %>%
      slice(1) %>%
      select(one_of(columns_to_group_by), one_of(columns_max_char))
    
    return(data_dummy_aggregated)
    
  } else if (method_name == "first") {
    
    columns_first <- priority_df %>%
      filter(apply_function == "first") %>%
      distinct(field) %>%
      pull(field)
    
    priority_join <- priority_df %>%
      filter(field %in% columns_first) %>%
      pivot_wider(names_from = "field", values_from = "priority") %>%
      rename_at(vars(-c("project", "apply_function")), str_c, "_prio")
    
    data_dummy_aggregated <- df %>%
      left_join(priority_join, by = "project") %>%
      select(one_of(columns_to_group_by), matches(str_c(columns_first, collapse = "|"))) %>%
      pivot_longer(cols = contains("prio")) %>%
      pivot_longer(cols = c('dx', 'ey'), names_to = 'name1', values_to = 'value1') %>%
      mutate(name = str_sub(name, 1, 2)) %>% 
      filter(name == name1) %>% 
      select(-name1) %>% 
      group_by_at(vars(one_of(columns_to_group_by), name)) %>% 
      arrange(value) %>%
      summarise_all(funs(first(na.omit(.)))) %>%
      pivot_wider(names_from = "name", values_from = "value1") %>%
      select(-value)
    
    return(data_dummy_aggregated)
    
  } else if (method_name == "noclue") {
    
    columns_noclue <- priority_df %>%
      filter(apply_function == "noclue") %>%
      distinct(field) %>%
      pull(field)
    
    data_dummy_aggregated <- data_dummy %>% 
      group_by_at(vars(one_of(columns_to_group_by))) %>% 
      summarise_all(funs(first(na.omit(.)))) %>% 
      select(one_of(columns_to_group_by), one_of(columns_noclue))
    
    return(data_dummy_aggregated)
    
  }
}