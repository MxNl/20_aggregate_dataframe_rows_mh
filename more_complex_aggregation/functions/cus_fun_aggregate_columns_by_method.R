cus_fun_aggregate_columns_by_method <- function(df, priority_df, method_name) {
  
  if (method_name == "mean") {
    
    columns_mean <- priority_df %>%
      filter(apply_function == method_name) %>%
      distinct(field) %>%
      pull(field)

    data_dummy_aggregated <- df %>%
      group_by(id, project) %>%
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
      group_by(id, project) %>%
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
      group_by(id, project) %>%
      filter(alang_lenght == max(alang_lenght, na.rm = TRUE)) %>%
      slice(1) %>%
      select(id, project, one_of(columns_max_char))
    
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
      select(id, project, matches(str_c(columns_first, collapse = "|"))) %>%
      pivot_longer(cols = contains("prio")) %>%
      group_by(id, project, name) %>%
      arrange(value) %>%
      summarise_all(funs(first(na.omit(.)))) %>%
      pivot_wider(names_from = "name", values_from = "value") %>%
      select(-contains("prio"))
    
    return(data_dummy_aggregated)
    
  } else if (method_name == "noclue") {
    
    columns_noclue <- priority_df %>%
      filter(apply_function == "noclue") %>%
      distinct(field) %>%
      pull(field)
    
    data_dummy_aggregated <- data_dummy %>% 
      group_by(id, project) %>% 
      summarise_all(funs(first(na.omit(.)))) %>% 
      select(id, project, one_of(columns_noclue))
    
    return(data_dummy_aggregated)
    
  }
}