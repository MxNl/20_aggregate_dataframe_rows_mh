data_dummy <- read_csv2('raw_data/dummy2.csv') %>% 
  janitor::clean_names()

data_dummy <- data_dummy %>% 
  mutate(alang_lenght = str_length(alang))

priority_key <- read_csv2('raw_data/dummy_priority.csv') %>% 
  mutate(field = str_to_lower(field))

# column_groups <- priority_key %>% 
#   group_by(apply_function) %>% 
#   distinct(field) %>% 
#   pull(field) %>% 
#   group_split()

files <- list.files('')

source('')


