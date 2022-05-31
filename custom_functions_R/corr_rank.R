corr_rank <- function(df) {
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
  
  df_corr <- df %>% 
    as.matrix %>% 
    cor %>% 
    as.data.frame %>% 
    rownames_to_column(var = 'var1') %>% 
    gather(var2, value, -var1)
  
  # remove duplicates and sort by correlation value
  df_corr %>%
    mutate(var_order = paste(var1, var2) %>%
             strsplit(split = ' ') %>%
             purrr::map_chr( ~ sort(.x) %>% 
                               paste(collapse = ' '))) %>%
    mutate(cnt = 1) %>%
    group_by(var_order) %>%
    mutate(cumsum = cumsum(cnt)) %>%
    filter(cumsum != 2) %>%
    ungroup %>%
    select(-var_order, -cnt, -cumsum) %>% 
    filter(value != 1) %>% 
    arrange(desc(value))
}
