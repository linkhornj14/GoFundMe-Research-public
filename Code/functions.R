#function list for econometrics

above_below_median <- function(df, var){
  df %>% 
    select(df.var) %>% 
    mutate(df.var.median = median(df.var),
           def.var.above.below = ifelse(df.var.median > df.var.median, "above", "below")
    ) 
}


summarise_continuous = function(d, vars) {
  d %>%
    select(all_of(vars)) %>%
    mutate_all(as.numeric) %>%
    summarise(across(all_of(vars), list(N = ~sum(!is.na(.)), 
                                         mean = ~mean(., na.rm=T), 
                                         sd = ~sd(., na.rm=T), 
                                         median = ~median(., na.rm=T),
                                         min = ~min(., na.rm=T),
                                         max = ~max(., na.rm=T)))) %>% 
    pivot_longer(everything(), 
                 names_to = c("variable",".value"),
                 names_pattern = "(.+)_(.+)") # %>%
  # knitr::kable()
  # uncomment these bits if you want a nicely formatted table in a .Rmd document
}

#list duplicates
dups = function(df) {
  df %>%
  group_by_all() %>%
  filter(n() >1) %>%
  ungroup()
}

#list na's
dataNA = function(data){ data %>% filter_all(any_vars(is.na(.))) }
