
library(tidyverse)

cntrs <- list.files("data/predictions/", ".csv$") %>% 
  str_sub(., 12, nchar(.) - 4)

list.files("data/predictions/", ".csv$", full.names = TRUE) %>%
  map(read_csv) %>%
  map(., ~ filter(., prediction_fire_prob > 0.5)) %>%
  map(., ~ dplyr::select(., patch, year, x, y, area, magnitude.NBR, pre.NBR)) %>%
  map2(.x = ., .y = cntrs, ~ write_csv(.x, paste0("data/patches/fire_patches_", .y, ".csv")))
