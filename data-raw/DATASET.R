## code to prepare `DATASET` dataset goes here

library(tidyverse)

library(magrittr)

ARI <- openxlsx::read.xlsx(file.choose()) %>% as_tibble

usethis::use_data(ARI, overwrite = TRUE, compress = 'xz')
