library(fs)
library(tidyverse)
library(here)

data_pp <- dir_ls(here("data")) %>%
   map(~ read_csv2(., col_types = "ccdcccdddddd") %>% as_tibble()) %>%
   bind_rows()

data_pp_kk <-  data_pp %>%
   select(1:5, 9) %>%
   rename_all(tolower) %>%
   group_by(mittausid, vuosi, kuukausi) %>%
   summarise(pp = sum(pp_yht)) %>%
   ungroup()

pituus_yli_4 <- data_pp_kk %>%
   count(mittausid, vuosi) %>%
   count(mittausid) %>%
   filter(n > 4)

data_pp_kk %>%
   semi_join(pituus_yli_4) %>%
   count(mittausid, vuosi) %>% view
