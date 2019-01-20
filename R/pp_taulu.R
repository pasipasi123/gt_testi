#' ---
#' title: Trying out the `gt` package
#' author: Pasi Haapakorva
#' output: github_document
#' ---

library(gt)
library(tidyverse)
library(here)

# Here I'm loading data acquired from the City of Oulu about the number of cyclist passing an ecocounter

hep <- fs::dir_ls(here("data")) %>%
   str_subset("ID_(89|96)") %>%
   map(~ read_csv2(., col_types = "ccdcccdddddd", locale = locale(encoding = "ISO-8859-1")) %>% as_tibble())

hep2 <- hep %>%
   bind_rows() %>%
   rename_all(tolower) %>%
   mutate(kuukausi = as.numeric(kuukausi)) %>%
   group_by(mittausid, vuosi, kuukausi) %>%
   summarise(pp = sum(pp_yht)) %>%
   ungroup() %>%
   complete(mittausid, vuosi, kuukausi, fill = list(pp = 0)) %>%
   filter(!(vuosi == 2018 & kuukausi > 9))

hep2 %>%
   ggplot(aes(lubridate::make_date(vuosi, kuukausi), pp, color = mittausid)) +
   geom_line()

# There seems to be some problems with the data. Some periods have less
# observations. Let's replace them with previous years' values.

hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4] <-
   hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2011), 4]

hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4] <-
   hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4]

hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2014), 4] <-
   hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4]

hep2 %>%
   ggplot(aes(lubridate::make_date(vuosi, kuukausi), pp, color = mittausid)) +
   geom_line()

# I want to compare later years to mean of the first three years.
# Year 2018 is incomplete, so I'll calculate another mean for it.

pp_all_mean <- hep2 %>%
   group_by(mittausid, vuosi) %>%
   summarise(pp_all = sum(pp)) %>%
   group_by(mittausid) %>%
   mutate(pp_all_mean = mean(pp_all[vuosi %in% 2011:2013])) %>%
   distinct(mittausid, pp_all_mean)

pp_9_mean <- hep2 %>%
   filter(kuukausi < 10) %>%
   group_by(mittausid, vuosi) %>%
   summarise(pp_all = sum(pp)) %>%
   group_by(mittausid) %>%
   mutate(pp_9_mean = mean(pp_all[vuosi %in% 2011:2013])) %>%
   distinct(mittausid, pp_9_mean)

pp_means <- pp_all_mean %>%
   left_join(pp_9_mean)

gt_data <- hep2 %>%
   group_by(mittausid, vuosi) %>%
   summarise(pp_all = sum(pp)) %>%
   left_join(pp_means) %>%
   mutate(osuus = case_when(vuosi == 2018 ~ pp_all / pp_9_mean - 1,
                            TRUE ~ pp_all / pp_all_mean - 1)) %>%
   ungroup() %>%
   mutate(mittausid = case_when(mittausid == "89" ~ "Hupisaaret",
                                TRUE ~ "Ouluhalli"))

# Building the `gt` table

gt_pp <- gt_data %>%
   gt(rowname_col = "vuosi", groupname_col = "mittausid") %>%
   cols_hide(vars(pp_all_mean, pp_9_mean)) %>%
   cols_label(pp_all = "Pyöräliikenne, lkm", osuus = "Vertailuluku") %>%
   fmt_percent(columns = vars(osuus), sep_mark = " ", dec_mark = ",", decimals = 1, incl_space = TRUE) %>%
   fmt_number(columns = vars(pp_all), sep_mark = " ", dec_mark = ",", decimals = 0)

# It looks like some of the formatting is lost in this markdown file, eg. alignment.

gt_pp

fs::file_copy(here("R", "pp_taulu.md"), here("readme.MD"))
