library(gt)
library(tidyverse)
library(here)

hep <- fs::dir_ls(here("data")) %>%
   str_subset("ID_(89|96)") %>%
   map(~ read_csv2(., col_types = "ccdcccdddddd") %>% as_tibble())

hep2 <- hep %>%
   bind_rows() %>%
   group_by(MittausID, Vuosi, Kuukausi) %>%
   summarise(pp = sum(PP_YHT)) %>%
   rename_all(tolower) %>%
   mutate(kuukausi = as.numeric(kuukausi)) %>%
   ungroup() %>%
   complete(mittausid, vuosi, kuukausi, fill = list(pp = 0)) %>%
   filter(!(vuosi == 2018 & kuukausi > 9))

hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4] <-
   hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2011), 4]

hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4] <-
   hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4]

hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2014), 4] <-
   hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4]

pp_summary <- function(x) {
   mean(x[.data[[vuosi]] %in% 2011:2013])
   mean(x[.data[[vuosi]] %in% 2011:2013 & .data[[kuukausi]] %in% 1:9])
}

hep2 %>%
   group_by(mittausid, vuosi) %>%
   summarise(pp_vuosi = sum(pp))

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
   ungroup()


pp2018 <- ainola %>%
   group_by(Vuosi, Kuukausi) %>%
   summarise(pp = sum(PP_YHT)) %>%
   # mutate(Kuukausi = as.numeric(Kuukausi)) %>%
   filter(as.numeric(Kuukausi) %in% 1:9) %>%
   summarise(pp = sum(pp)) %>%
   mutate(vrt = mean(pp[.$Vuosi %in% 2011:2013])) %>%
   tail(1)

ainola_data <- ainola %>%
   filter(Vuosi < 2018) %>%
   group_by(Vuosi) %>%
   summarise(pp = sum(PP_YHT)) %>%
   mutate(vrt = mean(pp[.$Vuosi %in% 2011:2013])) %>%
   bind_rows(pp2018) %>%
   mutate(osuus = pp / vrt - 1) %>%
   mutate(osuus = paste0(format(round(osuus * 100, 1), decimal.mark = ","), " %")) %>%
   mutate(pp = format(pp, big.mark = " "))

ainola_data %>%
   gt() %>%
   cols_label(pp = "Pyöräliikenne, lkm", osuus = "Vertailuluku") %>%
   cols_hide(vars(vrt)) %>%
   cols_align(align = "right", columns = vars(Vuosi, pp, osuus)) %>%
   tab_header("Pyöräliikenteen määrä vuosittain", "Mittauspiste: Hupisaaret") %>%
   tab_footnote("Verrattuna vuosien 2011-2013 keskiarvoon", locations = cells_column_labels("osuus")) %>%
   tab_footnote("Kuin edellinen, mutta kuukaudet 1-9", locations = cells_data(columns = "osuus", rows = 8))

gt_data %>%
   # mutate(mittausid = as.factor(mittausid)) %>%
   mutate(mittausid = case_when(mittausid == "89" ~ "Hupisaaret",
                                TRUE ~ "Ouluhalli")) %>%
   gt(rowname_col = "vuosi", groupname_col = "mittausid") %>%
   cols_label(pp_all = "Pyöräliikenne, lkm", osuus = "Vertailuluku") %>%
   cols_hide(vars(pp_all_mean, pp_9_mean)) %>%
   fmt_percent(columns = vars(osuus), sep_mark = " ", dec_mark = ",", decimals = 1, incl_space = TRUE) %>%
   fmt_number(columns = vars(pp_all), sep_mark = " ", dec_mark = ",", decimals = 0) %>%
   data_color(columns = vars(osuus), colors = scales::col_numeric(palette = "Reds", domain = NULL))


