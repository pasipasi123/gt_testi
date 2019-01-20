Trying out the `gt` package
================
Pasi Haapakorva
Sun Jan 20 15:58:57 2019

``` r
library(gt)
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.1.0.9000     v purrr   0.2.5     
    ## v tibble  2.0.1          v dplyr   0.8.0     
    ## v tidyr   0.8.2          v stringr 1.3.1     
    ## v readr   1.3.1          v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(here)
```

    ## here() starts at C:/Users/pasih_000/Documents/rprojektit/gt_testi

``` r
hep <- fs::dir_ls(here("data")) %>%
   str_subset("ID_(89|96)") %>%
   map(~ read_csv2(., col_types = "ccdcccdddddd") %>% as_tibble())
```

    ## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.

    ## Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.

``` r
hep2 <- hep %>%
   bind_rows() %>%
   group_by(MittausID, Vuosi, Kuukausi) %>%
   summarise(pp = sum(PP_YHT)) %>%
   ungroup() %>%
   rename_all(tolower) %>%
   mutate(kuukausi = as.numeric(kuukausi)) %>%
   complete(mittausid, vuosi, kuukausi, fill = list(pp = 0)) %>%
   filter(!(vuosi == 2018 & kuukausi > 9))

hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4] <-
   hep2[c(hep2$kuukausi %in% 8:11 & hep2$mittausid == 96 & hep2$vuosi == 2011), 4]

hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4] <-
   hep2[c(hep2$kuukausi %in% 4:6 & hep2$mittausid == 96 & hep2$vuosi == 2012), 4]

hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2014), 4] <-
   hep2[c(hep2$kuukausi %in% 10:12 & hep2$mittausid == 96 & hep2$vuosi == 2013), 4]

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
```

    ## Joining, by = "mittausid"

``` r
gt_data <- hep2 %>%
   group_by(mittausid, vuosi) %>%
   summarise(pp_all = sum(pp)) %>%
   left_join(pp_means) %>%
   mutate(osuus = case_when(vuosi == 2018 ~ pp_all / pp_9_mean - 1,
                            TRUE ~ pp_all / pp_all_mean - 1)) %>%
   ungroup()
```

    ## Joining, by = "mittausid"

``` r
gt_data %>%
   mutate(mittausid = case_when(mittausid == "89" ~ "Hupisaaret",
                                TRUE ~ "Ouluhalli")) %>%
   gt(rowname_col = "vuosi", groupname_col = "mittausid") %>%
   cols_hide(vars(pp_all_mean, pp_9_mean)) %>%
   cols_label(pp_all = "Pyöräliikenne, lkm", osuus = "Vertailuluku") %>%
   fmt_percent(columns = vars(osuus), sep_mark = " ", dec_mark = ",", decimals = 1, incl_space = TRUE) %>%
   fmt_number(columns = vars(pp_all), sep_mark = " ", dec_mark = ",", decimals = 0)
```

<!--html_preserve-->
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ggllgiqvfk .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #000000;
  font-size: 16px;
  background-color: #FFFFFF;
  /* table.background.color */
  width: auto;
  /* table.width */
  border-top-style: solid;
  /* table.border.top.style */
  border-top-width: 2px;
  /* table.border.top.width */
  border-top-color: #A8A8A8;
  /* table.border.top.color */
}

#ggllgiqvfk .gt_heading {
  background-color: #FFFFFF;
  /* heading.background.color */
  border-bottom-color: #FFFFFF;
}

#ggllgiqvfk .gt_title {
  color: #000000;
  font-size: 125%;
  /* heading.title.font.size */
  padding-top: 4px;
  /* heading.top.padding */
  padding-bottom: 1px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ggllgiqvfk .gt_subtitle {
  color: #000000;
  font-size: 85%;
  /* heading.subtitle.font.size */
  padding-top: 1px;
  padding-bottom: 4px;
  /* heading.bottom.padding */
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ggllgiqvfk .gt_bottom_border {
  border-bottom-style: solid;
  /* heading.border.bottom.style */
  border-bottom-width: 2px;
  /* heading.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* heading.border.bottom.color */
}

#ggllgiqvfk .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  padding-top: 4px;
  padding-bottom: 4px;
}

#ggllgiqvfk .gt_col_heading {
  color: #000000;
  background-color: #FFFFFF;
  /* column_labels.background.color */
  font-size: 16px;
  /* column_labels.font.size */
  font-weight: initial;
  /* column_labels.font.weight */
  vertical-align: middle;
  padding: 10px;
  margin: 10px;
}

#ggllgiqvfk .gt_sep_right {
  border-right: 5px solid #FFFFFF;
}

#ggllgiqvfk .gt_group_heading {
  padding: 8px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#ggllgiqvfk .gt_empty_group_heading {
  padding: 0.5px;
  color: #000000;
  background-color: #FFFFFF;
  /* stub_group.background.color */
  font-size: 16px;
  /* stub_group.font.size */
  font-weight: initial;
  /* stub_group.font.weight */
  border-top-style: solid;
  /* stub_group.border.top.style */
  border-top-width: 2px;
  /* stub_group.border.top.width */
  border-top-color: #A8A8A8;
  /* stub_group.border.top.color */
  border-bottom-style: solid;
  /* stub_group.border.bottom.style */
  border-bottom-width: 2px;
  /* stub_group.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* stub_group.border.bottom.color */
  vertical-align: middle;
}

#ggllgiqvfk .gt_striped {
  background-color: #f2f2f2;
}

#ggllgiqvfk .gt_row {
  padding: 10px;
  /* row.padding */
  margin: 10px;
  vertical-align: middle;
}

#ggllgiqvfk .gt_stub {
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #A8A8A8;
  padding-left: 12px;
}

#ggllgiqvfk .gt_stub.gt_row {
  background-color: #FFFFFF;
}

#ggllgiqvfk .gt_summary_row {
  background-color: #FFFFFF;
  /* summary_row.background.color */
  padding: 6px;
  /* summary_row.padding */
  text-transform: inherit;
  /* summary_row.text_transform */
}

#ggllgiqvfk .gt_first_summary_row {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
}

#ggllgiqvfk .gt_table_body {
  border-top-style: solid;
  /* field.border.top.style */
  border-top-width: 2px;
  /* field.border.top.width */
  border-top-color: #A8A8A8;
  /* field.border.top.color */
  border-bottom-style: solid;
  /* field.border.bottom.style */
  border-bottom-width: 2px;
  /* field.border.bottom.width */
  border-bottom-color: #A8A8A8;
  /* field.border.bottom.color */
}

#ggllgiqvfk .gt_footnote {
  font-size: 90%;
  /* footnote.font.size */
  padding: 4px;
  /* footnote.padding */
}

#ggllgiqvfk .gt_sourcenote {
  font-size: 90%;
  /* sourcenote.font.size */
  padding: 4px;
  /* sourcenote.padding */
}

#ggllgiqvfk .gt_center {
  text-align: center;
}

#ggllgiqvfk .gt_left {
  text-align: left;
}

#ggllgiqvfk .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ggllgiqvfk .gt_font_normal {
  font-weight: normal;
}

#ggllgiqvfk .gt_font_bold {
  font-weight: bold;
}

#ggllgiqvfk .gt_font_italic {
  font-style: italic;
}

#ggllgiqvfk .gt_super {
  font-size: 65%;
}

#ggllgiqvfk .gt_footnote_glyph {
  font-style: italic;
  font-size: 65%;
}
</style>
<!--gt table start-->
<table class="gt_table">
<tr>
<th class="gt_col_heading gt_left" rowspan="1" colspan="1">
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Pyöräliikenne, lkm
</th>
<th class="gt_col_heading gt_right" rowspan="1" colspan="1">
Vertailuluku
</th>
</tr>
<tbody class="gt_table_body">
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
Hupisaaret
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2011
</td>
<td class="gt_row gt_right">
947 328
</td>
<td class="gt_row gt_right">
4,5 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2012
</td>
<td class="gt_row gt_right gt_striped">
863 748
</td>
<td class="gt_row gt_right gt_striped">
-4,7 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2013
</td>
<td class="gt_row gt_right">
907 509
</td>
<td class="gt_row gt_right">
0,1 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2014
</td>
<td class="gt_row gt_right gt_striped">
856 681
</td>
<td class="gt_row gt_right gt_striped">
-5,5 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2015
</td>
<td class="gt_row gt_right">
857 191
</td>
<td class="gt_row gt_right">
-5,4 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2016
</td>
<td class="gt_row gt_right gt_striped">
818 306
</td>
<td class="gt_row gt_right gt_striped">
-9,7 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2017
</td>
<td class="gt_row gt_right">
731 361
</td>
<td class="gt_row gt_right">
-19,3 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2018
</td>
<td class="gt_row gt_right gt_striped">
610 149
</td>
<td class="gt_row gt_right gt_striped">
-16,5 %
</td>
</tr>
<tr class="gt_group_heading_row">
<td colspan="3" class="gt_group_heading">
Ouluhalli
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2011
</td>
<td class="gt_row gt_right">
631 486
</td>
<td class="gt_row gt_right">
2,4 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2012
</td>
<td class="gt_row gt_right gt_striped">
605 763
</td>
<td class="gt_row gt_right gt_striped">
-1,8 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2013
</td>
<td class="gt_row gt_right">
612 616
</td>
<td class="gt_row gt_right">
-0,6 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2014
</td>
<td class="gt_row gt_right gt_striped">
631 236
</td>
<td class="gt_row gt_right gt_striped">
2,4 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2015
</td>
<td class="gt_row gt_right">
670 178
</td>
<td class="gt_row gt_right">
8,7 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2016
</td>
<td class="gt_row gt_right gt_striped">
646 460
</td>
<td class="gt_row gt_right gt_striped">
4,8 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2017
</td>
<td class="gt_row gt_right">
632 357
</td>
<td class="gt_row gt_right">
2,6 %
</td>
</tr>
<tr>
<td class="gt_row gt_stub gt_left">
2018
</td>
<td class="gt_row gt_right gt_striped">
507 098
</td>
<td class="gt_row gt_right gt_striped">
7,0 %
</td>
</tr>
</tbody>
</table>
<!--gt table end-->

<!--/html_preserve-->
