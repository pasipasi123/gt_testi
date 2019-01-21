# function test

library(gt)
library(tidyverse)

gt_fun <- function(data, group_col, row_col) {
   group_col <- enquo(group_col)
   row_col <- enquo(row_col)

   gt(data, groupname_col = vars(!!group_col), rowname_col = vars(!!row_col))
}

gt_sample <- gtcars %>%
   filter(str_detect(ctry_origin, "Ger|King")) %>%
   group_by(ctry_origin) %>%
   sample_n(5) %>%
   ungroup()

gt_fun(gt_sample, ctry_origin, model)

gt(gt_sample, vars(ctry_origin), vars(mfr))

