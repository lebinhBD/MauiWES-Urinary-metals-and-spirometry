library(dplyr)
library(gtsummary)

data_metal <- HM_inMaui_clean_des


## descriptive table for lung function by locations ## 

table_gtsummary <- data_metal %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    by = location,
    statistic = list(all_continuous() ~ "{median} (IQR: {p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    missing_text = "(Missing: {n})",# Exclude missing data rows
    digits = list(all_categorical() ~ c(0,2),
                  digits = all_continuous() ~ 2)) %>%
  add_p(test = list(
    all_continuous() ~ "kruskal.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "metal_summary_location.docx")

