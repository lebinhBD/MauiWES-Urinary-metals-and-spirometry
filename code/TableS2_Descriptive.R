library(dplyr)
library(gtsummary)

data_demo <- demo_inMaui_des

#### descriptive table for overall sample ####

table_gtsummary <- data_demo %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    statistic = list(all_continuous() ~ "{median} (IQR: {p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    missing_text = "(Missing: {n})",# Exclude missing data rows
    digits = list(all_categorical() ~ c(0,2),
                  digits = all_continuous() ~ 2)) %>%
  # add_p(test = list(
  #   all_continuous() ~ "t.test",
  #   all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "demo_summary.docx")


#### descriptive table by location ####

table_gtsummary <- data_demo %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    by = location,
    statistic = list(all_continuous() ~ "{median} (IQR: {p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    missing_text = "(Missing: {n})",# Exclude missing data rows
    digits = list(all_categorical() ~ c(0,2),
                  digits = all_continuous() ~ 2)) %>%
  # add_p(test = list(
  #   all_continuous() ~ "t.test",
  #   all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "demo_summary_location.docx")
