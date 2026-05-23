library(dplyr)
library(tidyr)
library(gtsummary)

data_sum <- HM_nhanes_maui_notcorrected_111525 %>% select(
  barium,        
  cadmium,
  cobalt,        
  cesium,
  moly,         
  magnanese,
  lead,         
  antimony,
  tin,           
  strontium,
  thallium,      
  tungsten,
  uranium,
  asenic,
  maui    
) 

table_gtsummary <- data_sum %>%
  tbl_summary(
    by = maui,
    statistic = list(all_continuous() ~ "{median} (IQR: {p25}, {p75})",
                     all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany",
    missing_text = "(Missing: {n})",# Exclude missing data rows
    digits = list(all_categorical() ~ c(0,2),
                  digits = all_continuous() ~ 2)) %>%
  add_p(test = list(
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "metals_nhanes_maui_111525.docx")
