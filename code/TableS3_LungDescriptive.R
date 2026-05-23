library(dplyr)
library(gtsummary)


#### descriptive lung data ####

data_lung <- pulmonary_inMaui_clean

data_lung <- data_lung %>% select(
  participant_id,
  fvc_percent_nhanes,   
  fev1_percent_nhanes,
  fef2575_percent_nhanes,
  fev1_fvcratio,
  fvcbelowlln_N,         
  fev1belowlln_N,
  ffbelowlln_N,         
  fef2575belowlln_N,
  fvc_quality,
  fev1_quality,
  addcity,
  zip
)
dim(data_lung)
View(data_lung)

table(data_lung$zip, data_lung$addcity, useNA = "always")

data_lung <- data_lung %>% mutate(
  zip = case_when(
    is.na(data_lung$zip) & data_lung$addcity == "Lahaina" ~ "96761",
    is.na(data_lung$zip) & data_lung$addcity == "Wailuku" ~ "96793",
    is.na(data_lung$zip) & data_lung$addcity == "Kihei" ~ "96753",
    is.na(data_lung$zip) & data_lung$addcity == "Kula" ~ "96790",
    TRUE ~ zip ))

table(data_lung$zip, useNA = "always")


data_lung <- data_lung %>% filter(!data_lung$zip %in% c(
  "19444",
  "76961",
  "94044",
  "96361",
  "96701",
  "96741",
  "96744",
  "96752",
  "96762",
  "96763",
  "96770",
  "96778",
  "96780",
  "96814",
  "96815",
  "96817",
  "96818",
  "96822",
  "96825",
  "96853",
  "96861",
  "97653",
  "97690",
  "98226",
  "98761" ))
dim(data_lung)

data_lung <- data_lung %>% mutate(
  addcity  = case_when(
    zip == "96708" | zip == "96713" | zip == "96720" | zip == "96779" |zip == "96788" ~ "Kula",
    zip == "96733" ~ "Kahului",
    TRUE ~ addcity
  )
)

table(data_lung$zip, data_lung$addcity, useNA = "always")

data_lung <- data_lung %>% mutate(
  location = case_when(
    addcity == "Lahaina" ~ "Lahaina",
    addcity == "Kula" | addcity == "Other" ~ "Kula",
    addcity == "Kihei" ~ "Kihei",
    addcity == "Kahului" | addcity == "Wailuku" ~ "Wailuku/ Kahului",
    .default = NA
  ))


table(data_lung$location, useNA = "always")
dim(data_lung)
table(data_lung$addcity, useNA = "always")
names(data_lung)
table(data_lung$fev1belowlln_N, useNA = "always")



data_lung <- pulmonary_inMaui_clean %>% select(
  fvcbelowlln_N,        
  fev1belowlln_N,       
  ffbelowlln_N,         
  fef2575belowlln_N,
  location)
table(data_lung$location,useNA = "always")

data_lung <- pulmonary_inMaui_clean
names(data_lung)

#### descriptive table for lung function in overall sample ####

table_gtsummary <- data_lung %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    # by = location,
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
  gt::gtsave(filename = "lung_summary.docx")


#### descriptive table for lung function by locations ####

table_gtsummary <- data_lung %>%
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
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "lung_summary_location.docx")


#### session quality A-C for FVC ####
table(data_lung$f)

data_fvc <- pulmonary_inMaui_clean %>% 
  filter (fvc_quality %in% c("A", "B", "C"))

data_fvc <- data_fvc %>% select(
  fvcbelowlln_N,         
  location
)


table_gtsummary <- data_fvc %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    # by = location,
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
  gt::gtsave(filename = "lung_summary_fvc.docx")


## descriptive table for lung function by locations ##

table_gtsummary <- data_fvc %>%
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
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "lung_summary_fvc_location.docx")


#### session quality A-C for FEV1 ####
names(pulmonary_inMaui_clean)

data_fev1 <- pulmonary_inMaui_clean %>% 
  filter (fev1_quality %in% c("A", "B", "C"))

data_fev1 <- data_fev1 %>% select(
  fev1belowlln_N,         
  location
)


table_gtsummary <- data_fev1 %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    # by = location,
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
  gt::gtsave(filename = "lung_summary_fev.docx")


## descriptive table for lung function by locations ## 

table_gtsummary <- data_fev1 %>%
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
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "lung_summary_fev_location.docx")


#### session quality A-C for FVC and FEV1 ####
names(pulmonary_inMaui_clean)

data_ff <- pulmonary_inMaui_clean %>% 
  filter (fev1_quality %in% c("A", "B", "C") & 
            fvc_quality %in% c("A", "B", "C") )

data_ff <- data_ff %>% select(
  ffbelowlln_N,          
fef2575belowlln_N,         
  location
)

dim(data_ff)


table_gtsummary <- data_ff %>%
  tbl_summary(
    type = all_categorical() ~ "categorical",
    # by = location,
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
  gt::gtsave(filename = "lung_summary_ff.docx")


## descriptive table for lung function by locations ## 

table_gtsummary <- data_ff %>%
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
    all_continuous() ~ "t.test",
    all_categorical() ~ "chisq.test"))%>%
  as_gt() %>%
  gt::gtsave(filename = "lung_summary_ff_location.docx")





