library(dplyr)
library(tidyr)
library(gWQS)
library(ggplot2)
library(forestplot)
library(qgcomp)

#### Standard WQS model by regions ####

#### LAHAINA + KULA ####
data <- HM_lung_demo_nomiss_111525_use
dim(data)
table(data$location, useNA = "always")
data_laku <- data %>% filter(data$location %in% c("Lahaina", "Kula"))
dim(data_laku)

data_fvc <- data_laku %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  sb_ln,
  v_ln,
  ni_ln) %>% filter(fvc_quality %in% c("A", "B", "C")) %>% rename(
    "age" = "age_new", 
    "male" = "male_new" 
  )

dim(data_fvc)  


set.seed(234)

metals <- c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "sb_ln",
  "v_ln",
  "ni_ln"
)

#### FVC < LLN in WQS model ####

wqs_fvc <- gwqs(
  formula = fvcbelowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fvc,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Poisson
  
)
summary(wqs_fvc)

coef_table <- summary(wqs_fvc)$coefficients

# Convert to data.frame
df_fvc <- as.data.frame(coef_table)
df_fvc$term <- rownames(coef_table)

# Add ORs and 95% CI
df_fvc  <- df_fvc  %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fvc <- df_fvc[-c(1),]

df_fvc$term

df_fvc$term[1] <- "WQS"
df_fvc$term[2] <- "Age"
df_fvc$term[3] <- "Sex (=1 male)"
df_fvc$term[4] <- "BMI score"
df_fvc$term[5] <- "Smoking status"

View(df_fvc)


df_fvc$term <- ifelse(df_fvc$p <= 0.001,
                      paste0(df_fvc$term, "***"),
                      ifelse(df_fvc$p > 0.001 & df_fvc$p <= 0.01,
                             paste0(df_fvc$term, "**"),
                             ifelse(df_fvc$p > 0.01 & df_fvc$p <= 0.05,
                                    paste0(df_fvc$term, "*"),
                                    ifelse(df_fvc$p > 0.05 & df_fvc$p <= 0.1,
                                           paste0(df_fvc$term, "+"),
                                           paste0(df_fvc$term)))))


colnames(df_fvc)
df_fvc


#### FEV1 < LLN in WQS model ####

data_fev1 <- data_laku %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  sb_ln,
  v_ln,
  ni_ln) %>% filter(fev1_quality %in% c("A", "B", "C")) %>% rename(
    "age" = "age_new", 
    "male" = "male_new" 
  )


dim(data_fev1) 
set.seed(123)

wqs_fev1 <- gwqs(
  formula =  fev1belowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fev1,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Poisson
  
)
summary(wqs_fev1)

coef_table <- summary(wqs_fev1)$coefficients

# Convert to data.frame
df_fev1 <- as.data.frame(coef_table)
df_fev1$term <- rownames(coef_table)

# Add ORs and 95% CI
df_fev1  <- df_fev1 %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev1 <- df_fev1[-1,]

df_fev1$term

df_fev1$term[1] <- "WQS"
df_fev1$term[2] <- "Age"
df_fev1$term[3] <- "Sex (=1 male)"
df_fev1$term[4] <- "BMI score"
df_fev1$term[5] <- "Smoking status"


df_fev1$term <- ifelse(df_fev1$p <= 0.001,
                       paste0(df_fev1$term, "***"),
                       ifelse(df_fev1$p > 0.001 & df_fev1$p <= 0.01,
                              paste0(df_fev1$term, "**"),
                              ifelse(df_fev1$p > 0.01 & df_fev1$p <= 0.05,
                                     paste0(df_fev1$term, "*"),
                                     ifelse(df_fev1$p > 0.05 & df_fev1$p <= 0.1,
                                            paste0(df_fev1$term, "+"),
                                            paste0(df_fev1$term)))))


colnames(df_fev1)
df_fev1


#### FEV1/FVC < LLN in WQS model ####

data_ff <- data_laku %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  sb_ln,
  v_ln,
  ni_ln) %>% filter(fvc_quality %in% c("A", "B", "C") &
                      fev1_quality %in% c("A", "B", "C")) %>% rename(
                        "age" ="age_new", 
                        "male" = "male_new"  )

dim(data_ff)  


wqs_ff <- gwqs(
  formula = ffbelowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_ff,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Gaussian
  
)
summary(wqs_ff)
coef_table <- summary(wqs_ff)$coefficients

# Convert to data.frame
df_ff <- as.data.frame(coef_table)
df_ff$term <- rownames(coef_table)
# View(df_ff)

# Add ORs and 95% CI
df_ff  <- df_ff %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)
df_ff$term
df_ff <- df_ff[-1,]


df_ff$term

df_ff$term[1] <- "WQS"
df_ff$term[2] <- "Age"
df_ff$term[3] <- "Sex (=1 male)"
df_ff$term[4] <- "BMI score"
df_ff$term[5] <- "Smoking status"


df_ff$term <- ifelse(df_ff$p <= 0.001,
                     paste0(df_ff$term, "***"),
                     ifelse(df_ff$p > 0.001 & df_ff$p <= 0.01,
                            paste0(df_ff$term, "**"),
                            ifelse(df_ff$p > 0.01 & df_ff$p <= 0.05,
                                   paste0(df_ff$term, "*"),
                                   ifelse(df_ff$p > 0.05 & df_ff$p <= 0.1,
                                          paste0(df_ff$term, "+"),
                                          paste0(df_ff$term)))))


colnames(df_ff)
df_ff


#### FEF2575 < LLN in WQS model ####

data_fef <- data_laku %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  sb_ln,
  v_ln,
  ni_ln) %>% filter(fvc_quality %in% c("A", "B", "C") &
                      fev1_quality %in% c("A", "B", "C")) %>% rename(
                        "age" ="age_new", 
                        "male" = "male_new"  )


dim(data_fef)  

set.seed(234)

wqs_fef <- gwqs(
  formula = fef2575belowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fef,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Gaussian
  
)
summary(wqs_fef)
coef_table <- summary(wqs_fef)$coefficients

# Convert to data.frame
df_fef <- as.data.frame(coef_table)
df_fef$term <- rownames(coef_table)
View(df_fef)

# Add ORs and 95% CI
df_fef  <- df_fef %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)
df_fef$term
df_fef <- df_fef[-1,]

df_fef$term

df_fef$term[1] <- "WQS"
df_fef$term[2] <- "Age"
df_fef$term[3] <- "Sex (=1 male)"
df_fef$term[4] <- "BMI score"
df_fef$term[5] <- "Smoking status"


df_fef$term <- ifelse(df_fef$p <= 0.001,
                     paste0(df_fef$term, "***"),
                     ifelse(df_fef$p > 0.001 & df_fef$p <= 0.01,
                            paste0(df_fef$term, "**"),
                            ifelse(df_fef$p > 0.01 & df_fef$p <= 0.05,
                                   paste0(df_fef$term, "*"),
                                   ifelse(df_fef$p > 0.05 & df_fef$p <= 0.1,
                                          paste0(df_fef$term, "+"),
                                          paste0(df_fef$term)))))


colnames(df_fef)
df_fef


df_laku <- bind_rows(df_fvc,
                     df_fev1,
                     df_ff,
                     df_fef)
View(df_laku)
write.csv(df_laku, file = "df_laku.csv")



#### OTHER LOCATIONS in WQS model ####
data <- HM_lung_demo_nomiss_111525_use
dim(data)
names(data)
table(data$location, useNA = "always")
data_other <- data %>% filter(!data$location %in% c("Lahaina", "Kula"))
dim(data_other)

data_fvc <- data_other %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln, # yes
  cd_ln, # yes
  cu_ln, # yes
  fe_ln, # yes
  se_ln, # yes
  w_ln, # yes
  tl_ln) %>% filter(fvc_quality %in% c("A", "B", "C")) %>% rename(
    "age" = "age_new", 
    "male"  = "male_new" 
  )

dim(data_fvc)  


set.seed(234)

metals <- c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "fe_ln",
  "se_ln",
  "w_ln",
  "tl_ln"
)

#### FVC < LLN in WQS ####

wqs_fvc <- gwqs(
  formula = fvcbelowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fvc,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Poisson
  
)
summary(wqs_fvc)

coef_table <- summary(wqs_fvc)$coefficients

# Convert to data.frame
df_fvc <- as.data.frame(coef_table)
df_fvc$term <- rownames(coef_table)

# Add ORs and 95% CI
df_fvc  <- df_fvc  %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fvc <- df_fvc[-1,]

df_fvc$term

df_fvc$term[1] <- "WQS"
df_fvc$term[2] <- "Age"
df_fvc$term[3] <- "Sex (=1 male)"
df_fvc$term[4] <- "BMI score"
df_fvc$term[5] <- "Smoking status"


df_fvc$term <- ifelse(df_fvc$p <= 0.001,
                      paste0(df_fvc$term, "***"),
                      ifelse(df_fvc$p > 0.001 & df_fvc$p <= 0.01,
                             paste0(df_fvc$term, "**"),
                             ifelse(df_fvc$p > 0.01 & df_fvc$p <= 0.05,
                                    paste0(df_fvc$term, "*"),
                                    ifelse(df_fvc$p > 0.05 & df_fvc$p <= 0.1,
                                           paste0(df_fvc$term, "+"),
                                           paste0(df_fvc$term)))))


colnames(df_fvc)
df_fvc

#### FEV1 < LLN in WQS ####

data_fev1 <- data_other %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  fe_ln,
  se_ln,
  w_ln,
  tl_ln) %>% filter(fev1_quality %in% c("A", "B", "C"))%>% rename(
    "age" = "age_new", 
    "male" = "male_new" 
  )

dim(data_fev1)  

set.seed(123)

wqs_fev1 <- gwqs(
  formula =  fev1belowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fev1,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Poisson
  
)
summary(wqs_fev1)

coef_table <- summary(wqs_fev1)$coefficients

# Convert to data.frame
df_fev1 <- as.data.frame(coef_table)
df_fev1$term <- rownames(coef_table)

# Add ORs and 95% CI
df_fev1  <- df_fev1 %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev1 <- df_fev1[-1,]

df_fev1$term

df_fev1$term[1] <- "WQS"
df_fev1$term[2] <- "Age"
df_fev1$term[3] <- "Sex (=1 male)"
df_fev1$term[4] <- "BMI score"
df_fev1$term[5] <- "Smoking status"


df_fev1$term <- ifelse(df_fev1$p <= 0.001,
                       paste0(df_fev1$term, "***"),
                       ifelse(df_fev1$p > 0.001 & df_fev1$p <= 0.01,
                              paste0(df_fev1$term, "**"),
                              ifelse(df_fev1$p > 0.01 & df_fev1$p <= 0.05,
                                     paste0(df_fev1$term, "*"),
                                     ifelse(df_fev1$p > 0.05 & df_fev1$p <= 0.1,
                                            paste0(df_fev1$term, "+"),
                                            paste0(df_fev1$term)))))


colnames(df_fev1)
df_fev1


#### FEV1/FVC < LLN in WQS model ####

data_ff <- data_other %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  fe_ln,
  se_ln,
  w_ln,
  tl_ln) %>% filter(fvc_quality %in% c("A", "B", "C") &
                      fev1_quality %in% c("A", "B", "C")) %>% rename(
                        "age" =   "age_new", 
                        "male" = "male_new" )


dim(data_ff)  

set.seed(123)

wqs_ff <- gwqs(
  formula = ffbelowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_ff,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Gaussian
)

summary(wqs_ff)
coef_table <- summary(wqs_ff)$coefficients

# Convert to data.frame
df_ff <- as.data.frame(coef_table)
df_ff$term <- rownames(coef_table)

# Add ORs and 95% CI
df_ff  <- df_ff %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)
df_ff$term
df_ff <- df_ff[-1,]


df_ff$term

df_ff$term[1] <- "WQS"
df_ff$term[2] <- "Age"
df_ff$term[3] <- "Sex (=1 male)"
df_ff$term[4] <- "BMI score"
df_ff$term[5] <- "Smoking status"


df_ff$term <- ifelse(df_ff$p <= 0.001,
                     paste0(df_ff$term, "***"),
                     ifelse(df_ff$p > 0.001 & df_ff$p <= 0.01,
                            paste0(df_ff$term, "**"),
                            ifelse(df_ff$p > 0.01 & df_ff$p <= 0.05,
                                   paste0(df_ff$term, "*"),
                                   ifelse(df_ff$p > 0.05 & df_ff$p <= 0.1,
                                          paste0(df_ff$term, "+"),
                                          paste0(df_ff$term)))))


colnames(df_ff)
df_ff


#### FEF2575 < LLN in WQS model ####

data_fef <- data_other %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  BMIscore,
  tobaco,  
  age_new, 
  male_new,
  fvc_quality,
  fev1_quality,
  as_ln,
  cd_ln,
  cu_ln,
  fe_ln,
  se_ln,
  w_ln,
  tl_ln) %>% filter(fvc_quality %in% c("A", "B", "C") &
                      fev1_quality %in% c("A", "B", "C")) %>% rename(
                        "age" =    "age_new", 
                        "male" =  "male_new")

dim(data_fef)  

wqs_fef <- gwqs(
  formula = fef2575belowlln_N ~ wqs + age + male + BMIscore + tobaco,
  mix_name = metals,
  data = data_fef,
  q = 4,                # quartiles
  validation = 0.6,     # 40% training / 60% validation
  b = 1000,          # bootstrap samples (increase to ≥1000 for real analysis)
  family = binomial() ## estimate Gaussian
  
)
summary(wqs_fef)
coef_table <- summary(wqs_fef)$coefficients

# Convert to data.frame
df_fef <- as.data.frame(coef_table)
df_fef$term <- rownames(coef_table)
View(df_fef)

# Add ORs and 95% CI
df_fef  <- df_fef %>%
  mutate(
    OR = exp(Estimate),
    CI_low = exp(Estimate - 1.96 * `Std. Error`),
    CI_high = exp(Estimate + 1.96 * `Std. Error`),
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(`Pr(>|z|)`, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)
df_fef$term
df_fef <- df_fef[-1,]


df_fef$term

df_fef$term[1] <- "WQS"
df_fef$term[2] <- "Age"
df_fef$term[3] <- "Sex (=1 male)"
df_fef$term[4] <- "BMI score"
df_fef$term[5] <- "Smoking status"


df_fef$term <- ifelse(df_fef$p <= 0.001,
                     paste0(df_fef$term, "***"),
                     ifelse(df_fef$p > 0.001 & df_fef$p <= 0.01,
                            paste0(df_fef$term, "**"),
                            ifelse(df_fef$p > 0.01 & df_fef$p <= 0.05,
                                   paste0(df_fef$term, "*"),
                                   ifelse(df_fef$p > 0.05 & df_fef$p <= 0.1,
                                          paste0(df_fef$term, "+"),
                                          paste0(df_fef$term)))))


colnames(df_fef)
df_fef

df_other <- bind_rows(df_fvc,
                      df_fev1,
                      df_ff,
                      df_fef)
write.csv(df_other, file = "wqs_otherlocation.csv")


#### Q-gcomputation model by regions ####


data <- HM_lung_demo_nomiss_111525_use
dim(data)
names(data)
data <- data %>% rename(
  "age" = "age_new",               
  "male" = "male_new"  
)
table(data$location, useNA = "always")
data_laku <- subset(data, data$location =="Kula" | data$location =="Lahaina")
dim(data_laku)
data_fvc <- subset(data_laku, data_laku$fvc_quality %in% c("A", "B", "C"))
data_fev1 <- subset(data_laku, data_laku$fev1_quality %in% c("A", "B", "C"))
data_ff <- subset(data_laku, data_laku$fev1_quality %in% c("A", "B", "C")& data_laku$fvc_quality %in% c("A", "B", "C"))
data_fef <- subset(data_laku, data_laku$fev1_quality %in% c("A", "B", "C")& data_laku$fvc_quality %in% c("A", "B", "C"))
dim(data_fvc)
dim(data_fev1)
dim(data_fef)
dim(data_ff)

#### LAHAINA + KULA in Q-gcomp model ####

#### FVC < LLN in Q-gcomp ####
fit_fvc <- qgcomp.glm.noboot(
  f = fvcbelowlln_N ~  as_ln + cd_ln + cu_ln + sb_ln + v_ln + ni_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "sb_ln",
             "v_ln",
             "ni_ln"),
  data = data_fvc,
  q = 4,
  family = binomial()
)
summary(fit_fvc)


coefs <- summary(fit_fvc)$coefficients

# Create a results table
data_fvc_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

data_fvc_qcomp <- data_fvc_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fvc_qcomp


data_fvc_qcomp <- data_fvc_qcomp[-1,]

summary(fit_fvc$fit)

tab_fvc <- tidy(fit_fvc$fit)
tab_fvc$OR  <- exp(tab_fvc$estimate)
tab_fvc$CI_low  <- exp(tab_fvc$estimate - 1.96*tab_fvc$std.error)
tab_fvc$CI_high <- exp(tab_fvc$estimate + 1.96*tab_fvc$std.error)

class(tab_fvc)
data_fvc_cov <- as.data.frame(tab_fvc)
names(data_fvc_cov)

data_fvc_cov %>% select(term, OR, CI_low, CI_high, p.value)

data_fvc_cov <- data_fvc_cov[-c(1:7),]
data_fvc_cov

#### fvc weights ####

pos_df <- data.frame(
  term = names(fit_fvc$pos.weights),
  weight = fit_fvc$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fvc$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fvc$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df <- rbind(pos_df, neg_df)
# View(all_weights_df)
colnames(all_weights_df)

all_weights_df <- all_weights_df %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ni_ln" ~ "Nickel",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "v_ln" ~ "Vanadium"))



# 4. draw a graph

custom_order <- c("Vanadium", "Nickel", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df$term <- factor(all_weights_df$term,
                              levels = custom_order)
# View(all_weights_df)

p_fvc_qcomp <- ggplot(all_weights_df,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FVC < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fvc_qcomp


#### FEV1 < LLN in Q-gcomp model ####
fit_fev <- qgcomp.glm.noboot(
  f = fev1belowlln_N ~  as_ln + cd_ln + cu_ln + sb_ln + v_ln + ni_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "sb_ln",
             "v_ln",
             "ni_ln"),
  data = data_fev1,
  q = 4,
  family = binomial()
)
summary(fit_fev)


coefs <- summary(fit_fev)$coefficients

# Create a results table
df_fev_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_fev_qcomp <- df_fev_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_qcomp


df_fev_qcomp <- df_fev_qcomp[-1,]

summary(fit_fev$fit)

tab_fev <- tidy(fit_fev$fit)
tab_fev$OR  <- exp(tab_fev$estimate)
tab_fev$CI_low  <- exp(tab_fev$estimate - 1.96*tab_fev$std.error)
tab_fev$CI_high <- exp(tab_fev$estimate + 1.96*tab_fev$std.error)

class(tab_fev)
df_fev_cov <- as.data.frame(tab_fev)
names(df_fev_cov)

df_fev_cov <- df_fev_cov %>%  select(term, OR, CI_low, CI_high, p.value)

df_fev_cov  <- df_fev_cov [-c(1:7),]
df_fev_cov 


#### fev1 weights ####

pos_df <- data.frame(
  term = names(fit_fev$pos.weights),
  weight = fit_fev$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fev$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fev$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_fev <- rbind(pos_df, neg_df)
View(all_weights_df_fev)
colnames(all_weights_df_fev)

all_weights_df_fev <- all_weights_df_fev %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ni_ln" ~ "Nickel",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "v_ln" ~ "Vanadium"))



# 4. draw a graph

custom_order <- c("Vanadium", "Nickel", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df_fev$term <- factor(all_weights_df_fev$term,
                                  levels = custom_order)
# View(all_weights_df_fev)

p_fev_qcomp <- ggplot(all_weights_df_fev,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEV1 < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fev_qcomp 



#### FEV1/FVC < LLN in Q-gcomp model ####
fit_ff <- qgcomp.glm.noboot(
  f = ffbelowlln_N ~  as_ln + cd_ln + cu_ln + sb_ln + v_ln + ni_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "sb_ln",
             "v_ln",
             "ni_ln"),
  data = data_ff,
  q = 4,
  family = binomial()
)
summary(fit_ff)


coefs <- summary(fit_ff)$coefficients

# Create a results table
df_ff_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_ff_qcomp <- df_ff_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_ff_qcomp


df_ff_qcomp <- df_ff_qcomp[-1,]

summary(fit_ff$fit)

tab_ff <- tidy(fit_ff$fit)
tab_ff$OR  <- exp(tab_ff$estimate)
tab_ff$CI_low  <- exp(tab_ff$estimate - 1.96*tab_ff$std.error)
tab_ff$CI_high <- exp(tab_ff$estimate + 1.96*tab_ff$std.error)

class(tab_ff)
df_ff_cov <- as.data.frame(tab_ff)
names(df_ff_cov)

df_ff_cov <- df_ff_cov %>%  select(term, OR, CI_low, CI_high, p.value)

df_ff_cov  <- df_ff_cov [-c(1:7),]
df_ff_cov 

#### ff weights ####
summary(fit_ff)

pos_df <- data.frame(
  term = names(fit_ff$pos.weights),
  weight = fit_ff$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_ff$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_ff$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_ff <- rbind(pos_df, neg_df)
# View(all_weights_df_ff)
colnames(all_weights_df_ff)

all_weights_df_ff <- all_weights_df_ff %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ni_ln" ~ "Nickel",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "v_ln" ~ "Vanadium"))



# 4. draw a graph

custom_order <- c("Vanadium", "Nickel", "Copper", "Cadmium", "Antimony", "Arsenic")


all_weights_df_ff$term <- factor(all_weights_df_ff$term,
                                 levels = custom_order)
# View(all_weights_df_ff)

p_ff_qcomp <- ggplot(all_weights_df_ff,
                     aes(x = term,
                         y = weight,
                         fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEV1/FVC < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_ff_qcomp 




#### FEF25-75 < LLN in Q-gcomp model ####
fit_fef <- qgcomp.glm.noboot(
  f = fef2575belowlln_N ~  as_ln + cd_ln + cu_ln + sb_ln + v_ln + ni_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "sb_ln",
             "v_ln",
             "ni_ln"),
  data = data_fef,
  q = 4,
  family = binomial()
)
summary(fit_fef)


coefs <- summary(fit_fef)$coefficients

# Create a results table
df_fef_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_fef_qcomp <- df_fef_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fef_qcomp


df_fef_qcomp <- df_fef_qcomp[-1,]

summary(fit_fef$fit)

tab_fef <- tidy(fit_fef$fit)
tab_fef$OR  <- exp(tab_fef$estimate)
tab_fef$CI_low  <- exp(tab_fef$estimate - 1.96*tab_fef$std.error)
tab_fef$CI_high <- exp(tab_fef$estimate + 1.96*tab_fef$std.error)

class(tab_fef)
df_fef_cov <- as.data.frame(tab_fef)
names(df_fef_cov)

df_fef_cov <- df_fef_cov %>%  select(term, OR, CI_low, CI_high, p.value)

df_fef_cov  <- df_fef_cov [-c(1:7),]
df_fef_cov

df_comp_mix <- bind_rows(data_fvc_qcomp,
                         df_fev_qcomp,
                         df_ff_qcomp,
                         df_fef_qcomp)
write.csv(df_comp_mix, file = "qcom_Lahaina.csv")

df_comp <- bind_rows(data_fvc_cov,
                     df_fev_cov,
                     df_fev_cov,
                     df_fef_cov)
write.csv(df_comp, file = "qcom_cov_Lahaina.csv")

#### fef weights ####
summary(fit_fef)

pos_df <- data.frame(
  term = names(fit_fef$pos.weights),
  weight = fit_fef$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fef$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fef$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_fef <- rbind(pos_df, neg_df)
# View(all_weights_df_fef)
colnames(all_weights_df_fef)

all_weights_df_fef <- all_weights_df_fef %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ni_ln" ~ "Nickel",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "v_ln" ~ "Vanadium"))



# 4. draw a graph

custom_order <- c("Vanadium", "Nickel", "Copper", "Cadmium", "Antimony", "Arsenic")


all_weights_df_fef$term <- factor(all_weights_df_fef$term,
                                  levels = custom_order)
# View(all_weights_df_fef)

p_fef_qcomp <- ggplot(all_weights_df_fef,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEF2575 < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.2, 1.2),
            size = 4)
p_fef_qcomp 

#### combine weights graph ####
p_fvc_qcomp 
p_fev_qcomp 
p_ff_qcomp 
p_fef_qcomp 
library(patchwork)
p_weight_qcomp <- (p_fvc_qcomp|p_fev_qcomp)/ (p_ff_qcomp |p_fef_qcomp )

ggsave("FigS10_qcomp_weight_Laku.png",
       plot = p_weight_qcomp,
       width = 16,
       height = 14,
       dpi = 300)

#### OTHER LOCATIONS in Q-gComp model ####

data <- HM_lung_demo_nomiss_111525_use
dim(data)
names(data)
data <- data %>% rename(
  "age" = "age_new",               
  "male" = "male_new"  
)
table(data$location, useNA = "always")
data_other <- subset(data, data$location %in% c("Kihei", "Wailuku/ Kahului"))
dim(data_other)

data_fvc <- subset(data_other, data_other$fvc_quality %in% c("A", "B", "C"))
data_fev1 <- subset(data_other, data_other$fev1_quality %in% c("A", "B", "C"))
data_ff <- subset(data_other, data_other$fev1_quality %in% c("A", "B", "C")& data_other$fvc_quality %in% c("A", "B", "C"))
data_fef <- subset(data_other, data_other$fev1_quality %in% c("A", "B", "C")& data_other$fvc_quality %in% c("A", "B", "C"))
dim(data_fvc)
dim(data_fev1)
dim(data_fef)
dim(data_ff)


#### FVC < LLN in Q-gComp model ####
fit_fvc <- qgcomp.glm.noboot(
  f = fvcbelowlln_N ~  as_ln + cd_ln + cu_ln + fe_ln + se_ln + w_ln +tl_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "fe_ln",
             "se_ln",
             "w_ln",
             "tl_ln"),
  data = data_fvc,
  q = 4,
  family = binomial()
)
summary(fit_fvc)


coefs <- summary(fit_fvc)$coefficients

# Create a results table
data_fvc_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

data_fvc_qcomp <- data_fvc_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fvc_qcomp


data_fvc_qcomp <- data_fvc_qcomp[-1,]

summary(fit_fvc$fit)

tab_fvc <- tidy(fit_fvc$fit)
tab_fvc$OR  <- exp(tab_fvc$estimate)
tab_fvc$CI_low  <- exp(tab_fvc$estimate - 1.96*tab_fvc$std.error)
tab_fvc$CI_high <- exp(tab_fvc$estimate + 1.96*tab_fvc$std.error)

class(tab_fvc)
data_fvc_cov <- as.data.frame(tab_fvc)
names(data_fvc_cov)

data_fvc_cov %>% select(term,
                        OR, 
                        CI_low, 
                        CI_high, 
                        p.value)

data_fvc_cov <- data_fvc_cov[-c(1:7),]
data_fvc_cov

#### fvc weights ####

pos_df <- data.frame(
  term = names(fit_fvc$pos.weights),
  weight = fit_fvc$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fvc$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fvc$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df <- rbind(pos_df, neg_df)
# View(all_weights_df)
colnames(all_weights_df)

all_weights_df <- all_weights_df %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "fe_ln" ~ "Iron",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "se_ln" ~ "Selenium",
                   term == "w_ln" ~ "Tungsten",
                   term == "tl_ln" ~ "Thallium"))



# 4. draw a graph

custom_order <- c("Iron", "Tungsten", "Selenium", "Thallium", "Cadmium", "Copper", "Arsenic")

all_weights_df$term <- factor(all_weights_df$term,
                              levels = custom_order)
# View(all_weights_df)

p_fvc_qcomp <- ggplot(all_weights_df,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FVC < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fvc_qcomp

#### FEV1 < LLN in Q-gComp model ####
fit_fev <- qgcomp.glm.noboot(
  f = fev1belowlln_N ~  as_ln + cd_ln + cu_ln + fe_ln + se_ln + w_ln + tl_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "fe_ln",
             "se_ln",
             "w_ln",
             "tl_ln"),
  data = data_fev1,
  q = 4,
  family = binomial()
)
summary(fit_fev)


coefs <- summary(fit_fev)$coefficients

# Create a results table
df_fev_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_fev_qcomp <- df_fev_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_qcomp


df_fev_qcomp <- df_fev_qcomp[-1,]

summary(fit_fev$fit)

tab_fev <- tidy(fit_fev$fit)
tab_fev$OR  <- exp(tab_fev$estimate)
tab_fev$CI_low  <- exp(tab_fev$estimate - 1.96*tab_fev$std.error)
tab_fev$CI_high <- exp(tab_fev$estimate + 1.96*tab_fev$std.error)

class(tab_fev)
df_fev_cov <- as.data.frame(tab_fev)
names(df_fev_cov)

df_fev_cov <- df_fev_cov %>%  select(term, 
                                     OR, 
                                     CI_low,
                                     CI_high, 
                                     p.value)

df_fev_cov  <- df_fev_cov [-c(1:7),]
df_fev_cov 


#### fev1 weights ####

pos_df <- data.frame(
  term = names(fit_fev$pos.weights),
  weight = fit_fev$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fev$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fev$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_fev <- rbind(pos_df, neg_df)
# View(all_weights_df_fev)
colnames(all_weights_df_fev)

all_weights_df_fev <- all_weights_df_fev %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "fe_ln" ~ "Iron",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "se_ln" ~ "Selenium",
                   term == "w_ln" ~ "Tungsten",
                   term == "tl_ln" ~ "Thallium"))



# 4. draw a graph

custom_order <- c("Iron", "Tungsten", "Selenium", "Thallium", "Cadmium", "Copper", "Arsenic")

all_weights_df_fev$term <- factor(all_weights_df_fev$term,
                                  levels = custom_order)
# View(all_weights_df_fev)

p_fev_qcomp <- ggplot(all_weights_df_fev,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEV1 < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fev_qcomp 

#### FEV1/FVC < LLN in Q-gComp model ####
fit_ff <- qgcomp.glm.noboot(
  f = ffbelowlln_N ~  as_ln + cd_ln + cu_ln + fe_ln + se_ln + w_ln + tl_ln +  age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "fe_ln",
             "se_ln",
             "w_ln",
             "tl_ln"),
  data = data_ff,
  q = 4,
  family = binomial()
)
summary(fit_ff)


coefs <- summary(fit_ff)$coefficients

# Create a results table
df_ff_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_ff_qcomp <- df_ff_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_ff_qcomp


df_ff_qcomp <- df_ff_qcomp[-1,]

summary(fit_ff$fit)

tab_ff <- tidy(fit_ff$fit)
tab_ff$OR  <- exp(tab_ff$estimate)
tab_ff$CI_low  <- exp(tab_ff$estimate - 1.96*tab_ff$std.error)
tab_ff$CI_high <- exp(tab_ff$estimate + 1.96*tab_ff$std.error)

class(tab_ff)
df_ff_cov <- as.data.frame(tab_ff)
names(df_ff_cov)

df_ff_cov <- df_ff_cov %>%  select(term, OR, CI_low, CI_high, p.value)

df_ff_cov  <- df_ff_cov [-c(1:7),]
df_ff_cov 

#### ff weights ####
summary(fit_ff)

pos_df <- data.frame(
  term = names(fit_ff$pos.weights),
  weight = fit_ff$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_ff$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_ff$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_ff <- rbind(pos_df, neg_df)
# View(all_weights_df_ff)
colnames(all_weights_df_ff)

all_weights_df_ff <- all_weights_df_ff %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "fe_ln" ~ "Iron",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "se_ln" ~ "Selenium",
                   term == "w_ln" ~ "Tungsten",
                   term == "tl_ln" ~ "Thallium"))



# 4. draw a graph

custom_order <- c("Iron", "Tungsten", "Selenium", "Thallium", "Cadmium", "Copper", "Arsenic")

all_weights_df_ff$term <- factor(all_weights_df_ff$term,
                                 levels = custom_order)
# View(all_weights_df_ff)

p_ff_qcomp <- ggplot(all_weights_df_ff,
                     aes(x = term,
                         y = weight,
                         fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEV1/FVC < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_ff_qcomp 

#### FEF2575 < LLN in Q-gComp model ####
fit_fef <- qgcomp.glm.noboot(
  f = fef2575belowlln_N ~  as_ln + cd_ln + cu_ln + fe_ln + se_ln + w_ln + tl_ln +  age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "cd_ln",
             "cu_ln",
             "fe_ln",
             "se_ln",
             "w_ln",
             "tl_ln"),
  data = data_fef,
  q = 4,
  family = binomial()
)
summary(fit_fef)


coefs <- summary(fit_fef)$coefficients

# Create a results table
df_fef_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

df_fef_qcomp <- df_fef_qcomp %>%
  mutate(
    OR = round(OR, 3),
    CI_low = round(CI_low, 3),
    CI_high = round(CI_high, 3),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fef_qcomp


df_fef_qcomp <- df_fef_qcomp[-1,]

summary(fit_fef$fit)

tab_fef <- tidy(fit_fef$fit)
tab_fef$OR  <- exp(tab_fef$estimate)
tab_fef$CI_low  <- exp(tab_fef$estimate - 1.96*tab_fef$std.error)
tab_fef$CI_high <- exp(tab_fef$estimate + 1.96*tab_fef$std.error)

class(tab_fef)
df_fef_cov <- as.data.frame(tab_fef)
names(df_fef_cov)

df_fef_cov <- df_fef_cov %>%  select(term, OR, CI_low, CI_high, p.value)

df_fef_cov  <- df_fef_cov [-c(1:7),]
df_fef_cov

df_comp_mix <- bind_rows(data_fvc_qcomp,
                         df_fev_qcomp,
                         df_ff_qcomp,
                         df_fef_qcomp)
write.csv(df_comp_mix, file = "qcom_other.csv")

df_comp <- bind_rows(data_fvc_cov,
                     df_fev_cov,
                     df_fev_cov,
                     df_fef_cov)
write.csv(df_comp, file = "qcom_cov_other.csv")


#### fef weights ####
summary(fit_fef)

pos_df <- data.frame(
  term = names(fit_fef$pos.weights),
  weight = fit_fef$pos.weights,
  direction = "Positive")
# Remove the row name that is automatically created
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

# 2. Access the negative weights and convert the named vector to a data frame
neg_df <- data.frame(
  term = names(fit_fef$neg.weights),
  # Weights are presented as absolute values in the output, so re-apply the negative sign
  weight = -fit_fef$neg.weights, 
  direction = "Negative"
)
# Remove the row name that is automatically created
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL


# 3. Combine the positive and negative weights
all_weights_df_fef <- rbind(pos_df, neg_df)
# View(all_weights_df_fef)
colnames(all_weights_df_fef)

all_weights_df_fef <- all_weights_df_fef %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "fe_ln" ~ "Iron",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "se_ln" ~ "Selenium",
                   term == "w_ln" ~ "Tungsten",
                   term == "tl_ln" ~ "Thallium"))

# 4. draw a graph

custom_order <- c("Iron", "Tungsten", "Selenium", "Thallium", "Cadmium", "Copper", "Arsenic")

all_weights_df_fef$term <- factor(all_weights_df_fef$term,
                                  levels = custom_order)
# View(all_weights_df_fef)

p_fef_qcomp <- ggplot(all_weights_df_fef,
                      aes(x = term,
                          y = weight,
                          fill = direction)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#088F8F",
                               "Negative" = "#CC5500")) +
  
  scale_y_continuous(
    limits = c(-1, 1), 
    breaks = seq(-1, 1, by = 0.25),
    labels = c("1" , "0.75", "0.50", "0.25", "0.00", 
               "0.25", "0.50", "0.75", "1")
  ) +
  labs(
    x = "Metal",
    y = "Negative weights            Positive weights",
    title = "FEF2575 < LLN"
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.2, 1.2),
            size = 4)
p_fef_qcomp 

#### combine weights graph ####
p_fvc_qcomp 
p_fev_qcomp 
p_ff_qcomp 
p_fef_qcomp 
library(patchwork)
p_weight_qcomp <- (p_fvc_qcomp|p_fev_qcomp)/ (p_ff_qcomp |p_fef_qcomp )


ggsave("FigS11_qcomp_weight_other.png",
       plot = p_weight_qcomp,
       width = 16,
       height = 14,
       dpi = 300)

