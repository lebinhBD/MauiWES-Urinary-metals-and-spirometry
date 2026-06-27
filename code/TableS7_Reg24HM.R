library(dplyr)
library(survey)
library(broom)

data <- HM_lung_demo_nomiss_111525_use
dim(data)


data_reg <- data %>% select(
  li_ln,                            
  mg_ln,                         
  ca_ln,                                 
  v_ln,                         
  cr_ln,                         
  mn_ln,                          
  fe_ln,                          
  co_ln,                       
  ni_ln,                              
  cu_ln,                        
  zn_ln,                          
  as_ln,                        
  br_ln,                          
  se_ln,                      
  sr_ln,                        
  mo_ln,                       
  cd_ln,                       
  sn_ln,                       
  sb_ln,                    
  cs_ln,                     
  ba_ln,                    
  w_ln,                    
  tl_ln,                        
  pb_ln,  
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,    
  fvc_quality,
  fev1_quality,
  BMIscore,
  tobaco,
  age_new,               
  male_new
)%>% rename(
  "age" = "age_new",               
  "male" = "male_new" 
)

data_fev1 <- data_reg%>% filter(fev1_quality %in% c("A", "B", "C"))
data_fvc <- data_reg%>% filter(fvc_quality %in% c("A", "B", "C"))
data_ff <- data_reg%>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))
data_fef <- data_reg%>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))


#### USING LLN values as lung function outcomes ####

design_fev <- svydesign(id = ~1, data = data_fev1)


## fev1 < lln ##
model_fev <- svyglm(fev1belowlln_N ~ li_ln + mg_ln + ca_ln +  v_ln +  cr_ln +mn_ln + fe_ln +  co_ln +   ni_ln + cu_ln +   zn_ln + as_ln+  br_ln + se_ln +  sr_ln + mo_ln + cd_ln +  sn_ln +  sb_ln +  cs_ln +  ba_ln +  w_ln +  tl_ln + pb_ln + age + male + tobaco + BMIscore, 
                    design = design_fev, 
                    family = binomial())
summary(model_fev)

ORfev1<- tidy(model_fev,
              exponentiate = TRUE, 
              conf.int = TRUE) %>% 
  mutate(across(
    c(estimate,
      conf.low, 
      conf.high,
      p.value), \(x) round(x, 3)),
    model = "fev1_crude")


## fvc < lln ##

design_fvc <- svydesign(id = ~1, data = data_fvc)

model_fvc <- svyglm(fvcbelowlln_N ~ li_ln + mg_ln + ca_ln +  v_ln +  cr_ln +mn_ln + fe_ln +  co_ln +   ni_ln + cu_ln +   zn_ln + as_ln+  br_ln + se_ln +  sr_ln + mo_ln + cd_ln +  sn_ln +  sb_ln +  cs_ln +  ba_ln +  w_ln +  tl_ln + pb_ln + age + male + tobaco + BMIscore, 
                    design = design_fvc, 
                    family = binomial())
summary(model_fvc)

ORfvc<- tidy(model_fvc,
             exponentiate = TRUE, 
             conf.int = TRUE) %>% 
  mutate(across(c(estimate,
                  conf.low, 
                  conf.high, 
                  p.value),
                \(x) round(x, 3)),
         model = "fvc_crude")

## fev1/fvc < lln ##

design_ff <- svydesign(id = ~1, data = data_ff)

model_ff <- svyglm(ffbelowlln_N ~ li_ln + mg_ln + ca_ln +  v_ln +  cr_ln +mn_ln + fe_ln +  co_ln +   ni_ln + cu_ln +   zn_ln + as_ln+  br_ln + se_ln +  sr_ln + mo_ln + cd_ln +  sn_ln +  sb_ln +  cs_ln +  ba_ln +  w_ln +  tl_ln + pb_ln + age + male + tobaco + BMIscore, 
                   design = design_ff, 
                   family = binomial())
summary(model_ff)

ORff<- tidy(model_ff,
            exponentiate = TRUE, 
            conf.int = TRUE) %>% 
  mutate(across(c(estimate, 
                  conf.low,
                  conf.high,
                  p.value), \(x) round(x, 3)),
         model = "ff_crude")

## fef2575 < lln ##

design_fef <- svydesign(id = ~1, data = data_fef)

model_fef <- svyglm(fef2575belowlln_N~ li_ln + mg_ln + ca_ln +  v_ln +  cr_ln +mn_ln + fe_ln +  co_ln +   ni_ln + cu_ln +   zn_ln + as_ln+  br_ln + se_ln +  sr_ln + mo_ln + cd_ln +  sn_ln +  sb_ln +  cs_ln +  ba_ln +  w_ln +  tl_ln + pb_ln + age + male + tobaco + BMIscore, 
                    design = design_fef, 
                    family = binomial())
summary(model_fef)

ORfef<- tidy(model_fef,
             exponentiate = TRUE, 
             conf.int = TRUE) %>% 
  mutate(across(c(estimate,
                  conf.low, 
                  conf.high,
                  p.value), \(x) round(x, 3)),
         model = "fef_crude")

View(ORfef)
ORfef$term

combine <- bind_rows(ORfvc,
                     ORfev1,
                     ORff,
                     ORfef)

write.csv(combine, file = "OR_combine24.csv")

