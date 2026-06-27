library(bkmr)
library(parallel)
library(coda)
library(bkmrhat)
library(ggplot2)
library(patchwork)

data <- HM_lung_demo_nomiss_111525_use 

#### FEV1 < LLN ####
data1 <- data %>% select(
  fev1belowlln_N,
  fvcbelowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  tobaco,
  male_new,
  age_new,
  BMIscore,
  cd_ln,
  as_ln,
  cu_ln,
  sb_ln,
  pb_ln,
  ca_ln,
  fvc_quality,
  fev1_quality
)%>% rename(
  "male" = "male_new",
  "age" = "age_new"
) %>% drop_na()
dim(data1)



table(data1$fev1_quality, useNA = "always")

data_fev <- subset(data1, data1$fev1_quality %in% c(
  "A", "B", "C"))

dim(data_fev)
colnames(data_fev)

data_fev <- data_fev[, -c(15,16)]
sapply(data_fev, class)

data_fev_num <- as.data.frame(lapply(data_fev, as.numeric))
sapply(data_fev_num, class)
dim(data_fev_num)

data_fev_num <- as.data.frame(lapply(data_fev, as.numeric))
sapply(data_fev_num, class)
dim(data_fev_num)


y <- data_fev_num$fev1belowlln_N
length(y)

Z <- data_fev_num[, c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "sb_ln",
  "pb_ln",
  # "w_ln",
  "ca_ln")]
Z <- scale(Z)
View(Z)
dim(Z)


X <- data_fev_num[, c(
  "tobaco",
  "male",
  "age",
  "BMIscore"
)] 
dim(X)
X <- scale(as.data.frame(X))

sapply(X, class)
nrow(X)

future:::plan(strategy = future:::multisession())

set.seed(112)
fit_fev <- kmbayes_parallel(
  nchains=6,
  y = y,
  Z = Z,
  X = X,
  iter = 3000,  
  family = "binomial", 
  est.h = TRUE,
  verbose = FALSE, 
  varsel = TRUE)

View(fit_fev)

saveRDS(fit_fev, file = "fit_bkmr_fev1_18000_111625.rds")



fit <- fit_bkmr_fev1_18000_111625
length(fit)

fit1 <- fit[[1]]
fit2 <- fit[[2]]
fit3 <- fit[[3]]
fit4 <- fit[[4]]
fit5 <- fit[[5]]
fit6 <- fit[[6]]


par(mfrow=c(3,3))  # put 6 plots together
TracePlot(fit1, par = "r")
TracePlot(fit2, par = "r")
TracePlot(fit3, par = "r")
TracePlot(fit4, par = "r")
TracePlot(fit5, par = "r")
TracePlot(fit6, par = "r")

mcmc1 <- as.mcmc(fit1$lambda)
mcmc2 <- as.mcmc(fit2$lambda)
mcmc3 <- as.mcmc(fit3$lambda)
mcmc4 <- as.mcmc(fit4$lambda)
mcmc5 <- as.mcmc(fit5$lambda)
mcmc6 <- as.mcmc(fit6$lambda)


chains <- mcmc.list(mcmc1, mcmc2, mcmc3, mcmc4, mcmc5, mcmc6)
gelman.diag(chains)


## check with parameter r ##

mcmc.list.obj <- mcmc.list(
  as.mcmc(fit[[1]]$r),
  as.mcmc(fit[[2]]$r),
  as.mcmc(fit[[3]]$r),
  as.mcmc(fit[[4]]$r),
  as.mcmc(fit[[5]]$r),
  as.mcmc(fit[[6]]$r)
)

gelman.diag(mcmc.list.obj)

fit_combined <- kmbayes_combine(fitkm.list = fit) ### combine 6 chains

pips <- colMeans(fit_combined$delta)
pips


## save a graph ##
library(ggplot2)
df_plot <- data.frame(variable = colnames(Z), 
                      pip = pips)

View(df_plot)
colnames(df_plot)

df_plot <- df_plot %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony"
  ))

colnames(df_plot)

df_plot$variable <- factor(df_plot$variable, 
                           levels = df_plot$variable[order(-df_plot$pip)])


p_fev <- ggplot(df_plot, 
                aes(x = variable, 
                    y = pip)) +
  ylim(0,1) + 
  geom_col(width = 0.8, fill = "#088F8F") +
  geom_text(aes(label = round(pip, 2)),
            vjust =-0.5, 
            size = 4) +
  # coord_flip() +
  labs(x = "Metals", 
       y = "Posterior Inclusion Probability",
       title = expression(paste("FEV"[1], " < LLN"))) +
  theme_bw(base_size = 20)

ggsave("pip_fev1_contribution_111625.png",
       plot = p_fev,
       width = 8,
       height = 8,
       dpi = 300)


### calculates the estimated outcome (or predicted response) as a function of a single exposure, while holding all other exposures fixed ###
pred.resp.univar <- PredictorResponseUnivar(fit = fit_combined)
class(pred.resp.univar)
df_uni <- as.data.frame(pred.resp.univar)
table(df_uni$variable, useNA = "always")

df_uni <- df_uni %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony",
    TRUE ~ variable))


p_predi_fev <- ggplot(df_uni, aes(z, est)) +
  geom_ribbon(aes(ymin = est - 1.96*se,
                  ymax = est + 1.96*se),
              fill = "orange",
              alpha = 0.3) +
  geom_line(color = "blue", size = 1) +
  facet_wrap(~ variable) +
  labs(x = "Metals exposure", 
       y = expression(paste("Predicted FEV"[1], " < LLN")),
       title = expression(paste("FEV"[1], " < LLN"))) +
  theme_bw(base_size = 20)

p_predi_fev 

#### FVC < LLN ####

data <- HM_lung_demo_nomiss_111525_use 
data1 <- data %>% select(
  fev1belowlln_N,
  fvcbelowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  tobaco,
  male_new,
  age_new,
  BMIscore,
  cd_ln,
  as_ln,
  cu_ln,
  sb_ln,
  pb_ln,
  ca_ln,
  fvc_quality,
  fev1_quality
)%>% rename(
  "male" = "male_new",
  "age" = "age_new"
) %>% drop_na()
dim(data1)


table(data1$fvc_quality, useNA = "always")

data_fvc <- subset(data1, data1$fvc_quality %in% c(
  "A", "B", "C"))

dim(data_fvc)
colnames(data_fvc)


data_fvc <- data_fvc[, -c(15,16)]
sapply(data_fvc, class)

data_fvc_num <- as.data.frame(lapply(data_fvc, as.numeric))
sapply(data_fvc_num, class)
dim(data_fvc_num)

data_fvc_num <- as.data.frame(lapply(data_fvc, as.numeric))
sapply(data_fvc_num, class)
dim(data_fvc_num)


y <- data_fvc_num$fvcbelowlln_N
length(y)

Z <- data_fvc_num[, c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "sb_ln",
  "pb_ln",
  # "w_ln",
  "ca_ln")]
Z <- scale(Z)
View(Z)
dim(Z)


X <- data_fvc_num[, c(
  "tobaco",
  "male",
  "age",
  "BMIscore"
)] 
dim(X)
X <- scale(as.data.frame(X))

sapply(X, class)
nrow(X)

future:::plan(strategy = future:::multisession())

set.seed(112)
fit_fvc <- kmbayes_parallel(
  nchains=6,
  y = y,
  Z = Z,
  X = X,
  iter = 3000,       # increase if convergence not stable
  family = "binomial", # change to "binomial" for binary outcomes
  est.h = TRUE,
  verbose = FALSE, 
  varsel = TRUE)

View(fit_fvc)

saveRDS(fit_fvc, file = "fit_bkmr_fvc_18000_111625.rds")



fit <- fit_bkmr_fvc_18000_111625
length(fit)

fit1 <- fit[[1]]
fit2 <- fit[[2]]
fit3 <- fit[[3]]
fit4 <- fit[[4]]
fit5 <- fit[[5]]
fit6 <- fit[[6]]


par(mfrow=c(3,3))  # put 6 plots together
TracePlot(fit1, par = "r")
TracePlot(fit2, par = "r")
TracePlot(fit3, par = "r")
TracePlot(fit4, par = "r")
TracePlot(fit5, par = "r")
TracePlot(fit6, par = "r")

mcmc1 <- as.mcmc(fit1$lambda)
mcmc2 <- as.mcmc(fit2$lambda)
mcmc3 <- as.mcmc(fit3$lambda)
mcmc4 <- as.mcmc(fit4$lambda)
mcmc5 <- as.mcmc(fit5$lambda)
mcmc6 <- as.mcmc(fit6$lambda)

chains <- mcmc.list(mcmc1, mcmc2, mcmc3, mcmc4, mcmc5, mcmc6)
gelman.diag(chains)


## check with parameter r ##

mcmc.list.obj <- mcmc.list(
  as.mcmc(fit[[1]]$r),
  as.mcmc(fit[[2]]$r),
  as.mcmc(fit[[3]]$r),
  as.mcmc(fit[[4]]$r),
  as.mcmc(fit[[5]]$r),
  as.mcmc(fit[[6]]$r)
)

gelman.diag(mcmc.list.obj)

fit_combined <- kmbayes_combine(fitkm.list = fit) ### combine 6 chains

pips <- colMeans(fit_combined$delta)
pips

## save a graph ##
library(ggplot2)
df_plot <- data.frame(variable = colnames(Z), 
                      pip = pips)

# View(df_plot)
colnames(df_plot)

df_plot <- df_plot %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony"
  ))

colnames(df_plot)

df_plot$variable <- factor(df_plot$variable, 
                           levels = df_plot$variable[order(-df_plot$pip)])


p_fvc <- ggplot(df_plot, 
                aes(x = variable, 
                    y = pip)) +
  ylim(0,1) + 
  geom_col(width = 0.8, fill = "#088F8F") +
  geom_text(aes(label = round(pip, 2)), 
            vjust =-0.5,
            size = 4) +
  labs(x = "Metals", 
       y = "Posterior Inclusion Probability",
       title = "FVC < LLN") +
  theme_bw(base_size = 20)
# p_fef


### calculates the estimated outcome (or predicted response) as a function of a single exposure, while holding all other exposures fixed ###
pred.resp.univar <- PredictorResponseUnivar(fit = fit_combined)
class(pred.resp.univar)
df_uni <- as.data.frame(pred.resp.univar)
# View(pred.resp.univar)
table(df_uni$variable, useNA = "always")

df_uni <- df_uni %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony",
    TRUE ~ variable
  ))


p_predi_fvc <- ggplot(df_uni, aes(z, est)) +
  geom_ribbon(aes(ymin = est - 1.96*se,
                  ymax = est + 1.96*se),
              fill = "orange",
              alpha = 0.3) +  # light orange with transparency
  geom_line(color = "blue", size = 1) +  # line color
  facet_wrap(~ variable) +
  labs(x = "Metals exposure", 
       y = "Predicted FVC < LLN",
       title = "FVC < LLN") +
  theme_bw(base_size = 20)

p_predi_fvc 

#### FEV1/FVC < LLN ####

data <- HM_lung_demo_nomiss_111525_use 
names(data)

data1 <- data %>% select(
  fev1belowlln_N,
  fvcbelowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  tobaco,
  male_new,
  age_new,
  BMIscore,
  cd_ln,
  as_ln,
  cu_ln,
  sb_ln,
  pb_ln,
  ca_ln,
  fvc_quality,
  fev1_quality
)%>% rename(
  "male" = "male_new",
  "age" = "age_new"
) %>% drop_na()
dim(data1)

data_ff <- subset(data1, data1$fev1_quality %in% c(
  "A", "B", "C") &  data1$fvc_quality %in% c(
    "A", "B", "C"))

dim(data_ff)
colnames(data_ff)

data_ff <- data_ff[, -c(15,16)]
sapply(data_fef, class)

data_ff_num <- as.data.frame(lapply(data_ff, as.numeric))
sapply(data_ff_num, class)
dim(data_ff_num)

data_ff_num <- as.data.frame(lapply(data_ff, as.numeric))
sapply(data_ff_num, class)
dim(data_ff_num)


y <- data_ff_num$ffbelowlln_N
length(y)

Z <- data_fef_num[, c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "sb_ln",
  "pb_ln",
  "ca_ln")]
Z <- scale(Z)
View(Z)
dim(Z)

X <- data_fef_num[, c(
  "tobaco",
  "male",
  "age",
  "BMIscore"
)] 
dim(X)
X <- scale(as.data.frame(X))

sapply(X, class)
nrow(X)

future:::plan(strategy = future:::multisession())

set.seed(112)
fit_ff <- kmbayes_parallel(
  nchains=6,
  y = y,
  Z = Z,
  X = X,
  iter = 3000,       # increase if convergence not stable
  family = "binomial", # change to "binomial" for binary outcomes
  est.h = TRUE,
  verbose = FALSE, 
  varsel = TRUE
)

View(fit_ff)


saveRDS(fit_ff, file = "fit_bkmr_ff_18000_111625.rds")


fit <- fit_bkmr_ff_18000_111625 
length(fit)

fit1 <- fit[[1]]
fit2 <- fit[[2]]
fit3 <- fit[[3]]
fit4 <- fit[[4]]
fit5 <- fit[[5]]
fit6 <- fit[[6]]


par(mfrow=c(3,3))  # put 6 plots together
TracePlot(fit1, par = "r")
TracePlot(fit2, par = "r")
TracePlot(fit3, par = "r")
TracePlot(fit4, par = "r")
TracePlot(fit5, par = "r")
TracePlot(fit6, par = "r")

mcmc1 <- as.mcmc(fit1$lambda)
mcmc2 <- as.mcmc(fit2$lambda)
mcmc3 <- as.mcmc(fit3$lambda)
mcmc4 <- as.mcmc(fit4$lambda)
mcmc5 <- as.mcmc(fit5$lambda)
mcmc6 <- as.mcmc(fit6$lambda)


chains <- mcmc.list(mcmc1, mcmc2, mcmc3, mcmc4, mcmc5, mcmc6)
gelman.diag(chains)


## check with parameter r ##

mcmc.list.obj <- mcmc.list(
  as.mcmc(fit[[1]]$r),
  as.mcmc(fit[[2]]$r),
  as.mcmc(fit[[3]]$r),
  as.mcmc(fit[[4]]$r),
  as.mcmc(fit[[5]]$r),
  as.mcmc(fit[[6]]$r)
)

gelman.diag(mcmc.list.obj)

fit_combined <- kmbayes_combine(fitkm.list = fit)

pips <- colMeans(fit_combined$delta)
pips

## save a graph ##
library(ggplot2)
df_plot <- data.frame(variable = colnames(Z), 
                      pip = pips)

colnames(df_plot)

df_plot <- df_plot %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony"
  ))

colnames(df_plot)

df_plot$variable <- factor(df_plot$variable, 
                           levels = df_plot$variable[order(-df_plot$pip)])


p_ff <- ggplot(df_plot, 
               aes(x = variable, 
                   y = pip)) +
  ylim(0,1) + 
  geom_col(width = 0.8, fill = "#088F8F") +
  geom_text(aes(label = round(pip, 2)), 
            vjust =-0.5,
            size = 4) +
  labs(x = "Metals", 
       y = "Posterior Inclusion Probability",
       title = expression(paste("FEV"[1], "/FVC < LLN"))) +
  theme_bw(base_size = 20)


### calculates the estimated outcome (or predicted response) as a function of a single exposure, while holding all other exposures fixed ###
pred.resp.univar <- PredictorResponseUnivar(fit = fit_combined)
class(pred.resp.univar)
df_uni <- as.data.frame(pred.resp.univar)
# View(pred.resp.univar)
table(df_uni$variable, useNA = "always")

df_uni <- df_uni %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony",
    TRUE ~ variable
  ))


p_predi_ff <- ggplot(df_uni, aes(z, est)) +
  geom_ribbon(aes(ymin = est - 1.96*se,
                  ymax = est + 1.96*se),
              fill = "orange",
              alpha = 0.3) +
  geom_line(color = "blue", size = 1) + 
  facet_wrap(~ variable) +
  labs(x = "Metals exposure", 
       y = expression(paste("Predicted FEV"[1], "/FVC < LLN")),
       title = expression(paste("FEV"[1],"/FVC < LLN"))) +
  theme_bw(base_size = 20)

p_predi_ff


#### FEF2575 < LLN ####

data <- HM_lung_demo_nomiss_111525_use 
names(data)

data1 <- data %>% select(
  fev1belowlln_N,
  fvcbelowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  tobaco,
  male_new,
  age_new,
  BMIscore,
  cd_ln,
  as_ln,
  cu_ln,
  sb_ln,
  pb_ln,
  ca_ln,
  fvc_quality,
  fev1_quality
)%>% rename(
  "male" = "male_new",
  "age" = "age_new"
) %>% drop_na()
dim(data1)

data_fef <- subset(data1, data1$fev1_quality %in% c(
  "A", "B", "C") &  data1$fvc_quality %in% c(
    "A", "B", "C"))

dim(data_fef)
colnames(data_fef)

data_fef <- data_fef[, -c(15,16)]
sapply(data_fef, class)

data_fef_num <- as.data.frame(lapply(data_fef, as.numeric))
sapply(data_fef_num, class)
dim(data_fef_num)

data_fef_num <- as.data.frame(lapply(data_fef, as.numeric))
sapply(data_fef_num, class)
dim(data_fef_num)


y <- data_fef_num$fef2575belowlln_N
length(y)

Z <- data_fef_num[, c(
  "as_ln",
  "cd_ln",
  "cu_ln",
  "sb_ln",
  "pb_ln",
  "ca_ln")]
Z <- scale(Z)
View(Z)
dim(Z)

X <- data_fef_num[, c(
  "tobaco",
  "male",
  "age",
  "BMIscore"
)] 
dim(X)
X <- scale(as.data.frame(X))

sapply(X, class)
nrow(X)

future:::plan(strategy = future:::multisession())

set.seed(112)
fit_fef <- kmbayes_parallel(
  nchains=6,
  y = y,
  Z = Z,
  X = X,
  iter = 3000,       # increase if convergence not stable
  family = "binomial", # change to "binomial" for binary outcomes
  est.h = TRUE,
  verbose = FALSE, 
  varsel = TRUE
)

View(fit_fef)


saveRDS(fit_fef, file = "fit_bkmr_fef_18000_111625.rds")


fit<- fit_bkmr_fef_18000_111625
length(fit)

fit1 <- fit[[1]]
fit2 <- fit[[2]]
fit3 <- fit[[3]]
fit4 <- fit[[4]]
fit5 <- fit[[5]]
fit6 <- fit[[6]]


par(mfrow=c(3,3))  # put 6 plots together
TracePlot(fit1, par = "r")
TracePlot(fit2, par = "r")
TracePlot(fit3, par = "r")
TracePlot(fit4, par = "r")
TracePlot(fit5, par = "r")
TracePlot(fit6, par = "r")

mcmc1 <- as.mcmc(fit1$lambda)
mcmc2 <- as.mcmc(fit2$lambda)
mcmc3 <- as.mcmc(fit3$lambda)
mcmc4 <- as.mcmc(fit4$lambda)
mcmc5 <- as.mcmc(fit5$lambda)
mcmc6 <- as.mcmc(fit6$lambda)


chains <- mcmc.list(mcmc1, mcmc2, mcmc3, mcmc4, mcmc5, mcmc6)
gelman.diag(chains)


## check with parameter r ##

mcmc.list.obj <- mcmc.list(
  as.mcmc(fit[[1]]$r),
  as.mcmc(fit[[2]]$r),
  as.mcmc(fit[[3]]$r),
  as.mcmc(fit[[4]]$r),
  as.mcmc(fit[[5]]$r),
  as.mcmc(fit[[6]]$r)
)

gelman.diag(mcmc.list.obj)

fit_combined <- kmbayes_combine(fitkm.list = fit) ### combine 6 chains

pips <- colMeans(fit_combined$delta)
pips

## save a graph ##
library(ggplot2)
df_plot <- data.frame(variable = colnames(Z), 
                      pip = pips)

View(df_plot)
colnames(df_plot)

df_plot <- df_plot %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony"
  ))

colnames(df_plot)

df_plot$variable <- factor(df_plot$variable, 
                           levels = df_plot$variable[order(-df_plot$pip)])


p_fef <- ggplot(df_plot, 
                aes(x = variable, 
                    y = pip)) +
  ylim(0,1) + 
  geom_col(width = 0.8, fill = "#088F8F") +
  geom_text(aes(label = round(pip, 2)), 
            vjust =-0.5,
            size = 4) +
  labs(x = "Metals", 
       y = "Posterior Inclusion Probability",
       title = expression(paste("FEF"[paste("25–75")], " < LLN"))) +
  theme_bw(base_size = 20)

ggsave("pip_fef_contribution_111625.png",
       plot = p_fef,
       width = 8,
       height = 8,
       dpi = 300)

### calculates the estimated outcome (or predicted response) as a function of a single exposure, while holding all other exposures fixed ###
pred.resp.univar <- PredictorResponseUnivar(fit = fit_combined)
class(pred.resp.univar)
df_uni <- as.data.frame(pred.resp.univar)
# View(pred.resp.univar)
table(df_uni$variable, useNA = "always")

df_uni <- df_uni %>% mutate(
  variable = case_when(
    variable == "as_ln" ~    "Arsenic",
    variable == "ca_ln"~  "Calcium",
    variable ==  "cd_ln" ~ "Cadmium",
    variable == "cu_ln" ~  "Copper" ,
    variable ==  "pb_ln" ~ "Lead" ,
    variable ==  "sb_ln" ~  "Antimony",
    TRUE ~ variable
  ))


p_predi <- ggplot(df_uni, aes(z, est)) +
  geom_ribbon(aes(ymin = est - 1.96*se,
                  ymax = est + 1.96*se),
              fill = "orange",
              alpha = 0.3) +  # light orange with transparency
  geom_line(color = "blue", size = 1) +  # line color
  facet_wrap(~ variable) +
  labs(x = "Metals exposure", 
       y = expression(paste("Predicted FEF"[paste("25–75")], " < LLN")),
       title = expression(paste("FEF"[paste("25–75")], " < LLN"))) +
  theme_bw(base_size = 20)

p_predi


#### pip contribution ####

p_fev
p_fvc
p_ff
p_fef

p_predi_fev
p_predi_fvc
p_predi_ff
p_predi

p_pip <- (p_fvc|p_fev)/ (p_ff |p_fef)

ggsave("FigS9_pip_new_BKMR.png",
       plot = p_pip ,
       width = 16,
       height = 14,
       dpi = 300)


#### univariate predictions ####
p_predi_fvc
p_predi_fev
p_predi_ff
p_predi
p_uni <- (p_predi_fvc|p_predi_fev)/ (p_predi_ff|p_predi)


p_uni   <-  p_uni  + plot_annotation(
  title = "Univariate exposure response curve of each heavy metals and pulmonary",
  theme = theme(
    plot.title = element_text(size = 18, face = "bold")
  ))
p_uni

ggsave("FigS6_uni_curve_BKMR.png",
       plot = p_uni ,
       width = 16,
       height = 14,
       dpi = 300)

