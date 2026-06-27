library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(patchwork)
library(ggpubr)
library(FactoMineR)
library(factoextra)

table(HM_inMaui_clean$addcity, useNA = "always")

data <- HM_inMaui_clean %>% select(
  li_clean,
  mg_clean,
  ca_clean,
  v_clean,
  cr_clean,
  mn_clean,
  fe_clean,
  co_clean,
  ni_clean,
  cu_clean,
  zn_clean,
  as_clean,
  br_clean,
  se_clean,
  sr_clean,
  mo_clean,
  cd_clean,
  sn_clean,
  sb_clean,
  cs_clean,
  ba_clean,
  w_clean,
  tl_clean,
  pb_clean,
  location
) %>% rename(
  "Li" = "li_clean",
  "Ca" = "ca_clean",
  "Cr" = "cr_clean",
  "Fe"= "fe_clean",
  "Ni" = "ni_clean",
  "Zn" = "zn_clean",
  "Br" = "br_clean",
  "Sr"  = "sr_clean" ,
  # "Pd" = "usg_corrected_pd_ug_l" ,
  "Sn" = "sn_clean",
  "Cs" = "cs_clean",
  "W"  = "w_clean",
  "Pb" = "pb_clean" ,
  # "U"  =  "u_clean",
  "Mg"  = "mg_clean",
  "V"  = "v_clean",
  "Mn"  = "mn_clean",
  "Co"  ="co_clean",
  "Cu"  = "cu_clean",
  "As"  = "as_clean",
  "Se"  = "se_clean",
  "Mo" = "mo_clean",
  "Cd"  = "cd_clean",
  "Sb"  = "sb_clean",
  "Ba"  = "ba_clean",
  "Tl"  = "tl_clean" 
)

names(data)
dim(data)
table(data$location, useNA = "always")

data <- data %>% 
  mutate(location = case_when(
    location == "Wailuku/ Kahului" ~ "Wailuku/Kahului",
    TRUE ~ location))


table(data$location, useNA = "always")
names(data)


## Kula ##
data_kula <- subset(data, data$location == "Kula") 
View(data_kula)
names(data_kula)
data_kula <- data_kula[, -c(25)]

str(data_kula) ## check format of variables in dataframe

data_kula_scaled <- scale(data_kula) %>%
  na.omit(data_kula_scaled)# standardize

pca_metal_kula <- prcomp(data_kula_scaled,
                         center = TRUE, 
                         scale. = TRUE) ## perform pca


summary(pca_metal_kula)  
rotation_kula <- pca_metal_kula$rotation
rotation_kula

head(pca_metal_kula$x)

# Scree plot
fviz_eig(pca_metal_kula, addlabels = TRUE)

p_kula <- fviz_pca_var(pca_metal_kula,
                       col.var = "cos2", 
                       gradient.cols = c("blue",
                                         "red", 
                                         "green"), 
                       repel = TRUE)

plot_kula <- p_kula + theme_bw(base_size = 20) + 
  labs(title = "Kula",
       x = "PC1 (15.0% of the total variance)", 
       y = "PC2 (11.7% of the total variance)") + theme(
         legend.key.height = unit(0.5, "cm"),  # make vertical legend taller
         legend.key.width = unit(2, "cm"), # width of legend bar
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.position = "none")

plot_kula


## lahaina ##
table(data$location, useNA = "always")

data_lahaina <- subset(data, data$location == "Lahaina") 
View(data_lahaina)
names(data_lahaina)
data_lahaina <- data_lahaina[, -c(25)]

str(data_lahaina) ## check format of variables in dataframe

data_lahaina_scaled <- scale(data_lahaina) %>%
  na.omit(data_lahaina_scaled)# standardize

pca_metal_lahaina <- prcomp(data_lahaina_scaled,
                            center = TRUE, 
                            scale. = TRUE) ## perform pca


summary(pca_metal_lahaina)  
rotation_lahaina <- pca_metal_lahaina$rotation
rotation_lahaina

head(pca_metal_lahaina$x)

fviz_eig(pca_metal_lahaina, addlabels = TRUE)

p_lahaina <- fviz_pca_var(pca_metal_lahaina,
                          col.var = "cos2", 
                          gradient.cols = c("blue",
                                            "red", 
                                            "green"), 
                          repel = TRUE)

p_lahaina <- p_lahaina + theme_bw(base_size = 20) + 
  labs(title = "Lahaina",
       x = "PC1 (15.6% of the total variance)", 
       y = "PC2 (9.5% of the total variance)") + theme(
         legend.key.height = unit(0.5, "cm"),  # make vertical legend taller
         legend.key.width = unit(2, "cm"), # width of legend bar
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.position = "none")

p_lahaina

## Kahului/Wailuku  ##
table(data$location, useNA = "always")

data_kahului <- subset(data, data$location == "Wailuku/Kahului") 
View(data_kahului)
names(data_kahului)
data_kahului <- data_kahului[, -c(25)]

str(data_kahului) 
data_kahului_scaled <- scale(data_kahului) %>%
  na.omit(data_kahului_scaled)# standardize

pca_metal_kahului <- prcomp(data_kahului_scaled,
                            center = TRUE, 
                            scale. = TRUE) ## perform pca


summary(pca_metal_kahului)  
rotation_kahului <- pca_metal_kahului$rotation
rotation_kahului

head(pca_metal_kahului$x)

fviz_eig(pca_metal_kahului, addlabels = TRUE)

p_kahului <- fviz_pca_var(pca_metal_kahului,
                          col.var = "cos2", 
                          gradient.cols = c("blue",
                                            "red", 
                                            "green"), 
                          repel = TRUE)

p_kahului <- p_kahului + theme_bw(base_size = 20) + 
  labs(title = "Kahului/Wailuku",
       x = "PC1 (13.7% of the total variance)", 
       y = "PC2 (11.5% of the total variance)") +theme(
         legend.key.height = unit(0.5, "cm"),  # make vertical legend taller
         legend.key.width = unit(2, "cm"), # width of legend bar
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.position = "none")

p_kahului

## kihei  ##
table(data$location, useNA = "always")

data_kihei <- subset(data, data$location == "Kihei") 
View(data_kihei)
names(data_kihei)
data_kihei <- data_kihei[, -c(25)]

str(data_kihei) 

data_kihei_scaled <- scale(data_kihei) %>%
  na.omit(data_kihei_scaled)# standardize

pca_metal_kihei <- prcomp(data_kihei_scaled,
                          center = TRUE, 
                          scale. = TRUE) ## perform pca


summary(pca_metal_kihei)  
rotation_kihei <- pca_metal_kihei$rotation
rotation_kihei

head(pca_metal_kihei$x)
fviz_eig(pca_metal_kihei, addlabels = TRUE)

p_kihei <- fviz_pca_var(pca_metal_kihei,
                        col.var = "cos2", 
                        gradient.cols = c("blue",
                                          "red", 
                                          "green"), 
                        repel = TRUE)

p_kihei <- p_kihei + theme_bw(base_size = 20) + 
  labs(title = "Kihei",
       x = "PC1 (11.9% of the total variance)", 
       y = "PC2 (11.5% of the total variance)") + theme(
         legend.key.height = unit(0.5, "cm"),  # make vertical legend taller
         legend.key.width = unit(2, "cm"), # width of legend bar
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 10),
         legend.position = "none")

p_kihei

library(patchwork)
plot_kula
p_lahaina
p_kahului
p_kihei

plot_com <- (p_kahului |  p_kihei) / (plot_kula |p_lahaina) 


ggsave("FigS3_pca_treat_locations.png",
       plot = plot_com,
       width = 18,
       height = 16,
       bg = "white",
       dpi = 300)


