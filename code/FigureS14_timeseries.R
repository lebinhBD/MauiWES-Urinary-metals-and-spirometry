library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

#### coding for timeline ####
data <- HM_inMaui_clean_des

data <- data %>% rename(
"Nickel" = "ni_ln",
"Arsenic" =  "as_ln",
"Selenium" = "se_ln",
"Copper" = "cu_ln",
"Lead" = "pb_ln",
"Cobalt" = "co_ln",
"Chromium" =  "cr_ln",
"Barium" =  "ba_ln",
"Antimony" = "sb_ln",
"Cesium" = "cs_ln",
"Strontium" =  "sr_ln",
"Iron" =  "fe_ln",
"Manganese" = "mn_ln",
"Magnesium" = "mg_ln", # y
"Anadium" = "v_ln",
"Lithium" =  "li_ln",
"Calcium"= "ca_ln",
"Zinc" =  "zn_ln",
"bromine" =  "br_ln",
"Molybdenum"=  "mo_ln",
"Cadmium" = "cd_ln",
"Tin" = "sn_ln",
"Wolfram" = "w_ln",
"Thallium"   = "tl_ln")

class(data$Thallium)

data <- data |> dplyr:::mutate(time_plot = case_when(
  as.numeric(duration_months) >= 5.6 &    as.numeric(duration_months) < 6  ~ '1', ## 5- less than 6 months
  as.numeric(duration_months) >= 6.07 &    as.numeric(duration_months) < 7  ~ '2', ## 6-7 months
  as.numeric(duration_months) >= 10.4 &    as.numeric(duration_months) < 11  ~ '3', ## 10-less than 11 months
  as.numeric(duration_months) >= 11.85 &    as.numeric(duration_months) <= 12  ~ '4', ## 11-less than 12 months
  as.numeric(duration_months) > 12 &    as.numeric(duration_months) < 13  ~ '5', ## 12-less than 13 months
  as.numeric(duration_months) >= 13 &    as.numeric(duration_months) < 14  ~ '6', ## 13 - less than 14 months
  as.numeric(duration_months) >= 14  ~ '7', ## > 14 months
  .default = NA
))

table(data$time_plot, useNA = "always")
table(data$duration_months, useNA = 'always')

data$time_plot <- factor(data$time_plot,
                         levels = c(1,2,3,4,5,6,7),
                         labels = c("5-<6", 
                                    "6-<7",
                                    "10-<11",
                                    "11-<12",
                                    "12-<13",
                                    "13-<14",
                                    "14+"))

table(data$time_plot, useNA = "always")
table(data$duration_months, useNA = "always")
class(data$duration_months)

data$treat_burn <- factor(data$treat_burn,
                          levels = c(1,0),
                          labels = c("Burn zone", "Outside burn zone"))

names(data)


## nickel ##
plot_ni <- ggplot(data, aes(x = time_plot,
                            y = Nickel,
                            color = treat_burn,
                            group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) + 
  # error bars
  
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs( title = "Nickel",
        x = "Months from the wildfire",
        y = "Log of Nickel", 
        color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("nickel_time.png",
       plot = plot_ni,
       width = 14,
       height = 10, 
       dpi = 300)

## arsenic ##

plot_as <- ggplot(data, 
                  aes(x = time_plot,
                      y = Arsenic,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Arsenic",
       x = "Months from the wildfire",
       y = "Log of Arsenic", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("as_time.png",
       plot = plot_as,
       width = 14,
       height = 10, 
       dpi = 300)


## selenium ##

plot_se <- ggplot(data, 
                  aes(x = time_plot,
                      y = Selenium,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Selenium",
       x = "Months from the wildfire",
       y = "Log of Selenium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("se_time.png",
       plot = plot_se,
       width = 14,
       height = 10, 
       dpi = 300)

## Copper ##

plot_cu <- ggplot(data, 
                  aes(x = time_plot,
                      y = Copper,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Copper",
       x = "Months from the wildfire",
       y = "Log of Copper", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("cu_time.png",
       plot = plot_cu,
       width = 14,
       height = 10, 
       dpi = 300)

## Lead ##

plot_lead <- ggplot(data, 
                    aes(x = time_plot,
                        y = Lead,
                        color = treat_burn,
                        group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Lead",
       x = "Months from the wildfire",
       y = "Log of Lead", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("lead_time.png",
       plot = plot_lead,
       width = 14,
       height = 10, 
       dpi = 300)


## Cobalt ##

plot_co <- ggplot(data, 
                  aes(x = time_plot,
                      y = Cobalt,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Cobalt",
       x = "Months from the wildfire",
       y = "Log of Cobalt", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("co_time.png",
       plot = plot_co,
       width = 14,
       height = 10, 
       dpi = 300)


## Chromium ##

plot_cr <- ggplot(data, 
                  aes(x = time_plot,
                      y = Chromium,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Chromium",
       x = "Months from the wildfire",
       y = "Log of Chromium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("cr_time.png",
       plot = plot_cr,
       width = 14,
       height = 10, 
       dpi = 300)

## Barium ##

plot_ba <- ggplot(data, 
                  aes(x = time_plot,
                      y = Barium,
                      color = treat_burn,
                      group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Barium",
       x = "Months from the wildfire",
       y = "Log of Barium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("ba_time.png",
       plot = plot_ba,
       width = 14,
       height = 10, 
       dpi = 300)

## Antimony ##

plot_antimony <- ggplot(data, 
                        aes(x = time_plot,
                            y = Antimony,
                            color = treat_burn,
                            group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Antimony",
       x = "Months from the wildfire",
       y = "Log of Antimony", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("antimony_time.png",
       plot = plot_antimony,
       width = 14,
       height = 10, 
       dpi = 300)


## Cesium ##

plot_cesium <- ggplot(data, 
                      aes(x = time_plot,
                          y = Cesium,
                          color = treat_burn,
                          group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Cesium",
       x = "Months from the wildfire",
       y = "Log of Cesium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("cesium_time.png",
       plot = plot_cesium,
       width = 14,
       height = 10, 
       dpi = 300)


## Strontium ##

plot_strontium <- ggplot(data, 
                         aes(x = time_plot,
                             y = Strontium,
                             color = treat_burn,
                             group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Strontium",
       x = "Months from the wildfire",
       y = "Log of Strontium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("strontium_time.png",
       plot = plot_strontium,
       width = 14,
       height = 10, 
       dpi = 300)


## Iron ##

plot_iron <- ggplot(data, 
                    aes(x = time_plot,
                        y = Iron,
                        color = treat_burn,
                        group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Iron",
       x = "Months from the wildfire",
       y = "Log of Iron", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("iron_time.png",
       plot = plot_iron,
       width = 14,
       height = 10, 
       dpi = 300)

## Manganese ##

plot_manganese <- ggplot(data, 
                         aes(x = time_plot,
                             y = Manganese,
                             color = treat_burn,
                             group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Manganese",
       x = "Months from the wildfire",
       y = "Log of Manganese", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("manganese_time.png",
       plot = plot_manganese,
       width = 14,
       height = 10, 
       dpi = 300)


## Magnesium ##

plot_magnesium <- ggplot(data, 
                         aes(x = time_plot,
                             y = Magnesium,
                             color = treat_burn,
                             group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Magnesium",
       x = "Months from the wildfire",
       y = "Log of Magnesium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("magnesium_time.png",
       plot = plot_magnesium,
       width = 14,
       height = 10, 
       dpi = 300)


## Anadium ##

plot_anadium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Anadium,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Anadium",
       x = "Months from the wildfire",
       y = "Log of Anadium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("anadium_time.png",
       plot = plot_anadium,
       width = 14,
       height = 10, 
       dpi = 300)

## Lithium ##

plot_lithium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Lithium,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Lithium",
       x = "Months from the wildfire",
       y = "Log of Lithium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("lithium_time.png",
       plot = plot_lithium,
       width = 14,
       height = 10, 
       dpi = 300)

## Calcium ##

plot_calcium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Calcium,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Calcium",
       x = "Months from the wildfire",
       y = "Log of Calcium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("calcium_time.png",
       plot = plot_calcium,
       width = 14,
       height = 10, 
       dpi = 300)


## Zinc ##

plot_zinc <- ggplot(data, 
                    aes(x = time_plot,
                        y = Zinc,
                        color = treat_burn,
                        group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Zinc",
       x = "Months from the wildfire",
       y = "Log of Zinc", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("zinc_time.png",
       plot = plot_zinc,
       width = 14,
       height = 10, 
       dpi = 300)


## bromine ##

plot_bromine <- ggplot(data, 
                       aes(x = time_plot,
                           y = bromine,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Bromine",
       x = "Months from the wildfire",
       y = "Log of Bromine", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("bromine_time.png",
       plot = plot_bromine,
       width = 14,
       height = 10, 
       dpi = 300)

##Molybdenum##

plot_molybdenum <- ggplot(data, 
                          aes(x = time_plot,
                              y = Molybdenum,
                              color = treat_burn,
                              group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Molybdenum",
       x = "Months from the wildfire",
       y = "Log of Molybdenum", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("molybdenum_time.png",
       plot = plot_molybdenum,
       width = 14,
       height = 10, 
       dpi = 300)


##Cadmium##

plot_cadmium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Cadmium,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Cadmium",
       x = "Months from the wildfire",
       y = "Log of Cadmium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("cadmium_time.png",
       plot = plot_cadmium,
       width = 14,
       height = 10, 
       dpi = 300)


##Tin##

plot_tin <- ggplot(data, 
                   aes(x = time_plot,
                       y = Tin,
                       color = treat_burn,
                       group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Tin",
       x = "Months from the wildfire",
       y = "Log of Tin", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("tin_time.png",
       plot = plot_tin,
       width = 14,
       height = 10, 
       dpi = 300)

##Wolfram ##
plot_wolfram <- ggplot(data, 
                       aes(x = time_plot,
                           y = Wolfram,
                           color = treat_burn,
                           group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Wolfram",
       x = "Months from the wildfire",
       y = "Log of Wolfram", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("wolfram_time.png",
       plot = plot_wolfram,
       width = 14,
       height = 10, 
       dpi = 300)

## Thallium##
plot_thallium <- ggplot(data, 
                        aes(x = time_plot,
                            y = Thallium,
                            color = treat_burn,
                            group = treat_burn)) +
  stat_summary(fun = mean, 
               geom = "line", 
               size = 1,
               position = position_dodge(width = 0.3)) +  # mean line
  stat_summary(fun = mean, 
               geom = "point", 
               size = 3,
               position = position_dodge(width = 0.3)) +
  # aes(shape = treat_burn)) +  # mean points
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               position = position_dodge(width = 0.3)) +  # error bars
  scale_color_manual(values = c(
    "Burn zone" = "#CC5500",
    "Outside burn zone" = "#088F8F"
  )) +
  
  labs(title = "Thallium",
       x = "Months from the wildfire",
       y = "Log of Thallium", 
       color = "Group") +
  theme_bw(base_size = 18)+
  theme(legend.position = "bottom")

ggsave("thallium_time.png",
       plot = plot_thallium,
       width = 14,
       height = 10, 
       dpi = 300)


p <- (plot_ni |plot_as | plot_se | plot_cu) / 
  (plot_lead | plot_co | plot_cr | plot_ba)/ 
  (plot_antimony | plot_cesium | plot_strontium | plot_iron) / 
  (plot_manganese | plot_magnesium | plot_anadium | plot_lithium)/ 
  (plot_calcium|plot_zinc| plot_bromine | plot_molybdenum)/ 
  (plot_cadmium |plot_tin|plot_wolfram |plot_thallium) + plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave("FigS13_heavy_metal_mean_new.png",
       plot = p,
       width = 30,
       height = 26,
       dpi = 300)
