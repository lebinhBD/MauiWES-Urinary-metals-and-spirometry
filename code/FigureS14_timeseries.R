library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

#### coding for timeline ####
data <- HM_inMaui_clean_des.rds
  
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
  as.numeric(duration_months) >= 5.6 &    as.numeric(duration_months) < 7  ~ '1', ## 6-  7 months
  as.numeric(duration_months) >= 10.4 &    as.numeric(duration_months) < 12  ~ '2', ## 10-less than 12 months
  as.numeric(duration_months) > 12 &    as.numeric(duration_months) < 14  ~ '3', ## 12-less than 14 months
  as.numeric(duration_months) >= 14  ~ '4', ## > 14 months
  .default = NA
))

table(data$time_plot, useNA = "always")
table(data$duration_months, useNA = 'always')

data$time_plot <- factor(data$time_plot,
                         levels = c(1,2,3,4),
                         labels = c("6-7", 
                                    "10-11",
                                    "12-13",
                                    "14+"))

table(data$time_plot, useNA = "always")
table(data$duration_months, useNA = "always")
class(data$duration_months)
# 
# data$treat_burn <- factor(data$treat_burn,
#                           levels = c(1,0),
#                           labels = c("Burn zone", "Outside burn zone"))
# 
# names(data)


## nickel ##
plot_ni <- ggplot(data, aes(x = time_plot,
                            y = Nickel,
                            group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               linewidth = 1,
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black") + 
  
  # scale_y_continuous(
  #   limits = c(0, 2),
  #   breaks = seq(0, 2, by = 0.5)) + ## will remove the values outside the range before calculate the mean and 95%CI
  
  coord_cartesian(ylim = c(0, 2)) +
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.5) )+ ## will not remove the values outside the range 
  labs( title = "Nickel",
        x = "Months from the wildfire",
        y = "Log of Nickel")+ 
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("nickel_time_full.png",
       plot = plot_ni,
       width = 14,
       height = 10, 
       dpi = 300)

## arsenic ##

plot_as <- ggplot(data, 
                  aes(x = time_plot,
                      y = Arsenic,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               linewidth = 1,
               color = "#2170b9",
               fill = "#2170b9",
               width = 0.7) +  # mean line
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black") + 
  
  coord_cartesian(ylim = c(0,4)) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1) )+
  
  labs(title = "Arsenic",
       x = "Months from the wildfire",
       y = "Log of Arsenic") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("as_time_full.png",
       plot = plot_as,
       width = 14,
       height = 10, 
       dpi = 300)


## selenium ##

plot_se <- ggplot(data, 
                  aes(x = time_plot,
                      y = Selenium,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               linewidth = 1,
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7) +  # mean line
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black") +
  
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 1) )+ 
  
  labs(title = "Selenium",
       x = "Months from the wildfire",
       y = "Log of Selenium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("se_time_full.png",
       plot = plot_se,
       width = 14,
       height = 10, 
       dpi = 300)

## Copper ##

plot_cu <- ggplot(data, 
                  aes(x = time_plot,
                      y = Copper,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               linewidth = 1,
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+ # mean line
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black") +   # error bars
  
  coord_cartesian(ylim = c(0, 4)) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1) )+
  
  labs(title = "Copper",
       x = "Months from the wildfire",
       y = "Log of Copper") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("cu_time_full.png",
       plot = plot_cu,
       width = 14,
       height = 10, 
       dpi = 300)

## Lead ##

plot_lead <- ggplot(data, 
                    aes(x = time_plot,
                        y = Lead,
                        group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+  # mean line
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-1, 0)) +
  scale_y_continuous(
    breaks = seq(-1, 0, by = 0.25) )+
  
  labs(title = "Lead",
       x = "Months from the wildfire",
       y = "Log of Lead") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("lead_time_full.png",
       plot = plot_lead,
       width = 14,
       height = 10, 
       dpi = 300)


## Cobalt ##

plot_co <- ggplot(data, 
                  aes(x = time_plot,
                      y = Cobalt,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-1, 0)) +
  scale_y_continuous(
    breaks = seq(-1, 0, by = 0.25) )+ 
  
  labs(title = "Cobalt",
       x = "Months from the wildfire",
       y = "Log of Cobalt") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("co_time_full.png",
       plot = plot_co,
       width = 14,
       height = 10, 
       dpi = 300)


## Chromium ##

plot_cr <- ggplot(data, 
                  aes(x = time_plot,
                      y = Chromium,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2) )+
  
  labs(title = "Chromium",
       x = "Months from the wildfire",
       y = "Log of Chromium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("cr_time_full.png",
       plot = plot_cr,
       width = 14,
       height = 10, 
       dpi = 300)

## Barium ##

plot_ba <- ggplot(data, 
                  aes(x = time_plot,
                      y = Barium,
                      group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  coord_cartesian(ylim = c(0, 2)) +
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.5) )+
  
  labs(title = "Barium",
       x = "Months from the wildfire",
       y = "Log of Barium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("ba_time_full.png",
       plot = plot_ba,
       width = 14,
       height = 10, 
       dpi = 300)

## Antimony ##

plot_antimony <- ggplot(data, 
                        aes(x = time_plot,
                            y = Antimony,
                            group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+ 
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  coord_cartesian(ylim = c(0, 1)) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.25) )+ 
  
  labs(title = "Antimony",
       x = "Months from the wildfire",
       y = "Log of Antimony") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("antimony_time_full.png",
       plot = plot_antimony,
       width = 14,
       height = 10, 
       dpi = 300)


## Cesium ##

plot_cesium <- ggplot(data, 
                      aes(x = time_plot,
                          y = Cesium,
                          group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+ 
  coord_cartesian(ylim = c(0, 3)) +
  scale_y_continuous(
    breaks = seq(0, 3, by = 0.5) )+
  
  labs(title = "Cesium",
       x = "Months from the wildfire",
       y = "Log of Cesium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("cesium_time_full.png",
       plot = plot_cesium,
       width = 14,
       height = 10, 
       dpi = 300)


## Strontium ##

plot_strontium <- ggplot(data, 
                         aes(x = time_plot,
                             y = Strontium,
                             group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0,6)) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 1) )+ 
  
  labs(title = "Strontium",
       x = "Months from the wildfire",
       y = "Log of Strontium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("strontium_time_full.png",
       plot = plot_strontium,
       width = 14,
       height = 10, 
       dpi = 300)


## Iron ##

plot_iron <- ggplot(data, 
                    aes(x = time_plot,
                        y = Iron,
                        group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 4)) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1) )+
  
  labs(title = "Iron",
       x = "Months from the wildfire",
       y = "Log of Iron") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("iron_time_full.png",
       plot = plot_iron,
       width = 14,
       height = 10, 
       dpi = 300)

## Manganese ##

plot_manganese <- ggplot(data, 
                         aes(x = time_plot,
                             y = Manganese,
                             group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-1, 0)) +
  scale_y_continuous(
    breaks = seq(-1, 0, by = 0.25) )+ 
  
  labs(title = "Manganese",
       x = "Months from the wildfire",
       y = "Log of Manganese") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("manganese_time_full.png",
       plot = plot_manganese,
       width = 14,
       height = 10, 
       dpi = 300)


## Magnesium ##

plot_magnesium <- ggplot(data, 
                         aes(x = time_plot,
                             y = Magnesium,
                             group = 1) )+
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 12)) +
  scale_y_continuous(
    breaks = seq(0, 12, by = 2) )+
  
  labs(title = "Magnesium",
       x = "Months from the wildfire",
       y = "Log of Magnesium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("magnesium_time_full.png",
       plot = plot_magnesium,
       width = 14,
       height = 10, 
       dpi = 300)


## Vanadium ##

plot_anadium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Anadium,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black") + 
  
  coord_cartesian(ylim = c(-2, 0)) +
  scale_y_continuous(
    breaks = seq(-2, 0, by = 0.5) )+ 
  
  labs(title = "Vanadium",
       x = "Months from the wildfire",
       y = "Log of Vanadium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("vanadium_time_full.png",
       plot = plot_anadium,
       width = 14,
       height = 10, 
       dpi = 300)

## Lithium ##

plot_lithium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Lithium,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  
  coord_cartesian(ylim = c(0, 4)) +
  scale_y_continuous(
    breaks = seq(0, 4, by = 1) )+ 
  
  labs(title = "Lithium",
       x = "Months from the wildfire",
       y = "Log of Lithium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("lithium_time_full.png",
       plot = plot_lithium,
       width = 14,
       height = 10, 
       dpi = 300)

## Calcium ##

plot_calcium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Calcium,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 14)) +
  scale_y_continuous(
    breaks = seq(0, 14, by = 2) )+
  
  labs(title = "Calcium",
       x = "Months from the wildfire",
       y = "Log of Calcium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("calcium_time_full.png",
       plot = plot_calcium,
       width = 14,
       height = 10, 
       dpi = 300)


## Zinc ##

plot_zinc <- ggplot(data, 
                    aes(x = time_plot,
                        y = Zinc,
                        group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 8)) +
  scale_y_continuous(
    breaks = seq(0, 8, by = 2) )+ 
  
  labs(title = "Zinc",
       x = "Months from the wildfire",
       y = "Log of Zinc") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("zinc_time_full.png",
       plot = plot_zinc,
       width = 14,
       height = 10, 
       dpi = 300)


## bromine ##

plot_bromine <- ggplot(data, 
                       aes(x = time_plot,
                           y = bromine,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 10)) +
  scale_y_continuous(
    breaks = seq(0,10 , by = 2) )+
  
  labs(title = "Bromine",
       x = "Months from the wildfire",
       y = "Log of Bromine") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("bromine_time_full.png",
       plot = plot_bromine,
       width = 14,
       height = 10, 
       dpi = 300)

##Molybdenum##

plot_molybdenum <- ggplot(data, 
                          aes(x = time_plot,
                              y = Molybdenum,
                              group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(0, 6)) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 2) )+
  
  labs(title = "Molybdenum",
       x = "Months from the wildfire",
       y = "Log of Molybdenum") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("molybdenum_time_full.png",
       plot = plot_molybdenum,
       width = 14,
       height = 10, 
       dpi = 300)


##Cadmium##

plot_cadmium <- ggplot(data, 
                       aes(x = time_plot,
                           y = Cadmium,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-2, 0)) +
  scale_y_continuous(
    breaks = seq(-2, 0, by = 0.25) )+
  
  labs(title = "Cadmium",
       x = "Months from the wildfire",
       y = "Log of Cadmium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("cadmium_time_full.png",
       plot = plot_cadmium,
       width = 14,
       height = 10, 
       dpi = 300)


##Tin##

plot_tin <- ggplot(data, 
                   aes(x = time_plot,
                       y = Tin,
                       group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar",
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-0.75,0.25 )) +
  scale_y_continuous(
    breaks = seq(-0.75, 0.25, by = 0.25) )+ 
  
  labs(title = "Tin",
       x = "Months from the wildfire",
       y = "Log of Tin") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("tin_time_full.png",
       plot = plot_tin,
       width = 14,
       height = 10, 
       dpi = 300)

##Wolfram ##
plot_wolfram <- ggplot(data, 
                       aes(x = time_plot,
                           y = Wolfram,
                           group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+
  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  coord_cartesian(ylim = c(-2, 0)) +
  scale_y_continuous(
    breaks = seq(-2, 0, by = 0.5) )+ 
  
  labs(title = "Wolfram",
       x = "Months from the wildfire",
       y = "Log of Wolfram") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")  
  
ggsave("wolfram_time_full.png",
       plot = plot_wolfram,
       width = 14,
       height = 10, 
       dpi = 300)

## Thallium##
plot_thallium <- ggplot(data, 
                        aes(x = time_plot,
                            y = Thallium,
                            group = 1)) +
  stat_summary(fun = mean, 
               geom = "bar", 
               color = "#2170b9", # 
               fill = "#2170b9",
               width = 0.7)+

  
  stat_summary(fun.data = mean_cl_normal, 
               geom = "errorbar", 
               width = 0.2,
               color = "black")+
  
  coord_cartesian(ylim = c(-2, 0)) +
  scale_y_continuous(
    breaks = seq(-2, 0, by = 0.5) )+ 
  
  labs(title = "Thallium",
       x = "Months from the wildfire",
       y = "Log of Thallium") +
  theme_bw(base_size = 20)+
  theme(legend.position = "bottom")

ggsave("thallium_time_full.png",
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


ggsave("FigS14_heavy_metal_mean_new.png",
       plot = p,
       width = 30,
       height = 26,
       dpi = 300)
