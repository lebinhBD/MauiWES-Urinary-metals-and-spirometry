library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)
library(ggrepel)

#### Geographic map for pulmonary ####

data_lung <- pulmonary_inMaui_clean
dim(data_lung)
table(data_lung$location, useNA = "always")

options(tigris_use_cache = TRUE)
zip_shapes <- zctas(cb = TRUE, year = 2020)
hi_zips <- zip_shapes %>%
  filter(substr(ZCTA5CE20, 1, 3) %in% c("967", "968")) ### take polygons for each zipcode
View(hi_zips)
names(hi_zips)
View(zip_shapes)


hi_counties <- counties(state = "HI", cb = TRUE, year =2020) ## take polygon for each county
plot(st_geometry(hi_counties))
names(data_lung)


data2 <- data_lung %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  zip
)

View(data2)
names(data2)

df_zip <- data2 %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(
    percent_fev1 = mean(fev1belowlln_N, na.rm = TRUE) *100,
    percent_fvc = mean(fvcbelowlln_N, na.rm = TRUE) *100,
    percent_ff = mean(ffbelowlln_N, na.rm = TRUE) *100,
    percent_fef = mean(fef2575belowlln_N, na.rm = TRUE) *100,
  )
View(df_zip)

hi_map <- hi_zips %>%
  left_join(df_zip, by = c("ZCTA5CE20" = "zip"))

View(hi_map)

maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data_lung %>% select(
  fvcbelowlln_N,
  fev1belowlln_N,
  ffbelowlln_N,
  fef2575belowlln_N,
  addcity
) 


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_fev1_city = mean(fev1belowlln_N, na.rm = TRUE)*100,
    mean_fvc_city = mean(fvcbelowlln_N, na.rm = TRUE)*100,
    mean_ff_city = mean(ffbelowlln_N, na.rm = TRUE)*100,
    mean_fef_city = mean(fef2575belowlln_N, na.rm = TRUE)*100)

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

#### FEV1 < LLN ####
p_fev1 <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fev1), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste("FEV"[1], " < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fev1_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste("FEV"[1], " < LLN (%)")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fev1 <- p_fev1 + axis_theme

ggsave("geo_fev1.png",
       plot = p_fev1,
       width = 12,
       height = 10,
       dpi = 300)

#### FVC < LLN ####
p_fvc <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fvc), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "FVC < LLN (%)",
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fvc_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "FVC < LLN (%)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fvc <- p_fvc + axis_theme

ggsave("geo_fvc.png",
       plot = p_fvc,
       width = 12,
       height = 10,
       dpi = 300)

#### FEV1/FVC < LLN ####
p_ff <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_ff), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste(FEV[1], "/FVC < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_ff_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste(FEV[1], "/FVC < LLN (%)")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_ff <- p_ff + axis_theme

ggsave("geo_ff.png",
       plot = p_ff,
       width = 12,
       height = 10,
       dpi = 300)

#### FEF2575  < LLN ####
p_fef <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fef), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste("FEF"[paste("25–75")], " < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fef_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste("FEF"[paste("25–75")], " < LLN (%)")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fef <- p_fef + axis_theme

ggsave("geo_fef.png",
       plot = p_fef,
       width = 12,
       height = 10,
       dpi = 300)


#### section quality ####

#### FEV1 < LLN ####
names(data_lung)

data_fev1 <- data_lung %>% select(
  fev1belowlln_N,
  fev1_quality, 
  zip
) %>% filter(fev1_quality %in% c("A", "B", "C"))

View(data_fev1)
table(data_fev1$fev1_quality, useNA = "always")

df_fev1_zip <- data_fev1 %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(
    percent_fev1 = mean(fev1belowlln_N, na.rm = TRUE) *100
  )
View(df_fev1_zip)

hi_map <- hi_zips %>%
  left_join(df_fev1_zip, by = c("ZCTA5CE20" = "zip"))

View(hi_map)
names(hi_map)

maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data_lung %>% select(
  fev1belowlln_N,
  fev1_quality,
  addcity
) %>% filter(fev1_quality %in% c("A", "B", "C"))


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_fev1_city = mean(fev1belowlln_N, na.rm = TRUE)*100)

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

#### FEV1 < LLN ####
p_fev1_quality <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fev1), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste("FEV"[1], " < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fev1_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste("FEV"[1], " < LLN (%) - Quality")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fev1_quality <- p_fev1_quality + axis_theme

ggsave("geo_fev1_quality.png",
       plot = p_fev1_quality,
       width = 12,
       height = 10,
       dpi = 300)

#### FVC < LLN ####
names(data_lung)

data_fvc <- data_lung %>% select(
  fvcbelowlln_N,
  fvc_quality, 
  zip
) %>% filter(fvc_quality %in% c("A", "B", "C"))

View(data_fvc)
table(data_fvc$fvc_quality, useNA = "always")

df_fvc_zip <- data_fvc %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(
    percent_fvc = mean(fvcbelowlln_N, na.rm = TRUE) *100
  )
View(df_fvc_zip)

hi_map <- hi_zips %>%
  left_join(df_fvc_zip, by = c("ZCTA5CE20" = "zip"))

View(hi_map)
names(hi_map)


### create data for town ###

maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data_lung %>% select(
  fvcbelowlln_N,
  fvc_quality,
  addcity
) %>% filter(fvc_quality %in% c("A", "B", "C"))


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_fvc_city = mean(fvcbelowlln_N, na.rm = TRUE)*100)

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

#### FVC < LLN ####
p_fvc_quality <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fvc), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "FVC < LLN (%)",
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fvc_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "FVC < LLN (%) - Quality",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fvc_quality <- p_fvc_quality + axis_theme

ggsave("geo_fvc_quality.png",
       plot = p_fvc_quality,
       width = 12,
       height = 10,
       dpi = 300)


#### FEV1/FVC < LLN ####
names(data_lung)

data_ff <- data_lung %>% select(
  ffbelowlln_N,
  fev1_quality,
  fvc_quality, 
  zip
) %>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))

table(data_ff$ffbelowlln_N, useNA = "always")

# View(data_ff)
table(data_ff$fvc_quality, useNA = "always")
table(data_ff$fev1_quality, useNA = "always")

df_ff_zip <- data_ff %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(
    percent_ff = mean(ffbelowlln_N, na.rm = TRUE) *100
  )


# View(df_ff_zip)

hi_map <- hi_zips %>%
  left_join(df_ff_zip, by = c("ZCTA5CE20" = "zip"))

# View(hi_map)
names(hi_map)


### create data for town ###

# Kahului yes  20.87; 156.46
# Kihei yes  20.764427; 156.445007
# Kula no ~ 20.77; - 156.33
# Lahaina yes 20.8743; 156.6766
# Other 20.92; -156.31 Haiku
# Wailuku yes 20.891483; 156.510284

maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data_lung %>% select(
  ffbelowlln_N,
  fev1_quality,
  fvc_quality,
  addcity
) %>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_ff_city = mean(ffbelowlln_N, na.rm = TRUE)*100)

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
# View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

#### FEV1/FVC < LLN ####
p_ff_quality <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_ff), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste("FEV"[1], "/FVC < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_ff_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste("FEV"[1], "/FVC < LLN (%) - Quality")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_ff_quality <- p_ff_quality + axis_theme

ggsave("geo_ff_quality.png",
       plot = p_ff_quality,
       width = 12,
       height = 10,
       dpi = 300)

#### FEF2575 < LLN ####
names(data_lung)

data_fef <- data_lung %>% select(
  fef2575belowlln_N,
  fev1_quality,
  fvc_quality, 
  zip
) %>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))

table(data_fef$fef2575belowlln_N, useNA = "always")

dim(data_fef)
table(data_fef$fvc_quality, useNA = "always")
table(data_fef$fev1_quality, useNA = "always")

df_fef_zip <- data_fef %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(
    percent_fef = mean(fef2575belowlln_N, na.rm = TRUE) *100
  )


View(df_fef_zip)

hi_map <- hi_zips %>%
  left_join(df_fef_zip, by = c("ZCTA5CE20" = "zip"))

View(hi_map)
names(hi_map)


### create data for town ###


maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data_lung %>% select(
  fef2575belowlln_N,
  fev1_quality,
  fvc_quality,
  addcity
) %>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_fef_city = mean(fef2575belowlln_N, na.rm = TRUE)*100)

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

#### FEF2575 < LLN ####
p_fef_quality <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = percent_fef), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = expression(paste("FEF"[paste("25–75")], " < LLN (%)")),
                       limits = c(0,100),
                       breaks = seq(0,100, by=20),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fef_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = expression(paste("FEF"[paste("25–75")], " < LLN (%) - Quality")),
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_fef_quality <- p_fef_quality + axis_theme

ggsave("geo_fef_quality.png",
       plot = p_fef_quality,
       width = 12,
       height = 10,
       dpi = 300)


library(patchwork)


p_fev1
p_fvc
p_ff
p_fef 

p_fev1_quality
p_fvc_quality
p_ff_quality
p_fef_quality


library(patchwork)
plot_com <- (p_fev1 | p_fev1_quality)/ 
  (p_fvc | p_fvc_quality)/ 
  (p_ff | p_ff_quality) /
  (p_fef | p_fef_quality) 
# # plot_annotation(
# # title = "Percentage of abnormal pulmonary",
# theme = theme(
#   plot.title = element_text(size = 21, face = "bold"),
#   plot.subtitle = element_text(size = 12 )
# ))
# plot_com
ggsave("pulmonary_com.png",
       plot = plot_com,
       width = 14,
       height = 18,
       bg = "white",
       dpi = 300)


#### Figure of heavy distribution by location ####

data <- HM_inMaui_clean
dim(data)

table(data$zip, useNA = "always")
table(data$zip, data$addcity, useNA = "always")
table(data$location, useNA = "always")
table(data$addcity, useNA = "always")

names(data)
dim(data)

sum(is.na(data$li_clean))
sum(is.na(data$pb_clean))
sum(is.na(data$cs_clean))
sum(is.na(data$li_ug_l))
summary(data$lod_li)
names(data)


data1 <- data %>% select(
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
  # cu_clean,
  # ca_clean,
  # fvcbelowlln_N,
  # fev1belowlln_N,
  # ffbelowlln_N,
  # fef2575belowlln_N.x,
  # fvc_percent_nhanes,
  # fev1_percent_nhanes,
  # fef2575_percent_nhanes,
  # fev1_fvcratio,
  zip,
  addcity
)

table(data1$zip, data1$addcity, useNA = "always")
class(data1$zip)


data1 <- data1 %>% mutate(
  zip = case_when(
    is.na(data1$zip) & data1$addcity == "Lahaina" ~ "96761",
    is.na(data1$zip) & data1$addcity == "Wailuku" ~ "96793",
    is.na(data1$zip) & data1$addcity == "Kihei" ~ "96753",
    is.na(data1$zip) & data1$addcity == "Kula" ~ "96790",
    TRUE ~ zip
    
  )
)

#### Geographic map ####

table(data1$zip, useNA = "always")

options(tigris_use_cache = TRUE)
zip_shapes <- zctas(cb = TRUE, year = 2020)
hi_zips <- zip_shapes %>%
  filter(substr(ZCTA5CE20, 1, 3) %in% c("967", "968")) ### take polygons for each zipcode
View(hi_zips)
names(hi_zips)
View(zip_shapes)

hi_counties <- counties(state = "HI", cb = TRUE, year =2020) ## take polygon for each county
plot(st_geometry(hi_counties))


data2 <- data1 %>% select(
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
  # fvcbelowlln_N,
  # fev1belowlln_N,
  # ffbelowlln_N,
  # fef2575belowlln_N,
  # fvc_percent_nhanes,
  # fev1_percent_nhanes,
  # fef2575_percent_nhanes,
  # fev1_fvcratio,
  zip
)

View(data2)
names(data2)

df_zip <- data2 %>%
  mutate(zip = as.character(zip)) %>%
  group_by(zip) %>%
  summarise(mean_li = mean(li_clean, na.rm = TRUE),
            mean_mg = mean(mg_clean, na.rm = TRUE),
            mean_ca = mean(ca_clean, na.rm = TRUE),
            mean_v = mean(v_clean, na.rm = TRUE),
            mean_cr = mean(cr_clean, na.rm = TRUE),
            mean_mn = mean(mn_clean, na.rm = TRUE),
            mean_fe = mean(fe_clean, na.rm = TRUE),
            mean_co = mean(co_clean, na.rm = TRUE),
            mean_ni = mean(ni_clean, na.rm = TRUE),
            mean_cu = mean(cu_clean, na.rm = TRUE),
            mean_zn = mean(zn_clean, na.rm = TRUE),
            mean_as = mean(as_clean, na.rm = TRUE),
            mean_br = mean(br_clean, na.rm = TRUE),
            mean_se = mean(se_clean, na.rm = TRUE),
            mean_sr= mean(sr_clean, na.rm = TRUE),
            mean_mo = mean(mo_clean, na.rm = TRUE),
            mean_cd = mean(cd_clean, na.rm = TRUE),
            mean_sn = mean(sn_clean, na.rm = TRUE),
            mean_sb = mean(sb_clean, na.rm = TRUE),
            mean_cs = mean(cs_clean, na.rm = TRUE),
            mean_ba = mean(ba_clean, na.rm = TRUE),
            mean_w = mean(w_clean, na.rm = TRUE),
            mean_tl = mean(tl_clean, na.rm = TRUE),
            mean_pb = mean(pb_clean, na.rm = TRUE) )

View(df_zip)

hi_map <- hi_zips %>%
  left_join(df_zip, by = c("ZCTA5CE20" = "zip"))

View(hi_map)


maui_towns <- data.frame(
  town = c("Wailuku",
           "Lahaina",
           "Kihei",
           "Kahului", 
           "Kula" 
           # 
  ),
  lat  = c(20.891483, 
           20.8743, 
           20.764427,
           20.87, 
           20.77
           # 20.92
           
  ),
  lon  = c(-156.510284, 
           -156.6766,
           -156.445007,
           -156.46,
           - 156.33
           # -156.31
  )
)
View(maui_towns)


data3 <- data1 %>% select(
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
  addcity
) 
# %>% mutate(
#   addcity = case_when(addcity == "Other" ~ "Kula",
#                     TRUE ~ addcity))
table(data3$addcity, useNA = "always")


df_town <- data3 %>%
  mutate(addcity = as.character(addcity)) %>%
  group_by(addcity) %>%
  summarise(
    mean_li_city = mean(li_clean, na.rm = TRUE),
    mean_mg_city = mean(mg_clean, na.rm = TRUE),
    mean_ca_city = mean(ca_clean, na.rm = TRUE),
    mean_v_city = mean(v_clean, na.rm = TRUE),
    mean_cr_city = mean(cr_clean, na.rm = TRUE),
    mean_mn_city = mean(mn_clean, na.rm = TRUE),
    mean_fe_city = mean(fe_clean, na.rm = TRUE),
    mean_co_city = mean(co_clean, na.rm = TRUE),
    mean_ni_city = mean(ni_clean, na.rm = TRUE),
    mean_cu_city = mean(cu_clean, na.rm = TRUE),
    mean_zn_city = mean(zn_clean, na.rm = TRUE),
    mean_as_city = mean(as_clean, na.rm = TRUE),
    mean_br_city = mean(br_clean, na.rm = TRUE),
    mean_se_city = mean(se_clean, na.rm = TRUE),
    mean_sr_city = mean(sr_clean, na.rm = TRUE),
    mean_mo_city  = mean(mo_clean, na.rm = TRUE),
    mean_cd_city = mean(cd_clean, na.rm = TRUE),
    mean_sn_city = mean(sn_clean, na.rm = TRUE),
    mean_sb_city = mean(sb_clean, na.rm = TRUE),
    mean_cs_city = mean(cs_clean, na.rm = TRUE),
    mean_ba_city = mean(ba_clean, na.rm = TRUE),
    mean_w_city = mean(w_clean, na.rm = TRUE),
    mean_tl_city = mean(tl_clean, na.rm = TRUE),
    mean_pb_city = mean(pb_clean, na.rm = TRUE) )
# mean_fvc_city = mean(fvc_percent_nhanes, na.rm = TRUE),
# mean_fev1_city = mean(fev1_percent_nhanes, na.rm = TRUE),
# mean_fef_city = mean(fef2575_percent_nhanes, na.rm = TRUE),
# mean_ff_city  = mean(fev1_fvcratio, na.rm = TRUE))

View(df_town)
table(df_town$addcity)
table(maui_towns$town)

maui_towns <- maui_towns %>% 
  left_join(df_town,
            by = c("town" = "addcity")
  )
View(maui_towns)


axis_theme <- theme(
  axis.text.x  = element_text(size = 12, angle = 45, hjust = 1),
  axis.text.y  = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12)
)

## barium ##
p_ba <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_ba), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Barium",
                       limits = c(0,13),
                       breaks = seq(1, 13, by=5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_ba_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Barium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))

p_ba <- p_ba + axis_theme

ggsave("geo_ba.png",
       plot = p_ba,
       width = 12,
       height = 10,
       dpi = 300)

## cadium ##

p_cd <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_cd), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Cadmium",
                       limits = c(0,1.5),
                       breaks = seq(0, 1.5, by=0.5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_cd_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Cadmium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_cd <- p_cd + axis_theme

ggsave("geo_cd.png",
       plot = p_cd,
       width = 12,
       height = 10,
       dpi = 300)

## asenic ##

p_as <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_as), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Arsenic",
                       limits = c(0,70),
                       breaks = seq(0, 70, by=10),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_as_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Arsenic (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_as <- p_as +  axis_theme

ggsave("geo_as.png",
       plot = p_as,
       width = 12,
       height = 10,
       dpi = 300)

## lead ##

p_pb <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_pb), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Lead",
                       limits = c(0,2),
                       breaks = seq(0, 2, by=0.5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_pb_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Lead (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_pb <- p_pb + axis_theme

ggsave("geo_pb.png",
       plot = p_pb,
       width = 12,
       height = 10,
       dpi = 300)


## antimony ##

p_sb <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_sb), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Antimony",
                       limits = c(0,4),
                       breaks = seq(0, 4, by=1),
                       guide = guide_colorbar(
                         barheight= unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_sb_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Antimony (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_sb <- p_sb + axis_theme

ggsave("geo_sb.png",
       plot = p_sb,
       width = 12,
       height = 10,
       dpi = 300)


## cobalt ##
summary(df_zip$mean_co)
table(df_zip$mean_co)

p_co <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_co), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Cobalt",
                       limits = c(0,2),
                       breaks = seq(0, 2, by=0.5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_co_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Cobalt (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_co <-p_co + axis_theme

ggsave("geo_co.png",
       plot = p_co,
       width = 12,
       height = 10,
       dpi = 300)


## cesium ##
p_cs <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_cs), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Cesium",
                       limits = c(0,15),
                       breaks = seq(0, 15, by=5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_cs_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Cesium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_cs <- p_cs + axis_theme

ggsave("geo_cs.png",
       plot = p_cs,
       width = 12,
       height = 10,
       dpi = 300)




## manganese##
summary(df_zip$mean_mn)
table(df_zip$mean_mn)

p_mn <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_mn), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Manganese",
                       limits = c(0,2),
                       breaks = seq(0, 2, by=0.5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_mn_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Manganese (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_mn <- p_mn + axis_theme

ggsave("geo_mn.png",
       plot = p_mn,
       width = 12,
       height = 10,
       dpi = 300)

## mo ## 
table(df_zip$mean_mo)
summary(df_zip$mean_mo)

p_mo <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_mo), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Molybdenum",
                       limits = c(0,150),
                       breaks = seq(0, 150, by=50),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_mo_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Molybdenum (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_mo <- p_mo + axis_theme

ggsave("geo_mo.png",
       plot = p_mo,
       width = 12,
       height = 10,
       dpi = 300)

## strontium (sr)
table(df_zip$mean_sr, useNA = "always")
summary(df_zip$mean_sr)

p_sr <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_sr), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Strontium",
                       limits = c(0,250),
                       breaks = seq(0, 250, by=50),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_sr_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Strontium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_sr <- p_sr + axis_theme

ggsave("geo_sr.png",
       plot = p_sr,
       width = 12,
       height = 10,
       dpi = 300)


## tin (sn) ##
table(df_zip$mean_sn)
summary(df_zip$mean_sn)


p_sn <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_sn), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Tin",
                       limits = c(0,4),
                       breaks = seq(0, 4, by=2),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_sn_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Tin (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_sn <- p_sn + axis_theme

ggsave("geo_sn.png",
       plot = p_sn,
       width = 12,
       height = 10,
       dpi = 300)



## tungsten (w) ##
table(df_zip$mean_w)
summary(df_zip$mean_w)


p_w <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_w), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Tungsten",
                       limits = c(0,1),
                       breaks = seq(0, 1, by=0.2),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_w_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Tungsten (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_w <- p_w + axis_theme

ggsave("geo_w.png",
       plot = p_w,
       width = 12,
       height = 10,
       dpi = 300)


## vanadium (v) ##
table(df_zip$mean_v)
summary(df_zip$mean_v)


p_v <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_v), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Vanadium",
                       limits = c(0,1),
                       breaks = seq(0, 1, by=0.2),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_v_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Vanadium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_v <- p_v + axis_theme

ggsave("geo_v.png",
       plot = p_v,
       width = 12,
       height = 10,
       dpi = 300)

## copper (cu) ##
table(df_zip$mean_cu)
summary(df_zip$mean_cu)


p_cu <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_cu), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Copper",
                       limits = c(0,35),
                       breaks = seq(0, 35, by=5),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_cu_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Copper (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_cu <- p_cu + axis_theme

ggsave("geo_cu.png",
       plot = p_cu,
       width = 12,
       height = 10,
       dpi = 300)


## calcium (ca) ##
table(df_zip$mean_ca)
summary(df_zip$mean_ca)


p_ca <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_ca), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Calcium",
                       limits = c(20000,220000),
                       breaks = seq(20000, 220000, by= 50000),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_ca_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Calcium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_ca <- p_ca + axis_theme

ggsave("geo_ca.png",
       plot = p_ca,
       width = 12,
       height = 10,
       dpi = 300)

## nickel (ni) ##
table(df_zip$mean_ni)
summary(df_zip$mean_ni)


p_ni <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_ni), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Nickel",
                       limits = c(0,5),
                       breaks = seq(0, 5, by= 1),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_ni_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Nickel (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_ni <- p_ni + axis_theme

ggsave("geo_ni.png",
       plot = p_ni,
       width = 12,
       height = 10,
       dpi = 300)

## thallium (tl) ##
table(df_zip$mean_tl)
summary(df_zip$mean_tl)


p_tl <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_tl), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Thallium",
                       limits = c(0,0.5),
                       breaks = seq(0, 0.5, by= 0.1),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_tl_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Thallium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_tl <- p_tl + axis_theme

ggsave("geo_tl.png",
       plot = p_tl,
       width = 12,
       height = 10,
       dpi = 300)

## iron (fe) ##
table(df_zip$mean_fe)
summary(df_zip$mean_fe)


p_fe <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_fe), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Iron",
                       limits = c(0,50),
                       breaks = seq(0, 50, by= 10),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_fe_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Iron (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_fe <- p_fe + axis_theme

ggsave("geo_fe.png",
       plot = p_fe,
       width = 12,
       height = 10,
       dpi = 300)

## selenium (se) ##
table(df_zip$mean_se)
summary(df_zip$mean_se)


p_se <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_se), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Selenium",
                       limits = c(10,70),
                       breaks = seq(10, 70, by= 10),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_se_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Selenium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_se <- p_se + axis_theme

ggsave("geo_se.png",
       plot = p_se,
       width = 12,
       height = 10,
       dpi = 300)


## lithium (li) ##
table(df_zip$mean_li)
summary(df_zip$mean_li)


p_li <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_li), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Lithium",
                       limits = c(0,80),
                       breaks = seq(0, 80, by= 10),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_li_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Lithium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_li <- p_li+ axis_theme

ggsave("geo_li.png",
       plot = p_li,
       width = 12,
       height = 10,
       dpi = 300)

##  magnesium (mg) ##
table(df_zip$mean_mg)
summary(df_zip$mean_mg)


p_mg <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_mg), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Magnesium",
                       limits = c(15000,150000),
                       breaks = seq(15000, 150000, by= 20000),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_mg_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Magnesium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_mg <- p_mg + axis_theme

ggsave("geo_mg.png",
       plot = p_mg,
       width = 12,
       height = 10,
       dpi = 300)

##  Chromium(cr) ##
table(df_zip$mean_cr)
summary(df_zip$mean_cr)


p_cr <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_cr), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Chromium",
                       limits = c(0,4),
                       breaks = seq(0, 4, by= 1),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_cr_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Chromium (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_cr <- p_cr + axis_theme

ggsave("geo_cr.png",
       plot = p_cr,
       width = 12,
       height = 10,
       dpi = 300)

##  Zinc (zn) ##
table(df_zip$mean_zn)
summary(df_zip$mean_zn)


p_zn <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_zn), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Zinc",
                       limits = c(200,1600),
                       breaks = seq(200, 1600, by= 200),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_zn_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Zinc (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_zn <- p_zn + axis_theme

ggsave("geo_zn.png",
       plot = p_zn,
       width = 12,
       height = 10,
       dpi = 300)


##  Bromine (br) ##
table(df_zip$mean_br)
summary(df_zip$mean_br)


p_br <- ggplot() +
  geom_sf(data = hi_map,
          aes(fill = mean_br), 
          color = "black", 
          size = 0.2,
          na.rm = FALSE) +
  geom_sf(data = hi_counties,
          fill = NA, 
          color = "black",
          size =0.5)+
  scale_fill_viridis_c(option = "plasma", 
                       direction = -1,
                       na.value = "grey80", 
                       name = "Bromine",
                       limits = c(1000,7000),
                       breaks = seq(1000, 7000, by= 1000),
                       guide = guide_colorbar(
                         barheight = unit(5,"cm"),
                         frame.colour = "black",
                         frame.linewidth = 0.1)
  ) +
  geom_point(
    data = maui_towns,
    aes(x = lon,
        y = lat),
    color = "black",
    size = 2
  ) +
  geom_text_repel(
    data = maui_towns,
    aes(x = lon,
        y = lat,
        label = paste0(town , "\n", round(mean_br_city,2))),
    color = "black",
    size = 4.5,
    nudge_y = 0.03,      # push labels slightly upward
    segment.color = "grey50", # draw leader lines from label to dot
    min.segment.length = 0   # always show connector
  ) + 
  labs(
    title = "Bromine (µg/L)",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_sf(
    xlim = c(-156.7187, -155.9),
    ylim = c(20.48, 21.1),
    expand = FALSE
  )+
  theme_bw(base_size = 20)  +
  theme(legend.position = "right",
        legend.title = element_text(size = 14),
        legend.text  = element_text(size = 12))
p_br <- p_br + axis_theme

ggsave("geo_br.png",
       plot = p_br,
       width = 12,
       height = 10,
       dpi = 300)



library(patchwork)
plot_com <- (p_as|p_sb |p_cd | p_cu) / ( p_co | p_pb | p_ni | p_ca ) / (p_v | p_tl | p_w | p_sr) / ( p_ba | p_cs | p_fe | p_se) / (p_li | p_mg | p_cr| p_mn) / (p_zn |p_br |p_mo | p_sn)



# +
#   plot_layout(guides = "collect") & theme(legend.position = "bottom")

# plot_com

plot_com <- plot_com + plot_annotation(
  title = "Average metals concentration by town and zipcode",
  theme = theme(
    plot.title = element_text(size = 21, face = "bold"),
    plot.subtitle = element_text(size = 12 )
  ))
# plot_com

ggsave("metals_town_new_05212026.png",
       plot = plot_com,
       width = 28,
       height = 30,
       bg = "white",
       dpi = 300)



plot_com_short <- (p_as|p_sb |p_cd | p_cu) / ( p_co | p_pb | p_ni | p_ca ) / (p_v | p_tl | p_w | p_sr) / ( p_ba | p_cs | p_fe | p_se) / (p_li | p_mg | p_cr| p_mn) / (p_zn |p_br |p_mo | p_sn)



library(patchwork)
plot_com1 <- (p_as | p_sb)/ 
  (p_cd | p_cu)/ 
  (p_pb | p_ni) /
  (p_v | p_mg) 
# # plot_annotation(
# # title = "Percentage of abnormal pulmonary",
# theme = theme(
#   plot.title = element_text(size = 21, face = "bold"),
#   plot.subtitle = element_text(size = 12 )
# ))
# plot_com
ggsave("metals_short.png",
       plot = plot_com1,
       width = 14,
       height = 18,
       bg = "white",
       dpi = 300)





























