library(dplyr)
library(tidyr)
library(pheatmap)
library(tidyverse)
library(ggplotify)
library(heatmaply)
library(plotly)
library(ggplotify)
library(magick)


#### heat map ####

data <- HM_inMaui_clean

table(data$location, useNA = "always")

data <- data %>% mutate(
  location = case_when(
    location == "Wailuku/ Kahului" ~ "Wailuku/Kahului",
    TRUE ~ location))

data_metals <- data %>% select(
  location,
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
  pb_ln) 

df_median <- data_metals %>%
  group_by(location) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
View(df_median)
colnames(df_median)

data_metals_map <- df_median %>% select(
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
  pb_ln) %>%
  rename(
    "Lithium" = "li_ln",
    "Magnesium" = "mg_ln",
    "Calcium" = "ca_ln",
    "Vanadium" = "v_ln",
    "Chromium" = "cr_ln",
    "Manganese" = "mn_ln",
    "Iron" = "fe_ln",
    "Cobalt" = "co_ln",
    "Nickel" = "ni_ln",
    "Copper" = "cu_ln",
    "Zinc" = "zn_ln",
    "Arsenic" = "as_ln",
    "Bromine" = "br_ln",
    "Selenium" = "se_ln",
    "Strontium" = "sr_ln",
    "Molybdenum" = "mo_ln",
    "Cadmium" = "cd_ln",
    "Tin" = "sn_ln",
    "Antimony" = "sb_ln",
    "Cesium" = "cs_ln",
    "Barium" = "ba_ln",
    "Tunsten" = "w_ln",
    "Thallium" = "tl_ln",
    "Lead" = "pb_ln" ) 

dim(data_metals)
table(data_metals$location, useNA = "always")


data_metals_map_zscore <- data_metals_map %>%
  mutate(across(everything(), ~ scale(.)[, 1]))
dim(data_metals_map_zscore)


## choose group label column in a dataframe ##
df_treat <- df_median %>% select(
  location
)

dim(df_treat)
table(df_treat$location, useNA = "always")


#### transposed the map ####

data_metals_trans <- as.data.frame(t(data_metals_map_zscore))
colnames(data_metals_trans ) <- rownames(data_metals_map_zscore)   # samples become column names
rownames(data_metals_trans ) <- colnames(data_metals_map_zscore)

# View(data_metals_trans)
class(data_metals_trans)

names(data_metals_trans)
rownames(data_metals_trans) <- toupper(rownames(data_metals_trans))

check <- sapply(data_metals_trans, is.numeric)
table(check)
dim(data_metals_trans)


any(is.na(data_metals_trans)) # check if there is NA values
any(is.infinite(as.matrix(data_metals_trans))) # check t if there is an infinite values 
any(is.nan(as.matrix(data_metals_trans))) # check if there are na values 

data_metals_trans <- data_metals_trans[, colSums(is.na(data_metals_trans)) == 0]

p1 <-as.ggplot(pheatmap(data_metals_trans,
                        scale="none",
                        show_colnames = FALSE,
                        cellheight=40,
                        color=colorRampPalette(c("white", "brown"))(50)))
p1

dim(df_treat)
names(df_treat)


df_treat$location <- factor(df_treat$location)

df_treat <- df_treat%>% rename(
  "Locations" =  "location"
)
table(df_treat$Locations, useNA = "always")
df_treat <- as.data.frame(df_treat)
View(df_treat)

annotation_colors_list <- list(
  Locations = c("Wailuku/Kahului" = "green",
                "Lahaina" = "red",
                "Kula" = "orange",
                "Kihei" = "blue"
  ))

annotation_colors_list$Locations

dim(data_metals_trans)
dim(df_treat)

data_metals_trans <- as.data.frame(data_metals_trans)

# Remove non-numeric columns (like Location)
data_metals_trans <- data_metals_trans %>%
  dplyr::select(where(is.numeric))

# Convert to matrix
data_metals_trans <- as.matrix(data_metals_trans)

# Double-check numeric
str(data_metals_trans)   # should show num [1:x, 1:y]
sapply(data_metals_trans, class)
str(data_metals_trans)

class(df_treat)

p <- pheatmap(data_metals_trans,
              scale="none",
              show_colnames = FALSE,
              annotation_col = df_treat,
              cellwidth=70,
              cellheight=20,
              fontsize=11,
              annotation_colors = annotation_colors_list, 
              color = colorRampPalette(c("white", "brown"))(50),
              silent = TRUE )
p


p2 <- as.ggplot(p)
p2 <- p2 + theme(plot.margin = margin(0,0,0,0),
                 legend.position = "bottom")
p2

ggsave("heatmap_metals_location_check.png",
       plot =p2,
       width = 8,
       height = 10,
       bg = "white",
       limitsize = FALSE,
       dpi = 300)

#### radar graph by locations ####

data <- HM_inMaui_clean
dim(data)
table(data$location, useNA = "always")

data <- data %>% mutate(
  location = case_when(
    location == "Wailuku/ Kahului" ~ "Wailuku/Kahului",
    TRUE ~ location))
table(data$location, useNA = "always")

data2 <- data %>% select(
  location,
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
  pb_ln )

View(data2)

data_med <- data2 %>%
  group_by(location) %>%
  summarise(across(where(is.numeric), median, na.rm = TRUE))
View(data_med)

names(data_med)

data_med <- data_med %>% rename(
  "Li" = "li_ln",
  "Mg" = "mg_ln",
  "Ca" = "ca_ln",
  "V" = "v_ln",
  "Cr" = "cr_ln",
  "Mn" = "mn_ln",
  "Fe" = "fe_ln",
  "Co" = "co_ln",
  "Ni" = "ni_ln",
  "Cu" = "cu_ln",
  "Zn" = "zn_ln",
  "As" = "as_ln",
  "Br" = "br_ln",
  "Se" = "se_ln",
  "Sr" = "sr_ln",
  "Mo" = "mo_ln",
  "Cd" = "cd_ln",
  "Sn" = "sn_ln",
  "Sb" = "sb_ln",
  "Cs" = "cs_ln",
  "Ba" = "ba_ln",
  "W" = "w_ln",
  "Tl" = "tl_ln",
  "Pb" = "pb_ln"
)

View(data_med)
colnames(data_med)
data_med <- data_med[, -1]

data_med_zscore <- data_med %>%
  mutate(across(everything(), ~ scale(.)[, 1]))
dim(data_med_zscore)

radar_data <- rbind(
  rep(2, ncol(data_med_zscore)),
  rep(-2, ncol(data_med_zscore)),
  data_med_zscore[,])

View(radar_data)

colors_border <- viridis(4, alpha = 1)
colors_in <- viridis(4, alpha = 0.3)

mytitle <- c("Wailuku/Kahului", 
             "Kihei",
             "Kula",
             "Lahaina")


png("radar_HM_location.png", 
    width = 2000,
    height = 1600,
    res = 300)

par(mar=rep(0.8,4),oma=c(0, 0, 2.5, 0))
par(mfrow=c(2,2))

# Loop for each plot

for(i in 1:4){
  radarchart(radar_data[c(1,2,i+2),],
             axistype=1,
             pcol=colors_border[i] ,
             pfcol=colors_in[i] ,
             plwd=1.5, 
             plty=1 ,
             pty = 20,
             pcex = 0.05,
             cglcol="grey",
             cglty=1,
             axislabcol="grey",
             caxislabels=seq(-2,2,1),
             cglwd=0.3,
             vlcex=0.8,
             title=mytitle[i])
}
mtext("", 
      side = 3, 
      outer = TRUE, 
      line = 1.5, 
      adj = 0, 
      cex = 0.8,
      font = 2)
dev.off()


#### radar graph by locations overlayed  ####

create_beautiful_radarchart <- function(data,
                                        color = "#00AFBB",
                                        # label = NULL,
                                        vlabels = colnames(data),
                                        vlcex = 1.2,
                                        caxislabels = NULL,
                                        title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

rownames(radar_data) <- c("Max", "Min", "Kihei", "Kula", "Lahaina", "Wailuku/Kahului")
View(radar_data)
legend_labels <- c("Kihei", "Kula", "Lahaina", "Wailuku/Kahului")


png("radar_HM_location_overlayed_check.png", 
    width = 2000,
    height = 1700,
    res = 300)

op <- par(mar = c(1, 2, 2, 2))
create_beautiful_radarchart(
  data = radar_data, 
  caxislabels=seq(-2,2,1),
  color = c("#702963",
            "#35638A", 
            "#00A36C",
            "#F2D74C"))
legend(
  x = "bottom",
  legend = legend_labels,
  horiz = TRUE,
  bty = "n",
  inset = c(0, -0.15),
  pch = 16 ,
  col = c("#CFC5D3",
          "#35638A", 
          "#5DBB7F",
          "#F2D74C"),
  text.col = "black",
  cex = 1, 
  pt.cex = 2
)
par(op)
dev.off()

imgA <- image_read("radar_HM_location.png")
imgB <- image_read("radar_HM_location_overlayed_check.png")

combined <- image_append(c(imgA, imgB), stack = TRUE) # stack vertically

image_write(combined, "combined_rada_heatmap.png") # save

imgC <- image_read("combined_rada_heatmap.png")
imgD <- image_read("heatmap_metals_location_check.png")
combined_1 <- image_append(c(imgC, imgD), stack = FALSE)

image_write(combined_1, "FigS2_rada_heatmap.png")







