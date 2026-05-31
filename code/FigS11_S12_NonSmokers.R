library(forestplot)
library(dplyr)
library(ggplot2)
library(tidyr)
library(qgcomp)
library(gWQS)

#### NON-SMOKERS in dual-WQS model ####

data <- HM_lung_demo_nomiss_111525_use
dim(data)
table(data$tobaco, useNA = "always")

data <- data %>% rename(
  "male"= "male_new", 
 "age" = "age_new"
)

data1 <- subset(data, data$tobaco ==0)
dim(data1)
## A, B, C Sessions ##
data_fvc <- subset(data1, data1$fvc_quality %in% c("A", "B", "C"))
dim(data_fvc)
dim(data1)
data_fev <- subset(data1, data1$fev1_quality %in% c("A", "B", "C"))
dim(data_fev)
data_ff <- subset(data1, data1$fev1_quality %in% c("A", "B", "C") &
                    data1$fvc_quality %in% c("A", "B", "C"))
dim(data_ff)


#### fvc < LLN ####

names(data)

metals <- c(
  "as_ln",
  "ca_ln",
  "cd_ln",
  "cu_ln",
  "pb_ln",
  "sb_ln")

results2i_l90 <- gwqs(fvcbelowlln_N ~ pwqs + nwqs + age + male + BMIscore,
                      mix_name = metals,
                      data = data_fvc, 
                      q = 4, 
                      validation = 0.6, 
                      b = 500, 
                      b1_pos = TRUE,
                      rh = 5,
                      family = binomial(), 
                      seed =123,
                      lambda = 90)

summary(results2i_l90 )


## select lambda with the lowest AIC value ##
coef_mat <- summary(results2i_l90)$coefficients
coef_mat

est  <- coef_mat[, "Estimate"]
se   <- coef_mat[, "Std. Error"]
OR   <- exp(est)
lower_CI <- exp(coef_mat[, "2.5 %"])
upper_CI <- exp(coef_mat[, "97.5 %"])
z_value <- est / se
p_value <- 2 * (1 - pnorm(abs(z_value)))

results_table <- data.frame(
  term = rownames(coef_mat),
  estimate = est,
  OR = OR,
  CI_lower = lower_CI,
  CI_upper = upper_CI,
  z_value = z_value,
  p_value = p_value
)

results_table

df_fvc_2iwqs <- results_table %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_lower, 2),
    CI_high = round(CI_upper, 2),
    p = round(p_value, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fvc_2iwqs


df_fvc_2iwqs <- df_fvc_2iwqs[-c(1,4:7),]

df_fvc_2iwqs$term

df_fvc_2iwqs$term[1] <- "Pos-WQS"
df_fvc_2iwqs$term[2] <- "Neg-WQS"


df_fvc_2iwqs$term <- ifelse(df_fvc_2iwqs$p <= 0.001,
                            paste0(df_fvc_2iwqs$term, "***"),
                            ifelse(df_fvc_2iwqs$p > 0.001 & df_fvc_2iwqs$p <= 0.01,
                                   paste0(df_fvc_2iwqs$term, "**"),
                                   ifelse(df_fvc_2iwqs$p > 0.01 & df_fvc_2iwqs$p <= 0.05,
                                          paste0(df_fvc_2iwqs$term, "*"),
                                          ifelse(df_fvc_2iwqs$p > 0.05 & df_fvc_2iwqs$p <= 0.1,
                                                 paste0(df_fvc_2iwqs$term, "+"),
                                                 paste0(df_fvc_2iwqs$term)))))

colnames(df_fvc_2iwqs)
df_fvc_2iwqs

df_fvc_2iwqs_chuan <- df_fvc_2iwqs[ , c(1, 2, 3, 4)]
df_fvc_2iwqs_chuan

plot_data <- with(df_fvc_2iwqs_chuan,
                  data.frame(
                    mean = OR,
                    lower =  CI_low ,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("2iwqs_fvc.png", 
    width = 7,
    height = 3,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FVC < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()

#### fvc weights ####

weight_fvc <- results2i_l90$final_weights
weight_fvc

## get positive weights 
pos_weights <- data.frame(
  metal = weight_fvc$mix_name,
  weight = weight_fvc$`Estimate pos`
)

pos_weights


## get negative weights 
neg_weights <- data.frame(
  metal = weight_fvc$mix_name,
  weight = weight_fvc$`Estimate neg`
)

neg_weights

## combining pos and neg weights 
weight_long <- weight_fvc %>%
  rename(
    Positive = `Estimate pos`,
    Negative = `Estimate neg`
  ) %>%
  pivot_longer(
    cols = c("Positive", "Negative"),
    names_to = "direction",
    values_to = "weight"
  )

weight_long
weight_long <- as.data.frame(weight_long)
View(weight_long)
names(weight_long)

weight_long  <- weight_long %>% mutate(
  mix_name = case_when(mix_name == "as_ln" ~ "Arsenic",
                       mix_name== "ca_ln" ~ "Calcium",
                       mix_name == "cd_ln" ~ "Cadmium",
                       mix_name == "cu_ln" ~ "Copper",
                       mix_name == "pb_ln" ~ "Lead",
                       mix_name == "sb_ln" ~ "Antimony"))


weight_long_fvc <- weight_long %>% select(
  mix_name,
  direction,
  weight
) %>% 
  rename(
    term = mix_name,
    weight = weight,
    direction = direction
  )
View(weight_long_fvc)

custom_order <- c("Calcium", 
                  "Lead",
                  "Copper", 
                  "Cadmium", 
                  "Antimony", 
                  "Arsenic")

weight_long_fvc$term <- factor(weight_long_fvc$term,
                               levels = custom_order)

names(weight_long_fvc)
weight_long_fvc$weight = ifelse(weight_long_fvc$direction == "Positive",
                                weight_long_fvc$weight,
                                -weight_long_fvc$weight)    # make negative weights truly negative

library(ggplot2)
p_fvc_2iwqs <- ggplot(weight_long_fvc,
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
    title = expression(bold(paste("FVC < LLN")))
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(weight_long_fvc$weight > 0, -0.1, 1.1),
            size = 4)
p_fvc_2iwqs



#### fev1 < LLN ####

names(data)
dim(data_fev)

fev2i_l90 <- gwqs(fev1belowlln_N ~ pwqs + nwqs + age + male +  BMIscore,
                  mix_name = metals,
                  data = data_fev, 
                  q = 4, 
                  validation = 0.6, 
                  b = 500, 
                  b1_pos = TRUE,
                  rh = 5,
                  family = binomial(), 
                  seed =123,
                  lambda = 90)

summary(fev2i_l90)

coef_mat <- summary(fev2i_l90)$coefficients
coef_mat

est  <- coef_mat[, "Estimate"]
se   <- coef_mat[, "Std. Error"]
OR   <- exp(est)
lower_CI <- exp(coef_mat[, "2.5 %"])
upper_CI <- exp(coef_mat[, "97.5 %"])
z_value <- est / se
p_value <- 2 * (1 - pnorm(abs(z_value)))

results_table <- data.frame(
  term = rownames(coef_mat),
  estimate = est,
  OR = OR,
  CI_lower = lower_CI,
  CI_upper = upper_CI,
  z_value = z_value,
  p_value = p_value
)

results_table

df_fev_2iwqs <- results_table %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_lower, 2),
    CI_high = round(CI_upper, 2),
    p = round(p_value, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_2iwqs


df_fev_2iwqs <- df_fev_2iwqs[-c(1,4:7),]

df_fev_2iwqs$term

df_fev_2iwqs$term[1] <- "Pos-WQS"
df_fev_2iwqs$term[2] <- "Neg-WQS"


df_fev_2iwqs$term <- ifelse(df_fev_2iwqs$p <= 0.001,
                            paste0(df_fev_2iwqs$term, "***"),
                            ifelse(df_fev_2iwqs$p > 0.001 & df_fev_2iwqs$p <= 0.01,
                                   paste0(df_fev_2iwqs$term, "**"),
                                   ifelse(df_fev_2iwqs$p > 0.01 & df_fev_2iwqs$p <= 0.05,
                                          paste0(df_fev_2iwqs$term, "*"),
                                          ifelse(df_fev_2iwqs$p > 0.05 & df_fev_2iwqs$p <= 0.1,
                                                 paste0(df_fev_2iwqs$term, "+"),
                                                 paste0(df_fev_2iwqs$term)))))

colnames(df_fev_2iwqs)
df_fev_2iwqs

df_fev_2iwqs_chuan <- df_fev_2iwqs[ , c(1, 2, 3, 4)]
df_fev_2iwqs_chuan

plot_data <- with(df_fev_2iwqs_chuan,
                  data.frame(
                    mean = OR,
                    lower =  CI_low ,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("2iwqs_fev.png", 
    width = 7,
    height = 3,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FEV"[1], " < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


#### fev1 weights ####
weight_fev1 <- fev2i_l90$final_weights
weight_fev1

## get positive weights 
pos_weights <- data.frame(
  metal = weight_fev1$mix_name,
  weight = weight_fev1$`Estimate pos`
)

pos_weights


## get negative weights 
neg_weights <- data.frame(
  metal = weight_fev1$mix_name,
  weight = weight_fev1$`Estimate neg`
)

neg_weights

## combining pos and neg weights 
weight_long <- weight_fev1 %>%
  rename(
    Positive = `Estimate pos`,
    Negative = `Estimate neg`
  ) %>%
  pivot_longer(
    cols = c("Positive", "Negative"),
    names_to = "direction",
    values_to = "weight"
  )

weight_long
weight_long <- as.data.frame(weight_long)
View(weight_long)
names(weight_long)

weight_long  <- weight_long %>% mutate(
  mix_name = case_when(mix_name == "as_ln" ~ "Arsenic",
                       mix_name== "ca_ln" ~ "Calcium",
                       mix_name == "cd_ln" ~ "Cadmium",
                       mix_name == "cu_ln" ~ "Copper",
                       mix_name == "pb_ln" ~ "Lead",
                       mix_name == "sb_ln" ~ "Antimony"))


weight_long_fev1 <- weight_long %>% select(
  mix_name,
  direction,
  weight
) %>% 
  rename(
    term = mix_name,
    weight = weight,
    direction = direction
  )
View(weight_long_fev1 )

custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

weight_long_fev1$term <- factor(weight_long_fev1$term,
                                levels = custom_order)

names(weight_long_fev1)
weight_long_fev1$weight = ifelse(weight_long_fev1$direction == "Positive",
                                 weight_long_fev1$weight,
                                 -weight_long_fev1$weight)    # make negative weights truly negative


p_fev_2iwqs <- ggplot(weight_long_fev1,
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
    title = expression(bold(paste("FEV"[1], " < LLN")))) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(weight_long_fvc$weight > 0, -0.1, 1.1),
            size = 4)
p_fev_2iwqs

ggsave("p_fev_2iwqs.png",
       plot =p_fev_2iwqs,
       width = 12,
       height = 10,
       dpi = 300)

#### fev1/fvc < LLN ####
names(data)
dim(data)
dim(data_ff)

ff2i_l90 <- gwqs(ffbelowlln_N ~ pwqs + nwqs + age + male +  BMIscore,
                 mix_name = metals,
                 data = data_ff, 
                 q = 4, 
                 validation = 0.6, 
                 b = 500, 
                 b1_pos = TRUE,
                 rh = 5,
                 family = binomial(), 
                 seed =123,
                 lambda = 90)

summary(ff2i_l90)


coef_mat <- summary(ff2i_l90)$coefficients
coef_mat

est  <- coef_mat[, "Estimate"]
se   <- coef_mat[, "Std. Error"]
OR   <- exp(est)
lower_CI <- exp(coef_mat[, "2.5 %"])
upper_CI <- exp(coef_mat[, "97.5 %"])
z_value <- est / se
p_value <- 2 * (1 - pnorm(abs(z_value)))

results_table <- data.frame(
  term = rownames(coef_mat),
  estimate = est,
  OR = OR,
  CI_lower = lower_CI,
  CI_upper = upper_CI,
  z_value = z_value,
  p_value = p_value
)

results_table

df_ff_2iwqs <- results_table %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_lower, 2),
    CI_high = round(CI_upper, 2),
    p = round(p_value, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_ff_2iwqs


df_ff_2iwqs <- df_ff_2iwqs[-c(1,4:7),]

df_ff_2iwqs$term

df_ff_2iwqs$term[1] <- "Pos-WQS"
df_ff_2iwqs$term[2] <- "Neg-WQS"


df_ff_2iwqs$term <- ifelse(df_ff_2iwqs$p <= 0.001,
                           paste0(df_ff_2iwqs$term, "***"),
                           ifelse(df_ff_2iwqs$p > 0.001 & df_ff_2iwqs$p <= 0.01,
                                  paste0(df_ff_2iwqs$term, "**"),
                                  ifelse(df_ff_2iwqs$p > 0.01 & df_ff_2iwqs$p <= 0.05,
                                         paste0(df_ff_2iwqs$term, "*"),
                                         ifelse(df_ff_2iwqs$p > 0.05 & df_ff_2iwqs$p <= 0.1,
                                                paste0(df_ff_2iwqs$term, "+"),
                                                paste0(df_ff_2iwqs$term)))))

colnames(df_ff_2iwqs)
df_ff_2iwqs

df_ff_2iwqs_chuan <- df_ff_2iwqs[ , c(1, 2, 3, 4)]
df_ff_2iwqs_chuan

plot_data <- with(df_ff_2iwqs_chuan,
                  data.frame(
                    mean = OR,
                    lower =  CI_low ,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("2iwqs_ff.png", 
    width = 7,
    height = 3,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FEV"[1], "/FVC < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


#### fev1/fvc weights ####

weight_ff <- ff2i_l90$final_weights
weight_ff

## get positive weights 
pos_weights <- data.frame(
  metal = weight_ff$mix_name,
  weight = weight_ff$`Estimate pos`
)

pos_weights


## get negative weights 
neg_weights <- data.frame(
  metal = weight_ff$mix_name,
  weight = weight_ff$`Estimate neg`
)

neg_weights

## combining pos and neg weights 
weight_long <- weight_ff %>%
  rename(
    Positive = `Estimate pos`,
    Negative = `Estimate neg`
  ) %>%
  pivot_longer(
    cols = c("Positive", "Negative"),
    names_to = "direction",
    values_to = "weight"
  )

weight_long
weight_long <- as.data.frame(weight_long)
View(weight_long)
names(weight_long)

weight_long  <- weight_long %>% mutate(
  mix_name = case_when(mix_name == "as_ln" ~ "Arsenic",
                       mix_name== "ca_ln" ~ "Calcium",
                       mix_name == "cd_ln" ~ "Cadmium",
                       mix_name == "cu_ln" ~ "Copper",
                       mix_name == "pb_ln" ~ "Lead",
                       mix_name == "sb_ln" ~ "Antimony"))


weight_long_ff <- weight_long %>% select(
  mix_name,
  direction,
  weight
) %>% 
  rename(
    term = mix_name,
    weight = weight,
    direction = direction
  )
View(weight_long_ff)

custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

weight_long_ff$term <- factor(weight_long_ff$term,
                              levels = custom_order)

names(weight_long_ff)
weight_long_ff$weight = ifelse(weight_long_ff$direction == "Positive",
                               weight_long_ff$weight,
                               -weight_long_ff$weight)    # make negative weights truly negative


p_ff_2iwqs <- ggplot(weight_long_ff,
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
    title = expression(bold(paste("FEV"[1], "/FVC < LLN")))) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(weight_long_fvc$weight > 0, -0.1, 1.1),
            size = 4)
p_ff_2iwqs

ggsave("p_ff_2iwqs.png",
       plot =p_ff_2iwqs,
       width = 12,
       height = 10,
       dpi = 300)


#### fef2575 < LLN ####
dim(data_ff)
table(data_ff$tobaco)

fef2i_l90 <- gwqs(fef2575belowlln_N ~ pwqs + nwqs + age + male + BMIscore,
                  mix_name = metals,
                  data = data_ff, 
                  q = 4, 
                  validation = 0.6, 
                  b = 500, 
                  b1_pos = TRUE,
                  rh = 5,
                  family = binomial(), 
                  seed =123,
                  lambda = 90)

summary(fef2i_l90 )


coef_mat <- summary(fef2i_l90 )$coefficients
coef_mat

est  <- coef_mat[, "Estimate"]
se   <- coef_mat[, "Std. Error"]
OR   <- exp(est)
lower_CI <- exp(coef_mat[, "2.5 %"])
upper_CI <- exp(coef_mat[, "97.5 %"])
z_value <- est / se
p_value <- 2 * (1 - pnorm(abs(z_value)))

results_table <- data.frame(
  term = rownames(coef_mat),
  estimate = est,
  OR = OR,
  CI_lower = lower_CI,
  CI_upper = upper_CI,
  z_value = z_value,
  p_value = p_value
)

results_table

df_fef_2iwqs <- results_table %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_lower, 2),
    CI_high = round(CI_upper, 2),
    p = round(p_value, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fef_2iwqs


df_fef_2iwqs <- df_fef_2iwqs[-c(1,4:7),]

df_fef_2iwqs$term

df_fef_2iwqs$term[1] <- "Pos-WQS"
df_fef_2iwqs$term[2] <- "Neg-WQS"


df_fef_2iwqs$term <- ifelse(df_fef_2iwqs$p <= 0.001,
                            paste0(df_fef_2iwqs$term, "***"),
                            ifelse(df_fef_2iwqs$p > 0.001 & df_fef_2iwqs$p <= 0.01,
                                   paste0(df_fef_2iwqs$term, "**"),
                                   ifelse(df_fef_2iwqs$p > 0.01 & df_fef_2iwqs$p <= 0.05,
                                          paste0(df_fef_2iwqs$term, "*"),
                                          ifelse(df_fef_2iwqs$p > 0.05 & df_fef_2iwqs$p <= 0.1,
                                                 paste0(df_fef_2iwqs$term, "+"),
                                                 paste0(df_fef_2iwqs$term)))))

colnames(df_fef_2iwqs)
df_fef_2iwqs

df_fef_2iwqs_chuan <- df_fef_2iwqs[ , c(1, 2, 3, 4)]
df_fef_2iwqs_chuan

plot_data <- with(df_fef_2iwqs_chuan,
                  data.frame(
                    mean = OR,
                    lower =  CI_low ,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("2iwqs_fef.png", 
    width = 7,
    height = 3,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(FEF[25*"\u201375"] ~ "< LLN")),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


#### fev1/fvc weights ####
weight_fef <- fef2i_l90$final_weights
weight_fef

## get positive weights 
pos_weights <- data.frame(
  metal = weight_fef$mix_name,
  weight = weight_fef$`Estimate pos`
)

pos_weights


## get negative weights 
neg_weights <- data.frame(
  metal = weight_fef$mix_name,
  weight = weight_fef$`Estimate neg`
)

neg_weights

## combining pos and neg weights 
weight_long <- weight_fef %>%
  rename(
    Positive = `Estimate pos`,
    Negative = `Estimate neg`
  ) %>%
  pivot_longer(
    cols = c("Positive", "Negative"),
    names_to = "direction",
    values_to = "weight"
  )

weight_long
weight_long <- as.data.frame(weight_long)
View(weight_long)
names(weight_long)

weight_long  <- weight_long %>% mutate(
  mix_name = case_when(mix_name == "as_ln" ~ "Arsenic",
                       mix_name== "ca_ln" ~ "Calcium",
                       mix_name == "cd_ln" ~ "Cadmium",
                       mix_name == "cu_ln" ~ "Copper",
                       mix_name == "pb_ln" ~ "Lead",
                       mix_name == "sb_ln" ~ "Antimony"))


weight_long_fef <- weight_long %>% select(
  mix_name,
  direction,
  weight
) %>% 
  rename(
    term = mix_name,
    weight = weight,
    direction = direction
  )
View(weight_long_fef)

custom_order <- c("Calcium",
                  "Lead", 
                  "Copper", 
                  "Cadmium", 
                  "Antimony", 
                  "Arsenic")

weight_long_fef$term <- factor(weight_long_fef$term,
                               levels = custom_order)

names(weight_long_fef)
weight_long_fef$weight = ifelse(weight_long_fef$direction == "Positive",
                                weight_long_fef$weight,
                                -weight_long_fef$weight)    # make negative weights truly negative

p_fef_2iwqs <- ggplot(weight_long_fef,
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
    title = expression(bold(FEF[25*"\u201375"] ~ "< LLN"))) + 
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(weight_long_fvc$weight > 0, -0.1, 1.1),
            size = 4)
p_fef_2iwqs

ggsave("p_fef_2iwqs.png",
       plot =p_fef_2iwqs,
       width = 12,
       height = 10,
       dpi = 300)


#### combine weights graph ####
p_fvc_2iwqs
p_fev_2iwqs
p_ff_2iwqs
p_fef_2iwqs 
library(patchwork)
p_weight_2iwqs <- (p_fvc_2iwqs|p_fev_2iwqs)/ (p_ff_2iwqs |p_fef_2iwqs )


ggsave("2iwqs_weight_nosmoker.png",
       plot = p_weight_2iwqs,
       width = 16,
       height = 14,
       dpi = 300)


#### combine images ####
library(magick)
img1 <- image_read("2iwqs_fvc.png")
img2 <- image_read("2iwqs_fev.png")
img3 <- image_read("2iwqs_ff.png")
img4 <- image_read("2iwqs_fef.png")

row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))

plot_2iwqs <- image_append(c(row1, row2), 
                           stack = TRUE)

plot_2iwqs 

final_2iwqs <- image_annotate(
  plot_2iwqs,
  text = "",
  size = 80,
  color = "black",
  boxcolor = "white",   # background box for contrast
  gravity = "northwest",   # position at top
  weight = 700  # bold title text  
)

image_write(final_2iwqs, "final_2iwqss_nosmoker.png")


#### NON-SMOKERS in non-boot Q-gComp model ####

#### fvc < LLN ####
fit_fvc <- qgcomp.glm.noboot(
  f = fvcbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fvc_qcomp
data_fvc_qcomp <- data_fvc_qcomp[-1,]

data_fvc_qcomp$term
data_fvc_qcomp[1] <- "Q-gcomp"


data_fvc_qcomp$term <- ifelse(data_fvc_qcomp$p <= 0.001,
                              paste0(data_fvc_qcomp$term, "***"),
                              ifelse(data_fvc_qcomp$p > 0.001 & data_fvc_qcomp$p <= 0.01,
                                     paste0(data_fvc_qcomp$term, "**"),
                                     ifelse(data_fvc_qcomp$p > 0.01 & data_fvc_qcomp$p <= 0.05,
                                            paste0(data_fvc_qcomp$term, "*"),
                                            ifelse(data_fvc_qcomp$p > 0.05 & data_fvc_qcomp$p <= 0.1,
                                                   paste0(data_fvc_qcomp$term, "+"),
                                                   paste0(data_fvc_qcomp$term)))))

colnames(data_fvc_qcomp)
data_fvc_qcomp

data_fvc_qcomp_chuan <- data_fvc_qcomp[ , c(1, 2, 3, 4)]
data_fvc_qcomp_chuan 

plot_data <- with(data_fvc_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

png("qcom_fvc.png", 
    width = 7,
    height = 2,
    res = 300, 
    units = "in"
)

my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FVC < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


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
View(all_weights_df)
colnames(all_weights_df)

all_weights_df <- all_weights_df %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "pb_ln" ~ "Lead",
                   term == "sb_ln" ~ "Antimony"))



# 4. draw a graph

custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df$term <- factor(all_weights_df$term,
                              levels = custom_order)
View(all_weights_df)

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


#### fev1 < LLN ####
fit_fev <- qgcomp.glm.noboot(
  f = fev1belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fev,
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_qcomp
df_fev_qcomp <- df_fev_qcomp[-1,]


df_fev_qcomp$term
df_fev_qcomp[1] <- "Q-gcomp"


df_fev_qcomp$term <- ifelse(df_fev_qcomp$p <= 0.001,
                            paste0(df_fev_qcomp$term, "***"),
                            ifelse(df_fev_qcomp$p > 0.001 & df_fev_qcomp$p <= 0.01,
                                   paste0(df_fev_qcomp$term, "**"),
                                   ifelse(df_fev_qcomp$p > 0.01 & df_fev_qcomp$p <= 0.05,
                                          paste0(df_fev_qcomp$term, "*"),
                                          ifelse(df_fev_qcomp$p > 0.05 & df_fev_qcomp$p <= 0.1,
                                                 paste0(df_fev_qcomp$term, "+"),
                                                 paste0(df_fev_qcomp$term)))))

colnames(df_fev_qcomp)
df_fev_qcomp

df_fev_qcomp_chuan <- df_fev_qcomp[ , c(1, 2, 3, 4)]
df_fev_qcomp_chuan

plot_data <- with(df_fev_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

# View(label_text)


png("qcom_fev.png", 
    width = 7,
    height = 2,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FEV"[1], " < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


#### fev1 weights ####
pos_df <- data.frame(
  term = names(fit_fev$pos.weights),
  weight = fit_fev$pos.weights,
  direction = "Positive")
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

neg_df <- data.frame(
  term = names(fit_fev$neg.weights),
  weight = -fit_fev$neg.weights, 
  direction = "Negative"
)
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL

all_weights_df_fev <- rbind(pos_df, neg_df)
View(all_weights_df_fev)
colnames(all_weights_df_fev)

all_weights_df_fev <- all_weights_df_fev %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "pb_ln" ~ "Lead",
                   term == "sb_ln" ~ "Antimony"))

custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df_fev$term <- factor(all_weights_df_fev$term,
                                  levels = custom_order)
View(all_weights_df_fev)

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

#### fev1/fvc < LLN ####
fit_ff <- qgcomp.glm.noboot(
  f = ffbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_ff_qcomp
df_ff_qcomp <- df_ff_qcomp[-1,]

df_ff_qcomp$term

df_ff_qcomp[1] <- "Q-gcomp"


df_ff_qcomp$term <- ifelse(df_ff_qcomp$p <= 0.001,
                           paste0(df_ff_qcomp$term, "***"),
                           ifelse(df_ff_qcomp$p > 0.001 & df_ff_qcomp$p <= 0.01,
                                  paste0(df_ff_qcomp$term, "**"),
                                  ifelse(df_ff_qcomp$p > 0.01 & df_ff_qcomp$p <= 0.05,
                                         paste0(df_ff_qcomp$term, "*"),
                                         ifelse(df_ff_qcomp$p > 0.05 & df_ff_qcomp$p <= 0.1,
                                                paste0(df_ff_qcomp$term, "+"),
                                                paste0(df_ff_qcomp$term)))))

colnames(df_ff_qcomp)
df_ff_qcomp

df_ff_qcomp_chuan <- df_ff_qcomp[ , c(1, 2, 3, 4)]
df_ff_qcomp_chuan

plot_data <- with(df_ff_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

View(label_text)


png("qcom_ff.png", 
    width = 7,
    height = 2,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(paste("FEV"[1], "/FVC < LLN"))),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


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
View(all_weights_df_ff)
colnames(all_weights_df_ff)

all_weights_df_ff <- all_weights_df_ff %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "pb_ln" ~ "Lead",
                   term == "sb_ln" ~ "Antimony"))



# 4. draw a graph

custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df_ff$term <- factor(all_weights_df_ff$term,
                                 levels = custom_order)
View(all_weights_df_ff)

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

#### fef2575 < LLN ####

# with A, B, C sections in FEV1 #
data_fef <- subset(data, data$fev1_quality %in% c("A", "B", "C") &
                     data$fvc_quality %in% c("A", "B", "C"))
dim(data_fef)
dim(data)

fit_fef <- qgcomp.glm.noboot(
  f = fef2575belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fef_qcomp


df_fef_qcomp <- df_fef_qcomp[-1,]

df_fef_qcomp$term

df_fef_qcomp[1] <- "Q-gcomp"


df_fef_qcomp$term <- ifelse(df_fef_qcomp$p <= 0.001,
                            paste0(df_fef_qcomp$term, "***"),
                            ifelse(df_fef_qcomp$p > 0.001 & df_fef_qcomp$p <= 0.01,
                                   paste0(df_fef_qcomp$term, "**"),
                                   ifelse(df_fef_qcomp$p > 0.01 & df_fef_qcomp$p <= 0.05,
                                          paste0(df_fef_qcomp$term, "*"),
                                          ifelse(df_fef_qcomp$p > 0.05 & df_fef_qcomp$p <= 0.1,
                                                 paste0(df_fef_qcomp$term, "+"),
                                                 paste0(df_fef_qcomp$term)))))

colnames(df_fef_qcomp)
df_fef_qcomp

df_fef_qcomp_chuan <- df_fef_qcomp[ , c(1, 2, 3, 4)]
df_fef_qcomp_chuan

plot_data <- with(df_fef_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]"
)

plot_data$or <- paste0(round(plot_data$mean,2))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

# View(label_text)


png("qcom_fef.png", 
    width = 7,
    height = 2,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.1, 0.5, 1,2,5,10)
attr(my_ticks, "labels") <- c("0.1", "0.5", "1", "2", "5", "10") ## adjust not decimal or decimal numbers 

plot_data |>
  forestplot(labeltext = c(label, or, ci_text),
             zero = 1,
             boxsize = 0.15,
             line.margin = 0.05,
             ci.vertices = TRUE,
             ci.vertices.height = 0.1,
             fn.ci_norm = fpDrawNormalCI,
             lwd.ci = 1,
             lty.zero = 3, 
             lwd.xaxis = 1,
             xlab = "Odds Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(FEF[25*"\u201375"] ~ "< LLN")),
             txt_gp = fpTxtGp(
               title = gpar(fontface = "bold", cex = 1.2),  # title size
               label = list(
                 gpar(cex = 1.1),  # first column (Variable)
                 gpar(cex = 1.1)  # other columns (OR, CI)
               ),
               xlab = gpar(cex = 0.8),     # bigger x-axis title
               ticks = gpar(cex = 0.8)
             ),
             hrzl_lines = list("2" = gpar(lwd = 1.5, 
                                          col = "#000044")), # horizontal line after row 2
             col = fpColors(
               box = "#008080",
               line = "darkblue",
               summary = "#008080",
               zero = "red"
             ))|> 
  fp_add_lines(h_2 = gpar(lty = 1,
                          lwd = 1,
                          columns = 1:3, 
                          col = "#000044" 
  )) |>
  fp_add_header(label = c("Variable"),
                or = c("OR"),
                ci_text = c("95% CI"))|>
  
  fp_decorate_graph(box = gpar(lty = 2,
                               col = "lightgray"),
                    graph.pos = 4) |>
  fp_set_zebra_style("#f9f9f9")

dev.off()


#### fef weights ####
summary(fit_fef)

pos_df <- data.frame(
  term = names(fit_fef$pos.weights),
  weight = fit_fef$pos.weights,
  direction = "Positive")
pos_df$term <- rownames(pos_df)
rownames(pos_df) <- NULL

neg_df <- data.frame(
  term = names(fit_fef$neg.weights),
  weight = -fit_fef$neg.weights, 
  direction = "Negative")
neg_df$term <- rownames(neg_df)
rownames(neg_df) <- NULL

all_weights_df_fef <- rbind(pos_df, neg_df)
View(all_weights_df_fef)
colnames(all_weights_df_fef)

all_weights_df_fef <- all_weights_df_fef %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "pb_ln" ~ "Lead",
                   term == "sb_ln" ~ "Antimony"))


custom_order <- c("Calcium", "Lead", "Copper", "Cadmium", "Antimony", "Arsenic")

all_weights_df_fef$term <- factor(all_weights_df_fef$term,
                                  levels = custom_order)
View(all_weights_df_fef)

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

#### combine images ####
library(magick)
img1 <- image_read("qcom_fvc.png")
img2 <- image_read("qcom_fev.png")
img3 <- image_read("qcom_ff.png")
img4 <- image_read("qcom_fef.png")

row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))

plot_qcomp <- image_append(c(row1, row2), 
                           stack = TRUE)



final_qcomp <- image_annotate(
  plot_qcomp,
  text = "",
  size = 80,
  color = "black",
  boxcolor = "white",   # background box for contrast
  gravity = "northwest",   # position at top
  weight = 700  # bold title text  
)


image_write(final_qcomp, "final_qgcomp_non_smokers.png")


#### combine weights graph ####
p_fvc_qcomp 
p_fev_qcomp 
p_ff_qcomp 
p_fef_qcomp 
library(patchwork)
p_weight_qcomp <- (p_fvc_qcomp|p_fev_qcomp)/ (p_ff_qcomp |p_fef_qcomp )


ggsave("qcomp_weight_new_nosmokers.png",
       plot = p_weight_qcomp,
       width = 16,
       height = 14,
       dpi = 300)


### stacked the wqs and g-computation model together #####
imgA <- image_read("final_2iwqss_nosmoker.png")
imgB <- image_read("final_qgcomp_non_smokers.png")

# add labels directly onto the images
imgA_labeled <- image_annotate(imgA, "[A] - WQS", size = 70, gravity = "northwest")
imgB_labeled <- image_annotate(imgB, "[B] - Q-gcomputation", size = 70, gravity = "northwest")

# stack vertically
combined <- image_append(c(imgA_labeled, imgB_labeled), stack = TRUE)

# save
image_write(combined, "combined_AB_nonsmokers.png")


#### NON-SMOKERS in boot Q-gComp model ####

#### fvc < LLN ####
fit_fvc <- qgcomp.glm.boot(
  f = fvcbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fvc,
  q = 4,
  B = 500,
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fvc_qcomp
data_fvc_qcomp <- data_fvc_qcomp[-1,]


#### fev < LLN ####

fit_fev <- qgcomp.glm.boot(
  f = fev1belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fev,
  q = 4,
  B = 500, 
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_qcomp
df_fev_qcomp <- df_fev_qcomp[-1,]



fit_fev <- qgcomp.glm.noboot(
  f = fev1belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fev,
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fev_qcomp
df_fev_qcomp <- df_fev_qcomp[-1,]


#### fev1/fvc < LLN ####

fit_ff <- qgcomp.glm.boot(
  f = ffbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_ff,
  q = 4,
  B = 500, 
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_ff_qcomp
df_ff_qcomp <- df_ff_qcomp[-1,]

#### fef2575 < LLN ####

fit_fef <- qgcomp.glm.boot(
  f = fef2575belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fef,
  q = 4,
  B = 500,
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
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

df_fef_qcomp
df_fef_qcomp <- df_fef_qcomp[-1,]
