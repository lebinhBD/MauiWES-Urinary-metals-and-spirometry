library(qgcomp)

#### Overall - Q-gComp with bootstrapping ####

data <- HM_lung_demo_nomiss_111525_use
data <- data %>% rename(
  "age" = "age_new",
  "male" = "male_new"
)

data_fvc <- data %>% filter(
  data$fvc_quality %in% c("A", "B", "C")
)
dim(data_fvc)

data_fev1 <- data %>% filter(
  data$fev1_quality %in% c("A", "B", "C")
)
dim(data_fev1)

data_ff <- data %>% filter(
  data$fev1_quality %in% c("A", "B", "C") & data$fvc_quality %in% c("A", "B", "C"))
dim(data_ff)


data_fef <- data %>% filter(
  data$fev1_quality %in% c("A", "B", "C") & data$fvc_quality %in% c("A", "B", "C"))

dim(data_fvc)
dim(data_fev1)
dim(data_ff)
dim(data_fef)

#### fvc < LLN ####

fit_fvc_boot <- qgcomp.glm.boot(
  f = fvcbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + tobaco + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fvc,
  q = 4,
  family = binomial(),
  B = 500
)
summary(fit_fvc_boot)

coefs <- summary(fit_fvc_boot)$coefficients

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
                            round(plot_data$lower,3), " - ",
                            round(plot_data$upper, 3), "]"
)

plot_data$or <- paste0(round(plot_data$mean,3))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))
# 
# View(label_text)


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
             title = "FVC < LLN",
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


#### fev1 < LLN ####
dim(data_fev1)

fit_fev1_boot <- qgcomp.glm.boot(
  f = fev1belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + tobaco +  BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fev1,
  q = 4,
  family = binomial(),
  B = 500
)
summary(fit_fev1_boot)

coefs <- summary(fit_fev1_boot)$coefficients

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
# 
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

#### fev1/fvc ####
dim(data_ff)

fit_ff_boot <- qgcomp.glm.boot(
  f = ffbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male  + tobaco +  BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_ff,
  q = 4,
  family = binomial(),
  B = 500
)
summary(fit_ff_boot)

coefs <- summary(fit_ff_boot)$coefficients

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

#### fef2575 ####
dim(data_fef)

fit_fef_boot <- qgcomp.glm.boot(
  f = fef2575belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + tobaco+ BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fef,
  q = 4,
  family = binomial(),
  B = 500
)
summary(fit_fef_boot)

coefs <- summary(fit_fef_boot)$coefficients

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

library(magick)
img1 <- image_read("qcom_fvc.png")
img2 <- image_read("qcom_fev.png")
img3 <- image_read("qcom_ff.png")
img4 <- image_read("qcom_fef.png")

row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))

plot_qcomp <- image_append(c(row1, row2), 
                           stack = TRUE)

final_qcomp_boot <- image_annotate(
  plot_qcomp,
  text = "B",
  size = 80,
  color = "black",
  boxcolor = "white",   # background box for contrast
  gravity = "northwest",   # position at top
  weight = 700  # bold title text  
)

image_write(final_qcomp_boot, "final_qcomp_boot.png")

#### Overall with Q-gComp non-bootstrapping model ####

#### fvc < LLN ####

fit_fvc <- qgcomp.glm.noboot(
  f = fvcbelowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + tobaco + BMIscore,
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
                            round(plot_data$lower,3), " - ",
                            round(plot_data$upper, 3), "]"
)

plot_data$or <- paste0(round(plot_data$mean,3))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))
# 
# View(label_text)


png("qcom_fvc_noboot.png", 
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
             title = "FVC < LLN",
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
# View(all_weights_df)
colnames(all_weights_df)

all_weights_df <- all_weights_df %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "pb_ln" ~ "Lead"))



# 4. draw a graph

custom_order <- c("Calcium",
                  "Lead",
                  "Copper",
                  "Cadmium",
                  "Antimony",
                  "Arsenic")

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
    title = expression(bold(paste("FVC < LLN" )))
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fvc_qcomp

#### fev1 < lln ####

fit_fev <- qgcomp.glm.noboot(
  f = fev1belowlln_N ~  as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + tobaco + BMIscore,
  expnms = c("as_ln",
             "ca_ln",
             "cd_ln",
             "cu_ln",
             "pb_ln",
             "sb_ln"),
  data = data_fev1,
  q = 4,
  family = binomial()
)
summary(fit_fev)

coefs <- summary(fit_fev)$coefficients


# Create a results table
data_fev_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

data_fev_qcomp <- data_fev_qcomp %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fev_qcomp


data_fev_qcomp <- data_fev_qcomp[-1,]

data_fev_qcomp$term

data_fev_qcomp[1] <- "Q-gcomp"


data_fev_qcomp$term <- ifelse(data_fev_qcomp$p <= 0.001,
                              paste0(data_fev_qcomp$term, "***"),
                              ifelse(data_fev_qcomp$p > 0.001 & data_fev_qcomp$p <= 0.01,
                                     paste0(data_fev_qcomp$term, "**"),
                                     ifelse(data_fev_qcomp$p > 0.01 & data_fev_qcomp$p <= 0.05,
                                            paste0(data_fev_qcomp$term, "*"),
                                            ifelse(data_fev_qcomp$p > 0.05 & data_fev_qcomp$p <= 0.1,
                                                   paste0(data_fev_qcomp$term, "+"),
                                                   paste0(data_fev_qcomp$term)))))

colnames(data_fev_qcomp)
data_fev_qcomp

data_fev_qcomp_chuan <- data_fev_qcomp[ , c(1, 2, 3, 4)]
data_fev_qcomp_chuan 

plot_data <- with(data_fev_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,3), " - ",
                            round(plot_data$upper, 3), "]"
)

plot_data$or <- paste0(round(plot_data$mean,3))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))
# 
# View(label_text)


png("qcom_fev_noboot.png", 
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

all_weights_df_fev <- all_weights_df_fev  %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "pb_ln" ~ "Lead"))

# 4. draw a graph

custom_order <- c("Calcium",
                  "Lead",
                  "Copper",
                  "Cadmium",
                  "Antimony",
                  "Arsenic")

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
    title = expression(bold(paste("FEV"[1], " < LLN")))
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fev_qcomp 


#### fev1/fvc < lln ####

fit_ff <- qgcomp.glm.noboot(
  f = ffbelowlln_N ~   as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln + age + male + tobaco + BMIscore,
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
data_ff_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

data_ff_qcomp <- data_ff_qcomp %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_ff_qcomp


data_ff_qcomp <- data_ff_qcomp[-1,]

data_ff_qcomp$term

data_ff_qcomp[1] <- "Q-gcomp"


data_ff_qcomp$term <- ifelse(data_ff_qcomp$p <= 0.001,
                              paste0(data_ff_qcomp$term, "***"),
                              ifelse(data_ff_qcomp$p > 0.001 & data_ff_qcomp$p <= 0.01,
                                     paste0(data_ff_qcomp$term, "**"),
                                     ifelse(data_ff_qcomp$p > 0.01 & data_ff_qcomp$p <= 0.05,
                                            paste0(data_ff_qcomp$term, "*"),
                                            ifelse(data_ff_qcomp$p > 0.05 & data_ff_qcomp$p <= 0.1,
                                                   paste0(data_ff_qcomp$term, "+"),
                                                   paste0(data_ff_qcomp$term)))))

colnames(data_ff_qcomp)
data_ff_qcomp

data_ff_qcomp_chuan <- data_ff_qcomp[ , c(1, 2, 3, 4)]
data_ff_qcomp_chuan 

plot_data <- with(data_ff_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,3), " - ",
                            round(plot_data$upper, 3), "]"
)

plot_data$or <- paste0(round(plot_data$mean,3))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))
# 
# View(label_text)


png("qcom_ff_noboot.png", 
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




#### fev1/fvc weights ####

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
# View(all_weights_df_fev)
colnames(all_weights_df_ff)

all_weights_df_ff <- all_weights_df_ff %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "pb_ln" ~ "Lead"))



# 4. draw a graph

custom_order <- c("Calcium",
                  "Lead",
                  "Copper",
                  "Cadmium",
                  "Antimony",
                  "Arsenic")

all_weights_df_ff$term <- factor(all_weights_df_ff$term,
                                 levels = custom_order)
# View(all_weights_df_fev)

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
    title = expression(bold(paste("FEV"[1], "/FVC < LLN")))
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_ff_qcomp 


#### fef2575 < lln ####

fit_fef <- qgcomp.glm.noboot(
  f = fef2575belowlln_N ~   as_ln + ca_ln + cd_ln + cu_ln + pb_ln + sb_ln  + age + male + tobaco + BMIscore,
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
data_fef_qcomp <- data.frame(
  term = rownames(coefs),
  OR = exp(coefs[, "Estimate"]),
  CI_low = exp(coefs[, "Lower CI"]),
  CI_high = exp(coefs[, "Upper CI"]),
  p      = coefs[, "Pr(>|z|)"]
)

data_fef_qcomp <- data_fef_qcomp %>%
  mutate(
    OR = round(OR, 2),
    CI_low = round(CI_low, 2),
    CI_high = round(CI_high, 2),
    p = round(p, 3)
  ) %>%
  select(term, OR, CI_low, CI_high, p)

data_fef_qcomp


data_fef_qcomp <- data_fef_qcomp[-1,]

data_fef_qcomp$term

data_fef_qcomp[1] <- "Q-gcomp"


data_fef_qcomp$term <- ifelse(data_fef_qcomp$p <= 0.001,
                             paste0(data_fef_qcomp$term, "***"),
                             ifelse(data_fef_qcomp$p > 0.001 & data_fef_qcomp$p <= 0.01,
                                    paste0(data_fef_qcomp$term, "**"),
                                    ifelse(data_fef_qcomp$p > 0.01 & data_fef_qcomp$p <= 0.05,
                                           paste0(data_fef_qcomp$term, "*"),
                                           ifelse(data_fef_qcomp$p > 0.05 & data_fef_qcomp$p <= 0.1,
                                                  paste0(data_fef_qcomp$term, "+"),
                                                  paste0(data_fef_qcomp$term)))))

colnames(data_fef_qcomp)
data_fef_qcomp

data_fef_qcomp_chuan <- data_fef_qcomp[ , c(1, 2, 3, 4)]
data_fef_qcomp_chuan 

plot_data <- with(data_fef_qcomp_chuan,
                  data.frame(
                    mean = OR,
                    lower = CI_low,
                    upper =  CI_high ,
                    label = term
                  ))
plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,3), " - ",
                            round(plot_data$upper, 3), "]"
)

plot_data$or <- paste0(round(plot_data$mean,3))

# View(plot_data)
class(plot_data)
names(plot_data)


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))
# 
# View(label_text)


png("qcom_fef_noboot.png", 
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
# View(all_weights_df_fev)
colnames(all_weights_df_fef)

all_weights_df_fef <- all_weights_df_fef %>% mutate(
  term = case_when(term == "as_ln" ~ "Arsenic",
                   term == "ca_ln" ~ "Calcium",
                   term == "cd_ln" ~ "Cadmium",
                   term == "cu_ln" ~ "Copper",
                   term == "sb_ln" ~ "Antimony",
                   term == "pb_ln" ~ "Lead"))


# 4. draw a graph
custom_order <- c("Calcium",
                  "Lead",
                  "Copper",
                  "Cadmium",
                  "Antimony",
                  "Arsenic")

all_weights_df_fef$term <- factor(all_weights_df_fef$term,
                                  levels = custom_order)
# View(all_weights_df_fev)

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
    title = expression(bold(FEF[25*"\u201375"] ~ "< LLN"))
  ) +
  theme_bw(base_size = 20) +
  theme(legend.position = "none") +
  geom_text(aes(label = round(abs(weight), 2)),
            hjust = ifelse(all_weights_df$weight > 0, -0.1, 1.1),
            size = 4)
p_fef_qcomp 


#### combine weights graphs ####
p_fvc_qcomp
p_fev_qcomp 
p_ff_qcomp 
p_fef_qcomp



#### combine weights graph ####
library(patchwork)
p_weight_noboot <- (p_fvc_qcomp|p_fev_qcomp)/ (p_ff_qcomp |p_fef_qcomp )

ggsave("FigS8_qcomp_weight_noboot.png",
       plot = p_weight_noboot,
       width = 16,
       height = 14,
       dpi = 300)


library(magick)
img1 <- image_read("qcom_fvc_noboot.png")
img2 <- image_read("qcom_fev_noboot.png")
img3 <- image_read("qcom_ff_noboot.png")
img4 <- image_read("qcom_fef_noboot.png")

row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))

plot_qcomp_noboot <- image_append(c(row1, row2), 
                           stack = TRUE)

final_qcomp_noboot <- image_annotate(
  plot_qcomp_noboot,
  text = "A",
  size = 80,
  color = "black",
  boxcolor = "white",   # background box for contrast
  gravity = "northwest",   # position at top
  weight = 700  # bold title text  
)

image_write(final_qcomp_noboot, "final_qcomp_noboot.png")


# stack vertically
imgA <- image_read("final_qcomp_noboot.png")
imgB <- image_read("final_qcomp_boot.png")

# stack vertically
combined <- image_append(c(imgA, imgB), stack = TRUE)

# save output
image_write(combined, "FigS7_overall_Qcomp_noboot_boot.png")




