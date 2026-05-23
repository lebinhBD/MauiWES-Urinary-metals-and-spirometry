library(dplyr)
library(tidyr)
library(survey)
library(meta)
library(metadat)
library(forestplot)
library(grid)
library(checkmate)
library(abind)
library(magick)
library(gWQS)
library(survey)
library(broom)

data <- HM_lung_demo_nomiss_111525_use
names(data)
table(data$location, useNA = "always")
data$Lahaina <- ifelse(data$location == "Lahaina",1,0)
table(data$Lahaina, useNA = "always")

data$Kaluhui <- ifelse(data$location == "Wailuku/ Kahului",1,0)
table(data$Kaluhui, useNA = "always")

data$Kihei <- ifelse(data$location == "Kihei",1,0)
table(data$Kihei , useNA = "always")

data1 <- data %>% select(
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
  pb_ln,
  ca_ln,
  Lahaina,
  Kaluhui,
  Kihei)%>% rename(
    "age" = "age_new", 
    "male" =  "male_new")

#### FVC < LLN ####
data_fvc <- data1%>% filter(fvc_quality %in% c("A", "B", "C"))
dim(data_fvc)

design <- svydesign(id = ~1, data = data_fvc)

# Poisson regression with robust SE → Relative Risks (RRs)
model_fvc <- svyglm(fvcbelowlln_N ~  as_ln + cd_ln + cu_ln + sb_ln + pb_ln +  ca_ln + age + male + tobaco + BMIscore + Lahaina + Kaluhui + Kihei,
                    design = design,
                    family = binomial())

summary(model_fvc)

or <- exp(coef(model_fvc))        # RRs
ci <- exp(confint(model_fvc)) ## 95% ci
pvals <- summary(model_fvc)$coefficients[,4]

OR_fvc <- data.frame(
  # variable = names(rr),
  or = or,
  ci_lower = ci[,1],
  ci_upper = ci[,2],
  p_value = pvals
)
print(OR_fvc)

OR_fvc <- tidy(model_fvc,
               # exponentiate = TRUE,
               conf.int = TRUE)  %>%
  filter(!term %in% c("(Intercept)", "age", "male", "tobaco1",
                      "BMIscore", "Lahaina",
                      "Kaluhui", "Kihei")) ### exclude intercept

OR_fvc
OR_fvc$term

OR_fvc$term[1] <- "Arsenic"
OR_fvc$term[2] <- "Cadmium"
OR_fvc$term[3] <- "Copper"
OR_fvc$term[4] <- "Antimony"
OR_fvc$term[5] <- "Lead"
OR_fvc$term[6] <- "Calcium"

OR_fvc$term <- ifelse(OR_fvc$p.value <= 0.001,
                      paste0(OR_fvc$term, "***"),
                      ifelse(OR_fvc$p.value > 0.001 & OR_fvc$p.value <= 0.01,
                             paste0(OR_fvc$term, "**"),
                             ifelse(OR_fvc$p.value > 0.01 & OR_fvc$p.value <= 0.05,
                                    paste0(OR_fvc$term, "*"), ifelse(OR_fvc$p.value > 0.05 & OR_fvc$p.value <= 0.1,
                                                                     paste0(OR_fvc$term, "+"),
                                                                     paste0(OR_fvc$term)))))

colnames(OR_fvc)
OR_fvc

OR_fvc_chuan <- OR_fvc[ , c(1, 2, 3, 6, 7)]
OR_fvc_chuan 

plot_data <- with(OR_fvc_chuan,
                  data.frame(
                    mean = estimate,
                    lower = conf.low,
                    upper = conf.high,
                    label = term))

plot_data <- plot_data |>
  dplyr::mutate(
    mean  = exp(mean),
    lower = exp(lower),
    upper = exp(upper) )

plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]")

plot_data$or <- paste0(round(plot_data$mean,2))


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

View(label_text)


png("fvc.png", 
    width = 8,
    height = 6,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.01, 0.1, 0.5, 1,2,5,10, 100)
attr(my_ticks, "labels") <- c("0.01", "0.1", "0.5", "1", "2", "5", "10", "100") ## adjust not decimal or decimal numbers 

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


#### FEV1 < LLN ####
data_fev <- data1%>% filter(fev1_quality %in% c("A", "B", "C"))
dim(data_fev)

design_fev <- svydesign(id = ~1, data = data_fev)

# Poisson regression with robust SE → Relative Risks (RRs)
model_fev1 <- svyglm(fev1belowlln_N ~ as_ln + cd_ln + cu_ln + sb_ln + pb_ln +  ca_ln + age + male + tobaco + BMIscore + Lahaina +Kaluhui + Kihei ,
                     design = design_fev,
                     family = binomial())

summary(model_fev1)

or <- exp(coef(model_fev1))        # RRs
ci <- exp(confint(model_fev1)) ## 95% ci
pvals <- summary(model_fev1)$coefficients[,4]

OR_fev1 <- data.frame(
  or = or,
  ci_lower = ci[,1],
  ci_upper = ci[,2],
  p_value = pvals
)
print(OR_fev1)

library(broom)
OR_fev1 <- tidy(model_fev1,
                # exponentiate = TRUE,
                conf.int = TRUE)  %>%
  filter(!term %in% c("(Intercept)", "age", "male", "tobaco1",
                      "BMIscore", "Lahaina",
                      "Kaluhui", "Kihei")) ### exclude intercept

OR_fev1
OR_fev1$term

OR_fev1$term[1] <- "Arsenic"
OR_fev1$term[2] <- "Cadmium"
OR_fev1$term[3] <- "Copper"
OR_fev1$term[4] <- "Antimony"
OR_fev1$term[5] <- "Lead"
OR_fev1$term[6] <- "Calcium"


OR_fev1$term <- ifelse(OR_fev1$p.value <= 0.001,
                       paste0(OR_fev1$term, "***"),
                       ifelse(OR_fev1$p.value > 0.001 & OR_fev1$p.value <= 0.01,
                              paste0(OR_fev1$term, "**"),
                              ifelse(OR_fev1$p.value > 0.01 & OR_fev1$p.value <= 0.05,
                                     paste0(OR_fev1$term, "*"), ifelse(OR_fev1$p.value > 0.05 & OR_fev1$p.value <= 0.1,
                                                                       paste0(OR_fev1$term, "+"),
                                                                       paste0(OR_fev1$term)))))


colnames(OR_fev1)
OR_fev1

OR_fev1_chuan <- OR_fev1[ , c(1, 2, 3, 6, 7)]
OR_fev1_chuan 

plot_data <- with(OR_fev1_chuan,
                  data.frame(
                    mean = estimate,
                    lower = conf.low,
                    upper = conf.high,
                    label = term))

plot_data <- plot_data |>
  dplyr::mutate(
    mean  = exp(mean),
    lower = exp(lower),
    upper = exp(upper) )

plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]")

plot_data$or <- paste0(round(plot_data$mean,2))


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("fev1.png", 
    width = 8,
    height = 6,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.01, 0.1, 0.5, 1,2,5,10, 100)
attr(my_ticks, "labels") <- c("0.01", "0.1", "0.5", "1", "2", "5", "10", "100") ## adjust not decimal or decimal numbers 

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
             title =  expression(bold(paste("FEV"[1], " < LLN"))),
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

#### FEV1/FVC < LLN ####
data_ff <- data1%>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))
dim(data_ff)

design_ff <- svydesign(id = ~1, data = data_ff) 
dim(data_ff)  


# Poisson regression with robust SE → Relative Risks (RRs)
model_ff <- svyglm(ffbelowlln_N ~as_ln + cd_ln + cu_ln + sb_ln + pb_ln + ca_ln + age + male + tobaco + BMIscore + Lahaina + Kaluhui + Kihei,
                   design = design_ff,
                   family = binomial())

summary(model_ff)

or <- exp(coef(model_ff))        # RRs
ci <- exp(confint(model_ff)) ## 95% ci
pvals <- summary(model_ff)$coefficients[,4]

OR_ff <- data.frame(
  # variable = names(rr),
  or = or,
  ci_lower = ci[,1],
  ci_upper = ci[,2],
  p_value = pvals
)
print(OR_ff)

library(broom)
OR_ff <- tidy(model_ff,
              # exponentiate = TRUE,
              conf.int = TRUE)  %>%
  filter(!term %in% c("(Intercept)", "age", "male", "tobaco1",
                      "BMIscore", "Lahaina",
                      "Kaluhui", "Kihei")) ### exclude intercept

OR_ff$term[1] <- "Arsenic"
OR_ff$term[2] <- "Cadmium"
OR_ff$term[3] <- "Copper"
OR_ff$term[4] <- "Antimony"
OR_ff$term[5] <- "Lead"
OR_ff$term[6] <- "Calcium"



OR_ff$term <- ifelse(OR_ff$p.value <= 0.001,
                     paste0(OR_ff$term, "***"),
                     ifelse(OR_ff$p.value > 0.001 & OR_ff$p.value <= 0.01,
                            paste0(OR_ff$term, "**"),
                            ifelse(OR_ff$p.value > 0.01 & OR_ff$p.value <= 0.05,
                                   paste0(OR_ff$term, "*"), ifelse(OR_ff$p.value > 0.05 & OR_ff$p.value <= 0.1,
                                                                   paste0(OR_ff$term, "+"),
                                                                   paste0(OR_ff$term)))))


colnames(OR_ff)
OR_ff

OR_ff_chuan <- OR_ff[ , c(1, 2, 3, 6, 7)]
OR_ff_chuan 

plot_data <- with(OR_ff_chuan,
                  data.frame(
                    mean = estimate,
                    lower = conf.low,
                    upper = conf.high,
                    label = term))

plot_data <- plot_data |>
  dplyr::mutate(
    mean  = exp(mean),
    lower = exp(lower),
    upper = exp(upper) )

plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]")

plot_data$or <- paste0(round(plot_data$mean,2))


label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))


png("ff.png", 
    width = 8,
    height = 6,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.01, 0.1, 0.5, 1,2,5,10, 100)
attr(my_ticks, "labels") <- c("0.01", "0.1", "0.5", "1", "2", "5", "10", "100") ## adjust not decimal or decimal numbers 

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

#### FEF2575 < LLN ####
data_fef <- data1%>% filter(fev1_quality %in% c("A", "B", "C") & fvc_quality %in% c("A", "B", "C"))
dim(data_fef)

dim(data_fef) 

design_fef <- svydesign(id = ~1, data = data_fef)

# Poisson regression with robust SE → Relative Risks (RRs)
model_fef <- svyglm(fef2575belowlln_N ~ as_ln + cd_ln + cu_ln + sb_ln + pb_ln  + ca_ln + age + male + tobaco + BMIscore + Lahaina + Kaluhui + Kihei,
                    design = design_fef,
                    family = binomial())

summary(model_fef)

or <- exp(coef(model_fef))        # RRs
ci <- exp(confint(model_fef)) ## 95% ci
pvals <- summary(model_fef)$coefficients[,4]

OR_fef <- data.frame(
  # variable = names(rr),
  or = or,
  ci_lower = ci[,1],
  ci_upper = ci[,2],
  p_value = pvals
)
print(OR_fef)


library(broom)
OR_fef <- tidy(model_fef,
               # exponentiate = TRUE,
               conf.int = TRUE)  %>%
  filter(!term %in% c("(Intercept)", "age", "male", "tobaco1",
                      "BMIscore", "Lahaina",
                      "Kaluhui", "Kihei")) ### exclude intercept


OR_fef$term

OR_fef$term[1] <- "Arsenic"
OR_fef$term[2] <- "Cadmium"
OR_fef$term[3] <- "Copper"
OR_fef$term[4] <- "Antimony"
OR_fef$term[5] <- "Lead"
OR_fef$term[6] <- "Calcium"


OR_fef$term <- ifelse(OR_fef$p.value <= 0.001,
                      paste0(OR_fef$term, "***"),
                      ifelse(OR_fef$p.value > 0.001 & OR_fef$p.value <= 0.01,
                             paste0(OR_fef$term, "**"),
                             ifelse(OR_fef$p.value > 0.01 & OR_fef$p.value <= 0.05,
                                    paste0(OR_fef$term, "*"),ifelse(OR_fef$p.value > 0.05 & OR_fef$p.value <= 0.1,
                                                                    paste0(OR_fef$term, "+"),
                                                                    paste0(OR_fef$term)))))

colnames(OR_fef)
OR_fef

OR_fef_chuan <- OR_fef[ , c(1, 2, 3, 6, 7)]
OR_fef_chuan 

plot_data <- with(OR_fef_chuan,
                  data.frame(
                    mean = estimate,
                    lower = conf.low,
                    upper = conf.high,
                    label = term))

plot_data <- plot_data |>
  dplyr::mutate(
    mean  = exp(mean),
    lower = exp(lower),
    upper = exp(upper) )

plot_data$ci_text <- paste0("[",
                            round(plot_data$lower,2), " - ",
                            round(plot_data$upper, 2), "]")

plot_data$or <- paste0(round(plot_data$mean,2))

label_text <- 
  rbind(
    c("Variable", "OR", "95% CI"),  # header row
    cbind(plot_data$label,
          plot_data$or,
          plot_data$ci_text))

png("fef.png", 
    width = 8,
    height = 6,
    res = 300, 
    units = "in"
)


my_ticks <- c(0.01, 0.1, 0.5, 1,2,5,10, 100)
attr(my_ticks, "labels") <- c("0.01", "0.1", "0.5", "1", "2", "5", "10", "100") ## adjust not decimal or decimal numbers 

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
             xlab = "Risk Ratio (log scale)",
             xticks = my_ticks,
             xlog = TRUE,
             title = expression(bold(FEF[25*"\u201375"] ~ "< LLN (%)")),
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


#### combine images ####
library(magick)
img1 <- image_read("fvc.png")
img2 <- image_read("fev1.png")
img3 <- image_read("ff.png")
img4 <- image_read("fef.png")

row1 <- image_append(c(img1, img2))
row2 <- image_append(c(img3, img4))

plot <- image_append(c(row1, row2), 
                     stack = TRUE)


image_write(plot, "FigS4_plot_linear.png")
