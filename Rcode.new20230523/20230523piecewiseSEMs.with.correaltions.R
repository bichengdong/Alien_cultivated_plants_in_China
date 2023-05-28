#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-31 11:24:22
# @Last Modified by: dbc
# @Last Modified time: 2023-05-26 17:48:28
# @Description: piecewiseSEMs in Fig.3 and Appendix Tables 4-6
#--------------------------------------------------------------------------#
# abbreviations of variables
# bg_num: number of botanical gardens; bg in SEMs
# chklst_prov_num: number of provinces; pr in SEMs
# new_sum.L3.code: number of tdwg level-03 regions, native range size; nr in SEMs
# residence_time_to_2020: minimum residence time; rt in SEMs
# Height_max_max: maximum height; he in SEMs
# hab_suit_mean: climatic suitability, 0-1; cs in SEMs
# Life_form_new: life form, including (1) short-lived herbs, (2) long-lived herbs, and (3) woody; lf in SEMs
# prop_type: propagation mode, including (1) seed, (2) vegetative, and (3) both; ve in SEMs
# ali_new_status: status of online nursery availability, yes or no; os in SEMs
# wcup_eco_use_num: number of economic use category, 0-10 categories
# wcup_eco_use_status: status of economic use, yes or no; eu in SEMs
# naturalized_status: naturalization status of alien plants in China, status in SEMs
# ==========================================================================
# preparation work
# ==========================================================================
# cleaning memory
cat("\014")
rm(list=ls())
gc()

# loading packages
library(broom)
library(corrplot)
library(data.table)
library(GGally)
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(Hmisc)

# piecewiseSEM version 2.3.0
# Install development branch from github
# library(devtools)
# install_github("jslefche/piecewiseSEM@devel")
library(piecewiseSEM)
library(psych)
library(readr)
library(semTools)
library(skimr)
library(tidyverse)
library(dplyr)

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China/data")
getwd()

# importing data, checklist_nat.pla_in_China
checklist_Invspc_new <- read_csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv", locale = locale(encoding = "GB2312"))

# check data structure
glimpse(checklist_Invspc_new)
View(checklist_Invspc_new)
table(checklist_Invspc_new$naturalized_status)

# check the data with NA values
length(checklist_Invspc_new$bg_num[!is.na(checklist_Invspc_new$bg_num)])
length(checklist_Invspc_new$chklst_prov_num[!is.na(checklist_Invspc_new$chklst_prov_num)])
length(checklist_Invspc_new$new_sum.L3.code[!is.na(checklist_Invspc_new$new_sum.L3.code)])
length(checklist_Invspc_new$residence_time_to_2020[!is.na(checklist_Invspc_new$residence_time_to_2020)])
length(checklist_Invspc_new$hab_suit_mean[!is.na(checklist_Invspc_new$hab_suit_mean)])
length(checklist_Invspc_new$Height_max_max[!is.na(checklist_Invspc_new$Height_max_max)])
length(checklist_Invspc_new$Life_form_new[!is.na(checklist_Invspc_new$Life_form_new)])
length(checklist_Invspc_new$prop_type[!is.na(checklist_Invspc_new$prop_type)])
length(checklist_Invspc_new$ali_new_status[!is.na(checklist_Invspc_new$ali_new_status)])
length(checklist_Invspc_new$wcup_eco_use_num[!is.na(checklist_Invspc_new$wcup_eco_use_num)])

# because the raw data included the category of economic use (0-10), rather the status of economic use (0 and 1),
# we need transform the category to status before analysis
checklist_Invspc_new <- checklist_Invspc_new %>%
                        mutate(wcup_eco_use_status = case_when(wcup_eco_use_num == 0 ~ 0,
                                                               wcup_eco_use_num > 0 ~ 1))

# --------------------------------------------------------------------------
# continuous variables
# --------------------------------------------------------------------------
# scale the data after log-transformation
# rename column names
checklist_Invspc_new$bg <- scale(log(checklist_Invspc_new$bg_num + 1))
checklist_Invspc_new$pr <- scale(log(checklist_Invspc_new$chklst_prov_num + 1))
checklist_Invspc_new$nr <- scale(log(checklist_Invspc_new$new_sum.L3.code))
checklist_Invspc_new$rt <- scale(log(checklist_Invspc_new$residence_time_to_2020))
checklist_Invspc_new$cs <- scale(log(checklist_Invspc_new$hab_suit_mean + 0.001))

# change variables as numeric variables
checklist_Invspc_new$bg <- as.numeric(checklist_Invspc_new$bg)
checklist_Invspc_new$pr <- as.numeric(checklist_Invspc_new$pr)
checklist_Invspc_new$nr <- as.numeric(checklist_Invspc_new$nr)
checklist_Invspc_new$rt <- as.numeric(checklist_Invspc_new$rt)
checklist_Invspc_new$cs <- as.numeric(checklist_Invspc_new$cs)

# --------------------------------------------------------------------------
# binary variables
# --------------------------------------------------------------------------
# change variables as numeric variables
# rename column names
checklist_Invspc_new$lf     <- as.numeric(checklist_Invspc_new$Life_form_new)
checklist_Invspc_new$ve     <- as.numeric(checklist_Invspc_new$prop_type)
checklist_Invspc_new$os     <- as.numeric(checklist_Invspc_new$ali_new_status)
checklist_Invspc_new$eu     <- as.numeric(checklist_Invspc_new$wcup_eco_use_status)
checklist_Invspc_new$status <- as.numeric(checklist_Invspc_new$naturalized_status)

# --------------------------------------------------------------------------
# dummy variables
# --------------------------------------------------------------------------
# # change prop_type as dummpy.codes, because it is hard to estimate the factors
# # with more than two levels in lavvan or piecewise SEM packages
# # https://www.aggieerin.com/post/quick-and-dirty-categorical-lavaan/

checklist_Invspc_new$ve <- checklist_Invspc_new$prop_type
checklist_Invspc_new$ve[which(checklist_Invspc_new$ve == 1)] <- "dummpy.ve_seed"
checklist_Invspc_new$ve[which(checklist_Invspc_new$ve == 2)] <- "dummpy.ve_veg"
checklist_Invspc_new$ve[which(checklist_Invspc_new$ve == 3)] <- "dummpy.ve_both"
checklist_Invspc_new$ve <- as.factor(checklist_Invspc_new$ve)

# checklist_Invspc_new$lf <- checklist_Invspc_new$Life_form_new
# checklist_Invspc_new$lf[which(checklist_Invspc_new$lf == 1)] <- "dummpy.lf_short_lived"
# checklist_Invspc_new$lf[which(checklist_Invspc_new$lf == 2)] <- "dummpy.lf_long_lived"
# checklist_Invspc_new$lf[which(checklist_Invspc_new$lf == 3)] <- "dummpy.lf_woody"
# checklist_Invspc_new$lf <- as.factor(checklist_Invspc_new$lf)

# dummpy factors
# library(psych)
# checklist_Invspc_new_01 <- cbind(checklist_Invspc_new, dummy.code(checklist_Invspc_new$lf), dummy.code(checklist_Invspc_new$ve))
checklist_Invspc_new_01 <- cbind(checklist_Invspc_new, dummy.code(checklist_Invspc_new$ve))

checklist_Invspc_new_01$dummpy.ve_seed <- factor(checklist_Invspc_new_01$dummpy.ve_seed, levels = c("0", "1"))
checklist_Invspc_new_01$dummpy.ve_veg  <- factor(checklist_Invspc_new_01$dummpy.ve_veg, levels = c("0", "1"))
checklist_Invspc_new_01$dummpy.ve_both <- factor(checklist_Invspc_new_01$dummpy.ve_both, levels = c("0", "1"))

# checklist_Invspc_new_01$dummpy.lf_short_lived <- factor(checklist_Invspc_new_01$dummpy.lf_short_lived, levels = c("0", "1"))
# checklist_Invspc_new_01$dummpy.lf_long_lived  <- factor(checklist_Invspc_new_01$dummpy.lf_long_lived, levels = c("0", "1"))
# checklist_Invspc_new_01$dummpy.lf_woody       <- factor(checklist_Invspc_new_01$dummpy.lf_woody, levels = c("0", "1"))

# --------------------------------------------------------------------------
# maximum height, Height_max_max, he in SEMs
# --------------------------------------------------------------------------
# check the replicates and data range of Height_max_max
# filter out the data on Height_max_max without life form
checklist_Invspc_new_01 <- checklist_Invspc_new_01 %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_new_01 %>% select(naturalized_status, Height_max_max, Life_form_new))

# based on category of Life_form_new，log-transform and scale Height_max_max
checklist_Invspc_new_01$he <- checklist_Invspc_new_01$Height_max_max
checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "1")] <- scale(log(checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "1")]))
checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "2")] <- scale(log(checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "2")]))
checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "3")] <- scale(log(checklist_Invspc_new_01$he[which(checklist_Invspc_new_01$Life_form_new == "3")]))

# check the structure of new data
View(checklist_Invspc_new_01)
glimpse(checklist_Invspc_new_01)
skimr::skim(checklist_Invspc_new_01 %>% select(bg:dummpy.ve_veg))

# ==========================================================================
# correlations
# it will give the correlations between each variable
# ==========================================================================
# select variables for correlations
model_data <- checklist_Invspc_new_01 %>% dplyr::select(bg, pr, nr, cs, he, dummpy.ve_veg, dummpy.ve_both, os, eu, status) %>% tidyr::drop_na(.)
model_data <- model_data %>% mutate_all(~as.character(.)) %>% mutate_all(~as.numeric(.))
skim(model_data)
glimpse(model_data)

# correlation test
cortest <- rcorr(as.matrix(model_data), type = "pearson")
View(round(cortest$P,3))

# correlation plot
ggcorr(model_data, nbreaks = 10, label = T, low = "red3", high = "green3",
       label_round = 2, name = "Correlation Scale", label_alpha = T, hjust = 0.75) +
       ggtitle(label = "Correlation Plot") +
       theme(plot.title = element_text(hjust = 0.6))

# ==========================================================================
# main: path analysis based on piecewiseSEM
# ==========================================================================
# # loading packages
# library(piecewiseSEM)

# based on life form, slice the data into three parts.
# short-lived herbs
checklist_Invspc_new_02_short_lived <- checklist_Invspc_new_01 %>%
                                       filter(Life_form_new == 1) %>%
                                       select(bg, pr, nr, cs, he, dummpy.ve_veg, dummpy.ve_both, os, eu, status) %>%
                                       drop_na(.) %>% mutate_all(~as.character(.)) %>% mutate_all(~as.numeric(.))

# long-lived herbs
checklist_Invspc_new_02_long_lived  <- checklist_Invspc_new_01 %>%
                                       filter(Life_form_new == 2) %>%
                                       select(bg, pr, nr, cs, he, dummpy.ve_veg, dummpy.ve_both, os, eu, status) %>%
                                       drop_na(.) %>% mutate_all(~as.character(.)) %>% mutate_all(~as.numeric(.))

# woody species
checklist_Invspc_new_02_woody       <- checklist_Invspc_new_01 %>%
                                       filter(Life_form_new == 3) %>%
                                       select(bg, pr, nr, cs, he, dummpy.ve_veg, dummpy.ve_both, os, eu, status) %>%
                                       drop_na(.) %>% mutate_all(~as.character(.)) %>% mutate_all(~as.numeric(.))

glimpse(checklist_Invspc_new_02_short_lived)
glimpse(checklist_Invspc_new_02_long_lived)
glimpse(checklist_Invspc_new_02_woody)

# Step-wise lms or glms, to find the possible combination of variables for each life form
# --------------------------------------------------------------------------
fit.lm01_shortlive  <- lm(bg ~ nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_short_lived)
fit.lm02_shortlive  <- lm(pr ~ bg + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_short_lived)
fit.glm01_shortlive <- glm(os ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + eu, family = "binomial", data = checklist_Invspc_new_02_short_lived)
fit.glm02_shortlive <- glm(status ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, family = "binomial", data = checklist_Invspc_new_02_short_lived)

step(fit.lm01_shortlive)
step(fit.lm02_shortlive)
step(fit.glm01_shortlive)
step(fit.glm02_shortlive)

fit.lm01_longlive  <- lm(bg ~ nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_long_lived)
fit.lm02_longlive  <- lm(pr ~ bg + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_long_lived)
fit.glm01_longlive <- glm(os ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + eu, family = "binomial", data = checklist_Invspc_new_02_long_lived)
fit.glm02_longlive <- glm(status ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, family = "binomial", data = checklist_Invspc_new_02_long_lived)

step(fit.lm01_longlive)
step(fit.lm02_longlive)
step(fit.glm01_longlive)
step(fit.glm02_longlive)

fit.lm01_woody  <- lm(bg ~ nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_woody)
fit.lm02_woody  <- lm(pr ~ bg + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, data = checklist_Invspc_new_02_woody)
fit.glm01_woody <- glm(os ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + eu, family = "binomial", data = checklist_Invspc_new_02_woody)
fit.glm02_woody <- glm(status ~ bg + pr + nr + cs + he + dummpy.ve_veg + dummpy.ve_both + os + eu, family = "binomial", data = checklist_Invspc_new_02_woody)

step(fit.lm01_woody)
step(fit.lm02_woody)
step(fit.glm01_woody)
step(fit.glm02_woody)

# ==========================================================================
# model without correlations
# specific models designed based on global goodness of fit and AICs
# ==========================================================================
# the results of piecewise SEMs are calculated via a updated version of package ‘piecewiseSEM’ (version 2.2.0)
# website: https://github.com/jslefche/piecewiseSEM
# library(devtools)
# install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE)

# # appendix table 4
# # short_lived
# # AIC = 1634.027, it will be changed, if you used the new version of piecewise SEM provided by github.
# mod_short_lived02 <- psem(lm(bg ~ eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_short_lived),
#                           lm(pr ~ bg + eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_short_lived),
#                           glm(os ~ pr + eu + cs, family = "binomial", data = checklist_Invspc_new_02_short_lived),
#                           glm(status ~ bg + pr + os + eu + cs + nr, family = "binomial", data = checklist_Invspc_new_02_short_lived))

# summary(mod_short_lived02, conserve = T)
# AIC(mod_short_lived02)
# write.csv(coefs(mod_short_lived02),"D:/R_codes_for_nat.pla_in.China/Results/SEMs_for_short.lived.csv", row.names = FALSE)



# # appendix table 5
# # long_lived
# # AIC = 7646.291
# mod_long_lived02 <- psem(lm(bg ~ eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_long_lived),
#                          lm(pr ~ bg + eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_long_lived),
#                          glm(os ~ bg + pr + eu + cs + he, family = "binomial", data = checklist_Invspc_new_02_long_lived),
#                          glm(status ~ bg + pr + os + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, family = "binomial", data = checklist_Invspc_new_02_long_lived))

# summary(mod_long_lived02, conserve = T)
# AIC(mod_long_lived02)
# write.csv(coefs(mod_long_lived02),"D:/R_codes_for_nat.pla_in.China/Results/SEMs_for_long.lived.csv", row.names = FALSE)


# # appendix table 6
# # woody
# # AIC = 10218.87
# mod_woody02 <- psem(lm(bg ~ eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_woody),
#                     lm(pr ~ bg + eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_woody),
#                     glm(os ~ bg + pr + eu + nr + dummpy.ve_veg + dummpy.ve_both, family = "binomial", data = checklist_Invspc_new_02_woody),
#                     glm(status ~ bg + pr + os + eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, family = "binomial", data = checklist_Invspc_new_02_woody))

# summary(mod_woody02, conserve = T)
# AIC(mod_woody02)
# write.csv(coefs(mod_woody02),"D:/R_codes_for_nat.pla_in.China/Results/SEMs_for_woody.csv", row.names = FALSE)

# ==========================================================================
# SEMs plot
# ==========================================================================
# SEM plots were manually made, using diagrams.net (formerly draw.io) software.
# website: https://app.diagrams.net/

# ==========================================================================
# model with correlations
# specific models designed based on global goodness of fit and AICs
# ==========================================================================
# appendix table 4
# short_lived
# https://jslefche.github.io/piecewiseSEM/articles/piecewiseSEM.html
mod_short_lived02_with.correlation <- psem(lm(bg ~ eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_short_lived),
                                           lm(pr ~ eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_short_lived),
                                           glm(os ~ eu + cs + he, family = "binomial", data = checklist_Invspc_new_02_short_lived),
                                           glm(status ~ bg + pr + os + eu + cs + nr, family = "binomial", data = checklist_Invspc_new_02_short_lived),
                                           pr %~~% bg,
                                           os %~~% bg,
                                           os %~~% pr)

summary(mod_short_lived02_with.correlation, conserve = T)
AIC(mod_short_lived02_with.correlation)
write.csv(coefs(mod_short_lived02_with.correlation),"D:/R_codes_for_nat.pla_in.China/Results20230523/SEMs_for_short.lived_with.correlation.csv", row.names = FALSE)


# appendix table 5
# long_lived
mod_long_lived02_with.correlation <- psem(lm(bg ~ eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_long_lived),
                                          lm(pr ~ eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_long_lived),
                                          glm(os ~ eu + cs + he, family = "binomial", data = checklist_Invspc_new_02_long_lived),
                                          glm(status ~ bg + pr + os + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, family = "binomial", data = checklist_Invspc_new_02_long_lived),
                                          pr %~~% bg,
                                          os %~~% bg,
                                          os %~~% pr)

summary(mod_long_lived02_with.correlation, conserve = T)
AIC(mod_long_lived02_with.correlation)
write.csv(coefs(mod_long_lived02_with.correlation),"D:/R_codes_for_nat.pla_in.China/Results20230523/SEMs_for_long.lived_with.correlation.csv", row.names = FALSE)


# appendix table 6
# woody
mod_woody02_with.correlation02 <- psem(lm(bg ~ eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_woody),
                                       lm(pr ~ eu + cs + dummpy.ve_veg + dummpy.ve_both + he, data = checklist_Invspc_new_02_woody),
                                       glm(os ~ eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, family = "binomial", data = checklist_Invspc_new_02_woody),
                                       glm(status ~ bg + pr + os + eu + cs + nr + dummpy.ve_veg + dummpy.ve_both + he, family = "binomial", data = checklist_Invspc_new_02_woody),
                                       pr %~~% bg,
                                       os %~~% bg,
                                       os %~~% pr)

summary(mod_woody02_with.correlation02, conserve = T)
AIC(mod_woody02_with.correlation02)
write.csv(coefs(mod_woody02_with.correlation02),"D:/R_codes_for_nat.pla_in.China/Results20230523/SEMs_for_woody_with.correlation.csv", row.names = FALSE)


# ==========================================================================
# SEMs plot
# ==========================================================================
# SEM plots were manually made, using diagrams.net (formerly draw.io) software.
# website: https://app.diagrams.net/
