# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by: dbc
# @Last Modified time: 2023-05-25 00:26:34
# @Description: separate glms in table 1 and appendix table 2
# ==========================================================================
# abbreviations of variables
# bg_num: number of botanical gardens
# chklst_prov_num: number of provinces
# new_sum.L3.code: number of tdwg level-03 regions, native range size
# residence_time_to_2020: minimum residence time
# Height_max_max: maximum height
# hab_suit_mean: climatic suitability, 0-1
# Life_form_new: life form, including (1) short-lived herbs, (2) long-lived herbs, and (3) woody
# prop_type: propagation mode, including (1) seed, (2) vegetative, and (3) both
# ali_new_status: status of online nursery availability, yes or no
# wcup_eco_use_num: number of economic use category, 0-10 categories
# wcup_eco_use_status: status of economic use, yes or no
# ==========================================================================
# preparation work
# ==========================================================================
# cleaning memory
cat("\014")
rm(list=ls())
gc()

# loading packages
library(ciTools)
library(data.table)
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(patchwork)
library(plyr)
library(rcompanion)
library(readr)
library(rlist)
library(scales)
library(dplyr)

# Define the directory and file path
file.path <- "D:/R_codes_for_nat.pla_in.China"

# Check if the file exists
if (!file.exists(file.path(file.path))) {
  # File does not exist, create it
  cat("Creating file...\n")
  file.create(file.path(file.path))
} else {
  # File already exists, display a message
  cat("File already exists.\n")
}

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# importing data, checklist_nat.pla_in_China
checklist_Invspc_new <- fread("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv")

# separate density plots
# --------------------------------------------------------------------------
ggdensity.bg_num                 <- ggdensity(checklist_Invspc_new$bg_num, main = "Density plot", xlab = "bg_num")
ggdensity.chklst_prov_num        <- ggdensity(checklist_Invspc_new$chklst_prov_num, main = "Density plot", xlab = "chklst_prov_num")
ggdensity.new_sum.L3.code        <- ggdensity(checklist_Invspc_new$new_sum.L3.code, main = "Density plot", xlab = "new_sum.L3.code")
ggdensity.residence_time_to_2020 <- ggdensity(checklist_Invspc_new$residence_time_to_2020, main = "Density plot", xlab = "residence_time_to_2020")
ggdensity.Height_max_max         <- ggdensity(checklist_Invspc_new$Height_max_max, main = "Density plot", xlab = "Height_max_max")
ggdensity.hab_suit_mean          <- ggdensity(checklist_Invspc_new$hab_suit_mean, main = "Density plot", xlab = "hab_suit_mean")

# # --------------------------------------------------------------------------
#  ggdensity.bg_num + ggdensity.chklst_prov_num + ggdensity.new_sum.L3.code + ggdensity.residence_time_to_2020 + ggdensity.Height_max_max + ggdensity.hab_suit_mean

# separate qq plots
ggqqplot.bg_num                 <- ggqqplot(checklist_Invspc_new$bg_num, main = "QQ plot", xlab = "bg_num")
ggqqplot.chklst_prov_num        <- ggqqplot(checklist_Invspc_new$chklst_prov_num, main = "QQ plot", xlab = "chklst_prov_num")
ggqqplot.new_sum.L3.code        <- ggqqplot(checklist_Invspc_new$new_sum.L3.code, main = "QQ plot", xlab = "new_sum.L3.code")
ggqqplot.residence_time_to_2020 <- ggqqplot(checklist_Invspc_new$residence_time_to_2020, main = "QQ plot", xlab = "residence_time_to_2020")
ggqqplot.Height_max_max         <- ggqqplot(checklist_Invspc_new$Height_max_max, main = "QQ plot", xlab = "Height_max_max")
ggqqplot.hab_suit_mean          <- ggqqplot(checklist_Invspc_new$hab_suit_mean, main = "QQ plot", xlab = "hab_suit_mean")

# # --------------------------------------------------------------------------
# ggqqplot.bg_num + ggqqplot.chklst_prov_num + ggqqplot.new_sum.L3.code + ggqqplot.residence_time_to_2020 + ggqqplot.Height_max_max + ggqqplot.hab_suit_mean

# ==========================================================================
# data transformation
# ==========================================================================
# check data range
range(checklist_Invspc_new$bg_num, na.rm = TRUE)
range(checklist_Invspc_new$chklst_prov_num, na.rm = TRUE)
range(checklist_Invspc_new$new_sum.L3.code, na.rm = TRUE)
range(checklist_Invspc_new$residence_time_to_2020, na.rm = TRUE)
range(checklist_Invspc_new$Height_max_max, na.rm = TRUE)
range(checklist_Invspc_new$hab_suit_mean, na.rm = TRUE)

# > range(checklist_Invspc_new$bg_num, na.rm = TRUE)
# [1]  0 26
# > range(checklist_Invspc_new$chklst_prov_num, na.rm = TRUE)
# [1]  0 33
# > range(checklist_Invspc_new$new_sum.L3.code, na.rm = TRUE)
# [1]   1 212
# > range(checklist_Invspc_new$residence_time_to_2020, na.rm = TRUE)
# [1]   3 156
# > range(checklist_Invspc_new$Height_max_max, na.rm = TRUE)
# [1]   0.01 115.00
# > range(checklist_Invspc_new$hab_suit_mean, na.rm = TRUE)
# [1] 0.084 0.574

# scale the data after log-transformation
checklist_Invspc_new$bg_num                 <- scale(log(checklist_Invspc_new$bg_num + 1))
checklist_Invspc_new$chklst_prov_num        <- scale(log(checklist_Invspc_new$chklst_prov_num + 1))
checklist_Invspc_new$new_sum.L3.code        <- scale(log(checklist_Invspc_new$new_sum.L3.code))
checklist_Invspc_new$residence_time_to_2020 <- scale(log(checklist_Invspc_new$residence_time_to_2020))
checklist_Invspc_new$hab_suit_mean          <- scale(log(checklist_Invspc_new$hab_suit_mean + 0.001))

checklist_Invspc_new <- data.frame(checklist_Invspc_new)

# we will transform the height based on life form later
# checklist_Invspc_new$Height_max_max_scaled  <- scale(log(checklist_Invspc_new$Height_max_max))

# check the data structure
glimpse(checklist_Invspc_new)
View(checklist_Invspc_new)

# # summary of naturalization status
# table(checklist_Invspc_new$naturalized_status)

ggdensity(checklist_Invspc_new$bg_num, main = "Density plot", xlab = "bg_num")
ggdensity(checklist_Invspc_new$chklst_prov_num, main = "Density plot", xlab = "chklst_prov_num")
ggdensity(checklist_Invspc_new$new_sum.L3.code, main = "Density plot", xlab = "new_sum.L3.code")
ggdensity(checklist_Invspc_new$residence_time_to_2020, main = "Density plot", xlab = "residence_time_to_2020")
ggdensity(checklist_Invspc_new$Height_max_max, main = "Density plot", xlab = "Height_max_max")
ggdensity(checklist_Invspc_new$hab_suit_mean, main = "Density plot", xlab = "hab_suit_mean")


# ==========================================================================
# separate glms process
# ==========================================================================
# --------------------------------------------------------------------------
# table 1
# number of botanical gardens, bg_num (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of bg_num
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$bg_num)])
range(checklist_Invspc_new$bg_num[!is.na(checklist_Invspc_new$bg_num)])

# model fitting
# logistic regression
# http://rcompanion.org/rcompanion/e_06.html

model_bg <- glm(naturalized_status ~ bg_num,
                family = binomial(link = "cloglog"),
                data = checklist_Invspc_new)

summary_bg <- summary(model_bg)
(summary_bg)

# --------------------------------------------------------------------------
# table 1
# number of provinces, chklst_prov_num (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of chklst_prov_num
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$chklst_prov_num)])
range(checklist_Invspc_new$chklst_prov_num[!is.na(checklist_Invspc_new$chklst_prov_num)])

# model fitting
model_prov <- glm(naturalized_status ~ chklst_prov_num,
                  family = binomial(link = "cloglog"),
                  data = checklist_Invspc_new)

summary_prov <- summary(model_prov)
(summary_prov)

# --------------------------------------------------------------------------
# table 1
# native range size, number of tdwg level-3 regions, new_sum.L3.code (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of new_sum.L3.code
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$new_sum.L3.code)])

# model fitting
model_tdwg_lvl3 <- glm(naturalized_status ~ new_sum.L3.code,
                       family = binomial(link = "cloglog"),
                       data = checklist_Invspc_new[!is.na(checklist_Invspc_new$new_sum.L3.code),])

summary_tdwg_lvl3 <- summary(model_tdwg_lvl3)
(summary_tdwg_lvl3)

# --------------------------------------------------------------------------
# table 1
# min. residence time, time length between the first record year and 2020,
# residence_time_to_2020 (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of residence_time_to_2020
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$residence_time_to_2020)])

# model fitting
model_res_time <- glm(naturalized_status ~ residence_time_to_2020,
                      family = binomial(link = "cloglog"),
                      data = checklist_Invspc_new[!is.na(checklist_Invspc_new$residence_time_to_2020),])

summary_res_time <- summary(model_res_time)
(summary_res_time)

# --------------------------------------------------------------------------
# table 1
# climatic suitability, hab_suit_mean (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of hab_suit_mean
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$hab_suit_mean)])

# model fitting
model_hab_suit <- glm(naturalized_status ~ hab_suit_mean,
                      family = binomial(link = "cloglog"),
                      data = checklist_Invspc_new[!is.na(checklist_Invspc_new$hab_suit_mean),])

summary_hab_suit <- summary(model_hab_suit)
(summary_hab_suit)

# --------------------------------------------------------------------------
# table 1
# maximum height, Height_max_max (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of Height_max_max
# filter out the data on Height_max_max without life form
checklist_Invspc_height_scaled <- checklist_Invspc_new %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_height_scaled %>% select(naturalized_status, Height_max_max, Life_form_new))

# modify Life_form_new as the ordered factors with (3) woody as baseline
checklist_Invspc_height_scaled$Life_form_new <- factor(checklist_Invspc_height_scaled$Life_form_new)
checklist_Invspc_height_scaled$Life_form_new <- relevel(checklist_Invspc_height_scaled$Life_form_new, ref = "3")

# based on category of Life_form_new，log-transform and scale Height_max_max
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "1")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "1")]))
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "2")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "2")]))
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "3")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "3")]))

# model fitting
model_height  <- glm(naturalized_status ~ Height_max_max,
                     family = binomial(link = "cloglog"),
                     data = checklist_Invspc_height_scaled[!is.na(checklist_Invspc_height_scaled$Height_max_max), ])

summary_height <- summary(model_height)
(summary_height)

# --------------------------------------------------------------------------
# table 1
# life form, Life_form_new (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates of life form
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$Life_form_new)])

# modify Life_form_new as the ordered factors with (3) woody as baseline
checklist_Invspc_new$Life_form_new <- factor(checklist_Invspc_new$Life_form_new)
checklist_Invspc_new$Life_form_new <- relevel(checklist_Invspc_new$Life_form_new, ref = "3")

# show levels of life form
levels(checklist_Invspc_new$Life_form_new)

# model fitting
model_lf_form <- glm(naturalized_status ~ factor(Life_form_new),
                     family = binomial(link = "cloglog"),
                     data = checklist_Invspc_new[!is.na(checklist_Invspc_new$Life_form_new), ])

summary_lf_form <- summary(model_lf_form)
(summary_lf_form)

# --------------------------------------------------------------------------
# table 1
# propagation mode, prop_type (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates of propagation
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$prop_type)])

# model fitting
model_prop_type <- glm(naturalized_status ~ factor(prop_type),
                       family = binomial(link = "cloglog"),
                       data = checklist_Invspc_new[!is.na(checklist_Invspc_new$prop_type), ])

summary_prop_type <- summary(model_prop_type)
(summary_prop_type)

# --------------------------------------------------------------------------
# table 1
# online nursery availability, ali_new_status (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates of online nursery availability
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$ali_new_status)])

# model fitting
model_ali_shops <- glm(naturalized_status ~ factor(ali_new_status),
                       family = binomial(link = "cloglog"),
                       data = checklist_Invspc_new[!is.na(checklist_Invspc_new$ali_new_status), ])

summary_ali_shops <- summary(model_ali_shops)
(summary_ali_shops)

# --------------------------------------------------------------------------
# table 1
# economic use, wcup_eco_use_status (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates of economic use
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$wcup_eco_use_num)])

# because the raw data included the category of economic use (0-10), rather the status of economic use (0 and 1),
# we need transform the category to status before analysis
checklist_Invspc_new <- checklist_Invspc_new %>%
                         mutate(wcup_eco_use_status = case_when(wcup_eco_use_num == 0 ~ 0,
                                                                wcup_eco_use_num > 0 ~ 1))

# model fitting
model_eco_use <- glm(naturalized_status ~ factor(wcup_eco_use_status),
                     family = binomial(link = "cloglog"),
                     data = checklist_Invspc_new[!is.na(checklist_Invspc_new$wcup_eco_use_status), ])

summary_eco_use <- summary(model_eco_use)
(summary_eco_use)

# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by:
# @Last Modified time: 2022-03-20 10:42:43
# @Description: separate glm in appendix table 2
# ==========================================================================
# --------------------------------------------------------------------------
# appendix table 2
# interaction between height and life form
# --------------------------------------------------------------------------
# check the replicates and data range of Height_max_max
# filter out the data on Height_max_max without life form
checklist_Invspc_interaction <- checklist_Invspc_new %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
# skimr::skim(checklist_Invspc_interaction %>% select(naturalized_status, Height_max_max, Life_form_new))

# modify Life_form_new as the ordered factors with (3) woody as reference
checklist_Invspc_interaction$Life_form_new <- factor(checklist_Invspc_interaction$Life_form_new)
checklist_Invspc_interaction$Life_form_new <- relevel(checklist_Invspc_interaction$Life_form_new, ref = "3")

# based on category of Life_form_new，log-transform and scale Height_max_max
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")]))
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")]))
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")]))

# model fitting
model_height_interaction <- glm(naturalized_status ~ Height_max_max + Life_form_new + Height_max_max:Life_form_new,
                                family = binomial(link = "cloglog"),
                                data = checklist_Invspc_interaction)

summary_height_interaction <- summary(model_height_interaction)
(summary_height_interaction)

# ==========================================================================
# calculate pseudo R squared of glms in table 1 and appendix table 1
# ==========================================================================
# # loading library
# library(rcompanion)

# # obtain pseudo r-squared values for separate models
# nagelkerke(model_bg)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_prov)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_ali_shops)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_eco_use)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_hab_suit)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_tdwg_lvl3)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_res_time)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_lf_form)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_prop_type)$Pseudo.R.squared.for.model.vs.null
# nagelkerke(model_height)$Pseudo.R.squared.for.model.vs.null

nag.R2_bg        <- nagelkerke(model_bg)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_prov      <- nagelkerke(model_prov)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_tdwg_lvl3 <- nagelkerke(model_tdwg_lvl3)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_res_time  <- nagelkerke(model_res_time)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_hab_suit  <- nagelkerke(model_hab_suit)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_height    <- nagelkerke(model_height)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_lf_form   <- nagelkerke(model_lf_form)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_prop_type <- nagelkerke(model_prop_type)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_ali_shops <- nagelkerke(model_ali_shops)$Pseudo.R.squared.for.model.vs.null[3]
nag.R2_eco_use   <- nagelkerke(model_eco_use)$Pseudo.R.squared.for.model.vs.null[3]

# bind separate pseudo r-squared values
nag.R2_summary <- rbind(nag.R2_bg = round(nag.R2_bg, 3),
                        nag.R2_prov = round(nag.R2_prov, 3),
                        nag.R2_ali_shops = round(nag.R2_ali_shops, 3),
                        nag.R2_eco_use = round(nag.R2_eco_use, 3),
                        nag.R2_hab_suit = round(nag.R2_hab_suit, 3),
                        nag.R2_tdwg_lvl3 = round(nag.R2_tdwg_lvl3, 3),
                        nag.R2_res_time = round(nag.R2_res_time, 3),
                        nag.R2_lf_form = round(nag.R2_lf_form, 3),
                        nag.R2_prop_type = round(nag.R2_prop_type, 3),
                        nag.R2_height = round(nag.R2_height, 3))

# save results of pseudo r-squared values
nag.R2_summary <- data.frame(nag.R2_summary)
write.csv(nag.R2_summary, "D:/R_codes_for_nat.pla_in.China/Results20230523/Table.1_nag.R2_summary.csv")

# ==========================================================================
# summary of separate glms in table 1 and appendix table 1
# ==========================================================================
# summarize the output from separate glms
summary_bg_table          <- cbind(summary_bg$coefficients, model.var = "No. botanical gardens")
summary_prov_table        <- cbind(summary_prov$coefficients, model.var = "No. provinces")
summary_ali_shops_table   <- cbind(summary_ali_shops$coefficients, model.var = "Online-nursery availability")
summary_eco_use_table     <- cbind(summary_eco_use$coefficients, model.var = "Economic use")
summary_hab_suit_table    <- cbind(summary_hab_suit$coefficients, model.var = "Climatic suitability")
summary_tdwg_lvl3_table   <- cbind(summary_tdwg_lvl3$coefficients, model.var = "Native range size")
summary_res_time_table    <- cbind(summary_res_time$coefficients, model.var = "Min. residence time")
summary_lf_form_table     <- cbind(summary_lf_form$coefficients, model.var = "Life form")
summary_prop_type_table   <- cbind(summary_prop_type$coefficients, model.var = "Propagation mode")
summary_height_table      <- cbind(summary_height$coefficients, model.var = "Maximum height")
summary_interaction_table <- cbind(summary_height_interaction$coefficients, model.var = "Interaction between max. height and life form")

summary_output <- rlist::list.rbind(list(summary_bg_table,
                                         summary_prov_table,
                                         summary_ali_shops_table,
                                         summary_eco_use_table,
                                         summary_hab_suit_table,
                                         summary_tdwg_lvl3_table,
                                         summary_res_time_table,
                                         summary_lf_form_table,
                                         summary_prop_type_table,
                                         summary_height_table,
                                         summary_interaction_table))

(summary_output)

# # save results of separate glms with_intecept
# summary_output <- data.frame(summary_output)
# write.csv(summary_output, "20220320_table.1_glm_summary_output_with_intecept.csv")

# removing "Intercept" part
summary_output     <- data.frame(summary_output)
summary_output.new <- summary_output %>% filter(!grepl("Intercept", row.names(summary_output)))

# # save results of separate glms without_intecept
# clipr::write_clip(summary_output.new)
write.csv(summary_output.new, "D:/R_codes_for_nat.pla_in.China/Results20230523/Table.1.and.Appendix.table.2_without_intercept.csv")

#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-07 22:20:25
# @Last Modified by:
# @Last Modified time: 2021-01-12 11:30:05
# @Description: fig. 2
#--------------------------------------------------------------------------#
# loading packages
library(ciTools)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(scales)

# # showing fitted glm models as listed above.
# these will be used to simulate the confidence intervals for each variable in plots
# model_bg
# model_prov
# model_tdwg_lvl3
# model_res_time
# model_hab_suit
# model_height
# model_lf_form
# model_prop_type
# model_ali_shops
# model_eco_use
# model_height_interaction

# ==========================================================================
# plots for continuous variables
# ==========================================================================
# --------------------------------------------------------------------------
# fig.2a
# number of botanical gardens
# --------------------------------------------------------------------------
# check the replicates of number of botanical gardens
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$bg_num)])
range(checklist_Invspc_new$bg_num[!is.na(checklist_Invspc_new$bg_num)])

# plotting
plot_bg_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$bg_num), ],
                     aes(x = bg_num, y = naturalized_status)) +
              geom_rangeframe(data = data.frame(bg_num = c(-2, 4),
                                                naturalized_status = c(0, 1)),
                                                size = 1) +
              theme(panel.background = element_rect(fill = NA),
                    # axis.line = element_line(size = 1, linetype = "solid"),
                    axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                    axis.text = element_text(family = "serif", colour = "black", size = 14),
                    axis.title = element_text(family = "serif", colour = "black", size = 14)) +
              scale_y_continuous(limits = c(-0.025, 1.025), breaks = seq(0, 1, by = 0.2)) +
              scale_x_continuous(limits = c(-2, 4), breaks = seq(-2, 4, by = 1))+
              labs(x = "Log(no. botanical gardens + 1)(scaled)", y = "Naturalization success") +
              coord_cartesian(ylim = c(0, 1))

(plot_bg_01)

# create prediction + confidence intervals, using ciTools package
model_bg_var  <- data.frame(bg_num = seq(-2, 4, 6/1000))
model_bg_pred <- add_ci(model_bg_var, model_bg, response = TRUE)
(model_bg_pred)

plot_bg_02 <- plot_bg_01 +
              geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.7) +
              geom_line(data = model_bg_pred, aes(x = bg_num, y = pred), size = 1, colour = "black") +
              geom_line(data = model_bg_pred, aes(x = bg_num, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
              geom_line(data = model_bg_pred, aes(x = bg_num, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_bg_02)

# --------------------------------------------------------------------------
# fig.2b
# number of provinces
# --------------------------------------------------------------------------
# check the replicates of number of provinces
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$chklst_prov_num)])
range(checklist_Invspc_new$chklst_prov_num[!is.na(checklist_Invspc_new$chklst_prov_num)])

# plotting
plot_prov_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$chklst_prov_num), ],
                       aes(x = chklst_prov_num, y = naturalized_status)) +
                geom_rangeframe(data = data.frame(chklst_prov_num = c(-2, 4),
                                                  naturalized_status = c(0, 1)),
                                                  size = 1) +
                theme(panel.background = element_rect(fill = NA),
                      # axis.line = element_line(size = 1, linetype = "solid"),
                      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                      axis.text = element_text(family = "serif", colour = "black", size = 14),
                      axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                scale_y_continuous(limits = c(-0.25, 1.25), breaks = seq(0, 1, by = 0.2)) +
                scale_x_continuous(limits = c(-2, 4), breaks = seq(-2, 4, by = 1)) +
                labs(x = "Log(no. provinces + 1)(scaled)", y = "Naturalization success") +
                coord_cartesian(ylim = c(0, 1))

(plot_prov_01)

# create prediction + confidence intervals, using ciTools package
model_prov_var  <- data.frame(chklst_prov_num = seq(-2, 4, 6/1000))
model_prov_pred <- add_ci(model_prov_var, model_prov, response = TRUE)
(model_prov_pred)

plot_prov_02 <- plot_prov_01 +
                geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.7) +
                geom_line(data = model_prov_pred, aes(x = chklst_prov_num, y = pred), size = 1, colour = "black") +
                geom_line(data = model_prov_pred, aes(x = chklst_prov_num, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
                geom_line(data = model_prov_pred, aes(x = chklst_prov_num, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_prov_02)

# --------------------------------------------------------------------------
# fig.2e
# min. residence time
# --------------------------------------------------------------------------
# check the replicates of min. residence time
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$residence_time_to_2020)])
range(checklist_Invspc_new$residence_time_to_2020[!is.na(checklist_Invspc_new$residence_time_to_2020)])

# plotting
plot_res_time_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$residence_time_to_2020), ],
                           aes(x = residence_time_to_2020, y = naturalized_status)) +
                    geom_rangeframe(data = data.frame(residence_time_to_2020 = c(-4, 2),
                                                      naturalized_status = c(0, 1)),
                                                      size = 1) +
                    theme(panel.background = element_rect(fill = NA),
                          # axis.line = element_line(size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                          axis.text = element_text(family = "serif", colour = "black", size = 14),
                          axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                    scale_y_continuous(limits = c(-0.025, 1.025), breaks = seq(0, 1, by = 0.2)) +
                    scale_x_continuous(limits = c(-4, 2), breaks = seq(-4, 2, by = 1))+
                    labs(x = "Log(min. residence time, year)(scaled)", y = "Naturalization success") +
                    coord_cartesian(ylim = c(0, 1))

(plot_res_time_01)

# create prediction + confidence intervals, using ciTools package
model_res_time_var  <- data.frame(residence_time_to_2020 = seq(-4, 2, 6/1000))
model_res_time_pred <- add_ci(model_res_time_var, model_res_time, response = TRUE)
(model_res_time_pred)

plot_res_time_02 <- plot_res_time_01 +
                    geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.7) +
                    geom_line(data = model_res_time_pred, aes(x = residence_time_to_2020, y = pred), size = 1, colour = "black") +
                    geom_line(data = model_res_time_pred, aes(x = residence_time_to_2020, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
                    geom_line(data = model_res_time_pred, aes(x = residence_time_to_2020, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_res_time_02)

# --------------------------------------------------------------------------
# fig.2g
# native range size
# --------------------------------------------------------------------------
# check the replicates of native range size
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$new_sum.L3.code)])
range(checklist_Invspc_new$new_sum.L3.code[!is.na(checklist_Invspc_new$new_sum.L3.code)])

# plotting
plot_native_range_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$new_sum.L3.code), ],
                               aes(x = new_sum.L3.code, y = naturalized_status)) +
                        geom_rangeframe(data = data.frame(new_sum.L3.code = c(-2, 4),
                                                          naturalized_status = c(0, 1)),
                                                          size = 1) +
                        theme(panel.background = element_rect(fill = NA),
                              # axis.line = element_line(size = 1, linetype = "solid"),
                              axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                              axis.text = element_text(family = "serif", colour = "black", size = 14),
                              axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                        scale_y_continuous(limits = c(-0.025, 1.025), breaks = seq(0, 1, by = 0.2)) +
                        scale_x_continuous(limits = c(-2, 4), breaks = seq(-2, 4, by = 1)) +
                        labs(x = "Native range size\nLog(no. TDWG level-3 regions)(scaled)", y = "Naturalization success") +
                        coord_cartesian(ylim = c(0, 1))

(plot_native_range_01)

# create prediction + confidence intervals, using ciTools package
model_tdwg_var  <- data.frame(new_sum.L3.code = seq(-2, 4, 6/1000))
model_tdwg_pred <- add_ci(model_tdwg_var, model_tdwg_lvl3, response = TRUE)
(model_tdwg_pred)

plot_native_range_02 <- plot_native_range_01 +
                        geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.7) +
                        geom_line(data = model_tdwg_pred, aes(x = new_sum.L3.code, y = pred), size = 1, colour = "black") +
                        geom_line(data = model_tdwg_pred, aes(x = new_sum.L3.code, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
                        geom_line(data = model_tdwg_pred, aes(x = new_sum.L3.code, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_native_range_02)

# --------------------------------------------------------------------------
# fig.2f
# climatic suitability
# --------------------------------------------------------------------------
# check the replicates of climatic suitability
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$hab_suit_mean)])
range(checklist_Invspc_new$hab_suit_mean[!is.na(checklist_Invspc_new$hab_suit_mean)])

# plotting
plot_hab_suit_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$hab_suit_mean), ],
                           aes(x = hab_suit_mean, y = naturalized_status)) +
                    geom_rangeframe(data = data.frame(hab_suit_mean = c(-2, 4.2),
                                                      naturalized_status = c(0, 1)),
                                                      size = 1) +
                    theme(panel.background = element_rect(fill = NA),
                          # axis.line = element_line(size = 1, linetype = "solid"),
                          axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                          axis.text = element_text(family = "serif", colour = "black", size = 14),
                          axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                    scale_y_continuous(limits = c(-0.025, 1.025), breaks = seq(0, 1, by = 0.2)) +
                    scale_x_discrete(limits = seq(-2, 4.2, by = 1)) +
                    labs(x = "Log(climatic suitability + 0.001)(scaled)", y = "Naturalization success") +
                    coord_cartesian(ylim = c(0, 1))

(plot_hab_suit_01)

# create prediction + confidence intervals, using ciTools package
model_hab_suit_var  <- data.frame(hab_suit_mean = seq(-2, 4.2, 5.2/1000))
model_hab_suit_pred <- add_ci(model_hab_suit_var, model_hab_suit, response = TRUE)
(model_hab_suit_pred)

plot_hab_suit_02 <- plot_hab_suit_01 +
                    geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.5) +
                    geom_line(data = model_hab_suit_pred, aes(x = hab_suit_mean, y = pred), size = 1, colour = "black") +
                    geom_line(data = model_hab_suit_pred, aes(x = hab_suit_mean, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
                    geom_line(data = model_hab_suit_pred, aes(x = hab_suit_mean, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_hab_suit_02)

# --------------------------------------------------------------------------
# fig.2j
# interaction between height and life form
# --------------------------------------------------------------------------
# check the replicates of data on height with life form
table(checklist_Invspc_interaction$naturalized_status[!is.na(checklist_Invspc_interaction$Height_max_max)])
range(checklist_Invspc_interaction$Height_max_max, na.rm = TRUE)

# # loading color packages
# library(RColorBrewer)
# display.brewer.all()
# color03 <- brewer.pal(3, "Set1")

# # select colors for each life form
# library(scales)
color03 <- c("red", "blue", "black")
show_col(color03)

# check the replicates of data on life form with height
checklist_Invspc_interaction$Life_form_new <- factor(checklist_Invspc_interaction$Life_form_new, order = TRUE, levels = c(1, 2, 3))
levels(checklist_Invspc_interaction$Life_form_new)


# user-defined jitter positions for each life form
checklist_Invspc_interaction <- checklist_Invspc_interaction %>% mutate(jitter.position = case_when(naturalized_status == 1 & Life_form_new == 1 ~ 1.08,
                                                                                                    naturalized_status == 1 & Life_form_new == 2 ~ 1.04,
                                                                                                    naturalized_status == 1 & Life_form_new == 3 ~ 1.00,
                                                                                                    naturalized_status == 0 & Life_form_new == 1 ~ 0.00,
                                                                                                    naturalized_status == 0 & Life_form_new == 2 ~ -0.04,
                                                                                                    naturalized_status == 0 & Life_form_new == 3 ~ -0.08))
table(checklist_Invspc_interaction$jitter.position)


# plotting
plot_interaction_01 <- ggplot(data = checklist_Invspc_interaction,
                              aes(x = Height_max_max, y = naturalized_status), group = factor(Life_form_new), colour = factor(Life_form_new)) +
                       geom_rangeframe(data = data.frame(Height_max_max = c(-4, 6),
                                                        naturalized_status = c(0, 1)),
                                                        size = 1) +
                       theme(panel.background = element_rect(fill = NA),
                             # axis.line = element_line(size = 1, linetype = "solid"),
                             axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                             axis.text = element_text(family = "serif", colour = "black", size = 14),
                             axis.title = element_text(family = "serif", colour = "black", size = 14),
                             legend.title = element_text(family = "serif", colour = "black", size = 14),
                             legend.text = element_text(family = "serif", colour = "black", size = 14),
                             legend.position = c(0.3, 0.68)) +
                       scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, by = 0.2)) +
                       scale_x_discrete(limits = seq(-4, 6, by = 2)) +
                       labs(x = "Log(max. height, cm)(scaled)", y = "Naturalization success") +
                       coord_cartesian(ylim = c(-0.1, 1.1))
(plot_interaction_01)

# create prediction + confidence intervals, using ciTools package
model_interaction_var <- expand.grid(Height_max_max = seq(-4, 6, 10/1000), Life_form_new = c(1, 2, 3))
model_interaction_var$Life_form_new <- as.factor(model_interaction_var$Life_form_new)

model_interaction_pred <- add_ci(model_interaction_var, model_height_interaction, response = TRUE)
(model_interaction_pred)

plot_interaction_02 <- plot_interaction_01 +
                       geom_jitter(aes(x = Height_max_max, y = jitter.position, colour = factor(Life_form_new)), shape = 1, size = 2, stroke = 0.5, width = 0, height = 0.005, alpha = 0.5) +
                       # geom_jitter(aes(colour = factor(Life_form_new)), shape = 1, size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.5) +
                       geom_line(data = model_interaction_pred, aes(x = Height_max_max, y = pred, colour = factor(Life_form_new)), size = 1) +
                       geom_line(data = model_interaction_pred, aes(x = Height_max_max, y = LCB0.025, colour = factor(Life_form_new)), size = 1, linetype = "dashed", alpha = 1) +
                       geom_line(data = model_interaction_pred, aes(x = Height_max_max, y = UCB0.975, colour = factor(Life_form_new)), size = 1, linetype = "dashed", alpha = 1) +
                       scale_colour_manual(values = color03, name = "Life form: ", labels = c("Short-lived", "Long-lived", "Woody"))

(plot_interaction_02)

# ==========================================================================
# output plots of continuous variables
# ==========================================================================
# library(patchwork)

# figure_01 <- plot_bg_02 +
#              plot_prov_02 +
#              plot_res_time_02 +
#              plot_native_range_02 +
#              plot_hab_suit_02 +
#              plot_interaction_02 +
#              plot_layout(ncol = 2) +
#              plot_annotation(tag_levels = "A")

# figure_01

# # figure_01 <- ggarrange(plot_bg_02,
# #                        plot_prov_02,
# #                        plot_res_time_02,
# #                        plot_native_range_02,
# #                        plot_hab_suit_02,
# #                        plot_height_01,
# #                        labels = c("A", "B", "C", "D", "E", "F"),
# #                        ncol = 2,
# #                        nrow = 3,
# #                        align = "hv")

# ggexport(figure_01, filename = "./2021_12_RESULTS/Figure_glm01.png",
#     width = 2400,
#     height = 3600,
#     pointsize = 12,
#     res = 300)

# ==========================================================================
# plots for discrete variables
# ==========================================================================
# --------------------------------------------------------------------------
# fig.2h
# life form
# --------------------------------------------------------------------------
# check the replicates of life form
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$Life_form_new)])
table(checklist_Invspc_new$Life_form_new[!is.na(checklist_Invspc_new$Life_form_new)])

# create a new data.frame only with naturalized_status and Life_form_new
data_life_form <- checklist_Invspc_new %>%
                  filter(!is.na(Life_form_new)) %>%
                  select(naturalized_status, Life_form_new)

# summarize the life form
# n_naturalized: number of naturalized plants for each life form
# n_not_naturalized: number of non-naturalized plants for each life form
# n_all_species: number of alien plants for each life form
# proportion: proportion of naturalized plants to alien plants for each life form
data_life_form_01 <- plyr::ddply(data_life_form, .(Life_form_new), summarise,
                                 n_naturalized = length(naturalized_status[naturalized_status == "1"]),
                                 n_not_naturalized = length(naturalized_status[naturalized_status == "0"]),
                                 n_all_species = n_naturalized + n_not_naturalized,
                                 proportion = n_naturalized/n_all_species)
(data_life_form_01)

# add a new column named lf_proportion,
# which is proportion of number of alien plants in each life form to number of alien plants in China
data_life_form_01$lf_proportion <- data_life_form_01$n_all_species/sum(data_life_form_01$n_all_species)
data_life_form_01 <- data.frame(data_life_form_01) %>% mutate_at(vars(Life_form_new), ~factor(., levels = c(1, 2, 3))) %>% arrange(Life_form_new)
(data_life_form_01)

# modify x-lab in the plot
x_axis_life_form <- c("1" = "Short-lived\nherb", "2" = "Long-lived\nherb", "3" = "Woody")

# plotting
plot_life_form_01 <- ggplot(data_life_form_01,
                            aes(x = factor(Life_form_new), y = proportion, width = lf_proportion)) +
                     geom_bar(colour = "transparent", fill= "grey", stat = "identity") +
                     geom_rangeframe(data = data.frame(Life_form_new = c(1, 3),
                                                       proportion = c(0, 0.4),
                                                       lf_proportion = c(0, 0.4)), size =1) +
                     theme(panel.background = element_rect(fill = NA),
                           # axis.line = element_line(size = 1, linetype = "solid"),
                           axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                           axis.text = element_text(family = "serif", colour = "black", size = 14),
                           axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                     scale_x_discrete(labels = x_axis_life_form) +
                     scale_y_continuous(limits = c(0, 0.4)) +
                     labs(x = "Life form", y = "Naturalization success")

(plot_life_form_01)


# create prediction + confidence intervals, using ciTools package
model_lf_form_var <- data.frame(Life_form_new = c(1, 2, 3))
(model_lf_form_var)
model_lf_form_pred <- add_ci(model_lf_form_var, model_lf_form, response = TRUE)
(model_lf_form_pred)

model_lf_form_pred02 <- model_lf_form_pred %>% mutate(proportion = pred, lf_proportion = data_life_form_01$lf_proportion)

# add error-bar of prediction values in the plot
plot_life_form_02 <- plot_life_form_01 +
                     geom_errorbar(data = model_lf_form_pred02, aes(ymin = LCB0.025, ymax = UCB0.975), colour = "blue", width = 0.2) +
                     geom_point(data = model_lf_form_pred02, mapping = aes(x = Life_form_new, y = proportion), size = 4, shape = 21, fill= "blue")

(plot_life_form_02)

# --------------------------------------------------------------------------
# fig.2i
# propagation mode
# --------------------------------------------------------------------------
# check the replicates of propagation mode
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$prop_type)])
table(checklist_Invspc_new$prop_type[!is.na(checklist_Invspc_new$prop_type)])

# create a new data.frame only with naturalized_status and prop_type
data_prop_type <- checklist_Invspc_new %>%
                  filter(!is.na(prop_type)) %>%
                  select(naturalized_status, prop_type) %>%
                  mutate_at(vars(prop_type), funs(as.factor))

# summarize the propagation mode
# n_naturalized: number of naturalized plants for each propagation mode
# n_not_naturalized: number of non-naturalized plants for each propagation mode
# n_all_species: number of alien plants for each propagation mode
# proportion: proportion of naturalized plants to alien plants for each propagation mode
data_prop_type_01 <- plyr::ddply(data_prop_type, .(prop_type), summarise,
                           n_naturalized = length(naturalized_status[naturalized_status == "1"]),
                           n_not_naturalized = length(naturalized_status[naturalized_status == "0"]),
                           n_all_species = n_naturalized + n_not_naturalized,
                           proportion = n_naturalized/n_all_species)
(data_prop_type_01)

# add a new column named pt_proportion,
# which is proportion of number of alien plants in propagation mode to number of alien plants in China
data_prop_type_01$pt_proportion <- data_prop_type_01$n_all_species/sum(data_prop_type_01$n_all_species)
(data_prop_type_01)

# modify x-lab in the plot
x_axis_prop_type <- c("1" = "Seed\n", "2" = "Vegetative\n", "3" = "Both\n")

# plotting
plot_prop_type_01 <- ggplot(data_prop_type_01,
                            aes(x = prop_type, y = proportion, width = pt_proportion)) +
                     geom_bar(colour = "transparent", fill= "grey", stat = "identity") +
                     geom_rangeframe(data = data.frame(prop_type = c(1, 3),
                                                       proportion = c(0, 0.2),
                                                       pt_proportion = c(0, 0.2)), size =1) +
                     theme(panel.background = element_rect(fill = NA),
                           # axis.line = element_line(size = 1, linetype = "solid"),
                           axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                           axis.text = element_text(family = "serif", colour = "black", size = 14),
                           axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                     scale_x_discrete(labels = x_axis_prop_type) +
                     scale_y_continuous(limits = c(0, 0.2)) +
                     labs(x = "Propagation mode", y = "Naturalization success")

(plot_prop_type_01)

# create prediction + confidence intervals, using ciTools package
model_prop_type_var <- data.frame(prop_type = c(1, 2, 3))
(model_prop_type_var)
model_prop_type_pred <- add_ci(model_prop_type_var, model_prop_type, response = TRUE)
(model_prop_type_pred)

model_prop_type_pred02 <- model_prop_type_pred %>% mutate(proportion = pred, pt_proportion = data_prop_type_01$pt_proportion)

# add error-bar of prediction values in the plot
plot_prop_type_02 <- plot_prop_type_01 +
                     geom_errorbar(data = model_prop_type_pred02, aes(ymin = LCB0.025, ymax = UCB0.975), colour = "blue", width = 0.2) +
                     geom_point(data = model_prop_type_pred02, mapping = aes(x= prop_type, y= proportion), size = 4, shape = 21, fill= "blue")

(plot_prop_type_02)

# --------------------------------------------------------------------------
# fig.2c
# online-nursery availability
# --------------------------------------------------------------------------
# check the replicates of online-nursery availability
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$ali_new_status)])
table(checklist_Invspc_new$ali_new_status[!is.na(checklist_Invspc_new$ali_new_status)])

# create a new data.frame only with naturalized_status and ali_new_status
data_ali_shops <- checklist_Invspc_new %>%
                  filter(!is.na(ali_new_status)) %>%
                  select(naturalized_status, ali_new_status) %>%
                  mutate_at(vars(ali_new_status), funs(as.factor))

# summarize the online-nursery availability
# n_naturalized: number of naturalized plants for each status of online-nursery availability
# n_not_naturalized: number of non-naturalized plants for each status of online-nursery availability
# n_all_species: number of alien plants for each status of online-nursery availability
# proportion: proportion of naturalized plants to alien plants for each status of online-nursery availability
data_ali_shops_01 <- plyr::ddply(data_ali_shops, .(ali_new_status), summarise,
                           n_naturalized = length(naturalized_status[naturalized_status == "1"]),
                           n_not_naturalized = length(naturalized_status[naturalized_status == "0"]),
                           n_all_species = n_naturalized + n_not_naturalized,
                           proportion = n_naturalized/n_all_species)
(data_ali_shops_01)

# add a new column named al_proportion,
# which is proportion of number of alien plants in propagation mode to number of alien plants in China
data_ali_shops_01$al_proportion <- data_ali_shops_01$n_all_species/sum(data_ali_shops_01$n_all_species)
(data_ali_shops_01)

# modify x-lab in the plot
x_axis_ali_shops <- c("0" = "No", "1" = "Yes")

# plotting
plot_ali_shops_01 <- ggplot(data_ali_shops_01,
                            aes(x = ali_new_status, y = proportion, width = al_proportion)) +
                     geom_bar(colour = "transparent", fill= "grey", stat = "identity") +
                     geom_rangeframe(data = data.frame(ali_new_status = c(1, 2),
                                                       proportion = c(0, 0.3),
                                                       al_proportion = c(0, 0.3)), size =1) +
                     theme(panel.background = element_rect(fill = NA),
                           # axis.line = element_line(size = 1, linetype = "solid"),
                           axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                           axis.text = element_text(family = "serif", colour = "black", size = 14),
                           axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                     scale_x_discrete(labels = x_axis_ali_shops) +
                     scale_y_continuous(limits = c(0, 0.3)) +
                     labs(x = "Online-nursery availability", y = "Naturalization success")

(plot_ali_shops_01)

# create prediction + confidence intervals, using ciTools package
model_ali_shops_var <- data.frame(ali_new_status = c(0, 1))
(model_ali_shops_var)
model_ali_shops_pred <- add_ci(model_ali_shops_var, model_ali_shops, response = TRUE)
(model_ali_shops_pred)

model_ali_shops_pred02 <- model_ali_shops_pred %>% mutate(proportion = pred, al_proportion = data_ali_shops_01$al_proportion)

# add error-bar of prediction values in the plot
plot_ali_shops_02 <- plot_ali_shops_01 +
                     geom_errorbar(data = model_ali_shops_pred02, aes(x = ali_new_status + 1, ymin = LCB0.025, ymax = UCB0.975), colour = "blue", width = 0.2) +
                     geom_point(data = model_ali_shops_pred02, mapping = aes(x = ali_new_status + 1, y = proportion), size = 4, shape = 21, fill= "blue")

(plot_ali_shops_02)

# --------------------------------------------------------------------------
# fig.2d
# economic use
# --------------------------------------------------------------------------
# check the replicates of economic use
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$wcup_eco_use_num)])
table(checklist_Invspc_new$wcup_eco_use_num[!is.na(checklist_Invspc_new$wcup_eco_use_num)])

# create a new data.frame only with naturalized_status and wcup_eco_use_status
data_eco_use <- checklist_Invspc_new %>%
                filter(!is.na(wcup_eco_use_num)) %>%
                mutate(wcup_eco_use_status = case_when(
                                          wcup_eco_use_num == 0 ~ 0,
                                          wcup_eco_use_num > 0 ~ 1
                                          ))

data_eco_use_01 <- data_eco_use %>%
                   select(naturalized_status, wcup_eco_use_status) %>%
                   mutate_at(vars(wcup_eco_use_status), funs(as.factor))

# table(data_eco_use_01$wcup_eco_use_status[!is.na(data_eco_use_01$wcup_eco_use_status)])

# summarize the online-nursery availability
# n_naturalized: number of naturalized plants for each status of economic use
# n_not_naturalized: number of non-naturalized plants for each status of economic use
# n_all_species: number of alien plants for each status of economic use
# proportion: proportion of naturalized plants to alien plants for each status of economic use
data_eco_use_02 <- plyr::ddply(data_eco_use_01, .(wcup_eco_use_status), summarise,
                         n_naturalized = length(naturalized_status[naturalized_status == "1"]),
                         n_not_naturalized = length(naturalized_status[naturalized_status == "0"]),
                         n_all_species = n_naturalized + n_not_naturalized,
                         proportion = n_naturalized/n_all_species)
(data_eco_use_02)

# add a new column named eu_proportion,
# which is proportion of number of alien plants in propagation mode to number of alien plants in China
data_eco_use_02$eu_proportion <- data_eco_use_02$n_all_species/sum(data_eco_use_02$n_all_species)
(data_eco_use_02)

# modify x-lab in the plot
x_axis_eco_use <- c("0" = "No", "1" = "Yes")

# plotting
plot_eco_use_01 <- ggplot(data_eco_use_02,
                          aes(x = wcup_eco_use_status, y = proportion, width = eu_proportion)) +
                   geom_bar(colour = "transparent", fill= "grey", stat = "identity") +
                   geom_rangeframe(data = data.frame(wcup_eco_use_status = c(1, 2),
                                                     proportion = c(0, 0.2),
                                                     eu_proportion = c(0, 0.2)), size =1) +
                   theme(panel.background = element_rect(fill = NA),
                          # axis.line = element_line(size = 1, linetype = "solid"),
                         axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                         axis.text = element_text(family = "serif", colour = "black", size = 14),
                         axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                   scale_x_discrete(labels = x_axis_eco_use) +
                   scale_y_continuous(limits = c(0, 0.2)) +
                   labs(x = "Economic use", y = "Naturalization success")

(plot_eco_use_01)

# create prediction + confidence intervals, using ciTools package
model_eco_use_var <- data.frame(wcup_eco_use_status = c(0, 1))
(model_eco_use_var)
model_eco_use_pred <- add_ci(model_eco_use_var, model_eco_use, response = TRUE)
(model_eco_use_pred)

model_eco_use_pred02 <- model_eco_use_pred %>% mutate(proportion = pred, eu_proportion = data_eco_use_02$eu_proportion)
model_eco_use_pred02

# add error-bar of prediction values in the plot
plot_eco_use_02 <- plot_eco_use_01 +
                   geom_errorbar(data = model_eco_use_pred02, aes(x = wcup_eco_use_status + 1, ymin = LCB0.025, ymax = UCB0.975), colour = "blue", width = 0.2) +
                   geom_point(data = model_eco_use_pred02, mapping = aes(x = wcup_eco_use_status + 1, y = proportion), size = 4, shape = 21, fill= "blue")

(plot_eco_use_02)

# ==========================================================================
# output plots of discrete variables
# ==========================================================================
# library(patchwork)

# figure_02 <- plot_life_form_02+
#                plot_prop_type_02+
#                plot_ali_shops_02 +
#                plot_eco_use_02 +
#                plot_layout(ncol = 2) +
#                plot_annotation(tag_levels = "A")

# figure_02

# ggexport(figure_02, filename = "./Figure_glm02.png",
#     width = 2400,
#     height = 2400,
#     pointsize = 12,
#     res = 300)

# ==========================================================================
# output of fig.2, including continuous and discrete variables
# ==========================================================================
# # loading packages
# library(patchwork)

# combine separate plots
figure_012 <- plot_bg_02 +
              plot_prov_02 +
              plot_ali_shops_02 +
              plot_eco_use_02 +
              plot_res_time_02 +
              plot_hab_suit_02 +
              plot_native_range_02 +
              plot_life_form_02 +
              plot_prop_type_02 +
              plot_interaction_02 +
              plot_layout(ncol = 3) +
              plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

(figure_012)

# export fig.2
ggexport(figure_012, filename = "D:/R_codes_for_nat.pla_in.China/Results20230523/Figure.2lowercase.png",
                     width = 3600,
                     height = 4800,
                     pointsize = 12,
                     res = 300)