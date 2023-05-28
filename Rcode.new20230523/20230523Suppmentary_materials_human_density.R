# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by: dbc
# @Last Modified time: 2023-05-24 16:50:10
# @Description: separate glms in table 1 and appendix table 2
# ==========================================================================
# abbreviations of variables
# bg_num: number of botanical gardens
# chklst_prov_num: number of provinces
# prov_pop.density: mean population density of provinces where alien plant are cultivated (people per square km).
# new_sum.L3.code: number of tdwg level-03 regions, native range size
# residence_time_to_2020: minimum residence time
# Height_max_max: maximum height
# hab_suit_prop_max: climatic suitability, 0-1
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

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# importing data, checklist_nat.pla_in_China
checklist_Invspc_new <- fread("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv")
str(checklist_Invspc_new)

# separate density plots
# --------------------------------------------------------------------------
ggdensity.prov_pop.density <- ggdensity(checklist_Invspc_new$prov_pop.density, main = "Density plot", xlab = "prov_pop.density")
ggdensity.prov_pop.density

ggqqplot.prov_pop.density <- ggqqplot(checklist_Invspc_new$prov_pop.density, main = "QQ plot", xlab = "prov_pop.density")
ggqqplot.prov_pop.density


# ==========================================================================
# data transformation
# ==========================================================================
# check data range
range(checklist_Invspc_new$prov_pop.density, na.rm = TRUE)

# > range(checklist_Invspc_new$prov_pop.density, na.rm = TRUE)
# [1]    8.0625 6591.4855

# scale the data after log-transformation
checklist_Invspc_new$prov_pop.density <- scale(log(checklist_Invspc_new$prov_pop.density))

checklist_Invspc_new <- data.frame(checklist_Invspc_new)

# check the data structure
glimpse(checklist_Invspc_new)
View(checklist_Invspc_new)

# # summary of naturalization status
ggdensity(checklist_Invspc_new$prov_pop.density, main = "Density plot", xlab = "prov_pop.density")

# ==========================================================================
# separate glms process
# ==========================================================================
# --------------------------------------------------------------------------
# table 1
# mean population density, prov_pop.density (variable name in glm)
# --------------------------------------------------------------------------
# check the replicates and data range of prov_pop.density
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$prov_pop.density)])
range(checklist_Invspc_new$prov_pop.density[!is.na(checklist_Invspc_new$prov_pop.density)])

# model fitting
model_pop.density <- glm(naturalized_status ~ prov_pop.density,
                  family = binomial(link = "cloglog"),
                  data = checklist_Invspc_new)

summary_pop.density <- summary(model_pop.density)
(summary_pop.density)

nag.R2_pop.density <- nagelkerke(model_pop.density)$Pseudo.R.squared.for.model.vs.null[3]
(nag.R2_pop.density)

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

# --------------------------------------------------------------------------
# prov_pop.density
# --------------------------------------------------------------------------
# check the replicates of number of provinces
table(checklist_Invspc_new$naturalized_status[!is.na(checklist_Invspc_new$prov_pop.density)])
range(checklist_Invspc_new$prov_pop.density[!is.na(checklist_Invspc_new$prov_pop.density)])

# plotting
plot_pop.density_01 <- ggplot(data = checklist_Invspc_new[!is.na(checklist_Invspc_new$prov_pop.density), ],
                       aes(x = prov_pop.density, y = naturalized_status)) +
                geom_rangeframe(data = data.frame(prov_pop.density = c(-6, 4),
                                                  naturalized_status = c(0, 1)),
                                                  size = 1) +
                theme(panel.background = element_rect(fill = NA),
                      # axis.line = element_line(size = 1, linetype = "solid"),
                      axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                      axis.text = element_text(family = "serif", colour = "black", size = 14),
                      axis.title = element_text(family = "serif", colour = "black", size = 14)) +
                scale_y_continuous(limits = c(-0.25, 1.25), breaks = seq(0, 1, by = 0.2)) +
                scale_x_continuous(limits = c(-6, 4), breaks = seq(-6, 4, by = 2)) +
                labs(x = expression(paste("Log(human population density, ", "people/km"^2, ")(scaled)")), y = "Naturalization success") +
                coord_cartesian(ylim = c(0, 1))

(plot_pop.density_01)

# create prediction + confidence intervals, using ciTools package
model_pop.density_var  <- data.frame(prov_pop.density = seq(-6, 4, 10/1000))
model_pop.density_pred <- add_ci(model_pop.density_var, model_pop.density, response = TRUE)
(model_pop.density_pred)

plot_pop.density_02 <- plot_pop.density_01 +
                geom_jitter(shape = 1, colour = "grey", size = 2, stroke = 0.5, width = 0, height = 0.025, alpha = 0.7) +
                geom_line(data = model_pop.density_pred, aes(x = prov_pop.density, y = pred), size = 1, colour = "black") +
                geom_line(data = model_pop.density_pred, aes(x = prov_pop.density, y = LCB0.025), size = 1, colour = "blue", linetype = "dashed", alpha = 1) +
                geom_line(data = model_pop.density_pred, aes(x = prov_pop.density, y = UCB0.975), size = 1, colour = "blue", linetype = "dashed", alpha = 1)

(plot_pop.density_02)

# export fig.2
ggexport(plot_pop.density_02, filename = "D:/R_codes_for_nat.pla_in.China/Results20230523/Figure.s_pop.density.png",
                     width = 1600,
                     height = 1600,
                     pointsize = 12,
                     res = 300)