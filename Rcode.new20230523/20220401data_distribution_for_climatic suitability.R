# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by:
# @Last Modified time: 2022-04-02 08:53:47
# @Description: separate glms in table 1 and appendix table 2
# ==========================================================================
# abbreviations of variables
# bg_num: number of botanical gardens
# chklst_prov_num: number of provinces
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
library(scales)
library(dplyr)

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# importing data, checklist_nat.pla_in_China
checklist_Invspc_new <- fread("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.csv")

# separate density plots
# --------------------------------------------------------------------------
ggdensity.hab_suit_prop_max <- ggdensity(checklist_Invspc_new$hab_suit_prop_max, main = "Density plot", xlab = "hab_suit_prop_max")
ggdensity.hab_suit_prop_max

# separate qq plots
ggqqplot.hab_suit_prop_max <- ggqqplot(checklist_Invspc_new$hab_suit_prop_max, main = "QQ plot", xlab = "hab_suit_prop_max")
ggqqplot.hab_suit_prop_max

# ==========================================================================
# data transformation
# ==========================================================================
# check data range
range(checklist_Invspc_new$hab_suit_prop_max, na.rm = TRUE)

# > range(checklist_Invspc_new$hab_suit_prop_max, na.rm = TRUE)
# [1] 0.0000000 0.9124758

checklist_Invspc_new$hab_suit_prop_max1     <- log(checklist_Invspc_new$hab_suit_prop_max + 1)
checklist_Invspc_new$hab_suit_prop_max0.001 <- log(checklist_Invspc_new$hab_suit_prop_max + 0.001)

 hist_climatic_suit      <- gghistogram(checklist_Invspc_new, x = "hab_suit_prop_max", add = "median") + scale_y_continuous(limits = c(0, 3000)) + labs(x = "Climatic suitability", y = "Count")
 hist_climatic_suit1     <- gghistogram(checklist_Invspc_new, x = "hab_suit_prop_max1", add = "median")  + scale_y_continuous(limits = c(0, 3000)) + labs(x = "Log(Climatic suitability + 1)", y = "Count")
 hist_climatic_suit0.001 <- gghistogram(checklist_Invspc_new, x = "hab_suit_prop_max0.001", add = "median")  + scale_y_continuous(limits = c(0, 3000)) + labs(x = "Log(Climatic suitability + 0.001)", y = "Count")

# ==========================================================================
# output of fig.2, including continuous and discrete variables
# ==========================================================================
# # loading packages
# library(patchwork)

# combine separate plots
hist_plots <- hist_climatic_suit +
              hist_climatic_suit1 +
              hist_climatic_suit0.001 +
              plot_layout(ncol = 3) +
              plot_annotation(tag_levels = "A")

(hist_plots)

# export fig.2
ggexport(hist_plots, filename = "D:/R_codes_for_nat.pla_in.China/Results/hist_plots.png",
                     width = 3600,
                     height = 1200,
                     pointsize = 12,
                     res = 300)
