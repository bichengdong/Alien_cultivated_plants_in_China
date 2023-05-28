# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by: dbc
# @Last Modified time: 2023-05-28 15:41:09
# @Description: GLMMs in appendix table 4 and fig. 4
# ==========================================================================
# abbreviations of variables in GLMMs
# prov_code: ID of provinces of China
# prov_planting_status: cultivation status of alien plants in province, 0-1
# Standardized_names: TPL-standardized names of alien plants
# naturalized_status_prov: naturalization status of alien plants in province, 0-1
# hab_suit_mean: climatic suitability in province, 0-1
# ==========================================================================
# preparation work
# ==========================================================================
# cleaning memory
cat("\014")
rm(list=ls())
gc()

# loading packages
library(broom)
library(broom.mixed)
library(ciTools)
library(data.table)
library(ggnewscale)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(lattice)
library(lme4)
library(readr)
library(scales)
library(skimr)
library(stringr)
library(tidyr)
library(dplyr)

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# importing data, checklist_nat.pla_in_provinces.of.China
checklist_Invspc_new_prov <- read.csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_provinces.of.China.updated20230523.csv")
skim(checklist_Invspc_new_prov)
str(checklist_Invspc_new_prov)

# scale the data of hab_suit_mean after log-transformation
# change prov_code, prov_planting_status, Standardized_names as factors
checklist_Invspc_new_prov$hab_suit_mean        <- checklist_Invspc_new_prov$EMwmean_mean / 1000
checklist_Invspc_new_prov$prov_code            <- as.factor(checklist_Invspc_new_prov$prov_code)
checklist_Invspc_new_prov$prov_planting_status <- as.factor(checklist_Invspc_new_prov$prov_planting_status)
checklist_Invspc_new_prov$Standardized_names   <- as.factor(checklist_Invspc_new_prov$Standardized_names)
checklist_Invspc_new_prov$hab_suit_mean_scaled <- scale(log(checklist_Invspc_new_prov$hab_suit_mean + 0.001))

checklist_Invspc_new_prov <- data.frame(checklist_Invspc_new_prov)
# check data range of transformed hab_suit_mean
range(checklist_Invspc_new_prov$hab_suit_mean_scaled)

# ==========================================================================
# GlMMs only for naturalization status in province
# ==========================================================================
# extract the data of naturalized plants from raw data, using "naturalized_status == 1"
checklist_Invspc_new_prov_nat <- checklist_Invspc_new_prov %>% filter(naturalized_status_China == 1)
skim(checklist_Invspc_new_prov_nat)

# # --------------------------------------------------------------------------
# # preliminary evaluate the optimizers of GLMMs
# # the results reported apart from nmkbw, other optimizers are suitable for GLMMs
# # so we used the optimizer by default
# # --------------------------------------------------------------------------
# # model fitting
# m0 <- glmer(naturalized_status_prov ~ hab_suit_mean_scaled +
#             prov_planting_status +
#             hab_suit_mean_scaled:prov_planting_status +
#             (1|Standardized_names) + (1|prov_code),
#             family = binomial(link = "cloglog"),
#             data = checklist_Invspc_new_prov_nat)

# # parallel computation for results of all optimizers
# # loading packages
# library(parallel)

# # start parallelization
# # no. cores of the local computer - 1 were used here
# nc <- detectCores() - 4
# optCls <- makeCluster(nc, type = "SOCK")
# clusterEvalQ(optCls, library("lme4"))
# clusterExport(optCls, "checklist_Invspc_new_prov_nat")

# # using allFit function, to check the results of all optimizers
# system.time(af1 <- allFit(m0, parallel = 'snow', ncpus = nc, cl = optCls))

# # stop parallelization
# stopCluster(optCls)

# # output of parallelization
# ss <- summary(af1)

# # logical vector: which optimizers worked?
# ss$which.OK
# # the other components only contain values for the optimizers that worked vector of log-likelihoods
# ss$llik
# # table of fixed effects
# ss$fixef
# # table of random effect SDs and correlations
# ss$sdcor
# # table of random effects parameters, Ch
# ss$theta

# --------------------------------------------------------------------------
# GLMMs process
# --------------------------------------------------------------------------
# model fitting
# Standardized_names and prov_code were treated as random factors
model_interaction_forNat <- glmer(naturalized_status_prov ~ hab_suit_mean_scaled +
                                  prov_planting_status +
                                  hab_suit_mean_scaled:prov_planting_status +
                                  (1|Standardized_names) + (1|prov_code),
                                  family = binomial(link = "cloglog"),
                                  data = checklist_Invspc_new_prov_nat)

# dispersion test
# the result of dispersion test is 0.8325727
# because the value is between 0.75 and 1.4, there may not be an overdispersion problem.
# https://www.rdocumentation.org/packages/blmeco/versions/1.4/topics/dispersion_glmer
blmeco::dispersion_glmer(model_interaction_forNat)

# output of model
summary_interaction_forNat <- summary(model_interaction_forNat)
summary_interaction_forNat

# save result
tidy.model_interaction_forNat <- broom.mixed::tidy(model_interaction_forNat)
write.csv(tidy.model_interaction_forNat, "D:/R_codes_for_nat.pla_in.China/Results20230523/GLMMs_nat.status.in.prov.csv")

# ==========================================================================
# Fig. 4
# plot for GlMMs
# ==========================================================================
# # loading package
# library(ciTools)
# library(RColorBrewer)
# library(scales)
# library(ggthemes)
# color02 <- brewer.pal(3, "Set1")[-3]

# --------------------------------------------------------------------------
# create prediction and confident intervals for the plot
# --------------------------------------------------------------------------
# create a data frame from all combinations of factor variables
var.interaction_forNat <- with(checklist_Invspc_new_prov_nat,
                               expand.grid(hab_suit_mean_scaled = seq(min(hab_suit_mean_scaled), max(hab_suit_mean_scaled), length.out = 100),
                                           prov_planting_status = levels(prov_planting_status)))

# check data structure of expand.grid
var.interaction_forNat <- data.frame(var.interaction_forNat)
skimr::skim(var.interaction_forNat)
View(var.interaction_forNat)

# base on data in the expand.grid,
# create prediction + confidence intervals, using ciTools package
# it will take ca. 1 hour
interaction_forNat_pred <- add_ci(var.interaction_forNat, model_interaction_forNat, includeRanef = FALSE, type = "boot", nSims = 100)
(interaction_forNat_pred)

# save the prediction data
write.csv(interaction_forNat_pred, "D:/R_codes_for_nat.pla_in.China/Results20230523/Prediction_based.on.GLMMs.for.nat.status.in.prov.csv", row.names = FALSE)

# check range of prediction data
range(checklist_Invspc_new_prov_nat$hab_suit_mean_scaled)

# --------------------------------------------------------------------------
# main plot
# --------------------------------------------------------------------------
# select two colors for factor of cultivation status in province
color02 <- c("#da372a", "#313695")
show_col(color02)

# user-defined jitter position
checklist_Invspc_new_prov_nat <- checklist_Invspc_new_prov_nat %>% mutate(jitter.position = case_when(naturalized_status_prov == 1 & prov_planting_status == 1 ~ 1.02,
                                                                                                      naturalized_status_prov == 1 & prov_planting_status == 0 ~ 0.98,
                                                                                                      naturalized_status_prov == 0 & prov_planting_status == 1 ~ 0.02,
                                                                                                      naturalized_status_prov == 0 & prov_planting_status == 0 ~ -0.02))

# check the data on jitter position
skimr::skim(checklist_Invspc_new_prov_nat$jitter.position)

# basic plot
plot_interaction_forNat_01 <- ggplot(data = checklist_Invspc_new_prov_nat,
                                     aes(x = hab_suit_mean_scaled,
                                         y = naturalized_status_prov),
                                     group = factor(prov_planting_status),
                                     colour = factor(prov_planting_status)) +
                              geom_rangeframe(data = data.frame(hab_suit_mean_scaled = c(-1.5, 2),
                                                                naturalized_status_prov = c(0, 1)),
                                                                size = 1) +
                              theme(panel.background = element_rect(fill = NA),
                                    # axis.line = element_line(size = 1, linetype = "solid"),
                                    axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                                    axis.text = element_text(family = "serif", colour = "black", size = 14),
                                    axis.title = element_text(family = "serif", colour = "black", size = 14),
                                    legend.title = element_text(family = "serif", colour = "black", size = 14),
                                    legend.text = element_text(family = "serif", colour = "black", size = 14),
                                    legend.position = c(0.3, 0.7)) +
                              scale_y_continuous(limits = c(-0.08, 1.08), breaks = seq(0, 1, by = 0.2)) +
                              scale_x_continuous(limits = c(-1.5, 2), breaks = seq(-1.5, 2, by = 0.5)) +
                              labs(x = "Log(climatic suitability + 0.001)(scaled)", y = "Naturalization success\nat the province scale") +
                              coord_cartesian(ylim = c(0, 1))

(plot_interaction_forNat_01)

# add prediction line and confidental intervals produced by
plot_interaction_forNat_02 <- plot_interaction_forNat_01 +
                              geom_jitter(aes(x = hab_suit_mean_scaled, y = jitter.position, colour = factor(prov_planting_status)), shape = 1, size = 2, stroke = 0.5, width = 0, height = 0.01, alpha = 0.5) +
                              geom_line(data = interaction_forNat_pred, aes(x = hab_suit_mean_scaled, y = pred, colour = factor(prov_planting_status)), size = 1) +
                              geom_line(data = interaction_forNat_pred, aes(x = hab_suit_mean_scaled, y = LCB0.025, colour = factor(prov_planting_status)), size = 1, linetype = "dashed", alpha = 1) +
                              geom_line(data = interaction_forNat_pred, aes(x = hab_suit_mean_scaled, y = UCB0.975, colour = factor(prov_planting_status)), size = 1, linetype = "dashed", alpha = 1) +
                              scale_colour_manual(values = color02, name = "Cultivation in province", labels = c("No", "Yes"))

(plot_interaction_forNat_02)

# ==========================================================================
# output plots of GlMMs
# ==========================================================================
# # loading packages
# library(ggpubr)

# export fig.4
ggexport(plot_interaction_forNat_02, filename = "D:/R_codes_for_nat.pla_in.China/Results20230523/Figure.S3.png",
    width = 1600,
    height = 1600,
    pointsize = 12,
    res = 300)