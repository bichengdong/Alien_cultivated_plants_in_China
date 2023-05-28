# ==========================================================================
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-14 17:17:02
# @Last Modified by: dbc
# @Last Modified time: 2023-05-28 12:12:38
# @Description: separate phylogenetic glms in appendix table 1 and appendix table 3
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
# BiocManager::install("ggtreeExtra")
library(ape)
library(broom)
library(clipr)
library(data.table)
library(geiger)
library(ggnewscale)
library(ggtree)
library(ggtreeExtra)
library(phylolm)
library(readr)
library(rlist)
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

# importing data, checklist_nat.pla_in_China.csv
checklist_Invspc_new <- readr::read_csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv", locale = locale(encoding = "GB2312"))

# # separate density plots
# # --------------------------------------------------------------------------
# ggdensity.bg_num                 <- ggdensity(checklist_Invspc_new$bg_num, main = "Density plot", xlab = "bg_num")
# ggdensity.chklst_prov_num        <- ggdensity(checklist_Invspc_new$chklst_prov_num, main = "Density plot", xlab = "chklst_prov_num")
# ggdensity.new_sum.L3.code        <- ggdensity(checklist_Invspc_new$new_sum.L3.code, main = "Density plot", xlab = "new_sum.L3.code")
# ggdensity.residence_time_to_2020 <- ggdensity(checklist_Invspc_new$residence_time_to_2020, main = "Density plot", xlab = "residence_time_to_2020")
# ggdensity.Height_max_max         <- ggdensity(checklist_Invspc_new$Height_max_max, main = "Density plot", xlab = "Height_max_max")
# ggdensity.hab_suit_mean      <- ggdensity(checklist_Invspc_new$hab_suit_mean, main = "Density plot", xlab = "hab_suit_mean")

# # # --------------------------------------------------------------------------
# # ggdensity.bg_num + ggdensity.chklst_prov_num + ggdensity.new_sum.L3.code + ggdensity.residence_time_to_2020 + ggdensity.Height_max_max + ggdensity.hab_suit_mean

# # qq图
# ggqqplot.bg_num                 <- ggqqplot(checklist_Invspc_new$bg_num, main = "QQ plot", xlab = "bg_num")
# ggqqplot.chklst_prov_num        <- ggqqplot(checklist_Invspc_new$chklst_prov_num, main = "QQ plot", xlab = "chklst_prov_num")
# ggqqplot.new_sum.L3.code        <- ggqqplot(checklist_Invspc_new$new_sum.L3.code, main = "QQ plot", xlab = "new_sum.L3.code")
# ggqqplot.residence_time_to_2020 <- ggqqplot(checklist_Invspc_new$residence_time_to_2020, main = "QQ plot", xlab = "residence_time_to_2020")
# ggqqplot.Height_max_max         <- ggqqplot(checklist_Invspc_new$Height_max_max, main = "QQ plot", xlab = "Height_max_max")
# ggqqplot.hab_suit_mean      <- ggqqplot(checklist_Invspc_new$hab_suit_mean, main = "QQ plot", xlab = "hab_suit_mean")

# # # --------------------------------------------------------------------------
# # ggqqplot.bg_num + ggqqplot.chklst_prov_num + ggqqplot.new_sum.L3.code + ggqqplot.residence_time_to_2020 + ggqqplot.Height_max_max + ggqqplot.hab_suit_mean

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

# ==========================================================================
# separate phylogenetic glms process
# ==========================================================================
# loading phylogenetic tree provided by Qiang Yang
# --------------------------------------------------------------------------
phylo_tree <- read.tree("D:/R_codes_for_nat.pla_in.China/data/tree.tre")
# View(phylo_tree)

# check tip.label with NA values
table(is.na(checklist_Invspc_new$tip.label))

# remove tip.label with NA values, and arrange the order of tip.label according to tip.order
checklist_Invspc_new_01 <- checklist_Invspc_new %>% filter(!is.na(tip.label)) %>% arrange(tip.order)

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01) <- checklist_Invspc_new_01$tip.label

# show new data
glimpse(checklist_Invspc_new_01)
View(checklist_Invspc_new_01)

# extract treedata from phylo_tree
phylo_Tree_01 <- treedata(phylo_tree, checklist_Invspc_new_01, sort = TRUE, warnings = F)$phy
# View(phylo_Tree_01)

# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# --------------------------------------------------------------------------
# appendix table 1
# number of botanical gardens, bg_num (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# check the data structure of bg_num
str(checklist_Invspc_new_01$bg_num)

# model fitting
model_phylo_bg <- phyloglm(naturalized_status ~ bg_num, phy = phylo_Tree_01, btol = 30, data = checklist_Invspc_new_01)
summary_phylo_bg <- summary(model_phylo_bg)
(summary_phylo_bg)

# --------------------------------------------------------------------------
# appendix table 1
# number of provinces, chklst_prov_num (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# check the data structure of chklst_prov_num
str(checklist_Invspc_new_01$chklst_prov_num)

# model fitting
model_phylo_prov <- phyloglm(naturalized_status ~ as.numeric(chklst_prov_num), phy = phylo_Tree_01, btol = 40, data = checklist_Invspc_new_01)
summary_phylo_prov <- summary(model_phylo_prov)
(summary_phylo_prov)

# ==========================================================================
# separate phylogenetic glms process
# because the data listed below are not 100% intact,
# we need to separately extract the treedata based on variables
# ==========================================================================
# --------------------------------------------------------------------------
# appendix table 1
# native range size, number of tdwg level-3 regions, new_sum.L3.code (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove new_sum.L3.code with NA values
checklist_Invspc_new_01_tdwg_lvl3 <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$new_sum.L3.code),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_tdwg_lvl3) <- checklist_Invspc_new_01_tdwg_lvl3$tip.label
row.names(checklist_Invspc_new_01_tdwg_lvl3)

# extract treedata from phylo_tree
phylo_Tree_01_tdwg_lvl3 <- treedata(phylo_tree, checklist_Invspc_new_01_tdwg_lvl3, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)
# str(checklist_Invspc_new_01_tdwg_lvl3$new_sum.L3.code)

# model fitting
model_phylo_tdwg_lvl3 <- phyloglm(naturalized_status ~ new_sum.L3.code, phy = phylo_Tree_01_tdwg_lvl3, btol
                                  = 20, data = checklist_Invspc_new_01_tdwg_lvl3)
summary_phylo_tdwg_lvl3 <- summary(model_phylo_tdwg_lvl3)
(summary_phylo_tdwg_lvl3)

# --------------------------------------------------------------------------
# appendix table 1
# min. residence time, time length between the first record year and 2020,
# residence_time_to_2020 (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove residence_time_to_2020 with NA values
checklist_Invspc_new_01_res_time <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$residence_time_to_2020),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_res_time) <- checklist_Invspc_new_01_res_time$tip.label
row.names(checklist_Invspc_new_01_res_time)

# extract treedata from phylo_tree
phylo_Tree_01_res_time <- treedata(phylo_tree, checklist_Invspc_new_01_res_time, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_res_time <- phyloglm(naturalized_status ~ residence_time_to_2020, phy = phylo_Tree_01_res_time, btol = 10,
                                 data = checklist_Invspc_new_01_res_time)
summary_phylo_res_time <- summary(model_phylo_res_time)
(summary_phylo_res_time)

# --------------------------------------------------------------------------
# appendix table 1
# climatic suitability, hab_suit_mean (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove hab_suit_mean with NA values
checklist_Invspc_new_01_hab_suit <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$hab_suit_mean),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_hab_suit) <- checklist_Invspc_new_01_hab_suit$tip.label
row.names(checklist_Invspc_new_01_hab_suit)

# extract treedata from phylo_tree
phylo_Tree_01_hab_suit <- treedata(phylo_tree, checklist_Invspc_new_01_hab_suit, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_hab_suit <- phyloglm(naturalized_status ~ hab_suit_mean, phy = phylo_Tree_01_hab_suit, btol = 10, data = checklist_Invspc_new_01_hab_suit)
summary_phylo_hab_suit <- summary(model_phylo_hab_suit)
(summary_phylo_hab_suit)

# --------------------------------------------------------------------------
# appendix table 1
# maximum height, Height_max_max (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# check the replicates and data range of Height_max_max
# filter out the data on Height_max_max without life form
checklist_Invspc_height_scaled <- checklist_Invspc_new_01 %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_height_scaled %>% select(naturalized_status, Height_max_max, Life_form_new))

# modify Life_form_new as the ordered factors with (3) woody as baseline
checklist_Invspc_height_scaled$Life_form_new <- factor(checklist_Invspc_height_scaled$Life_form_new)
checklist_Invspc_height_scaled$Life_form_new <- relevel(checklist_Invspc_height_scaled$Life_form_new, ref = "3")

# based on category of Life_form_new，log-transform and scale Height_max_max
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "1")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "1")]))
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "2")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "2")]))
checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "3")] <- scale(log(checklist_Invspc_height_scaled$Height_max_max[which(checklist_Invspc_height_scaled$Life_form_new == "3")]))

# based on tip.label, rename the row.names
row.names(checklist_Invspc_height_scaled) <- checklist_Invspc_height_scaled$tip.label
row.names(checklist_Invspc_height_scaled)

# extract treedata from phylo_tree
phylo_Tree_01_height_scaled <- treedata(phylo_tree, checklist_Invspc_height_scaled, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_height_scaled <- phyloglm(naturalized_status ~ Height_max_max, phy = phylo_Tree_01_height_scaled, btol = 10, data = checklist_Invspc_height_scaled)
summary_phylo_height_scaled <- summary(model_phylo_height_scaled)
(summary_phylo_height_scaled)

# --------------------------------------------------------------------------
# appendix table 1
# life form, Life_form_new (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove Life_form_new with NA values
checklist_Invspc_new_01_lf_form <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$Life_form_new),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_lf_form) <- checklist_Invspc_new_01_lf_form$tip.label
row.names(checklist_Invspc_new_01_lf_form)

# extract treedata from phylo_tree
phylo_Tree_01_lf_form <- treedata(phylo_tree, checklist_Invspc_new_01_lf_form, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# modify Life_form_new as the ordered factors with (3) woody as baseline
checklist_Invspc_new_01_lf_form$Life_form_new <- factor(checklist_Invspc_new_01_lf_form$Life_form_new)
checklist_Invspc_new_01_lf_form$Life_form_new <- relevel(checklist_Invspc_new_01_lf_form$Life_form_new, ref = "3")
levels(checklist_Invspc_new_01_lf_form$Life_form_new)

# model fitting
model_phylo_lf_form <- phyloglm(naturalized_status ~ factor(Life_form_new), phy = phylo_Tree_01_lf_form, btol = 10, data = checklist_Invspc_new_01_lf_form)
summary_phylo_lf_form <- summary(model_phylo_lf_form)
(summary_phylo_lf_form)

# --------------------------------------------------------------------------
# appendix table 1
# propagation mode, prop_type (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove prop_type with NA values
checklist_Invspc_new_01_prop_type <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$prop_type),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_prop_type) <- checklist_Invspc_new_01_prop_type$tip.label
row.names(checklist_Invspc_new_01_prop_type)

# extract treedata from phylo_tree
phylo_Tree_01_prop_type <- treedata(phylo_tree, checklist_Invspc_new_01_prop_type, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_prop_type <- phyloglm(naturalized_status ~ factor(prop_type), phy = phylo_Tree_01_prop_type, btol = 10, data = checklist_Invspc_new_01_prop_type)
summary_phylo_prop_type <- summary(model_phylo_prop_type)
(summary_phylo_prop_type)

# --------------------------------------------------------------------------
# appendix table 1
# online nursery availability, ali_new_status (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# remove ali_new_status with NA values
checklist_Invspc_new_01_ali_shops <- checklist_Invspc_new_01[!is.na(checklist_Invspc_new_01$ali_new_status),]

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_ali_shops) <- checklist_Invspc_new_01_ali_shops$tip.label
row.names(checklist_Invspc_new_01_ali_shops)

# extract treedata from phylo_tree
phylo_Tree_01_ali_shops <- treedata(phylo_tree, checklist_Invspc_new_01_ali_shops, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_ali_shops <- phyloglm(naturalized_status ~ factor(ali_new_status), phy = phylo_Tree_01_ali_shops, btol = 10, data = checklist_Invspc_new_01_ali_shops)
summary_phylo_ali_shops <- summary(model_phylo_ali_shops)
(summary_phylo_ali_shops)

# --------------------------------------------------------------------------
# appendix table 1
# economic use, wcup_eco_use_status (variable name in phylogenetic glms)
# --------------------------------------------------------------------------
# because the raw data included the category of economic use (0-10), rather the status of economic use (0 and 1),
# we need transform the category to status before analysis
checklist_Invspc_new_01_eco_use <- checklist_Invspc_new_01 %>%
   mutate(wcup_eco_use_status = case_when(
                                           wcup_eco_use_num == 0 ~ 0,
                                           wcup_eco_use_num > 0 ~ 1
                                           ))

# based on tip.label, rename the row.names
row.names(checklist_Invspc_new_01_eco_use) <- checklist_Invspc_new_01_eco_use$tip.label
row.names(checklist_Invspc_new_01_eco_use)

# extract treedata from phylo_tree
phylo_Tree_01_eco_use <- treedata(phylo_tree, checklist_Invspc_new_01_eco_use, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_eco_use <- phyloglm(naturalized_status ~ factor(wcup_eco_use_status), phy = phylo_Tree_01_eco_use, btol = 40, data = checklist_Invspc_new_01_eco_use)
summary_phylo_eco_use <- summary(model_phylo_eco_use)
(summary_phylo_eco_use)

# --------------------------------------------------------------------------
# appendix table 3
# interaction between height and life form
# --------------------------------------------------------------------------
# check the replicates and data range of Height_max_max
# filter out the data on Height_max_max without life form
checklist_Invspc_interaction <- checklist_Invspc_new %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_interaction %>% select(naturalized_status, Height_max_max, Life_form_new))

# modify Life_form_new as the ordered factors with (3) woody as reference
checklist_Invspc_interaction$Life_form_new <- factor(checklist_Invspc_interaction$Life_form_new)
checklist_Invspc_interaction$Life_form_new <- relevel(checklist_Invspc_interaction$Life_form_new, ref = "3")

# based on category of Life_form_new，log-transform and scale Height_max_max
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")]))
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")]))
checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")]))

# based on tip.label, rename the row.names
row.names(checklist_Invspc_interaction) <- checklist_Invspc_interaction$tip.label
row.names(checklist_Invspc_interaction)

# extract treedata from phylo_tree
phylo_Tree_01_interaction <- treedata(phylo_tree, checklist_Invspc_interaction, sort=TRUE, warnings = F)$phy
# checklist_Invspc_new_02 <- treedata(phylo_tree, checklist_Invspc_new_01, sort=TRUE, warnings = F)$data
# checklist_Invspc_new_02 <- data.frame(checklist_Invspc_new_02)

# View(phylo_Tree_01)
# View(checklist_Invspc_new_02)

# model fitting
model_phylo_interaction   <- phyloglm(naturalized_status ~ Height_max_max + Life_form_new + Height_max_max:Life_form_new, phy = phylo_Tree_01_interaction, btol = 10, data = checklist_Invspc_interaction)
summary_phylo_interaction <- summary(model_phylo_interaction)
(summary_phylo_interaction)

#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-01-07 22:20:25
# @Last Modified by:
# @Last Modified time: 2021-01-12 11:30:05
# @Description: summarize the results of phylogenetic glms
#--------------------------------------------------------------------------#
# loading packages
# library(dplyr)
# library(rlist)
# library(clipr)
# library(broom)

# a simple function to extract the results from phylogenetic glms
glance.phyloglm <- function(fit){out <- data.frame(n = fit$n,
                                                   method = fit$method,
                                                   alpha = fit$alpha,
                                                   mean.tip.height = fit$mean.tip.height,
                                                   AIC = fit$aic,
                                                   logLik = fit$logLik,
                                                   penlogLik = fit$penlogLik);
                                 return(out)}

# summarize the output from phylogenetic glms
summary_phylo_bg_table            <- cbind(summary_phylo_bg$coefficients, glance.phyloglm(model_phylo_bg), model.var = "No. botanical gardens")
summary_phylo_prov_table          <- cbind(summary_phylo_prov$coefficients, glance.phyloglm(model_phylo_prov), model.var = "No. provinces")
summary_phylo_tdwg_lvl3_table     <- cbind(summary_phylo_tdwg_lvl3$coefficients, glance.phyloglm(model_phylo_tdwg_lvl3), model.var = "Native range size")
summary_phylo_res_time_table      <- cbind(summary_phylo_res_time $coefficients, glance.phyloglm(model_phylo_res_time), model.var = "Min. residence time")
summary_phylo_hab_suit_table      <- cbind(summary_phylo_hab_suit$coefficients, glance.phyloglm(model_phylo_hab_suit), model.var = "Climatic suitability")
summary_phylo_height_scaled_table <- cbind(summary_phylo_height_scaled$coefficients, glance.phyloglm(model_phylo_height_scaled), model.var = "Maximum height")
summary_phylo_lf_form_table       <- cbind(summary_phylo_lf_form$coefficients, glance.phyloglm(model_phylo_lf_form), model.var = "Life form")
summary_phylo_prop_type_table     <- cbind(summary_phylo_prop_type$coefficients, glance.phyloglm(model_phylo_prop_type), model.var = "Propagation mode")
summary_phylo_ali_shops_table     <- cbind(summary_phylo_ali_shops$coefficients, glance.phyloglm(model_phylo_ali_shops), model.var = "Online-nursery availability")
summary_phylo_eco_use_table       <- cbind(summary_phylo_eco_use$coefficients, glance.phyloglm(model_phylo_eco_use), model.var = "Economic use")
summary_phylo_interaction_table   <- cbind(summary_phylo_interaction$coefficients, glance.phyloglm(model_phylo_interaction), model.var = "Interaction between max. height and life form")

summary_phylo_output <- rlist::list.rbind(list(summary_phylo_bg_table,
                                               summary_phylo_prov_table,
                                               summary_phylo_ali_shops_table,
                                               summary_phylo_eco_use_table,
                                               summary_phylo_hab_suit_table,
                                               summary_phylo_tdwg_lvl3_table,
                                               summary_phylo_res_time_table,
                                               summary_phylo_lf_form_table,
                                               summary_phylo_prop_type_table,
                                               summary_phylo_height_scaled_table,
                                               summary_phylo_interaction_table))

(summary_phylo_output)

# removing "Intercept" part
summary_phylo_output <- data.frame(summary_phylo_output)
summary_phylo_output.new <- summary_phylo_output %>% filter(!grepl("Intercept", row.names(summary_phylo_output)))

# # save results of phylogenetic glms without_intecept
# clipr::write_clip(summary_phylo_output.new)
write.csv(summary_phylo_output.new, "D:/R_codes_for_nat.pla_in.China/Results20230523/Appendix.tables.1and3.phylo_glms_without_intercept.csv")
