#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2020-12-31 23:02:10
# @Last Modified by: dbc
# @Last Modified time: 2023-05-26 16:27:52
# @Description: resampling tests of TDWG level-01 in Fig. 1
#--------------------------------------------------------------------------#
# ==========================================================================
# preparation work
# ==========================================================================
# set workspace
# modify your own workspace
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# cleaning memory
cat("\014")
rm(list=ls())
gc()

# loading packages
# library(data.table)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(patchwork)
library(readr)
library(rgdal)
library(scales)
library(skimr)
library(stringr)
library(tidyr)
library(dplyr)

# ==========================================================================
# loading data
# ==========================================================================
# loading data, checklist_nat.pla_in_China
# checklist_Invspc_new <- read_csv(file.choose())
checklist_Invspc_new <- read_csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv", locale = locale(encoding = "GB2312"))
View(checklist_Invspc_new)
table(checklist_Invspc_new$naturalized_status)

# rename X1:X9
checklist_Invspc_new_00 <- checklist_Invspc_new %>%
                           dplyr::rename(Europe = "X1",
                                          Africa = "X2",
                                          Asia_Temperate = "X3",
                                          Asia_Tropical = "X4",
                                          Australasia = "X5",
                                          Pacific = "X6",
                                          Northern_America = "X7",
                                          Southern_America = "X8",
                                          Antarctic = "X9")

# check data structure of checklist_Invspc_new
glimpse(checklist_Invspc_new_00)

# retain the available data of new_sum.L1.code
checklist_Invspc_new_01 <- checklist_Invspc_new_00 %>%
  filter(!is.na(new_sum.L1.code)) %>%
  filter(new_sum.L1.code > 0)

glimpse(checklist_Invspc_new_01)

# datasets with, naturalized_status == 1
checklist_Invspc_nat <- checklist_Invspc_new_01 %>%
  filter(naturalized_status == 1)

# datasets with, naturalized_status == 0
checklist_Invspc_nonnat <- checklist_Invspc_new_01 %>%
  filter(naturalized_status == 0)

# check total number of alien plants, n_sample_chklist
# check total number of naturalized plants, n_sample_chklst_nat
# check total number of non_naturalized plants, n_sample_chklst_nonnat
n_sample_chklist       <- nrow(checklist_Invspc_new_01)
n_sample_chklst_nat    <- nrow(checklist_Invspc_nat)
n_sample_chklst_nonnat <- nrow(checklist_Invspc_nonnat)

n_sample_chklist
n_sample_chklst_nat
n_sample_chklst_nonnat

# ==========================================================================
# setting up a pool of alien plants in China, the code from mark's eco-use paper
# ==========================================================================
china_dataset <- checklist_Invspc_new_01 %>% select(Standardized_names, Europe:Antarctic)

# create a data.frame with the summed number of alien species for each continent
china_pool <- c(
                 sum(checklist_Invspc_new_01$Europe),
                 sum(checklist_Invspc_new_01$Africa),
                 sum(checklist_Invspc_new_01$Asia_Temperate),
                 sum(checklist_Invspc_new_01$Asia_Tropical),
                 sum(checklist_Invspc_new_01$Australasia),
                 sum(checklist_Invspc_new_01$Pacific),
                 sum(checklist_Invspc_new_01$Northern_America),
                 sum(checklist_Invspc_new_01$Southern_America),
                 sum(checklist_Invspc_new_01$Antarctic))

china_pool <- as.numeric(china_pool)
china_pool

# ==========================================================================
# setting up a observed pool of naturalized plants, the code from mark's eco-use paper
# ==========================================================================
# create an data.frame with the summed number of naturalized species for each continent
observed_nat <- c(
                  sum(checklist_Invspc_nat$Europe),
                  sum(checklist_Invspc_nat$Africa),
                  sum(checklist_Invspc_nat$Asia_Temperate),
                  sum(checklist_Invspc_nat$Asia_Tropical),
                  sum(checklist_Invspc_nat$Australasia),
                  sum(checklist_Invspc_nat$Pacific),
                  sum(checklist_Invspc_nat$Northern_America),
                  sum(checklist_Invspc_nat$Southern_America),
                  sum(checklist_Invspc_nat$Antarctic))

observed_nat <- as.numeric(observed_nat)
observed_nat

# set seed
set.seed(123)

# number of random draws (keep this number low when trying it out)
n_random <- 9999

# create "empty" (0s) vectors
Origin.Europe           <- rep(0, n_random + 1)
Origin.Africa           <- rep(0, n_random + 1)
Origin.Asia_Temperate   <- rep(0, n_random + 1)
Origin.Asia_Tropical    <- rep(0, n_random + 1)
Origin.Australasia      <- rep(0, n_random + 1)
Origin.Pacific          <- rep(0, n_random + 1)
Origin.Northern_America <- rep(0, n_random + 1)
Origin.Southern_America <- rep(0, n_random + 1)
Origin.Antarctic        <- rep(0, n_random + 1)

# create an "empty" (0s) data.frame from the vectors above:
count_Origin_nat <- as.data.frame(cbind(
                                    Origin.Europe,
                                    Origin.Africa,
                                    Origin.Asia_Temperate,
                                    Origin.Asia_Tropical,
                                    Origin.Australasia,
                                    Origin.Pacific,
                                    Origin.Northern_America,
                                    Origin.Southern_America,
                                    Origin.Antarctic))

# put observed values in first row of data frame:
count_Origin_nat[1, ] <- observed_nat
head(count_Origin_nat)

# add the resampled values to the other rows of the data.frame:
for (i in 2:(n_random + 1)){

# reporting the process of code
  print(i)

# random sampling from china_dataset
  sample <- china_dataset[sample(nrow(china_dataset), n_sample_chklst_nat, replace = FALSE), ]

# add the sums for each continent to the columns in the count_Origin_nat data frame
  count_Origin_nat[i, 1] <- sum(sample$Europe)
  count_Origin_nat[i, 2] <- sum(sample$Africa)
  count_Origin_nat[i, 3] <- sum(sample$Asia_Temperate)
  count_Origin_nat[i, 4] <- sum(sample$Asia_Tropical)
  count_Origin_nat[i, 5] <- sum(sample$Australasia)
  count_Origin_nat[i, 6] <- sum(sample$Pacific)
  count_Origin_nat[i, 7] <- sum(sample$Northern_America)
  count_Origin_nat[i, 8] <- sum(sample$Southern_America)
  count_Origin_nat[i, 9] <- sum(sample$Antarctic)
}

# # save the file
# write.csv(count_Origin_nat, "20220319_count_Origin_nat_from_china_pool.csv")

# for the p-value, calculate the proportion of re-sampled values that is smaller than the observed value.
# for p < 0.05, the proportion should be <0.0250 or >0.9750.
# for p < 0.01, the proportion should be <0.0050 or >0.9950.
# for p < 0.001, the proportion should be <0.0005 or >0.9995.
p_Europe_nat           <- length(subset(count_Origin_nat[, 1], count_Origin_nat[, 1] < count_Origin_nat[1, 1])) / length(count_Origin_nat[, 1])
p_Africa_nat           <- length(subset(count_Origin_nat[, 2], count_Origin_nat[, 2] < count_Origin_nat[1, 2])) / length(count_Origin_nat[, 2])
p_Asia_Temperate_nat   <- length(subset(count_Origin_nat[, 3], count_Origin_nat[, 3] < count_Origin_nat[1, 3])) / length(count_Origin_nat[, 3])
p_Asia_Tropical_nat    <- length(subset(count_Origin_nat[, 4], count_Origin_nat[, 4] < count_Origin_nat[1, 4])) / length(count_Origin_nat[, 4])
p_Australasia_nat      <- length(subset(count_Origin_nat[, 5], count_Origin_nat[, 5] < count_Origin_nat[1, 5])) / length(count_Origin_nat[, 5])
p_Pacific_nat          <- length(subset(count_Origin_nat[, 6], count_Origin_nat[, 6] < count_Origin_nat[1, 6])) / length(count_Origin_nat[, 6])
p_Northern_America_nat <- length(subset(count_Origin_nat[, 7], count_Origin_nat[, 7] < count_Origin_nat[1, 7])) / length(count_Origin_nat[, 7])
p_Southern_America_nat <- length(subset(count_Origin_nat[, 8], count_Origin_nat[, 8] < count_Origin_nat[1, 8])) / length(count_Origin_nat[, 8])
p_Antarctic_nat        <- length(subset(count_Origin_nat[, 9], count_Origin_nat[, 9] < count_Origin_nat[1, 9])) / length(count_Origin_nat[, 9])

# create an data.frame with the p-value for each continent
probabilities <- c(# naturalized
                  p_Europe_nat,
                  p_Africa_nat,
                  p_Asia_Temperate_nat,
                  p_Asia_Tropical_nat,
                  p_Australasia_nat,
                  p_Pacific_nat,
                  p_Northern_America_nat,
                  p_Southern_America_nat,
                  p_Antarctic_nat)

(probabilities)

Source.Donor <- c(# naturalized
                     "Europe",
                     "Africa",
                     "Asia-Temp.",
                     "Asia-Trop.",
                     "Australasia",
                     "Pacific Isl.",
                     "North. America",
                     "South. America",
                     "Antarctica")

# combine results
d_nat              <- as.data.frame(cbind(Source.Donor, observed_nat, china_pool, probabilities))
d_nat$china_pool   <- as.numeric(d_nat$china_pool)
d_nat$observed_nat <- as.numeric(d_nat$observed_nat)

# add three new columns for new values
# proportion of naturalized plants to alien plants in each continent,
# proportion of alien plants in each continent to alien plants in China,
# proportion of naturalized plants to total naturalized plants in each continent,
d_nat <- d_nat %>% mutate(proportion_china_pool = observed_nat/china_pool)
d_nat <- d_nat %>% mutate(proportion_cultivated_species = china_pool/n_sample_chklist)
d_nat <- d_nat %>% mutate(proportion_nat_species = observed_nat/sum(observed_nat))

(d_nat)

# add five new columns for new values
d_nat$p_value          <- rep(NA, length(d_nat$observed_nat)) # create empty column in d_nat for the proportion of random values that is smaller than the observed value
d_nat$estimated_mean   <- rep(NA, length(d_nat$observed_nat)) # create empty column in d_nat for mean value of the random draws
d_nat$estimated_median <- rep(NA, length(d_nat$observed_nat)) # create empty column in d_nat for median of the random draws
d_nat$estimated_sd     <- rep(NA, length(d_nat$observed_nat)) # create empty column in d_nat for sd of the random draws
d_nat$stand_diff       <- rep(NA, length(d_nat$observed_nat)) # create empty column in d_nat for stand_diff of the random draws

# calculate whether observed flows are higher (proportion close to 1) or lower (proportion close to 0) than expected
for (e in 1:length(d_nat$observed_nat)){
  d_nat$p_value[e]          <- length(subset(count_Origin_nat[, e], count_Origin_nat[, e] < count_Origin_nat[1, e])) / length(count_Origin_nat[, e])
  d_nat$estimated_mean[e]   <- mean(count_Origin_nat[2:n_random, e]) # note that the observed values is excluded
  d_nat$estimated_median[e] <- median(count_Origin_nat[2:n_random, e]) # note that the observed values is excluded
  d_nat$estimated_sd[e]     <- sd(count_Origin_nat[2:n_random, e]) # note that the observed values is excluded

  tryCatch({d_nat$stand_diff[e] <- (d_nat$observed_nat[e] - d_nat$estimated_mean[e]) / d_nat$estimated_sd[e]}, error = function(e){d_nat$stand_diff[e] <- "NA"})
}

# check d_nat
glimpse(d_nat)
(d_nat)

# # save randomrization_test_result.csv
# write_excel_csv(d_nat, "20220319_randomization_test_result_for_mutiple_contients.csv")

#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2020-12-31 23:02:10
# @Last Modified by:
# @Last Modified time: 2022-03-19 16:54:58
# @Description: plotting Fig.1A and Fig. 1B
#--------------------------------------------------------------------------#
# ==========================================================================
# preparation work
# ==========================================================================
# # loading plot-related libraries
# library(ggplot2)
# library(ggpubr)
# library(ggthemes)
# library(mekko)
# library(rgdal)
# library(scales)

# select colors for continents in Fig. 1
map_color <- c(
               "#1A76C0",
               "#C659A2",
               "#FFE131",
               "#E1612A",
               "#C08446",
               "#000000",
               "#90AA3C",
               "#EF5640",
               "#00FFFF")

# show colors
show_col(map_color)

# ==========================================================================
# create Fig.1A
# ==========================================================================
# loading twdg level-1 map
# twdg level-1 map was downloaded in https://github.com/tdwg/wgsrpd
# warning: using your file location and map file
tdwg_lvl01_map <- readOGR("D:/R_codes_for_nat.pla_in.China/data/tdwg_map/level1/level1.shp", stringsAsFactors = FALSE)
plot(tdwg_lvl01_map)

# changing tdwg_lvl01_map as data.frame
tdwg_lvl01_map01 <- ggplot2::fortify(tdwg_lvl01_map) # change it as data.frame

# loading China map
# loading China map was download in https://www.resdc.cn/data.aspx?DATAID=200
# warning: using your file location and map file
china_map <- rgdal::readOGR("D:/R_codes_for_nat.pla_in.China/data/China_map/bou2_4p.shp", stringsAsFactors = FALSE)
china_map01 <- fortify(china_map)

# loading south China sea border
# south China sea border was download in https://www.resdc.cn/data.aspx?DATAID=200
southsea <- rgdal::readOGR("D:/R_codes_for_nat.pla_in.China/data/China_map/south_sea.shp")
southsea01 <- fortify(southsea)
View(southsea01)

# read map info of twdg level-1 map
x            <- tdwg_lvl01_map@data # read map info
xs           <- data.frame(x, id = seq(0, 8, by = 1))
xs$id        <- as.character(xs$id)
xs$map_color <- map_color

#   LEVEL1_COD       LEVEL1_NAM id map_color
# 0          1           EUROPE  0   #1A76C0
# 1          2           AFRICA  1   #C659A2
# 2          3   ASIA-TEMPERATE  2   #FFE131
# 3          4    ASIA-TROPICAL  3   #E1612A
# 4          5      AUSTRALASIA  4   #C08446
# 5          6          PACIFIC  5   #000000
# 6          7 NORTHERN AMERICA  6   #90AA3C
# 7          8 SOUTHERN AMERICA  7   #EF5640
# 8          9        ANTARCTIC  8   #E1E9EB

# create annotations for tdwg level-1 map
# set x position of annotations
# set y position of annotations
# set contents of annotations
annotation.x <- c(30, 15, 100, 110, 135, -150, -100, -60, 0)
annotation.y <- c(55, 15, 50, 0, -25, 0, 50, -15, -80)

# annotation.label <- c("Europe", "Africa", "Asia-Temp.", "Asia-Trop.", "Australasia", "Pacific Isl.", "North. America", "South. America", "Antarctica")
annotation.label <- c("Europe\n1436 (391)", "Africa\n3302 (2480)", "Asia-Temp.\n1642 (481)", "Asia-Trop.\n1895 (1371)", "Australasia\n967 (776)", "Pacific Isl.\n198 (79)", "North. America\n3202 (2043)", "South. America\n3659 (2554)", "Antarctica\n11 (0)")

# create a data.frame for annotations in the Fig.1A
annotation <- data.frame(
                         x = annotation.x,
                         y = annotation.y,
                         label = annotation.label)

# ==========================================================================
# create ggplot map of tdwg level-1 regions
# ==========================================================================
tdwg.map_plot <- ggplot(tdwg_lvl01_map01, aes(x = long, y = lat)) +
                        geom_polygon(aes(group = group, fill = factor(id)), alpha = 0.9, colour = "white", size = 0.2) +
                        theme_void() +
                        theme(legend.position = "none") +
                        scale_fill_manual(values = xs$map_color)

# intergrat tdwg.map_plot01 with China map
tdwg.map_plot00 <- tdwg.map_plot +
                   geom_polygon(data = china_map01, aes(x = long, y = lat, group = group), fill = "gray91", colour = "gray91", size = 0.3) +
                   geom_line(data = southsea01, aes(x = long, y = lat, group = group), color = "gray91", size = 0.3)

(tdwg.map_plot00)

# add annotation
tdwg.map_plot01 <- tdwg.map_plot00 + geom_text(data = annotation, aes(x = x, y = y, label = label))
(tdwg.map_plot01)

# ==========================================================================
# create Fig.1B
# ==========================================================================
# loading packages
# library(ggplot2)
# library(mekko)
# library(ggpubr)
# library(ggthemes)

# changing order of proportion_china_pool
d_nat            <- d_nat %>% arrange(desc(proportion_china_pool))
d_nat$x_axis_lab <- paste(d_nat$Source.Donor, " (", d_nat$observed_nat, ")", sep ="")

# change d_nat$x_axis_lab as ordered factors
d_nat$x_axis_lab <- factor(d_nat$x_axis_lab, order = TRUE, levels = d_nat$x_axis_lab)
levels(d_nat$x_axis_lab)

# add new columns for p_value_lab and color_lab
d_nat <- d_nat %>% mutate(p_value_lab = case_when(p_value < 0.0005|p_value > 0.9995 ~ "italic(p)<0.001",
                                                  p_value < 0.0050|p_value > 0.9950 ~ "italic(p)<0.01",
                                                  p_value < 0.0250|p_value > 0.9750 ~ "italic(p)<0.05",
                                                  TRUE ~ paste("italic(p)==", round(p_value, 3), sep = "")
                                                  )) %>%
                  mutate(color_lab = case_when(p_value > 0.9750 ~ "red",
                                               p_value < 0.0250 ~ "blue",
                                               TRUE ~ "black"))

(d_nat)

# keep consistence with colors of each continent in Fig. 1A and 1B
tdwg_01_names <- data.frame(LEVEL1_NAM = c("EUROPE", "AFRICA", "ASIA-TEMPERATE", "ASIA-TROPICAL", "AUSTRALASIA", "PACIFIC", "NORTHERN AMERICA", "SOUTHERN AMERICA", "ANTARCTIC"),
                            Source.Donor = c("Europe", "Africa", "Asia-Temp.", "Asia-Trop.", "Australasia", "Pacific Isl.", "North. America", "South. America", "Antarctica"))
xs.color <- xs %>% select(LEVEL1_NAM, map_color) %>% left_join(tdwg_01_names, by = "LEVEL1_NAM")
d_nat <- d_nat %>% left_join(xs.color, by = "Source.Donor")

# add reference_line in Fig. 1B
reference_line_china_pool <- n_sample_chklst_nat/n_sample_chklist

# modify bar width based on proportion_cultivated_species
d_nat$bar.width <- d_nat$proportion_cultivated_species

# https://ggplot2.tidyverse.org/reference/geom_abline.html
bar_plot_b <- ggplot(data = d_nat, aes(x = x_axis_lab, y = proportion_china_pool, width = bar.width*3))+
                   geom_bar(stat = "identity", fill = d_nat$map_color) +
                   theme(panel.background = element_rect(fill = NA),
                      # axis.line = element_line(size = 1, linetype = "solid"),
                         axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                         axis.text = element_text(colour = "black", size = 12),
                         legend.title = element_text(size = 12),
                         legend.text = element_text(size = 12),
                         legend.position = "none",
                         axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
                       # scale_x_discrete(limits = position_y) +
                   scale_y_continuous(limits = c(0, 0.4), breaks = seq(0, 0.4, by = 0.05)) +
                   geom_hline(yintercept = reference_line_china_pool, color = "red", size = 0.5) +
                   geom_rangeframe(data = data.frame(x_axis_lab = c(1, 9),
                                                     proportion_china_pool = c(0, 0.4),
                                                     bar.width = c(0, 1),
                                                     size = 1,
                                                     sides = "right"))+
                   labs(y = "Proportion of naturalized species", x = "")
(bar_plot_b)

# add p values and * symbols inbar_plot_b
# https://stackoverflow.com/questions/41231903/ggplot2-how-to-add-text-to-multiple-vertical-lines-geom-vlines-on-a-time-x-ax
bar_plot_b01 <- bar_plot_b + geom_text(data = d_nat, aes(x = x_axis_lab, y = proportion_china_pool + 0.08, label = p_value_lab), color = d_nat$color_lab, angle = 90, parse=TRUE)
(bar_plot_b01)

# # combined plots
# library(patchwork)
# mixed_fig <- tdwg.map_plot01 +
#              bar_plot_b01 +
#              plot_layout(ncol = 1, heights = c(3, 4)) +
#              plot_annotation(tag_levels = "A")
#
# (mixed_fig)
#
# # save plots
# ggexport(mixed_fig, filename = "D:/2020_DATABASE_ALL_IN_ONE/12. All_in_one/2021_12_RESULTS/figure_tdwg_2021_12_09.png",
#     width = 2400,
#     height = 2400,
#     pointsize = 12,
#     res = 300)

#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2020-12-31 23:02:10
# @Last Modified by:
# @Last Modified time: 2022-01-23 11:45:11
# @Description: resampling tests of TDWG level-01 in Fig. 1C
#--------------------------------------------------------------------------#
# # ==========================================================================
# # preparation work
# # ==========================================================================
# # set workspace
# setwd("F:/2020_DATABASE_ALL_IN_ONE/12. All_in_one")
# getwd()

# # cleaning memory
# cat("\014")
# rm(list=ls())
# gc()

# ==========================================================================
# loading data
# ==========================================================================
# # checklist_Invspc_new <- read_csv(file.choose())
# checklist_Invspc_new <- read_csv("D:/2020_DATABASE_ALL_IN_ONE/12. All_in_one/checklist_Invspc_new_20220124final_version_together_06.csv")
# View(checklist_Invspc_new)
# table(checklist_Invspc_new$naturalized_status)

# rename X1:X9
checklist_Invspc_new_00 <- checklist_Invspc_new %>%
                            dplyr::rename(Europe = "X1",
                                          Africa = "X2",
                                          Asia_Temperate = "X3",
                                          Asia_Tropical = "X4",
                                          Australasia = "X5",
                                          Pacific = "X6",
                                          Northern_America = "X7",
                                          Southern_America = "X8",
                                          Antarctic = "X9")

# check data structure of checklist_Invspc_new
glimpse(checklist_Invspc_new_00)

# retain the available data of new_sum.L1.code
# select the species only native to one continent
checklist_Invspc_new_01c <- checklist_Invspc_new_00 %>%
                            filter(!is.na(new_sum.L1.code)) %>%
                            filter(new_sum.L1.code == 1)

glimpse(checklist_Invspc_new_01c)

# datasets with, naturalized_status == 1
checklist_Invspc_nat_c <- checklist_Invspc_new_01c %>%
                          filter(naturalized_status == 1)

# datasets with, naturalized_status == 0
checklist_Invspc_nonnat_c <- checklist_Invspc_new_01c %>%
                             filter(naturalized_status == 0)

# check total number of alien plants, n_sample_chklist_c
# check total number of naturalized plants, n_sample_chklst_nat_c
# check total number of non_naturalized plants, n_sample_chklst_nonnat_c
n_sample_chklist_c       <- nrow(checklist_Invspc_new_01c)
n_sample_chklst_nat_c    <- nrow(checklist_Invspc_nat_c)
n_sample_chklst_nonnat_c <- nrow(checklist_Invspc_nonnat_c)

n_sample_chklist_c
n_sample_chklst_nat_c
n_sample_chklst_nonnat_c

# ==========================================================================
# setting up a pool of alien plants in China, the code from mark's eco-use paper
# ==========================================================================
# create a data.frame with the summed number of alien species for each continent
china_dataset_c <- checklist_Invspc_new_01c %>% select(Standardized_names, Europe:Antarctic)

china_pool_c <- c(
                 sum(checklist_Invspc_new_01c$Europe),
                 sum(checklist_Invspc_new_01c$Africa),
                 sum(checklist_Invspc_new_01c$Asia_Temperate),
                 sum(checklist_Invspc_new_01c$Asia_Tropical),
                 sum(checklist_Invspc_new_01c$Australasia),
                 sum(checklist_Invspc_new_01c$Pacific),
                 sum(checklist_Invspc_new_01c$Northern_America),
                 sum(checklist_Invspc_new_01c$Southern_America),
                 sum(checklist_Invspc_new_01c$Antarctic))

china_pool_c <- as.numeric(china_pool_c)
china_pool_c

# ==========================================================================
# setting up a observed pool of naturalized plants, the code from mark's eco-use paper
# ==========================================================================
# create an data.frame with the summed number of naturalized species for each continent
observed_nat_c <- c(
                    sum(checklist_Invspc_nat_c$Europe),
                    sum(checklist_Invspc_nat_c$Africa),
                    sum(checklist_Invspc_nat_c$Asia_Temperate),
                    sum(checklist_Invspc_nat_c$Asia_Tropical),
                    sum(checklist_Invspc_nat_c$Australasia),
                    sum(checklist_Invspc_nat_c$Pacific),
                    sum(checklist_Invspc_nat_c$Northern_America),
                    sum(checklist_Invspc_nat_c$Southern_America),
                    sum(checklist_Invspc_nat_c$Antarctic))

observed_nat_c <- as.numeric(observed_nat_c)
observed_nat_c

# set seed
set.seed(123)

# number of random draws (keep this number low when trying it out)
n_random <- 9999

# create "empty" (0s) vectors
Origin.Europe           <- rep(0, n_random + 1)
Origin.Africa           <- rep(0, n_random + 1)
Origin.Asia_Temperate   <- rep(0, n_random + 1)
Origin.Asia_Tropical    <- rep(0, n_random + 1)
Origin.Australasia      <- rep(0, n_random + 1)
Origin.Pacific          <- rep(0, n_random + 1)
Origin.Northern_America <- rep(0, n_random + 1)
Origin.Southern_America <- rep(0, n_random + 1)
Origin.Antarctic        <- rep(0, n_random + 1)

# create an "empty" (0s) data.frame from the vectors above:
count_Origin_nat_c <- as.data.frame(cbind(
                                          Origin.Europe,
                                          Origin.Africa,
                                          Origin.Asia_Temperate,
                                          Origin.Asia_Tropical,
                                          Origin.Australasia,
                                          Origin.Pacific,
                                          Origin.Northern_America,
                                          Origin.Southern_America,
                                          Origin.Antarctic))

# put observed values in first row of data frame:
count_Origin_nat_c[1, ] <- observed_nat_c
head(count_Origin_nat_c)

# add the resampled values to the other rows of the data.frame:
for (i in 2:(n_random + 1)){

# reporting the process of code
  print(i)

# random sampling from china_dataset_c
  sample <- china_dataset_c[sample(nrow(china_dataset_c), n_sample_chklst_nat_c, replace = FALSE), ]

# add the sums for each continent to the columns in the count_Origin_nat_c data frame
  count_Origin_nat_c[i, 1] <- sum(sample$Europe)
  count_Origin_nat_c[i, 2] <- sum(sample$Africa)
  count_Origin_nat_c[i, 3] <- sum(sample$Asia_Temperate)
  count_Origin_nat_c[i, 4] <- sum(sample$Asia_Tropical)
  count_Origin_nat_c[i, 5] <- sum(sample$Australasia)
  count_Origin_nat_c[i, 6] <- sum(sample$Pacific)
  count_Origin_nat_c[i, 7] <- sum(sample$Northern_America)
  count_Origin_nat_c[i, 8] <- sum(sample$Southern_America)
  count_Origin_nat_c[i, 9] <- sum(sample$Antarctic)
}

# # save the file
# write.csv(count_Origin_nat_c, "20220124_count_Origin_nat_from_china_pool_c.csv")

# for the p-value, calculate the proportion of re-sampled values that is smaller than the observed value.
# for p < 0.05, the proportion should be <0.0250 or >0.9750.
# for p < 0.01, the proportion should be <0.0050 or >0.9950.
# for p < 0.001, the proportion should be <0.0005 or >0.9995.
p_Europe_nat_c           <- length(subset(count_Origin_nat_c[, 1], count_Origin_nat_c[, 1] < count_Origin_nat_c[1, 1])) / length(count_Origin_nat_c[, 1])
p_Africa_nat_c           <- length(subset(count_Origin_nat_c[, 2], count_Origin_nat_c[, 2] < count_Origin_nat_c[1, 2])) / length(count_Origin_nat_c[, 2])
p_Asia_Temperate_nat_c   <- length(subset(count_Origin_nat_c[, 3], count_Origin_nat_c[, 3] < count_Origin_nat_c[1, 3])) / length(count_Origin_nat_c[, 3])
p_Asia_Tropical_nat_c    <- length(subset(count_Origin_nat_c[, 4], count_Origin_nat_c[, 4] < count_Origin_nat_c[1, 4])) / length(count_Origin_nat_c[, 4])
p_Australasia_nat_c      <- length(subset(count_Origin_nat_c[, 5], count_Origin_nat_c[, 5] < count_Origin_nat_c[1, 5])) / length(count_Origin_nat_c[, 5])
p_Pacific_nat_c          <- length(subset(count_Origin_nat_c[, 6], count_Origin_nat_c[, 6] < count_Origin_nat_c[1, 6])) / length(count_Origin_nat_c[, 6])
p_Northern_America_nat_c <- length(subset(count_Origin_nat_c[, 7], count_Origin_nat_c[, 7] < count_Origin_nat_c[1, 7])) / length(count_Origin_nat_c[, 7])
p_Southern_America_nat_c <- length(subset(count_Origin_nat_c[, 8], count_Origin_nat_c[, 8] < count_Origin_nat_c[1, 8])) / length(count_Origin_nat_c[, 8])
p_Antarctic_nat_c        <- length(subset(count_Origin_nat_c[, 9], count_Origin_nat_c[, 9] < count_Origin_nat_c[1, 9])) / length(count_Origin_nat_c[, 9])

# create an data.frame with the p-value for each continent
probabilities_c <- c(# naturalized
                     p_Europe_nat_c,
                     p_Africa_nat_c,
                     p_Asia_Temperate_nat_c,
                     p_Asia_Tropical_nat_c,
                     p_Australasia_nat_c,
                     p_Pacific_nat_c,
                     p_Northern_America_nat_c,
                     p_Southern_America_nat_c,
                     p_Antarctic_nat_c)
(probabilities_c)

Source.Donor_c <- c(# naturalized
                     "Europe",
                     "Africa",
                     "Asia-Temp.",
                     "Asia-Trop.",
                     "Australasia",
                     "Pacific Isl.",
                     "North. America",
                     "South. America",
                     "Antarctica")

# combine results
d_nat_c                <- as.data.frame(cbind(Source.Donor_c, observed_nat_c, china_pool_c, probabilities_c))
d_nat_c$china_pool_c   <- as.numeric(d_nat_c$china_pool_c)
d_nat_c$observed_nat_c <- as.numeric(d_nat_c$observed_nat_c)

# add three new columns for new values
# proportion of naturalized plants to alien plants in each continent,
# proportion of alien plants in each continent to alien plants in China,
# proportion of naturalized plants to total naturalized plants in each continent,
d_nat_c <- d_nat_c %>% mutate(proportion_china_pool = observed_nat_c/china_pool_c)
d_nat_c <- d_nat_c %>% mutate(proportion_cultivated_species = china_pool_c/n_sample_chklist_c)
d_nat_c <- d_nat_c %>% mutate(proportion_nat_species = observed_nat_c/sum(observed_nat_c))
d_nat_c

# add five new columns for new values
d_nat_c$p_value                <- rep(NA, length(d_nat_c$observed_nat_c)) #create empty column in d_nat for the proportion of random values that is smaller than the observed value
d_nat_c$estimated_mean         <- rep(NA, length(d_nat_c$observed_nat_c)) #create empty column in d_nat for mean value of the random draws
d_nat_c$estimated_median       <- rep(NA, length(d_nat_c$observed_nat_c)) #create empty column in d_nat for median of the random draws
d_nat_c$estimated_sd           <- rep(NA, length(d_nat_c$observed_nat_c)) #create empty column in d_nat for sd of the random draws
d_nat_c$stand_diff             <- rep(NA, length(d_nat_c$observed_nat_c)) #create empty column in d_nat for stand_diff of the random draws

# calculate whether observed flows are higher (proportion close to 1) or lower (proportion close to 0) than expected
for (e in 1:length(d_nat_c$observed_nat_c)) {
  d_nat_c$p_value[e]          <- length(subset(count_Origin_nat_c[, e], count_Origin_nat_c[, e] < count_Origin_nat_c[1, e])) / length(count_Origin_nat_c[, e])
  d_nat_c$estimated_mean[e]   <- mean(count_Origin_nat_c[2:n_random, e]) # note that the observed values is excluded
  d_nat_c$estimated_median[e] <- median(count_Origin_nat_c[2:n_random, e]) # note that the observed values is excluded
  d_nat_c$estimated_sd[e]     <- sd(count_Origin_nat_c[2:n_random, e]) # note that the observed values is excluded

  tryCatch({d_nat_c$stand_diff[e] <- (d_nat_c$observed_nat_c[e] - d_nat_c$estimated_mean[e]) / d_nat_c$estimated_sd[e]}, error = function(e){d_nat_c$stand_diff[e] <- "NA"})
}


# check d_nat_c
# please note that Antarctica was excluded in the resampling analysis here
glimpse(d_nat_c)
(d_nat_c)

# # save randomrization_test_result.csv
# write_excel_csv(d_nat_c, "20220319_randomization_test_result_for_single_contient.csv")

# ==========================================================================
# create Fig.1C
# ==========================================================================
# # loading packages
# library(ggplot2)
# library(mekko)
# library(ggpubr)
# library(ggthemes)

# changing order of proportion_china_pool_c
d_nat_c <- d_nat_c %>% dplyr::arrange(desc(proportion_china_pool))

# change d_nat_c$x_axis_lab as ordered factors
d_nat_c$x_axis_lab <- paste(d_nat_c$Source.Donor, " (", d_nat_c$observed_nat_c, ")", sep ="")
d_nat_c$x_axis_lab <- factor(d_nat_c$x_axis_lab, order = TRUE, levels = d_nat_c$x_axis_lab)
levels(d_nat_c$x_axis_lab)

# add new columns for p_value_lab and color_lab
d_nat_c <- d_nat_c %>% mutate(p_value_lab = case_when(p_value < 0.0005|p_value > 0.9995 ~ "italic(p)<0.001",
                                                      p_value < 0.0050|p_value > 0.9950 ~ "italic(p)<0.01",
                                                      p_value < 0.0250|p_value > 0.9750 ~ "italic(p)<0.05",
                                                     TRUE ~ paste("italic(p)==", round(p_value, 3), sep = "")
                                                     )) %>%
                       mutate(color_lab = case_when(p_value > 0.9750 ~ "red",
                                                    p_value < 0.0250 ~ "blue",
                                                    TRUE ~ "black"))

# keep consistence with bar position of each continent in Fig. 1B and 1C
d_nat_c$x_axis_lab <- factor(d_nat_c$x_axis_lab, order = TRUE, levels = c("Antarctica (0)",
                                                                          "North. America (78)",
                                                                          "Asia-Temp. (10)",
                                                                          "South. America (82)",
                                                                          "Europe (6)",
                                                                          "Pacific Isl. (0)",
                                                                          "Asia-Trop. (30)",
                                                                          "Africa (30)",
                                                                          "Australasia (11)"))

(d_nat_c)

# keep consistence with colors of each continent in Fig. 1A and 1C
tdwg_01_names <- data.frame(LEVEL1_NAM = c("EUROPE", "AFRICA", "ASIA-TEMPERATE", "ASIA-TROPICAL", "AUSTRALASIA", "PACIFIC", "NORTHERN AMERICA", "SOUTHERN AMERICA", "ANTARCTIC"),
                            Source.Donor_c = c("Europe", "Africa", "Asia-Temp.", "Asia-Trop.", "Australasia", "Pacific Isl.", "North. America", "South. America", "Antarctica"))
xs.color <- xs %>% select(LEVEL1_NAM, map_color) %>% left_join(tdwg_01_names, by = "LEVEL1_NAM")
d_nat_c <- d_nat_c %>% left_join(xs.color, by = "Source.Donor_c")
(d_nat_c)

# please note that Antarctica was excluded in the resampling analysis here
# so values in Antarctica should be modified
d_nat_c$proportion_china_pool[which(d_nat_c$Source.Donor_c == "Antarctica")] <- 0
d_nat_c$color_lab[which(d_nat_c$Source.Donor_c == "Antarctica")] <- "black"
d_nat_c$p_value_lab[which(d_nat_c$Source.Donor_c == "Antarctica")] <- "N.A."

# add reference_line in Fig. 1C
reference_line_china_pool_c <- n_sample_chklst_nat_c/n_sample_chklist_c

# modify bar width based on proportion_cultivated_species
d_nat_c$bar.width <- d_nat_c$proportion_cultivated_species

# using ggplot to make spineplot
# https://ggplot2.tidyverse.org/reference/geom_abline.html
bar_plot_c <- ggplot(data = d_nat_c, aes(x = x_axis_lab, y = proportion_china_pool, width = bar.width*3))+
                     geom_bar(stat = "identity", fill = d_nat_c$map_color) +
                     theme(panel.background = element_rect(fill = NA),
                         # axis.line = element_line(size = 1, linetype = "solid"),
                           axis.ticks = element_line(colour = "black", linetype = "solid", size = 1),
                           axis.text = element_text(colour = "black", size = 12),
                           legend.title = element_text(size = 12),
                           legend.text = element_text(size = 12),
                           legend.position = "none",
                           axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
                         # scale_x_discrete(limits = position_y) +
                     scale_y_continuous(limits = c(0, 0.1), breaks = seq(0, 0.1, by = 0.02)) +
                     geom_hline(yintercept = reference_line_china_pool_c, color = "red", size = 0.5) +
                     geom_rangeframe(data = data.frame(x_axis_lab = c(1, 9),
                                                       proportion_china_pool = c(0, 0.1),
                                                       bar.width = c(0, 1),
                                                       size = 1,
                                                       sides = "right"))+
                     labs(y = "Proportion of naturalized species", x = "")

(bar_plot_c)

# set y position of annotations
annotation.y02 <- c(0.03817915 + 0.02, 0.03210650 + 0.02, 0.04, 0.04, 0.04, 0.04, 0.04, 0.04, 0.01)

# add p values and * symbols to plot
# https://stackoverflow.com/questions/41231903/ggplot2-how-to-add-text-to-multiple-vertical-lines-geom-vlines-on-a-time-x-ax
bar_plot_c01 <- bar_plot_c + geom_text(data = d_nat_c, aes(x = x_axis_lab, y = annotation.y02, label = p_value_lab), color = d_nat_c$color_lab, angle = 90, parse=TRUE)
(bar_plot_c01)

# ==========================================================================
# export Fig.1A, B and C together
# ==========================================================================
# library(patchwork)
mixed_fig <- tdwg.map_plot01 +
             bar_plot_b01 +
             bar_plot_c01 +
             plot_layout(ncol = 1, heights = c(6, 4, 4)) +
             plot_annotation(tag_levels = "A")

(mixed_fig)

# save plots
ggexport(mixed_fig, filename = "D:/R_codes_for_nat.pla_in.China/Results20230523/Figure.1.png",
          width = 2400,
          height = 4000,
          pointsize = 12,
          res = 300)


