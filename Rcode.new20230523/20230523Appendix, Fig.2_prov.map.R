#--------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2021-11-25 13:07:47
# @Last Modified by: dbc
# @Last Modified time: 2023-05-25 17:40:42
# @Description: map for number of cultivated and naturalized plants in individual province
# @note： working with the readOGR function from the rgdal package or
# the read_csv function from the readr package. Both functions can be affected
# by encoding issues, and it's essential to handle them appropriately.
#--------------------------------------------------------------------------#
# cleaning memory
cat("\014")
rm(list=ls())
gc()

# loading packages
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(ggspatial)
library(maptools)
library(patchwork)
library(readxl)
library(readr)
library(rgdal)
library(sf)
library(stringr)
library(dplyr)
library(sp)

# set work directory
# set your own file path
setwd("D:/R_codes_for_nat.pla_in.China")
getwd()

# loading China map
# loading China map was download in https://www.resdc.cn/data.aspx?DATAID=200
# warning: using your file location and map file
china_map_province <- rgdal::readOGR("D:/R_codes_for_nat.pla_in.China/data/China_map/bou2_4p.shp", stringsAsFactors = FALSE, encoding = "GB2312")
china_map_province1 <- fortify(china_map_province) # 转化为数据框

# loading south China sea border
# south China sea border was download in https://www.resdc.cn/data.aspx?DATAID=200
southsea <- rgdal::readOGR("D:/R_codes_for_nat.pla_in.China/data/China_map/south_sea.shp", encoding = "GB2312")
southsea01 <- fortify(southsea)
View(southsea01)

# read map info of china_map_province map
x <- china_map_province@data
xs <- data.frame(x, id = seq(0, 924, by = 1))
xs$id <- as.character(xs$id)

# combining xs and china_map_province1
china_map_data_province <- left_join(china_map_province1, xs, by = "id")

# removing NA from china_map_data_province, which is the sea area of Hong Kong
china_map_data_province <- china_map_data_province %>% filter(NAME != "NA")

# ==========================================================================
# ggmap.batch_province code for batch mapping
# ==========================================================================
ggmap.batch_province <- function(name.color_group, map_color, map_labels, legend.title, data_source){

# data preparation
data.used <- data_source %>% select(long, lat, group, name.color_group)
colnames(data.used) <- c("long", "lat", "group", "color_group")

# main code
main_plot <- ggplot(data.used, aes(x = long, y = lat)) +
                             geom_polygon(aes(group = group, fill = as.factor(color_group)), colour = "grey45", size = 0.3) +
                             guides(fill = guide_legend(title = legend.title)) +
                             scale_fill_manual(values = map_color, labels = map_labels) +
                             coord_map("polyconic") +
                             scale_x_continuous(breaks = seq(70, 140, by = 10)) +
                             coord_cartesian(xlim = c(74, 135), ylim = c(17, 53)) +
                             theme_bw() +
                             xlab("Longtitude") +
                             ylab("Latitude") +
                             theme(# panel.grid = element_blank(),
                                   # panel.background = element_blank(),
                                   axis.line = element_blank(),
                                   axis.ticks = element_line(size = 0.3),
                                   axis.title = element_text(size = 19),
                                   axis.text = element_text(size = 16),
                                   legend.title = element_text(size = 19),
                                   legend.text = element_text(size = 16),
                                   legend.justification = "left",
                                   panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"))

return(main_plot)
}

# map of alien plant
# --------------------------------------------------------------------------
# color selection
library(RColorBrewer)
library(scales)

# importing no.alien.pla_in_provinces.of.China.csv
# which include the number of alien plants in each province
# --------------------------------------------------------------------------
summary_province_alien <- read_csv("D:/R_codes_for_nat.pla_in.China/data/no.alien.pla_in_provinces.of.China.csv", locale = locale(encoding = "GB2312"))
summary_province_alien <- summary_province_alien %>% rename(NAME = prov_name_cn)
summary_province_alien

# show the number of alien plants
summary_province_alien$n.species <- as.numeric(summary_province_alien$n.species)
sort(summary_province_alien$n.species)

# based on the number of alien plants, assign the color level to each province
summary_province_02_alien <- summary_province_alien %>% mutate(species.color_group = case_when(n.species > 0 & n.species <= 250 ~ "lvl_1",
                                                                                               n.species > 250 & n.species <= 500 ~ "lvl_2",
                                                                                               n.species > 500 & n.species <= 1000 ~ "lvl_3",
                                                                                               n.species > 1000 & n.species <= 2000 ~ "lvl_4",
                                                                                               n.species > 2000 & n.species <= 3000 ~ "lvl_5",
                                                                                               n.species > 3000 ~ "lvl_6"))

table(summary_province_02_alien$species.color_group)

# combining summary_province_02_alien and china_map_province1
china_map_data_province_01_alien <- left_join(china_map_data_province, summary_province_02_alien, by = "NAME")
View(china_map_data_province_01_alien)

# # show all colors
# display.brewer.all()

# user-defined colors for alien plants
species.map_color_alien <- brewer.pal(6, "Reds")
show_col(species.map_color_alien)

# user-defined map legend for alien plants
species.map_labels_alien <- c("1 - 250", "251 - 500", "501 - 1000", "1001 - 2000", "2001 - 3000", "> 3000")

# user-defined legend title for alien plants
# --------------------------------------------------------------------------
species.legend.title_alien <- "No. introduced plants"

# mapping using user-defined ggplot fucntion for alien plants
species.ggmap_province_alien <- ggmap.batch_province("species.color_group",
                                                     species.map_color_alien,
                                                     species.map_labels_alien,
                                                     species.legend.title_alien,
                                                     china_map_data_province_01_alien)

(species.ggmap_province_alien)


# importing no.nat.pla_in_provinces.of.China.csv
# which include the number of naturalized plants in each province
# --------------------------------------------------------------------------
summary_province_nat <- read_csv("D:/R_codes_for_nat.pla_in.China/data/no.nat.pla_in_provinces.of.China.csv", locale = locale(encoding = "GB2312"))
summary_province_nat <- summary_province_nat %>% rename(NAME = prov_name_cn)
summary_province_nat

# show the number of naturalized plants
summary_province_nat$n.species <- as.numeric(summary_province_nat$n.species)
sort(summary_province_nat$n.species)

# based on the number of naturalized plants, assign the color level to each province
summary_province_02_nat <- summary_province_nat %>% mutate(species.color_group = case_when(n.species > 0 & n.species <= 50 ~ "lvl_1",
                                                                                           n.species > 50 & n.species <= 100 ~ "lvl_2",
                                                                                           n.species > 100 & n.species <= 200 ~ "lvl_3",
                                                                                           n.species > 200 & n.species <= 300 ~ "lvl_4",
                                                                                           n.species > 300 & n.species <= 400 ~ "lvl_5",
                                                                                           n.species > 400 & n.species <= 500 ~ "lvl_6"))

table(summary_province_02_nat$species.color_group)

# combining summary_province_02_nat and china_map_province1
china_map_data_province_01_nat <- left_join(china_map_data_province, summary_province_02_nat, by = "NAME")
View(china_map_data_province_01_nat)

# user-defined colors for naturalized plants
species.map_color_nat <- brewer.pal(6, "Blues")
show_col(species.map_color_nat)

# user-defined map legend for naturalized plants
species.map_labels_nat <- c("1 - 50", "51 - 100", "101 - 200", "201 - 300", "301 - 400", "401 - 500")

# user-defined legend title for naturalized plants
# --------------------------------------------------------------------------
species.legend.title_nat <- "No. naturalized plants"

# mapping using user-defined ggplot fucntion for naturalized plants
species.ggmap_province_nat <- ggmap.batch_province("species.color_group",
                                                   species.map_color_nat,
                                                   species.map_labels_nat,
                                                   species.legend.title_nat,
                                                   china_map_data_province_01_nat)

(species.ggmap_province_nat)

# ==========================================================================
# add an attached map, showing southern sea
# ==========================================================================
attached_map <- ggplot(china_map_data_province_01_nat, aes(x = long, y = lat)) +
                geom_polygon(aes(group = group), fill = "white", colour = "grey45", size = 0.3) +
                geom_line(data = southsea01, aes(x = long, y = lat, group = group), color = "grey45", size = 0.3) + # add southsea01 map
                coord_cartesian(xlim = c(105, 125), ylim = c(3, 25)) +  # x- and y-axis adjustment
                theme(
                aspect.ratio = 1.25, # ratio adjustment
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.grid = element_blank(),
                panel.background = element_blank(),
                legend.position = "none",
                panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid"),
                plot.margin = unit(c(0, 0, 0, 0), "mm"))

(attached_map)

# integrating species.ggmap_province and attached_map
species.ggmap_province <- species.ggmap_province_alien + species.ggmap_province_nat + plot_layout(ncol = 1) + plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 16))
species.ggmap_province01 <- species.ggmap_province + inset_element(attached_map, 0.8, -0.1, 0.95, 0.4, ignore_tag = TRUE) +
                                                     inset_element(attached_map, 0.8, 1.06, 0.95, 1.56, ignore_tag = TRUE)
(species.ggmap_province01)

# exporting map
ggexport(species.ggmap_province01, filename = "D:/R_codes_for_nat.pla_in.China/Results20230523/sp_province_plot_both.png",
         width = 7000,
         height = 8000,
         pointsize = 12,
         res = 600)