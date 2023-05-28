# --------------------------------------------------------------------------#
# -*- coding: utf-8 -*-
# @Author: Ran Dong
# @Date: 2021-11-25 13:07:47
# @Last Modified by:
# @Last Modified time: 2022-04-13 08:53:43
# @Description: map for number of cultivated and naturalized plants in individual province
# --------------------------------------------------------------------------# 
# 清理内存
cat("\014")
rm(list = ls())

gc()
# memory.size()
# memory.size(max = TRUE)

# Working directory
path <- "D:/我的坚果云/撰写的文章20201031/R_codes_for_nat.pla_in.China/BG地图"

# create folder
if (!file.exists(path)) {
  dir.create(path, recursive = TRUE)
  cat("<-- The path is created -->")
} else {
  cat("<-- The path already exists -->")
}

# Set working directory
setwd(path)
getwd()

# loading packages
# install.package("pacman")
# library(ggplot2)
# library(ggpubr)
# library(ggthemes)
# library(ggspatial)
# library(maptools)
# library(patchwork)
# library(readxl)
# library(rgdal)
# library(sf)
# library(stringr)
# library(dplyr)

pacman::p_load(
amapGeocode, # 高德API
ChinaCoordinate, # 经纬度换算
ggplot2,
ggpubr,
ggspatial,
ggthemes,
magrittr,
maptools,
patchwork,
readxl,
rgdal,
rlist,
sf,
stringr,
hrbrthemes,
ggrepel,
viridis,
dplyr)

# ==========================================================================
# 加载中国地图
# ==========================================================================
# loading China map
# loading China map was download in https://www.resdc.cn/data.aspx? DATAID = 200
# warning: using your file location and map file
china_map_province <- rgdal::readOGR("D:/我的坚果云/撰写的文章20201031/R_codes_for_nat.pla_in.China/data/China_map/bou2_4p.shp", stringsAsFactors = FALSE)
china_map_province1 <- fortify(china_map_province) # 转化为数据框

# loading south China sea border
# south China sea border was download in https://www.resdc.cn/data.aspx? DATAID = 200
southsea <- rgdal::readOGR("D:/我的坚果云/撰写的文章20201031/R_codes_for_nat.pla_in.China/data/China_map/south_sea.shp", stringsAsFactors = FALSE)
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

# 坐标系转换
# Transform the coordinates to WGS84
# 坐标轴转换
# 火星坐标系(GCJ02)和百度坐标系(BD09)和地球坐标系(WGS84)的相互转换
# remotes::install_github("qlycool/geoChina", force = TRUE)
# https://github.com/versey-sherry/ChinaCoordinate
# 另外一种坐标系
# library(geoChina)
# wgs_coords <- gcjtowgs(gcjLat = geocoded_data01$lat, gcjLon = geocoded_data01$lng)

# ==========================================================================
# 加载高德API获取的信息
# ==========================================================================
# 加载高德解析的植物园信息
checklist_botanical_garden <- read_excel("checklist_botanical_garden.xlsx")
View(checklist_botanical_garden)

# 经纬度换算
wgs_coords <- purrr::map2(checklist_botanical_garden$latitude,
     checklist_botanical_garden$longitude, ~ gcjtowgs_acc(c(.x, .y))) %>%
list.rbind() %>% as.data.frame()

# 将est.year换成不同的等级
# merge data
checklist_bg01 <- checklist_botanical_garden %>%
cbind(wgs_coords) %>%
mutate(est_year_class = case_when(est.year < 1900 ~ 4,
                                  est.year >= 1901 & est.year < 1950 ~ 3,
                                  est.year >= 1951 & est.year < 2000 ~ 2,
                                  est.year >= 2000 ~ 1)) %>%
mutate(est_year_class = as.numeric(est_year_class))

# 参考数据
head(checklist_bg01)
tail(checklist_bg01)

# ==========================================================================
# 中国地图绘制
# ==========================================================================
# 主图绘制
checklist_bg_main_map <- ggplot() +
                         geom_polygon(data = china_map_province1, aes(x = long, y = lat, group = group), fill = "grey90", colour = "grey45") +
                         geom_point(data = checklist_bg01, aes(x = wgslon, y = wgslat, size = est_year_class, alpha = est_year_class), colour = "blue") +
                         scale_alpha(breaks = c(4, 3, 2, 1), range = c(1, 0.2), label = c("1880 - 1900", "1901 - 1950", "1951 - 2000", "2001 - 2020"),
                              name = "Founding time of\nbotanical garden") +
                         scale_size(breaks = c(4, 3, 2, 1), range = c(2, 8), label = c("1880 - 1900", "1901 - 1950", "1951 - 2000", "2001 - 2020"),
                              name = "Founding time of\nbotanical garden") +
                         coord_map("polyconic") +
                         coord_cartesian(xlim = c(74, 135), ylim = c(17, 53)) + # 调整地图区域
                         xlab("Longtitude") +
                         ylab("Latitude") +
                         theme(# 清除不必要的背景元素
                         # panel.grid = element_blank(),
                         # panel.background = element_rect(color = "white"),
                              rect = element_rect(fill = NA),
                              axis.line = element_blank(),
                              axis.ticks = element_line(size = 0.3, color = "black"),
                              axis.title = element_text(size = 16),
                              axis.text = element_text(size = 16),
                              legend.title = element_text(size = 16),
                              legend.text = element_text(size = 16),
                              legend.key = element_rect(fill = NA),
                              legend.position = c(0.125, 0.175),
                              panel.grid = element_line(colour = "grey90"),
                              panel.background = element_rect(fill = "white"),
                              panel.border = element_rect(fill = NA, color = "black", size = 0.3, linetype = "solid")
                              )

(checklist_bg_main_map)

# ==========================================================================

# 设计南海九段线
# ==========================================================================
attached_map <- ggplot(china_map_province1, aes(x = long, y = lat)) +
                geom_polygon(aes(group = group), fill = "white", colour = "grey45", size = 0.3) +
                geom_line(data = southsea01, aes(x = long, y = lat, group = group), color = "grey45", size = 0.3) + # add southsea01 map
                coord_cartesian(xlim = c(105, 125), ylim = c(3, 25)) +  # 缩小显示范围在南部区域
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
checklist_bg_main_map01 <- checklist_bg_main_map + inset_element(attached_map, 0.8, 0, 0.95, 0.5, ignore_tag = TRUE, align_to = 'plot')
                                                     # inset_element(attached_map, 0.8, 1.06, 0.95, 1.56, ignore_tag = TRUE)
(checklist_bg_main_map01)

# exporting map
ggexport(checklist_bg_main_map01, filename = "D:/我的坚果云/撰写的文章20201031/R_codes_for_nat.pla_in.China/BG地图/checklist_bg_map.png",
         width = 8000,
         height = 6000,
         pointsize = 12,
         res = 600)