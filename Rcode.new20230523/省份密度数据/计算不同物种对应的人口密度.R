# -*- coding: utf-8 -*-
# @Author: dbc
# @Date: 2023-05-23 22:45:13
# @Last Modified by: dbc
# @Last Modified time: 2023-05-24 16:47:33
# @Description: 计算省份的平均密度

# loading packages
library(readxl)
library(readr)
library(tidyr)
library(dplyr)

# cleaning memory
cat("\014")
rm(list=ls())
gc()

# 读取pages_summary_provience_Invspc_20201007NEW.v2.xlsx文件
file_path <- "G:/我的坚果云/撰写的文章20201031/R_codes_for_nat.pla_in.China/省份密度数据"
setwd(file_path)
getwd()

# 加载数据pages_summary_provience_Invspc_20201007NEW.v2.xlsx
checklist.prov <- read_excel("./pages_summary_provience_Invspc_20201007NEW.v2.xlsx", sheet = "pages_summary_provience_Invspc_")
str(checklist.prov)

# 将AH:ZJ列的数据变为长数据
checklist.prov.longer <- checklist.prov %>%
  pivot_longer(cols = AH:ZJ, names_to = "prov.id", values_to = "status") %>%
  filter(status == 1)

# View(checklist.prov.longer)
str(checklist.prov.longer)

# 读取human.density2001_2020.xlsx文件，读取总人口
prov.population <- read_excel("human.density2001_2020.xlsx", sheet = "province33")
str(prov.population)

# 计算X2001:X2020的数据平均值
prov.population01 <- prov.population %>%
  select(-c(X2001:X2010)) %>%
  rowwise() %>%
  mutate(population.mean = mean(c(X2011:X2020))) %>%
  select(prov.id, population.mean) %>%
  data.frame()

View(prov.population01)
str(prov.population01)

# 读取省份面积
prov.area <- read_excel("human.density2001_2020.xlsx", sheet = "area33")
str(prov.area)

prov.area01 <- prov.area %>% select(prov.id, area)
View(prov.area01)

# 合并两个数据框
checklist.prov.longer01 <- checklist.prov.longer %>%
left_join(prov.population01, by = "prov.id") %>%
left_join(prov.area01, by = "prov.id")

str(checklist.prov.longer01)

# 汇总计算省份的平均人口密度
checklist.prov.longer02 <- checklist.prov.longer01 %>%
group_by(Standardized_names) %>%
summarise(provinces = paste(prov.id, collapse = ", "),
          n.provinces = n(),
          population.total = sum(population.mean, na.rm = TRUE),
          area.total = sum(area, na.rm = TRUE),
          density = population.total/area.total)

str(checklist.prov.longer02)
View(checklist.prov.longer02)

write_excel_csv(checklist.prov.longer02, "checklist.prov.density.X2011_2020.csv")