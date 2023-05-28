#--------------------------------------------------------------------------#
#     .______.
#   __| _/\_ |__    ____
#  / __ |  | __ \ _/ ___\
# / /_/ |  | \_\ \\  \___
# \____ |  |___  / \___  >
#      \/      \/      \/
#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2022-02-10 20:09:02
# @Last Modified by: dbc
# @Last Modified time: 2023-05-26 17:10:07
# @Description: 对于物种来源与多个大陆进行分析
#--------------------------------------------------------------------------#
# set workspace
setwd("F:/2020_DATABASE_ALL_IN_ONE/12. All_in_one")
getwd()

#cleaning memory
cat("\014")
rm(list=ls())
gc()

#  loading packages
library(data.table)
library(migest)
library(readr)
library(stringr)
library(tidyr)
library(skimr)
library(dplyr)

# ==========================================================================
# loading data of checklist_Invspc_new
# ==========================================================================
# loading data of checklist_Invspc_new
checklist_Invspc_new <- read_csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv", locale = locale(encoding = "GB2312"))
View(checklist_Invspc_new)
glimpse(checklist_Invspc_new)
table(checklist_Invspc_new$naturalized_status)

#
# rename X1:X9
checklist_Invspc_new <- checklist_Invspc_new %>%
                            dplyr::rename(
                                          Europe = "X1",
                                          Africa = "X2",
                                          Asia_Temperate = "X3",
                                          Asia_Tropical = "X4",
                                          Australasia = "X5",
                                          Pacific = "X6",
                                          Northern_America = "X7",
                                          Southern_America = "X8",
                                          Antarctic = "X9")

# check str of checklist_Invspc_new
glimpse(checklist_Invspc_new)

# ==========================================================================
# 考虑多个来源大陆
# For the discussion of the overrepresentation of species from certain continents,
# it would be good to know how the different continents differ with regard to planting frequency, traits etc.
# The overrepresentation of the Americas could for example reflect that species from the Americas are planted in more gardens and/or have a higher climatic suitability.
# This would allow us to link the continent of origin results to the other data.
# Could you calculate for each continent of origin the mean values for each of the variables shown in Fig. 2, and send me a table with the results?
# Do this for all species (as in Fig. 1b) and for the subset of species native to a single continent (as in Fig. 1c).
# ==========================================================================
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
# [1] 0.0000000 0.9124758

# #
# # 数据转化
# # --------------------------------------------------------------------------
# # 对部分数据进行归一化
# checklist_Invspc_new$bg_num                 <- scale(log(checklist_Invspc_new$bg_num + 1))
# checklist_Invspc_new$chklst_prov_num        <- scale(log(checklist_Invspc_new$chklst_prov_num + 1))
# checklist_Invspc_new$new_sum.L3.code        <- scale(log(checklist_Invspc_new$new_sum.L3.code))
# checklist_Invspc_new$residence_time_to_2020 <- scale(log(checklist_Invspc_new$residence_time_to_2020))
# checklist_Invspc_new$hab_suit_mean      <- scale(log(checklist_Invspc_new$hab_suit_mean + 1))

# 查看数据
checklist_Invspc_new <- data.frame(checklist_Invspc_new)
glimpse(checklist_Invspc_new)
table(checklist_Invspc_new$naturalized_status)
View(checklist_Invspc_new)


# 基于生活史性状的Height_max_max
# --------------------------------------------------------------------------
# 提取!is.na()的数据
checklist_Invspc_interaction <- checklist_Invspc_new %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_interaction %>% select(naturalized_status, Height_max_max, Life_form_new))

# 改变Life_form_new的levels不同顺序
checklist_Invspc_interaction$Life_form_new <- factor(checklist_Invspc_interaction$Life_form_new)
checklist_Invspc_interaction$Life_form_new <- relevel(checklist_Invspc_interaction$Life_form_new, ref = "3")

# # 根据Life_form_new分组，对Height_max_max进行标准化
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")]))
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")]))
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")]))

# ==========================================================================
#
# 连续型数据
#
# ==========================================================================

continent.names = c(
                    "Europe",
                    "Africa",
                    "Asia_Temperate",
                    "Asia_Tropical",
                    "Australasia",
                    "Pacific",
                    "Northern_America",
                    "Southern_America",
                    "Antarctic"
                    )

# 空集
out <- vector()

for(i in 1:9){
# 大陆名称
cont.name <- continent.names[i]

# 赛选数据
checklist_Invspc_new01 <- checklist_Invspc_new[which(checklist_Invspc_new[, cont.name] == 1), ]
checklist_Invspc_interaction01 <- checklist_Invspc_interaction[which(checklist_Invspc_interaction[, cont.name] == 1), ]

# 汇总数据
bg_num_multiple.cont                 <- checklist_Invspc_new01 %>% filter(!is.na(bg_num)) %>% dplyr::summarise(n_species = n(), mean = mean(bg_num, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "bg_num")
chklst_prov_num_multiple.cont        <- checklist_Invspc_new01 %>% filter(!is.na(chklst_prov_num)) %>% dplyr::summarise(n_species = n(), mean = mean(chklst_prov_num, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "prov_num")
new_sum.L3.code_multiple.cont        <- checklist_Invspc_new01 %>% filter(!is.na(new_sum.L3.code)) %>% dplyr::summarise(n_species = n(), mean = mean(new_sum.L3.code, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "new_sum.L3.code")
residence_time_to_2020_multiple.cont <- checklist_Invspc_new01 %>% filter(!is.na(residence_time_to_2020)) %>% dplyr::summarise(n_species = n(), mean = mean(residence_time_to_2020, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "residence_time_to_2020")
hab_suit_mean_multiple.cont      <- checklist_Invspc_new01 %>% filter(!is.na(hab_suit_mean)) %>% dplyr::summarise(n_species = n(), mean = mean(hab_suit_mean, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "hab_suit_mean")

Height_max_max_multiple.cont         <- checklist_Invspc_interaction01 %>% filter(!is.na(Height_max_max)) %>% dplyr::summarise(n_species = n(), mean = mean(Height_max_max, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "Height_max_max")

# 合并数据集
cont.summary <- rbind(
                      bg_num_multiple.cont,
                      chklst_prov_num_multiple.cont,
                      new_sum.L3.code_multiple.cont,
                      residence_time_to_2020_multiple.cont,
                      hab_suit_mean_multiple.cont,
                      Height_max_max_multiple.cont
                      )

# 整合数据到out
out <- rbind(out, cont.summary)
}

# 整合数据
out$levels <- NA
multiple.cont.continuous_var_summary <- out %>% arrange(continent.name, variable)
write.csv(multiple.cont.continuous_var_summary, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523_multiple.cont.summary_for_continuous_variables_without_tf.csv", row.names = FALSE)

# 编程宽矩阵数据,指标
multiple.cont.continuous_var_summary_wide <- multiple.cont.continuous_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                                  names_from = continent.name,
                                                                                                  values_from = mean)

write.csv(multiple.cont.continuous_var_summary_wide, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_multiple.cont.summary_for_continuous_variables_without_tf.csv", row.names = FALSE)


# 编程宽矩阵数据，数量
multiple.cont.continuous_var_summary_wide_n.species <- multiple.cont.continuous_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                                  names_from = continent.name,
                                                                                                  values_from = n_species)

write.csv(multiple.cont.continuous_var_summary_wide_n.species, "D:/R_codes_for_nat.pla_in.China/Results20230523/XAXA20230523wide_multiple.cont.summary_for_continuous_variables_without_tf_n.species.csv", row.names = FALSE)


# ==========================================================================
#
# 离散型数据
#
# ==========================================================================
# 筛选数据集
data_life_form      <- checklist_Invspc_new %>% filter(!is.na(Life_form_new)) %>% select(naturalized_status, Life_form_new, Europe:Antarctic)
data_prop_type      <- checklist_Invspc_new %>% filter(!is.na(prop_type)) %>% select(naturalized_status, prop_type, Europe:Antarctic)
data_ali_new_status <- checklist_Invspc_new %>% filter(!is.na(ali_new_status)) %>% select(naturalized_status, ali_new_status, Europe:Antarctic)
# 筛选eco_use的数据
data_eco_use        <- checklist_Invspc_new %>% filter(!is.na(wcup_eco_use_num)) %>% select(naturalized_status, wcup_eco_use_num, Europe:Antarctic)
data_eco_use01      <- data_eco_use %>% mutate(wcup_eco_use_status = case_when(wcup_eco_use_num == 0 ~ 0, wcup_eco_use_num > 0 ~ 1))


continent.names = c(
                    "Europe",
                    "Africa",
                    "Asia_Temperate",
                    "Asia_Tropical",
                    "Australasia",
                    "Pacific",
                    "Northern_America",
                    "Southern_America",
                    "Antarctic"
                    )

# 空集
out <- vector()

for(i in 1:9){
# 大陆名称
cont.name <- continent.names[i]

# 筛选数据集
data_life_form01 <- data_life_form[which(data_life_form[, cont.name] == 1), ]
data_prop_type01 <- data_prop_type[which(data_prop_type[, cont.name] == 1), ]
data_ali_new_status01 <- data_ali_new_status[which(data_ali_new_status[, cont.name] == 1), ]
data_eco_use02 <- data_eco_use01[which(data_eco_use01[, cont.name] == 1), ]


# 汇总数据
Life_form_new_multiple.cont  <- data_life_form01 %>% filter(!is.na(Life_form_new)) %>% group_by(Life_form_new) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "Life_form_new")
prop_type_multiple.cont      <- data_prop_type01 %>% filter(!is.na(prop_type)) %>% group_by(prop_type) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "prop_type")
ali_new_status_multiple.cont <- data_ali_new_status01 %>% filter(!is.na(ali_new_status)) %>% group_by(ali_new_status) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "ali_new_status")
eco_use_multiple.cont        <- data_eco_use02  %>% filter(!is.na(wcup_eco_use_status)) %>% group_by(wcup_eco_use_status) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "wcup_eco_use_status")

Life_form_new_multiple.cont <- Life_form_new_multiple.cont %>% dplyr::rename(levels = Life_form_new)
prop_type_multiple.cont <- prop_type_multiple.cont %>% dplyr::rename(levels = prop_type)
ali_new_status_multiple.cont <- ali_new_status_multiple.cont %>% dplyr::rename(levels = ali_new_status)
eco_use_multiple.cont <- eco_use_multiple.cont %>% dplyr::rename(levels = wcup_eco_use_status)

# 合并数据集
cont.summary02 <- rbind(
                      Life_form_new_multiple.cont,
                      prop_type_multiple.cont,
                      ali_new_status_multiple.cont,
                      eco_use_multiple.cont
                      )


# 整合数据到out
out <- rbind(out, cont.summary02)
}

multiple.cont.discrete_var_summary <- out %>% arrange(continent.name, variable, levels)
write.csv(multiple.cont.discrete_var_summary, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523_multiple.cont.summary_for_discrete_variables_without_tf.csv", row.names = FALSE)

# 编程宽矩阵数据
multiple.cont.discrete_var_summary_wide <- multiple.cont.discrete_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                              names_from = continent.name,
                                                                                              values_from = proportion)

write.csv(multiple.cont.discrete_var_summary_wide, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_multiple.cont.summary_for_discrete_variables_without_tf.csv", row.names = FALSE)


# 编程宽矩阵数据，数量
multiple.cont.discrete_var_summary_wide_n.species <- multiple.cont.discrete_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                              names_from = continent.name,
                                                                                              values_from = n_species)

write.csv(multiple.cont.discrete_var_summary_wide_n.species, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_multiple.cont.summary_for_discrete_variables_n.species_without_tf.csv", row.names = FALSE)

#
# 汇总所有连续和离散数据,指标
# --------------------------------------------------------------------------
level.names = c(
                "bg_num",
                "prov_num",
                "ali_new_status",
                "wcup_eco_use_status",
                "hab_suit_mean",
                "new_sum.L3.code",
                "residence_time_to_2020",
                "Life_form_new",
                "prop_type",
                "Height_max_max"
                )

multiple.cont.all_var_summary_wide <- bind_rows(multiple.cont.continuous_var_summary_wide, multiple.cont.discrete_var_summary_wide)
multiple.cont.all_var_summary_wide$variable <- factor(multiple.cont.all_var_summary_wide$variable, levels = level.names)
multiple.cont.all_var_summary_wide01 <- multiple.cont.all_var_summary_wide[order(multiple.cont.all_var_summary_wide$variable), ]
multiple.cont.all_var_summary_wide01


write.csv(multiple.cont.all_var_summary_wide01, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_multiple.cont.summary_for_all_variables_without_tf.csv", row.names = FALSE)


# 汇总所有连续和离散数据,数量
multiple.cont.all_var_summary_wide_n.species <- bind_rows(multiple.cont.continuous_var_summary_wide_n.species, multiple.cont.discrete_var_summary_wide_n.species)
multiple.cont.all_var_summary_wide_n.species$variable <- factor(multiple.cont.all_var_summary_wide_n.species$variable, levels = level.names)
multiple.cont.all_var_summary_wide_n.species01 <- multiple.cont.all_var_summary_wide_n.species[order(multiple.cont.all_var_summary_wide_n.species$variable), ]
multiple.cont.all_var_summary_wide_n.species01

write.csv(multiple.cont.all_var_summary_wide_n.species01, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_multiple.cont.summary_for_all_variables_n.species_without_tf.csv", row.names = FALSE)


#--------------------------------------------------------------------------#
#     .______.
#   __| _/\_ |__    ____
#  / __ |  | __ \ _/ ___\
# / /_/ |  | \_\ \\  \___
# \____ |  |___  / \___  >
#      \/      \/      \/
#
# -*- coding: utf-8 -*-
# @Author: Bicheng Dong
# @Date: 2022-02-10 20:09:02
# @Last Modified by:
# @Last Modified time: 2022-02-10 22:19:10
# @Description: 对于物种只是来源于1个大陆进行分析
#--------------------------------------------------------------------------#
# set workspace
setwd("F:/2020_DATABASE_ALL_IN_ONE/12. All_in_one")
getwd()

#cleaning memory
cat("\014")
rm(list=ls())
gc()

#  loading packages
library(data.table)
library(migest)
library(readr)
library(stringr)
library(tidyr)
library(skimr)
library(dplyr)

# ==========================================================================
# loading data of checklist_Invspc_new
# ==========================================================================
# loading data of checklist_Invspc_new
checklist_Invspc_new00 <- read_csv("D:/R_codes_for_nat.pla_in.China/data/checklist_nat.pla_in_China.updated20230523.csv", locale = locale(encoding = "GB2312"))
View(checklist_Invspc_new00)
table(checklist_Invspc_new00$naturalized_status)

# 赛选只有一个来源大陆的物种
checklist_Invspc_new <- checklist_Invspc_new00 %>% filter(!is.na(new_sum.L1.code)) %>% filter(new_sum.L1.code == 1)
table(checklist_Invspc_new$naturalized_status)
range(checklist_Invspc_new$new_sum.L1.code)

#
# rename X1:X9
checklist_Invspc_new <- checklist_Invspc_new %>%
                            dplyr::rename(
                                          Europe = "X1",
                                          Africa = "X2",
                                          Asia_Temperate = "X3",
                                          Asia_Tropical = "X4",
                                          Australasia = "X5",
                                          Pacific = "X6",
                                          Northern_America = "X7",
                                          Southern_America = "X8",
                                          Antarctic = "X9")

# check str of checklist_Invspc_new
glimpse(checklist_Invspc_new)

# ==========================================================================
# 只是考虑单个来源大陆
# For the discussion of the overrepresentation of species from certain continents,
# it would be good to know how the different continents differ with regard to planting frequency, traits etc.
# The overrepresentation of the Americas could for example reflect that species from the Americas are planted in more gardens and/or have a higher climatic suitability.
# This would allow us to link the continent of origin results to the other data.
# Could you calculate for each continent of origin the mean values for each of the variables shown in Fig. 2, and send me a table with the results?
# Do this for all species (as in Fig. 1b) and for the subset of species native to a single continent (as in Fig. 1c).
# # ==========================================================================
# #
# # 数据转化
# # --------------------------------------------------------------------------
# # 对部分数据进行归一化
# checklist_Invspc_new$bg_num                 <- scale(log(checklist_Invspc_new$bg_num + 1))
# checklist_Invspc_new$chklst_prov_num        <- scale(log(checklist_Invspc_new$chklst_prov_num + 1))
# checklist_Invspc_new$new_sum.L3.code        <- scale(log(checklist_Invspc_new$new_sum.L3.code))
# checklist_Invspc_new$residence_time_to_2020 <- scale(log(checklist_Invspc_new$residence_time_to_2020))
# checklist_Invspc_new$hab_suit_mean      <- scale(log(checklist_Invspc_new$hab_suit_mean + 0.001))

# 查看数据
checklist_Invspc_new <- data.frame(checklist_Invspc_new)
glimpse(checklist_Invspc_new)
table(checklist_Invspc_new$naturalized_status)
View(checklist_Invspc_new)


# 基于生活史性状的Height_max_max
# --------------------------------------------------------------------------
# 提取!is.na()的数据
checklist_Invspc_interaction <- checklist_Invspc_new %>% filter(!is.na(Height_max_max) & !is.na(Life_form_new))
skimr::skim(checklist_Invspc_interaction %>% select(naturalized_status, Height_max_max, Life_form_new))

# 改变Life_form_new的levels不同顺序
checklist_Invspc_interaction$Life_form_new <- factor(checklist_Invspc_interaction$Life_form_new)
checklist_Invspc_interaction$Life_form_new <- relevel(checklist_Invspc_interaction$Life_form_new, ref = "3")

# # 根据Life_form_new分组，对Height_max_max进行标准化
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "1")]))
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "2")]))
# checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")] <- scale(log(checklist_Invspc_interaction$Height_max_max[which(checklist_Invspc_interaction$Life_form_new == "3")]))

# ==========================================================================
#
# 连续型数据
#
# ==========================================================================

continent.names = c(
                    "Europe",
                    "Africa",
                    "Asia_Temperate",
                    "Asia_Tropical",
                    "Australasia",
                    "Pacific",
                    "Northern_America",
                    "Southern_America",
                    "Antarctic"
                    )

# 空集
out <- vector()

for(i in 1:9){
# 大陆名称
cont.name <- continent.names[i]

# 赛选数据
checklist_Invspc_new01 <- checklist_Invspc_new[which(checklist_Invspc_new[, cont.name] == 1), ]
checklist_Invspc_interaction01 <- checklist_Invspc_interaction[which(checklist_Invspc_interaction[, cont.name] == 1), ]

# 汇总数据
bg_num_single.cont                 <- checklist_Invspc_new01 %>% filter(!is.na(bg_num)) %>% dplyr::summarise(n_species = n(), mean = mean(bg_num, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "bg_num")
chklst_prov_num_single.cont        <- checklist_Invspc_new01 %>% filter(!is.na(chklst_prov_num)) %>% dplyr::summarise(n_species = n(), mean = mean(chklst_prov_num, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "prov_num")
new_sum.L3.code_single.cont        <- checklist_Invspc_new01 %>% filter(!is.na(new_sum.L3.code)) %>% dplyr::summarise(n_species = n(), mean = mean(new_sum.L3.code, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "new_sum.L3.code")
residence_time_to_2020_single.cont <- checklist_Invspc_new01 %>% filter(!is.na(residence_time_to_2020)) %>%  dplyr::summarise(n_species = n(), mean = mean(residence_time_to_2020, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "residence_time_to_2020")
hab_suit_mean_single.cont      <- checklist_Invspc_new01 %>% filter(!is.na(hab_suit_mean)) %>% dplyr::summarise(n_species = n(), mean = mean(hab_suit_mean, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "hab_suit_mean")

Height_max_max_single.cont   <- checklist_Invspc_interaction01 %>% filter(!is.na(Height_max_max)) %>% dplyr::summarise(n_species = n(), mean = mean(Height_max_max, na.rm = TRUE)) %>% mutate(continent.name = cont.name, variable = "Height_max_max")

# 合并数据集
cont.summary <- rbind(
                      bg_num_single.cont,
                      chklst_prov_num_single.cont,
                      new_sum.L3.code_single.cont,
                      residence_time_to_2020_single.cont,
                      hab_suit_mean_single.cont,
                      Height_max_max_single.cont
                      )

# 整合数据到out
out <- rbind(out, cont.summary)
}

# 整合数据
out$levels <- NA
single.cont.continuous_var_summary <- out %>% arrange(continent.name, variable)
write.csv(single.cont.continuous_var_summary, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523_single.cont.summary_for_continuous_variables_without_tf.csv", row.names = FALSE)

# 编程宽矩阵数据,指标
single.cont.continuous_var_summary_wide <- single.cont.continuous_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                                  names_from = continent.name,
                                                                                                  values_from = mean)

write.csv(single.cont.continuous_var_summary_wide, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_continuous_variables_without_tf.csv", row.names = FALSE)


# 编程宽矩阵数据，数量
single.cont.continuous_var_summary_wide_n.species <- single.cont.continuous_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                                  names_from = continent.name,
                                                                                                  values_from = n_species)

write.csv(single.cont.continuous_var_summary_wide_n.species, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_continuous_variables_n.species_without_tf.csv", row.names = FALSE)


# ==========================================================================
#
# 离散型数据
#
# ==========================================================================
# 筛选数据集
data_life_form      <- checklist_Invspc_new %>% filter(!is.na(Life_form_new)) %>% select(naturalized_status, Life_form_new, Europe:Antarctic)
data_prop_type      <- checklist_Invspc_new %>% filter(!is.na(prop_type)) %>% select(naturalized_status, prop_type, Europe:Antarctic)
data_ali_new_status <- checklist_Invspc_new %>% filter(!is.na(ali_new_status)) %>% select(naturalized_status, ali_new_status, Europe:Antarctic)
# 筛选eco_use的数据
data_eco_use        <- checklist_Invspc_new %>% filter(!is.na(wcup_eco_use_num)) %>% select(naturalized_status, wcup_eco_use_num, Europe:Antarctic)
data_eco_use01      <- data_eco_use %>% mutate(wcup_eco_use_status = case_when(wcup_eco_use_num == 0 ~ 0, wcup_eco_use_num > 0 ~ 1))


continent.names = c(
                    "Europe",
                    "Africa",
                    "Asia_Temperate",
                    "Asia_Tropical",
                    "Australasia",
                    "Pacific",
                    "Northern_America",
                    "Southern_America",
                    "Antarctic"
                    )

# 空集
out <- vector()

for(i in 1:9){
# 大陆名称
cont.name <- continent.names[i]

# 筛选数据集
data_life_form01 <- data_life_form[which(data_life_form[, cont.name] == 1), ]
data_prop_type01 <- data_prop_type[which(data_prop_type[, cont.name] == 1), ]
data_ali_new_status01 <- data_ali_new_status[which(data_ali_new_status[, cont.name] == 1), ]
data_eco_use02 <- data_eco_use01[which(data_eco_use01[, cont.name] == 1), ]


# 汇总数据
Life_form_new_single.cont  <- data_life_form01 %>% filter(!is.na(Life_form_new)) %>% group_by(Life_form_new) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "Life_form_new")
prop_type_single.cont      <- data_prop_type01 %>% filter(!is.na(prop_type)) %>% group_by(prop_type) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "prop_type")
ali_new_status_single.cont <- data_ali_new_status01 %>% filter(!is.na(ali_new_status)) %>% group_by(ali_new_status) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "ali_new_status")
eco_use_single.cont        <- data_eco_use02  %>% filter(!is.na(wcup_eco_use_status)) %>% group_by(wcup_eco_use_status) %>% dplyr::summarise(n_species = n(), n_naturalized = length(naturalized_status[naturalized_status == "1"]), n_not_naturalized = length(naturalized_status[naturalized_status == "0"]), n_all_species = n_naturalized + n_not_naturalized, proportion = n_naturalized/n_all_species) %>% mutate(continent.name = cont.name, variable = "wcup_eco_use_status")

Life_form_new_single.cont <- Life_form_new_single.cont %>% dplyr::rename(levels = Life_form_new)
prop_type_single.cont <- prop_type_single.cont %>% dplyr::rename(levels = prop_type)
ali_new_status_single.cont <- ali_new_status_single.cont %>% dplyr::rename(levels = ali_new_status)
eco_use_single.cont <- eco_use_single.cont %>% dplyr::rename(levels = wcup_eco_use_status)

# 合并数据集
cont.summary02 <- rbind(
                      Life_form_new_single.cont,
                      prop_type_single.cont,
                      ali_new_status_single.cont,
                      eco_use_single.cont
                      )


# 整合数据到out
out <- rbind(out, cont.summary02)
}

single.cont.discrete_var_summary <- out %>% arrange(continent.name, variable, levels)
write.csv(single.cont.discrete_var_summary, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523_single.cont.summary_for_discrete_variables_without_tf.csv", row.names = FALSE)

# 编程宽矩阵数据
single.cont.discrete_var_summary_wide <- single.cont.discrete_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                              names_from = continent.name,
                                                                                              values_from = proportion)

write.csv(single.cont.discrete_var_summary_wide, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_discrete_variables_without_tf.csv", row.names = FALSE)


# 编程宽矩阵数据，数量
single.cont.discrete_var_summary_wide_n.species <- single.cont.discrete_var_summary %>% pivot_wider(id_cols = c("variable", "levels"),
                                                                                              names_from = continent.name,
                                                                                              values_from = n_species)

write.csv(single.cont.discrete_var_summary_wide_n.species, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_discrete_variables_n.species_without_tf.csv", row.names = FALSE)

#
# 汇总所有连续和离散数据,指标
# --------------------------------------------------------------------------
level.names = c(
                "bg_num",
                "prov_num",
                "ali_new_status",
                "wcup_eco_use_status",
                "hab_suit_mean",
                "new_sum.L3.code",
                "residence_time_to_2020",
                "Life_form_new",
                "prop_type",
                "Height_max_max"
                )


single.cont.all_var_summary_wide <- bind_rows(single.cont.continuous_var_summary_wide, single.cont.discrete_var_summary_wide)
single.cont.all_var_summary_wide$variable <- factor(single.cont.all_var_summary_wide$variable, levels = level.names)
single.cont.all_var_summary_wide01 <- single.cont.all_var_summary_wide[order(single.cont.all_var_summary_wide$variable), ]
single.cont.all_var_summary_wide01


write.csv(single.cont.all_var_summary_wide01, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_all_variables_without_tf.csv", row.names = FALSE)


# 汇总所有连续和离散数据,数量
single.cont.all_var_summary_wide_n.species <- bind_rows(single.cont.continuous_var_summary_wide_n.species, single.cont.discrete_var_summary_wide_n.species)
single.cont.all_var_summary_wide_n.species$variable <- factor(single.cont.all_var_summary_wide_n.species$variable, levels = level.names)
single.cont.all_var_summary_wide_n.species01 <- single.cont.all_var_summary_wide_n.species[order(single.cont.all_var_summary_wide_n.species$variable), ]
single.cont.all_var_summary_wide_n.species01

write.csv(single.cont.all_var_summary_wide_n.species01, "D:/R_codes_for_nat.pla_in.China/Results20230523/XA20230523wide_single.cont.summary_for_all_variables_n.species_without_tf.csv", row.names = FALSE)