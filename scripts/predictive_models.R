#Use final models to predict outcomes of climate/population change scenarios

#Load required packages

library(tidyverse)
library(broom)
library(ggpubr)
library(scales)
library(ggrepel)


#Import Datasets

analysis_hucs <- read.csv("data/scecap_hucs_for_analysis.csv")
analysis_buffs <- read.csv("data/scecap_buffers_for_analysis.csv")
analysis_grids <- read.csv("data/scecap_fishnetgrid_for_analysis.csv")


#Prepare datasets for each dependent variable----

huc_ermqddt_data <- analysis_hucs %>% 
                    filter(!StationCode %in% 
                           c("RT00526",  "RT00549",  "RT022152", 
                             "RT042194", "RO056099", "RT07038", 
                             "RT10132",  "RT13059",  "RO15368")) %>%
                    filter(!is.na(ERMQ_DDT_tot))

huc_ermqmet_data <- analysis_hucs %>% 
                    filter(!StationCode %in% 
                           c("RT01628", "RO026030")) %>%
                    filter(!is.na(ERMQ_met)) 

huc_ermqpah_data <- analysis_hucs %>% 
                    filter(!StationCode %in% 
                           c("RO00056", "RO056092", "RO06320", 
                             "RO11308", "RT11003"))

huc_ermqpcb_data <- analysis_hucs %>% 
                    filter(!StationCode %in% 
                           c("RO99322",  "RT00549",  "RT01633", 
                             "RO036042", "RO036053", "RO036054", 
                             "RO046082", "RT042191", "RT10132", 
                             "RO11308"))

huc_ermqall_data <- analysis_hucs %>% 
                    filter(!StationCode %in% 
                           c("RO00056", "RO06320", "RO11308"))
                  
buff_fecal_data <- analysis_buff %>% 
                   filter(!StationCode %in% 
                          c("RT12024"))

buff_fecal_data <- analysis_buff %>% 
                   filter(!StationCode %in% 
                          c("RT12024"))

buff_entero_data <- analysis_buff 

huc_trawlabund_data <- analysis_hucs %>% 
                       filter(!StationCode %in% 
                              c("RT022153", "RT06010", "RT06037"))

huc_trawlsprichall_data <- analysis_hucs

huc_trawlsprichtidal_data <- analysis_hucs %>% 
                             filter(RT_RO_Other == "TidalCreek")

huc_trawlsprichopen_data <- analysis_hucs %>% 
                            filter(RT_RO_Other == "OpenWater")

huc_benthabund_data <- analysis_hucs %>% 
                       filter(!StationCode %in% 
                       c("RO00059", "RT032180", "RO06305"))

huc_benthsprich_data <- analysis_hucs

grid_mambi_data <- analysis_grids


#Build Linear Models----

ermq_ddt_model <- lm(data = huc_ermqddt_data[-c(1276, 1075, 1351, 1449, 1264,
                                                1482, 1467, 1088, 1441, 1495,
                                                1222, 1507, 884, 1372, 1301, 
                                                1122, 1362, 1340, 1013, 1043,
                                                1086),], 
                     subset = (HUC_SIZE == 10), 
                     ERMQ_DDT_tot ~ 
                       TOC_sediment +
                       DJF_PRCP_diff +
                       WATCAT_prct +
                       AGRCAT_prct +
                       IMPERV_PRCT)


ermq_metal_model<- lm(data = huc_ermqmet_data[-c(1358,1451,1300,816,988, 1367,
                                                 1441),], 
                      subset = (HUC_SIZE == 10),
                      ERMQ_met ~
                        SiltClay +
                        SP02 +
                        preSON_PRCP_diff +
                        POORLY_DRAINE_prct +
                        IMPERV_PRCT)


ermq_pah_model <- lm(data = huc_ermqpah_data[-c(1212,989,1053,827,1048,
                                                1512, 1005, 1554, 1370, 1361, 991, 
                                                1003, 904, 1403, 1327, 916, 1429,
                                                932, 872, 1128),], 
                     subset = (HUC_SIZE == 10),
                     ERMQ_PAH_tot ~ 
                       SiltClay +
                       CHANNEL_WIDTH_log +
                       DJF_PRCP_diff*DJF_TAVG_diff +  
                       MIXED_FOREST_prct +
                       personsPERha)


ermq_pcb_model<- lm(data = huc_ermqpcb_data[-c(1358,1345,1560,1135,1306,838,
                                               1083, 1059, 1078, 1317, 899,
                                               1045, 1042, 1086, 1034),], 
                    subset = (HUC_SIZE == 10),
                    ERMQ_PCB_tot ~ 
                      SiltClay +
                      MAM_PRCP_diff*DJF_TAVG_diff +
                      personsPERha)


ermq_all_model <- lm(data = huc_ermqall_data[-c(1050, 988, 1007, 1394, 
                                                816, 829, 1373),], 
                     subset = (HUC_SIZE == 10),
                     ERMQ_ALL ~ 
                       SiltClay +
                       DJF_PRCP_diff +
                       DJF_TAVG_diff +
                       GROUP_A_prct +
                       personsPERha)


fecal_model <- lm(data = buff_fecal_data, 
                  subset = (BUFFER_SIZE == 2),
                  FecalColiform_LOG ~ 
                    Logger_Salin_mean +
                    CHANNEL_WIDTH_log +
                    PRCP_2 + 
                    PDSI +
                    UPLAND_IMPERV_PRCT +
                    MIX_FOREST_prct +
                    MARCAT_prct)


entero_model <- lm(data = buff_entero_data[-c(989, 1013, 1173, 890, 894,
                                              914, 860, 972, 1258, 1237),], 
                   subset = (BUFFER_SIZE == 3),
                   Entero_LOG ~ 
                     Logger_Salin_mean +
                     CHANNEL_WIDTH_log +
                     PRCP_2 +
                     DAYMET_30dTAVG_degC +
                     UPLAND_IMPERV_PRCT)


trawlabund_model <- lm(data = huc_trawlabund_data, 
                       subset = (HUC_SIZE == 12),
                       log(trawl_ABUNDANCE_area) ~ 
                         CHANNEL_WIDTH_log +
                         IMPERV_PRCT +
                         ANN_TAVG_diff +
                         NAO)


trawlsprichopen_model <- lm(data = huc_trawlsprichopen_data[-c(1222,1113),], 
                            subset = (HUC_SIZE == 12),
                            trawl_SP_RICH  ~ 
                              CHANNEL_WIDTH_log +
                              Logger_Salin_mean +
                              NAO +
                              MARCAT_prct +
                              DAYMET_30dPRCP_cm +
                              preANN_TAVG_diff)


trawlsprichtidal_model <- lm(data = huc_trawlsprichtidal_data[-c(1028, 1086),], 
                             subset = (HUC_SIZE == 12),
                             trawl_SP_RICH  ~ 
                               Logger_Salin_mean +
                               AGRCAT_prct +
                               ANN_TAVG_diff +
                               SP02 +
                               CHANNEL_WIDTH_log)


benthabund_model <- lm(data = huc_benthabund_data[-c(1687, 1848, 1976, 1930, 2404,
                                                     2189, 2029, 2248, 2039, 1896, 
                                                     2179, 2163, 2424),], 
                       subset = (HUC_SIZE == 12),
                       log1p(benth_ABUNDANCE_AREA) ~ 
                         Logger_Salin_mean +
                         DAYMET_90dTAVG_degC +
                         SiltClay +
                         DJF_TAVG_diff +
                         DAYMET_45dPRCP_cm +
                         MARCAT_prct)


benthsprich_model <- lm(data = huc_benthsprich_data, 
                        subset = (HUC_SIZE == 12),
                        log(benth_SP_RICH) ~ 
                          Logger_Salin_mean +
                          DAYMET_90dTAVG_degC +
                          DAYMET_45dPRCP_cm +
                          SiltClay +
                          CHANNEL_WIDTH_log +
                          DEVLOWCAT_prct + 
                          MARCAT_prct)


mambi_model <- lm(data = grid_mambi_data[-c(1628,1757, 1792,
                                            2105, 1918, 1956,
                                            1849, 1823, 1925,
                                            2123, 1965, 2212),], 
                  subset = (FISHNET_SIZE == 400),
                  MAMBI ~ 
                    Logger_Salin_mean +
                    CHANNEL_WIDTH_log +
                    SiltClay +
                    DAYMET_90dTAVG_degC +
                    DAYMET_45dPRCP_cm +
                    DEVOPEN_prct +
                    DEVHIGHCAT_prct)

#Create "average" SCECAP conditions for prediction datasets for each spatial grouping----


##HUC watershed prediction summary dataset

analysis_summ_byhabitat <- analysis_hucs %>% 
                           group_by(HUC_SIZE, RT_RO_Other) %>% 
                           summarize_if(is.numeric, mean, na.rm = T)

analysis_summ_byall <-  analysis_hucs %>%
                        mutate(RT_RO_Other = "AllHabitats") %>%
                        group_by(HUC_SIZE, RT_RO_Other) %>% 
                        summarize_if(is.numeric, mean, na.rm = T)

analysis_summary_hucs <- rbind(analysis_summ_byhabitat, analysis_summ_byall)


##Buffer prediction summary dataset

analysis_buff_byhabitat <- analysis_buffs %>% 
  group_by(BUFFER_SIZE, RT_RO_Other) %>% 
  summarize_if(is.numeric, mean, na.rm = T)

analysis_buff_byall <-  analysis_buffs %>%
  mutate(RT_RO_Other = "AllHabitats") %>%
  group_by(BUFFER_SIZE, RT_RO_Other) %>% 
  summarize_if(is.numeric, mean, na.rm = T)

analysis_summary_buffs <- rbind(analysis_buff_byhabitat, analysis_buff_byall)


#Grid prediction summary dataset

analysis_grid_byhabitat <- analysis_grids %>% 
  group_by(FISHNET_SIZE, RT_RO_Other) %>% 
  summarize_if(is.numeric, mean, na.rm = T)

analysis_grid_byall <-  analysis_grids %>%
  mutate(RT_RO_Other = "AllHabitats") %>%
  group_by(FISHNET_SIZE, RT_RO_Other) %>% 
  summarize_if(is.numeric, mean, na.rm = T)

analysis_summary_grids <- rbind(analysis_grid_byhabitat, analysis_grid_byall)



#Build functions to change climate and population variables in prediction datasets based on low and high change scenarios----

#Precipitation: 5% increase (low), 10% increase (high) [observed data]
#Precipitation: +6.6cm/yr or 1.7cm/season (low), +13cm/yr or 3.3cm/season (high) [annual, seasonal data]
#Temperature:   5% increase (low), 6.5% increase (high) [observed data]
#Temperature:   +1.9degC (low), +2.4degC (high) [annual, seasonal data]
#Population:    130% increase (low), 180% increase (high) [census, developed landcover, impervious cover data]

prcp_change_low <- function(x){
  ((x*0.05)+x)
}

prcp_change_high <- function(x){
  ((x*0.1)+x)
}

temp_change_low <- function(x){
  ((x*0.05)+x)
}

temp_change_high <- function(x){
  ((x*0.065)+x)
}

seasonprcp_change_low  <- function(x){
  (x+1.7)
}

seasonprcp_change_high <- function(x){
  (x+3.3)
}

annprcp_change_low  <- function(x){
  (x+6.6)
}

annprcp_change_high <- function(x){
  (x+13)
}

seasontemp_change_low <- function(x){
  (x + 1.9)
}

seasontemp_change_high <- function(x){
  (x + 2.4)
}

pop_change_low <- function(x){
  ((x*1.3)+x)
}

pop_change_high <- function(x){
  ((x*1.8)+x)
}

remove_response <- function(x){
  as.numeric(NA)
}

climate_normals <- function(x){
  x*0
}


#Build scenarios based on different combinations of projections (including baseline scenario)----

#Scenario 0: Normal Climate, Current Population
#Scenario 1: Population Growth (low)
#Scenario 2: Temperature Change (low)
#Scenario 3: Precipitation Change (low)
#Scenario 4: Temperature and Precipitation Change (low)
#Scenario 5: Population Growth and Temperature Change (low)
#Scenario 6: Population Growth and Precipitation Change (low)
#Scenario 7: Population Growth, Temperature Change, Precipitation Change (low)
#Scenario 8: Population Growth (high)
#Scenario 9: Temperature Change (high)
#Scenario 10: Precipitation Change (high)
#Scenario 11: Temperature and Precipitation Change (high)
#Scenario 12: Population Growth and Temperature Change (high)
#Scenario 13: Population Growth and Precipitation Change (high)
#Scenario 14: Population Growth, Temperature Change, Precipitation Change (high)

##HUC Watersheds

hucs_scenario_0 <- analysis_summary_hucs %>%
  mutate_at(vars(71:82), .funs = climate_normals) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "0")

hucs_scenario_1 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "1")

hucs_scenario_2 <- analysis_summary_hucs %>% 
  mutate_at(vars(52:58, 65:70), .funs = temp_change_low) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "2")

hucs_scenario_3 <- analysis_summary_hucs %>% 
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_low) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(83,85), .funs = annprcp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "3")

hucs_scenario_4 <- analysis_summary_hucs %>% 
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_low) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_low) %>%
  mutate_at(vars(52:58, 65:70), .funs = temp_change_low) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "4")

hucs_scenario_5 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_low) %>%
  mutate_at(vars(52:58, 65:70), .funs = temp_change_low) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "5")

hucs_scenario_6 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_low) %>%
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_low) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "6")

hucs_scenario_7 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_low) %>%
  mutate_at(vars(52:58,65:70), .funs = temp_change_low) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_low) %>%
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_low) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_low) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "7")

hucs_scenario_8 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "8")

hucs_scenario_9 <- analysis_summary_hucs %>% 
  mutate_at(vars(52:58, 65:70), .funs = temp_change_high) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "9")

hucs_scenario_10 <- analysis_summary_hucs %>% 
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_high) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "10")

hucs_scenario_11 <- analysis_summary_hucs %>% 
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_high) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_high) %>%
  mutate_at(vars(52:58, 65:70), .funs = temp_change_high) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "11")

hucs_scenario_12 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_high) %>%
  mutate_at(vars(52:58, 65:70), .funs = temp_change_high) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "12")

hucs_scenario_13 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_high) %>%
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_high) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "13")

hucs_scenario_14 <- analysis_summary_hucs %>% 
  mutate_at(vars(99,101:104,115:117,141,142), .funs = pop_change_high) %>%
  mutate_at(vars(52:58, 65:70), .funs = temp_change_high) %>%
  mutate_at(vars(71,73,75,77,79,81), .funs = seasontemp_change_high) %>%
  mutate_at(vars(44:50,59:64,90:96), .funs = prcp_change_high) %>%
  mutate_at(vars(72,74,76,80), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(78,82), .funs = annprcp_change_high) %>%
  mutate_at(vars(13,16:43), .funs = remove_response) %>%
  mutate(SCENARIO = "14")


##merge into one dataset

hucs_scenarios_all <- rbind(hucs_scenario_0, hucs_scenario_1, hucs_scenario_2,
                            hucs_scenario_3, hucs_scenario_4, hucs_scenario_5,
                            hucs_scenario_6, hucs_scenario_7, hucs_scenario_8,
                            hucs_scenario_9, hucs_scenario_10, hucs_scenario_11,
                            hucs_scenario_12, hucs_scenario_13, hucs_scenario_14)


##filter by HUC size as needed

huc10_scenarios <- hucs_scenarios_all %>% filter(HUC_SIZE == 10)
huc12_scenarios <- hucs_scenarios_all %>% filter(HUC_SIZE == 12)


#Buffers

buffs_scenario_0 <- analysis_summary_buffs %>%
  mutate_at(vars(71:82), .funs = climate_normals) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "0")

buffs_scenario_1 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "1")

buffs_scenario_2 <- analysis_summary_buffs %>% 
  mutate_at(vars(50:56,62:68), .funs = temp_change_low) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "2")

buffs_scenario_3 <- analysis_summary_buffs %>% 
  mutate_at(vars(42:48,57:62), .funs = prcp_change_low) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(83,85), .funs = annprcp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "3")

buffs_scenario_4 <- analysis_summary_buffs %>% 
  mutate_at(vars(42:48,57:62), .funs = prcp_change_low) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_low) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "4")

buffs_scenario_5 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_low) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_low) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "5")

buffs_scenario_6 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_low) %>%
  mutate_at(vars(42:48,57:62), .funs = prcp_change_low) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "6")

buffs_scenario_7 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_low) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_low) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_low) %>%
  mutate_at(vars(42:48,57:62), .funs = prcp_change_low) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "7")

buffs_scenario_8 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "8")

buffs_scenario_9 <- analysis_summary_buffs %>% 
  mutate_at(vars(50:56,62:68), .funs = temp_change_high) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "9")

buffs_scenario_10 <- analysis_summary_buffs %>% 
  mutate_at(vars(42:48,57:62), .funs = prcp_change_high) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "10")

buffs_scenario_11 <- analysis_summary_buffs %>% 
  mutate_at(vars(42:48,57:62), .funs = prcp_change_high) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_high) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "11")

buffs_scenario_12 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_high) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_high) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "12")

buffs_scenario_13 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_high) %>%
  mutate_at(vars(42:48,57:62), .funs = prcp_change_high) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "13")

buffs_scenario_14 <- analysis_summary_buffs %>% 
  mutate_at(vars(96,98:103,112:114), .funs = pop_change_high) %>%
  mutate_at(vars(50:56,62:68), .funs = temp_change_high) %>%
  mutate_at(vars(69,71,73,75,77,79), .funs = seasontemp_change_high) %>%
  mutate_at(vars(42:48,57:62), .funs = prcp_change_high) %>%
  mutate_at(vars(70,72,74,78), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(76,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(12:41), .funs = remove_response) %>%
  mutate(SCENARIO = "14")


##merge into one dataset

buffs_scenarios_all <- rbind(buffs_scenario_0, buffs_scenario_1, buffs_scenario_2,
                            buffs_scenario_3, buffs_scenario_4, buffs_scenario_5,
                            buffs_scenario_6, buffs_scenario_7, buffs_scenario_8,
                            buffs_scenario_9, buffs_scenario_10, buffs_scenario_11,
                            buffs_scenario_12, buffs_scenario_13, buffs_scenario_14)


##filter by buffer size as needed

buff2km_scenarios <- buffs_scenarios_all %>% filter(BUFFER_SIZE == 2)
buff3km_scenarios <- buffs_scenarios_all %>% filter(BUFFER_SIZE == 3)


#Grids

grids_scenario_0 <- analysis_summary_grids %>%
  mutate_at(vars(67:78), .funs = climate_normals) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "0")

grids_scenario_1 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "1")

grids_scenario_2 <- analysis_summary_grids %>% 
  mutate_at(vars(44:54,61:66), .funs = temp_change_low) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "2")

grids_scenario_3 <- analysis_summary_grids %>% 
  mutate_at(vars(40:46,55:60), .funs = prcp_change_low) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(83,85), .funs = annprcp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "3")

grids_scenario_4 <- analysis_summary_grids %>% 
  mutate_at(vars(40:46,55:60), .funs = prcp_change_low) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_low) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "4")

grids_scenario_5 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_low) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_low) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "5")

grids_scenario_6 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_low) %>%
  mutate_at(vars(40:46,55:60), .funs = prcp_change_low) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "6")

grids_scenario_7 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_low) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_low) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_low) %>%
  mutate_at(vars(40:46,55:60), .funs = prcp_change_low) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_low) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_low) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "7")

grids_scenario_8 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "8")

grids_scenario_9 <- analysis_summary_grids %>% 
  mutate_at(vars(44:54,61:66), .funs = temp_change_high) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "9")

grids_scenario_10 <- analysis_summary_grids %>% 
  mutate_at(vars(40:46,55:60), .funs = prcp_change_high) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "10")

grids_scenario_11 <- analysis_summary_grids %>% 
  mutate_at(vars(40:46,55:60), .funs = prcp_change_high) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_high) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "11")

grids_scenario_12 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_high) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_high) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "12")

grids_scenario_13 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_high) %>%
  mutate_at(vars(40:46,55:60), .funs = prcp_change_high) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "13")

grids_scenario_14 <- analysis_summary_grids %>% 
  mutate_at(vars(94,96:99,110:112,126,127), .funs = pop_change_high) %>%
  mutate_at(vars(44:54,61:66), .funs = temp_change_high) %>%
  mutate_at(vars(67,69,71,73,75,77), .funs = seasontemp_change_high) %>%
  mutate_at(vars(40:46,55:60), .funs = prcp_change_high) %>%
  mutate_at(vars(68,70,72,76), .funs = seasonprcp_change_high) %>%
  mutate_at(vars(74,78), .funs = annprcp_change_high) %>%
  mutate_at(vars(11:39), .funs = remove_response) %>%
  mutate(SCENARIO = "14")


##merge into one dataset

grids_scenarios_all <- rbind(grids_scenario_0, grids_scenario_1, grids_scenario_2,
                             grids_scenario_3, grids_scenario_4, grids_scenario_5,
                             grids_scenario_6, grids_scenario_7, grids_scenario_8,
                             grids_scenario_9, grids_scenario_10, grids_scenario_11,
                             grids_scenario_12, grids_scenario_13, grids_scenario_14)


##filter by grid size as needed

grid400_scenarios <- grids_scenarios_all %>% filter(FISHNET_SIZE == 400)


#Predict responses with linear models and scenario datasets----

#ERMQ All

##use predict.lm() on existing linear model with scenario datasets

ermqallpredict <- predict.lm(ermq_all_model, huc10_scenarios, se.fit = T, interval = "confidence")


##organize predictive outptus into dataframe

ermqallpredict_out <- data.frame(RESPONSE = "ERMQ All",
                                 RESPONSE_CATEGORY = "sediment quality",
                                 SPATIAL_UNIT = "HUC 10",
                                 METHOD = "lm",
                                 SCENARIO = huc10_scenarios$SCENARIO,
                                 HABITAT = huc10_scenarios$RT_RO_Other,
                                 PREDICTED = ermqallpredict$fit[,1],
                                 LOWER = ermqallpredict$fit[,2],
                                 UPPER = ermqallpredict$fit[,3],
                                 STD_ERR = ermqallpredict$se.fit,
                                 RESIDUAL_SCALE = ermqallpredict$residual.scale,
                                 DF = ermqallpredict$df)


#ERMQ DDT

ermqddtpredict <- predict.lm(ermq_ddt_model, huc10_scenarios, se.fit = T, interval = "confidence")

ermqddtpredict_out <- data.frame(RESPONSE = "ERMQ DDT",
                                 RESPONSE_CATEGORY = "sediment quality",
                                 SPATIAL_UNIT = "HUC 10",
                                 METHOD = "lm",
                                 SCENARIO = huc10_scenarios$SCENARIO,
                                 HABITAT = huc10_scenarios$RT_RO_Other,
                                 PREDICTED = ermqddtpredict$fit[,1],
                                 LOWER = ermqddtpredict$fit[,2],
                                 UPPER = ermqddtpredict$fit[,3],
                                 STD_ERR = ermqddtpredict$se.fit,
                                 RESIDUAL_SCALE = ermqddtpredict$residual.scale,
                                 DF = ermqallpredict$df)


#ERMQ PAHs

ermqpahpredict <- predict.lm(ermq_pah_model, huc10_scenarios, se.fit = T, interval = "confidence")

ermqpahpredict_out <- data.frame(RESPONSE = "ERMQ PAH",
                                 RESPONSE_CATEGORY = "sediment quality",
                                 SPATIAL_UNIT = "HUC 10",
                                 METHOD = "lm",
                                 SCENARIO = huc10_scenarios$SCENARIO,
                                 HABITAT = huc10_scenarios$RT_RO_Other,
                                 PREDICTED = ermqpahpredict$fit[,1],
                                 LOWER = ermqpahpredict$fit[,2],
                                 UPPER = ermqpahpredict$fit[,3],
                                 STD_ERR = ermqpahpredict$se.fit,
                                 RESIDUAL_SCALE = ermqpahpredict$residual.scale,
                                 DF = ermqpahpredict$df)


#ERMQ PCBs

ermqpcbpredict <- predict.lm(ermq_pcb_model, huc10_scenarios, se.fit = T, interval = "confidence")

ermqpcbpredict_out <- data.frame(RESPONSE = "ERMQ PCB",
                                 RESPONSE_CATEGORY = "sediment quality",
                                 SPATIAL_UNIT = "HUC 10",
                                 METHOD = "lm",
                                 SCENARIO = huc10_scenarios$SCENARIO,
                                 HABITAT = huc10_scenarios$RT_RO_Other,
                                 PREDICTED = ermqpcbpredict$fit[,1],
                                 LOWER = ermqpcbpredict$fit[,2],
                                 UPPER = ermqpcbpredict$fit[,3],
                                 STD_ERR = ermqpcbpredict$se.fit,
                                 RESIDUAL_SCALE = ermqpcbpredict$residual.scale,
                                 DF = ermqpcbpredict$df)


#ERMQ Metals

ermqmetalpredict <- predict.lm(ermq_metal_model, huc10_scenarios, se.fit = T, interval = "confidence")

ermqmetalpredict_out <- data.frame(RESPONSE = "ERMQ metals",
                                   RESPONSE_CATEGORY = "sediment quality",
                                   SPATIAL_UNIT = "HUC 10",
                                   METHOD = "lm",
                                   SCENARIO = huc10_scenarios$SCENARIO,
                                   HABITAT = huc10_scenarios$RT_RO_Other,
                                   PREDICTED = ermqmetalpredict$fit[,1],
                                   LOWER = ermqmetalpredict$fit[,2],
                                   UPPER = ermqmetalpredict$fit[,3],
                                   STD_ERR = ermqmetalpredict$se.fit,
                                   RESIDUAL_SCALE = ermqmetalpredict$residual.scale,
                                   DF = ermqmetalpredict$df)


#Fecal coliform

fecalpredict <- predict.lm(fecal_model, buff2km_scenarios, se.fit = T, interval = "confidence")

fecalpredict_out <-   data.frame(RESPONSE = "Fecal",
                                 RESPONSE_CATEGORY = "water quality",
                                 SPATIAL_UNIT = "Buffer 2km",
                                 METHOD = "lm",
                                 SCENARIO = buff2km_scenarios$SCENARIO,
                                 HABITAT = buff2km_scenarios$RT_RO_Other,
                                 PREDICTED = fecalpredict$fit[,1],
                                 LOWER = fecalpredict$fit[,2],
                                 UPPER = fecalpredict$fit[,3],
                                 STD_ERR = fecalpredict$se.fit,
                                 RESIDUAL_SCALE = fecalpredict$residual.scale,
                                 DF = fecalpredict$df)


#Enterococcus

enteropredict <- predict.lm(entero_model, buff3km_scenarios, se.fit = T, interval = "confidence")

enteropredict_out <-  data.frame(RESPONSE = "entero",
                                 RESPONSE_CATEGORY = "water quality",
                                 SPATIAL_UNIT = "Buffer 3km",
                                 METHOD = "lm",
                                 SCENARIO = buff3km_scenarios$SCENARIO,
                                 HABITAT = buff3km_scenarios$RT_RO_Other,
                                 PREDICTED = enteropredict$fit[,1],
                                 LOWER = enteropredict$fit[,2],
                                 UPPER = enteropredict$fit[,3],
                                 STD_ERR = enteropredict$se.fit,
                                 RESIDUAL_SCALE = enteropredict$residual.scale,
                                 DF = enteropredict$df)


#Trawl abundance

trawlabundpredict <- predict.lm(trawlabund_model, huc12_scenarios, se.fit = T, interval = "confidence")

trawlabundpredict_out <- data.frame(RESPONSE = "trawlabund",
                                    RESPONSE_CATEGORY = "biological",
                                    SPATIAL_UNIT = "HUC 12",
                                    METHOD = "lm",
                                    SCENARIO = huc12_scenarios$SCENARIO,
                                    HABITAT = huc12_scenarios$RT_RO_Other,
                                    PREDICTED = trawlabundpredict$fit[,1],
                                    LOWER = trawlabundpredict$fit[,2],
                                    UPPER = trawlabundpredict$fit[,3],
                                    STD_ERR = trawlabundpredict$se.fit,
                                    RESIDUAL_SCALE = trawlabundpredict$residual.scale,
                                    DF = trawlabundpredict$df)


#Trawl richness (open water)

trawlsprichopenpredict <- predict.lm(trawlsprichopen_model, huc12_scenarios, se.fit = T, interval = "confidence")

trawlsprichopenpredict_out <-  data.frame(RESPONSE = "trawlsprichopen",
                                          RESPONSE_CATEGORY = "biological",
                                          SPATIAL_UNIT = "HUC 12",
                                          METHOD = "lm",
                                          SCENARIO = huc12_scenarios$SCENARIO,
                                          HABITAT = huc12_scenarios$RT_RO_Other,
                                          PREDICTED = trawlsprichopenpredict$fit[,1],
                                          LOWER = trawlsprichopenpredict$fit[,2],
                                          UPPER = trawlsprichopenpredict$fit[,3],
                                          STD_ERR = trawlsprichopenpredict$se.fit,
                                          RESIDUAL_SCALE = trawlsprichopenpredict$residual.scale,
                                          DF = trawlsprichopenpredict$df)


#Trawl richness (tidal creek)

trawlsprichtidalpredict <- predict.lm(trawlsprichtidal_model, huc12_scenarios, se.fit = T, interval = "confidence")

trawlsprichtidalpredict_out <- data.frame(RESPONSE = "trawlsprichtidal",
                                          RESPONSE_CATEGORY = "biological",
                                          SPATIAL_UNIT = "HUC 12",
                                          METHOD = "lm",
                                          SCENARIO = huc12_scenarios$SCENARIO,
                                          HABITAT = huc12_scenarios$RT_RO_Other,
                                          PREDICTED = trawlsprichtidalpredict$fit[,1],
                                          LOWER = trawlsprichtidalpredict$fit[,2],
                                          UPPER = trawlsprichtidalpredict$fit[,3],
                                          STD_ERR = trawlsprichtidalpredict$se.fit,
                                          RESIDUAL_SCALE = trawlsprichtidalpredict$residual.scale,
                                          DF = trawlsprichtidalpredict$df)


#Benthic abundance

benthabundpredict <- predict.lm(benthabund_model, huc12_scenarios, se.fit = T, interval = "confidence")

benthabundpredict_out <- data.frame(RESPONSE = "benthabund",
                                          RESPONSE_CATEGORY = "biological",
                                          SPATIAL_UNIT = "HUC 12",
                                          METHOD = "lm",
                                          SCENARIO = huc12_scenarios$SCENARIO,
                                          HABITAT = huc12_scenarios$RT_RO_Other,
                                          PREDICTED = benthabundpredict$fit[,1],
                                          LOWER = benthabundpredict$fit[,2],
                                          UPPER = benthabundpredict$fit[,3],
                                          STD_ERR = benthabundpredict$se.fit,
                                          RESIDUAL_SCALE = benthabundpredict$residual.scale,
                                          DF = benthabundpredict$df)


#Benthic richness

benthsprichpredict <- predict.lm(benthsprich_model, huc12_scenarios, se.fit = T, interval = "confidence")

benthsprichpredict_out <- data.frame(RESPONSE = "benthsprich",
                                      RESPONSE_CATEGORY = "biological",
                                      SPATIAL_UNIT = "HUC 12",
                                      METHOD = "lm",
                                      SCENARIO = huc12_scenarios$SCENARIO,
                                      HABITAT = huc12_scenarios$RT_RO_Other,
                                      PREDICTED = benthsprichpredict$fit[,1],
                                      LOWER = benthsprichpredict$fit[,2],
                                      UPPER = benthsprichpredict$fit[,3],
                                      STD_ERR = benthsprichpredict$se.fit,
                                      RESIDUAL_SCALE = benthsprichpredict$residual.scale,
                                      DF = benthsprichpredict$df)


#MAMBI

mambipredict <- predict.lm(mambi_model, grid400_scenarios, se.fit = T, interval = "confidence")

mambipredict_out <- data.frame(RESPONSE = "mambi",
                                    RESPONSE_CATEGORY = "biological",
                                    SPATIAL_UNIT = "Grid 400",
                                    METHOD = "lm",
                                    SCENARIO = grid400_scenarios$SCENARIO,
                                    HABITAT = grid400_scenarios$RT_RO_Other,
                                    PREDICTED = mambipredict$fit[,1],
                                    LOWER = mambipredict$fit[,2],
                                    UPPER = mambipredict$fit[,3],
                                    STD_ERR = mambipredict$se.fit,
                                    RESIDUAL_SCALE = mambipredict$residual.scale,
                                    DF = mambipredict$df)


#Combine all predictive outputs into one dataset

modelprediction_outputs <- rbind(ermqmetalpredict_out, ermqpahpredict_out, ermqpcbpredict_out,
                                 ermqddtpredict_out, ermqallpredict_out, fecalpredict_out,
                                 enteropredict_out, trawlabundpredict_out, trawlsprichopenpredict_out,
                                 trawlsprichtidalpredict_out, benthabundpredict_out, benthsprichpredict_out,
                                 mambipredict_out)

#Format model prediction outputs----

##create specific scenario names

modelprediction_outputs$ChangeScenario <- NULL
modelprediction_outputs$Projection <- paste("NA")


#use for loop to change scenario type

for(i in 1:nrow(modelprediction_outputs)) {
  if (modelprediction_outputs$SCENARIO[i] == 0) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Baseline") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(1,8)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Pop") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(2,9)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Temp") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(3,10)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Precip") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(4,11)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Temp + Precip") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(5,12)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Pop + Temp") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(6,13)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Pop + Precip") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(7,14)) {
    modelprediction_outputs$ChangeScenario[i] <- paste("Pop + Temp + Precip") }
 }


#use for loop to change scenario projection

for(i in 1:nrow(modelprediction_outputs)) {
  if (modelprediction_outputs$SCENARIO[i] == 0) {
    modelprediction_outputs$Projection[i] <- paste("NA") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(1:7)) {
    modelprediction_outputs$Projection[i] <- paste("Low") }
  else if (modelprediction_outputs$SCENARIO[i] %in% c(8:14)) {
    modelprediction_outputs$Projection[i] <- paste("High") }}


#paste scenario and projection type 

modelprediction_outputs$Scenario_Projection <- paste(modelprediction_outputs$ChangeScenario, 
                                                     " (", modelprediction_outputs$Projection, ")",
                                                     sep = "")


#Some dependent variables were log transformed during modeling
#create functions to 'untransform' these variables


#log10 plus one untransform

log10plus1_untransform <- function(x) {
  ((10^x) - 1)
}


#natural log plus one untransform

logEplus1_untransform <- function(x) {
  ((2.71828^x)-1)
}


#natural log untransform

logE_untransform <- function(x) {
  (2.71828^x)
}


#create new 'untransformed' columns 

modelprediction_outputs$PREDICTED_untransform <- as.numeric(NA)
modelprediction_outputs$LOWER_untransform <- as.numeric(NA)
modelprediction_outputs$UPPER_untransform <- as.numeric(NA)


#use for loop to untransform variable predictions

for(i in 1:nrow(modelprediction_outputs)) {
    if(modelprediction_outputs$RESPONSE[i] == "Fecal") {
      modelprediction_outputs$PREDICTED_untransform[i] <- log10plus1_untransform(modelprediction_outputs$PREDICTED[i])
      modelprediction_outputs$LOWER_untransform[i] <- log10plus1_untransform(modelprediction_outputs$LOWER[i])
      modelprediction_outputs$UPPER_untransform[i] <- log10plus1_untransform(modelprediction_outputs$UPPER[i])
    } else if(modelprediction_outputs$RESPONSE[i] == "entero") {
      modelprediction_outputs$PREDICTED_untransform[i] <- log10plus1_untransform(modelprediction_outputs$PREDICTED[i])
      modelprediction_outputs$LOWER_untransform[i] <- log10plus1_untransform(modelprediction_outputs$LOWER[i])
      modelprediction_outputs$UPPER_untransform[i] <- log10plus1_untransform(modelprediction_outputs$UPPER[i])
    } else if(modelprediction_outputs$RESPONSE[i] == "benthabund") {
      modelprediction_outputs$PREDICTED_untransform[i] <- logEplus1_untransform(modelprediction_outputs$PREDICTED[i])
      modelprediction_outputs$LOWER_untransform[i] <- logEplus1_untransform(modelprediction_outputs$LOWER[i])
      modelprediction_outputs$UPPER_untransform[i] <- logEplus1_untransform(modelprediction_outputs$UPPER[i])
    } else if(modelprediction_outputs$RESPONSE[i] == "trawlabund") {
      modelprediction_outputs$PREDICTED_untransform[i] <- logE_untransform(modelprediction_outputs$PREDICTED[i])
      modelprediction_outputs$LOWER_untransform[i] <- logE_untransform(modelprediction_outputs$LOWER[i])
      modelprediction_outputs$UPPER_untransform[i] <- logE_untransform(modelprediction_outputs$UPPER[i])
    } else if(modelprediction_outputs$RESPONSE[i] == "benthsprich") {
      modelprediction_outputs$PREDICTED_untransform[i] <- logE_untransform(modelprediction_outputs$PREDICTED[i])
      modelprediction_outputs$LOWER_untransform[i] <- logE_untransform(modelprediction_outputs$LOWER[i])
      modelprediction_outputs$UPPER_untransform[i] <- logE_untransform(modelprediction_outputs$UPPER[i])
    } else{
      modelprediction_outputs$PREDICTED_untransform[i] <- modelprediction_outputs$PREDICTED[i]
      modelprediction_outputs$LOWER_untransform[i] <- modelprediction_outputs$LOWER[i]
      modelprediction_outputs$UPPER_untransform[i] <- modelprediction_outputs$UPPER[i]}
  }


#calculate percent change from baseline for each variable, scenario, and habitat


##create 'baseline' dataset by filtering for baseline scenario 

modelprediction_baselines <- modelprediction_outputs %>%
                             filter(SCENARIO == 0)


##create new baseline value column

modelprediction_baselines$Baseline <- modelprediction_baselines$PREDICTED_untransform

modelprediction_baselines$Baseline_LOWER <- modelprediction_baselines$LOWER_untransform

modelprediction_baselines$Baseline_UPPER <- modelprediction_baselines$UPPER_untransform


##create join ID to join baselines data column to model prediction outputs

modelprediction_baselines$joinID <- paste(modelprediction_baselines$RESPONSE, modelprediction_baselines$HABITAT, sep = "-")

modelprediction_outputs$joinID <- paste(modelprediction_outputs$RESPONSE, modelprediction_outputs$HABITAT, sep = "-")

modelprediction_baselines <- modelprediction_baselines %>% select(joinID, Baseline, Baseline_LOWER, Baseline_UPPER)

modelprediction_outputs2 <- left_join(modelprediction_outputs, modelprediction_baselines, by = "joinID")


##create percent change function

percent_change <- function (old, new) {
  ((new - old)/abs(old))*100
}


##and use function to calculate new percent change function

modelprediction_outputs2$PercentChange <- percent_change(old = modelprediction_outputs2$Baseline,
                                                         new = modelprediction_outputs2$PREDICTED_untransform)


#clean up model prediction output dataset 

modelprediction_outputs2 <- modelprediction_outputs2 %>% select(RESPONSE, RESPONSE_CATEGORY, HABITAT, SPATIAL_UNIT,
                                                                SCENARIO, ChangeScenario, Projection,
                                                                Scenario_Projection, PREDICTED_untransform, LOWER_untransform,
                                                                UPPER_untransform, Baseline, Baseline_LOWER, Baseline_UPPER, PercentChange)

names(modelprediction_outputs2) <- c("RESPONSE","RESPONSE_CATEGORY","HABITAT","SPATIAL_UNIT","SCENARIO_NUMBER",             
                                     "SCENARIO_NAME","PROJECTION","SCENARIO_PROJECTION","PREDICTED",
                                     "PREDICTED_LOWER","PREDICTED_UPPER","BASELINE","BASELINE_LOWER",
                                     "BASELINE_UPPER", "PRCT_CHANGE")


#export model prediction output dataset as .csv

write.csv(modelprediction_outputs2, "outputs/modelpredictionoutputs.csv" )



#Graph prediction data----

##reorder factor levels of scenarios and projections

modelprediction_outputs2$SCENARIO_NAME <- factor(modelprediction_outputs2$SCENARIO_NAME, 
                                                 levels = c("Baseline","Pop","Temp","Precip",
                                                 "Temp + Precip","Pop + Temp","Pop + Precip",
                                                 "Pop + Temp + Precip"))

modelprediction_outputs2$PROJECTION <- factor(modelprediction_outputs2$PROJECTION,
                                              levels = c("NA", "Low", "High"))

modelprediction_outputs2$SCENARIO_PROJECTION <- factor(modelprediction_outputs2$SCENARIO_PROJECTION,
                                                       levels = c("Baseline (NA)",
                                                                  "Pop (Low)", "Pop (High)", 
                                                                  "Temp (Low)", "Temp (High)",               
                                                                  "Precip (Low)", "Precip (High)",
                                                                  "Temp + Precip (Low)", "Temp + Precip (High)",  
                                                                  "Pop + Temp (Low)", "Pop + Temp (High)",         
                                                                  "Pop + Precip (Low)", "Pop + Precip (High)",
                                                                  "Pop + Temp + Precip (Low)", "Pop + Temp + Precip (High)"))


benthabund_prediction_graph <- ggplot(data = benthabund_prediction_graphdata, aes(y = PREDICTED, x = CHANGE)) + 
  geom_point() +
  geom_errorbar(aes(ymin = LOWER, ymax = UPPER), width = 0.2, position = position_dodge(0)) +
  scale_shape_manual(values = c(1,16)) +
  geom_hline(yintercept = baseline_values[1,4], size = 0.3) +
  geom_hline(yintercept = baseline_values[1,5], linetype = 3, size = 0.3) +
  geom_hline(yintercept = baseline_values[1,6], linetype = 3, size = 0.3) +
  labs(x = "", y = expression(paste("Benthic Abundance Area (individuals/",m^2,")")), title = "Benthic abundance modeling predictions") +
  theme_classic(base_family = "Times New Roman") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

unique(modelprediction_outputs2$RESPONSE)
modelprediction_outputs2 %>% filter(RESPONSE == "ERMQ All", HABITAT == "AllHabitats", SCENARIO_NUMBER != 0) %>%
ggplot(data = ., aes(y = PREDICTED, x = SCENARIO_NUMBER)) + geom_point()

#ERMQ All

ermqall_predict_graphdata <-  modelprediction_outputs2 %>% 
                              filter(RESPONSE == "ERMQ All", 
                                     HABITAT == "AllHabitats", 
                                     SCENARIO_NUMBER != 0)

ermqall_predict_graph <-  ggplot(data = ermqall_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                          geom_point() + 
                          geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                          geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                          geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                          geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                          labs(x = "Climate and Population Change Scenarios", y = "ERMQ All", title = "SCECAP Modelling Predictions") +
                          theme_classic(base_family = "Times New Roman") +
                          theme(plot.title = element_text(hjust = 0.5), 
                                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#ERMQ DDT

ermqddt_predict_graphdata <- modelprediction_outputs2 %>% 
                             filter(RESPONSE == "ERMQ DDT", 
                                    HABITAT == "AllHabitats", 
                                    SCENARIO_NUMBER != 0)

ermqddt_predict_graph <- ggplot(data = ermqddt_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                          geom_point() + 
                          geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                          geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                          geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                          geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                          labs(x = "Climate and Population Change Scenarios", y = "ERMQ DDT", title = "SCECAP Modelling Predictions") +
                          theme_classic(base_family = "Times New Roman") +
                          theme(plot.title = element_text(hjust = 0.5), 
                                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

                  


#ERMQ Metals

ermqmetal_predict_graphdata <-  modelprediction_outputs2 %>% 
                                filter(RESPONSE == "ERMQ metals", 
                                       HABITAT == "AllHabitats", 
                                       SCENARIO_NUMBER != 0)

ermqmetal_predict_graph <-  ggplot(data = ermqmetal_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                            geom_point() + 
                            geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                            geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                            geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                            geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                            labs(x = "Climate and Population Change Scenarios", y = "ERMQ Metals", title = "SCECAP Modelling Predictions") +
                            theme_classic(base_family = "Times New Roman") +
                            theme(plot.title = element_text(hjust = 0.5), 
                                  axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

#ERMQ PAHs

ermqpah_predict_graphdata <-  modelprediction_outputs2 %>% 
                              filter(RESPONSE == "ERMQ PAH", 
                                     HABITAT == "AllHabitats", 
                                     SCENARIO_NUMBER != 0)

ermqpah_predict_graph <-  ggplot(data = ermqpah_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                          geom_point() + 
                          geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                          geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                          geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                          geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                          labs(x = "Climate and Population Change Scenarios", y = "ERMQ PAHs", title = "SCECAP Modelling Predictions") +
                          theme_classic(base_family = "Times New Roman") +
                          theme(plot.title = element_text(hjust = 0.5), 
                                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 

       
                        
#ERMQ PCBs

ermqpcb_predict_graphdata <-  modelprediction_outputs2 %>% 
                              filter(RESPONSE == "ERMQ PCB", 
                                     HABITAT == "AllHabitats", 
                                     SCENARIO_NUMBER != 0)

ermqpcb_predict_graph <-  ggplot(data = ermqpcb_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                          geom_point() + 
                          geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                          geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                          geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                          geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                          labs(x = "Climate and Population Change Scenarios", y = "ERMQ PCBs", title = "SCECAP Modelling Predictions") +
                          theme_classic(base_family = "Times New Roman") +
                          theme(plot.title = element_text(hjust = 0.5), 
                                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#Enterococcus

entero_predict_graphdata <-   modelprediction_outputs2 %>% 
                              filter(RESPONSE == "entero", 
                                     HABITAT == "AllHabitats", 
                                     SCENARIO_NUMBER != 0)

entero_predict_graph <-   ggplot(data = entero_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                          geom_point() + 
                          geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                          geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                          geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                          geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                          labs(x = "Climate and Population Change Scenarios", y = "Enterococcus (MPN/100mL)", title = "SCECAP Modelling Predictions") +
                          theme_classic(base_family = "Times New Roman") +
                          theme(plot.title = element_text(hjust = 0.5), 
                                axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#Fecal

fecal_predict_graphdata <-  modelprediction_outputs2 %>% 
                            filter(RESPONSE == "Fecal", 
                                   HABITAT == "AllHabitats", 
                                   SCENARIO_NUMBER != 0)

fecal_predict_graph <-  ggplot(data = fecal_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                        geom_point() + 
                        geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                        geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                        geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                        geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                        labs(x = "Climate and Population Change Scenarios", y = "Fecal coliform (MPN/100mL)", title = "SCECAP Modelling Predictions") +
                        theme_classic(base_family = "Times New Roman") +
                        theme(plot.title = element_text(hjust = 0.5), 
                              axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#Trawl abundance

trawlabund_predict_graphdata <- modelprediction_outputs2 %>% 
                                filter(RESPONSE == "trawlabund", 
                                       HABITAT == "AllHabitats", 
                                       SCENARIO_NUMBER != 0)

trawlabund_predict_graph <-   ggplot(data = trawlabund_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                              geom_point() + 
                              geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                              geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                              geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                              geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                              labs(x = "Climate and Population Change Scenarios",
                                   y = expression(paste("Trawl Abundance Area (individuals/",m^2,")")), 
                                   title = "SCECAP Modelling Predictions") +
                              theme_classic(base_family = "Times New Roman") +
                              theme(plot.title = element_text(hjust = 0.5), 
                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 
                            

#Trawl Richness (Open Water)

trawlsprichopen_predict_graphdata <-  modelprediction_outputs2 %>% 
                                      filter(RESPONSE == "trawlsprichopen", 
                                             HABITAT == "OpenWater", 
                                             SCENARIO_NUMBER != 0)

trawlsprichopen_predict_graph <-  ggplot(data = trawlsprichopen_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                                  geom_point() + 
                                  geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                                  geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                                  geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                                  geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                                  labs(x = "Climate and Population Change Scenarios", y = "Trawl Species Richness (Open Water)",
                                       title = "SCECAP Modelling Predictions") +
                                  theme_classic(base_family = "Times New Roman") +
                                  theme(plot.title = element_text(hjust = 0.5), 
                                        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#Trawl Richness (Tidal Creek)

trawlsprichtidal_predict_graphdata <-   modelprediction_outputs2 %>% 
                                        filter(RESPONSE == "trawlsprichtidal", 
                                               HABITAT == "TidalCreek", 
                                               SCENARIO_NUMBER != 0)

trawlsprichtidal_predict_graph <-   ggplot(data = trawlsprichtidal_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                                    geom_point() + 
                                    geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                                    geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                                    geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                                    geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                                    labs(x = "Climate and Population Change Scenarios", y = "Trawl Species Richness (Tidal Creek)",
                                         title = "SCECAP Modelling Predictions") +
                                    theme_classic(base_family = "Times New Roman") +
                                    theme(plot.title = element_text(hjust = 0.5), 
                                          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#benth abundance

benthabund_predict_graphdata <-   modelprediction_outputs2 %>% 
                                  filter(RESPONSE == "benthabund", 
                                         HABITAT == "AllHabitats", 
                                         SCENARIO_NUMBER != 0)

benthabund_predict_graph <-   ggplot(data = benthabund_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                              geom_point() + 
                              geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                              geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                              geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                              geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                              labs(x = "Climate and Population Change Scenarios",
                                   y = expression(paste("Benthic Abundance Area (individuals/",m^2,")")), 
                                   title = "SCECAP Modelling Predictions") +
                              theme_classic(base_family = "Times New Roman") +
                              theme(plot.title = element_text(hjust = 0.5), 
                                    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#Benthic Species Richness 

benthsprich_predict_graphdata <-   modelprediction_outputs2 %>% 
                                        filter(RESPONSE == "benthsprich", 
                                               HABITAT == "AllHabitats", 
                                               SCENARIO_NUMBER != 0)

benthsprich_predict_graph <-    ggplot(data = benthsprich_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                                geom_point() + 
                                geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                                geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                                geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                                geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                                labs(x = "Climate and Population Change Scenarios", y = "Benthic Species Richness",
                                     title = "SCECAP Modelling Predictions") +
                                theme_classic(base_family = "Times New Roman") +
                                theme(plot.title = element_text(hjust = 0.5), 
                                      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#MAMBI

mambi_predict_graphdata <-    modelprediction_outputs2 %>% 
                                    filter(RESPONSE == "mambi", 
                                           HABITAT == "AllHabitats", 
                                           SCENARIO_NUMBER != 0)

mambi_predict_graph <-    ggplot(data = mambi_predict_graphdata, aes(y = PREDICTED, x = SCENARIO_PROJECTION)) +
                                geom_point() + 
                                geom_errorbar(aes(ymin = PREDICTED_LOWER, ymax = PREDICTED_UPPER), width = 0.2) +
                                geom_hline(aes(yintercept = BASELINE), size = 0.3, linetype = 1) +
                                geom_hline(aes(yintercept = BASELINE_LOWER), size = 0.3, linetype = 3) +
                                geom_hline(aes(yintercept = BASELINE_UPPER), size = 0.3, linetype = 3) +
                                labs(x = "Climate and Population Change Scenarios", y = "MAMBI",
                                     title = "SCECAP Modelling Predictions") +
                                theme_classic(base_family = "Times New Roman") +
                                theme(plot.title = element_text(hjust = 0.5), 
                                      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


#export graphs

ggsave("./outputs/prediction_graph_ermqall.png",
       ermqall_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_ermqddt.png",
       ermqddt_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_ermqmetal.png",
       ermqmetal_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_ermqpah.png",
       ermqpah_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_ermqpcb.png",
       ermqpcb_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_trawlabund.png",
       trawlabund_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_trawlsprichopen.png",
       trawlsprichopen_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_trawlsprichtidal.png",
       trawlsprichtidal_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_benthabund.png",
       benthabund_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_benthsprich.png",
       benthsprich_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/prediction_graph_mambi.png",
       mambi_predict_graph, width = 250, height = 200, units = "mm",dpi = "retina")


