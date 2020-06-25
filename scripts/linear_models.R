#Linear Models----
##Build linear models for each dependent variable at each spatial unit and scale
##Format model results in tidy output and export as .csv

#load required packages----
library(tidyverse)
library(broom)

#import datasets----

#HUC Watersheds
analysis_hucs <- read.csv("data/scecap_hucs_for_analysis.csv")

#Buffers
analysis_buff <- read.csv("data/scecap_buffers_for_analysis.csv")

#Grids
analysis_grids <- read.csv("data/scecap_fishnetgrid_for_analysis.csv")

#HUC Watersheds----

#Enterococcus----

huc_entero_data <- analysis_hucs 

#linear models

huc08_entero_lm <- lm(data = huc_entero_data, 
                      subset = (HUC_SIZE == 08),
                      Entero_LOG ~ 
                        Logger_Salin_mean + 
                        PRCP_2 +  
                        DAYMET_30dTAVG_degC + 
                        DAYMET_14dPRCP_cm +
                        CHANNEL_WIDTH_log)

huc10_entero_lm <- lm(data = huc_entero_data, 
                      subset = (HUC_SIZE == 10),
                      Entero_LOG ~ 
                        Logger_Salin_mean + 
                        PRCP_2 +  
                        DAYMET_30dTAVG_degC + 
                        DAYMET_14dPRCP_cm +
                        IMPERV_PRCT +
                        CHANNEL_WIDTH_log)

huc12_entero_lm <- lm(data = huc_entero_data, 
                      subset = (HUC_SIZE == 12),
                      Entero_LOG ~ 
                        Logger_Salin_mean + 
                        PRCP_2 +  
                        DAYMET_30dTAVG_degC + 
                        DAYMET_14dPRCP_cm +
                        personsPERha +
                        CHANNEL_WIDTH_log)

huc14_entero_lm <- lm(data = huc_entero_data, 
                      subset = (HUC_SIZE == 14),
                      Entero_LOG ~ 
                        Logger_Salin_mean + 
                        PRCP_2 +  
                        DAYMET_30dTAVG_degC + 
                        DAYMET_14dPRCP_cm +
                        personsPERha +
                        CHANNEL_WIDTH_log)

#model outputs

huc08_entero_tidy  <-  tidy(huc08_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "08")

huc08_entero_glance<-  glance(huc08_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "08")

huc10_entero_tidy  <-  tidy(huc10_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "10")

huc10_entero_glance<-  glance(huc10_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "10")

huc12_entero_tidy  <-  tidy(huc12_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "12")

huc12_entero_glance<-  glance(huc12_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "12")

huc14_entero_tidy  <-  tidy(huc14_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "14")

huc14_entero_glance<-  glance(huc14_entero_lm) %>%
  mutate(response = "entero", spatialunit = "huc", size = "14")

#Fecal coliform----

huc_fecal_data <- analysis_hucs %>% 
                  filter(!StationCode %in% 
                          c("RT12024"))

#linear models

huc08_fecal_lm <- lm(data = huc_fecal_data, 
                      subset = (HUC_SIZE == 08),
                      Fecal_LOG ~ 
                        CHANNEL_WIDTH_log +
                        Logger_Salin_mean +
                        personsPERha +
                        PRCP_2 +
                        WELL_DRAIN_prct +
                        SP02)

huc10_fecal_lm <- lm(data = huc_fecal_data, 
                      subset = (HUC_SIZE == 10),
                      Fecal_LOG ~ 
                        CHANNEL_WIDTH_log +
                        Logger_Salin_mean +
                        IMPERV_PRCT +
                        StationDepth_m +
                        PRCP_2 +
                        SP02)

huc12_fecal_lm <- lm(data = huc_fecal_data, 
                      subset = (HUC_SIZE == 12),
                      Fecal_LOG ~ 
                        CHANNEL_WIDTH_log +
                        Logger_Salin_mean +
                        personsPERha +
                        MARCAT_prct +
                        StationDepth_m +
                        PRCP_2 +
                        SP02)
                    
huc14_fecal_lm <- lm(data = huc_fecal_data, 
                      subset = (HUC_SIZE == 14),
                      Fecal_LOG ~ 
                        CHANNEL_WIDTH_log +
                        Logger_Salin_mean +
                        personsPERha +
                        MARCAT_prct +
                        ALL_PONDS_prct +
                        personsPERha +
                        SP02)

#model outputs

huc08_fecal_tidy  <-  tidy(huc08_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "08")

huc08_fecal_glance<-  glance(huc08_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "08")

huc10_fecal_tidy  <-  tidy(huc10_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "10")

huc10_fecal_glance<-  glance(huc10_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "10")

huc12_fecal_tidy  <-  tidy(huc12_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "12")

huc12_fecal_glance<-  glance(huc12_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "12")

huc14_fecal_tidy  <-  tidy(huc14_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "14")

huc14_fecal_glance<-  glance(huc14_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "huc", size = "14")


#Metals----

huc_metal_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO00056","RO026030"))

#linear models

huc08_metal_lm <- lm(data = huc_metal_data, 
                     subset = (HUC_SIZE == 08),
                     Met_Total ~ 
                      SiltClay +
                      preSON_PRCP_diff +
                      SP02 +
                      ALL_PONDS_density +
                      IMPERV_PRCT)

huc10_metal_lm <- lm(data = huc_metal_data, 
                     subset = (HUC_SIZE == 10),
                     Met_Total ~ 
                      SiltClay +
                      SP02 +
                      preSON_PRCP_diff +
                      personsPERha +
                      POORLY_DRAINE_prct)

huc12_metal_lm <- lm(data = huc_metal_data, 
                     subset = (HUC_SIZE == 12),
                     Met_Total ~ 
                      SiltClay +
                      SP02 +
                      preSON_PRCP_diff +
                      IMPERV_PRCT)

huc14_metal_lm <- lm(data = huc_metal_data, 
                     subset = (HUC_SIZE == 14),
                     Met_Total ~ 
                      SiltClay +
                      SP02 +
                      preSON_PRCP_diff +
                      IMPERV_PRCT)

#model outputs

huc08_metal_tidy  <-  tidy(huc08_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "08")

huc08_metal_glance<-  glance(huc08_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "08")

huc10_metal_tidy  <-  tidy(huc10_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "10")

huc10_metal_glance<-  glance(huc10_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "10")

huc12_metal_tidy  <-  tidy(huc12_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "12")

huc12_metal_glance<-  glance(huc12_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "12")

huc14_metal_tidy  <-  tidy(huc14_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "14")

huc14_metal_glance<-  glance(huc14_metal_lm) %>%
  mutate(response = "metal", spatialunit = "huc", size = "14")

#PAHs----

huc_pah_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO036042", "RO056092", 
             "RO06320", "RO11308", "RT11003"))

#linear models

huc08_pah_lm <- lm(data = huc_pah_data, 
                   subset = (HUC_SIZE == 08),
                   PAH_Total ~ 
                      personsPERha +
                      SiltClay +
                      DJF_PRCP_diff +
                      MAM_TAVG_diff)

huc10_pah_lm <- lm(data = huc_pah_data, 
                   subset = (HUC_SIZE == 10),
                   PAH_Total ~ 
                      personsPERha +
                      SiltClay +
                      DJF_PRCP_diff +
                      MAM_TAVG_diff)

huc12_pah_lm <- lm(data = huc_pah_data, 
                   subset = (HUC_SIZE == 12),
                   PAH_Total ~ 
                      personsPERha +
                      SiltClay +
                      ALL_PONDS_density +
                      GROUP_A_prct +
                      DJF_PRCP_diff +
                      MAM_TAVG_diff)

huc14_pah_lm <- lm(data = huc_pah_data, 
                   subset = (HUC_SIZE == 14),
                   PAH_Total ~ 
                      personsPERha +
                      SiltClay +
                      ALL_PONDS_density +
                      GROUP_C_prct +
                      DJF_PRCP_diff+
                      MAM_TAVG_diff)

#model outputs

huc08_pah_tidy  <-  tidy(huc08_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "08")

huc08_pah_glance<-  glance(huc08_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "08")

huc10_pah_tidy  <-  tidy(huc10_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "10")

huc10_pah_glance<-  glance(huc10_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "10")

huc12_pah_tidy  <-  tidy(huc12_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "12")

huc12_pah_glance<-  glance(huc12_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "12")

huc14_pah_tidy  <-  tidy(huc14_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "14")

huc14_pah_glance<-  glance(huc14_pah_lm) %>%
  mutate(response = "pah", spatialunit = "huc", size = "14")


#PCBs----

huc_pcb_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

huc08_pcb_lm <- lm(data = huc_pcb_data, 
                   subset = (HUC_SIZE == 08),
                   PCB_Total ~ 
                    MAM_PRCP_diff +
                    SiltClay +
                    DJF_TAVG_diff +
                    personsPERha)

huc10_pcb_lm <- lm(data = huc_pcb_data, 
                   subset = (HUC_SIZE == 10),
                   PCB_Total ~ 
                    MAM_PRCP_diff +
                    SiltClay +
                    DJF_TAVG_diff +
                    personsPERha)

huc12_pcb_lm <- lm(data = huc_pcb_data, 
                   subset = (HUC_SIZE == 12),
                   PCB_Total ~ 
                    MAM_PRCP_diff +
                    SiltClay +
                    DJF_TAVG_diff +
                    personsPERha)

huc14_pcb_lm <- lm(data = huc_pcb_data, 
                   subset = (HUC_SIZE == 14),
                   PCB_Total ~ 
                    MAM_PRCP_diff +
                    SiltClay +
                    DJF_TAVG_diff +
                    personsPERha)

#model outputs

huc08_pcb_tidy  <-  tidy(huc08_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "08")

huc08_pcb_glance<-  glance(huc08_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "08")

huc10_pcb_tidy  <-  tidy(huc10_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "10")

huc10_pcb_glance<-  glance(huc10_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "10")

huc12_pcb_tidy  <-  tidy(huc12_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "12")

huc12_pcb_glance<-  glance(huc12_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "12")

huc14_pcb_tidy  <-  tidy(huc14_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "14")

huc14_pcb_glance<-  glance(huc14_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "huc", size = "14")

#DDTs----

huc_ddt_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

huc08_ddt_lm <- lm(data = huc_ddt_data, 
                   subset = (HUC_SIZE == 08),
                   DDT_Total ~ 
                    SiltClay +
                    DJF_PRCP_diff +
                    AGRCAT_prct +
                    MAM_TAVG_diff +
                    POORLY_DRAINE_prct +
                    personsPERha +
                    Logger_Salin_mean)

huc10_ddt_lm <- lm(data = huc_ddt_data, 
                   subset = (HUC_SIZE == 10),
                   DDT_Total ~ 
                    TOC_sediment +
                    DJF_PRCP_diff +
                    AGRCAT_prct +
                    Logger_Salin_mean +
                    personsPERha +
                    MAM_TAVG_diff)

huc12_ddt_lm <- lm(data = huc_ddt_data, 
                   subset = (HUC_SIZE == 12),
                   DDT_Total ~ 
                    SiltClay +
                    DJF_PRCP_diff +
                    AGRCAT_prct +
                    Logger_Salin_mean +
                    personsPERha +
                    MAM_TAVG_diff +
                    MARCAT_prct)

huc14_ddt_lm <- lm(data = huc_ddt_data, 
                   subset = (HUC_SIZE == 14),
                   DDT_Total ~ 
                    SiltClay +
                    DJF_PRCP_diff +
                    AGRCAT_prct +
                    Logger_Salin_mean +
                    personsPERha +
                    MAM_TAVG_diff +
                    MARCAT_prct)

#model outputs

huc08_ddt_tidy  <-  tidy(huc08_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "08")

huc08_ddt_glance<-  glance(huc08_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "08")

huc10_ddt_tidy  <-  tidy(huc10_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "10")

huc10_ddt_glance<-  glance(huc10_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "10")

huc12_ddt_tidy  <-  tidy(huc12_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "12")

huc12_ddt_glance<-  glance(huc12_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "12")

huc14_ddt_tidy  <-  tidy(huc14_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "14")

huc14_ddt_glance<-  glance(huc14_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "huc", size = "14")


#PBDEs----

huc_pbde_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO10379", "RO10380", "RO10392", 
             "RT10115", "RT10129", "RT10137"))

#linear models

huc08_pbde_lm <- lm(data = huc_pbde_data, 
                   subset = (HUC_SIZE == 08),
                   PBDE_Total ~ 
                    DJF_PRCP_diff +
                    DJF_TAVG_diff +
                    AMO +
                    IMPERV_PRCT)

huc10_pbde_lm <- lm(data = huc_pbde_data, 
                   subset = (HUC_SIZE == 10),
                   PBDE_Total ~ 
                    DJF_PRCP_diff +
                    DJF_TAVG_diff +
                    AMO +
                    IMPERV_PRCT)

huc12_pbde_lm <- lm(data = huc_pbde_data, 
                   subset = (HUC_SIZE == 12),
                   PBDE_Total ~ 
                    DJF_PRCP_diff +
                    DJF_TAVG_diff +
                    AMO)

huc14_pbde_lm <- lm(data = huc_pbde_data, 
                   subset = (HUC_SIZE == 14),
                   PBDE_Total ~ 
                    DJF_PRCP_diff +
                    DJF_TAVG_diff +
                    AMO)

#model outputs

huc08_pbde_tidy  <-  tidy(huc08_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "08")

huc08_pbde_glance<-  glance(huc08_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "08")

huc10_pbde_tidy  <-  tidy(huc10_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "10")

huc10_pbde_glance<-  glance(huc10_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "10")

huc12_pbde_tidy  <-  tidy(huc12_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "12")

huc12_pbde_glance<-  glance(huc12_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "12")

huc14_pbde_tidy  <-  tidy(huc14_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "14")

huc14_pbde_glance<-  glance(huc14_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "huc", size = "14")



#ERMQ DDT----

huc_ermqddt_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

huc08_ermqddt_lm <- lm(data = huc_ermqddt_data, 
                    subset = (HUC_SIZE == 08),
                    ERMQ_DDT_tot ~ 
                      TOC_sediment +
                      DJF_PRCP_diff +
                      personsPERha +
                      AGRCAT_prct +
                      JJA_TAVG_diff +
                      Logger_Salin_mean)

huc10_ermqddt_lm <- lm(data = huc_ermqddt_data, 
                    subset = (HUC_SIZE == 10),
                    ERMQ_DDT_tot ~ 
                      TOC_sediment +
                      DJF_PRCP_diff +
                      WATCAT_prct +
                      AGRCAT_prct +
                      personsPERha +
                      JJA_TAVG_diff)

huc12_ermqddt_lm <- lm(data = huc_ermqddt_data, 
                    subset = (HUC_SIZE == 12),
                    ERMQ_DDT_tot ~ 
                      TOC_sediment +
                      DJF_PRCP_diff +
                      AGRCAT_prct +
                      personsPERha +
                      JJA_TAVG_diff +
                      MARCAT_prct +
                      Logger_Salin_mean)

huc14_ermqddt_lm <- lm(data = huc_ermqddt_data, 
                    subset = (HUC_SIZE == 14),
                    ERMQ_DDT_tot ~ 
                      SiltClay +
                      DJF_PRCP_diff +
                      AGRCAT_prct +
                      personsPERha +
                      JJA_TAVG_diff +
                      MARCAT_prct +
                      Logger_Salin_mean)

#model outputs

huc08_ermqddt_tidy  <-  tidy(huc08_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "08")

huc08_ermqddt_glance<-  glance(huc08_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "08")

huc10_ermqddt_tidy  <-  tidy(huc10_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "10")

huc10_ermqddt_glance<-  glance(huc10_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "10")

huc12_ermqddt_tidy  <-  tidy(huc12_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "12")

huc12_ermqddt_glance<-  glance(huc12_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "12")

huc14_ermqddt_tidy  <-  tidy(huc14_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "14")

huc14_ermqddt_glance<-  glance(huc14_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "huc", size = "14")



#ERMQ Metals----

huc_ermqmet_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RT01628", "RO026030"))

#linear models

huc08_ermqmet_lm <- lm(data = huc_ermqmet_data, 
                       subset = (HUC_SIZE == 08),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         GROUP_B_prct +
                         IMPERV_PRCT)

huc10_ermqmet_lm <- lm(data = huc_ermqmet_data, 
                       subset = (HUC_SIZE == 10),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         POORLY_DRAINE_prct +
                         personsPERha)

huc12_ermqmet_lm <- lm(data = huc_ermqmet_data, 
                       subset = (HUC_SIZE == 12),
                       ERMQ_met ~ 
                         SiltClay +
                         IMPERV_PRCT +
                         preSON_PRCP_diff +
                         SP02 +
                         MIXED_FOREST_prct +
                         ALL_PONDS_density)

huc14_ermqmet_lm <- lm(data = huc_ermqmet_data, 
                       subset = (HUC_SIZE == 14),
                       ERMQ_met ~ 
                         SiltClay +
                         IMPERV_PRCT +
                         preSON_PRCP_diff +
                         SP02 +
                         MIXED_FOREST_prct +
                         ALL_PONDS_density)

#model outputs

huc08_ermqmet_tidy  <-  tidy(huc08_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "08")

huc08_ermqmet_glance<-  glance(huc08_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "08")

huc10_ermqmet_tidy  <-  tidy(huc10_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "10")

huc10_ermqmet_glance<-  glance(huc10_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "10")

huc12_ermqmet_tidy  <-  tidy(huc12_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "12")

huc12_ermqmet_glance<-  glance(huc12_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "12")

huc14_ermqmet_tidy  <-  tidy(huc14_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "14")

huc14_ermqmet_glance<-  glance(huc14_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "huc", size = "14")


#ERMQ PAHs----

huc_ermqpah_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO056092", "RO06320", 
             "RO11308", "RT11003"))

#linear models

huc08_ermqpah_lm <- lm(data = huc_ermqpah_data, 
                       subset = (HUC_SIZE == 08),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         personsPERha +
                         CHANNEL_WIDTH_log +
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         POORLY_DRAINE_prct)

huc10_ermqpah_lm <- lm(data = huc_ermqpah_data, 
                       subset = (HUC_SIZE == 10),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         personsPERha +
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         MIXED_FOREST_prct +
                         CHANNEL_WIDTH_log +
                         ALL_PONDS_density)

huc12_ermqpah_lm <- lm(data = huc_ermqpah_data, 
                       subset = (HUC_SIZE == 12),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         DEVELOPED_HIGH_INTENSITY_prct +
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         CHANNEL_WIDTH_log +
                         POORLY_DRAINE_prct)

huc14_ermqpah_lm <- lm(data = huc_ermqpah_data, 
                       subset = (HUC_SIZE == 14),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         DEVELOPED_HIGH_INTENSITY_prct +
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         CHANNEL_WIDTH_log +
                         GROUP_C_prct)

#model outputs

huc08_ermqpah_tidy  <-  tidy(huc08_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "08")

huc08_ermqpah_glance<-  glance(huc08_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "08")

huc10_ermqpah_tidy  <-  tidy(huc10_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "10")

huc10_ermqpah_glance<-  glance(huc10_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "10")

huc12_ermqpah_tidy  <-  tidy(huc12_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "12")

huc12_ermqpah_glance<-  glance(huc12_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "12")

huc14_ermqpah_tidy  <-  tidy(huc14_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "14")

huc14_ermqpah_glance<-  glance(huc14_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "huc", size = "14")


#ERMQ PCBs----

huc_ermqpcb_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

huc08_ermqpcb_lm <- lm(data = huc_ermqpcb_data, 
                       subset = (HUC_SIZE == 08),
                       ERMQ_PCB_tot ~ 
                       SiltClay +
                       MAM_PRCP_diff +
                       DJF_TAVG_diff +
                       FORCAT_prct +
                       personsPERha +
                       POORLY_DRAINE_prct)

huc10_ermqpcb_lm <- lm(data = huc_ermqpcb_data, 
                       subset = (HUC_SIZE == 10),
                       ERMQ_PCB_tot ~ 
                       MAM_PRCP_diff +
                       SiltClay +
                       DJF_TAVG_diff +
                       personsPERha)

huc12_ermqpcb_lm <- lm(data = huc_ermqpcb_data, 
                       subset = (HUC_SIZE == 12),
                       ERMQ_PCB_tot ~ 
                       SiltClay +
                       MAM_PRCP_diff +
                       DJF_TAVG_diff +
                       IMPERV_PRCT +
                       Logger_Salin_mean)

huc14_ermqpcb_lm <- lm(data = huc_ermqpcb_data, 
                       subset = (HUC_SIZE == 14),
                       ERMQ_PCB_tot ~ 
                       MAM_PRCP_diff +
                       SiltClay +
                       DJF_TAVG_diff +
                       DEVELOPED_HIGH_INTENSITY_prct +
                       MARCAT_prct)

#model outputs

huc08_ermqpcb_tidy  <-  tidy(huc08_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "08")

huc08_ermqpcb_glance<-  glance(huc08_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "08")

huc10_ermqpcb_tidy  <-  tidy(huc10_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "10")

huc10_ermqpcb_glance<-  glance(huc10_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "10")

huc12_ermqpcb_tidy  <-  tidy(huc12_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "12")

huc12_ermqpcb_glance<-  glance(huc12_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "12")

huc14_ermqpcb_tidy  <-  tidy(huc14_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "14")

huc14_ermqpcb_glance<-  glance(huc14_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "huc", size = "14")


#ERMQ ALL----

huc_ermqall_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO06320", "RO11308"))

#linear models

huc08_ermqall_lm <- lm(data = huc_ermqall_data, 
                       subset = (HUC_SIZE == 08),
                       ERMQ_ALL ~ 
                         SiltClay +
                         personsPERha +
                         preSON_PRCP_diff +
                         MIXED_FOREST_prct +
                         DJF_PRCP_diff)

huc10_ermqall_lm <- lm(data = huc_ermqall_data, 
                       subset = (HUC_SIZE == 10),
                       ERMQ_ALL ~ 
                         SiltClay +
                         personsPERha +
                         preSON_PRCP_diff +
                         GROUP_A_prct +
                         DJF_PRCP_diff)

huc12_ermqall_lm <- lm(data = huc_ermqall_data, 
                       subset = (HUC_SIZE == 12),
                       ERMQ_ALL ~ 
                         SiltClay +
                         IMPERV_PRCT +
                         preSON_PRCP_diff +
                         GROUP_C_prct +
                         DJF_PRCP_diff)

huc14_ermqall_lm <- lm(data = huc_ermqall_data, 
                       subset = (HUC_SIZE == 14),
                       ERMQ_ALL ~ 
                         SiltClay +
                         IMPERV_PRCT +
                         preSON_PRCP_diff +
                         GROUP_C_prct +
                         DJF_PRCP_diff)

#model outputs

huc08_ermqall_tidy  <-  tidy(huc08_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "08")

huc08_ermqall_glance<-  glance(huc08_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "08")

huc10_ermqall_tidy  <-  tidy(huc10_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "10")

huc10_ermqall_glance<-  glance(huc10_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "10")

huc12_ermqall_tidy  <-  tidy(huc12_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "12")

huc12_ermqall_glance<-  glance(huc12_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "12")

huc14_ermqall_tidy  <-  tidy(huc14_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "14")

huc14_ermqall_glance<-  glance(huc14_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "huc", size = "14")



#Trawl Abundance Area----

huc_trawlabund_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RT022153", "RT06010", "RT06037"))

#linear models

huc08_trawlabund_lm <- lm(data = huc_trawlabund_data, 
                       subset = (HUC_SIZE == 08),
                       trawl_ABUNDANCE_area ~ 
                         CHANNEL_WIDTH_log +
                         IMPERV_PRCT +
                         ANN_TAVG_diff +
                         NAO)

huc10_trawlabund_lm <- lm(data = huc_trawlabund_data, 
                       subset = (HUC_SIZE == 10),
                       trawl_ABUNDANCE_area ~ 
                         CHANNEL_WIDTH_log +
                         AGRCAT_prct +
                         IMPERV_PRCT +
                         NAO +
                         ANN_TAVG_diff)

huc12_trawlabund_lm <- lm(data = huc_trawlabund_data, 
                       subset = (HUC_SIZE == 12),
                       trawl_ABUNDANCE_area ~ 
                         CHANNEL_WIDTH_log +
                         IMPERV_PRCT +
                         GROUP_B_prct +
                         ANN_TAVG_diff +
                         NAO)

huc14_trawlabund_lm <- lm(data = huc_trawlabund_data, 
                       subset = (HUC_SIZE == 14),
                       trawl_ABUNDANCE_area ~ 
                         CHANNEL_WIDTH_log +
                         IMPERV_PRCT +
                         AGRCAT_prct +
                         ANN_TAVG_diff +
                         NAO)

#model outputs

huc08_trawlabund_tidy  <-  tidy(huc08_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "08")

huc08_trawlabund_glance<-  glance(huc08_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "08")

huc10_trawlabund_tidy  <-  tidy(huc10_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "10")

huc10_trawlabund_glance<-  glance(huc10_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "10")

huc12_trawlabund_tidy  <-  tidy(huc12_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "12")

huc12_trawlabund_glance<-  glance(huc12_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "12")

huc14_trawlabund_tidy  <-  tidy(huc14_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "14")

huc14_trawlabund_glance<-  glance(huc14_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "14")


#Trawl Species Richness (openwater)----

huc_trawlsprichopen_data <- analysis_hucs %>% 
  filter(RT_RO_Other == "OpenWater")

#linear models

huc08_trawlsprichopen_lm <- lm(data = huc_trawlsprichopen_data, 
                          subset = (HUC_SIZE == 08),
                          trawl_SP_RICH ~ 
                          CHANNEL_WIDTH_log +
                          Logger_Salin_mean +
                          NAO +
                          FORCAT_prct +
                          DAYMET_30dPRCP_cm +
                          preANN_TAVG_diff)

huc10_trawlsprichopen_lm <- lm(data = huc_trawlsprichopen_data, 
                          subset = (HUC_SIZE == 10),
                          trawl_SP_RICH  ~ 
                          CHANNEL_WIDTH_log +
                          Logger_Salin_mean +
                          NAO +
                          AGRCAT_prct +
                          DAYMET_30dPRCP_cm +
                          preANN_TAVG_diff)

huc12_trawlsprichopen_lm <- lm(data = huc_trawlsprichopen_data, 
                          subset = (HUC_SIZE == 12),
                          trawl_SP_RICH  ~ 
                          CHANNEL_WIDTH_log +
                          Logger_Salin_mean +
                          NAO +
                          AGRCAT_prct +
                          MARCAT_prct +
                          DAYMET_30dPRCP_cm +
                          preANN_TAVG_diff)

huc14_trawlsprichopen_lm <- lm(data = huc_trawlsprichopen_data, 
                          subset = (HUC_SIZE == 14),
                          trawl_SP_RICH ~ 
                          CHANNEL_WIDTH_log +
                          Logger_Salin_mean +
                          MARCAT_prct +
                          NAO +
                          GROUP_D_prct +
                          preANN_TAVG_diff +
                          DAYMET_30dPRCP_cm)

#model outputs

huc08_trawlsprichopen_tidy  <-  tidy(huc08_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "08")

huc08_trawlsprichopen_glance<-  glance(huc08_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "08")

huc10_trawlsprichopen_tidy  <-  tidy(huc10_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "10")

huc10_trawlsprichopen_glance<-  glance(huc10_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "10")

huc12_trawlsprichopen_tidy  <-  tidy(huc12_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "12")

huc12_trawlsprichopen_glance<-  glance(huc12_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "12")

huc14_trawlsprichopen_tidy  <-  tidy(huc14_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "14")

huc14_trawlsprichopen_glance<-  glance(huc14_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "14")


#Trawl Species Richness (tidal creeks)----

huc_trawlsprichtidal_data <- analysis_hucs %>% 
  filter(RT_RO_Other == "TidalCreek")

#linear models

huc08_trawlsprichtidal_lm <- lm(data = huc_trawlsprichtidal_data, 
                               subset = (HUC_SIZE == 08),
                               trawl_SP_RICH ~ 
                               Logger_Salin_mean +
                               IMPERV_PRCT +
                               ANN_TAVG_diff +
                               SP02 +
                               MARCAT_prct +
                               CHANNEL_WIDTH_log)

huc10_trawlsprichtidal_lm <- lm(data = huc_trawlsprichtidal_data, 
                               subset = (HUC_SIZE == 10),
                               trawl_SP_RICH  ~ 
                               Logger_Salin_mean +
                               IMPERV_PRCT +
                               ANN_TAVG_diff +
                               SP02 +
                               GROUP_B_prct +
                               CHANNEL_WIDTH_log)

huc12_trawlsprichtidal_lm <- lm(data = huc_trawlsprichtidal_data, 
                               subset = (HUC_SIZE == 12),
                               trawl_SP_RICH  ~ 
                               Logger_Salin_mean +
                               AGRCAT_prct +
                               ANN_TAVG_diff +
                               SP02 +
                               GROUP_B_prct +
                               CHANNEL_WIDTH_log)

huc14_trawlsprichtidal_lm <- lm(data = huc_trawlsprichtidal_data, 
                               subset = (HUC_SIZE == 14),
                               trawl_SP_RICH ~ 
                               Logger_Salin_mean +
                               AGRCAT_prct +
                               ANN_TAVG_diff +
                               SP02 +
                               GROUP_B_prct +
                               CHANNEL_WIDTH_log)

#model outputs

huc08_trawlsprichtidal_tidy  <-  tidy(huc08_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "08")

huc08_trawlsprichtidal_glance<-  glance(huc08_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "08")

huc10_trawlsprichtidal_tidy  <-  tidy(huc10_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "10")

huc10_trawlsprichtidal_glance<-  glance(huc10_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "10")

huc12_trawlsprichtidal_tidy  <-  tidy(huc12_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "12")

huc12_trawlsprichtidal_glance<-  glance(huc12_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "12")

huc14_trawlsprichtidal_tidy  <-  tidy(huc14_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "14")

huc14_trawlsprichtidal_glance<-  glance(huc14_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "14")


#Benthic Abundance Area----

huc_benthabund_data <- analysis_hucs %>% 
  filter(!StationCode %in% 
           c("RO00059", "RT032180", "RO06305"))

#linear models

huc08_benthabund_lm <- lm(data = huc_benthabund_data, 
                          subset = (HUC_SIZE == 08),
                          benth_ABUNDANCE_AREA ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm +
                            IMPERV_PRCT)

huc10_benthabund_lm <- lm(data = huc_benthabund_data, 
                          subset = (HUC_SIZE == 10),
                          benth_ABUNDANCE_AREA ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm)

huc12_benthabund_lm <- lm(data = huc_benthabund_data, 
                          subset = (HUC_SIZE == 12),
                          benth_ABUNDANCE_AREA ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm +
                            MARCAT_prct)

huc14_benthabund_lm <- lm(data = huc_benthabund_data, 
                          subset = (HUC_SIZE == 14),
                          benth_ABUNDANCE_AREA ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm +
                            MARCAT_prct)

#model outputs

huc08_benthabund_tidy  <-  tidy(huc08_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "08")

huc08_benthabund_glance<-  glance(huc08_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "08")

huc10_benthabund_tidy  <-  tidy(huc10_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "10")

huc10_benthabund_glance<-  glance(huc10_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "10")

huc12_benthabund_tidy  <-  tidy(huc12_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "12")

huc12_benthabund_glance<-  glance(huc12_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "12")

huc14_benthabund_tidy  <-  tidy(huc14_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "14")

huc14_benthabund_glance<-  glance(huc14_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "14")


#Benthic Species Richness----

huc_benthsprich_data <- analysis_hucs

#linear models

huc08_benthsprich_lm <- lm(data = huc_benthsprich_data, 
                          subset = (HUC_SIZE == 08),
                          benth_SP_RICH ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            DAYMET_45dPRCP_cm +
                            SiltClay +
                            CHANNEL_WIDTH_log +
                            MARCAT_prct)

huc10_benthsprich_lm <- lm(data = huc_benthsprich_data, 
                          subset = (HUC_SIZE == 10),
                          benth_SP_RICH  ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            DAYMET_45dPRCP_cm +
                            SiltClay +
                            CHANNEL_WIDTH_log +
                            DEVLOWCAT_prct +
                            IMPERV_PRCT)

huc12_benthsprich_lm <- lm(data = huc_benthsprich_data, 
                          subset = (HUC_SIZE == 12),
                          benth_SP_RICH ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            DAYMET_45dPRCP_cm +
                            SiltClay +
                            CHANNEL_WIDTH_log +
                            DEVLOWCAT_prct +
                            MARCAT_prct)

huc14_benthsprich_lm <- lm(data = huc_benthsprich_data, 
                          subset = (HUC_SIZE == 14),
                          benth_SP_RICH ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            DAYMET_45dPRCP_cm +
                            SiltClay +
                            CHANNEL_WIDTH_log +
                            DEVLOWCAT_prct +
                            MARCAT_prct)

#model outputs

huc08_benthsprich_tidy  <-  tidy(huc08_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "08")

huc08_benthsprich_glance<-  glance(huc08_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "08")

huc10_benthsprich_tidy  <-  tidy(huc10_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "10")

huc10_benthsprich_glance<-  glance(huc10_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "10")

huc12_benthsprich_tidy  <-  tidy(huc12_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "12")

huc12_benthsprich_glance<-  glance(huc12_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "12")

huc14_benthsprich_tidy  <-  tidy(huc14_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "14")

huc14_benthsprich_glance<-  glance(huc14_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "14")


#BIBI----

huc_bibi_data <- analysis_hucs

#linear models

huc08_bibi_lm <- lm(data = huc_bibi_data, 
                    subset = (HUC_SIZE == 08),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      MARCAT_prct +
                      DAYMET_45dPRCP_cm)

huc10_bibi_lm <- lm(data = huc_bibi_data, 
                    subset = (HUC_SIZE == 10),
                    BIBI  ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      MARCAT_prct +
                      DAYMET_45dPRCP_cm)

huc12_bibi_lm <- lm(data = huc_bibi_data, 
                    subset = (HUC_SIZE == 12),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      ALL_PONDS_prct +
                      personsPERha +
                      DAYMET_45dPRCP_cm)

huc14_bibi_lm <- lm(data = huc_bibi_data, 
                    subset = (HUC_SIZE == 14),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      DEVLOWCAT_prct +
                      personsPERha +
                      MARCAT_prct +
                      DAYMET_45dPRCP_cm)

#model outputs

huc08_bibi_tidy  <-  tidy(huc08_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "08")

huc08_bibi_glance<-  glance(huc08_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "08")

huc10_bibi_tidy  <-  tidy(huc10_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "10")

huc10_bibi_glance<-  glance(huc10_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "10")

huc12_bibi_tidy  <-  tidy(huc12_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "12")

huc12_bibi_glance<-  glance(huc12_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "12")

huc14_bibi_tidy  <-  tidy(huc14_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "14")

huc14_bibi_glance<-  glance(huc14_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "huc", size = "14")


#MAMBI----

huc_mambi_data <- analysis_hucs

#linear models

huc08_mambi_lm <- lm(data = huc_mambi_data, 
                    subset = (HUC_SIZE == 08),
                    MAMBI ~ 
                      Logger_Salin_mean +
                      CHANNEL_WIDTH_log +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      DAYMET_45dPRCP_cm)

huc10_mambi_lm <- lm(data = huc_mambi_data, 
                    subset = (HUC_SIZE == 10),
                    MAMBI ~ 
                      Logger_Salin_mean +
                      CHANNEL_WIDTH_log +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      DAYMET_45dPRCP_cm +
                      DEVHIGHCAT_prct +
                      DEVLOWCAT_prct)

huc12_mambi_lm <- lm(data = huc_mambi_data, 
                    subset = (HUC_SIZE == 12),
                    MAMBI ~ 
                      Logger_Salin_mean +
                      CHANNEL_WIDTH_log +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      DAYMET_45dPRCP_cm +
                      personsPERha +
                      DEVLOWCAT_prct)

huc14_mambi_lm <- lm(data = huc_mambi_data, 
                    subset = (HUC_SIZE == 14),
                    MAMBI ~ 
                      Logger_Salin_mean +
                      CHANNEL_WIDTH_log +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      DAYMET_45dPRCP_cm +
                      personsPERha +
                      DEVLOWCAT_prct)

#model outputs

huc08_mambi_tidy  <-  tidy(huc08_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "08")

huc08_mambi_glance<-  glance(huc08_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "08")

huc10_mambi_tidy  <-  tidy(huc10_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "10")

huc10_mambi_glance<-  glance(huc10_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "10")

huc12_mambi_tidy  <-  tidy(huc12_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "12")

huc12_mambi_glance<-  glance(huc12_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "12")

huc14_mambi_tidy  <-  tidy(huc14_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "14")

huc14_mambi_glance<-  glance(huc14_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "huc", size = "14")




#FISHNET GRIDS----

#Enterococcus----

grid_entero_data <- analysis_grids 

#linear models

fish25_entero_lm <- lm(data = grid_entero_data, 
                      subset = (FISHNET_SIZE == 25),
                      Entero_LOG ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        PRCP_2 +
                        DAYMET_30dTAVG_degC +
                        IMPERV_PRCT +
                        DAYMET_14dPRCP_cm)

fish100_entero_lm <- lm(data = grid_entero_data, 
                      subset = (FISHNET_SIZE == 100),
                      Entero_LOG ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        PRCP_2 +
                        DAYMET_30dTAVG_degC +
                        IMPERV_PRCT +
                        DAYMET_14dPRCP_cm)

fish400_entero_lm <- lm(data = grid_entero_data, 
                      subset = (FISHNET_SIZE == 400),
                      Entero_LOG ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        PRCP_2 +
                        DAYMET_30dTAVG_degC +
                        DAYMET_14dPRCP_cm)

fish1600_entero_lm <- lm(data = grid_entero_data, 
                      subset = (FISHNET_SIZE == 1600),
                      Entero_LOG ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        PRCP_2 +
                        DAYMET_30dTAVG_degC +
                        DAYMET_14dPRCP_cm)

#model outputs

fish25_entero_tidy  <-  tidy(fish25_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "25")

fish25_entero_glance<-  glance(fish25_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "25")

fish100_entero_tidy  <-  tidy(fish100_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "100")

fish100_entero_glance<-  glance(fish100_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "100")

fish400_entero_tidy  <-  tidy(fish400_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "400")

fish400_entero_glance<-  glance(fish400_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "400")

fish1600_entero_tidy  <-  tidy(fish1600_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "1600")

fish1600_entero_glance<-  glance(fish1600_entero_lm) %>%
  mutate(response = "entero", spatialunit = "grid", size = "1600")

#Fecal coliform----

grid_fecal_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO07340","R013346","RO11301",
             "RO09368"))

#linear models

fish25_fecal_lm <- lm(data = grid_fecal_data, 
                     subset = (FISHNET_SIZE == 25),
                     Fecal_LOG ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       IMPERV_PRCT +
                       PRCP_2 +
                       SP02 +
                       MARCAT_prct +
                       FORCAT_prct)

fish100_fecal_lm <- lm(data = grid_fecal_data, 
                     subset = (FISHNET_SIZE == 100),
                     Fecal_LOG ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       IMPERV_PRCT +
                       PRCP_2 +
                       SP02 +
                       MARCAT_prct +
                       FORCAT_prct)

fish400_fecal_lm <- lm(data = grid_fecal_data, 
                     subset = (FISHNET_SIZE == 400),
                     Fecal_LOG ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       IMPERV_PRCT +
                       PRCP_2 +
                       SP02 +
                       FORCAT_prct)


fish1600_fecal_lm <- lm(data = grid_fecal_data, 
                     subset = (FISHNET_SIZE == 1600),
                     Fecal_LOG ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       IMPERV_PRCT +
                       PRCP_2 +
                       SP02)

#model outputs

fish25_fecal_tidy  <-  tidy(fish25_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "25")

fish25_fecal_glance<-  glance(fish25_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "25")

fish100_fecal_tidy  <-  tidy(fish100_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "100")

fish100_fecal_glance<-  glance(fish100_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "100")

fish400_fecal_tidy  <-  tidy(fish400_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "400")

fish400_fecal_glance<-  glance(fish400_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "400")

fish1600_fecal_tidy  <-  tidy(fish1600_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "1600")

fish1600_fecal_glance<-  glance(fish1600_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "grid", size = "1600")


#Metals----

grid_metal_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO00056","RO026030"))

#linear models

fish25_metal_lm <- lm(data = grid_metal_data, 
                     subset = (FISHNET_SIZE == 25),
                     Met_Total ~ 
                       preSON_PRCP_diff +
                       SP02 +
                       IMPERV_PRCT)

fish100_metal_lm <- lm(data = grid_metal_data, 
                     subset = (FISHNET_SIZE == 100),
                     Met_Total ~ 
                       preSON_PRCP_diff +
                       SP02 +
                       IMPERV_PRCT)

fish400_metal_lm <- lm(data = grid_metal_data, 
                     subset = (FISHNET_SIZE == 400),
                     Met_Total ~ 
                       preSON_PRCP_diff +
                       SP02 +
                       IMPERV_PRCT)

fish1600_metal_lm <- lm(data = grid_metal_data, 
                     subset = (FISHNET_SIZE == 1600),
                     Met_Total ~ 
                       preSON_PRCP_diff +
                       SP02 +
                       IMPERV_PRCT +
                       GROUP_A_prct)

#model outputs

fish25_metal_tidy  <-  tidy(fish25_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "25")

fish25_metal_glance<-  glance(fish25_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "25")

fish100_metal_tidy  <-  tidy(fish100_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "100")

fish100_metal_glance<-  glance(fish100_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "100")

fish400_metal_tidy  <-  tidy(fish400_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "400")

fish400_metal_glance<-  glance(fish400_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "400")

fish1600_metal_tidy  <-  tidy(fish1600_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "1600")

fish1600_metal_glance<-  glance(fish1600_metal_lm) %>%
  mutate(response = "metal", spatialunit = "grid", size = "1600")

#PAHs----

grid_pah_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO036042", "RO056092", 
             "RO06320", "RO11308", "RT11003"))

#linear models

fish25_pah_lm <- lm(data = grid_pah_data, 
                   subset = (FISHNET_SIZE == 25),
                   PAH_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     MAM_TAVG_diff +
                     IMPERV_PRCT +
                     MARCAT_prct)

fish100_pah_lm <- lm(data = grid_pah_data, 
                   subset = (FISHNET_SIZE == 100),
                   PAH_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     census2010_personsPERha +
                     MAM_TAVG_diff)

fish400_pah_lm <- lm(data = grid_pah_data, 
                   subset = (FISHNET_SIZE == 400),
                   PAH_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     census2010_personsPERha +
                     MAM_TAVG_diff)

fish1600_pah_lm <- lm(data = grid_pah_data, 
                   subset = (FISHNET_SIZE == 1600),
                   PAH_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     census2010_personsPERha +
                     WATCAT_prct +
                     MAM_TAVG_diff)

#model outputs

fish25_pah_tidy  <-  tidy(fish25_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "25")

fish25_pah_glance<-  glance(fish25_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "25")

fish100_pah_tidy  <-  tidy(fish100_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "100")

fish100_pah_glance<-  glance(fish100_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "100")

fish400_pah_tidy  <-  tidy(fish400_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "400")

fish400_pah_glance<-  glance(fish400_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "400")

fish1600_pah_tidy  <-  tidy(fish1600_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "1600")

fish1600_pah_glance<-  glance(fish1600_pah_lm) %>%
  mutate(response = "pah", spatialunit = "grid", size = "1600")


#PCBs----

grid_pcb_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

fish25_pcb_lm <- lm(data = grid_pcb_data, 
                   subset = (FISHNET_SIZE == 25),
                   PCB_Total ~ 
                     MAM_PRCP_diff +
                     SiltClay +
                     DJF_TAVG_diff +
                     census2010_personsPERha)

fish100_pcb_lm <- lm(data = grid_pcb_data, 
                   subset = (FISHNET_SIZE == 100),
                   PCB_Total ~ 
                     MAM_PRCP_diff +
                     SiltClay +
                     DJF_TAVG_diff +
                     census2010_personsPERha)

fish400_pcb_lm <- lm(data = grid_pcb_data, 
                   subset = (FISHNET_SIZE == 400),
                   PCB_Total ~ 
                     MAM_PRCP_diff +
                     SiltClay +
                     DJF_TAVG_diff +
                     census2010_personsPERha)

fish1600_pcb_lm <- lm(data = grid_pcb_data, 
                   subset = (FISHNET_SIZE == 1600),
                   PCB_Total ~ 
                     MAM_PRCP_diff +
                     SiltClay +
                     DJF_TAVG_diff +
                     census2010_personsPERha)

#model outputs

fish25_pcb_tidy  <-  tidy(fish25_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "25")

fish25_pcb_glance<-  glance(fish25_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "25")

fish100_pcb_tidy  <-  tidy(fish100_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "100")

fish100_pcb_glance<-  glance(fish100_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "100")

fish400_pcb_tidy  <-  tidy(fish400_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "400")

fish400_pcb_glance<-  glance(fish400_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "400")

fish1600_pcb_tidy  <-  tidy(fish1600_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "1600")

fish1600_pcb_glance<-  glance(fish1600_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "grid", size = "1600")

#DDTs----

grid_ddt_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

fish25_ddt_lm <- lm(data = grid_ddt_data, 
                   subset = (FISHNET_SIZE == 25),
                   DDT_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     MAM_TAVG_diff +
                     Logger_Salin_mean +
                     census2010_personsPERha +
                     CROPS_prct +
                     MARCAT_prct)

fish100_ddt_lm <- lm(data = grid_ddt_data, 
                   subset = (FISHNET_SIZE == 100),
                   DDT_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     MAM_TAVG_diff +
                     Logger_Salin_mean +
                     census2010_personsPERha +
                     CROPS_prct)

fish400_ddt_lm <- lm(data = grid_ddt_data, 
                   subset = (FISHNET_SIZE == 400),
                   DDT_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     MAM_TAVG_diff +
                     Logger_Salin_mean +
                     IMPERV_PRCT +
                     AGRCAT_prct)

fish1600_ddt_lm <- lm(data = grid_ddt_data, 
                   subset = (FISHNET_SIZE == 1600),
                   DDT_Total ~ 
                     SiltClay +
                     DJF_PRCP_diff +
                     MAM_TAVG_diff +
                     Logger_Salin_mean +
                     IMPERV_PRCT +
                     AGRCAT_prct)

#model outputs

fish25_ddt_tidy  <-  tidy(fish25_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "25")

fish25_ddt_glance<-  glance(fish25_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "25")

fish100_ddt_tidy  <-  tidy(fish100_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "100")

fish100_ddt_glance<-  glance(fish100_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "100")

fish400_ddt_tidy  <-  tidy(fish400_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "400")

fish400_ddt_glance<-  glance(fish400_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "400")

fish1600_ddt_tidy  <-  tidy(fish1600_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "1600")

fish1600_ddt_glance<-  glance(fish1600_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "grid", size = "1600")


#PBDEs----

grid_pbde_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO10379", "RO10380", "RO10392", 
             "RT10115", "RT10129", "RT10137"))

#linear models

fish25_pbde_lm <- lm(data = grid_pbde_data, 
                    subset = (FISHNET_SIZE == 25),
                    PBDE_Total ~ 
                      DJF_PRCP_diff +
                      DJF_TAVG_diff +
                      AMO)

fish100_pbde_lm <- lm(data = grid_pbde_data, 
                    subset = (FISHNET_SIZE == 100),
                    PBDE_Total ~ 
                      DJF_PRCP_diff +
                      DJF_TAVG_diff +
                      AMO)

fish400_pbde_lm <- lm(data = grid_pbde_data, 
                    subset = (FISHNET_SIZE == 400),
                    PBDE_Total ~ 
                      DJF_PRCP_diff +
                      DJF_TAVG_diff +
                      AMO)

fish1600_pbde_lm <- lm(data = grid_pbde_data, 
                    subset = (FISHNET_SIZE == 1600),
                    PBDE_Total ~ 
                      DJF_PRCP_diff +
                      DJF_TAVG_diff +
                      AMO)

#model outputs

fish25_pbde_tidy  <-  tidy(fish25_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "25")

fish25_pbde_glance<-  glance(fish25_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "25")

fish100_pbde_tidy  <-  tidy(fish100_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "100")

fish100_pbde_glance<-  glance(fish100_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "100")

fish400_pbde_tidy  <-  tidy(fish400_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "400")

fish400_pbde_glance<-  glance(fish400_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "400")

fish1600_pbde_tidy  <-  tidy(fish1600_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "1600")

fish1600_pbde_glance<-  glance(fish1600_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "grid", size = "1600")



#ERMQ DDT----

grid_ermqddt_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

fish25_ermqddt_lm <- lm(data = grid_ermqddt_data, 
                       subset = (FISHNET_SIZE == 25),
                       ERMQ_DDT_tot ~ 
                         TOC_sediment +
                         DJF_PRCP_diff +
                         JJA_TAVG_diff +
                         Logger_Salin_mean +
                         CROPS_prct +
                         IMPERV_PRCT +
                         MARCAT_prct)

fish100_ermqddt_lm <- lm(data = grid_ermqddt_data, 
                       subset = (FISHNET_SIZE == 100),
                       ERMQ_DDT_tot ~ 
                         TOC_sediment +
                         DJF_PRCP_diff +
                         JJA_TAVG_diff +
                         Logger_Salin_mean +
                         CROPS_prct +
                         IMPERV_PRCT)

fish400_ermqddt_lm <- lm(data = grid_ermqddt_data, 
                       subset = (FISHNET_SIZE == 400),
                       ERMQ_DDT_tot ~ 
                         TOC_sediment +
                         DJF_PRCP_diff +
                         JJA_TAVG_diff +
                         Logger_Salin_mean +
                         AGRCAT_prct +
                         IMPERV_PRCT +
                         WATCAT_prct)

fish1600_ermqddt_lm <- lm(data = grid_ermqddt_data, 
                       subset = (FISHNET_SIZE == 1600),
                       ERMQ_DDT_tot ~ 
                         TOC_sediment +
                         DJF_PRCP_diff +
                         JJA_TAVG_diff +
                         Logger_Salin_mean +
                         AGRCAT_prct +
                         IMPERV_PRCT)

#model outputs

fish25_ermqddt_tidy  <-  tidy(fish25_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "25")

fish25_ermqddt_glance<-  glance(fish25_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "25")

fish100_ermqddt_tidy  <-  tidy(fish100_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "100")

fish100_ermqddt_glance<-  glance(fish100_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "100")

fish400_ermqddt_tidy  <-  tidy(fish400_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "400")

fish400_ermqddt_glance<-  glance(fish400_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "400")

fish1600_ermqddt_tidy  <-  tidy(fish1600_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "1600")

fish1600_ermqddt_glance<-  glance(fish1600_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "grid", size = "1600")



#ERMQ Metals----

grid_ermqmet_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RT01628", "RO026030"))

#linear models

fish25_ermqmet_lm <- lm(data = grid_ermqmet_data, 
                       subset = (FISHNET_SIZE == 25),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         IMPERV_PRCT +
                         GROUP_D_prct)

fish100_ermqmet_lm <- lm(data = grid_ermqmet_data, 
                       subset = (FISHNET_SIZE == 100),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         DEVHIGHCAT_prct +
                         GROUP_D_prct)

fish400_ermqmet_lm <- lm(data = grid_ermqmet_data, 
                       subset = (FISHNET_SIZE == 400),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         DEVHIGHCAT_prct +
                         GROUP_D_prct)

fish1600_ermqmet_lm <- lm(data = grid_ermqmet_data, 
                       subset = (FISHNET_SIZE == 1600),
                       ERMQ_met ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         census2010_personsPERha +
                         GROUP_D_prct)

#model outputs

fish25_ermqmet_tidy  <-  tidy(fish25_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "25")

fish25_ermqmet_glance<-  glance(fish25_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "25")

fish100_ermqmet_tidy  <-  tidy(fish100_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "100")

fish100_ermqmet_glance<-  glance(fish100_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "100")

fish400_ermqmet_tidy  <-  tidy(fish400_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "400")

fish400_ermqmet_glance<-  glance(fish400_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "400")

fish1600_ermqmet_tidy  <-  tidy(fish1600_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "1600")

fish1600_ermqmet_glance<-  glance(fish1600_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "grid", size = "1600")


#ERMQ PAHs----

grid_ermqpah_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO056092", "RO06320", 
             "RO11308", "RT11003"))

#linear models

fish25_ermqpah_lm <- lm(data = grid_ermqpah_data, 
                       subset = (FISHNET_SIZE == 25),
                       ERMQ_PAH_tot ~ 
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         IMPERV_PRCT +
                         WATCAT_prct +
                         MARCAT_prct)

fish100_ermqpah_lm <- lm(data = grid_ermqpah_data, 
                       subset = (FISHNET_SIZE == 100),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         DEVHIGHCAT_prct +
                         DJF_TAVG_diff +
                         GROUP_C_prct +
                         DJF_PRCP_diff +
                         CHANNEL_WIDTH_log)

fish400_ermqpah_lm <- lm(data = grid_ermqpah_data, 
                       subset = (FISHNET_SIZE == 400),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         DEVHIGH_prct +
                         DJF_TAVG_diff +
                         GROUP_A_prct +
                         DJF_PRCP_diff +
                         CHANNEL_WIDTH_log)

fish1600_ermqpah_lm <- lm(data = grid_ermqpah_data, 
                       subset = (FISHNET_SIZE == 1600),
                       ERMQ_PAH_tot ~ 
                         SiltClay +
                         census2010_personsPERha +
                         DJF_PRCP_diff +
                         WATCAT_prct +
                         DJF_TAVG_diff)

#model outputs

fish25_ermqpah_tidy  <-  tidy(fish25_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "25")

fish25_ermqpah_glance<-  glance(fish25_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "25")

fish100_ermqpah_tidy  <-  tidy(fish100_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "100")

fish100_ermqpah_glance<-  glance(fish100_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "100")

fish400_ermqpah_tidy  <-  tidy(fish400_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "400")

fish400_ermqpah_glance<-  glance(fish400_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "400")

fish1600_ermqpah_tidy  <-  tidy(fish1600_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "1600")

fish1600_ermqpah_glance<-  glance(fish1600_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "grid", size = "1600")


#ERMQ PCBs----

grid_ermqpcb_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

fish25_ermqpcb_lm <- lm(data = grid_ermqpcb_data, 
                       subset = (FISHNET_SIZE == 25),
                       ERMQ_PCB_tot ~ 
                         SiltClay +
                         MAM_PRCP_diff +
                         DJF_TAVG_diff +
                         census2010_personsPERha)

fish100_ermqpcb_lm <- lm(data = grid_ermqpcb_data, 
                       subset = (FISHNET_SIZE == 100),
                       ERMQ_PCB_tot ~ 
                         MAM_PRCP_diff +
                         DJF_TAVG_diff +
                         census2010_personsPERha +
                         GROUP_D_prct +
                         SiltClay)

fish400_ermqpcb_lm <- lm(data = grid_ermqpcb_data, 
                       subset = (FISHNET_SIZE == 400),
                       ERMQ_PCB_tot ~ 
                         SiltClay +
                         MAM_PRCP_diff +
                         DJF_TAVG_diff +
                         census2010_personsPERha)

fish1600_ermqpcb_lm <- lm(data = grid_ermqpcb_data, 
                       subset = (FISHNET_SIZE == 1600),
                       ERMQ_PCB_tot ~ 
                         SiltClay +
                         MAM_PRCP_diff +
                         DJF_TAVG_diff +
                         census2010_personsPERha +
                         WATCAT_prct)

#model outputs

fish25_ermqpcb_tidy  <-  tidy(fish25_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "25")

fish25_ermqpcb_glance<-  glance(fish25_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "25")

fish100_ermqpcb_tidy  <-  tidy(fish100_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "100")

fish100_ermqpcb_glance<-  glance(fish100_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "100")

fish400_ermqpcb_tidy  <-  tidy(fish400_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "400")

fish400_ermqpcb_glance<-  glance(fish400_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "400")

fish1600_ermqpcb_tidy  <-  tidy(fish1600_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "1600")

fish1600_ermqpcb_glance<-  glance(fish1600_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "grid", size = "1600")


#ERMQ ALL----

grid_ermqall_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO06320", "RO11308"))

#linear models

fish25_ermqall_lm <- lm(data = grid_ermqall_data, 
                       subset = (FISHNET_SIZE == 25),
                       ERMQ_ALL ~ 
                         SiltClay +
                         preSON_PRCP_diff +
                         DJF_PRCP_diff +
                         GROUP_C_prct +
                         census2010_personsPERha)

fish100_ermqall_lm <- lm(data = grid_ermqall_data, 
                       subset = (FISHNET_SIZE == 100),
                       ERMQ_ALL ~ 
                         SiltClay +
                         preSON_PRCP_diff +
                         DJF_PRCP_diff +
                         census2010_personsPERha)

fish400_ermqall_lm <- lm(data = grid_ermqall_data, 
                       subset = (FISHNET_SIZE == 400),
                       ERMQ_ALL ~ 
                         SiltClay +
                         preSON_PRCP_diff +
                         DJF_PRCP_diff +
                         DEVHIGHCAT_prct)

fish1600_ermqall_lm <- lm(data = grid_ermqall_data, 
                       subset = (FISHNET_SIZE == 1600),
                       ERMQ_ALL ~ 
                         SiltClay +
                         preSON_PRCP_diff +
                         DJF_PRCP_diff +
                         IMPERV_PRCT +
                         WELL_DRAINED_prct)

#model outputs

fish25_ermqall_tidy  <-  tidy(fish25_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "25")

fish25_ermqall_glance<-  glance(fish25_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "25")

fish100_ermqall_tidy  <-  tidy(fish100_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "100")

fish100_ermqall_glance<-  glance(fish100_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "100")

fish400_ermqall_tidy  <-  tidy(fish400_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "400")

fish400_ermqall_glance<-  glance(fish400_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "400")

fish1600_ermqall_tidy  <-  tidy(fish1600_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "1600")

fish1600_ermqall_glance<-  glance(fish1600_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "grid", size = "1600")



#Trawl Abundance Area----

grid_trawlabund_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RT022153", "RT06010", "RT06037"))

#linear models

fish25_trawlabund_lm <- lm(data = grid_trawlabund_data, 
                          subset = (FISHNET_SIZE == 25),
                          trawl_ABUNDANCE_area ~ 
                            CHANNEL_WIDTH_log +
                            ANN_TAVG_diff +
                            NAO +
                            IMPERV_PRCT +
                            MARCAT_prct)

fish100_trawlabund_lm <- lm(data = grid_trawlabund_data, 
                          subset = (FISHNET_SIZE == 100),
                          trawl_ABUNDANCE_area ~ 
                            CHANNEL_WIDTH_log +
                            ANN_TAVG_diff +
                            NAO +
                            IMPERV_PRCT)

fish400_trawlabund_lm <- lm(data = grid_trawlabund_data, 
                          subset = (FISHNET_SIZE == 400),
                          trawl_ABUNDANCE_area ~ 
                            CHANNEL_WIDTH_log +
                            ANN_TAVG_diff +
                            NAO +
                            IMPERV_PRCT)

fish1600_trawlabund_lm <- lm(data = grid_trawlabund_data, 
                          subset = (FISHNET_SIZE == 1600),
                          trawl_ABUNDANCE_area ~ 
                            CHANNEL_WIDTH_log +
                            ANN_TAVG_diff +
                            NAO +
                            IMPERV_PRCT)

#model outputs

fish25_trawlabund_tidy  <-  tidy(fish25_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "25")

fish25_trawlabund_glance<-  glance(fish25_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "25")

fish100_trawlabund_tidy  <-  tidy(fish100_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "100")

fish100_trawlabund_glance<-  glance(fish100_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "100")

fish400_trawlabund_tidy  <-  tidy(fish400_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "400")

fish400_trawlabund_glance<-  glance(fish400_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "400")

fish1600_trawlabund_tidy  <-  tidy(fish1600_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "1600")

fish1600_trawlabund_glance<-  glance(fish1600_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "grid", size = "1600")


#Trawl Species Richness (openwater)----

grid_trawlsprichopen_data <- analysis_grids %>% 
  filter(RT_RO_Other == "OpenWater")

#linear models

fish25_trawlsprichopen_lm <- lm(data = grid_trawlsprichopen_data, 
                               subset = (FISHNET_SIZE == 25),
                               trawl_SP_RICH ~ 
                                 CHANNEL_WIDTH_log +
                                 Logger_Salin_mean +
                                 NAO +
                                 DAYMET_30dPRCP_cm +
                                 preANN_TAVG_diff)

fish100_trawlsprichopen_lm <- lm(data = grid_trawlsprichopen_data, 
                               subset = (FISHNET_SIZE == 100),
                               trawl_SP_RICH  ~ 
                                 CHANNEL_WIDTH_log +
                                 Logger_Salin_mean +
                                 NAO +
                                 DAYMET_30dPRCP_cm +
                                 preANN_TAVG_diff)

fish400_trawlsprichopen_lm <- lm(data = grid_trawlsprichopen_data, 
                               subset = (FISHNET_SIZE == 400),
                               trawl_SP_RICH  ~ 
                                 CHANNEL_WIDTH_log +
                                 Logger_Salin_mean +
                                 NAO +
                                 DAYMET_30dPRCP_cm +
                                 preANN_TAVG_diff)

fish1600_trawlsprichopen_lm <- lm(data = grid_trawlsprichopen_data, 
                               subset = (FISHNET_SIZE == 1600),
                               trawl_SP_RICH ~ 
                                 CHANNEL_WIDTH_log +
                                 Logger_Salin_mean +
                                 NAO +
                                 DAYMET_30dPRCP_cm +
                                 preANN_TAVG_diff)

#model outputs

fish25_trawlsprichopen_tidy  <-  tidy(fish25_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "25")

fish25_trawlsprichopen_glance<-  glance(fish25_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "25")

fish100_trawlsprichopen_tidy  <-  tidy(fish100_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "100")

fish100_trawlsprichopen_glance<-  glance(fish100_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "100")

fish400_trawlsprichopen_tidy  <-  tidy(fish400_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "400")

fish400_trawlsprichopen_glance<-  glance(fish400_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "400")

fish1600_trawlsprichopen_tidy  <-  tidy(fish1600_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "1600")

fish1600_trawlsprichopen_glance<-  glance(fish1600_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "grid", size = "1600")


#Trawl Species Richness (tidal creeks)----

grid_trawlsprichtidal_data <- analysis_grids %>% 
  filter(RT_RO_Other == "TidalCreek")

#linear models

fish25_trawlsprichtidal_lm <- lm(data = grid_trawlsprichtidal_data, 
                                subset = (FISHNET_SIZE == 25),
                                trawl_SP_RICH ~ 
                                  Logger_Salin_mean +
                                  ANN_TAVG_diff +
                                  SP02 +
                                  CHANNEL_WIDTH_log)

fish100_trawlsprichtidal_lm <- lm(data = grid_trawlsprichtidal_data, 
                                subset = (FISHNET_SIZE == 100),
                                trawl_SP_RICH  ~ 
                                  Logger_Salin_mean +
                                  ANN_TAVG_diff +
                                  SP02 +
                                  MARCAT_prct)

fish400_trawlsprichtidal_lm <- lm(data = grid_trawlsprichtidal_data, 
                                subset = (FISHNET_SIZE == 400),
                                trawl_SP_RICH  ~ 
                                  Logger_Salin_mean +
                                  ANN_TAVG_diff +
                                  SP02 +
                                  MARCAT_prct)

fish1600_trawlsprichtidal_lm <- lm(data = grid_trawlsprichtidal_data, 
                                subset = (FISHNET_SIZE == 1600),
                                trawl_SP_RICH ~ 
                                  Logger_Salin_mean +
                                  ANN_TAVG_diff +
                                  SP02 +
                                  CHANNEL_WIDTH_log)

#model outputs

fish25_trawlsprichtidal_tidy  <-  tidy(fish25_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "25")

fish25_trawlsprichtidal_glance<-  glance(fish25_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "25")

fish100_trawlsprichtidal_tidy  <-  tidy(fish100_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "100")

fish100_trawlsprichtidal_glance<-  glance(fish100_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "100")

fish400_trawlsprichtidal_tidy  <-  tidy(fish400_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "400")

fish400_trawlsprichtidal_glance<-  glance(fish400_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "400")

fish1600_trawlsprichtidal_tidy  <-  tidy(fish1600_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "1600")

fish1600_trawlsprichtidal_glance<-  glance(fish1600_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "grid", size = "1600")


#Benthic Abundance Area----

grid_benthabund_data <- analysis_grids %>% 
  filter(!StationCode %in% 
           c("RO00059", "RT032180", "RO06305"))

#linear models

fish25_benthabund_lm <- lm(data = grid_benthabund_data, 
                          subset = (FISHNET_SIZE == 25),
                          benth_ABUNDANCE_area ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm +
                            MARCAT_prct)

fish100_benthabund_lm <- lm(data = grid_benthabund_data, 
                          subset = (FISHNET_SIZE == 100),
                          benth_ABUNDANCE_area  ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm +
                            MARCAT_prct)

fish400_benthabund_lm <- lm(data = grid_benthabund_data, 
                          subset = (FISHNET_SIZE == 400),
                          benth_ABUNDANCE_area ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm)

fish1600_benthabund_lm <- lm(data = grid_benthabund_data, 
                          subset = (FISHNET_SIZE == 1600),
                          benth_ABUNDANCE_area ~ 
                            Logger_Salin_mean +
                            DAYMET_90dTAVG_degC +
                            SiltClay +
                            StationDepth_m +
                            DJF_TAVG_diff +
                            DAYMET_45dPRCP_cm)

#model outputs

fish25_benthabund_tidy  <-  tidy(fish25_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "25")

fish25_benthabund_glance<-  glance(fish25_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "25")

fish100_benthabund_tidy  <-  tidy(fish100_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "100")

fish100_benthabund_glance<-  glance(fish100_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "100")

fish400_benthabund_tidy  <-  tidy(fish400_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "400")

fish400_benthabund_glance<-  glance(fish400_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "400")

fish1600_benthabund_tidy  <-  tidy(fish1600_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "1600")

fish1600_benthabund_glance<-  glance(fish1600_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "grid", size = "1600")


#Benthic Species Richness----

grid_benthsprich_data <- analysis_grids

#linear models

fish25_benthsprich_lm <- lm(data = grid_benthsprich_data, 
                           subset = (FISHNET_SIZE == 25),
                           benth_SP_RICH ~ 
                             Logger_Salin_mean +
                             DAYMET_90dTAVG_degC +
                             DAYMET_45dPRCP_cm +
                             SiltClay +
                             CHANNEL_WIDTH_log +
                             MARCAT_prct +
                             DEVLOWCAT_prct)

fish100_benthsprich_lm <- lm(data = grid_benthsprich_data, 
                           subset = (FISHNET_SIZE == 100),
                           benth_SP_RICH  ~ 
                             Logger_Salin_mean +
                             DAYMET_90dTAVG_degC +
                             DAYMET_45dPRCP_cm +
                             SiltClay +
                             CHANNEL_WIDTH_log +
                             GROUP_C_prct +
                             DEVLOWCAT_prct +
                             MIXFOREST_prct)

fish400_benthsprich_lm <- lm(data = grid_benthsprich_data, 
                           subset = (FISHNET_SIZE == 400),
                           benth_SP_RICH ~ 
                             Logger_Salin_mean +
                             DAYMET_90dTAVG_degC +
                             DAYMET_45dPRCP_cm +
                             SiltClay +
                             WATCAT_prct +
                             FORCAT_prct +
                             DEVLOWCAT_prct)

fish1600_benthsprich_lm <- lm(data = grid_benthsprich_data, 
                           subset = (FISHNET_SIZE == 1600),
                           benth_SP_RICH ~ 
                             Logger_Salin_mean +
                             DAYMET_90dTAVG_degC +
                             DAYMET_45dPRCP_cm +
                             SiltClay +
                             CHANNEL_WIDTH_log +
                             DEVOPEN_prct)

#model outputs

fish25_benthsprich_tidy  <-  tidy(fish25_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "25")

fish25_benthsprich_glance<-  glance(fish25_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "25")

fish100_benthsprich_tidy  <-  tidy(fish100_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "100")

fish100_benthsprich_glance<-  glance(fish100_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "100")

fish400_benthsprich_tidy  <-  tidy(fish400_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "400")

fish400_benthsprich_glance<-  glance(fish400_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "400")

fish1600_benthsprich_tidy  <-  tidy(fish1600_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "1600")

fish1600_benthsprich_glance<-  glance(fish1600_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "grid", size = "1600")


#BIBI----

grid_bibi_data <- analysis_grids

#linear models

fish25_bibi_lm <- lm(data = grid_bibi_data, 
                    subset = (FISHNET_SIZE == 25),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      DAYMET_45dPRCP_cm +
                      CROPS_prct +
                      MARCAT_prct +
                      DEVLOWCAT_prct)

fish100_bibi_lm <- lm(data = grid_bibi_data, 
                    subset = (FISHNET_SIZE == 100),
                    BIBI  ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      DAYMET_45dPRCP_cm +
                      CROPS_prct)

fish400_bibi_lm <- lm(data = grid_bibi_data, 
                    subset = (FISHNET_SIZE == 400),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      DAYMET_45dPRCP_cm +
                      DEVOPEN_prct +
                      census2010_personsPERha)

fish1600_bibi_lm <- lm(data = grid_bibi_data, 
                    subset = (FISHNET_SIZE == 1600),
                    BIBI ~ 
                      Logger_Salin_mean +
                      SiltClay +
                      DAYMET_90dTAVG_degC +
                      CHANNEL_WIDTH_log +
                      AGRCAT_prct +
                      DAYMET_45dPRCP_cm +
                      census2010_personsPERha)

#model outputs

fish25_bibi_tidy  <-  tidy(fish25_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "25")

fish25_bibi_glance<-  glance(fish25_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "25")

fish100_bibi_tidy  <-  tidy(fish100_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "100")

fish100_bibi_glance<-  glance(fish100_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "100")

fish400_bibi_tidy  <-  tidy(fish400_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "400")

fish400_bibi_glance<-  glance(fish400_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "400")

fish1600_bibi_tidy  <-  tidy(fish1600_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "1600")

fish1600_bibi_glance<-  glance(fish1600_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "grid", size = "1600")


#MAMBI----

grid_mambi_data <- analysis_grids

#linear models

fish25_mambi_lm <- lm(data = grid_mambi_data, 
                     subset = (FISHNET_SIZE == 25),
                     MAMBI ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       SiltClay +
                       DAYMET_90dTAVG_degC +
                       DAYMET_45dPRCP_cm +
                       MARCAT_prct +
                       AGRCAT_prct)

fish100_mambi_lm <- lm(data = grid_mambi_data, 
                     subset = (FISHNET_SIZE == 100),
                     MAMBI ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       SiltClay +
                       DAYMET_90dTAVG_degC +
                       DAYMET_45dPRCP_cm +
                       MARCAT_prct +
                       DEVOPEN_prct +
                       MIXFOREST_prct +
                       MARCAT_prct)

fish400_mambi_lm <- lm(data = grid_mambi_data, 
                     subset = (FISHNET_SIZE == 400),
                     MAMBI ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       SiltClay +
                       DAYMET_90dTAVG_degC +
                       DAYMET_45dPRCP_cm +
                       DEVOPEN_prct +
                       DEVMED_prct)

fish1600_mambi_lm <- lm(data = grid_mambi_data, 
                     subset = (FISHNET_SIZE == 1600),
                     MAMBI ~ 
                       Logger_Salin_mean +
                       CHANNEL_WIDTH_log +
                       SiltClay +
                       DAYMET_90dTAVG_degC +
                       DAYMET_45dPRCP_cm +
                       DEVOPEN_prct +
                       census2010_personsPERha)

#model outputs

fish25_mambi_tidy  <-  tidy(fish25_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "25")

fish25_mambi_glance<-  glance(fish25_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "25")

fish100_mambi_tidy  <-  tidy(fish100_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "100")

fish100_mambi_glance<-  glance(fish100_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "100")

fish400_mambi_tidy  <-  tidy(fish400_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "400")

fish400_mambi_glance<-  glance(fish400_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "400")

fish1600_mambi_tidy  <-  tidy(fish1600_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "1600")

fish1600_mambi_glance<-  glance(fish1600_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "1600")



#BUFFERS----

#Enterococcus----

buff_entero_data <- analysis_buff 

#linear models

buff1km_entero_lm <- lm(data = buff_entero_data, 
                       subset = (BUFFER_SIZE == 1),
                       Entero_LOG ~ 
                         Logger_Salin_mean +
                         CHANNEL_WIDTH_log +
                         PRCP_2 +
                         DAYMET_30dTAVG_degC +
                         MARCAT_prct +
                         UPLAND_IMPERV_PRCT)

buff2km_entero_lm <- lm(data = buff_entero_data, 
                        subset = (BUFFER_SIZE == 2),
                        Entero_LOG ~ 
                          Logger_Salin_mean +
                          CHANNEL_WIDTH_log +
                          PRCP_2 +
                          DAYMET_30dTAVG_degC +
                          MARCAT_prct +
                          UPLAND_IMPERV_PRCT)

buff3km_entero_lm <- lm(data = buff_entero_data, 
                        subset = (BUFFER_SIZE == 3),
                        Entero_LOG ~ 
                          Logger_Salin_mean +
                          CHANNEL_WIDTH_log +
                          PRCP_2 +
                          DAYMET_30dTAVG_degC +
                          UPLAND_IMPERV_PRCT)

#model outputs

buff1km_entero_tidy  <-  tidy(buff1km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "1")

buff1km_entero_glance<-  glance(buff1km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "1")

buff2km_entero_tidy  <-  tidy(buff2km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "2")

buff2km_entero_glance<-  glance(buff2km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "2")

buff3km_entero_tidy  <-  tidy(buff3km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "3")

buff3km_entero_glance<-  glance(buff3km_entero_lm) %>%
  mutate(response = "entero", spatialunit = "buff", size = "3")


#Fecal coliform----

buff_fecal_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RT12024"))

#linear models

buff1km_fecal_lm <- lm(data = buff_fecal_data, 
                      subset = (BUFFER_SIZE == 1),
                      FecalColiform_LOG ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        MARCAT_prct +
                        PRCP_2 +
                        UPLAND_IMPERV_PRCT +
                        MIX_FOREST_prct +
                        PDSI)

buff2km_fecal_lm <- lm(data = buff_fecal_data, 
                       subset = (BUFFER_SIZE == 2),
                       FecalColiform_LOG ~ 
                         Logger_Salin_mean +
                         CHANNEL_WIDTH_log +
                         MARCAT_prct +
                         PRCP_2 +
                         UPLAND_IMPERV_PRCT +
                         MIX_FOREST_prct +
                         PDSI)

buff3km_fecal_lm <- lm(data = buff_fecal_data, 
                       subset = (BUFFER_SIZE == 3),
                       FecalColiform_LOG ~ 
                         Logger_Salin_mean +
                         CHANNEL_WIDTH_log +
                         MARCAT_prct +
                         PRCP_2 +
                         UPLAND_IMPERV_PRCT +
                         MIX_FOREST_prct +
                         PDSI)


#model outputs

buff1km_fecal_tidy  <-  tidy(buff1km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "1")

buff1km_fecal_glance<-  glance(buff1km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "1")

buff2km_fecal_tidy  <-  tidy(buff2km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "2")

buff2km_fecal_glance<-  glance(buff2km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "2")

buff3km_fecal_tidy  <-  tidy(buff3km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "3")

buff3km_fecal_glance<-  glance(buff3km_fecal_lm) %>%
  mutate(response = "fecal", spatialunit = "buff", size = "3")

#Metals----

buff_metal_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO00056","RO026030"))

#linear models

buff1km_metal_lm <- lm(data = buff_metal_data, 
                      subset = (BUFFER_SIZE == 1),
                      Met_Total ~ 
                        SiltClay +
                        SP02 +
                        preSON_PRCP_diff +
                        UPLAND_IMPERV_PRCT)

buff2km_metal_lm <- lm(data = buff_metal_data, 
                       subset = (BUFFER_SIZE == 2),
                       Met_Total ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         UPLAND_IMPERV_PRCT)

buff3km_metal_lm <- lm(data = buff_metal_data, 
                       subset = (BUFFER_SIZE == 3),
                       Met_Total ~ 
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         UPLAND_IMPERV_PRCT)

#model outputs

buff1km_metal_tidy  <-  tidy(buff1km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "1")

buff1km_metal_glance<-  glance(buff1km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "1")

buff2km_metal_tidy  <-  tidy(buff2km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "2")

buff2km_metal_glance<-  glance(buff2km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "2")

buff3km_metal_tidy  <-  tidy(buff3km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "3")

buff3km_metal_glance<-  glance(buff3km_metal_lm) %>%
  mutate(response = "metal", spatialunit = "buff", size = "3")

#PAHs----

buff_pah_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO036042", "RO056092", 
             "RO06320", "RO11308", "RT11003"))

#linear models

buff1km_pah_lm <- lm(data = buff_pah_data, 
                    subset = (BUFFER_SIZE == 1),
                    PAH_Total ~ 
                      SiltClay +
                      UPLAND_IMPERV_PRCT +
                      MARCAT_prct +
                      DJF_PRCP_diff +
                      MAM_TAVG_diff)

buff2km_pah_lm <- lm(data = buff_pah_data, 
                     subset = (BUFFER_SIZE == 2),
                     PAH_Total ~ 
                       SiltClay +
                       UPLAND_IMPERV_PRCT +
                       MARCAT_prct +
                       DJF_PRCP_diff +
                       MAM_TAVG_diff)

buff3km_pah_lm <- lm(data = buff_pah_data, 
                     subset = (BUFFER_SIZE == 3),
                     PAH_Total ~ 
                       SiltClay +
                       UPLAND_IMPERV_PRCT +
                       MARCAT_prct +
                       DJF_PRCP_diff +
                       MAM_TAVG_diff)

#model outputs

buff1km_pah_tidy  <-  tidy(buff1km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "1")

buff1km_pah_glance<-  glance(buff1km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "1")

buff2km_pah_tidy  <-  tidy(buff2km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "2")

buff2km_pah_glance<-  glance(buff2km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "2")

buff3km_pah_tidy  <-  tidy(buff3km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "3")

buff3km_pah_glance<-  glance(buff3km_pah_lm) %>%
  mutate(response = "pah", spatialunit = "buff", size = "3")


#PCBs----

buff_pcb_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

buff1km_pcb_lm <- lm(data = buff_pcb_data, 
                    subset = (BUFFER_SIZE == 1),
                    PCB_Total ~ 
                      MAM_PRCP_diff +
                      SiltClay +
                      DJF_TAVG_diff +
                      UPLAND_IMPERV_PRCT)

buff2km_pcb_lm <- lm(data = buff_pcb_data, 
                     subset = (BUFFER_SIZE == 2),
                     PCB_Total ~ 
                       MAM_PRCP_diff +
                       SiltClay +
                       DJF_TAVG_diff +
                       UPLAND_IMPERV_PRCT)

buff3km_pcb_lm <- lm(data = buff_pcb_data, 
                     subset = (BUFFER_SIZE == 3),
                     PCB_Total ~ 
                       MAM_PRCP_diff +
                       SiltClay +
                       DJF_TAVG_diff +
                       UPLAND_IMPERV_PRCT)

#model outputs

buff1km_pcb_tidy  <-  tidy(buff1km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "1")

buff1km_pcb_glance<-  glance(buff1km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "1")

buff2km_pcb_tidy  <-  tidy(buff2km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "2")

buff2km_pcb_glance<-  glance(buff2km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "2")

buff3km_pcb_tidy  <-  tidy(buff3km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "3")

buff3km_pcb_glance<-  glance(buff3km_pcb_lm) %>%
  mutate(response = "pcb", spatialunit = "buff", size = "3")

#DDTs----

buff_ddt_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

buff1km_ddt_lm <- lm(data = buff_ddt_data, 
                    subset = (BUFFER_SIZE == 1),
                    DDT_Total ~ 
                      SiltClay +
                      DJF_PRCP_diff +
                      Logger_Salin_mean +
                      MAM_TAVG_diff +
                      CULTIVATED_CROPS_prct +
                      UPLAND_IMPERV_PRCT)

buff2km_ddt_lm <- lm(data = buff_ddt_data, 
                     subset = (BUFFER_SIZE == 2),
                     DDT_Total ~ 
                       SiltClay +
                       DJF_PRCP_diff +
                       Logger_Salin_mean +
                       MAM_TAVG_diff +
                       CULTIVATED_CROPS_prct +
                       UPLAND_IMPERV_PRCT)

buff3km_ddt_lm <- lm(data = buff_ddt_data, 
                     subset = (BUFFER_SIZE == 3),
                     DDT_Total ~ 
                       SiltClay +
                       DJF_PRCP_diff +
                       Logger_Salin_mean +
                       MAM_TAVG_diff +
                       MARCAT_prct +
                       CULTIVATED_CROPS_prct +
                       UPLAND_IMPERV_PRCT)

#model outputs

buff1km_ddt_tidy  <-  tidy(buff1km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "1")

buff1km_ddt_glance<-  glance(buff1km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "1")

buff2km_ddt_tidy  <-  tidy(buff2km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "2")

buff2km_ddt_glance<-  glance(buff2km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "2")

buff3km_ddt_tidy  <-  tidy(buff3km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "3")

buff3km_ddt_glance<-  glance(buff3km_ddt_lm) %>%
  mutate(response = "ddt", spatialunit = "buff", size = "3")

#PBDEs----

buff_pbde_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO10379", "RO10380", "RO10392", 
             "RT10115", "RT10129", "RT10137"))

#linear models

buff1km_pbde_lm <- lm(data = buff_pbde_data, 
                     subset = (BUFFER_SIZE == 1),
                     PBDE_Total ~ 
                       DJF_PRCP_diff +
                       DJF_TAVG_diff +
                       AMO)

buff2km_pbde_lm <- lm(data = buff_pbde_data, 
                      subset = (BUFFER_SIZE == 2),
                      PBDE_Total ~ 
                        DJF_PRCP_diff +
                        DJF_TAVG_diff +
                        AMO)

buff3km_pbde_lm <- lm(data = buff_pbde_data, 
                      subset = (BUFFER_SIZE == 3),
                      PBDE_Total ~ 
                        DJF_PRCP_diff +
                        DJF_TAVG_diff +
                        AMO)

#model outputs

buff1km_pbde_tidy  <-  tidy(buff1km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "1")

buff1km_pbde_glance<-  glance(buff1km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "1")

buff2km_pbde_tidy  <-  tidy(buff2km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "2")

buff2km_pbde_glance<-  glance(buff2km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "2")

buff3km_pbde_tidy  <-  tidy(buff3km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "3")

buff3km_pbde_glance<-  glance(buff3km_pbde_lm) %>%
  mutate(response = "pbde", spatialunit = "buff", size = "3")


#ERMQ DDT----

buff_ermqddt_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RT00526", "RT00549", "RT022152", 
             "RT042194", "RO056099", "RT07038", 
             "RT10132", "RT13059", "RO15368"))

#linear models

buff1km_ermqddt_lm <- lm(data = buff_ermqddt_data, 
                        subset = (BUFFER_SIZE == 1),
                        ERMQ_DDT_tot ~ 
                          TOC_sediment +
                          DJF_PRCP_diff +
                          UPLAND_IMPERV_PRCT +
                          CULTIVATED_CROPS_prct +
                          Logger_Salin_mean +
                          CHANNEL_WIDTH_log +
                          MAM_TAVG_diff)

buff2km_ermqddt_lm <- lm(data = buff_ermqddt_data, 
                         subset = (BUFFER_SIZE == 2),
                         ERMQ_DDT_tot ~ 
                           TOC_sediment +
                           DJF_PRCP_diff +
                           UPLAND_IMPERV_PRCT +
                           CULTIVATED_CROPS_prct +
                           Logger_Salin_mean +
                           CHANNEL_WIDTH_log +
                           MAM_TAVG_diff +
                           MARCAT_prct)

buff3km_ermqddt_lm <- lm(data = buff_ermqddt_data, 
                         subset = (BUFFER_SIZE == 3),
                         ERMQ_DDT_tot ~ 
                           TOC_sediment +
                           DJF_PRCP_diff +
                           UPLAND_IMPERV_PRCT +
                           CULTIVATED_CROPS_prct +
                           Logger_Salin_mean +
                           CHANNEL_WIDTH_log +
                           MAM_TAVG_diff +
                           MARCAT_prct)


#model outputs

buff1km_ermqddt_tidy  <-  tidy(buff1km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "1")

buff1km_ermqddt_glance<-  glance(buff1km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "1")

buff2km_ermqddt_tidy  <-  tidy(buff2km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "2")

buff2km_ermqddt_glance<-  glance(buff2km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "2")

buff3km_ermqddt_tidy  <-  tidy(buff3km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "3")

buff3km_ermqddt_glance<-  glance(buff3km_ermqddt_lm) %>%
  mutate(response = "ermqddt", spatialunit = "buff", size = "3")


#ERMQ Metals----

buff_ermqmet_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RT01628", "RO026030"))

#linear models

buff1km_ermqmet_lm <- lm(data = buff_ermqmet_data, 
                        subset = (BUFFER_SIZE == 1),
                        ERMQ_met ~ 
                          SiltClay +
                          SP02 +
                          preSON_PRCP_diff +
                          UPLAND_IMPERV_PRCT)

buff2km_ermqmet_lm <- lm(data = buff_ermqmet_data, 
                         subset = (BUFFER_SIZE == 2),
                         ERMQ_met ~ 
                           SiltClay +
                           SP02 +
                           preSON_PRCP_diff +
                           UPLAND_IMPERV_PRCT)

buff3km_ermqmet_lm <- lm(data = buff_ermqmet_data, 
                         subset = (BUFFER_SIZE == 3),
                         ERMQ_met ~ 
                           SiltClay +
                           SP02 +
                           preSON_PRCP_diff +
                           UPLAND_IMPERV_PRCT)

#model outputs

buff1km_ermqmet_tidy  <-  tidy(buff1km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "1")

buff1km_ermqmet_glance<-  glance(buff1km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "1")

buff2km_ermqmet_tidy  <-  tidy(buff2km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "2")

buff2km_ermqmet_glance<-  glance(buff2km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "2")

buff3km_ermqmet_tidy  <-  tidy(buff3km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "3")

buff3km_ermqmet_glance<-  glance(buff3km_ermqmet_lm) %>%
  mutate(response = "ermqmet", spatialunit = "buff", size = "3")


#ERMQ PAHs----

buff_ermqpah_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO056092", "RO06320", 
             "RO11308", "RT11003"))

#linear models

buff1km_ermqpah_lm <- lm(data = buff_ermqpah_data, 
                        subset = (BUFFER_SIZE == 1),
                        ERMQ_PAH_tot ~ 
                          SiltClay +
                          UPLAND_IMPERV_PRCT +
                          MARCAT_prct +
                          DJF_PRCP_diff +
                          DJF_TAVG_diff)

buff2km_ermqpah_lm <- lm(data = buff_ermqpah_data, 
                         subset = (BUFFER_SIZE == 2),
                         ERMQ_PAH_tot ~ 
                           SiltClay +
                           UPLAND_IMPERV_PRCT +
                           MARCAT_prct +
                           DJF_PRCP_diff +
                           DJF_TAVG_diff)

buff3km_ermqpah_lm <- lm(data = buff_ermqpah_data, 
                         subset = (BUFFER_SIZE == 3),
                         ERMQ_PAH_tot ~ 
                           SiltClay +
                           UPLAND_IMPERV_PRCT +
                           MARCAT_prct +
                           DJF_PRCP_diff +
                           DJF_TAVG_diff)

#model outputs

buff1km_ermqpah_tidy  <-  tidy(buff1km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "1")

buff1km_ermqpah_glance<-  glance(buff1km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "1")

buff2km_ermqpah_tidy  <-  tidy(buff2km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "2")

buff2km_ermqpah_glance<-  glance(buff2km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "2")

buff3km_ermqpah_tidy  <-  tidy(buff3km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "3")

buff3km_ermqpah_glance<-  glance(buff3km_ermqpah_lm) %>%
  mutate(response = "ermqpah", spatialunit = "buff", size = "3")


#ERMQ PAHs----

buff_ermqpcb_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO99322", "RT00549", "RT01633", 
             "RO036042", "RO036053", "RO036054", 
             "RO046082", "RT042191", "RT10132", 
             "RO11308"))

#linear models

buff1km_ermqpcb_lm <- lm(data = buff_ermqpcb_data, 
                        subset = (BUFFER_SIZE == 1),
                        ERMQ_PCB_tot ~ 
                          MAM_PRCP_diff +
                          SiltClay +
                          DJF_TAVG_diff +
                          UPLAND_IMPERV_PRCT)

buff2km_ermqpcb_lm <- lm(data = buff_ermqpcb_data, 
                         subset = (BUFFER_SIZE == 2),
                         ERMQ_PCB_tot ~ 
                           MAM_PRCP_diff +
                           SiltClay +
                           DJF_TAVG_diff +
                           UPLAND_IMPERV_PRCT +
                           WATCAT_prct)

buff3km_ermqpcb_lm <- lm(data = buff_ermqpcb_data, 
                         subset = (BUFFER_SIZE == 3),
                         ERMQ_PCB_tot ~ 
                           MAM_PRCP_diff +
                           SiltClay +
                           DJF_TAVG_diff +
                           UPLAND_IMPERV_PRCT +
                           WATCAT_prct)


#model outputs

buff1km_ermqpcb_tidy  <-  tidy(buff1km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "1")

buff1km_ermqpcb_glance<-  glance(buff1km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "1")

buff2km_ermqpcb_tidy  <-  tidy(buff2km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "2")

buff2km_ermqpcb_glance<-  glance(buff2km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "2")

buff3km_ermqpcb_tidy  <-  tidy(buff3km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "3")

buff3km_ermqpcb_glance<-  glance(buff3km_ermqpcb_lm) %>%
  mutate(response = "ermqpcb", spatialunit = "buff", size = "3")



#ERMQ ALL----

buff_ermqall_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO00056", "RO06320", "RO11308"))

#linear models

buff1km_ermqall_lm <- lm(data = buff_ermqall_data, 
                        subset = (BUFFER_SIZE == 1),
                        ERMQ_ALL ~ 
                          SiltClay +
                          UPLAND_IMPERV_PRCT +
                          preSON_PRCP_diff +
                          DJF_PRCP_diff +
                          MARCAT_prct)

buff2km_ermqall_lm <- lm(data = buff_ermqall_data, 
                         subset = (BUFFER_SIZE == 2),
                         ERMQ_ALL ~ 
                           SiltClay +
                           UPLAND_IMPERV_PRCT +
                           preSON_PRCP_diff +
                           DJF_PRCP_diff)

buff3km_ermqall_lm <- lm(data = buff_ermqall_data, 
                         subset = (BUFFER_SIZE == 3),
                         ERMQ_ALL ~ 
                           SiltClay +
                           UPLAND_IMPERV_PRCT +
                           preSON_PRCP_diff +
                           DJF_PRCP_diff)


#model outputs

buff1km_ermqall_tidy  <-  tidy(buff1km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "1")

buff1km_ermqall_glance<-  glance(buff1km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "1")

buff2km_ermqall_tidy  <-  tidy(buff2km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "2")

buff2km_ermqall_glance<-  glance(buff2km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "2")

buff3km_ermqall_tidy  <-  tidy(buff3km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "3")

buff3km_ermqall_glance<-  glance(buff3km_ermqall_lm) %>%
  mutate(response = "ermqall", spatialunit = "buff", size = "3")


#Trawl Abundance Area----

buff_trawlabund_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RT022153", "RT06010", "RT06037"))

#linear models

buff1km_trawlabund_lm <- lm(data = buff_trawlabund_data, 
                           subset = (BUFFER_SIZE == 1),
                           trawl_ABUNDANCE_area ~ 
                             CHANNEL_WIDTH_log +
                             MARCAT_prct +
                             UPLAND_IMPERV_PRCT +
                             NAO +
                             ANN_TAVG_diff)

buff2km_trawlabund_lm <- lm(data = buff_trawlabund_data, 
                            subset = (BUFFER_SIZE == 2),
                            trawl_ABUNDANCE_area ~ 
                              CHANNEL_WIDTH_log +
                              MARCAT_prct +
                              UPLAND_IMPERV_PRCT +
                              NAO +
                              ANN_TAVG_diff)

buff3km_trawlabund_lm <- lm(data = buff_trawlabund_data, 
                            subset = (BUFFER_SIZE == 3),
                            trawl_ABUNDANCE_area ~ 
                              CHANNEL_WIDTH_log +
                              MARCAT_prct +
                              UPLAND_IMPERV_PRCT +
                              NAO +
                              ANN_TAVG_diff)

#model outputs

buff1km_trawlabund_tidy  <-  tidy(buff1km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "1")

buff1km_trawlabund_glance<-  glance(buff1km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "1")

buff2km_trawlabund_tidy  <-  tidy(buff2km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "2")

buff2km_trawlabund_glance<-  glance(buff2km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "2")

buff3km_trawlabund_tidy  <-  tidy(buff3km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "3")

buff3km_trawlabund_glance<-  glance(buff3km_trawlabund_lm) %>%
  mutate(response = "trawlabund", spatialunit = "buff", size = "3")



#Trawl Species Richness (openwater)----

buff_trawlsprichopen_data <- analysis_buff %>% 
  filter(RT_RO_Other == "OpenWater")

#linear models

buff1km_trawlsprichopen_lm <- lm(data = buff_trawlsprichopen_data, 
                                subset = (BUFFER_SIZE == 1),
                                trawl_SP_RICH ~ 
                                  CHANNEL_WIDTH_log +
                                  Logger_Salin_mean +
                                  NAO +
                                  DAYMET_30dPRCP_cm)

buff2km_trawlsprichopen_lm <- lm(data = buff_trawlsprichopen_data, 
                                 subset = (BUFFER_SIZE == 2),
                                 trawl_SP_RICH  ~ 
                                   CHANNEL_WIDTH_log +
                                   Logger_Salin_mean +
                                   NAO +
                                   DAYMET_30dPRCP_cm)

buff3km_trawlsprichopen_lm <- lm(data = buff_trawlsprichopen_data, 
                                 subset = (BUFFER_SIZE == 3),
                                 trawl_SP_RICH  ~ 
                                   CHANNEL_WIDTH_log +
                                   Logger_Salin_mean +
                                   NAO +
                                   DAYMET_30dPRCP_cm +
                                   DEVELOPED_OPEN_prct)


#model outputs

buff1km_trawlsprichopen_tidy  <-  tidy(buff1km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "1")

buff1km_trawlsprichopen_glance<-  glance(buff1km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "1")

buff2km_trawlsprichopen_tidy  <-  tidy(buff2km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "2")

buff2km_trawlsprichopen_glance<-  glance(buff2km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "2")

buff3km_trawlsprichopen_tidy  <-  tidy(buff3km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "3")

buff3km_trawlsprichopen_glance<-  glance(buff3km_trawlsprichopen_lm) %>%
  mutate(response = "trawlsprichopen", spatialunit = "buff", size = "3")


#Trawl Species Richness (tidal creeks)----

buff_trawlsprichtidal_data <- analysis_buff %>% 
  filter(RT_RO_Other == "TidalCreek")

#linear models

buff1km_trawlsprichtidal_lm <- lm(data = buff_trawlsprichtidal_data, 
                                 subset = (BUFFER_SIZE == 1),
                                 trawl_SP_RICH ~ 
                                   Logger_Salin_mean +
                                   ANN_TAVG_diff +
                                   SP02 +
                                   DEVHIGHCAT_prct +
                                   CHANNEL_WIDTH_log)

buff2km_trawlsprichtidal_lm <- lm(data = buff_trawlsprichtidal_data, 
                                  subset = (BUFFER_SIZE == 2),
                                  trawl_SP_RICH  ~ 
                                    Logger_Salin_mean +
                                    ANN_TAVG_diff +
                                    SP02 +
                                    DEVHIGHCAT_prct +
                                    CHANNEL_WIDTH_log)

buff3km_trawlsprichtidal_lm <- lm(data = buff_trawlsprichtidal_data, 
                                  subset = (BUFFER_SIZE == 3),
                                  trawl_SP_RICH  ~ 
                                    Logger_Salin_mean +
                                    ANN_TAVG_diff +
                                    SP02 +
                                    DEVHIGHCAT_prct +
                                    CHANNEL_WIDTH_log)

#model outputs

buff1km_trawlsprichtidal_tidy  <-  tidy(buff1km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "1")

buff1km_trawlsprichtidal_glance<-  glance(buff1km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "1")

buff2km_trawlsprichtidal_tidy  <-  tidy(buff2km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "2")

buff2km_trawlsprichtidal_glance<-  glance(buff2km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "2")

buff3km_trawlsprichtidal_tidy  <-  tidy(buff3km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "3")

buff3km_trawlsprichtidal_glance<-  glance(buff3km_trawlsprichtidal_lm) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "buff", size = "3")


#Benthic Abundance Area----

buff_benthabund_data <- analysis_buff %>% 
  filter(!StationCode %in% 
           c("RO00059", "RT032180", "RO06305"))

#linear models

buff1km_benthabund_lm <- lm(data = buff_benthabund_data, 
                           subset = (BUFFER_SIZE == 1),
                           benth_ABUNDANCE_area ~ 
                             Logger_Salin_mean +
                             DAYMET_90dTAVG_degC +
                             SiltClay +
                             StationDepth_m +
                             DJF_TAVG_diff +
                             DAYMET_45dPRCP_cm)

buff2km_benthabund_lm <- lm(data = buff_benthabund_data, 
                            subset = (BUFFER_SIZE == 2),
                            benth_ABUNDANCE_area  ~ 
                              Logger_Salin_mean +
                              DAYMET_90dTAVG_degC +
                              SiltClay +
                              StationDepth_m +
                              DJF_TAVG_diff +
                              DAYMET_45dPRCP_cm)

buff3km_benthabund_lm <- lm(data = buff_benthabund_data, 
                            subset = (BUFFER_SIZE == 3),
                            benth_ABUNDANCE_area ~ 
                              Logger_Salin_mean +
                              DAYMET_90dTAVG_degC +
                              SiltClay +
                              StationDepth_m +
                              DJF_TAVG_diff +
                              DAYMET_45dPRCP_cm +
                              MARCAT_prct)


#model outputs

buff1km_benthabund_tidy  <-  tidy(buff1km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "1")

buff1km_benthabund_glance<-  glance(buff1km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "1")

buff2km_benthabund_tidy  <-  tidy(buff2km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "2")

buff2km_benthabund_glance<-  glance(buff2km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "2")

buff3km_benthabund_tidy  <-  tidy(buff3km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "3")

buff3km_benthabund_glance<-  glance(buff3km_benthabund_lm) %>%
  mutate(response = "benthabund", spatialunit = "buff", size = "3")


#Benthic Species Richness----

buff_benthsprich_data <- analysis_buff

#linear models

buff1km_benthsprich_lm <- lm(data = buff_benthsprich_data, 
                            subset = (BUFFER_SIZE == 1),
                            benth_SP_RICH ~ 
                              Logger_Salin_mean +
                              DAYMET_90dTAVG_degC +
                              DAYMET_45dPRCP_cm +
                              SiltClay +
                              CHANNEL_WIDTH_log +
                              MARCAT_prct)

buff2km_benthsprich_lm <- lm(data = buff_benthsprich_data, 
                             subset = (BUFFER_SIZE == 2),
                             benth_SP_RICH  ~ 
                               Logger_Salin_mean +
                               DAYMET_90dTAVG_degC +
                               DAYMET_45dPRCP_cm +
                               SiltClay +
                               CHANNEL_WIDTH_log +
                               MARCAT_prct)

buff3km_benthsprich_lm <- lm(data = buff_benthsprich_data, 
                             subset = (BUFFER_SIZE == 3),
                             benth_SP_RICH ~ 
                               Logger_Salin_mean +
                               DAYMET_90dTAVG_degC +
                               DAYMET_45dPRCP_cm +
                               SiltClay +
                               CHANNEL_WIDTH_log +
                               MARCAT_prct +
                               DEVLOWCAT_prct)


#model outputs

buff1km_benthsprich_tidy  <-  tidy(buff1km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "1")

buff1km_benthsprich_glance<-  glance(buff1km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "1")

buff2km_benthsprich_tidy  <-  tidy(buff2km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "2")

buff2km_benthsprich_glance<-  glance(buff2km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "2")

buff3km_benthsprich_tidy  <-  tidy(buff3km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "3")

buff3km_benthsprich_glance<-  glance(buff3km_benthsprich_lm) %>%
  mutate(response = "benthsprich", spatialunit = "buff", size = "3")



#BIBI----

buff_bibi_data <- analysis_buff

#linear models

buff1km_bibi_lm <- lm(data = buff_bibi_data, 
                     subset = (BUFFER_SIZE == 1),
                     BIBI ~ 
                       Logger_Salin_mean +
                       SiltClay +
                       DAYMET_90dTAVG_degC +
                       CHANNEL_WIDTH_log +
                       DAYMET_45dPRCP_cm +
                       MARCAT_prct)

buff2km_bibi_lm <- lm(data = buff_bibi_data, 
                      subset = (BUFFER_SIZE == 2),
                      BIBI  ~ 
                        Logger_Salin_mean +
                        SiltClay +
                        DAYMET_90dTAVG_degC +
                        CHANNEL_WIDTH_log +
                        DAYMET_45dPRCP_cm +
                        MARCAT_prct)

buff3km_bibi_lm <- lm(data = buff_bibi_data, 
                      subset = (BUFFER_SIZE == 3),
                      BIBI ~ 
                        Logger_Salin_mean +
                        SiltClay +
                        DAYMET_90dTAVG_degC +
                        CHANNEL_WIDTH_log +
                        DAYMET_45dPRCP_cm +
                        MARCAT_prct +
                        DEVELOPED_OPEN_prct)


#model outputs

buff1km_bibi_tidy  <-  tidy(buff1km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "1")

buff1km_bibi_glance<-  glance(buff1km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "1")

buff2km_bibi_tidy  <-  tidy(buff2km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "2")

buff2km_bibi_glance<-  glance(buff2km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "2")

buff3km_bibi_tidy  <-  tidy(buff3km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "3")

buff3km_bibi_glance<-  glance(buff3km_bibi_lm) %>%
  mutate(response = "bibi", spatialunit = "buff", size = "3")


#MAMBI----

buff_mambi_data <- analysis_buff

#linear models

buff1km_mambi_lm <- lm(data = buff_mambi_data, 
                      subset = (BUFFER_SIZE == 1),
                      MAMBI ~ 
                        Logger_Salin_mean +
                        CHANNEL_WIDTH_log +
                        SiltClay +
                        DAYMET_90dTAVG_degC +
                        DAYMET_45dPRCP_cm)

buff2km_mambi_lm <- lm(data = buff_mambi_data, 
                       subset = (BUFFER_SIZE == 2),
                       MAMBI ~ 
                         Logger_Salin_mean +
                         CHANNEL_WIDTH_log +
                         SiltClay +
                         DAYMET_90dTAVG_degC +
                         DAYMET_45dPRCP_cm)

buff3km_mambi_lm <- lm(data = buff_mambi_data, 
                       subset = (BUFFER_SIZE == 3),
                       MAMBI ~ 
                         Logger_Salin_mean +
                         CHANNEL_WIDTH_log +
                         SiltClay +
                         DAYMET_90dTAVG_degC +
                         DAYMET_45dPRCP_cm)

#model outputs

buff1km_mambi_tidy  <-  tidy(buff1km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "1")

buff1km_mambi_glance<-  glance(buff1km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "1")

buff2km_mambi_tidy  <-  tidy(buff2km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "2")

buff2km_mambi_glance<-  glance(buff2km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "2")

buff3km_mambi_tidy  <-  tidy(buff3km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "3")

buff3km_mambi_glance<-  glance(buff3km_mambi_lm) %>%
  mutate(response = "mambi", spatialunit = "buff", size = "3")


#Combine model outputs into one database-----

tidy_models_output <- rbind(huc08_entero_tidy, huc10_entero_tidy, huc12_entero_tidy, huc14_entero_tidy,
                            huc08_fecal_tidy, huc10_fecal_tidy, huc12_fecal_tidy, huc14_fecal_tidy,
                            huc08_metal_tidy, huc10_metal_tidy, huc12_metal_tidy, huc14_metal_tidy,
                            huc08_pah_tidy, huc10_pah_tidy, huc12_pah_tidy, huc14_pah_tidy,
                            huc08_pcb_tidy, huc10_pcb_tidy, huc12_pcb_tidy, huc14_pcb_tidy,
                            huc08_ddt_tidy, huc10_ddt_tidy, huc12_ddt_tidy, huc14_ddt_tidy,
                            huc08_pbde_tidy, huc10_pbde_tidy, huc12_pbde_tidy, huc14_pbde_tidy,
                            huc08_ermqddt_tidy, huc10_ermqddt_tidy, huc12_ermqddt_tidy, huc14_ermqddt_tidy,
                            huc08_ermqmet_tidy, huc10_ermqmet_tidy, huc12_ermqmet_tidy, huc14_ermqmet_tidy,
                            huc08_ermqpah_tidy, huc10_ermqpah_tidy, huc12_ermqpah_tidy, huc14_ermqpah_tidy,
                            huc08_ermqpcb_tidy, huc10_ermqpcb_tidy, huc12_ermqpcb_tidy, huc14_ermqpcb_tidy,
                            huc08_ermqall_tidy, huc10_ermqall_tidy, huc12_ermqall_tidy, huc14_ermqall_tidy,
                            huc08_trawlabund_tidy, huc10_trawlabund_tidy, huc12_trawlabund_tidy, huc14_trawlabund_tidy,
                            huc08_trawlsprichtidal_tidy, huc10_trawlsprichtidal_tidy, huc12_trawlsprichtidal_tidy, huc14_trawlsprichtidal_tidy,
                            huc08_trawlsprichopen_tidy, huc10_trawlsprichopen_tidy, huc12_trawlsprichopen_tidy, huc14_trawlsprichopen_tidy,
                            huc08_benthabund_tidy, huc10_benthabund_tidy, huc12_benthabund_tidy, huc14_benthabund_tidy,
                            huc08_benthsprich_tidy, huc10_benthsprich_tidy, huc12_benthsprich_tidy, huc14_benthsprich_tidy,
                            huc08_bibi_tidy, huc10_bibi_tidy, huc12_bibi_tidy, huc14_bibi_tidy,
                            huc08_mambi_tidy, huc10_mambi_tidy, huc12_mambi_tidy, huc14_mambi_tidy,
                            fish25_entero_tidy, fish100_entero_tidy, fish400_entero_tidy, fish1600_entero_tidy,
                            fish25_fecal_tidy, fish100_fecal_tidy, fish400_fecal_tidy, fish1600_fecal_tidy,
                            fish25_metal_tidy, fish100_metal_tidy, fish400_metal_tidy, fish1600_metal_tidy,
                            fish25_pah_tidy, fish100_pah_tidy, fish400_pah_tidy, fish1600_pah_tidy,
                            fish25_pcb_tidy, fish100_pcb_tidy, fish400_pcb_tidy, fish1600_pcb_tidy,
                            fish25_ddt_tidy, fish100_ddt_tidy, fish400_ddt_tidy, fish1600_ddt_tidy,
                            fish25_pbde_tidy, fish100_pbde_tidy, fish400_pbde_tidy, fish1600_pbde_tidy,
                            fish25_ermqddt_tidy, fish100_ermqddt_tidy, fish400_ermqddt_tidy, fish1600_ermqddt_tidy,
                            fish25_ermqmet_tidy, fish100_ermqmet_tidy, fish400_ermqmet_tidy, fish1600_ermqmet_tidy,
                            fish25_ermqpah_tidy, fish100_ermqpah_tidy, fish400_ermqpah_tidy, fish1600_ermqpah_tidy,
                            fish25_ermqpcb_tidy, fish100_ermqpcb_tidy, fish400_ermqpcb_tidy, fish1600_ermqpcb_tidy,
                            fish25_ermqall_tidy, fish100_ermqall_tidy, fish400_ermqall_tidy, fish1600_ermqall_tidy,
                            fish25_trawlabund_tidy, fish100_trawlabund_tidy, fish400_trawlabund_tidy, fish1600_trawlabund_tidy,
                            fish25_trawlsprichtidal_tidy, fish100_trawlsprichtidal_tidy, fish400_trawlsprichtidal_tidy, fish1600_trawlsprichtidal_tidy,
                            fish25_trawlsprichopen_tidy, fish100_trawlsprichopen_tidy, fish400_trawlsprichopen_tidy, fish1600_trawlsprichopen_tidy,
                            fish25_benthabund_tidy, fish100_benthabund_tidy, fish400_benthabund_tidy, fish1600_benthabund_tidy,
                            fish25_benthsprich_tidy, fish100_benthsprich_tidy, fish400_benthsprich_tidy, fish1600_benthsprich_tidy,
                            fish25_bibi_tidy, fish100_bibi_tidy, fish400_bibi_tidy, fish1600_bibi_tidy,
                            fish25_mambi_tidy, fish100_mambi_tidy, fish400_mambi_tidy, fish1600_mambi_tidy,
                            buff1km_entero_tidy, buff2km_entero_tidy, buff3km_entero_tidy, 
                            buff1km_fecal_tidy, buff2km_fecal_tidy, buff3km_fecal_tidy, 
                            buff1km_metal_tidy, buff2km_metal_tidy, buff3km_metal_tidy, 
                            buff1km_pah_tidy, buff2km_pah_tidy, buff3km_pah_tidy, 
                            buff1km_pcb_tidy, buff2km_pcb_tidy, buff3km_pcb_tidy,
                            buff1km_ddt_tidy, buff2km_ddt_tidy, buff3km_ddt_tidy, 
                            buff1km_pbde_tidy, buff2km_pbde_tidy, buff3km_pbde_tidy, 
                            buff1km_ermqddt_tidy, buff2km_ermqddt_tidy, buff3km_ermqddt_tidy, 
                            buff1km_ermqmet_tidy, buff2km_ermqmet_tidy, buff3km_ermqmet_tidy, 
                            buff1km_ermqpah_tidy, buff2km_ermqpah_tidy, buff3km_ermqpah_tidy, 
                            buff1km_ermqpcb_tidy, buff2km_ermqpcb_tidy, buff3km_ermqpcb_tidy, 
                            buff1km_ermqall_tidy, buff2km_ermqall_tidy, buff3km_ermqall_tidy, 
                            buff1km_trawlabund_tidy, buff2km_trawlabund_tidy, buff3km_trawlabund_tidy, 
                            buff1km_trawlsprichtidal_tidy, buff2km_trawlsprichtidal_tidy, buff3km_trawlsprichtidal_tidy, 
                            buff1km_trawlsprichopen_tidy, buff2km_trawlsprichopen_tidy, buff3km_trawlsprichopen_tidy, 
                            buff1km_benthabund_tidy, buff2km_benthabund_tidy, buff3km_benthabund_tidy, 
                            buff1km_benthsprich_tidy, buff2km_benthsprich_tidy, buff3km_benthsprich_tidy,
                            buff1km_bibi_tidy, buff2km_bibi_tidy, buff3km_bibi_tidy, 
                            buff1km_mambi_tidy, buff2km_mambi_tidy, buff3km_mambi_tidy)
                            
                            
glance_models_output <- rbind(huc08_entero_glance, huc10_entero_glance, huc12_entero_glance, huc14_entero_glance,
                            huc08_fecal_glance, huc10_fecal_glance, huc12_fecal_glance, huc14_fecal_glance,
                            huc08_metal_glance, huc10_metal_glance, huc12_metal_glance, huc14_metal_glance,
                            huc08_pah_glance, huc10_pah_glance, huc12_pah_glance, huc14_pah_glance,
                            huc08_pcb_glance, huc10_pcb_glance, huc12_pcb_glance, huc14_pcb_glance,
                            huc08_ddt_glance, huc10_ddt_glance, huc12_ddt_glance, huc14_ddt_glance,
                            huc08_pbde_glance, huc10_pbde_glance, huc12_pbde_glance, huc14_pbde_glance,
                            huc08_ermqddt_glance, huc10_ermqddt_glance, huc12_ermqddt_glance, huc14_ermqddt_glance,
                            huc08_ermqmet_glance, huc10_ermqmet_glance, huc12_ermqmet_glance, huc14_ermqmet_glance,
                            huc08_ermqpah_glance, huc10_ermqpah_glance, huc12_ermqpah_glance, huc14_ermqpah_glance,
                            huc08_ermqpcb_glance, huc10_ermqpcb_glance, huc12_ermqpcb_glance, huc14_ermqpcb_glance,
                            huc08_ermqall_glance, huc10_ermqall_glance, huc12_ermqall_glance, huc14_ermqall_glance,
                            huc08_trawlabund_glance, huc10_trawlabund_glance, huc12_trawlabund_glance, huc14_trawlabund_glance,
                            huc08_trawlsprichtidal_glance, huc10_trawlsprichtidal_glance, huc12_trawlsprichtidal_glance, huc14_trawlsprichtidal_glance,
                            huc08_trawlsprichopen_glance, huc10_trawlsprichopen_glance, huc12_trawlsprichopen_glance, huc14_trawlsprichopen_glance,
                            huc08_benthabund_glance, huc10_benthabund_glance, huc12_benthabund_glance, huc14_benthabund_glance,
                            huc08_benthsprich_glance, huc10_benthsprich_glance, huc12_benthsprich_glance, huc14_benthsprich_glance,
                            huc08_bibi_glance, huc10_bibi_glance, huc12_bibi_glance, huc14_bibi_glance,
                            huc08_mambi_glance, huc10_mambi_glance, huc12_mambi_glance, huc14_mambi_glance,
                            fish25_entero_glance, fish100_entero_glance, fish400_entero_glance, fish1600_entero_glance,
                            fish25_fecal_glance, fish100_fecal_glance, fish400_fecal_glance, fish1600_fecal_glance,
                            fish25_metal_glance, fish100_metal_glance, fish400_metal_glance, fish1600_metal_glance,
                            fish25_pah_glance, fish100_pah_glance, fish400_pah_glance, fish1600_pah_glance,
                            fish25_pcb_glance, fish100_pcb_glance, fish400_pcb_glance, fish1600_pcb_glance,
                            fish25_ddt_glance, fish100_ddt_glance, fish400_ddt_glance, fish1600_ddt_glance,
                            fish25_pbde_glance, fish100_pbde_glance, fish400_pbde_glance, fish1600_pbde_glance,
                            fish25_ermqddt_glance, fish100_ermqddt_glance, fish400_ermqddt_glance, fish1600_ermqddt_glance,
                            fish25_ermqmet_glance, fish100_ermqmet_glance, fish400_ermqmet_glance, fish1600_ermqmet_glance,
                            fish25_ermqpah_glance, fish100_ermqpah_glance, fish400_ermqpah_glance, fish1600_ermqpah_glance,
                            fish25_ermqpcb_glance, fish100_ermqpcb_glance, fish400_ermqpcb_glance, fish1600_ermqpcb_glance,
                            fish25_ermqall_glance, fish100_ermqall_glance, fish400_ermqall_glance, fish1600_ermqall_glance,
                            fish25_trawlabund_glance, fish100_trawlabund_glance, fish400_trawlabund_glance, fish1600_trawlabund_glance,
                            fish25_trawlsprichtidal_glance, fish100_trawlsprichtidal_glance, fish400_trawlsprichtidal_glance, fish1600_trawlsprichtidal_glance,
                            fish25_trawlsprichopen_glance, fish100_trawlsprichopen_glance, fish400_trawlsprichopen_glance, fish1600_trawlsprichopen_glance,
                            fish25_benthabund_glance, fish100_benthabund_glance, fish400_benthabund_glance, fish1600_benthabund_glance,
                            fish25_benthsprich_glance, fish100_benthsprich_glance, fish400_benthsprich_glance, fish1600_benthsprich_glance,
                            fish25_bibi_glance, fish100_bibi_glance, fish400_bibi_glance, fish1600_bibi_glance,
                            fish25_mambi_glance, fish100_mambi_glance, fish400_mambi_glance, fish1600_mambi_glance,
                            buff1km_entero_glance, buff2km_entero_glance, buff3km_entero_glance, 
                            buff1km_fecal_glance, buff2km_fecal_glance, buff3km_fecal_glance, 
                            buff1km_metal_glance, buff2km_metal_glance, buff3km_metal_glance, 
                            buff1km_pah_glance, buff2km_pah_glance, buff3km_pah_glance, 
                            buff1km_pcb_glance, buff2km_pcb_glance, buff3km_pcb_glance,
                            buff1km_ddt_glance, buff2km_ddt_glance, buff3km_ddt_glance, 
                            buff1km_pbde_glance, buff2km_pbde_glance, buff3km_pbde_glance, 
                            buff1km_ermqddt_glance, buff2km_ermqddt_glance, buff3km_ermqddt_glance, 
                            buff1km_ermqmet_glance, buff2km_ermqmet_glance, buff3km_ermqmet_glance, 
                            buff1km_ermqpah_glance, buff2km_ermqpah_glance, buff3km_ermqpah_glance, 
                            buff1km_ermqpcb_glance, buff2km_ermqpcb_glance, buff3km_ermqpcb_glance, 
                            buff1km_ermqall_glance, buff2km_ermqall_glance, buff3km_ermqall_glance, 
                            buff1km_trawlabund_glance, buff2km_trawlabund_glance, buff3km_trawlabund_glance, 
                            buff1km_trawlsprichtidal_glance, buff2km_trawlsprichtidal_glance, buff3km_trawlsprichtidal_glance, 
                            buff1km_trawlsprichopen_glance, buff2km_trawlsprichopen_glance, buff3km_trawlsprichopen_glance, 
                            buff1km_benthabund_glance, buff2km_benthabund_glance, buff3km_benthabund_glance, 
                            buff1km_benthsprich_glance, buff2km_benthsprich_glance, buff3km_benthsprich_glance,
                            buff1km_bibi_glance, buff2km_bibi_glance, buff3km_bibi_glance, 
                            buff1km_mambi_glance, buff2km_mambi_glance, buff3km_mambi_glance)




#Export combined model outputs as .csv files

write.csv(tidy_models_output, "~/Documents/R_Thesis_Graphs/outputs/tidy_models_output.csv")
write.csv(glance_models_output, "~/Documents/R_Thesis_Graphs/outputs/glance_models_output.csv")




