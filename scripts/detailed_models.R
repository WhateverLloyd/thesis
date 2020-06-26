#Fine tune select linear models and explore GAMs

#load required packages
library(tidyverse)
library(car)
library(mgcv)
library(MASS)
library(olsrr)
library(stargazer)


#import datasets
analysis_hucs <- read.csv("data/scecap_hucs_for_analysis.csv")
analysis_buff <- read.csv("data/scecap_buffers_for_analysis.csv")
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


#Linear models----

#ERMQ DDT

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


#ERMQ Metals

ermq_metal_model<- lm(data = huc_ermqmet_data[-c(1358,1451,1300,816,988, 1367,
                                                 1441),], 
                       subset = (HUC_SIZE == 10),
                       ERMQ_met ~
                         SiltClay +
                         SP02 +
                         preSON_PRCP_diff +
                         POORLY_DRAINE_prct +
                         IMPERV_PRCT)


#ERMQ PAHs

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


#ERMQ PCBs

ermq_pcb_model<- lm(data = huc_ermqpcb_data[-c(1358,1345,1560,1135,1306,838,
                                               1083, 1059, 1078, 1317, 899,
                                               1045, 1042, 1086, 1034),], 
                       subset = (HUC_SIZE == 10),
                       ERMQ_PCB_tot ~ 
                         SiltClay +
                         MAM_PRCP_diff*DJF_TAVG_diff +
                         personsPERha)


#ERMQ All

ermq_all_model <- lm(data = huc_ermqall_data[-c(1050, 988, 1007, 1394, 
                                                816, 829, 1373),], 
                       subset = (HUC_SIZE == 10),
                       ERMQ_ALL ~ 
                         SiltClay +
                         DJF_PRCP_diff +
                         DJF_TAVG_diff +
                         GROUP_A_prct +
                         personsPERha)


#Fecal coliform

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


#Enterococcus

entero_model <- lm(data = buff_entero_data[-c(989, 1013, 1173, 890, 894,
                                                   914, 860, 972, 1258, 1237),], 
                        subset = (BUFFER_SIZE == 3),
                        Entero_LOG ~ 
                          Logger_Salin_mean +
                          CHANNEL_WIDTH_log +
                          PRCP_2 +
                          DAYMET_30dTAVG_degC +
                          UPLAND_IMPERV_PRCT)


#Trawl abundance

trawlabund_model <- lm(data = huc_trawlabund_data, 
                          subset = (HUC_SIZE == 12),
                          log(trawl_ABUNDANCE_area) ~ 
                            CHANNEL_WIDTH_log +
                            IMPERV_PRCT +
                            ANN_TAVG_diff +
                            NAO)


#Trawl richness (open water)

trawlsprichopen_model <- lm(data = huc_trawlsprichopen_data[-c(1222,1113),], 
                               subset = (HUC_SIZE == 12),
                               trawl_SP_RICH  ~ 
                                 CHANNEL_WIDTH_log +
                                 Logger_Salin_mean +
                                 NAO +
                                 MARCAT_prct +
                                 DAYMET_30dPRCP_cm +
                                 preANN_TAVG_diff)


#Trawl richness (tidal creek)

trawlsprichtidal_model <- lm(data = huc_trawlsprichtidal_data[-c(1028, 1086),], 
                                subset = (HUC_SIZE == 12),
                                trawl_SP_RICH  ~ 
                                  Logger_Salin_mean +
                                  AGRCAT_prct +
                                  ANN_TAVG_diff +
                                  SP02 +
                                  CHANNEL_WIDTH_log)


#Benthic abundance

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


#Benthic richness

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


#MAMBI

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

#GAMs-----

#recreate same conditions as linear models 
#but with GAMs and s() functions around each independent variable

ermq_ddt_gam <- gam(data = huc_ermqddt_data[-c(1276, 1075, 1351, 1449, 1264,
                                               1482, 1467, 1088, 1441, 1495,
                                               1222, 1507, 884, 1372, 1301, 
                                               1122, 1362, 1340, 1013, 1043,
                                               1086),], 
                    subset = (HUC_SIZE == 10),
                    ERMQ_DDT_tot ~ 
                      s(TOC_sediment) +
                      s(DJF_PRCP_diff) +
                      s(JJA_TAVG_diff) +
                      s(WATCAT_prct) +
                      s(AGRCAT_prct) +
                      s(IMPERV_PRCT), method = "REML")


ermq_metal_gam<- gam(data = huc_ermqmet_data[-c(1358,1451,1300,816,988, 1367,
                                                1441),], 
                     subset = (HUC_SIZE == 10),
                     ERMQ_met ~ 
                       s(SiltClay) +
                       s(SP02) +
                       s(preSON_PRCP_diff) +
                       s(POORLY_DRAINE_prct) +
                       s(IMPERV_PRCT),
                     method = "REML")


ermq_pah_gam <- gam(data = huc_ermqpah_data[-c(1212,989,1053,827,1048,
                                               1512, 1005, 1554, 1370, 1361, 991, 
                                               1003, 904, 1403, 1327, 916, 1429,
                                               932, 872, 1128),], 
                    subset = (HUC_SIZE == 10),
                    ERMQ_PAH_tot ~ 
                      s(SiltClay) +
                      s(CHANNEL_WIDTH_log) +
                      s(DJF_PRCP_diff, DJF_TAVG_diff) +
                      s(MIXED_FOREST_prct) +
                      s(DJF_PRCP_diff) +
                      s(DJF_TAVG_diff) +
                      s(personsPERha), 
                    method = "REML")


ermq_pcb_gam<- gam(data = huc_ermqpcb_data[-c(1358,1345,1560,1135,1306,838,
                                              1083, 1059, 1078, 1317, 899,
                                              1045, 1042, 1086, 1034),], 
                   subset = (HUC_SIZE == 10),
                   ERMQ_PCB_tot ~ 
                     s(SiltClay) +
                     s(DJF_TAVG_diff, MAM_PRCP_diff) +
                     s(DJF_TAVG_diff) +
                     s(MAM_PRCP_diff) +
                     s(personsPERha),
                   method = "REML")


ermq_all_gam <- gam(data = huc_ermqall_data[-c(1050, 988, 1007, 1394, 
                                               816, 829, 1373),], 
                    subset = (HUC_SIZE == 10),
                    ERMQ_ALL ~ 
                      s(SiltClay) +
                      s(DJF_PRCP_diff) +
                      s(DJF_TAVG_diff) +
                      s(GROUP_A_prct) +
                      s(personsPERha),
                    method = "REML")


fecal_gam  <- gam(data = buff_fecal_data, 
                  subset = (BUFFER_SIZE == 2),
                  FecalColiform_LOG ~ 
                    s(Logger_Salin_mean) +
                    s(CHANNEL_WIDTH_log) +
                    s(MARCAT_prct) +
                    s(PRCP_2) +
                    s(UPLAND_IMPERV_PRCT) +
                    s(MIX_FOREST_prct) +
                    s(PDSI),
                  method = "REML")


entero_gam <- gam(data = buff_entero_data[-c(989, 1013, 1173, 890, 894,
                                             914, 860, 972, 1258, 1237),], 
                  subset = (BUFFER_SIZE == 3),
                  Entero_LOG ~ 
                    s(Logger_Salin_mean) +
                    s(CHANNEL_WIDTH_log) +
                    s(PRCP_2) +
                    s(DAYMET_30dTAVG_degC) +
                    s(UPLAND_IMPERV_PRCT), 
                  method = "REML")


trawlabund_gam <- gam(data = huc_trawlabund_data, 
                      subset = (HUC_SIZE == 12),
                      log(trawl_ABUNDANCE_area) ~ 
                        s(IMPERV_PRCT) +
                        s(CHANNEL_WIDTH_log) +
                        s(ANN_TAVG_diff) +
                        s(NAO),
                      method = "REML")


trawlsprichopen_gam <- gam(data = huc_trawlsprichopen_data[-c(1222),], 
                           subset = (HUC_SIZE == 12),
                           trawl_SP_RICH  ~ 
                             s(CHANNEL_WIDTH_log) +
                             s(Logger_Salin_mean) +
                             s(NAO) +
                             s(MARCAT_prct) +
                             s(DAYMET_30dPRCP_cm) +
                             s(preANN_TAVG_diff),
                           method = "REML")


trawlsprichtidal_gam <- gam(data = huc_trawlsprichtidal_data[-c(1028, 1086),], 
                            subset = (HUC_SIZE == 12),
                            trawl_SP_RICH  ~ 
                              s(Logger_Salin_mean) +
                              s(AGRCAT_prct) +
                              s(ANN_TAVG_diff) +
                              s(SP02),
                            method = "REML")


benthabund_gam <- gam(data = huc_benthabund_data[-c(1687, 1848, 1976, 1930, 2404,
                                                    2189, 2029, 2248, 2039, 1896, 
                                                    2179, 2163, 2424),], 
                      subset = (HUC_SIZE == 12),
                      log1p(benth_ABUNDANCE_AREA) ~ 
                        s(Logger_Salin_mean) +
                        s(DAYMET_90dTAVG_degC) +
                        s(SiltClay) +
                        s(DJF_TAVG_diff) +
                        s(DAYMET_45dPRCP_cm) +
                        s(MARCAT_prct),
                      method = "REML")


benthsprich_gam <- gam(data = huc_benthsprich_data, 
                       subset = (HUC_SIZE == 12),
                       log(benth_SP_RICH) ~ 
                         s(Logger_Salin_mean) +
                         s(DAYMET_90dTAVG_degC) +
                         s(DAYMET_45dPRCP_cm) +
                         s(SiltClay) +
                         s(CHANNEL_WIDTH_log) +
                         s(DEVLOWCAT_prct) +
                         s(MARCAT_prct),
                       method = "REML")


mambi_gam <- gam(data = grid_mambi_data, 
                 subset = (FISHNET_SIZE == 400),
                 MAMBI ~
                   s(Logger_Salin_mean) +
                   s(CHANNEL_WIDTH_log) +
                   s(SiltClay) +
                   s(DAYMET_90dTAVG_degC) +
                   s(DAYMET_45dPRCP_cm) +
                   s(DEVOPEN_prct) +
                   s(DEVHIGHCAT_prct),
                 method = "REML")

#Use broom's tidy and glance functions to create summarized .csv model outputs----

ermq_ddt_model_tidy <-  tidy(ermq_ddt_model) %>%
  mutate(response = "ERMQ DDT", spatialunit = "huc", size = "10")

ermq_ddt_model_glance <-  glance(ermq_ddt_model) %>%
  mutate(response = "ERMQ DDT", spatialunit = "huc", size = "10")

ermq_pah_model_tidy <-  tidy(ermq_pah_model) %>%
  mutate(response = "ERMQ PAH", spatialunit = "huc", size = "10")

ermq_pah_model_glance <-  glance(ermq_pah_model) %>%
  mutate(response = "ERMQ PAH", spatialunit = "huc", size = "10")

ermq_pcb_model_tidy <-  tidy(ermq_pcb_model) %>%
  mutate(response = "ERMQ PCB", spatialunit = "huc", size = "10")

ermq_pcb_model_glance <-  glance(ermq_pcb_model) %>%
  mutate(response = "ERMQ PCB", spatialunit = "huc", size = "10")

ermq_metal_model_tidy <-  tidy(ermq_metal_model) %>%
  mutate(response = "ERMQ Metal", spatialunit = "huc", size = "10")

ermq_metal_model_glance <-  glance(ermq_metal_model) %>%
  mutate(response = "ERMQ Metal", spatialunit = "huc", size = "10")

ermq_all_model_tidy <-  tidy(ermq_all_model) %>%
  mutate(response = "ERMQ All", spatialunit = "huc", size = "10")

ermq_all_model_glance <-  glance(ermq_all_model) %>%
  mutate(response = "ERMQ All", spatialunit = "huc", size = "10")

fecal_model_tidy <-  tidy(fecal_model) %>%
  mutate(response = "fecal", spatialunit = "buffer", size = "2")

fecal_model_glance <-  glance(fecal_model) %>%
  mutate(response = "fecal", spatialunit = "buffer", size = "2")

entero_model_tidy <-  tidy(entero_model) %>%
  mutate(response = "entero", spatialunit = "buffer", size = "3")

entero_model_glance <-  glance(entero_model) %>%
  mutate(response = "entero", spatialunit = "buffer", size = "3")

mambi_model_tidy <-  tidy(mambi_model) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "400")

mambi_model_glance <-  glance(mambi_model) %>%
  mutate(response = "mambi", spatialunit = "grid", size = "400")

benthabund_model_tidy <-  tidy(benthabund_model) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "12")

benthabund_model_glance <-  glance(benthabund_model) %>%
  mutate(response = "benthabund", spatialunit = "huc", size = "12")

benthsprich_model_tidy <-  tidy(benthsprich_model) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "12")

benthsprich_model_glance <-  glance(benthsprich_model) %>%
  mutate(response = "benthsprich", spatialunit = "huc", size = "12")

trawlabund_model_tidy <-  tidy(trawlabund_model) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "12")

trawlabund_model_glance <-  glance(trawlabund_model) %>%
  mutate(response = "trawlabund", spatialunit = "huc", size = "12")

trawlsprichopen_model_tidy <-  tidy(trawlsprichopen_model) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "12")

trawlsprichopen_model_glance <-  glance(trawlsprichopen_model) %>%
  mutate(response = "trawlsprichopen", spatialunit = "huc", size = "12")

trawlsprichtidal_model_tidy <-  tidy(trawlsprichtidal_model) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "12")

trawlsprichtidal_model_glance <-  glance(trawlsprichtidal_model) %>%
  mutate(response = "trawlsprichtidal", spatialunit = "huc", size = "12")


final_models_tidy <- rbind(ermq_ddt_model_tidy, ermq_pah_model_tidy, ermq_pcb_model_tidy,
                           ermq_metal_model_tidy, ermq_all_model_tidy, fecal_model_tidy,
                           entero_model_tidy, trawlabund_model_tidy, trawlsprichopen_model_tidy,
                           trawlsprichtidal_model_tidy, benthabund_model_tidy, benthsprich_model_tidy,
                           mambi_model_tidy)

final_models_glance <- rbind(ermq_ddt_model_glance, ermq_pah_model_glance, ermq_pcb_model_glance,
                             ermq_metal_model_glance, ermq_all_model_glance, fecal_model_glance,
                             entero_model_glance, trawlabund_model_glance, trawlsprichopen_model_glance,
                             trawlsprichtidal_model_glance, benthabund_model_glance, benthsprich_model_glance,
                             mambi_model_glance)

write.csv(final_models_tidy, "outputs/final_models_tidy.csv")
write.csv(final_models_glance, "outputs/final_models_glance.csv")

#Use Stargazer to export linear model results as tables

stargazer(ermq_all_model, ermq_ddt_model, ermq_metal_model, ermq_pah_model, ermq_pcb_model,
          digits = NA, single.row = T, type = "html", out = "outputs/stargazer_sediment.doc")

stargazer(entero_model, fecal_model,
          digits = NA, single.row = T, type = "html", out = "outputs/stargazer_bacteria.doc")

stargazer(trawlabund_model, trawlsprichopen_model, trawlsprichtidal_model,
          digits = NA, single.row = T, type = "html",  out = "outputs/stargazer_trawl.doc")

stargazer(benthabund_model, benthsprich_model, mambi_model,
          digits = NA, single.row = T, type = "html",  out = "outputs/stargazer_benthic.doc")



