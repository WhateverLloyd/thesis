#LOAD REQUIRED PACKAGES----
library(nlme)
library(tidyverse)
library(stargazer)

#IMPORT DATASETS----
scecap <- read.csv("./data/scecap_for_analysis.csv")
weather <- read.csv("./data/weather_data_for_analysis.csv")

#PREPARE DATASETS----
#SCECAP Data
#Separate SCECAP data by habitat type and summarize variables by year

scecap_ALL  <-  scecap %>% 
                group_by(YEAR) %>% 
                summarize(DO = mean(Logger_DO_mean, na.rm = T), SAL = mean(Logger_Salin_mean, na.rm = T),
                          TEMP = mean(Logger_Temp_mean, na.rm = T), PH = mean(Logger_pH_mean, na.rm = T),
                          ENTERO = mean(LOG_ENTERO, na.rm = T), FC = mean(LOG_FC, na.rm = T),
                          MET = mean(Met_Total, na.rm = T), MET_ERMQ = mean(ERMQ_met, na.rm = T),
                          PAH = mean(PAH_Total, na.rm = T), PAH_ERMQ = mean(ERMQ_PAH_tot, na.rm = T),
                          PCB = mean(PCB_Total, na.rm = T), PCB_ERMQ = mean(ERMQ_PCB_tot, na.rm = T),
                          DDT = mean(DDT_Total, na.rm = T), DDT_ERMQ = mean(ERMQ_DDT_tot, na.rm = T),
                          PBDE = mean(PBDE_Total, na.rm = T), ERMQ_ALL = mean(ERMQ_ALL, na.rm = T),
                          trawlRICH = mean(trawl_SP_RICH, na.rm = T), trawlABUND = mean(trawl_ABUNDANCE, na.rm = T),
                          benthRICH = mean(benth_SP_RICH, na.rm = T), benthABUND = mean(benth_ABUNDANCE, na.rm = T),
                          BIBI = mean(BIBI, na.rm = T), MAMBI = mean(MAMBI, na.rm = T))

scecap_TC   <-  scecap %>% 
                filter(RT_RO_Other == "TidalCreek") %>% 
                group_by(YEAR) %>% 
                summarize(DO = mean(Logger_DO_mean, na.rm = T), SAL = mean(Logger_Salin_mean),
                          TEMP = mean(Logger_Temp_mean, na.rm = T), PH = mean(Logger_pH_mean, na.rm = T),
                          ENTERO = mean(LOG_ENTERO, na.rm = T), FC = mean(LOG_FC, na.rm = T),
                          MET = mean(Met_Total, na.rm = T), MET_ERMQ = mean(ERMQ_met, na.rm = T),
                          PAH = mean(PAH_Total, na.rm = T), PAH_ERMQ = mean(ERMQ_PAH_tot, na.rm = T),
                          PCB = mean(PCB_Total, na.rm = T), PCB_ERMQ = mean(ERMQ_PCB_tot, na.rm = T),
                          DDT = mean(DDT_Total, na.rm = T), DDT_ERMQ = mean(ERMQ_DDT_tot, na.rm = T),
                          PBDE = mean(PBDE_Total, na.rm = T), ERMQ_ALL = mean(ERMQ_ALL, na.rm = T),
                          trawlRICH = mean(trawl_SP_RICH, na.rm = T), trawlABUND = mean(trawl_ABUNDANCE, na.rm = T),
                          benthRICH = mean(benth_SP_RICH, na.rm = T), benthABUND = mean(benth_ABUNDANCE, na.rm = T),
                          BIBI = mean(BIBI, na.rm = T), MAMBI = mean(MAMBI, na.rm = T))

scecap_OW   <-  scecap %>% 
                filter(RT_RO_Other == "OpenWater") %>% 
                group_by(YEAR) %>% 
                summarize(DO = mean(Logger_DO_mean, na.rm = T), SAL = mean(Logger_Salin_mean, na.rm = T),
                          TEMP = mean(Logger_Temp_mean, na.rm = T), PH = mean(Logger_pH_mean, na.rm = T),
                          ENTERO = mean(LOG_ENTERO, na.rm = T), FC = mean(LOG_FC, na.rm = T),
                          MET = mean(Met_Total, na.rm = T), MET_ERMQ = mean(ERMQ_met, na.rm = T),
                          PAH = mean(PAH_Total, na.rm = T), PAH_ERMQ = mean(ERMQ_PAH_tot, na.rm = T),
                          PCB = mean(PCB_Total, na.rm = T), PCB_ERMQ = mean(ERMQ_PCB_tot, na.rm = T),
                          DDT = mean(DDT_Total, na.rm = T), DDT_ERMQ = mean(ERMQ_DDT_tot, na.rm = T),
                          PBDE = mean(PBDE_Total, na.rm = T), ERMQ_ALL = mean(ERMQ_ALL, na.rm = T),
                          trawlRICH = mean(trawl_SP_RICH, na.rm = T), trawlABUND = mean(trawl_ABUNDANCE, na.rm = T),
                          benthRICH = mean(benth_SP_RICH, na.rm = T), benthABUND = mean(benth_ABUNDANCE, na.rm = T),
                          BIBI = mean(BIBI, na.rm = T), MAMBI = mean(MAMBI, na.rm = T))

#Weathter Data
#separate weather data by station

weather_bro <- weather %>% filter(STATION == "BROOKGREEN")
weather_cha <- weather %>% filter(STATION == "CHARLESTON")
weather_sav <- weather %>% filter(STATION == "SAVANNAH")
weather_all <- weather %>% filter(STATION == "ALL_STATIONS")

#REGRESSION ANALYSIS----
#Run models with NLME's GLS regression formula (RESPONSE ~ YEAR)
#with the autocorrelation structure: "correlation = corAR1(form = ~ YEAR)"

#SCECAP DATA----

#Open Water Sites

ow_DO_gls           <- gls(DO ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_SAL_gls          <- gls(SAL ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_TEMP_gls         <- gls(TEMP ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PH_gls           <- gls(PH ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_ENTERO_gls       <- gls(ENTERO ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_FC_gls           <- gls(FC ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_MET_gls          <- gls(MET ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_METERMQ_gls      <- gls(MET_ERMQ ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PAH_gls          <- gls(PAH ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PAHERMQ_gls      <- gls(PAH_ERMQ ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PCB_gls          <- gls(PCB ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PCBERMQ_gls      <- gls(PCB_ERMQ ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_DDT_gls          <- gls(DDT ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_DDTERMQ_gls      <- gls(DDT_ERMQ ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_PBDE_gls         <- gls(PBDE ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_ERMQALL_gls      <- gls(ERMQ_ALL ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_TRAWLRICH_gls    <- gls(trawlRICH ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_TRAWLABUND_gls   <- gls(trawlABUND ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_BENTHRICH_gls    <- gls(benthRICH ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_BENTHABUND_gls   <- gls(benthABUND ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_BIBI_gls         <- gls(BIBI ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
ow_MAMBI_gls        <- gls(MAMBI ~ YEAR, data = scecap_OW, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#Tidal Creek Sites

tc_DO_gls           <- gls(DO ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_SAL_gls          <- gls(SAL ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_TEMP_gls         <- gls(TEMP ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PH_gls           <- gls(PH ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_ENTERO_gls       <- gls(ENTERO ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_FC_gls           <- gls(FC ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_MET_gls          <- gls(MET ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_METERMQ_gls      <- gls(MET_ERMQ ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PAH_gls          <- gls(PAH ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PAHERMQ_gls      <- gls(PAH_ERMQ ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PCB_gls          <- gls(PCB ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PCBERMQ_gls      <- gls(PCB_ERMQ ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_DDT_gls          <- gls(DDT ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_DDTERMQ_gls      <- gls(DDT_ERMQ ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_PBDE_gls         <- gls(PBDE ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_ERMQALL_gls      <- gls(ERMQ_ALL ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_TRAWLRICH_gls    <- gls(trawlRICH ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_TRAWLABUND_gls   <- gls(trawlABUND ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_BENTHRICH_gls    <- gls(benthRICH ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_BENTHABUND_gls   <- gls(benthABUND ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_BIBI_gls         <- gls(BIBI ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
tc_MAMBI_gls        <- gls(MAMBI ~ YEAR, data = scecap_TC, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#All Sites

all_DO_gls           <- gls(DO ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_SAL_gls          <- gls(SAL ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_TEMP_gls         <- gls(TEMP ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PH_gls           <- gls(PH ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_ENTERO_gls       <- gls(ENTERO ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_FC_gls           <- gls(FC ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_MET_gls          <- gls(MET ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_METERMQ_gls      <- gls(MET_ERMQ ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PAH_gls          <- gls(PAH ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PAHERMQ_gls      <- gls(PAH_ERMQ ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PCB_gls          <- gls(PCB ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PCBERMQ_gls      <- gls(PCB_ERMQ ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_DDT_gls          <- gls(DDT ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_DDTERMQ_gls      <- gls(DDT_ERMQ ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_PBDE_gls         <- gls(PBDE ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_ERMQALL_gls      <- gls(ERMQ_ALL ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_TRAWLRICH_gls    <- gls(trawlRICH ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_TRAWLABUND_gls   <- gls(trawlABUND ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_BENTHRICH_gls    <- gls(benthRICH ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_BENTHABUND_gls   <- gls(benthABUND ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_BIBI_gls         <- gls(BIBI ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
all_MAMBI_gls        <- gls(MAMBI ~ YEAR, data = scecap_ALL, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#EXPORT RESULTS

stargazer(tc_TEMP_gls, tc_DO_gls, 
          tc_SAL_gls, tc_PH_gls,
          tc_FC_gls, tc_ENTERO_gls,
          type = "text", dep.var.caption  = "Water Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_waterquality_gls_results.txt")

stargazer(tc_TEMP_gls, tc_DO_gls, 
          tc_SAL_gls, tc_PH_gls,
          tc_FC_gls, tc_ENTERO_gls,
          type = "html", dep.var.caption  = "Water Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_waterquality_gls_results.html")

stargazer(tc_MET_gls, tc_METERMQ_gls, 
          tc_PAH_gls, tc_PAHERMQ_gls,
          tc_PCB_gls, tc_PCBERMQ_gls,
          tc_DDT_gls, tc_DDTERMQ_gls,
          tc_PBDE_gls, tc_ERMQALL_gls,
          type = "text", dep.var.caption  = "Sediment Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_sediment_gls_results.txt")

stargazer(tc_MET_gls, tc_METERMQ_gls, 
          tc_PAH_gls, tc_PAHERMQ_gls,
          tc_PCB_gls, tc_PCBERMQ_gls,
          tc_DDT_gls, tc_DDTERMQ_gls,
          tc_PBDE_gls, tc_ERMQALL_gls,
          type = "html", dep.var.caption  = "Sediment Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_sediment_gls_results.html")

stargazer(tc_TRAWLRICH_gls, tc_TRAWLABUND_gls,
          tc_BENTHRICH_gls, tc_BENTHABUND_gls,
          tc_BIBI_gls, tc_MAMBI_gls,
          type = "text", dep.var.caption  = "Biological Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_biological_gls_results.txt")

stargazer(tc_TRAWLRICH_gls, tc_TRAWLABUND_gls,
          tc_BENTHRICH_gls, tc_BENTHABUND_gls,
          tc_BIBI_gls, tc_MAMBI_gls,
          type = "html", dep.var.caption  = "Biological Quality (Tidal Creek Sites)",
          out = "./outputs/tidalcreek_biological_gls_results.html")

stargazer(ow_TEMP_gls, ow_DO_gls, 
          ow_SAL_gls, ow_PH_gls,
          ow_FC_gls, ow_ENTERO_gls,
          type = "text", dep.var.caption  = "Water Quality (Open Water Sites)",
          out = "./outputs/openwater_waterquality_gls_results.txt")

stargazer(ow_TEMP_gls, ow_DO_gls, 
          ow_SAL_gls, ow_PH_gls,
          ow_FC_gls, ow_ENTERO_gls,
          type = "html", dep.var.caption  = "Water Quality (Open Water Sites)",
          out = "./outputs/openwater_waterquality_gls_results.html")

stargazer(ow_MET_gls, ow_METERMQ_gls, 
          ow_PAH_gls, ow_PAHERMQ_gls,
          ow_PCB_gls, ow_PCBERMQ_gls,
          ow_DDT_gls, ow_DDTERMQ_gls,
          ow_PBDE_gls, ow_ERMQALL_gls,
          type = "text", dep.var.caption  = "Sediment Quality (Open Water Sites)",
          out = "./outputs/openwater_sediment_gls_results.txt")

stargazer(ow_MET_gls, ow_METERMQ_gls, 
          ow_PAH_gls, ow_PAHERMQ_gls,
          ow_PCB_gls, ow_PCBERMQ_gls,
          ow_DDT_gls, ow_DDTERMQ_gls,
          ow_PBDE_gls, ow_ERMQALL_gls,
          type = "html", dep.var.caption  = "Sediment Quality (Open Water Sites)",
          out = "./outputs/openwater_sediment_gls_results.html")

stargazer(ow_TRAWLRICH_gls, ow_TRAWLABUND_gls,
          ow_BENTHRICH_gls, ow_BENTHABUND_gls,
          ow_BIBI_gls, ow_MAMBI_gls,
          type = "text", dep.var.caption  = "Biological Quality (Open Water Sites)",
          out = "./outputs/openwater_biological_gls_results.txt")

stargazer(ow_TRAWLRICH_gls, ow_TRAWLABUND_gls,
          ow_BENTHRICH_gls, ow_BENTHABUND_gls,
          ow_BIBI_gls, ow_MAMBI_gls,
          type = "html", dep.var.caption  = "Biological Quality (Open Water Sites)",
          out = "./outputs/openwater_biological_gls_results.html")

stargazer(all_TEMP_gls, all_DO_gls, 
          all_SAL_gls, all_PH_gls,
          all_FC_gls, all_ENTERO_gls,
          type = "text", dep.var.caption  = "Water Quality (All Sites)",
          out = "./outputs/allsites_waterquality_gls_results.txt")

stargazer(all_TEMP_gls, all_DO_gls, 
          all_SAL_gls, all_PH_gls,
          all_FC_gls, all_ENTERO_gls,
          type = "html", dep.var.caption  = "Water Quality (All Sites)",
          out = "./outputs/allsites_waterquality_gls_results.html")

stargazer(all_MET_gls, all_METERMQ_gls, 
          all_PAH_gls, all_PAHERMQ_gls,
          all_PCB_gls, all_PCBERMQ_gls,
          all_DDT_gls, all_DDTERMQ_gls,
          all_PBDE_gls, all_ERMQALL_gls,
          type = "text", dep.var.caption  = "Sediment Quality (All Sites)",
          out = "./outputs/allsites_sediment_gls_results.text")

stargazer(all_MET_gls, all_METERMQ_gls, 
          all_PAH_gls, all_PAHERMQ_gls,
          all_PCB_gls, all_PCBERMQ_gls,
          all_DDT_gls, all_DDTERMQ_gls,
          all_PBDE_gls, all_ERMQALL_gls,
          type = "html", dep.var.caption  = "Sediment Quality (All Sites)",
          out = "./outputs/allsites_sediment_gls_results.html")

stargazer(all_TRAWLRICH_gls, all_TRAWLABUND_gls,
          all_BENTHRICH_gls, all_BENTHABUND_gls,
          all_BIBI_gls, all_MAMBI_gls,
          type = "text", dep.var.caption  = "Biological Quality (All Sites)",
          out = "./outputs/allsites_biological_gls_results.txt")

stargazer(all_TRAWLRICH_gls, all_TRAWLABUND_gls,
          all_BENTHRICH_gls, all_BENTHABUND_gls,
          all_BIBI_gls, all_MAMBI_gls,
          type = "html", dep.var.caption  = "Biological Quality (All Sites)",
          out = "./outputs/allsites_biological_gls_results.html")

#WEATHER DATA----

#Brookgreen Gardens

anntavg_bro_gls <- gls(ANN_TAVG_degC ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djftavg_bro_gls <- gls(DJF_TAVG_degC ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamtavg_bro_gls <- gls(MAM_TAVG_degC ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjatavg_bro_gls <- gls(JJA_TAVG_degC ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sontavg_bro_gls <- gls(SON_TAVG_degC ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
exttmax_bro_gls <- gls(DAYS_95prctl_TMAX ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
annprcp_bro_gls <- gls(ANN_PRCP_cm ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djfprcp_bro_gls <- gls(DJF_PRCP_cm ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamprcp_bro_gls <- gls(MAM_PRCP_cm ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjaprcp_bro_gls <- gls(JJA_PRCP_cm ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sonprcp_bro_gls <- gls(SON_PRCP_cm ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
extprcp_bro_gls <- gls(DAYS_95prctl_PRCP ~ YEAR, data = weather_bro, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#Charleston

anntavg_cha_gls <- gls(ANN_TAVG_degC ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djftavg_cha_gls <- gls(DJF_TAVG_degC ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamtavg_cha_gls <- gls(MAM_TAVG_degC ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjatavg_cha_gls <- gls(JJA_TAVG_degC ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sontavg_cha_gls <- gls(SON_TAVG_degC ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
exttmax_cha_gls <- gls(DAYS_95prctl_TMAX ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
annprcp_cha_gls <- gls(ANN_PRCP_cm ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djfprcp_cha_gls <- gls(DJF_PRCP_cm ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamprcp_cha_gls <- gls(MAM_PRCP_cm ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjaprcp_cha_gls <- gls(JJA_PRCP_cm ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sonprcp_cha_gls <- gls(SON_PRCP_cm ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
extprcp_cha_gls <- gls(DAYS_95prctl_PRCP ~ YEAR, data = weather_cha, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#Savannah

anntavg_sav_gls <- gls(ANN_TAVG_degC ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djftavg_sav_gls <- gls(DJF_TAVG_degC ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamtavg_sav_gls <- gls(MAM_TAVG_degC ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjatavg_sav_gls <- gls(JJA_TAVG_degC ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sontavg_sav_gls <- gls(SON_TAVG_degC ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
exttmax_sav_gls <- gls(DAYS_95prctl_TMAX ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
annprcp_sav_gls <- gls(ANN_PRCP_cm ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djfprcp_sav_gls <- gls(DJF_PRCP_cm ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamprcp_sav_gls <- gls(MAM_PRCP_cm ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjaprcp_sav_gls <- gls(JJA_PRCP_cm ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sonprcp_sav_gls <- gls(SON_PRCP_cm ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
extprcp_sav_gls <- gls(DAYS_95prctl_PRCP ~ YEAR, data = weather_sav, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#All Stations

anntavg_all_gls <- gls(ANN_TAVG_degC ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djftavg_all_gls <- gls(DJF_TAVG_degC ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamtavg_all_gls <- gls(MAM_TAVG_degC ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjatavg_all_gls <- gls(JJA_TAVG_degC ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sontavg_all_gls <- gls(SON_TAVG_degC ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
exttmax_all_gls <- gls(DAYS_95prctl_TMAX ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
annprcp_all_gls <- gls(ANN_PRCP_cm ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
djfprcp_all_gls <- gls(DJF_PRCP_cm ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
mamprcp_all_gls <- gls(MAM_PRCP_cm ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
jjaprcp_all_gls <- gls(JJA_PRCP_cm ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
sonprcp_all_gls <- gls(SON_PRCP_cm ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)
extprcp_all_gls <- gls(DAYS_95prctl_PRCP ~ YEAR, data = weather_all, correlation = corAR1(form = ~ YEAR), na.action = na.omit)

#Export GLS Results using stargazer

stargazer(anntavg_bro_gls, djftavg_bro_gls, mamtavg_bro_gls,
          jjatavg_bro_gls, sontavg_bro_gls, exttmax_bro_gls,
          type = "text", dep.var.caption = "Brookgreen Gardens Temperature",
          out = "./outputs/brookgreentemp_gls_results.txt")

stargazer(anntavg_bro_gls, djftavg_bro_gls, mamtavg_bro_gls,
          jjatavg_bro_gls, sontavg_bro_gls, exttmax_bro_gls,
          type = "html", dep.var.caption = "Brookgreen Gardens Temperature",
          out = "./outputs/brookgreentemp_gls_results.html")

stargazer(annprcp_bro_gls, djfprcp_bro_gls, mamprcp_bro_gls,
          jjaprcp_bro_gls, sonprcp_bro_gls, extprcp_bro_gls,
          type = "text", dep.var.caption = "Brookgreen Gardens Precipitation",
          out = "./outputs/brookgreenprcp_gls_results.txt")

stargazer(annprcp_bro_gls, djfprcp_bro_gls, mamprcp_bro_gls,
          jjaprcp_bro_gls, sonprcp_bro_gls, extprcp_bro_gls,
          type = "html", dep.var.caption = "Brookgreen Gardens Precipitation",
          out = "./outputs/brookgreenprcp_gls_results.html")

stargazer(anntavg_cha_gls, djftavg_cha_gls, mamtavg_cha_gls,
          jjatavg_cha_gls, sontavg_cha_gls, exttmax_cha_gls,
          type = "text", dep.var.caption = "Charleston Int'l Airport Temperature",
          out = "./outputs/charlestontemp_gls_results.txt")

stargazer(anntavg_cha_gls, djftavg_cha_gls, mamtavg_cha_gls,
          jjatavg_cha_gls, sontavg_cha_gls, exttmax_cha_gls,
          type = "html", dep.var.caption = "Charleston Int'l Airport Temperature",
          out = "./outputs/charlestontemp_gls_results.html")

stargazer(annprcp_cha_gls, djfprcp_cha_gls, mamprcp_cha_gls,
          jjaprcp_cha_gls, sonprcp_cha_gls, extprcp_cha_gls,
          type = "text", dep.var.caption = "Charleston Int'l Airport Precipitation",
          out = "./outputs/charlestonprcp_gls_results.txt")

stargazer(annprcp_cha_gls, djfprcp_cha_gls, mamprcp_cha_gls,
          jjaprcp_cha_gls, sonprcp_cha_gls, extprcp_cha_gls,
          type = "html", dep.var.caption = "Charleston Int'l Airport Precipitation",
          out = "./outputs/charlestonprcp_gls_results.html")

stargazer(anntavg_sav_gls, djftavg_sav_gls, mamtavg_sav_gls,
          jjatavg_sav_gls, sontavg_sav_gls, exttmax_sav_gls,
          type = "text", dep.var.caption = "Savannah Int'l Airport Temperature",
          out = "./outputs/savannahtemp_gls_results.txt")

stargazer(anntavg_sav_gls, djftavg_sav_gls, mamtavg_sav_gls,
          jjatavg_sav_gls, sontavg_sav_gls, exttmax_sav_gls,
          type = "html", dep.var.caption = "Savannah Int'l Airport Temperature",
          out = "./outputs/savannahtemp_gls_results.html")

stargazer(annprcp_sav_gls, djfprcp_sav_gls, mamprcp_sav_gls,
          jjaprcp_sav_gls, sonprcp_sav_gls, extprcp_sav_gls,
          type = "text", dep.var.caption = "Savannah Int'l Airport Precipitation",
          out = "./outputs/savannahprcp_gls_results.txt")

stargazer(annprcp_sav_gls, djfprcp_sav_gls, mamprcp_sav_gls,
          jjaprcp_sav_gls, sonprcp_sav_gls, extprcp_sav_gls,
          type = "html", dep.var.caption = "Savannah Int'l Airport Precipitation",
          out = "./outputs/savannahprcp_gls_results.html")

stargazer(anntavg_all_gls, djftavg_all_gls, mamtavg_all_gls,
          jjatavg_all_gls, sontavg_all_gls, exttmax_all_gls,
          type = "text", dep.var.caption = "Coastal South Carolina Temperature (all 3 stations)",
          out = "./outputs/allstationstemp_gls_results.txt")

stargazer(anntavg_all_gls, djftavg_all_gls, mamtavg_all_gls,
          jjatavg_all_gls, sontavg_all_gls, exttmax_all_gls,
          type = "html", dep.var.caption = "Coastal South Carolina Temperature (all 3 stations)",
          out = "./outputs/allstationstemp_gls_results.html")

stargazer(annprcp_all_gls, djfprcp_all_gls, mamprcp_all_gls,
          jjaprcp_all_gls, sonprcp_all_gls, extprcp_all_gls,
          type = "text", dep.var.caption = "Coastal South Carolina Precipitation (all 3 stations)",
          out = "./outputs/allstationsprcp_gls_results.txt")

stargazer(annprcp_all_gls, djfprcp_all_gls, mamprcp_all_gls,
          jjaprcp_all_gls, sonprcp_all_gls, extprcp_all_gls,
          type = "html", dep.var.caption = "Coastal South Carolina Precipitation (all 3 stations)",
          out = "./outputs/allstationsprcp_gls_results.html")



          