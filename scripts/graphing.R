#LOAD REQUIRED PACKAGES----
library(tidyverse)
library(ggpubr)
library(scales)

#IMPORT DATASETS----
weather <- read.csv("./data/weather_data_for_analysis.csv")
scecap_for_analysis <- read.csv("./data/scecap_for_analysis.csv")
scecap_hucs <- read.csv("./data/scecap_hucs.csv")
county_population <- read.csv("./data/county_population.csv")
landcover_counties <- read.csv("./data/landcover_counties.csv")
gain_lost <- read.csv("./data/gain_lost_landcover.csv")
gain_lost2 <- read.csv("./data/change_county.csv")
change_nochange <- read.csv("./data/change_nochange.csv")

#PREPARE DATASETS----
weather_3stations <- weather %>% filter(STATION != "ALL_STATIONS")
weather_allstations <- weather %>% filter(STATION == "ALL_STATIONS")

scecap_ALL <- scecap_for_analysis %>% 
  group_by(YEAR) %>% 
  summarize(DO = mean(Logger_DO_mean, na.rm = T), SAL = mean(Logger_Salin_mean, na.rm = T),
            TEMP = mean(Logger_Temp_mean, na.rm = T),PH = mean(Logger_pH_mean, na.rm = T),
            ENTERO = mean(LOG_ENTERO, na.rm = T),FC = mean(LOG_FC, na.rm = T),
            MET = mean(Met_Total, na.rm = T),MET_ERMQ = mean(ERMQ_met, na.rm = T),
            PAH = mean(PAH_Total, na.rm = T),PAH_ERMQ = mean(ERMQ_PAH_tot, na.rm = T),
            PCB = mean(PCB_Total, na.rm = T),PCB_ERMQ = mean(ERMQ_PCB_tot, na.rm = T),
            DDT = mean(DDT_Total, na.rm = T),DDT_ERMQ = mean(ERMQ_DDT_tot, na.rm = T),
            PBDE = mean(PBDE_Total, na.rm = T),ERMQ_ALL = mean(ERMQ_ALL, na.rm = T),
            trawlRICH = mean(trawl_SP_RICH, na.rm = T),trawlABUND = mean(trawl_ABUNDANCE, na.rm = T),
            benthRICH = mean(benth_SP_RICH, na.rm = T),benthABUND = mean(benth_ABUNDANCE, na.rm = T),
            BIBI = mean(BIBI, na.rm = T),MAMBI = mean(MAMBI, na.rm = T))


scecap_RTRO <- scecap_for_analysis %>% 
  group_by(YEAR, RT_RO_Other) %>% 
  summarize(DO = mean(Logger_DO_mean, na.rm = T), SAL = mean(Logger_Salin_mean, na.rm = T),
            TEMP = mean(Logger_Temp_mean, na.rm = T),PH = mean(Logger_pH_mean, na.rm = T),
            ENTERO = mean(LOG_ENTERO, na.rm = T),FC = mean(LOG_FC, na.rm = T),
            MET = mean(Met_Total, na.rm = T),MET_ERMQ = mean(ERMQ_met, na.rm = T),
            PAH = mean(PAH_Total, na.rm = T),PAH_ERMQ = mean(ERMQ_PAH_tot, na.rm = T),
            PCB = mean(PCB_Total, na.rm = T),PCB_ERMQ = mean(ERMQ_PCB_tot, na.rm = T),
            DDT = mean(DDT_Total, na.rm = T),DDT_ERMQ = mean(ERMQ_DDT_tot, na.rm = T),
            PBDE = mean(PBDE_Total, na.rm = T),ERMQ_ALL = mean(ERMQ_ALL, na.rm = T),
            trawlRICH = mean(trawl_SP_RICH, na.rm = T),trawlABUND = mean(trawl_ABUNDANCE, na.rm = T),
            benthRICH = mean(benth_SP_RICH, na.rm = T),benthABUND = mean(benth_ABUNDANCE, na.rm = T),
            BIBI = mean(BIBI, na.rm = T),MAMBI = mean(MAMBI, na.rm = T))


county_population <- county_population %>% filter(COUNTY != "SC_COASTAL")
county_population$COUNTY2 <- county_population$COUNTY


landcover_counties2 <- landcover_counties %>% filter(COUNTY != "SC_COASTAL")
landcover_counties2$COUNTY2 <- landcover_counties2$COUNTY

landcover_long <- landcover_counties %>% select(COUNTY, YEAR, Developed, Agriculture, Forested, Marsh, Water) %>%
  gather(key = "LandcoverCategory", value = "Percent", Developed, Agriculture, Forested, Marsh, Water)

landcover_long_1 <- landcover_long %>% filter(COUNTY != "SC_COASTAL")

gain_lost$Landcover <- fct_rev(factor(gain_lost$Landcover, levels = c("High Intensity Developed",
                                                              "Low Intensity Developed",
                                                              "Agriculture",
                                                              "Forested Upland",
                                                              "Forested Wetland",
                                                              "Marsh",
                                                              "Water")))

gain_lost2$Landcover <- fct_rev(factor(gain_lost2$Landcover, levels = c("High Intensity Developed",
                                                                "Low Intensity Developed",
                                                                "Agriculture",
                                                                "Forested Upland",
                                                                "Forested Wetland",
                                                                "Marsh",
                                                                "Water")))



change_nochange$County <- factor(change_nochange$County, levels = c("Berkeley",
                                                                    "Horry",
                                                                    "Colleton",
                                                                    "Charleston",
                                                                    "Georgetown",
                                                                    "Jasper",
                                                                    "Beaufort",
                                                                    "Dorchester"))


#1. TIME SERIES PLOTS----
#1.1. Weather----
#1.1.1. Seasonal Temperature----
#1.1.1.1. Seasonal Temperature Combined Stations----

djf_tavg2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = DJF_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Average Winter Temperature (°C)", x = "")

mam_tavg2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = MAM_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Average Spring Temperature (°C)", x = "")

jja_tavg2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = JJA_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Average Summer Temperature (°C)", x = "")

son_tavg2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = SON_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Average Fall Temperature (°C)", x = "Year")

seasonal_temperature <- ggarrange(djf_tavg2, mam_tavg2, 
                                  jja_tavg2, son_tavg2, 
                                  nrow = 2, ncol =2, align = "hv") %>%
                        annotate_figure(top = text_grob("Seasonal Temperature Data for Coastal South Carolina (1999-2018)",
                                  face = "bold", size = 14))
#1.1.1.2. Seasonal Temperature Separate Stations----

facet_djf_tavg <- ggplot(data = weather_3stations, aes(x = YEAR, y = DJF_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DJF_TAVG_degC, linetype = STATION)) +
  labs(y = "Average Winter Temperature (°C)", x = "") +
  scale_linetype_manual(values=c(3, 3, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_mam_tavg <- ggplot(data = weather_3stations, aes(x = YEAR, y = MAM_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = MAM_TAVG_degC, linetype = STATION)) +
  labs(y = "Average Spring Temperature (°C)", x = "") +
  scale_linetype_manual(values=c(3, 3, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_jja_tavg <- ggplot(data = weather_3stations, aes(x = YEAR, y = JJA_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = JJA_TAVG_degC, linetype = STATION)) +
  labs(y = "Average Summer Temperature (°C)", x = "") +
  scale_linetype_manual(values=c(3, 1, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_son_tavg <- ggplot(data = weather_3stations, aes(x = YEAR, y = SON_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = SON_TAVG_degC, linetype = STATION)) +
  labs(y = "Average Fall Temperature (°C)", x = "Year") +
  scale_linetype_manual(values=c(3, 1, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

seasonal_temperature_faceted <- ggarrange(facet_djf_tavg,facet_mam_tavg,
                                      facet_jja_tavg, facet_son_tavg, 
                                      nrow = 2, ncol =2, align = "hv") %>%
  annotate_figure(top = text_grob("Seasonal Temperature Data by Weather Station (1999-2018)", face = "bold", size = 14))

#1.1.2. Sesaonal Precipitation----
#1.1.2.1. Seasonal Precipitation Combined Stations----

djf_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = DJF_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Total Winter Precipitation (cm)", x = "")

mam_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = MAM_PRCP_cm)) +
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Total Spring Precipitation (cm)", x = "")

jja_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = JJA_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Total Summer Precipitation (cm)", x = "")

son_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = SON_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Total Fall Precipitation (cm)", x = "Year")

seasonal_precip <- ggarrange(djf_prcp2, mam_prcp2,
                              jja_prcp2, son_prcp2,
                              nrow = 2, ncol =2, align = "hv") %>%
                    annotate_figure(top = text_grob("Seasonal Precipitation Data for Coastal South Carolina (1999-2018)",
                                  face = "bold", size = 14))

#1.1.2.2. Sesaonal Precipitation Separate Stations----

facet_djf_prcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = DJF_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DJF_PRCP_cm, linetype = STATION)) +
  labs(y = "Total Winter Precipitation (cm)", x = "") +
  scale_linetype_manual(values=c(3, 3, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_mam_prcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = MAM_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = MAM_PRCP_cm, linetype = STATION)) +
  labs(y = "Total Spring Precipitation (cm)", x = "") +
  scale_linetype_manual(values=c(3, 3, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_jja_prcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = JJA_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = JJA_PRCP_cm, linetype = STATION)) +
  labs(y = "Total Summer Precipitation (cm)", x = "") +
  scale_linetype_manual(values=c(3, 3, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_son_prcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = SON_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = SON_PRCP_cm, linetype = STATION)) +
  labs(y = "Total Fall Precipitation (cm)", x = "Year") +
  scale_linetype_manual(values=c(3, 3, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

seasonal_precip_faceted <- ggarrange(facet_djf_prcp, facet_mam_prcp,
                                      facet_jja_prcp,facet_son_prcp,
                                      nrow = 2, ncol =2, align = "hv") %>%
  annotate_figure(top = text_grob("Seasonal Precipitation Data by Weather Station (1999-2018)", face = "bold", size = 14))

#1.1.3. Annual Temperature and Precipitation----
#1.1.3.1. Annual Temperature and Precipitation Combined Stations----

ann_tavg2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = ANN_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "Average Annual Temperature (°C)", x = "Year")

ann_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = ANN_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Total Annual Precipitation (cm)", x = "Year")

annual_climate <- ggarrange(ann_tavg2, ann_prcp2,
                            nrow = 1, ncol = 2,
                            align = "hv") %>%
  annotate_figure(top = text_grob("Annual Climate Data for Coastal South Carolina (1999-2018)",
                                  face = "bold", size = 14))
#1.1.3.2. Annual Temperature and Precipitation Separate Stations----

facet_ann_tavg <- ggplot(data = weather_3stations, aes(x = YEAR, y = ANN_TAVG_degC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = ANN_TAVG_degC, linetype = STATION)) +
  labs(y = "Average Annual Temperature (°C)", x = "Year") +
  scale_linetype_manual(values=c(3, 1, 1)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_ann_prcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = ANN_PRCP_cm)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = ANN_PRCP_cm, linetype = STATION)) +
  labs(y = "Total Annual Precipitation (cm)", x = "Year") +
  scale_linetype_manual(values=c(3, 1, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

annual_climate_faceted <- ggarrange(facet_ann_tavg, facet_ann_prcp,
                                    nrow = 1, ncol=2, align = "hv") %>%
  annotate_figure(top = text_grob("Annual Climate Data by Weather Station (1999-2018)", face = "bold", size = 14))

#1.1.4. Extreme Temperature and Precipitation----
#1.1.4.1. Extreme Temperature and Precipitation Combined Stations----

extreme_tmax2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = DAYS_95prctl_TMAX)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Days of Extreme Temperature", x = "Year")

extreme_prcp2 <- ggplot(data = weather_allstations, aes(x = YEAR, y = DAYS_95prctl_PRCP)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "Days of Extreme Precipitation", x = "Year")

extreme_climate <- ggarrange(extreme_tmax2, extreme_prcp2,
                             nrow = 1, ncol = 2,
                             align = "hv") %>%
  annotate_figure(top = text_grob("Annual Extreme Temperature and Precipitation Events for Coastal South Carolina (1999-2018)",
                                  face = "bold", size = 14))

#1.1.4.2. Extreme Temperature and Precipitation Separate Stations----

facet_extremetemp <- ggplot(data = weather_3stations, aes(x = YEAR, y = DAYS_95prctl_TMAX)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DAYS_95prctl_TMAX, linetype = STATION)) +
  labs(y = "Days of Extreme Temperature", x = "Year") +
  scale_linetype_manual(values=c(3, 3, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

facet_extremeprcp <- ggplot(data = weather_3stations, aes(x = YEAR, y = DAYS_95prctl_PRCP)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DAYS_95prctl_PRCP, linetype = STATION)) +
  labs(y = "Days of Extreme Precipitation", x = "Year") +
  scale_linetype_manual(values=c(3, 1, 3)) +
  theme(legend.position = "none") +
  facet_wrap(~STATION, dir = "v")

extreme_climate_faceted <- ggarrange(facet_extremetemp, facet_extremeprcp,
                                     nrow = 1, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("Annual Extreme Temperature and Precipitation Events by Weather Station (1999-2018)", face = "bold", size = 14))



#1.2. Environmental----
#1.2.1. Water Quality----
#1.2.1.1. Water Quality Combined Habitats----

wqplot_DO2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = DO)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Dissolved Oxygen (mg/L)", x = "")

wqplot_SAL2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = SAL)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Salinity (ppt)", x = "")

wqplot_TEMP2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = TEMP)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Water Temperature (°C)", x = "")

wqplot_PH2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "pH", x = "")

wqplot_ENTERO2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = ENTERO)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Enterococcus (log MPN/100mL)", x = "Year")

wqplot_FC2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = FC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "Year")


wqplots2 <- ggarrange(wqplot_TEMP2, wqplot_DO2, wqplot_SAL2, 
                      wqplot_PH2, wqplot_FC2, wqplot_ENTERO2, 
                      nrow = 3, ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Water Quality Data (1999 to 2018)", face = "bold", size = 14))

#1.2.1.2. Water Quality Separate Habitats----

TEMP_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = TEMP)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = TEMP, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Water Temperature (°C)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

DO_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = DO)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DO, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

SAL_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = SAL)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = SAL, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Salinity (ppt)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

PH_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3,
              inherit.aes = F, aes(x = YEAR, y = PH, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "pH", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

FC_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = FC)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = FC, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ENTERO_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = ENTERO)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = ENTERO, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Enterococcus (log MPN/100mL)", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

facet_waterquality <- ggarrange(TEMP_facet, DO_facet,
                                SAL_facet, PH_facet,
                                FC_facet, ENTERO_facet,
                                nrow = 3, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data (1999-2018)", face = "bold", size = 14))

#1.2.2. Sediment Quality----
#1.2.2.1. Sediment Quality Combined Habitats----

sedplot_met2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = MET)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Metals (ug/g)", x = "")

sedplot_metermq2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = MET_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Metals (ERMQ)", x = "")

sedplot_pah2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PAH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "PAHs (ng/g)", x = "")

sedplot_pahermq2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PAH_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "PAHs (ERMQ)", x = "")

sedplot_pcb2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PCB)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "PCBs (ng/g)", x = "")

sedplot_pcbermq2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PCB_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "PCBs (ERMQ)", x = "")

sedplot_ddt2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = DDT)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "DDT (ng/g)", x = "")

sedplot_ddtermq2<- ggplot(data = scecap_ALL, aes(x = YEAR, y = DDT_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "DDT (ERMQ)", x = "")

sedplot_pbde2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = PBDE)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "PBDEs (ng/g)", x = "Year")

sedplot_ermqall2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = ERMQ_ALL)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "All Contaminants (ERMQ)", x = "Year")

sedplots2 <- ggarrange(sedplot_met2, sedplot_metermq2, sedplot_pah2, 
                       sedplot_pahermq2, sedplot_pcb2, sedplot_pcbermq2,
                       sedplot_ddt2, sedplot_ddtermq2, sedplot_pbde2, sedplot_ermqall2,
                       ncol = 2, nrow = 5, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data (1999 to 2018)", 
                                  face = "bold", size = 14))


#1.2.2.2. Sediment Quality Separate Habitats----

MET_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = MET)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = MET, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Metals (ug/g)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ERMQMET_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = MET_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = MET_ERMQ, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Metals (ERMQ)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

PAH_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PAH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = PAH, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "PAHs (ng/g)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ERMQPAH_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PAH_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3,
              inherit.aes = F, aes(x = YEAR, y = PAH_ERMQ, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "PAHs (ERMQ)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

PCB_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PCB)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = PCB, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "PCBs (ng/g)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ERMQPCB_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PCB_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = PCB_ERMQ, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "PCBs (ERMQ)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

DDT_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = DDT)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DDT, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "DDT (ng/g)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ERMQDDT_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = DDT_ERMQ)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = DDT_ERMQ, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "DDT (ERMQ)", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

PBDE_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = PBDE)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = PBDE, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "PBDEs (ng/g)", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

ERMQALL_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = ERMQ_ALL)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = ERMQ_ALL, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "All Contaminants (ERMQ)", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

facet_sediment <- ggarrange(MET_facet, ERMQMET_facet,
                            PAH_facet, ERMQPAH_facet,
                            PCB_facet, ERMQPCB_facet,
                            DDT_facet, ERMQDDT_facet,
                            PBDE_facet, ERMQALL_facet,
                            nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data (1999-2018)", face = "bold", size = 14))

#1.2.3. Biological Quality----
#1.2.3.1. Biological Quality Combined Habitats----

bioplot_trawlrich2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = trawlRICH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "Trawl Species Richness", x = "")

bioplot_trawlabun2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = trawlABUND)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "Trawl Total Abundance", x = "")

bioplot_benthrich2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = benthRICH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "Benthic Species Richness", x = "")

bioplot_benthabun2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = benthABUND)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "Benthic Total Abundance", x = "")

bioplot_bibi2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = BIBI)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 3) +
  labs(y = "BIBI", x = "Year")

bioplot_mambi2 <- ggplot(data = scecap_ALL, aes(x = YEAR, y = MAMBI)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, linetype = 1) +
  labs(y = "MAMBI", x = "Year")

bioplots2 <- ggarrange(bioplot_trawlrich2, bioplot_trawlabun2,
                       bioplot_benthrich2, bioplot_benthabun2,
                       bioplot_bibi2, bioplot_mambi2, nrow = 3, ncol = 2, align = "hv")  %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data (1999 to 2018)", 
                                  face = "bold", size = 14))


#1.2.3.2. Biological Quality Separate Habitats----

trawlrich_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = trawlRICH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = trawlRICH, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(1, 1)) +
  labs(y = "Trawl Richness", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

trawlabund_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = trawlABUND)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = trawlABUND, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 1)) +
  labs(y = "Trawl Abundance", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

benthrich_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = benthRICH)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = benthRICH, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(1, 3)) +
  labs(y = "Benthic Species Richness", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

benthabund_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = benthABUND)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3,
              inherit.aes = F, aes(x = YEAR, y = benthABUND, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "Benthic Abundance", x = "") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

BIBI_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = BIBI)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y = BIBI, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 3)) +
  labs(y = "BIBI", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

MAMBI_facet <- ggplot(data = scecap_RTRO, aes(x = YEAR, y = MAMBI)) + 
  geom_line() +
  theme_classic() +
  stat_smooth(method = "lm", se = F, color = "black", size = 0.3, 
              inherit.aes = F, aes(x = YEAR, y =  MAMBI, linetype = RT_RO_Other)) +
  scale_linetype_manual(values=c(3, 1)) +
  labs(y = "MAMBI", x = "Year") + 
  theme(legend.position = "none") +
  facet_wrap(~RT_RO_Other, dir = "v")

facet_biological <- ggarrange(trawlrich_facet, trawlabund_facet,
                              benthrich_facet, benthabund_facet,
                              BIBI_facet, MAMBI_facet,
                              nrow = 3, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Biological Quality Data (1999-2018)", face = "bold", size = 14))


#1.3. TIME SERIES EXPORTS----
#1.3.1. Weather

ggsave("./outputs/timeseries_all_seasonaltemp.png",
       seasonal_temperature, width = 200, height = 150, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_all_seasonalprecip.png",
       seasonal_precip, width = 200, height = 150, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_all_annualclimate.png",
       annual_climate, width = 300, height = 100, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_all_extremeclimate.png",
       extreme_climate, width = 300, height = 100, units = "mm",dpi = "retina")


ggsave("./outputs/timeseries_faceted_seasonaltemperature.png",
       seasonal_temperature_faceted, width = 200, height = 250, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_faceted_seasonalprecip.png",
       seasonal_precip_faceted, width = 200, height = 250, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_faceted_annualclimate.png",
       annual_climate_faceted, width = 250, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_faceted_extremeclimate.png",
       extreme_climate_faceted, width = 250, height = 200, units = "mm",dpi = "retina")

#1.3.2. Environmental

ggsave("./outputs/timeseries_all_waterquality.png",
       wqplots2, width = 200, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_all_sediment.png",
       sedplots2, width = 200, height = 280, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_all_biological.png",
       bioplots2, width = 200, height = 200, units = "mm",dpi = "retina")


ggsave("./outputs/timeseries_faceted_waterquality.png",
       facet_waterquality, width = 200, height = 300, units = "mm",dpi = "retina")

ggsave("./outputs/timeseries_faceted_sediment.png",
       facet_sediment, width = 200, height = 390, units = "mm",dpi = "retina")

ggsave("./timeseries_faceted_biological.png",
       facet_biological, width = 200, height = 300, units = "mm",dpi = "retina")



#2. BOXPLOTS----
#2.1. Habitat (Tidal Creek vs. Open Water)----
#2.1.1. Water Quality----

boxplot_habitat_DO <- ggplot(data = scecap_for_analysis, aes(y = Logger_DO_mean, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_sal <- ggplot(data = scecap_for_analysis, aes(y = Logger_Salin_mean, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Salinity (ppt)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_temp <- ggplot(data = scecap_for_analysis, aes(y = Logger_Temp_mean, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Water Temperature (°C)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_ph <- ggplot(data = scecap_for_analysis, aes(y = Logger_pH_mean, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "pH", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_fc <- ggplot(data = scecap_for_analysis, aes(y = LOG_FC, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_entero <- ggplot(data = scecap_for_analysis, aes(y = LOG_ENTERO, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Enterococcus (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

waterquality_boxplots_habitats <- ggarrange(boxplot_habitat_temp, boxplot_habitat_DO,
                                            boxplot_habitat_sal, boxplot_habitat_ph,
                                            boxplot_habitat_fc, boxplot_habitat_entero,
                                            ncol = 2, nrow = 3, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data by Habitat Type", face = "bold", size = 14))

#2.1.2. Sediment Quality----

boxplot_habitat_logmet <- ggplot(data = scecap_for_analysis, aes(y = log1p(Met_Total), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ug/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))
boxplot_habitat_logmetermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_met), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logpah <- ggplot(data = scecap_for_analysis, aes(y = log1p(PAH_Total), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logpahermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PAH_tot), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logpcb <- ggplot(data = scecap_for_analysis, aes(y = log1p(PCB_Total), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logpcbermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PCB_tot), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logddt <- ggplot(data = scecap_for_analysis, aes(y = log1p(DDT_Total), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logddtermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_DDT_tot), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logpbde <- ggplot(data = scecap_for_analysis, aes(y = log1p(PBDE_Total), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PBDE (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_logermqall <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_ALL), x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "All Contaminants (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

logsedimentquality_boxplots_habitats <- ggarrange(boxplot_habitat_logmet, boxplot_habitat_logmetermq,
                                                  boxplot_habitat_logpah, boxplot_habitat_logpahermq,
                                                  boxplot_habitat_logpcb, boxplot_habitat_logpcbermq,
                                                  boxplot_habitat_logddt, boxplot_habitat_logddtermq,
                                                  boxplot_habitat_logpbde, boxplot_habitat_logermqall,
                                                  nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data by Habitat Type", face = "bold", size = 14))

#2.1.3. Biological Quality----

boxplot_habitat_trawlrich <- ggplot(data = scecap_for_analysis, aes(y = trawl_SP_RICH, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_trawlabund <- ggplot(data = scecap_for_analysis, aes(y = trawl_ABUNDANCE, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_benthrich <- ggplot(data = scecap_for_analysis, aes(y = benth_SP_RICH, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_benthabund <- ggplot(data = scecap_for_analysis, aes(y = benth_ABUNDANCE, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Total Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_bibi <- ggplot(data = scecap_for_analysis, aes(y = BIBI, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "BIBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitat_mambi <- ggplot(data = scecap_for_analysis, aes(y = MAMBI, x = RT_RO_Other)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "MAMBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_habitats_biological <- ggarrange(boxplot_habitat_trawlrich, boxplot_habitat_trawlabund,
                                         boxplot_habitat_benthrich, boxplot_habitat_benthabund,
                                         boxplot_habitat_bibi, boxplot_habitat_mambi, 
                                         nrow = 3, ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data by Habitat Type", face = "bold", size = 14))

#2.2. Time Periods (1999-2008 vs. 2009-2018)----
#2.2.1. Water Quality----
#2.2.1.1. Water Quality Combined Habitats----

boxplot_period_DO <- ggplot(data = scecap_for_analysis, aes(y = Logger_DO_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_period_sal <- ggplot(data = scecap_for_analysis, aes(y = Logger_Salin_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Salinity (ppt)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_temp <- ggplot(data = scecap_for_analysis, aes(y = Logger_Temp_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Water Temperature (°C)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_ph <- ggplot(data = scecap_for_analysis, aes(y = Logger_pH_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "pH", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_fc <- ggplot(data = scecap_for_analysis, aes(y = LOG_FC, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_period_entero <- ggplot(data = scecap_for_analysis, aes(y = LOG_ENTERO, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Enterococcus (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

waterquality_boxplots_periods <- ggarrange(boxplot_period_temp , boxplot_period_DO,
                                           boxplot_period_sal, boxplot_period_ph,
                                           boxplot_period_fc, boxplot_period_entero,
                                           ncol = 2, nrow = 3, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data by Time Period", face = "bold", size = 14))

#2.2.1.2. Water Quality Separate Habitats----

facetboxplot_period_DO <- ggplot(data = scecap_for_analysis, aes(y = Logger_DO_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_sal <- ggplot(data = scecap_for_analysis, aes(y = Logger_Salin_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Salinity (ppt)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_temp <- ggplot(data = scecap_for_analysis, aes(y = Logger_Temp_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Water Temperature (°C)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_ph <- ggplot(data = scecap_for_analysis, aes(y = Logger_pH_mean, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "pH", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_fc <- ggplot(data = scecap_for_analysis, aes(y = LOG_FC, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_period_entero <- ggplot(data = scecap_for_analysis, aes(y = LOG_ENTERO, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Enterococcus (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetwaterquality_boxplots_periods <- ggarrange(facetboxplot_period_temp , facetboxplot_period_DO,
                                                facetboxplot_period_sal, facetboxplot_period_ph,
                                                facetboxplot_period_fc, facetboxplot_period_entero,
                                                ncol = 2, nrow = 3, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data by Time Period and Habitat Type", face = "bold", size = 14))

#2.2.2. Sediment Quality----
#2.2.2.1. Sediment Quality Combined Habitats----

boxplot_period_logmet <- ggplot(data = scecap_for_analysis, aes(y = log1p(Met_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ug/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logmetermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_met), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logpah <- ggplot(data = scecap_for_analysis, aes(y = log1p(PAH_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logpahermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PAH_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_period_logpcb <- ggplot(data = scecap_for_analysis, aes(y = log1p(PCB_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logpcbermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PCB_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logddt <- ggplot(data = scecap_for_analysis, aes(y = log1p(DDT_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logddtermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_DDT_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logpbde <- ggplot(data = scecap_for_analysis, aes(y = log1p(PBDE_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PBDE (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_logermqall <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_ALL), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "All Contaminants (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

logsedimentquality_boxplots_periods <- ggarrange(boxplot_period_logmet, boxplot_period_logmetermq,
                                                 boxplot_period_logpah, boxplot_period_logpahermq,
                                                 boxplot_period_logpcb, boxplot_period_logpcbermq,
                                                 boxplot_period_logddt, boxplot_period_logddtermq,
                                                 boxplot_period_logpbde, boxplot_period_logermqall,
                                                 nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data by Time Period", face = "bold", size = 14))

#2.2.2.2. Sediment Quality Separate Habitats----

facetboxplot_period_logmet <- ggplot(data = scecap_for_analysis, aes(y = log1p(Met_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ug/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logmetermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_met), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logpah <- ggplot(data = scecap_for_analysis, aes(y = log1p(PAH_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logpahermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PAH_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logpcb <- ggplot(data = scecap_for_analysis, aes(y = log1p(PCB_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logpcbermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_PCB_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logddt <- ggplot(data = scecap_for_analysis, aes(y = log1p(DDT_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logddtermq <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_DDT_tot), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logpbde <- ggplot(data = scecap_for_analysis, aes(y = log1p(PBDE_Total), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PBDE (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_logermqall <- ggplot(data = scecap_for_analysis, aes(y = log1p(ERMQ_ALL), x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "All Contaminants (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetlogsedimentquality_boxplots_periods <- ggarrange(facetboxplot_period_logmet, facetboxplot_period_logmetermq,
                                                      facetboxplot_period_logpah, facetboxplot_period_logpahermq,
                                                      facetboxplot_period_logpcb, facetboxplot_period_logpcbermq,
                                                      facetboxplot_period_logddt, facetboxplot_period_logddtermq,
                                                      facetboxplot_period_logpbde, facetboxplot_period_logermqall,
                                                      nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data by Time Period and Habitat Type", face = "bold", size = 14))

#2.2.3. Biological Quality----
#2.2.3.1. Biological Quality Combined Habitats----

boxplot_period_trawlrich <- ggplot(data = scecap_for_analysis, aes(y = trawl_SP_RICH, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_trawlabund <- ggplot(data = scecap_for_analysis, aes(y = trawl_ABUNDANCE, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_benthrich <- ggplot(data = scecap_for_analysis, aes(y = benth_SP_RICH, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_benthabund <- ggplot(data = scecap_for_analysis, aes(y = benth_ABUNDANCE, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Total Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_period_bibi <- ggplot(data = scecap_for_analysis, aes(y = BIBI, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "BIBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_period_mambi <- ggplot(data = scecap_for_analysis, aes(y = MAMBI, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "MAMBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) 

boxplot_periods_biological <- ggarrange(boxplot_period_trawlrich, boxplot_period_trawlabund,
                                        boxplot_period_benthrich, boxplot_period_benthabund,
                                        boxplot_period_bibi, boxplot_period_mambi, 
                                        nrow = 3, ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data by Time Period", face = "bold", size = 14))

#2.2.3.2. Biological Quality Separate Habitats----

facetboxplot_period_trawlrich <- ggplot(data = scecap_for_analysis, aes(y = trawl_SP_RICH, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_trawlabund <- ggplot(data = scecap_for_analysis, aes(y = trawl_ABUNDANCE, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_benthrich <- ggplot(data = scecap_for_analysis, aes(y = benth_SP_RICH, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_benthabund <- ggplot(data = scecap_for_analysis, aes(y = benth_ABUNDANCE, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Total Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_bibi <- ggplot(data = scecap_for_analysis, aes(y = BIBI, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "BIBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_period_mambi <- ggplot(data = scecap_for_analysis, aes(y = MAMBI, x = PERIOD)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "MAMBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_periods_biological <- ggarrange(facetboxplot_period_trawlrich, facetboxplot_period_trawlabund,
                                             facetboxplot_period_benthrich, facetboxplot_period_benthabund,
                                             facetboxplot_period_bibi, facetboxplot_period_mambi, 
                                             nrow = 3,  ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data by Time Period and Habitat Type", face = "bold", size = 14))

#2.3. Watershed Development (Developed vs. Undeveloped)----
#2.3.1. Water Quality----
#2.3.1.1. Water Quality Combined Habitats----

boxplot_development_DO <- ggplot(data = scecap_hucs, aes(y = Logger_DO_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_sal <- ggplot(data = scecap_hucs, aes(y = Logger_Salin_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Salinity (ppt)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_temp <- ggplot(data = scecap_hucs, aes(y = Logger_Temp_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Water Temperature (°C)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_ph <- ggplot(data = scecap_hucs, aes(y = Logger_pH_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "pH", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_fc <- ggplot(data = scecap_hucs, aes(y = LOG_FC, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_entero <- ggplot(data = scecap_hucs, aes(y = LOG_entero, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Enterococcus (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

waterquality_boxplots_developments <- ggarrange(boxplot_development_temp, boxplot_development_DO,
                                                boxplot_development_sal, boxplot_development_ph,
                                                boxplot_development_fc, boxplot_development_entero,
                                                ncol = 2, nrow = 3, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data by Watershed Development", face = "bold", size = 14))

#2.3.1.2. Water Quality Separate Habitats----

facetboxplot_development_DO <- ggplot(data = scecap_hucs, aes(y = Logger_DO_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Dissolved Oxygen (mg/L)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", ""))) +
  facet_wrap(~RT_RO_Other)

facetboxplot_development_sal <- ggplot(data = scecap_hucs, aes(y = Logger_Salin_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Salinity (ppt)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_temp <- ggplot(data = scecap_hucs, aes(y = Logger_Temp_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Water Temperature (°C)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_ph <- ggplot(data = scecap_hucs, aes(y = Logger_pH_mean, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "pH", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_fc <- ggplot(data = scecap_hucs, aes(y = LOG_FC, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Fecal Coliform (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_entero <- ggplot(data = scecap_hucs, aes(y = LOG_entero, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Enterococcus (log MPN/100mL)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetwaterquality_boxplots_developments <- ggarrange(facetboxplot_development_temp, facetboxplot_development_DO,
                                                     facetboxplot_development_sal, facetboxplot_development_ph,
                                                     facetboxplot_development_fc, facetboxplot_development_entero,
                                                     ncol = 2, nrow = 3, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Water Quality Data by Watershed Development and Habitat Type", face = "bold", size = 14))

#2.3.2. Sediment Quality----
#2.3.2.1. Sediment Quality Combined Habitats----

boxplot_development_logmet <- ggplot(data = scecap_hucs, aes(y = log1p(Met_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ug/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logmetermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_met), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logpah <- ggplot(data = scecap_hucs, aes(y = log1p(PAH_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logpahermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_PAH_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logpcb <- ggplot(data = scecap_hucs, aes(y = log1p(PCB_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logpcbermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_PCB_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logddt <- ggplot(data = scecap_hucs, aes(y = log1p(DDT_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logddtermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_DDT_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logpbde <- ggplot(data = scecap_hucs, aes(y = log1p(PBDE_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PBDE (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_logermqall <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_ALL), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "All Contaminants (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

logsedimentquality_boxplots_developments <- ggarrange(boxplot_development_logmet, boxplot_development_logmetermq,
                                                      boxplot_development_logpah, boxplot_development_logpahermq,
                                                      boxplot_development_logpcb, boxplot_development_logpcbermq,
                                                      boxplot_development_logddt,boxplot_development_logddtermq,
                                                      boxplot_development_logpbde, boxplot_development_logermqall,
                                                      nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data by Watershed Development", face = "bold", size = 14))

#2.3.2.2. Sediment Quality Separate Habitats----

facetboxplot_development_logmet <- ggplot(data = scecap_hucs, aes(y = log1p(Met_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ug/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logmetermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_met), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Metals (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logpah <- ggplot(data = scecap_hucs, aes(y = log1p(PAH_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logpahermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_PAH_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PAHs (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logpcb <- ggplot(data = scecap_hucs, aes(y = log1p(PCB_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logpcbermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_PCB_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PCBs (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logddt <- ggplot(data = scecap_hucs, aes(y = log1p(DDT_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logddtermq <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_DDT_tot), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "DDT (log ERMQ)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logpbde <- ggplot(data = scecap_hucs, aes(y = log1p(PBDE_Total), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "PBDE (log ng/g)", x = "")  + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_logermqall <- ggplot(data = scecap_hucs, aes(y = log1p(ERMQ_ALL), x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "All Contaminants (log ERMQ)", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetlogsedimentquality_boxplots_developments <- ggarrange(facetboxplot_development_logmet, facetboxplot_development_logmetermq,
                                                           facetboxplot_development_logpah, facetboxplot_development_logpahermq,
                                                           facetboxplot_development_logpcb, facetboxplot_development_logpcbermq,
                                                           facetboxplot_development_logddt,  facetboxplot_development_logddtermq,
                                                           facetboxplot_development_logpbde, facetboxplot_development_logermqall,
                                                           nrow = 5, ncol = 2, align = "hv") %>%
  annotate_figure(top = text_grob("SCECAP Sediment Quality Data by Watershed Development and Habitat Type", face = "bold", size = 14))

#2.3.3. Biological Quality----
#2.3.3.1. Biological Quality Combined Habitats----

boxplot_development_trawlrich <- ggplot(data = scecap_hucs, aes(y = trawl_SP_RICH, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_trawlabund <- ggplot(data = scecap_hucs, aes(y = trawl_ABUND, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_benthrich <- ggplot(data = scecap_hucs, aes(y = benth_SP_RICH, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_benthabund <- ggplot(data = scecap_hucs, aes(y = benth_ABUNDANCE, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Total Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_bibi <- ggplot(data = scecap_hucs, aes(y = BIBI, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "BIBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_development_mambi <- ggplot(data = scecap_hucs, aes(y = MAMBI, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "MAMBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))

boxplot_developments_biological <- ggarrange(boxplot_development_trawlrich, boxplot_development_trawlabund,
                                             boxplot_development_benthrich, boxplot_development_benthabund,
                                             boxplot_development_bibi, boxplot_development_mambi, nrow = 3,
                                             ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data by Watershed Development", face = "bold", size = 14))

#2.3.3.2. Biological Quality Separate Habitats----

facetboxplot_development_trawlrich <- ggplot(data = scecap_hucs, aes(y = trawl_SP_RICH, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_trawlabund <- ggplot(data = scecap_hucs, aes(y = trawl_ABUND, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Trawl Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_benthrich <- ggplot(data = scecap_hucs, aes(y = benth_SP_RICH, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Species Richness", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_benthabund <- ggplot(data = scecap_hucs, aes(y = benth_ABUNDANCE, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "Benthic Total Abundance", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_bibi <- ggplot(data = scecap_hucs, aes(y = BIBI, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "BIBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_development_mambi <- ggplot(data = scecap_hucs, aes(y = MAMBI, x = HUC14_DEV_cut)) + 
  geom_boxplot() + 
  theme_classic() +
  labs(y = "MAMBI", x = "") + 
  stat_compare_means(method = "t.test", label = "p.signif", label.x = 1.5,
                     symnum.args =  list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                         symbols = c("***", "**", "*", "")))+
  facet_wrap(~RT_RO_Other)

facetboxplot_developments_biological <- ggarrange(facetboxplot_development_trawlrich, facetboxplot_development_trawlabund,
                                                  facetboxplot_development_benthrich, facetboxplot_development_benthabund,
                                                  facetboxplot_development_bibi, facetboxplot_development_mambi,
                                                  nrow = 3, ncol = 2, align = "hv") %>% 
  annotate_figure(top = text_grob("SCECAP Biological Quality Data by Watershed Development and Habitat Type", face = "bold", size = 14))



#2.4. BOXPLOTS EXPORTS----

#2.4.1. Habitats

ggsave("./outputs/boxplot_all_habitats_waterquality.png",
       waterquality_boxplots_habitats, width = 200, height = 200, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_habitats_sediment.png",
       logsedimentquality_boxplots_habitats, width = 200, height = 250, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_habitats_biological.png",
       boxplot_habitats_biological , width = 200, height = 200, units = "mm",dpi = "retina")

#2.4.2. Time Periods

ggsave("./outputs/boxplot_all_period_waterquality.png",
       waterquality_boxplots_periods, width = 200, height = 200, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_period_sediment.png",
       logsedimentquality_boxplots_periods, width = 200, height = 250, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_period_biological.png",
       boxplot_periods_biological , width = 200, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/boxplot_faceted_period_waterquality.png",
       facetwaterquality_boxplots_periods, width = 200, height = 200, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_faceted_period_sediment.png",
       facetlogsedimentquality_boxplots_periods, width = 200, height = 250, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_faceted_period_biological.png",
       facetboxplot_periods_biological, width = 200, height = 200, units = "mm",dpi = "retina")


#2.4.3. Watershed Development

ggsave("./outputs/boxplot_all_development_waterquality.png",
       waterquality_boxplots_developments, width = 200, height = 200, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_development_sediment.png",
       logsedimentquality_boxplots_developments, width = 200, height = 250, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_all_development_biological.png",
       boxplot_developments_biological, width = 200, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/boxplot_faceted_development_waterquality.png",
       facetwaterquality_boxplots_developments, width = 200, height = 200, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_faceted_development_sediment.png",
       facetlogsedimentquality_boxplots_developments, width = 200, height = 250, units = "mm",dpi = "retina")
ggsave("./outputs/boxplot_faceted_development_biological.png",
       facetboxplot_developments_biological , width = 200, height = 200, units = "mm",dpi = "retina")







#3. PLOTTING LANDCOVER----
#3.1. County Population Data----

population_by_county <- county_population %>%
  ggplot( aes(x=CENSUS_YEAR, y=POP_DENS)) +
  geom_line(data=county_population %>% dplyr::select(-COUNTY),
            aes(group=COUNTY2), color = "black", linetype=1, size=0.5, alpha = 0.3) +
  geom_line(color="black", size=1)+
  theme_classic() +
  labs(x = "Year", y = "Population Density (persons per hectare)") +
  facet_wrap(~COUNTY) 

population_by_county <- population_by_county %>%  
  annotate_figure(top = text_grob("Population Growth in Coastal South Carolina Counties (2000-2018)", 
                                  face = "bold", size = 14))

#3.2. County Impervious Data----

impervious_by_county <- landcover_counties2 %>%
  ggplot( aes(x=YEAR, y=MEAN_UPLAND_IMPERV_PRCT)) +
  geom_line(data=landcover_counties2 %>% dplyr::select(-COUNTY),
            aes(group=COUNTY2), color = "black", linetype=1, size=0.5, alpha = 0.3) +
  geom_line(color="black", size=1)+
  geom_point(size = 1.5, shape = 18) +
  theme_classic() +
  labs(x = "Year", y = "Mean Upland Impervious Cover (%)") +
  facet_wrap(~COUNTY) 

impervious_by_county <- impervious_by_county %>% 
  annotate_figure(top = text_grob("Change in Impervious Cover in Coastal South Carolina Counties (2001-2016)", 
                                  face = "bold", size = 14))

#3.3. County Landcover Data----

landcover_by_county <- ggplot(data = landcover_long_1, aes(x= YEAR, y = Percent, group = LandcoverCategory)) +
  geom_point(aes(shape = LandcoverCategory)) + 
  scale_shape_manual(values = c(0,17,16,18,1)) +
  labs(x = "Year", y = "Percent") +
  geom_line(size = 0.3, color = "black", alpha = 1) + theme_classic() + facet_wrap(~COUNTY) 

landcover_by_county <- landcover_by_county %>%   annotate_figure(top = text_grob("Changes in Landcover in Coastal South Carolina Counties (2001-2016)", 
                                                                                 face = "bold", size = 14))



#3.4. Landuse Changes----

coastal_landchange <- ggplot(data = gain_lost, aes(x= Landcover, y = Hectares, fill = Change)) +
  geom_bar(stat = "identity") +
  geom_text(label= c("","",
                     "-3,987","",
                     "-4,992","",
                     "-12,749","",
                     "-7,878","",
                     "","",
                     "",""), 
            position = position_dodge(-1), hjust = -0.2, size = 3) +
  geom_text(label= c("","+5,562",
                     "","",
                     "","",
                     "","",
                     "","",
                     "","+15,194",
                     "","+8,850"), 
            position = position_dodge(1), hjust = 1.1, size = 3, color = "white") +
  scale_fill_grey() +
  theme_classic() +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.title.align = 0.5) +
  coord_flip() +
  labs(title = "Change in Landcover in Coastal South Carolina Counties (2001-2016)")

#3.5. Landuse Changes by County----

coastal_landchange_county <- ggplot(data = gain_lost2, aes(x= Landcover, y = Hectares, fill = Change)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  theme_classic() +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.title.align = 0.5) +
  coord_flip() +
  labs(title = "Change in Landcover in Coastal South Carolina Counties (2001-2016)") +
  facet_wrap(~County, dir = "h", ncol = 2)


#3.6. County Change-No Change----

change_nochange_county <- ggplot(data = change_nochange, aes(x = County, y = Hectares, fill = Category)) + 
  geom_bar(stat = "identity", position = position_dodge(-1)) +
  scale_fill_grey() +
  scale_y_continuous(labels = comma) +
  theme_classic() +
  guides(fill = guide_legend(reverse=T))  +
  theme(legend.title = element_blank(),
        legend.position = "bottom", 
        legend.title.align = 0.5)+
  labs(title = "Landcover Changes in Coastal South Carolina Counties (2001-2016)")

#3.7. Exporting Landcover Graphs----

ggsave("./outputs/landcovercounties_population.png",
       population_by_county, width = 200, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/landcovercounties_impervious.png",
       impervious_by_county, width = 200, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/landcovercounties_landcover.png",
       landcover_by_county, width = 225, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/landcoverchange_total.png",
       coastal_landchange, width = 225, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/landcoverchange_counties.png",
       coastal_landchange_county, width = 225, height = 200, units = "mm",dpi = "retina")

ggsave("./outputs/change_summmary_counties.png",
       change_nochange_county, width = 225, height = 200, units = "mm",dpi = "retina")
