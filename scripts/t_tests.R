#LOAD REQUIRED PACKAGES----
library(tidyverse)

#IMPORT DATATSETS----
scecap<- read.csv("./data/scecap_for_analysis.csv")
scecap_hucs <- read.csv("./data/scecap_hucs.csv")

#PREPARE DATASETS----

#create duplicate of data, changing tidalcreek/openwater to all sites
scecap_allsites <- scecap
scecap_allsites$RT_RO_Other <- "AllSites"

#paste this duplciate dataset to main data (to later separate data into tidal creek, openwater, and combined sites)
scecap_1 <- rbind(scecap, scecap_allsites)

#join watershed development data to new scecap data based on station code
scecap_2 <- scecap_hucs %>% 
            select(StationCode, HUC14_DEV_cut) %>% 
            full_join(., scecap_1, by = "StationCode")

#create new datasets based upon groupings (habitat, time periods, watershed development) 
#use gather to convert data from wide to long format

scecap_habitat    <-  scecap_2 %>%  filter(RT_RO_Other != "AllSites") %>%
                                    select(StationCode, RT_RO_Other, 
                                         Logger_DO_mean, Logger_Salin_mean,
                                         Logger_Temp_mean, Logger_pH_mean,
                                         LOG_ENTERO, LOG_FC, Met_Total,
                                         ERMQ_met, PAH_Total, ERMQ_PAH_tot,
                                         PCB_Total, ERMQ_PCB_tot, DDT_Total,
                                         ERMQ_DDT_tot, PBDE_Total, ERMQ_ALL,
                                         trawl_SP_RICH, trawl_ABUNDANCE, 
                                         benth_SP_RICH, benth_ABUNDANCE,
                                         BIBI, MAMBI) %>% 
                                    gather(key = "Parameter", value = "Value", 
                                        -StationCode, -RT_RO_Other)

scecap_period     <-  scecap_2 %>%   select(StationCode, PERIOD, RT_RO_Other,
                                         Logger_DO_mean, Logger_Salin_mean,
                                         Logger_Temp_mean, Logger_pH_mean,
                                         LOG_ENTERO, LOG_FC, Met_Total,
                                         ERMQ_met, PAH_Total, ERMQ_PAH_tot,
                                         PCB_Total, ERMQ_PCB_tot, DDT_Total,
                                         ERMQ_DDT_tot, PBDE_Total, ERMQ_ALL,
                                         trawl_SP_RICH, trawl_ABUNDANCE, 
                                         benth_SP_RICH, benth_ABUNDANCE,
                                         BIBI, MAMBI) %>% 
                                    gather(key = "Parameter", value = "Value",
                                     -StationCode, -RT_RO_Other, -PERIOD)

scecap_development <- scecap_2 %>% select(StationCode, RT_RO_Other, HUC14_DEV_cut,
                                          Logger_DO_mean, Logger_Salin_mean,
                                          Logger_Temp_mean, Logger_pH_mean,
                                          LOG_ENTERO, LOG_FC, Met_Total,
                                          ERMQ_met, PAH_Total, ERMQ_PAH_tot,
                                          PCB_Total, ERMQ_PCB_tot, DDT_Total,
                                          ERMQ_DDT_tot, PBDE_Total, ERMQ_ALL,
                                          trawl_SP_RICH, trawl_ABUNDANCE, 
                                          benth_SP_RICH, benth_ABUNDANCE,
                                          BIBI, MAMBI) %>% 
                                    gather(key = "Parameter", value = "Value", 
                                          -StationCode, -RT_RO_Other, -HUC14_DEV_cut)
      
#RUN T-TESTS----
#use 'broom's tidy function to run t-tests for each parameter and each habitat grouping 
#and save results to tabkle

#Open Water vs. Tidal Creek
habitat_ttest_results     <-  scecap_habitat %>% 
                              group_by(Parameter) %>% 
                              do(broom::tidy(t.test(.$Value ~ .$RT_RO_Other)))

#1999-2008 vs. 2009-2018
period_ttest_results      <-  scecap_period %>% 
                              group_by(Parameter, RT_RO_Other) %>% 
                              do(broom::tidy(t.test(.$Value ~ .$PERIOD)))

#Developed vs. Undeveloped
development_ttest_results <-  scecap_development %>% 
                              group_by(Parameter, RT_RO_Other) %>% 
                              do(broom::tidy(t.test(.$Value ~ .$HUC14_DEV_cut)))

#edit results tables
#select and rename data columns of interest

habitat_ttest_results_2             <-  habitat_ttest_results %>% 
                                        select(Parameter, estimate1, estimate2, p.value)
names(habitat_ttest_results_2)      <- c("Parameter", "OpenWater", "TidalCreek","p.value")
  
period_ttest_results_2              <-  period_ttest_results %>% 
                                        select(Parameter, RT_RO_Other, estimate1, estimate2, p.value)
names(period_ttest_results_2)       <-  c("Parameter", "Habitat", "1999to2008", "2009to2018","p.value")

development_ttest_results_2         <-  development_ttest_results %>% 
                                        select(Parameter, RT_RO_Other, estimate1, estimate2, p.value)
names(development_ttest_results_2)  <-  c("Parameter", "Habitat", "Developed","Undeveloped", "p.value")

#EXPORT RESULTS----
#write tables to .csv files 

write.csv(habitat_ttest_results_2, "./outputs/ttest_results_habitat.csv")
write.csv(period_ttest_results_2, "./outputs/ttest_results_period.csv")
write.csv(development_ttest_results_2, "./outputs/ttest_results_development.csv")
