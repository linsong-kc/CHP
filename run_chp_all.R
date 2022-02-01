#union data from acs (1519), brfss (1620), death (1620), birth (1620*) and OFM (2020)
rm(list=ls())
pacman::p_load(dplyr, foreign, readxl, epiDisplay, data.table, janitor, rads, naniar, DBI)
setwd("S:/WORK/City Health Profiles/2021/")

chp_acs <- read.csv("result_acs.csv")
chp_brfs <- read.csv("res_brfs.csv")
chp_death <- read.csv("result_death.csv")
chp_ofm <- read.csv("result_ofm.csv")
chp_ofm$year <- as.character(chp_ofm$year)
chp_birth <- read.csv("result_birth.csv")
chp_all <- bind_rows(chp_acs, chp_brfs, chp_death, chp_ofm, chp_birth)

#-----Big city data
chp_big <- subset(chp_all, subset=cat1=="Big cities", select=-c(tab, cat1, ranks, source_date, run_date))
colnames(chp_big) <- paste(colnames(chp_big), "big", sep = "_")
chp_big<- chp_big %>% rename("data_source"="data_source_big", "indicator_key" = "indicator_key_big")

chp_kc <- subset(chp_all, subset=tab=="_kingcounty", 
                 select=c(data_source, year, indicator_key, result, lower_bound, upper_bound))
chp_kc <- chp_kc %>% rename("result_kc"="result", "LB_kc" = "lower_bound", "UB_kc" = "upper_bound")

chp_wa <- subset(chp_all, subset=tab=="_wastate", 
                 select=c(data_source, year, indicator_key, result, lower_bound, upper_bound))
chp_wa <- chp_wa %>% rename("result_wa"="result", "LB_wa" = "lower_bound", "UB_wa" = "upper_bound")

chp_all <- chp_all %>% mutate(tab = case_when(cat1=="Big cities" ~"bigcities", TRUE ~tab)) 
chp_all <- chp_all %>% mutate(cat1_group = case_when(cat1_group=="Fed Way-Dash Point/Woodmont"
                                                     ~"Fed Way-Dash Pt", TRUE ~cat1_group))

chp_all <- chp_all %>% mutate(cat1_group_alias = case_when(cat1_group=="Auburn city" ~"Auburn",
                              cat1_group=="Bellevue city" ~"Bellevue",
                              cat1_group=="Federal Way city" ~"Federal Way",
                              cat1_group=="Kent city" ~"Kent",
                              cat1_group=="Renton city" ~"Renton",
                              cat1_group=="Seattle city" ~"Seattle",
                              TRUE ~cat1_group))


chp_all <- chp_all[, c("data_source", "indicator_key", "tab", "year", 
                       "cat1", "cat1_group", "cat1_group_alias",
                       "result", "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", 
                       "comparison_with_kc",  "significance", "caution", "suppression", 
                                            "ranks", "source_date", "run_date")]

chp_all <- chp_all %>% mutate_at(vars(result, lower_bound, upper_bound, se, rse), list(~ round(., 3)))
chp_all <- chp_all %>% mutate_at(vars(numerator, denominator), list(~ round(., 0)))     
chp_kc <- chp_kc %>% mutate_at(vars(result_kc, LB_kc, UB_kc), list(~ round(., 3)))     
chp_wa <- chp_wa %>% mutate_at(vars(result_wa, LB_wa, UB_wa), list(~ round(., 3)))

write.csv(chp_all, "chp_all.csv", row.names = F)

#DB connection
db51 <- odbc::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "KCITSQLUTPDBH51",
                        Database = "PHExtractStore")

dbWriteTable(db51, Id(schema = "APDE_WIP", table = "CHP_HRA"), chp_all, overwrite = T)
dbWriteTable(db51, Id(schema = "APDE_WIP", table = "CHP_KC"), chp_kc, overwrite = T)
dbWriteTable(db51, Id(schema = "APDE_WIP", table = "CHP_WA"), chp_wa, overwrite = T)
DBI::dbDisconnect(db51)
