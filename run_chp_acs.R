#run ACS data from Excel files, 2016-2020 5-year data will be release in March, 2022
rm(list=ls())
pacman::p_load(dplyr, foreign, readxl, epiDisplay, data.table, janitor, rads, naniar, stringr)
setwd("S:/WORK/City Health Profiles/2021/")
f1519 <-"S:/WORK/surveys/ACS/2015-2019_5_year/Analysis/03_ACS_Calculations/output_ACS_data_2015_2019_by_geography.xlsx"

#-----pull HRA data from Excel for different 5-year periods----
acs_hra1519 <- read_excel(f1519, "hra", range = cell_cols("D:K"))
acs_hra1519$year <- "2015-2019"

acs_hra <- subset(acs_hra1519, select=-c(se))
acs_hra$cat1 <- "Cities/neighborhoods"
acs_hra$variable <- tolower(acs_hra$variable)
acs_hra <- acs_hra %>% group_by(year, variable) %>% mutate(ranks=rank(-percent, ties.method ="random"))

#-----pull Big city data from Excel for different 5-year periods----
acs_big1519 <- read_excel(f1519, "place", range = cell_cols("F:M"))
acs_big1519$year <- "2015-2019"

acs_big <- subset(acs_big1519, select=-c(se))
acs_big$variable <- tolower(acs_big$variable)

acs_big <- subset(acs_big, name=="Auburn city" | name=="Bellevue city" | name=="Federal Way city" |
                  name=="Kent city" | name=="King County" | name=="Renton city" | name=="Seattle city" |
                  name=="Washington State")
acs_big$cat1 <- "Big cities"
acs_big <- acs_big %>% mutate(cat1= case_when(name=="King County" ~"King County", 
                                          name=="Washington State" ~"Washington State", 
                                          TRUE ~cat1))

acs_all <- bind_rows(acs_hra, acs_big)
acs_all <- acs_all %>% rename("cat1_group" = "name", "indicator_key" = "variable", "result" = "percent",
                              "numerator" = "estimate", "se" = "pse")    
acs_all$lower_bound <- acs_all$result - 1.645*acs_all$se
acs_all$upper_bound <- acs_all$result + 1.645*acs_all$se

acs_kc <- subset(acs_all, cat1=="King County")
acs_kc <- subset(acs_kc, select=c(year, indicator_key, lower_bound, upper_bound))
acs_kc <- acs_kc %>% rename("kc_lower"="lower_bound", "kc_upper"="upper_bound")

acs_data <-merge(acs_all, acs_kc, by=c("year", "indicator_key"))
acs_data <- acs_data %>% mutate(comparison_with_kc 
                              = case_when(upper_bound < kc_lower ~as.character("lower"), 
                                          lower_bound > kc_upper ~as.character("higher"), 
                                          TRUE ~as.character("not different")))
acs_data <- acs_data %>% mutate(significance 
                              = case_when(upper_bound < kc_lower ~as.character("*"), 
                                          lower_bound > kc_upper ~as.character("*"), TRUE ~as.character("")))
acs_data <- acs_data %>% mutate(suppression= case_when(numerator >0 & numerator< 10 ~as.character("^"), TRUE ~as.character("NA")))
acs_data <- acs_data %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
acs_data <- acs_data %>% mutate(tab = case_when(cat1=="King County" ~"_kingcounty", 
                                                cat1=="Washington State" ~"_wastate", 
                                                cat1=="Cities/neighborhoods" ~"demgroups",
                                                cat1=="Big cities" ~"bigcities", TRUE ~""))
acs_data <- subset(acs_data, select=-c(kc_lower, kc_upper))
acs_data <- acs_data %>% mutate(cat1_group  = case_when(cat1_group=='Fed Way-Dash Pt' ~'Fed Way-Dash Point/Woodmont',
                                          TRUE ~cat1_group))
acs_data$data_source <- "acs"
acs_data$source_date <- "2021-0901"
acs_data$run_date <- "2021-1007"

result_acs <- acs_data[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                         "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc",
                         "significance", "caution", "suppression", "ranks", "source_date", "run_date")]
write.csv(result_acs, "result_acs.csv", row.names = F)