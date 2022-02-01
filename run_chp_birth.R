#run CHAT generated birth data (infant mortality is 1519 and birth risk factors are 1620)
rm(list=ls())
pacman::p_load(dplyr, foreign, readxl, epiDisplay, data.table, janitor, rads, naniar)
setwd("S:/WORK/City Health Profiles/2021/")
#merge files: HRA (already added SE in Excel), Big (already added SE in Excel), KCWA (I did not run SE for KCWA), 

#-----process HRA data
b_hra1 <- read_excel("CHPbirth1620.xlsx", "birth_hra")
b_hra1 <- subset(b_hra1, select = -c(Age, Race, Ethnicity))
b_hra1 <- b_hra1 %>% rename("year" = "Year", "numerator" = "Count", "denominator" = "Population",
                            "result" = "Rate", "lower_bound" = "Lower CI", "upper_bound" = "Upper CI") 

b_hra2 <- read_excel("CHPbirth1620.xlsx", "birth_hra_se")
b_hra2 <- b_hra2[-c(2:4, 6:8)]
b_hra2 <- b_hra2 %>% rename("Geography" = "Geography_Name")
b_hra3 <- inner_join(b_hra1, b_hra2, by = c("indicator_key", "Geography"))

hraname <- read_excel("S:/WORK/City Health Profiles/2021/CHPdeath1620.xlsx", "hraname")
b_hra4 <- merge(x = b_hra3, y=hraname, by="Geography", all = T)
b_hra4$cat1 <- "Cities/neighborhoods"
b_hra4 <- subset(b_hra4, select = - c(Geography, exclude))
b_hra4 <- b_hra4 %>% group_by(indicator_key) %>% mutate(ranks=rank(-result, ties.method ="random"))

#------------------------
#process Big city data
b_big1 <- read_excel("CHPbirth1620.xlsx", "birth_big")
b_big1 <- subset(b_big1, select = -c(Age, Race, Ethnicity))
b_big1 <- b_big1 %>% rename("year" = "Year", "numerator" = "Count", "denominator" = "Population",
           "result" = "Rate", "lower_bound" = "Lower CI", "upper_bound" = "Upper CI") 
b_big2 <- read_excel("CHPbirth1620.xlsx", "birth_big_se")
b_big2 <- b_big2[-c(2:4, 6:8)]
b_big2 <- b_big2 %>% rename("Geography" = "Geography_Name")

b_big3 <- inner_join(b_big1, b_big2, by = c("indicator_key", "Geography"))
b_big3 <- b_big3 %>% rename("cat1_group" = "Geography")
b_big3$cat1 <- "Big cities"

#------------------------
#process KCWA data
b_kcwa1 <- read_excel("CHPbirth1620.xlsx", "birth_kcwa")
b_kcwa1 <- subset(b_kcwa1, select = -c(Age, Race, Ethnicity))
b_kcwa1 <- b_kcwa1 %>% rename("year" = "Year", "numerator" = "Count", "denominator" = "Population",
                            "result" = "Rate", "lower_bound" = "Lower CI", "upper_bound" = "Upper CI") 
b_kcwa2 <- read_excel("CHPbirth1620.xlsx", "birth_kcwa_se")
b_kcwa2 <- b_kcwa2[-c(2:4, 6:8)]
b_kcwa2 <- b_kcwa2 %>% rename("Geography" = "Geography_Name")
b_kcwa3 <- inner_join(b_kcwa1, b_kcwa2, by = c("indicator_key", "Geography"))
b_kcwa3 <- b_kcwa3 %>% rename("cat1_group" = "Geography")
b_kcwa3 <- b_kcwa3 %>% mutate(tab = case_when(cat1_group=="King" ~"_kingcounty", 
                                              cat1_group=="State Total" ~ "_wastate"))
b_kcwa3 <- b_kcwa3 %>% mutate(cat1 = case_when(cat1_group=="King" ~"King County", 
                                               cat1_group=="State Total" ~ "Washington State"))
b_kcwa3 <- b_kcwa3 %>% mutate(cat1_group = case_when(cat1_group=="King" ~"King County", 
                                                     cat1_group=="State Total" ~ "Washington State"))

#-----Combine HRA/Big cities and KCWA data-----
b_data1 <- bind_rows(b_hra4, b_big3)  #function in dplyr to merge 2 dfs with different number of columns
b_data1 <- bind_rows(b_data1, b_kcwa3)
b_data1 <- b_data1 %>% rename("se" = "SE")
b_data1$rse <- 100*b_data1$se / b_data1$result
b_data2 <- b_data1[ , c("indicator_key", "year", "tab", "cat1", "cat1_group", "result", "se", "rse",
                       "lower_bound", "upper_bound", "numerator", "denominator", "ranks")]

#-----King County HRA/big cities data, compare to Kc data -----
b_kc1 <- subset(b_data2, subset=tab=="_kingcounty", select= c(indicator_key, year, lower_bound, upper_bound))
b_kc1 <- b_kc1 %>% rename("kc_lower"="lower_bound", "kc_upper"="upper_bound")

#-----King County big cities data, for ranking the 26 City/Area (hra2code) by result
b_data3 <-merge(b_data2, b_kc1, by=c("indicator_key"))
b_data3 <- b_data3 %>% mutate(comparison_with_kc 
                            = case_when(upper_bound < kc_lower ~as.character("lower"), 
                                        lower_bound > kc_upper ~as.character("higher"), 
                                        TRUE ~as.character("not different")))
b_data3 <- b_data3 %>% mutate(significance 
                            = case_when(upper_bound < kc_lower ~as.character("*"), 
                              lower_bound > kc_upper ~as.character("*"), TRUE ~as.character("")))
b_data3 <- b_data3 %>% mutate(suppression= case_when(numerator >0 & numerator< 10 ~as.character("^"), TRUE ~as.character("NA")))
b_data3 <- b_data3 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))

result_b <- subset(b_data3, select=-c(kc_lower, kc_upper))
result_b[order(result_b$indicator_key, b_data3$tab, b_data3$cat1), ]

result_b$data_source <- "birth"
result_b$source_date <- "2021-0901"
result_b$run_date <- "2021-1007"

result_b <- result_b %>% mutate(cat1_group=case_when(cat1_group=='Fed Way-Dash Pt' ~'Fed Way-Dash Point/Woodmont',
                                        TRUE ~cat1_group))

result_b <- result_b[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                         "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc",
                         "significance", "caution", "suppression", "ranks", "source_date", "run_date")]
write.csv(result_b, "result_birth.csv", row.names = F)