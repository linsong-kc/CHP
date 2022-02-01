#run CHAT generated death data
rm(list=ls())
pacman::p_load(dplyr, foreign, readxl, epiDisplay, data.table, janitor, rads, naniar)
setwd("S:/WORK/City Health Profiles/2021/")
#merge files: death and LE0: HRA/SE, Big/SE, KCWA/SE, merge with cause_of_death file and HRA name fil

#-----process HRA data
d_hra1 <- read_excel("CHPdeath1620.xlsx", "hra_data")
d_hra1 <- subset(d_hra1, select = -c(Age, Gender, Race, Ethnicity))
d_hra1 <- d_hra1 %>% rename("cause_of_death" = "Cause Of Death", "year" = "Year", "result"="Age-Adjusted Rate",
              "lower_bound" = "Age-Adjusted Lower CI", "upper_bound" = "Age-Adjusted Upper CI",
              "numerator" = "Count", "denominator" = "Population")
d_hra1$cause_of_death <- gsub("^#", "", d_hra1$cause_of_death) #change variable names start with "#"
d_hra1$cause_of_death <- gsub("^#", "", d_hra1$cause_of_death) #change variable names start with "#"

d_hra2 <- read_excel("CHPdeath1620.xlsx", "hra_se")
d_hra2 <- d_hra2[-c(1, 3, 5, 7:9)]
d_hra2 <- d_hra2 %>% rename("year" = "Year_Name", "cause_of_death" = "Cause Of Death_Name", "Geography" = "Geography_Name")
d_hra3 <- inner_join(d_hra1, d_hra2, by = c("cause_of_death", "year", "Geography"))

#-----------
le0_hra1 <- read_excel("CHPle1620.xlsx", "le_hra")
le0_hra1 <- subset(le0_hra1, select = -c(Age, Gender, Race, Ethnicity))
le0_hra1 <- le0_hra1 %>% rename("year" = "Year", "result"="Rate",
                            "lower_bound" = "Lower CI", "upper_bound" = "Upper CI")
le0_hra1$cause_of_death <- "LE0"

le0_hra2 <- read_excel("CHPle1620.xlsx", "le_hra_se")
le0_hra2 <- le0_hra2[-c(1, 3, 5:7)]
le0_hra2$cause_of_death <- "LE0"
le0_hra2 <- le0_hra2 %>% rename("year" = "Year_Name", "Geography" = "Geography_Name")
le0_hra3 <- inner_join(le0_hra1, le0_hra2, by = c("cause_of_death", "year", "Geography"))
d_hra4 <- bind_rows(d_hra3, le0_hra3)

hraname <- read_excel("S:/WORK/City Health Profiles/2021/CHPdeath1620.xlsx", "hraname")
d_hra5 <- merge(x = d_hra4, y=hraname, by="Geography", all = T)
d_hra5$cat1 <- "Cities/neighborhoods"
d_hra5 <- subset(d_hra5, select = - c(Geography, exclude)) #excluding in big cities HRAs
d_hra5 <- d_hra5 %>% group_by(cause_of_death) %>% mutate(ranks=rank(-result, ties.method ="random"))

#------------------------
#process Big citie data
d_big1 <- read_excel("CHPdeath1620.xlsx", "big_data")
d_big1 <- subset(d_big1, select = -c(Age, Gender, Race, Ethnicity))
d_big1 <- d_big1 %>% rename("cause_of_death" = "Cause Of Death", "year" = "Year", "result"="Age-Adjusted Rate",
                            "lower_bound" = "Age-Adjusted Lower CI", "upper_bound" = "Age-Adjusted Upper CI",
                            "numerator" = "Count", "denominator" = "Population")
d_big1$cause_of_death <- gsub("^#", "", d_big1$cause_of_death) #change variable names start with "#"
d_big1$cause_of_death <- gsub("^#", "", d_big1$cause_of_death) #change variable names start with "#"

d_big2 <- read_excel("CHPdeath1620.xlsx", "big_se")
d_big2 <- d_big2[-c(1, 3, 5, 7:9)]
d_big2 <- d_big2 %>% rename("year" = "Year_Name", "cause_of_death" = "Cause Of Death_Name", "Geography" = "Geography_Name")
d_big3 <- inner_join(d_big1, d_big2, by = c("cause_of_death", "year", "Geography"))

le0_big1 <- read_excel("CHPle1620.xlsx", "le_big")
le0_big1 <- subset(le0_big1, select = -c(Age, Gender, Race, Ethnicity))
le0_big1 <- le0_big1 %>% rename("year" = "Year", "result"="Rate",
                                "lower_bound" = "Lower CI", "upper_bound" = "Upper CI")
le0_big2 <- read_excel("CHPle1620.xlsx", "le_big_se")
le0_big2 <- le0_big2[-c(1, 3, 5:7)]
le0_big2 <- le0_big2 %>% rename("year" ="Year_Name", "Geography" = "Geography_Name")
le0_big3 <- inner_join(le0_big1, le0_big2, by = c("year","Geography"))
le0_big3$cause_of_death <- "LE0"

d_big4 <- bind_rows(d_big3, le0_big3)
d_big4 <- d_big4 %>% rename("cat1_group" = "Geography")
d_big4$cat1 <- "Big cities"

#-----Combine HRA data with big cities
d_bigx <- bind_rows(d_hra5, d_big4)
d_bigx$tab <- "demgroups"

#------------------------
#process KCWA data
d_kcwa1 <- read_excel("CHPdeath1620.xlsx", "kcwa_data")
d_kcwa1 <- subset(d_kcwa1, select = -c(Age, Gender, Race, Ethnicity))
d_kcwa1 <- d_kcwa1 %>% rename("cause_of_death" = "Cause Of Death", "year" = "Year", "result"="Age-Adjusted Rate",
                            "lower_bound" = "Age-Adjusted Lower CI", "upper_bound" = "Age-Adjusted Upper CI",
                            "numerator" = "Count", "denominator" = "Population")
d_kcwa1$cause_of_death <- gsub("^#", "", d_kcwa1$cause_of_death) #change variable names start with "#"
d_kcwa1$cause_of_death <- gsub("^#", "", d_kcwa1$cause_of_death) #change variable names start with "#"
#d_kcwa1[, 4:8] <- lapply(d_kcwa1[, 4:8], as.numeric)

d_kcwa2 <- read_excel("CHPdeath1620.xlsx", "kcwa_se")
d_kcwa2 <- d_kcwa2[-c(1, 3, 5, 7:9)]
d_kcwa2 <- d_kcwa2 %>% rename("year" = "Year_Name", "cause_of_death" = "Cause Of Death_Name", "Geography" = "Geography_Name")
d_kcwa3 <- inner_join(d_kcwa1, d_kcwa2, by = c("cause_of_death", "year", "Geography"))

#-----
le0_kcwa1 <- read_excel("CHPle1620.xlsx", "le_kcwa")
le0_kcwa1 <- subset(le0_kcwa1, select = -c(Age, Gender, Race, Ethnicity))
le0_kcwa1 <- le0_kcwa1 %>% rename("year" = "Year", "result"="Rate",
                                "lower_bound" = "Lower CI", "upper_bound" = "Upper CI")
le0_kcwa1$cause_of_death <- "LE0"

le0_kcwa2 <- read_excel("CHPle1620.xlsx", "le_kcwa_se")
le0_kcwa2 <- le0_kcwa2[-c(1, 3, 5:7)]
le0_kcwa2$cause_of_death <- "LE0"
le0_kcwa2 <- le0_kcwa2 %>% rename("year" = "Year_Name", "Geography" = "Geography_Name")
le0_kcwa3 <- inner_join(le0_kcwa1, le0_kcwa2, by = c("cause_of_death", "year", "Geography"))
d_kcwa4 <- bind_rows(d_kcwa3, le0_kcwa3)
d_kcwa4 <- d_kcwa4 %>% rename("cat1_group" = "Geography")
d_kcwa4 <- d_kcwa4 %>% mutate(tab = case_when(cat1_group=="King" ~"_kingcounty", 
                                              cat1_group=="State Total" ~ "_wastate"))
d_kcwa4 <- d_kcwa4 %>% mutate(cat1 = case_when(cat1_group=="King" ~"King County", 
                                               cat1_group=="State Total" ~ "Washington State"))
d_kcwa4 <- d_kcwa4 %>% mutate(cat1_group = case_when(cat1_group=="King" ~"King County", 
                                                     cat1_group=="State Total" ~ "Washington State"))

#-----Combine Big cities and KCWA data-----
d_data1 <- bind_rows(d_bigx, d_kcwa4)  #function in dplyr to merge 2 dfs with different number of columns
d_data1 <- d_data1 %>% rename("se" = "SE")
d_data1$rse <- 100*d_data1$se / d_data1$result

deathicd <- read_excel("CHPdeath1620.xlsx", "death_icd")
d_data2 <- merge(x =d_data1, y=deathicd, by="cause_of_death", all = T)

d_data2 <- d_data2[ , c("indicator_key", "year","tab", "cat1", "cat1_group", "result", "se", "rse",
                       "lower_bound", "upper_bound", "numerator", "denominator", "ranks", "cause_of_death")]

#-----King County big cities data, compare to Kc data -----
d_kc1 <- subset(d_data2, subset=tab=="_kingcounty", select= c(indicator_key, lower_bound, upper_bound))
d_kc1 <- d_kc1 %>% rename("kc_lower"="lower_bound", "kc_upper"="upper_bound")

#-----King County big cities data, for ranking the 26 City/Area (hra2code) by result
d_data3 <-merge(d_data2, d_kc1, by=c("indicator_key"))
d_data3 <- d_data3 %>% mutate(comparison_with_kc 
                            = case_when(upper_bound < kc_lower ~as.character("lower"), 
                                        lower_bound > kc_upper ~as.character("higher"), 
                                        TRUE ~as.character("not different")))
d_data3 <- d_data3 %>% mutate(significance 
                            = case_when(upper_bound < kc_lower ~as.character("*"), 
                              lower_bound > kc_upper ~as.character("*"), TRUE ~as.character("")))
d_data3 <- d_data3 %>% mutate(suppression= case_when(numerator >0 & numerator< 10 ~as.character("^"), TRUE ~as.character("NA")))
d_data3 <- d_data3 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))

result_d <- subset(d_data3, select=-c(kc_lower, kc_upper))
result_d$data_source <- "death"
result_d$source_date <- "2021-0901"
result_d$run_date <- "2021-1007"

result_d <- result_d %>% mutate(cat1_group=case_when(cat1_group=='Fed Way-Dash Pt' ~'Fed Way-Dash Point/Woodmont',
                                 cat1=='Big cities' & cat1_group=='Bellevue' ~'Bellevue city',
                                 cat1=='Big cities' & cat1_group=='Federal Way City' ~'Federal Way city',
                                 cat1=='Big cities' & cat1_group=='Kirkland City' ~'Kirkland city',
                                 cat1=='Big cities' & cat1_group=='Seattle' ~'Seattle city',
                                        TRUE ~cat1_group))

result_d <- result_d[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                         "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc",
                         "significance", "caution", "suppression", "ranks", "source_date", "run_date")]
write.csv(result_d, "result_death.csv", row.names = F)