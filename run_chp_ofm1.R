#run CHAT generated 2020 OFM population data. the imported Excel files have been manually processed
#for population data, confidence interval and se/rse do not apply so there are no comparisons with King County
#numerator is the population count for the subgroup
rm(list=ls())
pacman::p_load(dplyr, foreign, readxl, epiDisplay, data.table, janitor, rads, naniar)
setwd("S:/WORK/City Health Profiles/2021/")
#merge files: age and race data by HRA, Big Cities, and KCWA

#-----process HRA data
hraname <- read_excel("S:/WORK/City Health Profiles/2021/CHPdeath1620.xlsx", "hraname")
hratotal <- read_excel("CHPofm2020.xlsx", "hra_total")
hratotal <- hratotal %>% rename("denominator" = "Population")
ofm_hra <- read_excel("CHPofm2020.xlsx", "pop_hra")
ofm_hra <- ofm_hra %>% rename("numerator" = "Population")
ofm_hra <- merge(x=ofm_hra, y=hratotal, by=c("Year", "Geography"))
ofm_hra <- merge(x=ofm_hra, y=hraname, by="Geography", all=T)
ofm_hra$result <- ofm_hra$numerator/ofm_hra$denominator
ofm_hra <- ofm_hra %>% group_by(Year, indicator_key) %>% mutate(ranks=rank(-result, ties.method ="random"))
ofm_hra <- subset(ofm_hra, select = - c(Geography, exclude))
ofm_hra$tab <- "demgroups"
ofm_hra$cat1 <- "Cities/neighborhoods"

bigtotal <- read_excel("CHPofm2020.xlsx", "big_total")
bigtotal <- bigtotal %>% rename("denominator" = "Population")
ofm_big <- read_excel("CHPofm2020.xlsx", "pop_big")
ofm_big <- ofm_big %>% rename("numerator" = "Population")
ofm_big <- merge(x=ofm_big, y=bigtotal, by=c("Year", "Geography"))
ofm_big$result <- ofm_big$numerator/ofm_big$denominator
ofm_big <- ofm_big %>% rename("cat1_group" = "Geography")
ofm_big$tab <- "demgroups"
ofm_big$cat1 <- "Big cities"

kcwatotal <- read_excel("CHPofm2020.xlsx", "kcwa_total")
kcwatotal <- kcwatotal %>% rename("denominator" = "Population")
ofm_kcwa <- read_excel("CHPofm2020.xlsx", "pop_kcwa")
ofm_kcwa <- ofm_kcwa %>% rename("numerator" = "Population")
ofm_kcwa <- merge(x=ofm_kcwa, y=kcwatotal, by=c("Year", "Geography"))
ofm_kcwa$result <- ofm_kcwa$numerator/ofm_kcwa$denominator
ofm_kcwa <- ofm_kcwa %>% rename("cat1_group" = "Geography")

ofm_kcwa <- ofm_kcwa %>% mutate(tab=case_when(cat1_group=='King' ~'_kingcounty',
                                              cat1_group=='State Total' ~'_wastate', TRUE ~""))
ofm_kcwa <- ofm_kcwa %>% mutate(cat1=case_when(cat1_group=='King' ~'King County',
                                              cat1_group=='State Total' ~'Washington State', TRUE ~""))
ofm_kcwa <- ofm_kcwa %>% mutate(cat1_group=case_when(cat1_group=='King' ~'King County',
                                               cat1_group=='State Total' ~'Washington State', TRUE ~""))

#-----Combine HRA, Big, and KCWA data-----
ofm_data1 <- bind_rows(ofm_hra, ofm_big, ofm_kcwa)
ofm_data1$data_source <- "OFM"
ofm_data1$source_date <- "2021-0901"
ofm_data1$run_date    <- "2021-1007"
ofm_data1 <- ofm_data1 %>% mutate(indicator_key=case_when(
                                  indicator_key=='American Indian/Alaskan Native Only-NH' ~'AIAN Only-NH',
                                  indicator_key=='Pacific Islander Only-NH' ~'NHPI Only-NH', TRUE ~indicator_key))

ofm_data1 <- ofm_data1 %>% mutate(cat1_group=case_when(cat1_group=='Fed Way-Dash Pt' ~'Fed Way-Dash Point/Woodmont',
                                                       TRUE ~cat1_group))
ofm_data1 <- ofm_data1 %>% rename("year" = "Year")

ofm_data1 <- ofm_data1[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                          "numerator", "denominator", "ranks", "source_date", "run_date")]
write.csv(ofm_data1, "result_ofm.csv", row.names = F)