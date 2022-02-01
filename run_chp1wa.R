#generate BRFSS data for City Health Profile, no custom data as in CHI
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, conflicted,data.table, janitor, rads)
setwd("S:/WORK/City Health Profiles/2021/")

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfs1wa <- read.dta(file="c:/R_learning/wa_1520.dta", warn.missing.labels = FALSE)
brfs1 <- brfs1wa %>% dplyr::select(order(colnames(brfs1wa)))
names(brfs1)<-gsub("^_", "x_", names(brfs1)) #change variable names start with "_"

brfs1 <- brfs1 %>% mutate(county = case_when(year>2017~ctycode2, TRUE ~ctycode1))
brfs1$county <- as.character(brfs1$county)
brfs1 <- brfs1 %>% mutate(county = case_when(county=="Don't know" | county=="Other State" | county=="Refused" ~"NA",
                                              TRUE ~county))
brfs1$county <- as.factor(brfs1$county)
brfs1$age <- as.numeric(brfs$age)

brfs1 <- brfs1 %>% mutate(x_pastaer = case_when(year==2019~x_pastae2, TRUE ~x_pastae1))
brfs1 <- brfs1 %>% mutate(pneuvac3 = case_when(year>2017~pneuvac4, TRUE ~pneuvac3))
brfs1 <- brfs1 %>% mutate(chccopd1 = case_when(year>2018~chccopd2, TRUE ~chccopd1))

brfs1 <- brfs1 %>% mutate(cholchk = case_when(cholchk=="Past Year" | cholchk=="1-2 years" | cholchk=="2-5 years" ~"Yes",
                                            cholchk=="5+ years" | cholchk=="Never" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(cholchk1 = case_when(cholchk1=="Past Year" | cholchk1=="1-2 years" | cholchk1=="2-5 years" ~"Yes",
                                                  cholchk1=="5+ years" | cholchk1=="Never" ~"No", TRUE ~"NA"))

brfs1 <- brfs1 %>% mutate(cholchk2 = case_when(cholchk2=="Past year" | cholchk2=="1-2 years" |  cholchk2=="2-3 years" |
                                                  cholchk2=="3-4 years" | cholchk2=="4-5 years" ~"Yes",
                                                  cholchk2=="5+ years" | cholchk2=="Never" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(cholchk = case_when(year==2017~cholchk1, year==2019~cholchk2, TRUE ~cholchk))
table(brfs1$year, brfs1$cholchk) #reverse yes/no below
brfs1 <- brfs1 %>% mutate(cholchk = case_when(cholchk=="Yes" ~"No", cholchk=="No" ~"Yes", TRUE ~"NA"))

brfs1 <- brfs1 %>% mutate(x_rfchol = case_when(year==2017~x_rfchol1, year==2019~x_rfchol2, TRUE ~x_rfchol))
brfs1 <- brfs1 %>% mutate(x_rfchol = case_when(x_rfchol=="Yes" ~"Yes", x_rfchol=="No" ~"No", TRUE ~"NA"))

brfs1 <- brfs1 %>% mutate(x_denvst2 = case_when(x_denvst2=="Yes" ~"Yes", x_denvst2=="No" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(x_denvst3 = case_when(x_denvst3=="Yes" ~"Yes", x_denvst3=="No" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(denvst1 = case_when(year>=2018 ~x_denvst3, TRUE ~x_denvst2))
table(brfs1$year, brfs1$denvst1) #reverse yes/no below
brfs1 <- brfs1 %>% mutate(denvst1 = case_when(denvst1=="Yes" ~"No", denvst1=="No" ~"Yes", TRUE ~"NA"))

brfs1$asthnow <- as.character(brfs1$asthnow)
brfs1$asthma3 <- as.character(brfs1$asthma3)
brfs1 <- brfs1 %>% mutate(asthnow  = case_when(asthma3=="No" ~"No", TRUE ~asthnow))
brfs1 <- brfs1 %>% mutate(asthnow  = case_when(asthnow=="Yes" ~"Yes", asthnow=="No" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(diabete3 = case_when(diabete3=="Yes" ~"Yes", 
                                               diabete3=="Yes, in pregnancy" ~"No",
                                               diabete3=="No" ~"No",
                                               diabete3=="Borderline/Pre-Diabetes" ~"No",
                                               TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(diabete4 = case_when(diabete4=="Yes" ~"Yes", 
                                               diabete4=="Yes, in pregnancy" ~"No",
                                               diabete4=="No" ~"No",
                                               diabete4=="Pre-diabetes" ~"No",
                                               TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(diab2 = case_when(year==2019 ~diabete4, TRUE ~diabete3))

brfs1 <- brfs1 %>% mutate_at(vars("deaf", "blind", "decide", "diffwalk", "diffdres", "diffalon"), 
                             list(~recode(., 'Yes'=1, "No"=0, .default =NULL)))
brfs1$disab1 <- rowSums(brfs1[, c("deaf", "blind", "decide", "diffwalk", "diffdres","diffalon")], na.rm=T)
brfs1 <- brfs1 %>% mutate(disab2 = case_when(disab1<2 ~ 'No', disab1>1 & disab1<=6 ~ 'Yes', TRUE ~"NA"))

brfs1 <- brfs1 %>% mutate_at(vars("fallinj2", "fallinj3", "fallinj4"), 
                             list(~case_when(.<77 ~ "Yes", .==88 ~"No", TRUE ~"NA")))
brfs1 <- brfs1 %>% mutate(fallinjx = case_when(year==2018~fallinj3, year==2020~fallinj4, TRUE ~fallinj2))
brfs1 <- brfs1 %>% mutate(fallinjx = case_when(fall12mn==88 ~"No", TRUE ~fallinjx))

brfs1 <- brfs1 %>% mutate(genhlth2 = case_when(genhlth=="Excellent" | genhlth=="Very Good" | genhlth=="Good" ~"No",
                                               genhlth=="Fair" | genhlth=="Poor" ~"Yes", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(obese = case_when(x_bmi5cat=="Obese" ~"Yes",  
                                  x_bmi5cat=="Under" | x_bmi5cat=="Normal" | x_bmi5cat=="Over" ~"No", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(havarth2 = case_when(year>=2019~havarth4, TRUE ~havarth3))
brfs1 <- brfs1 %>% mutate(pap3yrs = case_when(year==2018~x_rfpap34, year==2020~x_rfpap35, TRUE ~x_rfpap33))
brfs1 <- brfs1 %>% mutate(pneumvac = case_when(year>=2018~pneuvac4, TRUE ~pneuvac3))
brfs1 <- brfs1 %>% mutate(flushot = case_when(year>=2019~flushot7, TRUE ~flushot6))
brfs1$poorhlth <- as.double(brfs1$poorhlth)
brfs1 <- brfs1 %>% mutate(poorhlth = case_when(menthlth==88 | physhlth==88 ~88, TRUE ~poorhlth))

names(brfs1)
#rename variables to be consistent with the KC data
brfs1 <- brfs1 %>% rename("bphigh"="bphigh4", "cholchk5"="cholchk",  
                        "cvdcorhd"="cvdcrhd4", "cvdstrok"="cvdstrk3",
                        "drk_acu2" = "x_rfbing5", "exerany" = "exerany2",
                        "frlt1" = "x_frtlt1", "vglt1" = "x_veglt1", 
                        "mam2yrs" = "x_rfmam2y", "overw1" = "x_bmi5cat",
                          "persdoc" = "persdoc2", "smoker1" = "x_rfsmok3", "smokless" = "usenow3")

brfs <- brfs1[ , c("year", "age", "x_ststr", "x_llcpwt", "poorhlth",
                     "x_pastaer", "bphigh", "cholchk5", "x_rfchol", "frlt1", "vglt1",
                     "x_crcrec", "denvst1", "fallinjx", "firearm4", "mam2yrs", "pap3yrs", "smi",
                     "fnotlast",
                     "asthnow", "chccopd1", "cvdcorhd", "cvdstrok", "diab2", "disab2","drk_acu2", 
                     "exerany", "flushot",  "genhlth2", "havarth2", "medcost", "menthlth", 
                     "mjpast30", "obese", "overw1",  "persdoc", "pneumvac", "smoker1", "smokless")]
#recode
brfs <- brfs %>% mutate_at(vars("asthnow", "chccopd1", "x_crcrec", "cvdcorhd", "cvdstrok", "drk_acu2", "exerany", 
                               "firearm4", "flushot", "havarth2", "mam2yrs", "pap3yrs", "medcost", 
                               "pneumvac", "smoker1"), 
                               list(~case_when(.=='Yes' ~"Yes", .=='No' ~"No", TRUE ~"NA")))

brfs <- brfs %>% mutate(x_pastaer = case_when(x_pastaer=='Met both Recs' ~'Yes', 
                                              x_pastaer=='Did not meet both Recs' ~'No', TRUE ~"NA"))
brfs <- brfs %>% mutate(bphigh = case_when(bphigh=="Yes" ~"Yes", 
                                           bphigh=="No" ~"No",
                                           bphigh=="Yes, in pregnancy" ~"No",
                                           bphigh=="Told Borderline" ~"No",
                                           TRUE ~"NA"))

table(brfs$year,brfs$exerany) #reverse yes/no below
brfs <- brfs %>% mutate(exerany = case_when(exerany=="Yes" ~"No", exerany=="No" ~"Yes", TRUE ~"NA"))
brfs <- brfs %>% mutate(pneumvac = case_when(pneumvac=="Yes" ~"No", pneumvac=="No" ~"Yes", TRUE ~"NA"))
tab1(brfs$overw1)
brfs <- brfs %>% mutate(overw1 = case_when(overw1=="Over" ~"Yes", 
                                           overw1=="Under" | overw1=="Normal" | overw1=="Obese" ~"No",
                                           TRUE ~"NA"))

brfs <- brfs %>% mutate_at(vars("frlt1", "vglt1"), 
                             list(~case_when(.=="One or more per day" ~"Yes", 
                                            .=="Less than once per day" ~"No", TRUE ~"NA")))
brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 | menthlth==88 ~'No', menthlth>=14 & menthlth<=30 ~'Yes', TRUE ~"NA"))
table(brfs$year, brfs$menthlth)

brfs <- brfs %>% mutate(mjpast30 = case_when(mjpast30==88  ~'No', mjpast30<=30 ~'Yes', TRUE ~"NA"))

brfs$poorhlth <- as.numeric(as.character(brfs$poorhlth))
brfs <- brfs %>% mutate(poorhlth = case_when(poorhlth==88  ~0,
                                             poorhlth>=1 & poorhlth<=30 ~poorhlth,
                                             TRUE ~NA_real_))


brfs <- brfs %>% mutate(persdoc = case_when(persdoc=="Yes" | persdoc=='More than 1' ~'Yes', 
                                            persdoc =='No' ~'No', TRUE ~"NA"))
brfs <- brfs %>% mutate(persdoc = case_when(persdoc=="Yes" ~"No", persdoc=="No" ~"Yes", TRUE ~"NA"))
brfs1 <- brfs1 %>% mutate(persdoc = case_when(persdoc=="Yes" ~"No", persdoc=="No" ~"Yes", TRUE ~"NA"))
brfs <- brfs %>% mutate(smokless = case_when(smokless=='Every Day' | smokless=="Some days" ~'Yes',
                                    smokless =='Not at all' ~'No', TRUE ~"NA"))

brfs <- brfs %>% mutate(flushot_v1 = case_when(age>=65 ~"NA", TRUE ~flushot))
brfs <- brfs %>% mutate(flushot_v2 = case_when(age>=65 ~flushot, TRUE ~"NA"))
brfs <- brfs %>% mutate(poorhlth_v2 =case_when(age>=65 ~poorhlth))
brfs <- brfs %>% mutate(mam2yrs = case_when(age<50 | age>74 ~"NA", TRUE ~mam2yrs))
brfs <- brfs %>% mutate(pap3yrs = case_when(age<21 | age>65 ~"NA", TRUE ~pap3yrs))

brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~"Yes",
                                             fnotlast=='Never true' ~"No",  TRUE ~"NA"))

myvars <- c("asthnow", "chccopd1", "cvdcorhd", "cvdstrok", "diab2", "disab2","drk_acu2", 
            "exerany", "flushot_v1", "flushot_v2",  "genhlth2", "havarth2", "medcost", "menthlth", 
            "mjpast30", "obese", "overw1",  "persdoc", "pneumvac", "smoker1", "smokless",
            "x_crcrec", "denvst1", "fallinjx", "firearm4", "mam2yrs", "pap3yrs", "smi")
myvars2 <- c("x_pastaer", "bphigh", "cholchk5", "x_rfchol", "frlt1", "vglt1", "fnotlast")   #for years 2015, 2017, 2019
myvars3 <- c("poorhlth", "poorhlth_v2") #continuous variable
allvars <-c(myvars, myvars2)
brfs$all <- "WA"

#Set all "NA" as missing
brfs[] <- lapply(brfs, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

brfs[, myvars] <- lapply(brfs[, myvars], as.character)
brfs[, myvars2] <- lapply(brfs[, myvars2], as.character)

#-----set survey design weight, myvars are 2015-2019 variables for all adults-----
brfs <- brfs[complete.cases(brfs$x_llcpwt), ]     #drop cases with missing weight
brfs$x_llcpwt <- as.numeric(brfs$x_llcpwt)
options(survey.lonely.psu = "adjust")
brfswa <- srvyr::as_survey_design(brfs, ids=1, strata = x_ststr, weights = x_llcpwt)
brfs2 <- subset(brfs, year==2015 | year==2017 | year==2019)
brfs2wa <- srvyr::as_survey_design(brfs2, ids=1, strata = x_ststr, weights = x_llcpwt)

result1 <- calc(ph.data = brfswa, 
    what = myvars,
    year > 2015, 
    metrics = c("mean", "rse", "numerator", "denominator"),
    proportion = T,
    by = "all")

result2 <- calc(ph.data = brfs2wa,
                what = myvars2,
                metrics = c("mean", "rse", "numerator", "denominator"),
                proportion = T,
                by = "all")

result3 <- calc(ph.data = brfswa, 
                what = myvars3,
                year > 2015, 
                metrics = c("mean", "rse", "numerator", "denominator"),
                proportion = F,
                by = "all")

resultwa <- rbind(result1, result2)
resultwa <- subset(resultwa, subset=level=="Yes")
resultwa <- rbind(resultwa, result3)
resultwa <- subset(resultwa, select=-c(level))

resultwa <- resultwa %>% rename("indicator_key"="variable", "cat1_group"="all", "result"="mean", 
                                  "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")
resultwa <- resultwa %>% mutate(tab = case_when(cat1_group=='WA' ~'_wastate', TRUE ~as.character("NA")))
resultwa <- resultwa %>% mutate(cat1 = case_when(cat1_group=='WA' ~'Washington State',TRUE ~as.character("NA")))
resultwa <- resultwa %>% mutate(cat1_group = case_when(cat1_group=='WA' ~'Washington State',TRUE ~as.character("NA")))

resultwa <- resultwa[, c("tab","indicator_key", "cat1", "cat1_group", 
                       "result", "se", "lower_bound", "upper_bound", "rse", "numerator", "denominator")]
write.csv(resultwa, "resultwa.csv", row.names = F)
