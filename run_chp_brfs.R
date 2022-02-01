#generate BRFSS data for City Health Profile, no custom data needed as in CHI
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, naniar, stringr)
setwd("S:/WORK/City Health Profiles/2021/")

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2011)
names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfsraw <- brfsraw %>% rename("pneumvac"="pneuvac3")
brfsraw <- brfsraw %>% dplyr::select(order(colnames(brfsraw)))
names(brfsraw)
brfs <- brfsraw[ , c("year", "age", "hracode", "hra2code", "x_ststr", "kcwt_llcp", "poorhlth",
                     "x_pastaer", "bphigh", "cholchk5", "x_rfchol", "frlt1", "vglt1",
                     "x_crcrec", "denvst1", "fallinjx", "firearm4", "mam2yrs", "pap3yrs", "smi",
                     "fnotlast", "asthnow", "chccopd1", "cvdcorhd", "cvdstrok", "diab2", "disab2","drk_acu2", 
                     "exerany", "flushot",  "genhlth2", "havarth2", "medcost", "menthlth", 
                     "mjpast30", "obese", "overw1",  "persdoc", "pneumvac", "smoker1", "smokless")]
table(brfs$hracode, useNA = "ifany")
brfs <- subset(brfs, hracode!="NA") #exclude cases with hracode missing (5 cases in 2015)
tab1(brfs$x_crcrec,  sort.group="decreasing", cum.percent = FALSE, graph =F) #from package epiDisplay

brfs <- brfs %>% mutate(hra2  = case_when(hra2code=='Auburn' ~'Auburn city',
                                          hra2code=='Seattle' ~'Seattle city',
                                          hra2code=='Bellevue' ~'Bellevue city',
                                          hra2code=='Federal Way' ~'Federal Way city',
                                          hra2code=='Kent' ~'Kent city',
                                          hra2code=='Kirkland' ~'Kirkland city',
                                          hra2code=='Renton' ~'Renton city',
                                          TRUE ~'Other cities'))
tab1(brfs$hra2, graph = F)

#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("x_crcrec", "bphigh", "x_rfchol", "chccopd1", "cvdcorhd", "cvdstrok", 
                                "fallinjx","flushot","havarth2", "overw1", "pneumvac", "disab2"),
                             list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs <- brfs %>% mutate_at(vars("frlt1", "vglt1", "smi"), list(~recode(., '1'='Yes', '2'="No")))
brfs <- brfs %>% mutate(persdoc  = case_when(persdoc =='0' ~'Yes', persdoc  =='1' ~'No'))

brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))
brfs <- brfs %>% mutate(mjpast30 = case_when(mjpast30==0 ~'No', mjpast30<=30 ~'Yes'))

#recode the following factor variables
brfs <- brfs %>% mutate(x_pastaer  = case_when(x_pastaer=='Meet both guidelines' ~'Yes', 
                                               x_pastaer=='does not meet both guidelines' ~'No'))
brfs <- brfs %>% mutate_at(vars("cholchk5", "denvst1"), list(~recode(., 'No'='Yes', 'Yes'="No")))
brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~'Yes',
      fnotlast=='Never true' ~ 'No', fnotlast=="Don't know/Not sure" | fnotlast=='Refused' ~'NA'))

brfs <- brfs %>% mutate(flushot_v1 = case_when(age>=65 ~"NA", TRUE ~flushot))
brfs <- brfs %>% mutate(flushot_v2 = case_when(age>=65 ~flushot, TRUE ~"NA"))
brfs <- brfs %>% mutate(poorhlth_v2 =case_when(age>=65 ~poorhlth))

#convert mam2yrs and pap3yrs from factor to character, then set NA based on age
brfs$mam2yrs <- as.character(brfs$mam2yrs)
brfs$pap3yrs <- as.character(brfs$pap3yrs)
brfs <- brfs %>% mutate(mam2yrs = case_when(age<50 | age>74 ~"NA", TRUE ~mam2yrs))
brfs <- brfs %>% mutate(pap3yrs = case_when(age<21 | age>65 ~"NA", TRUE ~pap3yrs))
brfs$all <- as.character("Total")

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]     #drop cases with missing weight
myvars <- c("asthnow", "chccopd1", "cvdcorhd", "cvdstrok", "diab2", "disab2","drk_acu2", 
             "exerany", "flushot_v1", "flushot_v2",  "genhlth2", "havarth2", "medcost", "menthlth", 
             "mjpast30", "obese", "overw1",  "persdoc", "pneumvac", "smoker1",
             "x_crcrec", "denvst1", "fallinjx", "firearm4", "mam2yrs", "pap3yrs")
myvars2 <- c("x_pastaer", "bphigh", "cholchk5", "x_rfchol", "frlt1", "vglt1", "fnotlast")   #for years 2015, 2017, 2019
myvars3 <- c("poorhlth", "poorhlth_v2") #continuous variable

#Set all "NA" as missing
brfs[] <- lapply(brfs, function(x) {
  is.na(levels(x)) <- levels(x) == "NA"
  x
})

brfs[, myvars] <- lapply(brfs[, myvars], as.character)
brfs[, myvars2] <- lapply(brfs[, myvars2], as.character)
brfs$hracode <- as.character(brfs$hracode)
byvars <- c("all", "hra2",  "hracode")

options(survey.lonely.psu = "adjust")
brfskc <- srvyr::as_survey_design(brfs, ids=1, strata = x_ststr, weights = kcwt_llcp)

#-----Run data for 2016-2020-----
#(2016-2020), (2016, 2018, 2020), (2015, 2017, 2019) This is the most recent, for demgroups analysis

#-----Period 1: 2016-2020 or 2016, 2018, 2020 variables-----
mygrid <- data.frame(expand.grid(myvars = myvars, byvars = byvars))
result.function <- function(X){
  temp <- calc(ph.data = brfskc %>% srvyr::filter(!(is.na(hracode) & is.na(get(paste0(mygrid[X, ]$myvars))) )),
          what = paste0(mygrid[X, ]$myvars),
          year >=2016,
          metrics = c("mean", "rse", "numerator", "denominator"),
          proportion = T,
          by = paste0(mygrid[X, ]$byvars))  
  temp[, byvar := paste0(mygrid[X, ]$byvars)]
  setnames(temp, paste0(mygrid[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid))), result.function), 
  use.names = T, fill = T
)

#-----2015, 2017, 2019 variables-----
mygrid <- data.frame(expand.grid(myvars = myvars2, byvars = byvars))
result2.function <- function(X){
  temp <- calc(ph.data = brfskc %>% srvyr::filter(!(is.na(hracode) & is.na(get(paste0(mygrid[X, ]$myvars))) )),
               what = paste0(mygrid[X, ]$myvars),
               year>=2015 & year<=2019,
               metrics = c("mean", "rse", "numerator", "denominator"),
               proportion = T,
               by = paste0(mygrid[X, ]$byvars))  
  temp[, byvar := paste0(mygrid[X, ]$byvars)]
  setnames(temp, paste0(mygrid[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result2 <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid))), result2.function), 
  use.names = T, fill = T
)

#-----poorhlth, a continuous variable-----
mygrid <- data.frame(expand.grid(myvars = myvars3, byvars = byvars))
result3.function <- function(X){
  temp <- calc(ph.data = brfskc %>% srvyr::filter(!(is.na(hracode) & is.na(get(paste0(mygrid[X, ]$myvars))) )),
               what = paste0(mygrid[X, ]$myvars),
               year >= 2016,
               metrics = c("mean", "rse", "numerator", "denominator"),
               proportion = F,
               by = paste0(mygrid[X, ]$byvars))  
  temp[, byvar := paste0(mygrid[X, ]$byvars)]
  setnames(temp, paste0(mygrid[X, ]$byvars), "byvar_level")
  setcolorder(temp, c("variable", "level", "byvar", "byvar_level"))
  return(temp)
}

result3 <- rbindlist(
  lapply(X = as.list(seq(1, nrow(mygrid))), result3.function), 
  use.names = T, fill = T
)
result3 <- dplyr::select(result3, -c(level))
result3$level <- as.character("Yes")

#----------------------------------------
res_1620 <- rbind(result, result2, result3)
res_1620 <- subset(res_1620, byvar_level!="Other cities")

res_x <- subset(res_1620, subset=level=="Yes", select= -c(level))
remove(result, result2, result3)
res_x <- res_x %>% rename("indicator_key"="variable", "cat1" = "byvar", "cat1_group"="byvar_level", 
                          "result"="mean", "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")

res_x <- res_x %>% mutate(tab = case_when(cat1=='all' ~'_kingcounty', TRUE ~as.character("demgroups")))
res_x <- res_x %>% mutate(cat1 = case_when(cat1=='all' ~'King County', cat1=="hra2" ~"Big cities",
                                                   TRUE ~"Cities/neighborhoods"))
res_x <- res_x %>% mutate(cat1_group= case_when(cat1_group=='Total' ~'King County', TRUE ~as.character(cat1_group)))

#-----King County average for significance comparison-----
res_kc <- subset(res_x, subset=res_x$tab=="_kingcounty", 
                    select= c("indicator_key", "result", "lower_bound", "upper_bound" ))
res_kc <- res_kc %>% rename("kc_result" = "result", "kc_lower" = "lower_bound", "kc_upper"="upper_bound")

#-----ranking HRA level data by result
res_hra <- subset(res_x, subset=res_x$cat1=="Cities/neighborhoods")
res_hra <- res_hra %>% group_by(year, indicator_key) %>% mutate(ranks=rank(-result))
res_oth <- subset(res_x, subset=res_x$cat1!="Cities/neighborhoods")
res_all1 <- plyr::rbind.fill(res_hra, res_oth)

res_all2 <-merge(res_all1, res_kc, by="indicator_key")
res_all2 <- res_all2 %>% mutate(comparison_with_kc = case_when(upper_bound < kc_lower ~as.character("lower"), 
                                          lower_bound > kc_upper ~as.character("higher"), 
                                          TRUE ~as.character("not different")))

res_all2 <- res_all2 %>% mutate(significance = case_when(upper_bound < kc_lower ~as.character("*"), 
                                                  lower_bound > kc_upper ~as.character("*"), 
                                                  TRUE ~as.character("")))
res_all2 <- res_all2 %>% mutate(suppression= case_when(denominator < 50 ~as.character("^"), TRUE ~as.character("NA")))
res_all2 <- res_all2 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
res_all2[c("result", "lower_bound", "upper_bound")][which(res_all2$denominator<50), ] <-NA

res_all2 <- res_all2 %>% mutate(comparison_with_kc = case_when(tab=='_kingcounty' ~"NA", TRUE ~comparison_with_kc))
res_all2 <- res_all2 %>% mutate(significance = case_when(tab=='_kingcounty' ~"NA", TRUE ~significance))
res_all2 <- res_all2 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
res_all2 <- subset(res_all2, select=-c(kc_result, kc_lower, kc_upper))
res_all2 <- res_all2 %>% dplyr::filter(!is.na(cat1_group))

res_wa <- read.csv("resultwa.csv")
res_all3 <- bind_rows(res_all2, res_wa)  #function in dplyr to merge 2 dfs with different number of columns

brfsvaryr <- read.csv("brfsvaryr.csv")
res_brfs <- merge(res_all3, brfsvaryr, by="indicator_key")

res_brfs$data_source <- "brfss"
res_brfs$source_date <- "2021-0901"
res_brfs$run_date <- "2021-1007"
res_brfs <- subset(res_brfs, select=-c(year1418, year1519))
res_brfs <- res_brfs %>% rename("year"="year1620")

res_brfs <- res_brfs[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                         "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc",
                         "significance", "caution", "suppression", "ranks", "source_date", "run_date")]
res_brfs[order(res_brfs$indicator_key, res_brfs$tab, res_brfs$cat1), ]
write.csv(res_brfs, "res_brfs.csv", row.names = F)