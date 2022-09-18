
library(Hmisc)
library(summarytools)
library(tidyverse)
library(readstata13)
library(haven)
library(countrycode)

rec_orig <- read.dta13("downloaded/data/10080_da_en_v1_0.dta")
rec <- rec_orig
ctable(rec$W1_Q9, rec$W1_Q4)
freq(rec$W1_INCOME_DE)

#variables#####
# W1_S0         cntry
# ID            persID
# W1_R          wave

# W1_Q2         gender (2 levels)
# W1_Q1b        age
# W1_Q_edu      edu
# W1_Q4         citizen (already filtered in data)
# w1_Q16        rile (11 levels, needs inversion)
# W1_Q75        unemp (4 and 5 to 1, rest 0)

# W1_Q12        deprivation sociotropic retro last 12 months (needs inversion)
# W1_Q13        deprivation personal retro last 12 months (needs inversion)
# W1_Q79        deprivation personal current (4 levels)

# W1_Q39_3      anti_immA (needs inversion)
# W1_Q39_4      anti_immB (needs inversion)
# W1_Q39_9      anti_immC (needs inversion)

# W1_Q11        dissat_gov (needs inversion)
# W1_Q9         dissat_dem (needs inversion)

# W1_Q14_3      trust_parl (needs inversion)
# W1_Q14_4      trust_gov  (needs inversion)

# W1_Q57_XX_Y   rrp_sym (country number 10-70 and party number 1-6)
# W1_Q64_X      populistA B C D E F G (1-7) 2 4 6 need inversion

#recode country variable####
freq(rec$W1_S0)
rec$cntry <- as.numeric(rec$W1_S0)
rec$cntry <- as.factor(rec$cntry)
levels(rec$cntry) <- c("DE","FR","ES","HU","PL","DK","IT")
freq(rec$cntry)

#coding rrp_sym variable####
#done before variable selecting because different variables are merged into one
rec$rrp_sym <- NA

#for Germany defining AfD as rrp
rec$rrp_sym[rec$cntry == "DE"] <- rec$W1_Q57_10_6[rec$cntry == "DE"]
#for France defning FN/RN as rrp
rec$rrp_sym[rec$cntry == "FR"] <- rec$W1_Q57_20_6[rec$cntry == "FR"]
#for Spain defining VOX as rrp
rec$rrp_sym[rec$cntry == "ES"] <- rec$W1_Q57_30_5[rec$cntry == "ES"]
#for Hungary defining Fidesz as rrp, other option: Jobbik (1)
rec$rrp_sym[rec$cntry == "HU"] <- rec$W1_Q57_40_6[rec$cntry == "HU"]
#for Poland defining PiS as rrp, other option: Kukiz´15 (5)
rec$rrp_sym[rec$cntry == "PL"] <- rec$W1_Q57_50_4[rec$cntry == "PL"]
#for Denmark defining DF as rrp
rec$rrp_sym[rec$cntry == "DK"] <- rec$W1_Q57_60_1[rec$cntry == "DK"]
#for italy defining Lega as rrp, other option: Fratelli d'Italia (5) 
rec$rrp_sym[rec$cntry == "IT"] <- rec$W1_Q57_70_6[rec$cntry == "IT"]
ctable(rec$rrp_sym, rec$cntry)
freq(rec$W1_Q57_40_6)

#generating rrp_in_gov Variable####
#check fieldwork period
rec$int_date <- paste0(rec$W1_Day_End, sep = "-", rec$W1_Month_End, sep = "-", rec$W1_Year_End)
freq(rec$int_date)

#wave 1 fieldwork period: 2.-22.4.2019
#set to 0 for all respondents in all countries
rec$rrp_in_gov <- 0

#set 1 for specific countries
rec$rrp_in_gov[rec$cntry == "HU"] <- 1 #Fidesz in Gov
rec$rrp_in_gov[rec$cntry == "IT"] <- 1 #L in Gov
rec$rrp_in_gov[rec$cntry == "PL"] <- 1 #PiS in Gov
rec$rrp_in_gov[rec$cntry == "DK"] <- 1 #gov supported by DF

#define variables to keep####
vars_to_keep <-  c("ID","cntry","W1_R","W1_Q1b","W1_Q2","W1_Q_edu","W1_Q16","W1_Q75", #socio-demographics
                   "W1_Q12","W1_Q13", "W1_Q79", #economic deprivation
                   "W1_Q39_3", "W1_Q39_4", "W1_Q39_9",#anti-immigrants
                   "W1_Q11", "W1_Q9",  #dissatisfaction
                   "W1_Q14_3", "W1_Q14_4", #trust
                   "rrp_sym","rrp_in_gov"
) 
rec2 <- subset(rec, select = vars_to_keep)

colnames(rec2) <- c("persID","cntry","wave","age","female","edu","rile","unemp",
                    "depr_soc_retro","depr_pers_retro", "depr_pers",
                    "anti_immA","anti_immB","anti_immC",
                    "dissat_gov","dissat_dem",
                    "trust_parl","trust_gov",
                    "rrp_sym","rrp_in_gov")
rec <- rec2

freq(rec$cntry)

#checking variables for coding of missings####
ctable(rec$age,rec$cntry , prop = "c")        #none
ctable(rec$female,rec$cntry , prop = "c")     #none
ctable(rec$edu,rec$cntry , prop = "c")        #none
ctable(rec$rile,rec$cntry , prop = "c")       #missing is "Don't know" 12
ctable(rec$unemp,rec$cntry , prop = "c")      #none
ctable(rec$depr_soc_retro,rec$cntry , prop = "c")    #missing is "Don't know" 6
ctable(rec$depr_pers_retro,rec$cntry , prop = "c")   #missing is "Don't know" 6
ctable(rec$depr_pers,rec$cntry , prop = "c")  #none
ctable(rec$anti_immA,rec$cntry , prop = "c")  #none
ctable(rec$anti_immB,rec$cntry , prop = "c")  #none
ctable(rec$anti_immC,rec$cntry , prop = "c")  #none
ctable(rec$dissat_gov,rec$cntry , prop = "c") #none
ctable(rec$dissat_dem,rec$cntry , prop = "c") #none
ctable(rec$trust_parl,rec$cntry , prop = "c") #none
ctable(rec$trust_gov,rec$cntry , prop = "c")  #none
ctable(rec$rrp_sym,rec$cntry , prop = "c")    #none
ctable(rec$rrp_in_gov,rec$cntry , prop = "c") #none

levels(rec$rile)
rec$rile <- as.numeric(rec$rile)
rec$depr_soc_retro <- as.numeric(rec$depr_soc_retro)
rec$depr_pers_retro <- as.numeric(rec$depr_pers_retro)
rec$rile[rec$rile == 12] <- NA
rec$depr_soc_retro[rec$depr_soc_retro == 6] <- NA
rec$depr_pers_retro[rec$depr_pers_retro == 6] <- NA

freq(rec$rile)

#invert and recode variables####
ctable(rec$unemp,rec$cntry , prop = "c")
rec$unemp <- as.numeric(rec$unemp)
rec$unemp <- recode(rec$unemp, "4"=1, "5"=1, .default = 0)

ctable(rec$rile,rec$cntry , prop = "c")
rec$rile <- abs(rec$rile-12)
ctable(rec$depr_soc_retro,rec$cntry , prop = "c")
rec$depr_soc_retro <- abs(rec$depr_soc_retro-6)
ctable(rec$depr_pers_retro,rec$cntry , prop = "c")
rec$depr_pers_retro <- abs(rec$depr_pers_retro-6)

ctable(rec$anti_immA,rec$cntry , prop = "c")
rec$anti_immA <- abs(as.numeric(rec$anti_immA)-12)
ctable(rec$anti_immB,rec$cntry , prop = "c")
rec$anti_immB <- abs(as.numeric(rec$anti_immB)-12)
ctable(rec$anti_immC,rec$cntry , prop = "c")
rec$anti_immC <- abs(as.numeric(rec$anti_immC)-12)

ctable(rec$dissat_gov,rec$cntry , prop = "c")
rec$dissat_gov <- abs(as.numeric(rec$dissat_gov)-12)
ctable(rec$dissat_dem,rec$cntry , prop = "c")
rec$dissat_dem <- abs(as.numeric(rec$dissat_dem)-12)

ctable(rec$trust_parl,rec$cntry , prop = "c")
rec$trust_parl <- abs(as.numeric(rec$trust_parl)-12)
ctable(rec$trust_gov,rec$cntry , prop = "c")
rec$trust_gov <- abs(as.numeric(rec$trust_gov)-12)

#define other variables as numeric
rec$female <- as.numeric(rec$female)
rec$edu <- as.numeric(rec$edu)
rec$depr_pers <- as.numeric(rec$depr_pers)

#labelling variables####
label(rec[["rile"]])              <- "right-left self-placement, inverted from original variable w1_Q16"
label(rec[["unemp"]])             <- "unemployed, 1= unemployed 0= not unemployed, coded from original variable W1_Q75 (4, 5 = 1)"
label(rec[["depr_soc_retro"]])    <- "deprivation sociotropic retrospective, inverted from original variable W1_Q12"
label(rec[["depr_pers_retro"]])   <- "deprivation personal retrospective, inverted from original variable W1_Q13"
label(rec[["depr_pers"]])         <- "deprivation personally now, inverted of original variable Q79"
label(rec[["anti_immA"]])         <- "anti-immigration attitudeA (customs), inverted from original variable W1_Q39_3"
label(rec[["anti_immB"]])         <- "anti-immigration attitudeB (jobs), inverted from original variable W1_Q39_4"
label(rec[["anti_immC"]])         <- "anti-immigration attitudeC (welfare), inverted from original variable W1_Q39_9"
label(rec[["dissat_gov"]])        <- "dissatisfaction with government, inverted from original variable W1_Q11"
label(rec[["dissat_dem"]])        <- "dissatisfaction with democracy, inverted from original variable W1_Q9"
label(rec[["trust_parl"]])        <- "trust national parliament, inverted from original variable W1_Q14_3"
label(rec[["trust_gov"]])         <- "trust national government, inverted from original variable W1_Q14_4"
label(rec[["rrp_sym"]])           <- "sympathy for RRPs: higher values, greater sympathy, for coding see rec_codebook"
label(rec[["rrp_in_gov"]])        <- "RRP party in Government during survey period, for coding see rec_codebook"

#save data ####
rec_final <- rec

save(rec_final, file = "rec_final.Rda")
write_sav(rec_final, path = "rec_final.sav")
