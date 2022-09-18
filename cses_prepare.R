
library(Hmisc)
library(summarytools)
library(tidyverse)
library(haven)


cses5 <- read_spss("downloaded/cses5.sav")

cses <- cses5

#variables#####
#E1005              persID
#E1006_UNALPHA2     cntry
#E1034              year
#E2001_Y            year_of_birth
#E2002              gender
#E2003              education

#E2006          >>> D06     CURRENT EMPLOYMENT STATUS
#E2007          >>> D07     MAIN OCCUPATION
#E2008          >>> D07a    SOCIO ECONOMIC STATUS
#E2010          >>>         HOUSEHOLD INCOME - QUINTILES
#E2013          >>> D10     RELIGIOUS DENOMINATION
#E2014          >>> D11     RELIGIOUS SERVICES ATTENDANCE
#E2015          >>> D12     RACE
#E2016          >>> D13     ETHNICITY
#E2017          >>> D14     COUNTRY OF BIRTH
#E2018          >>> D15     WAS EITHER BIOLOGICAL PARENT BORN OUTSIDE OF THE COUNTRY

#E3009          >>> Q09     GOVERNMENT PERFORMANCE: GENERAL
#E3016_1        >>> Q14a    WHO IS IN POWER CAN MAKE DIFFERENCE
#E3016_2        >>> Q14b    WHO PEOPLE VOTE FOR MAKES A DIFFERENCE
#E3003          >>> Q03     INTERNAL EFFICACY
#E3004_1        >>> Q04a    ATTITUDES ABOUT ELITES: COMPROMISE IS SELLING OUT ONES PRINCIPLES
#E3004_2        >>> Q04b    ATTITUDES ABOUT ELITES: DO NOT CARE ABOUT THE PEOPLE
#E3004_3        >>> Q04c    ATTITUDES ABOUT ELITES: ARE TRUSTWORTHY
#E3004_4        >>> Q04d    ATTITUDES ABOUT ELITES: ARE THE MAIN PROBLEM
#E3004_5        >>> Q04e    ATTITUDES ABOUT ELITES: STRONG LEADER BENDS THE RULES
#E3004_6        >>> Q04f    ATTITUDES ABOUT ELITES: PEOPLE SHOULD MAKE POLICY DECISIONS
#E3004_7        >>> Q04g    ATTITUDES ABOUT ELITES: RICH AND POWERFUL

#E3005_1        >>> Q05a    OUT-GROUP ATTITUDES: MINORITIES - CUSTOMS AND TRADITIONS
#E3005_2        >>> Q05b    OUT-GROUP ATTITUDES: MINORITIES - WILL OF THE MAJORITY
#E3005_3        >>> Q05c    OUT-GROUP ATTITUDES: IMMIGRANTS GOOD FOR ECONOMY
#E3005_4        >>> Q05d    OUT-GROUP ATTITUDES: CULTURE HARMED BY IMMIGRANTS
#E3005_5        >>> Q05e    OUT-GROUP ATTITUDES: IMMIGRANTS INCREASE CRIME

#E3010_2        >>> Q10b    PARTY THAT REPRESENTS RESPONDENTS VIEWS BEST
#E3011          >>> Q11     STATE OF THE ECONOMY
#E3013_LH_PL    >>> Q12LH-b CURRENT LOWER HOUSE ELECTION: VOTE CHOICE - PARTY LIST
#E3013_UH_PL    >>> Q12UH-b CURRENT UPPER HOUSE ELECTION: VOTE CHOICE - PARTY LIST

#E3017_A        >>> Q15a    LIKE-DISLIKE - PARTY A
#E3017_B        >>> Q15b    LIKE-DISLIKE - PARTY B
#E3017_C        >>> Q15c    LIKE-DISLIKE - PARTY C
#E3017_D        >>> Q15d    LIKE-DISLIKE - PARTY D
#E3017_E        >>> Q15e    LIKE-DISLIKE - PARTY E
#E3017_F        >>> Q15f    LIKE-DISLIKE - PARTY F
#E3017_G        >>> Q15g    LIKE-DISLIKE - ADDITIONAL - PARTY G
#E3017_H        >>> Q15h    LIKE-DISLIKE - ADDITIONAL - PARTY H
#E3017_I        >>> Q15i    LIKE-DISLIKE - ADDITIONAL - PARTY I
#E3020          >>> Q18     LEFT-RIGHT - SELF
#E3023          >>> Q21     SATISFACTION WITH DEMOCRACY


#select countries to keep####
freq(cses$E1006_UNALPHA2)
freq(cses$E1003)

cses$E1006_UNALPHA2[cses$E1003 == 5612019] <- c("BE-WA")
cses$E1006_UNALPHA2[cses$E1003 == 5622019] <- c("BE-FL")

cses$E1006_UNALPHA2 <- as.factor(cses$E1006_UNALPHA2)
levels(cses$E1006_UNALPHA2)
freq(cses$E1006_UNALPHA2)

cses2 <- cses[cses$E1006_UNALPHA2 == "DE" | 
                      cses$E1006_UNALPHA2 == "AT" |
                      cses$E1006_UNALPHA2 == "FI" |
                      cses$E1006_UNALPHA2 == "CH" |
                      cses$E1006_UNALPHA2 == "FR" |
                      cses$E1006_UNALPHA2 == "BE-WA" |
                      cses$E1006_UNALPHA2 == "BE-FL" |
                      cses$E1006_UNALPHA2 == "HU" |
                      cses$E1006_UNALPHA2 == "IT" |
                      cses$E1006_UNALPHA2 == "SE" |
                      cses$E1006_UNALPHA2 == "NO" |
                      cses$E1006_UNALPHA2 == "PT"
              ,]
cses2$E1006_UNALPHA2 <- droplevels(cses2$E1006_UNALPHA2)


#coding rrp_sym variable####
#done before variable selecting because different variables are merged into one
cses2$rrp_sym <- NA
ctable(cses2$E3017_G, cses2$E1006_UNALPHA2)

#for Austria defining FPoe as rrp
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "AT"] <- cses2$E3017_C[cses2$E1006_UNALPHA2 == "AT"]
#for Belgium-Flandern defining Vlaams Belang as rrp (also FN or DN/DNAT possible, but not included in data), for Wallonia PP
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "BE-FL"] <- cses2$E3017_B[cses2$E1006_UNALPHA2 == "BE-FL"]
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "BE-WA"] <- cses2$E3017_G[cses2$E1006_UNALPHA2 == "BE-WA"]
#for Finland defining Perussuomalaiset
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "FI"] <- cses2$E3017_B[cses2$E1006_UNALPHA2 == "FI"]
#for France defning Front National as rrp
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "FR"] <- cses2$E3017_B[cses2$E1006_UNALPHA2 == "FR"]
#for Germany defining AfD as rrp
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "DE"] <- cses2$E3017_C[cses2$E1006_UNALPHA2 == "DE"]
#for Great Britain defining UKIP (E) as rrp
#
#for Greece sympathy for all right-wing-populist parties not asked
#
#for Hungary defining Fidesz as rrp, other option: Jobbik (B)
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "HU"] <- cses2$E3017_A[cses2$E1006_UNALPHA2 == "HU"]
#for italy defining Lega as rrp, other option: Fratelli d'Italia (E) 
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "IT"] <- cses2$E3017_C[cses2$E1006_UNALPHA2 == "IT"]
#for Lithuania sympathy for Jauniji Lietuva not asked
##
#for norway defining Fremskrittspartiet as rrp
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "NO"] <- cses2$E3017_C[cses2$E1006_UNALPHA2 == "NO"]
#for Portugal defining Chega (G) as rrp
#
#for sweden defining Sverigedemokraterna as rrp
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "SE"] <- cses2$E3017_C[cses2$E1006_UNALPHA2 == "SE"]
#for Switzerland defining SVP as rrp, other option: Lega dei Ticinesi (H)
cses2$rrp_sym[cses2$E1006_UNALPHA2 == "CH"] <- cses2$E3017_A[cses2$E1006_UNALPHA2 == "CH"]


#coding rrp_vote variable####
#done before variable selecting because different variables are merged into one
ctable(cses2$E3013_LH_PL, cses2$E1006_UNALPHA2, prop = "c") #Lower house vote, GB all missing
ctable(cses2$E3013_UH_PL, cses2$E1006_UNALPHA2, prop = "c") #upper house vote, GB all missing
ctable(cses2$E3010_2, cses2$E1006_UNALPHA2, prop = "c")     #party represents respondents views best: 19 respondents choose UKIP

cses2$rrp_vote <- NA
cses2$rrp_vote[cses2$E3013_LH_PL <= 999988] <- 0

#for Austria defining FPoe as rrp (BZoe 40010 not in data)
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "AT" & cses2$E3013_LH_PL == 40003] <- 1
#for Belgium-Flanndern defining Vlaams Belang as rrp (also FN or DN/DNAT, but not in data), for Wallonia PP
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "BE-FL" & cses2$E3013_LH_PL == 56002] <- 1
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "BE-WA" & cses2$E3013_LH_PL == 56907] <- 1
#for Finland defining Perussuomalaiset
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "FI" & cses2$E3013_LH_PL == 246002] <- 1
#for France defning Front National as rrp but all missing!!
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "FR" & cses2$E3013_LH_PL == 250002] <- 1
#for Germany defining AfD as rrp (NPD not classified as populist by popuList)
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "DE" & cses2$E3013_LH_PL == 276003] <- 1
#for Great Britain defining UKIP as rrp but no data for lower house vote, nor upper house vote, party represents respondents view with 19 UKIP respondents
#
#for Greece Golden Dawn not classified as populist by popuList (~5%), all three other rrp parties (EL,LAOS,POLAN) did not exist or were not included in the data collection 
#
#for Hungary defining Fidesz and Jobbik as rrp
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "HU" & cses2$E3013_LH_PL == 348001] <- 1
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "HU" & cses2$E3013_LH_PL == 348002] <- 1
#for italy defining Lega and Fratelli d'Italia as rrp, party LAM from popuList not in data
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "IT" & cses2$E3013_LH_PL == 380003] <- 1
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "IT" & cses2$E3013_LH_PL == 380005] <- 1
#for Lithuania voting for Jauniji Lietuva not in data
#
#for norway defining Fremskrittspartiet as rrp
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "NO" & cses2$E3013_LH_PL == 578003] <- 1
#for Portugal defining Chega as rrp but only 16 persons
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "PT" & cses2$E3013_LH_PL == 620007] <- 1
#for sweden defining Sverigedemokraterna as rrp
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "SE" & cses2$E3013_LH_PL == 752003] <- 1
#for Switzerland defining SVP and Lega dei Ticinesi as rrp
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "CH" & cses2$E3013_LH_PL == 756001] <- 1
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "CH" & cses2$E3013_LH_PL == 756011] <- 1
cses2$rrp_vote[cses2$E1006_UNALPHA2 == "CH" & cses2$E3013_LH_PL == 756015] <- 1

#check coding:
ctable(cses2$rrp_vote, cses2$E1006_UNALPHA2, prop = "c")
ctable(cses2$E3013_LH_PL, cses2$E1006_UNALPHA2, prop = "c") #Lower house vote, GB all missing

cses <- cses2

#generating rrp_in_gov Variable####

#set to 0 for all respondents in all countries
cses$rrp_in_gov <- 0

#set 1 for specific countries
cses$rrp_in_gov[cses$E1006_UNALPHA2 == "HU"] <- 1 #Fidesz in Gov before and after election
cses$rrp_in_gov[cses$E1006_UNALPHA2 == "NO"] <- 1 #Fremskrittspartiet in gov before and after election
cses$rrp_in_gov[cses$E1006_UNALPHA2 == "CH"] <- 1 #SVP in Gov before and after election

#define variables to keep####

vars_to_keep2 <- c("E1005","E1006_UNALPHA2","E1034","E2001_Y","E2002","E2003","E2006","E3020", #socio-demographics
                   "E2008","E2010", #economic deprivation
                   "E3004_1","E3004_2","E3004_3","E3004_4","E3004_5","E3004_6","E3004_7", #populism
                   "E3005_3","E3005_4","E3005_5", #inti-immigrants
                   "E3023","E3009", #dissatisfaction
                   "rrp_sym", "rrp_vote","rrp_in_gov"
) 

cses2 <- subset(cses, select = vars_to_keep2)

colnames(cses2) <- c("persID","cntry","year","yearborn","female","edu","unemp","rile",
                     "ses","income",
                     "populismA","populismB","populismC","populismD","populismE","populismF","populismG",
                     "anti_immA", "anti_immB", "anti_immC",
                     "dissat_dem","dissat_gov",
                     "rrp_sym", "rrp_vote","rrp_in_gov")
cses <- cses2

freq(cses$cntry)

#checking variables for coding of missings####
ctable(cses$rrp_sym, cses$cntry, prop = "c")   #values 96-99 are missings. 
ctable(cses$dissat_dem, cses$cntry, prop = "c")    #values 7 8 9 are missings. recode 4 to 3 and 5 to 4
ctable(cses$dissat_gov, cses$cntry, prop = "c")    #values 7 8 9 are missings. country specific coding of value 6
ctable(cses$populismC, cses$cntry, prop = "c") #values 7,8,9 are missings. populismA items not asked in sweden. item C coded different in Norway
ctable(cses$anti_immA, cses$cntry, prop = "c") #values 7 8 9 are missings. item c not asked in sweden
ctable(cses$ses, cses$cntry, prop = "c")       #values 7 8 9 are missings. many missings in BE, CH, DE, FI,IT
ctable(cses$income, cses$cntry, prop = "c")    #values 7 8 9 are missings.
ctable(cses$female, cses$cntry, prop = "c")    #values 3 9 are missings
ctable(cses$yearborn, cses$cntry, prop = "c")  #values 9999 are missings.
ctable(cses$edu, cses$cntry, prop = "c")       #values 96-99 are missings. 10% missing in sweden, other countries ~1%
ctable(cses$unemp, cses$cntry, prop = "c")     #values 96-99 are missings. different coding in Austria (10 == 5)
ctable(cses$rile, cses$cntry, prop = "c")      #values 95-99 are missings. 


cses[cses == 95|cses == 96|cses == 97|cses == 98|cses == 99|
       cses == 995|cses == 996|cses == 997|cses == 998|cses == 999|
       cses == 9995|cses == 9996|cses == 9997|cses == 9998|cses == 9999|
       cses == 99995|cses == 99996|cses == 99997|cses == 99998|cses == 99999|
       cses == 999995|cses == 999996|cses == 999997|cses == 999998|cses == 999999|
       cses == 9999995|cses == 9999996|cses == 9999997|cses == 9999998|cses == 9999999|
       cses == 99999995|cses == 99999996|cses == 99999997|cses == 99999998|cses == 99999999|
       cses == 999999995|cses == 999999996|cses == 999999997|cses == 999999998|cses == 999999999] <- NA

cses$female[cses$female == 3|cses$female == 9] <- NA

#selecting those variables, for which 7 8 and 9 are missings and coding them as missings
cses3 <- subset(cses, select = c("ses","income","anti_immA","anti_immB","anti_immC","dissat_dem","dissat_gov",
                                 "populismA","populismB","populismC","populismD","populismE","populismF","populismG"
                                 ))
cses3[cses3 == 7 | cses3 == 8 | cses3 == 9] <- NA
cses3$dissat_gov[cses3$dissat_gov == 6] <- NA #norway and finland with neutral category coded as 6, coding as missings
cses$unemp[cses$cntry == "AT" & cses$unemp == 10] <- 5

#merge again with the other variables
cses4 <- select(cses, -c("ses","income","anti_immA","anti_immB","anti_immC","dissat_dem","dissat_gov",
                         "populismA","populismB","populismC","populismD","populismE","populismF","populismG"
                         ))
cses <- cbind(cses4, cses3)

#invert scale of variables####

ctable(cses$populismA, cses$cntry, prop = "c")
cses$populismA <- abs(cses$populismA-6)
ctable(cses$populismB, cses$cntry, prop = "c")
cses$populismB <- abs(cses$populismB-6)
ctable(cses$populismD, cses$cntry, prop = "c")
cses$populismD <- abs(cses$populismD-6)
ctable(cses$populismE, cses$cntry, prop = "c")
cses$populismE <- abs(cses$populismE-6)
ctable(cses$populismF, cses$cntry, prop = "c")
cses$populismF <- abs(cses$populismF-6)
ctable(cses$populismG, cses$cntry, prop = "c")
cses$populismG <- abs(cses$populismG-6)


ctable(cses$anti_immB, cses$cntry, prop = "c")
cses$anti_immB <- abs(cses$anti_immB-6)
ctable(cses$anti_immC, cses$cntry, prop = "c")
cses$anti_immC <- abs(cses$anti_immC-6)

ctable(cses$dissat_dem, cses$cntry, prop = "c")
cses$dissat_dem[cses$dissat_dem == 4] <- 3
cses$dissat_dem[cses$dissat_dem == 5] <- 4


ctable(cses$rile, cses$cntry, prop = "c")
cses$rile <- abs(cses$rile-11)

#recode unemployment
ctable(cses$unemp, cses$cntry, prop = "c")
cses$unemp <- as.numeric(cses$unemp)
cses$unemp <- recode(cses$unemp, "5"=1, .default = 0)

#labelling variables####
label(cses[["rrp_sym"]])   <- "Sympathy for RRPs: higher values, greater sympathy (own coding, see CSES_codebook.txt)"
label(cses[["rrp_vote"]])  <- "Voted for RRPs: 1=did vote for RRP, 0=did vote for other party (own coding, see CSES_codebook.txt)"
label(cses[["rrp_in_gov"]])<- "RRP party in Government during survey period"
label(cses[["populismA"]]) <- "ATTITUDES ABOUT ELITES: COMPROMISE IS SELLING OUT ONES PRINCIPLES (inverted from original)"
label(cses[["populismB"]]) <- "ATTITUDES ABOUT ELITES: DO NOT CARE ABOUT THE PEOPLE (inverted from original)"
label(cses[["populismC"]]) <- "ATTITUDES ABOUT ELITES: ARE TRUSTWORTHY"
label(cses[["populismD"]]) <- "ATTITUDES ABOUT ELITES: ARE THE MAIN PROBLEM (inverted from original)"
label(cses[["populismE"]]) <- "ATTITUDES ABOUT ELITES: STRONG LEADER BENDS THE RULES (inverted from original)"
label(cses[["populismF"]]) <- "ATTITUDES ABOUT ELITES: PEOPLE SHOULD MAKE POLICY DECISIONS (inverted from original)"
label(cses[["populismG"]]) <- "ATTITUDES ABOUT ELITES: (care only about) RICH AND POWERFUL (inverted from original)"
label(cses[["anti_immA"]]) <- "OUT-GROUP ATTITUDES: IMMIGRANTS GOOD FOR ECONOMY"
label(cses[["anti_immB"]]) <- "OUT-GROUP ATTITUDES: CULTURE HARMED BY IMMIGRANTS (inverted from original)"
label(cses[["anti_immC"]]) <- "OUT-GROUP ATTITUDES: IMMIGRANTS INCREASE CRIME (inverted from original)"
label(cses[["dissat_dem"]]) <- "SATISFACTION WITH DEMOCRACY (E3023) coding adjusted to linear scale"
label(cses[["dissat_gov"]]) <- "GOVERNMENT PERFORMANCE: GENERAL (E3009)"


#dropping sweden due to all missings on populismA and anti_immC####
#cses <- cses[cses$cntry != "SE",]

#save data ####
CSES_final <- cses

save(CSES_final, file = "CSES_final.Rda")
write_sav(CSES_final, path = "CSES_final.sav")

#investigating relationship between sympathy for different rrps within one country####

#hungary
#fidesz <- as.data.frame(as.numeric(cses$E3017_A[cses$E1006_UNALPHA2 == "HU"]))
#jobbik <- as.data.frame(as.numeric(cses$E3017_B[cses$E1006_UNALPHA2 == "HU"]))

#rrp_sym_hu <- cbind(fidesz, jobbik)
#colnames(rrp_sym_hu) <- c("fidesz","jobbik")
#rrp_sym_hu[rrp_sym_hu == 95|rrp_sym_hu == 96|rrp_sym_hu == 97|rrp_sym_hu == 98|rrp_sym_hu == 99] <- NA
             
#cor(rrp_sym_hu$fidesz, rrp_sym_hu$jobbik, use = c("complete.obs"), method = c("pearson"))
#write.csv2(rrp_sym_hu, file="rrp_sym_hu.csv")

#italy 
#lega <- as.data.frame(as.numeric(cses$E3017_C[cses$E1006_UNALPHA2 == "IT"]))
#frat <- as.data.frame(as.numeric(cses$E3017_E[cses$E1006_UNALPHA2 == "IT"]))

#rrp_sym_it <- cbind(lega, frat)
#colnames(rrp_sym_it) <- c("lega","frat")
#rrp_sym_it[rrp_sym_it == 95|rrp_sym_it == 96|rrp_sym_it == 97|rrp_sym_it == 98|rrp_sym_it == 99] <- NA

#cor(rrp_sym_it$lega, rrp_sym_it$frat, use = c("complete.obs"), method = c("pearson"))
#write.csv2(rrp_sym_it, file="rrp_sym_it.csv")
        