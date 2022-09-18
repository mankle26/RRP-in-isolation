
library(Rmisc)
library(tidyverse)
library(summarytools)
library(lavaan)
library(semTools)
library(corrplot)


load("CSES_final.Rda")
cses <- CSES_final

#descriptive anlyses
freq(cses$income)
corr_matrix_cses <- cor(select(cses, -c("persID","cntry")), use = "complete.obs", method = "pearson")
corrplot(corr_matrix_cses, method = 'number')

#preparations for lavaan####
#judd-kenny approach to realize interactions with latent variables in lavaan 
#center indicators anti_imm, dissat and populism
cses <- cses %>% dplyr::group_by(cntry) %>% dplyr::mutate(
                                            anti_immA = scale(anti_immA, scale = FALSE),
                                            anti_immB = scale(anti_immB, scale = FALSE),
                                            anti_immC = scale(anti_immC, scale = FALSE),
                                            dissat_gov= scale(dissat_gov, scale = FALSE),
                                            dissat_dem= scale(dissat_dem, scale = FALSE),
                                            populismA = scale(populismA, scale = FALSE),
                                            populismB = scale(populismB, scale = FALSE),
                                            populismC = scale(populismC, scale = FALSE),
                                            populismD = scale(populismD, scale = FALSE),
                                            populismE = scale(populismE, scale = FALSE),
                                            populismF = scale(populismF, scale = FALSE),
                                            populismG = scale(populismG, scale = FALSE))

#create products
#interaction anti_immXdissat
cses <- cses %>% 
  dplyr::group_by(cntry) %>%  
  dplyr::mutate(imm_dissat_gA = anti_immA * dissat_gov,
                imm_dissat_gB = anti_immB * dissat_gov,
                imm_dissat_gC = anti_immC * dissat_gov,
                imm_dissat_dA = anti_immA * dissat_dem,
                imm_dissat_dB = anti_immB * dissat_dem,
                imm_dissat_dC = anti_immC * dissat_dem,)

#remove countries for which there is no data (cH and PT) and create seperate sample for SE (less variables)####
cses_se <- cses[cses$cntry == "SE",]
cses <- cses[cses$cntry != "CH" & cses$cntry != "PT" & cses$cntry != "SE",]

#lavaan model simple####
model_cses <- '
  # latent variable definitions
    anti_imm   =~ anti_immA + anti_immB + anti_immC
    dissat     =~ dissat_gov #+ dissat_dem
    immXdissat =~ imm_dissat_gA + imm_dissat_gB + imm_dissat_gC #+ imm_dissat_dA + imm_dissat_dB + imm_dissat_dC 

  # Covariances
    anti_imm ~~ a*dissat
    
  # regressions
    rrp_sym ~ yearborn + female + edu + rile + unemp + income 
    rrp_sym ~ anti_imm + dissat + immXdissat 
'

#define swedish model without anti_immC cause this is missing
model_cses_se <- '
  # latent variable definitions
    anti_imm =~ anti_immA + anti_immB
    dissat     =~ dissat_gov #+ dissat_dem
    immXdissat =~ imm_dissat_gA + imm_dissat_gB #+ imm_dissat_dA + imm_dissat_dB

  # Covariances
    anti_imm ~~ a*dissat
 
  # regressions
    rrp_sym ~ yearborn + female + edu + rile + unemp + income 
    rrp_sym ~ anti_imm + dissat + immXdissat 
'

#run model####

fit_cses <- sem(model_cses, data = cses, estimator = "ML", group = "cntry")
sink(file = "fit_cses.txt")
summary(fit_cses, standardized = TRUE)
sink(NULL)

fit_cses_se <- sem(model_cses_se, data = cses_se, estimator = "ML", group = "cntry")
sink(file = "fit_cses_se.txt")
summary(fit_cses_se, standardized = TRUE)
sink(NULL)



#save models as data frame####
cses_res <- standardizedSolution(fit_cses,  type = "std.all", output = "data.frame")
cses_se_res <- standardizedSolution(fit_cses_se,  type = "std.all", output = "data.frame")
cses_res$cntry <- as.factor(cses_res$group) 
cses_res$group <- NULL
cses_se_res$cntry <- as.factor(10)
cses_res <- rbind(cses_res, cses_se_res)




#for the following see comi_plot_cses_rec instead ####
#plot coefficients as forest plot####

#label group values with country names
levels(cses_res$cntry) <- c("AT","BE-WA","BE-FL","DE","FI","FR","HU","IT","NO","SE")

#set order of data sets
cses_res$cntry <- factor(cses_res$cntry, levels = c("DE","FR","SE","AT","BE-FL","BE-WA","FI","IT","NO","HU"))

#combine columns lhs and rhs to generate unique identifier for each parameter
cses_res <- cbind(within(cses_res, variable <- paste(rhs, lhs, sep='->')))

# select variables to be plotted in forest plot:
var_facets <- c("anti_imm->rrp_sym","dissat->rrp_sym","immXdissat->rrp_sym")

#subset data set only to those variables
cses_res_sub <- subset(cses_res, cses_res$variable %in% var_facets)

#select only direct variables and create subset
direct_facets <- c("anti_imm->rrp_sym","dissat->rrp_sym")
cses_res_sub_direct <- subset(cses_res_sub, cses_res_sub$variable %in% direct_facets)

#select only interaction variables and create subset
interaction_facets <- c("immXdissat->rrp_sym")
cses_res_sub_interaction <- subset(cses_res_sub, cses_res_sub$variable %in% interaction_facets)

#changing coefficient names to labels for plots
test_unicode_arrows <- c("u2794 ist \u2794, u2799 ist \u2799, u27a1 ist \u27a1,
                         u279c ist \u279c, u279e ist \u279e")
test_unicode_arrows
#set variable to factor, adjust order of the levels and add unicode arrows and multiplication symbol
#for direct variables
cses_res_sub_direct$variable <- as.factor(cses_res_sub_direct$variable)
cses_res_sub_direct$variable <-    factor(cses_res_sub_direct$variable, 
                                         levels = c("anti_imm->rrp_sym","dissat->rrp_sym"))
levels(cses_res_sub_direct$variable)
levels(cses_res_sub_direct$variable) <- c("Anti-immigration \n \u279c RRP-Sympathy" ,       
                                         "Pol. Dissatisfaction \n \u279c RRP-Sympathy")
#for interaction variables
cses_res_sub_interaction$variable <- as.factor(cses_res_sub_interaction$variable)
cses_res_sub_interaction$variable <-    factor(cses_res_sub_interaction$variable, 
                                              levels = c("immXdissat->rrp_sym"))
levels(cses_res_sub_interaction$variable)
levels(cses_res_sub_interaction$variable) <- c("Anti-imm X Pol. Dissat \n \u279c RRP-Sympathy")

#creating plots
#create two-layered plot sort by variables
# split variables in two rows with different scaling

plot_direct <- ggplot(cses_res_sub_direct, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(cses_res_sub_direct$cntry))) +
  scale_y_continuous(breaks=seq(-.8,.4,by=.4)) +
  geom_hline(yintercept = 0, color = "grey") + 
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

plot_interaction <- ggplot(cses_res_sub_interaction, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(cses_res_sub$cntry))) +
  scale_y_continuous(breaks=seq(-.1,.2,by=.1)) +
  geom_hline(yintercept = 0, color = "grey") + 
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

multiplot(plot_direct, plot_interaction, cols=1)



