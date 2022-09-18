
library(tidyverse)
library(Hmisc)
library(Rmisc)
library(summarytools)
library(lavaan)
library(semTools)
library(corrplot)

load("rec_final.Rda")
rec <- rec_final

#descriptive analyses
#aggregate(rec$W1dissat_gov, list(rec$W1cntry, rec$W1rrp_sym), mean)
corr_matrix_rec <- cor(select(rec, -c("persID","W1cntry","W1wave","W2cntry","W2wave")), use = "complete.obs", method = "pearson")
corrplot(corr_matrix_rec, method = 'number')

#preparations for lavaan####
#judd-keeny approach to realize interactions with latent variables in lavaan 
#center indicators anti_imm, dissat and populism by country group variable

rec <- rec %>% dplyr::group_by(W1cntry) %>% dplyr::mutate(W1anti_immA = scale(W1anti_immA, scale = FALSE),
                                             W1anti_immB = scale(W1anti_immB, scale = FALSE),
                                             W1anti_immC = scale(W1anti_immC, scale = FALSE),
                                             W1dissat_gov = scale(W1dissat_gov, scale = FALSE),
                                             W1dissat_dem = scale(W1dissat_dem, scale = FALSE),
                                             W1trust_parl = scale(W1trust_parl, scale = FALSE),
                                             W1trust_gov = scale(W1trust_gov, scale = FALSE),
                                             W2dissat_gov = scale(W2dissat_gov, scale = FALSE),
                                             W2dissat_dem = scale(W2dissat_dem, scale = FALSE))

#create products
#interaction anti_immXdissat
rec <- rec %>% 
  dplyr::group_by(W1cntry) %>%  
  dplyr::mutate(W1imm_dissat_gA = W1anti_immA * W1dissat_gov,
              W1imm_dissat_gB = W1anti_immB * W1dissat_gov,
              W1imm_dissat_gC = W1anti_immC * W1dissat_gov,
              W1imm_dissat_dA = W1anti_immA * W1dissat_dem,
              W1imm_dissat_dB = W1anti_immB * W1dissat_dem,
              W1imm_dissat_dC = W1anti_immC * W1dissat_dem,)

#lavaan model only wave 1####
model_W1 <- '
  # latent variable definitions
    W1anti_imm   =~ W1anti_immA + W1anti_immB + W1anti_immC
    W1dissat     =~ W1dissat_gov # + W1dissat_dem
    W1immXdissat =~ W1imm_dissat_gA + W1imm_dissat_gB + W1imm_dissat_gC # + W1imm_dissat_dA + W1imm_dissat_dB + W1imm_dissat_dC

  # Covariances
    W1anti_imm ~~ a*W1dissat
    
  # regressions
    W1rrp_sym ~ W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers 
    W1rrp_sym ~ W1anti_imm + W1dissat + W1immXdissat 
'

#run model####
fit_W1 <- sem(model_W1, data = rec, estimator = "ML", group = "W1cntry")
#fit_W1 <- sem(model, data = rec, estimator = "ML", group = "W1cntry", se = "bootstrap", bootstrap = 100) #takes more than 10 min
sink(file = "fit_rec_W1.txt")
summary(fit_W1, standardized = TRUE)
sink(NULL)


#lavaan model both waves####
model_W12 <- '
  # latent variable definitions
    W1anti_imm =~ W1anti_immA + W1anti_immB + W1anti_immC
    W1dissat     =~ W1dissat_gov # + W1dissat_dem
    W1immXdissat =~ W1imm_dissat_gA + W1imm_dissat_gB + W1imm_dissat_gC # + W1imm_dissat_dA + W1imm_dissat_dB + W1imm_dissat_dC

  # Covariances
    W1anti_imm ~~ a*W1dissat
    
  # regressions
    W2rrp_sym ~ W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers + W1rrp_sym + W1anti_imm + W1dissat + W1immXdissat 
'

#run model####

fit_W12 <- sem(model_W12, data = rec, estimator = "ML", group = "W2cntry")
sink(file = "fit_rec_W12.txt")
summary(fit_W12, standardized = TRUE)
sink(NULL)


#save models as data frame####
rec_resW1 <- standardizedSolution(fit_W1,  type = "std.all", output = "data.frame")
rec_resW12 <-standardizedSolution(fit_W12, type = "std.all", output = "data.frame")

rec_resW1$cntry <- as.factor(rec_resW1$group)
rec_resW12$cntry <- as.factor(rec_resW12$group)
levels(rec_resW1$cntry) <- c("FR1","DE1","IT1","DK1","ES1","PL1","HU1")
levels(rec_resW12$cntry) <- c("FR2","DE2","IT2","DK2","ES2","PL2","HU2")
rec_res <- rbind(rec_resW1, rec_resW12)


#plot coefficients as forest plot####

#set order of data sets
rec_res$cntry <- factor(rec_res$cntry, levels = c("DE1","DE2","FR1","FR2","ES1","ES2","DK1","DK2","IT1","IT2","PL1","PL2","HU1","HU2"))

#combine columns lhs and rhs to generate unique identifier for each parameter
rec_res <- cbind(within(rec_res, variable <- paste(rhs, lhs, sep='->')))

# select variables to be plotted in forest plot:
var_facets <- c("W1anti_imm->W1rrp_sym","W1dissat_gov->W1rrp_sym","W1immXdissat->W1rrp_sym",
                "W1anti_imm->W2rrp_sym","W1dissat_gov->W2rrp_sym","W1immXdissat->W2rrp_sym")

#subset data set only to those variables
rec_res_sub <- subset(rec_res, rec_res$variable %in% var_facets)

#set factors for W1 and W2 equal
rec_res_sub$variable <- gsub('W1', '', rec_res_sub$variable)
rec_res_sub$variable <- gsub('W2', '', rec_res_sub$variable)

#select only direct variables and create subset
direct_facets <- c("anti_imm->rrp_sym","dissat_gov->rrp_sym")
rec_res_sub_direct <- subset(rec_res_sub, rec_res_sub$variable %in% direct_facets)

#select only interaction variables and create subset
interaction_facets <- c("immXdissat->rrp_sym")
rec_res_sub_interaction <- subset(rec_res_sub, rec_res_sub$variable %in% interaction_facets)

#changing coefficient names to labels for plots
test_unicode_arrows <- c("u2794 ist \u2794, u2799 ist \u2799, u27a1 ist \u27a1,
                         u279c ist \u279c, u279e ist \u279e")
test_unicode_arrows
#set variable to factor, adjust order of the levels and add unicode arrows and multiplication symbol
#for direct variables
rec_res_sub_direct$variable <- as.factor(rec_res_sub_direct$variable)
rec_res_sub_direct$variable <-    factor(rec_res_sub_direct$variable, 
                                         levels = c("anti_imm->rrp_sym","dissat_gov->rrp_sym"))
levels(rec_res_sub_direct$variable)
levels(rec_res_sub_direct$variable) <- c("Anti-immigration \n \u279c RRP-Sympathy" ,       
                                         "Pol. Dissatisfaction \n \u279c RRP-Sympathy")
#for interaction variables
rec_res_sub_interaction$variable <- as.factor(rec_res_sub_interaction$variable)
rec_res_sub_interaction$variable <-    factor(rec_res_sub_interaction$variable, 
                                          levels = c("immXdissat->rrp_sym"))
levels(rec_res_sub_interaction$variable)
levels(rec_res_sub_interaction$variable) <- c("Anti-imm X Pol. Dissat \n \u279c RRP-Sympathy")

#creating plots
#create two-layered plot sort by variables
# split variables in two rows with different scaling

plot_direct <- ggplot(rec_res_sub_direct, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(rec_res_sub_direct$cntry))) +
  scale_y_continuous(breaks=seq(-.8,.4,by=.4)) +
  geom_hline(yintercept = 0, color = "grey") + 
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

plot_interaction <- ggplot(rec_res_sub_interaction, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(rec_res_sub$cntry))) +
  scale_y_continuous(breaks=seq(0,.4,by=.2)) +
  geom_hline(yintercept = 0, color = "grey") + 
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

multiplot(plot_direct, plot_interaction, cols=1)


