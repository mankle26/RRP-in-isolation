
library(stringr)

levels(rec_resW1$cntry) <- c("FR(R)","DE(R)","IT(R)","DK(R)","ES(R)","PL(R)","HU(R)")
levels(cses_res$cntry) <- c("AT(C)","BE-WA(C)","BE-FL(C)","DE(C)","FI(C)","FR(C)","HU(C)","IT(C)","NO(C)","SE(C)")

rec_resW1$group <- NULL
cses_res$variable <- NULL

combi_res <- rbind(rec_resW1, cses_res)

combi_res$lhs <- str_replace(combi_res$lhs, "W1", "")
combi_res$rhs <- str_replace(combi_res$rhs, "W1", "")
combi_res$lhs <- str_replace(combi_res$lhs, "_gov", "")
combi_res$rhs <- str_replace(combi_res$rhs, "_gov", "")


#set order of data sets
combi_res$cntry <- factor(combi_res$cntry, levels = c("DE(R)","ES(R)","DE(C)","SE(C)","FR(C)","FR(R)",
                                                      "AT(C)","FI(C)","DK(R)","IT(C)",
                                                      "HU(R)","PL(R)","IT(R)","HU(C)","NO(C)"))

#combine columns lhs and rhs to generate unique identifier for each parameter
combi_res <- cbind(within(combi_res, variable <- paste(rhs, lhs, sep='->')))

# select variables to be plotted in forest plot:
var_facets <- c("anti_imm->rrp_sym","dissat->rrp_sym","immXdissat->rrp_sym")

#subset data set only to those variables
combi_res_sub <- subset(combi_res, combi_res$variable %in% var_facets)

#select only direct variables and create subset
direct_facets <- c("anti_imm->rrp_sym","dissat->rrp_sym")
combi_res_sub_direct <- subset(combi_res_sub, combi_res_sub$variable %in% direct_facets)

#select only interaction variables and create subset
interaction_facets <- c("immXdissat->rrp_sym")
combi_res_sub_interaction <- subset(combi_res_sub, combi_res_sub$variable %in% interaction_facets)

#changing coefficient names to labels for plots
test_unicode_arrows <- c("u2794 ist \u2794, u2799 ist \u2799, u27a1 ist \u27a1,
                         u279c ist \u279c, u279e ist \u279e")
test_unicode_arrows
#set variable to factor, adjust order of the levels and add unicode arrows and multiplication symbol
#for direct variables
combi_res_sub_direct$variable <- as.factor(combi_res_sub_direct$variable)
combi_res_sub_direct$variable <-    factor(combi_res_sub_direct$variable, 
                                          levels = c("anti_imm->rrp_sym","dissat->rrp_sym"))
levels(combi_res_sub_direct$variable)
levels(combi_res_sub_direct$variable) <- c("Anti-immigration \n \u279c RRP-Sympathy" ,       
                                          "Pol. Dissatisfaction \n \u279c RRP-Sympathy")
#for interaction variables
combi_res_sub_interaction$variable <- as.factor(combi_res_sub_interaction$variable)
combi_res_sub_interaction$variable <-    factor(combi_res_sub_interaction$variable, 
                                               levels = c("immXdissat->rrp_sym"))
levels(combi_res_sub_interaction$variable)
levels(combi_res_sub_interaction$variable) <- c("Anti-imm X Pol. Dissat \n \u279c RRP-Sympathy")

#combi_res_sub_interaction$rrp_pos <- as.factor(c(rep("RRP isolated", 6),rep("RRP was in Gov", 6),rep("RRP is in Gov", 5)))
#combi_res_sub_direct$rrp_pos <- as.factor(c(rep("RRP isolated", 12),rep("RRP was in Gov", 12),rep("RRP is in Gov", 10)))
#rrp_pos <- c("RRP isolated","RRP was in Gov","RRP is in Gov")

#rrp_color <- as.character(c("#33FFFF", "#33B2FF", "#335EFF"))


#creating plots
#create two-layered plot sort by variables
# split variables in two rows with different scaling

plot_direct <- ggplot(combi_res_sub_direct, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(combi_res_sub_direct$cntry))) +
  scale_y_continuous(breaks=seq(-.8,.4,by=.4)) +
  geom_hline(yintercept = 0, color = "grey") + 
  #scale_colour_manual(values=setNames(rrp_color, rrp_pos)) +
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

plot_interaction <- ggplot(combi_res_sub_interaction, aes(x = cntry, y = est.std)) + 
  geom_pointrange(aes(ymax = ci.upper, ymin = ci.lower), color = "black", size=.8) +
  scale_x_discrete(limits = rev(levels(combi_res_sub$cntry))) +
  scale_y_continuous(breaks=seq(-.1,.2,by=.1)) +
  geom_hline(yintercept = 0, color = "grey") + 
  theme_bw(base_size = 20) +
  theme(axis.title.y = element_blank()) +
  ylab(NULL) + 
  coord_flip() +
  facet_wrap(~ variable, nrow = 1)

multiplot(plot_direct, plot_interaction, cols=1)


#plotting interaction effects for countries####


library(sjPlot)
load("CSES_final.Rda")
cses <- CSES_final

load("rec_final.Rda")
rec <- rec_final

cses_int <- cses
cses_int$anti_imm <- rowMeans(subset(cses_int, select = c(anti_immA, anti_immB, anti_immC)), na.rm = TRUE)
cses_int$dissat <- as.numeric(as.factor(cses_int$dissat_gov))
cses_int$yearborn <- as.numeric(cses_int$yearborn)
cses_int$female <- as.numeric(cses_int$female)
cses_int$edu <- as.numeric(cses_int$edu)
cses_int$rrp_sym <- as.numeric(cses_int$rrp_sym)
cses_int$rile <- as.numeric(cses_int$rile)
cses_int$unemp <- as.numeric(cses_int$unemp)
cses_int$income <- as.numeric(cses_int$income)
rec_int <- rec
rec_int$W1anti_imm <- rowMeans(subset(rec_int, select = c(W1anti_immA, W1anti_immB, W1anti_immC)), na.rm = TRUE)
rec_int$W1dissat <- as.numeric(as.factor(rec_int$W1dissat_gov))
rec_int$W1age <- as.numeric(rec_int$W1age)
rec_int$W1female <- as.numeric(rec_int$W1female)
rec_int$W1edu <- as.numeric(rec_int$W1edu)
rec_int$W1rrp_sym <- as.numeric(rec_int$W1rrp_sym)
rec_int$W1rile <- as.numeric(rec_int$W1rile)
rec_int$W1unemp <- as.numeric(rec_int$W1unemp)
rec_int$W1depr_pers <- as.numeric(rec_int$W1depr_pers)

de_r <- rec_int[rec_int$W1cntry == "DE",]
es_r <- rec_int[rec_int$W1cntry == "ES",]
de_c <- cses_int[cses_int$cntry == "DE",]
se_c <- cses_int[cses_int$cntry == "SE",]
fr_c <- cses_int[cses_int$cntry == "FR",]
fr_r <- rec_int[rec_int$W1cntry == "FR",]
at_c <- cses_int[cses_int$cntry == "AT",]
fi_c <- cses_int[cses_int$cntry == "FI",]
dk_r <- rec_int[rec_int$W1cntry == "DK",]
it_c <- cses_int[cses_int$cntry == "IT",]
hu_r <- rec_int[rec_int$W1cntry == "HU",]
pl_r <- rec_int[rec_int$W1cntry == "PL",]
it_r <- rec_int[rec_int$W1cntry == "IT",]
hu_c <- cses_int[cses_int$cntry == "HU",]
no_c <- cses_int[cses_int$cntry == "NO",]

fit_de_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = de_r)
fit_es_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = es_r)
fit_de_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = de_c)
fit_se_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = se_c)
fit_fr_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = fr_c)
fit_fr_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = fr_r)
fit_at_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = at_c)
fit_fi_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = fi_c)
fit_dk_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = dk_r)
fit_it_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = it_c)
fit_hu_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = hu_r)
fit_pl_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = pl_r)
fit_it_r <- lm(W1rrp_sym ~ W1anti_imm * W1dissat + W1age + W1female + W1edu + W1rile + W1unemp + W1depr_pers , data = it_r)
fit_hu_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = hu_c)
fit_no_c <- lm(rrp_sym ~ anti_imm * dissat + yearborn + female + edu + rile + unemp + income, data = no_c)


plot_de_r <- plot_model(fit_de_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "DE(R)")
plot_es_r <- plot_model(fit_es_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "ES(R)")
plot_de_c <- plot_model(fit_de_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "DE(C)")
plot_se_c <- plot_model(fit_se_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "SE(C)")
plot_fr_c <- plot_model(fit_fr_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "FR(C)")
plot_fr_r <- plot_model(fit_fr_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "FR(R)")
plot_at_c <- plot_model(fit_at_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "AT(C)")
plot_fi_c <- plot_model(fit_fi_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "FI(C)")
plot_dk_r <- plot_model(fit_dk_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "DK(R)")
plot_it_c <- plot_model(fit_it_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "IT(C)")
plot_hu_r <- plot_model(fit_hu_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "HU(R)")
plot_pl_r <- plot_model(fit_pl_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "PL(R)")
plot_it_r <- plot_model(fit_it_r, type = "pred", terms = c("W1dissat", "W1anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "IT(R)")
plot_hu_c <- plot_model(fit_hu_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "HU(C)")
plot_no_c <- plot_model(fit_no_c, type = "pred", terms = c("dissat", "anti_imm"),
                        mdrt.values = "meansd", show.legend = F, title = "NO(C)")

plot_grid(list(plot_de_r,plot_es_r,plot_de_c,plot_se_c,
               plot_fr_c,plot_fr_r,plot_at_c,plot_fi_c,
               plot_dk_r,plot_it_c,plot_hu_r,plot_pl_r,
               plot_it_r,plot_hu_c,plot_no_c))

ctable(rec_int$W1cntry, rec_int$W1dissat)
summary(fit_no_c)

