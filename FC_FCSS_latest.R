####FC####

#### read data ####

#install.packages("openxlsx")
library(openxlsx)

dat<-read.xlsx("00_final_masterfile_final.xlsx", sheet="FC")


str(dat)


dat$pop<-as.factor(dat$pop)
dat$pop_ID<-as.factor(dat$pop_ID)
dat$ploidy_all_levels<-as.factor(dat$ploidy_all_levels)

dat$PK1<-as.numeric(dat$PK1)
dat$counts_leaf<-as.numeric(dat$counts_leaf)
dat$counts_total<-as.numeric(dat$counts_total)

str(dat)

#### sample size and population means ####

sample_size_pop_FC<-tapply(dat$PK1, dat$pop, length)#there are no NAs

#write.csv(sample_size_pop_FC, "sample_size_pop_FC.csv")




#min
mean_pop_min_FC<-tapply(dat$PK1, dat$pop, min)#there are no NAs

#write.csv(mean_pop_min_FC, "mean_pop_min_FC.csv")


#max
mean_pop_max_FC<-tapply(dat$PK1, dat$pop, max)#there are no NAs

#write.csv(mean_pop_max_FC, "mean_pop_max_FC.csv")








####FCSS####

# read data

#install.packages("openxlsx")
library(openxlsx)

dat<-read.xlsx("00_final_masterfile_final.xlsx", sheet="FCSS")


str(dat)


dat$pop<-as.factor(dat$pop)
dat$pop_ID<-as.factor(dat$pop_ID)
dat$pop_ID_nr<-as.factor(dat$pop_ID_nr)
dat$ploidy_embryo<-as.factor(dat$ploidy_embryo)
dat$sex<-as.factor(dat$sex)
dat$asex<-as.factor(dat$asex)
dat$sexuality_simple<-as.factor(dat$sexuality_simple)

dat$PI<-as.numeric(dat$PI)
dat$counts_total<-as.numeric(dat$counts_total)

str(dat)
#


dat2<-read.xlsx("00_final_masterfile_final.xlsx", sheet="Tabelle4")
str(dat2)

dat2$pop_ID <- as.factor(dat2$pop_ID)

res <- tapply(dat2$mean_embryo, dat2$pop_ID,  mean, na.rm=TRUE)

write.csv(res, "res.csv")


#### sample size and population means ####

#ind per pop

sample_size_ind_pop_FCSS<-tapply(dat$ID, dat$pop, FUN = function(x) length(unique(x)))#there are no NAs

#write.csv(sample_size_ind_pop_FCSS, "sample_size_ind_per_pop_FCSS.csv")



#seeds per pop

sample_size_seeds_pop_FCSS<-tapply(dat$pop_ID_nr, dat$pop, length)#there are no NAs

#write.csv(sample_size_seeds_pop_FCSS, "sample_size_seeds_per_pop_FCSS.csv")




#min_embryo
min_embryo_pop_FCSS<-tapply(dat$mean_embryo, dat$pop, min, na.rm = TRUE)

write.csv(min_embryo_pop_FCSS, "min_embryo_pop_FCSS.csv")


#max_embryo
max_embryo_pop_FCSS<-tapply(dat$mean_embryo, dat$pop, max, na.rm = TRUE)

#write.csv(max_embryo_pop_FCSS, "max_embryo_pop_FCSS.csv")


#mean_embryo_per_ind
mean_embryo_ind_FCSS<-tapply(dat$mean_embryo, dat$pop_ID, mean, na.rm=TRUE)

#write.csv(mean_embryo_ind_FCSS, "mean_embryo_ind_FCSS2.csv")




#mean_embryo_per_pop
mean_embryo_pop_FCSS<-tapply(dat$mean_embryo, dat$pop, mean, na.rm=TRUE)

#write.csv(mean_embryo_pop_FCSS, "mean_embryo_pop_FCSS2.csv")





#ploidy_levels_embryo
ploidy_levels_embryo_pop_FCSS<-table(dat$pop, dat$ploidy_embryo)

#write.csv(ploidy_levels_embryo_pop_FCSS, "ploidy_levels_embryo_pop_FCSS.csv")


#sexuality_endosperm
sexuality_endosperm_pop_FCSS<-table(dat$pop, dat$sexuality)

#write.csv(sexuality_endosperm_pop_FCSS, "sexuality_endosperm_pop_FCSS_pop_FCSS.csv")




#peak_index_min

peak_index_min_pop_FCSS<-tapply(dat$PI, dat$pop, min, na.rm=TRUE)

#write.csv(peak_index_min_pop_FCSS, "peak_index_min_pop_FCSS_pop_FCSS.csv")



#peak_index_max

peak_index_max_pop_FCSS<-tapply(dat$PI, dat$pop, max, na.rm=TRUE)

#write.csv(peak_index_max_pop_FCSS, "peak_index_max_pop_FCSS_pop_FCSS.csv")



#pi_pop

peak_index_max_pop_FCSS<-tapply(dat$PI, dat$pop, max, na.rm=TRUE)

#write.csv(peak_index_max_pop_FCSS, "peak_index_max_pop_FCSS_pop_FCSS.csv")




#sexuality_pop
mean_sexual_pop_FCSS<-tapply(dat$sexual, dat$pop, mean, na.rm=TRUE)

#write.csv(mean_sexual_pop_FCSS, "mean_sexuality_pop_FCSS.csv")


#reviser_pop
reviser_pop_FCSS<-table(dat$pop, dat$reviser)

write.csv(reviser_pop_FCSS, "reviser_pop_FCSS.csv")




#sexuality_ind
mean_sexual_ind_FCSS<-tapply(dat$sexual, dat$pop_ID, mean, na.rm=TRUE)

#write.csv(mean_sexual_ind_FCSS, "mean_sexuality_ind_FCSS.csv")



#ploidy_levels_embryo_ind
ploidy_levels_embryo_ind_FCSS<-table(dat$pop_ID, dat$ploidy_embry_total)

write.csv(ploidy_levels_embryo_ind_FCSS, "ploidy_levels_embryo_ind_FCSS.csv")



# repro mode per pop

res <- table(dat$pop, dat$sexuality_exact)

#write.csv(res, "repro_mode_per_pop.csv")

####reproduction modes per ploidy level####
par(mfrow=c(2,2))

#subsets

# 3D Exploded Pie Chart

#install.packages("plotrix")
library(plotrix)


dat_2n<-subset(dat, dat$ploidy_embry_total=="diploid")

str(dat_2n$ploidy_embry_total)

table(dat_2n$ploidy_embry_total, dat_2n$sexuality_simple)

slices <- c(99.0,1.0) 
lbls <- c("99.0% sexual seeds", "1.0% asexual seeds")
pie3D(slices,labels=lbls,explode=0.1, col=c("orange", "dodgerblue"))

#?pie3D

dat_3n<-subset(dat, dat$ploidy_embry_total=="triploid")

table(dat_3n$ploidy_embry_total, dat_3n$sexuality_simple)

slices <- c(100) 
lbls <- c("100% asexual seeds")
pie3D(slices,labels=lbls,explode=0.1, col=c( "dodgerblue"))


dat_4n<-subset(dat, dat$ploidy_embry_total=="tetraploid")

table(dat_4n$ploidy_embry_total, dat_4n$sexuality_simple)

slices <- c(4.5,95.5) 
lbls <- c("4.5% sexual seeds", "95.5% asexual seeds")
pie3D(slices,labels=lbls,explode=0.1, col=c("orange", "dodgerblue"))



dat_6n<-subset(dat, dat$ploidy_embry_total=="hexaploid")

table(dat_6n$ploidy_embry_total, dat_6n$sexuality_simple)

slices <- c(6.7, 93.3) 
lbls <- c("6.7% sexual seeds", "93.3% asexual seeds")
pie3D(slices,labels=lbls,explode=0.1, col=c("orange", "dodgerblue"))


# Simple Pie Chart

par(mfrow=c(2,2))

slices1 <- c(99.0,1.0) # 2n
slices2 <- c(100) # 3n
slices3 <- c(4.1,95.9) #4n
slices4 <- c(6.3, 93.7) #6m

pie(slices1)
pie(slices2)
pie(slices3)
pie(slices4)



# different apomictic pathways

tapply(dat$pop, dat$sexuality_exact, length)


# sex per individual

res <- tapply(dat$sexual, dat$pop_ID,  mean)

write.csv(res, "sexuality_per_ind.csv")


#### quality checks and variable selection genetic diversity ####

library(openxlsx)

dat_genetics<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_diversity_ISC_BSC")


str(dat_genetics)




het_sites_pop<-tapply(dat_genetics$het_sites, dat_genetics$pop, mean)
#write.csv(het_sites_pop, "het_sites_pop.csv")


#differences between two randomly sampled individuals of a population


boxplot(dat_genetics$het_sites_1_2~dat_genetics$ind_1_2)

dat_genetics_1<-subset(dat_genetics, ind_1_2=="1")
shapiro.test(dat_genetics_1$het_sites_1_2)#nope

dat_genetics_2<-subset(dat_genetics, ind_1_2=="2")
shapiro.test(dat_genetics_2$het_sites_1_2)#nope

wilcox.test(dat_genetics$het_sites_1_2~dat_genetics$ind_1_2)#significant



### individual sexuality to individual genetic diversity?

# read data

#install.packages("openxlsx")
library(openxlsx)

dat_gen_ind<-read.xlsx("00_final_masterfile_final.xlsx", sheet="FCSS_pop_ind")


str(dat_gen_ind)

mod_gen_ind<-glm(sexuality_ind~gen_ind, family = "quasibinomial", data = dat_gen_ind)
summary(mod_gen_ind)#sign.


plot(sexuality_ind~gen_ind, data = dat_gen_ind)

#without obligate apomicts (too many)

mod_gen_ind_2<-glm(sexuality_ind_wt_zeroes~gen_ind, family = "quasibinomial", data = dat_gen_ind)
summary(mod_gen_ind_2)#sign.


plot(sexuality_ind_wt_zeroes~gen_ind, data = dat_gen_ind)#non-sign.



####review: path analysis#####


#install.packages("openxlsx")
library(openxlsx)

dat_summary<-read.xlsx("00_final_masterfile_final.xlsx", sheet="locations_FC_FCSS_meassurements")


str(dat_summary)



dat_summary$pop_ID<-as.factor(dat_summary$pop_ID)
dat_summary$taxon<-as.factor(dat_summary$taxon)
dat_summary$DNA_ploidy<-as.factor(dat_summary$DNA_ploidy)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FC)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FCSS)
dat_summary$no_seeds<-as.factor(dat_summary$no_seeds)
dat_summary$ploidy_embryo<-as.factor(dat_summary$ploidy_embryo)
dat_summary$ploidy_embryo_simple<-as.factor(dat_summary$ploidy_embryo_simple)
dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)
dat_summary$reproductive_pathway_simple<-as.factor(dat_summary$reproductive_pathway_simple)
dat_summary$habitat<-as.factor(dat_summary$habitat)
dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)
dat_summary$seed_development_in_situ<-as.factor(dat_summary$seed_development_in_situ)
dat_summary$corolla_leaves_max<-as.numeric(dat_summary$corolla_leaves_max)

dat_summary$genetic_group_Nnet<-as.factor(dat_summary$genetic_group_Nnet)

dat_summary$genetic_group_snmf<-as.factor(dat_summary$genetic_group_snmf)

str(dat_summary)



#install.packages("lavaan")
library(lavaan)

#specification of model using auto.var argument


model <-'
#equation where sexuality is predicted by habitat and ploidy
sexuality_2 ~ ploidy_embryo_simple + habitat_simple
#equation where petals is predicted by sexuality
corolla_leaves_mean ~ sexuality_2
#equation where heterozygosity is predicted by habitat, ploidy, reproduction mode, sexuality
genetic_diversity_2_prop ~  ploidy_embryo_simple + habitat_simple + reproductive_pathway_ordered + sexuality_2
#estimating the variances of the exogenous variables (ploidy_embryo_simple, habitat_simple, reproduction mode)
ploidy_embryo_simple~~ploidy_embryo_simple
habitat_simple~~habitat_simple
reproductive_pathway_ordered~~reproductive_pathway_ordered
#estimating the covariances of the exogenous variables
ploidy_embryo_simple~~habitat_simple+reproductive_pathway_ordered
habitat_simple~~reproductive_pathway_ordered
#The auto.var argument when fitting the model can be used so that you do not have to directly request estimation of residual variances'


fit <- lavaan(model, data=dat_summary, auto.var=TRUE)

#problem: Error in lav_data_full(data = data, group = group, cluster = cluster,  : 

#lavaan ERROR: unordered factor(s) detected; make them numeric or ordered: ploidy_embryo_simple habitat_simpl

# no glm supported

#only beta



#install.packages("piecewiseSEM")
#library(piecewiseSEM)

devtools::install_github("jslefche/piecewiseSEM@devel")

library(piecewiseSEM)#version 2.2.0


#install developmental branch
#install Rtools before

library(devtools)

install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE, force=TRUE)
# Load library
library(piecewiseSEM)

# Read vignette
vignette("piecewiseSEM")


#install.packages('multcompView')
library(multcompView)

#Piecewise SEM (or confirmatory path analysis) expands upon traditional SEM by introducing a flexible mathematical framework that can incorporate a wide variety of model structures, distributions, and assumptions. These include: interactions and non-normal responses, random effects and hierarchical models, ...

#... and alternate correlation structures (including phylogenetic, spatial, and temporal).



### exclude rows with missing data in sexuality_2, ploidy_embryo_simple, habitat_simple, reproductive_pathway, genetic_diversity_2_prop, corolla_leaves_max and exclude triploids

dat_summary_without_3n<-dat_summary[-c(1,2,3,5,87,104,112,166,205,214,215,233, 47, 64, 102, 103, 108, 110, 229, 230, 233, 218, 219, 215, 214, 206, 194, 172, 170, 167, 162, 123, 2, 3, 95, 89, 87, 79, 72, 64, 57, 53, 48, 39, 35, 31, 19, 18, 15, 12, 1, 107, 109, 242),]


boxplot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple)


###### subset exclude triploid sample, without interactions, without phylogenetic distances


#ret <- handleCategoricalCoefs(ret, model, dat_summary_without_3n, test.statistic = "F", test.type = "II")

model <- psem(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple,     data = dat_summary_without_3n, family = quasibinomial), glm(corolla_leaves_max ~ sexuality_2, data = dat_summary_without_3n, family = quasipoisson),  glm(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway,     data = dat_summary_without_3n, family = quasibinomial))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

summary(model, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#makes no difference to Menard.OE, for categorcial variables, estimates are means (generated using emmeans package; see also multcomp::cld package for significance codes)

#specify claims as correlated errors (which make no biological sense)

basisSet(model, direction=NULL)
summary(update(model, ploidy_embryo_simple %~~% corolla_leaves_max), test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")

model2 <- update(model, habitat_simple %~~% corolla_leaves_max)
summary(model2, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")




model3 <- update(model2, corolla_leaves_max %~~% genetic_diversity_2_prop)
summary(model3, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")


model4 <- update(model3, reproductive_pathway %~~% corolla_leaves_max)
summary(model4, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model5 <- update(model4, corolla_leaves_max %~~% genetic_diversity_2_prop)
summary(model5, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model6 <- update(model5, corolla_leaves_max %~~% ploidy_embryo_simple)
summary(model6, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")






###### subset exclude triploid sample, without interactions, with genetic groups ######

#install.packages("lme4")
library(lme4)

#install.packages("lmerTest")
library(lmerTest)

str(dat_summary_without_3n$sexuality_2)

#underdispersion in models with sexuality (too conservate results, not so problematic like overdispersion)

?glmmPQL
library(MASS)
library(nlme)

#summary(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = list(genetic_group = ~1), family=quasibinomial, data= dat_summary_without_3n, correlation=corAR1(0, form = ~ 1 | pop_ID)))

#summary(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = list(genetic_group = ~1), family=quasibinomial, data= dat_summary_without_3n))

#summary(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = list(genetic_group = ~1), family=binomial, data= dat_summary_without_3n))
                
# no difference in p values 


#summary(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = list(genetic_group = ~1), family=quasipoisson, data= dat_summary_without_3n))


#summary(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = ~1|genetic_group, family=quasibinomial, data= dat_summary_without_3n))



#summary(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, family=quasibinomial, data= dat_summary_without_3n))



#glmer with many errors

#model <- psem(glmer(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + 1|genetic_group, family=binomial, data = dat_summary_without_3n), glmer(corolla_leaves_max ~ sexuality_2 + 1|genetic_group, family=poisson, data = dat_summary_without_3n),  glmer(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + 1|genetic_group, family=binomial, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

#summary(model, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#makes no difference to Menard.OE, for categorcial variables, estimates are means (generated using emmeans package; see also multcomp::cld package for significance codes)







### without "quasi" (makes only difference in R2 calculations) --> likelihood estimations can handle under- and overdispersion
#glmmPQL

### log genetic_diversity_2_prop (see below)

dat_summary_without_3n$genetic_diversity_2_log <- log(dat_summary_without_3n$genetic_diversity_2)

dat_summary_without_3n$genetic_diversity_2_log <- dat_summary_without_3n$genetic_diversity_2_log + 10

#due to gaussian model, arcsin transformation of sexuality

qqnorm(dat_summary_without_3n$sexuality_2)
qqline(dat_summary_without_3n$sexuality_2)

shapiro.test(dat_summary_without_3n$sexuality_2)

library(base)

shapiro.test(asin(dat_summary_without_3n$sexuality_2))

#why R2 of genetic div response so low?? y standardization??
#?rsquared
#rsquared(model)
#rsquared(model, method = "delta")
#rsquared(model, method = "lognormal")
#rsquared(model, method = "trigamma")

#model <- psem(glmmPQL(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway, random = ~1|genetic_group, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

#summary(model, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#

#the issue is the low range of genetic_diversity_2_prop (widely below 0.1), for binomial 0-1 would be optimal (like sexuality)

#shapiro.test(log(dat_summary_without_3n$genetic_diversity_2_prop))#yes
#qqnorm(dat_summary_without_3n$genetic_diversity_2_prop)
#qqline(dat_summary_without_3n$genetic_diversity_2_prop)
#hist(dat_summary_without_3n$genetic_diversity_2_prop)


model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, random = ~1|genetic_group, family=binomial, data = dat_summary_without_3n), glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group, family=poisson, data = dat_summary_without_3n),  glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway, random = ~1|genetic_group, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

summary(model, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#



### specify claims as correlated errors (which make no biological sense)

basisSet(model, direction=NULL)

model2 <- update(model, habitat_simple %~~% corolla_leaves_max)
summary(model2, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")




model3 <- update(model2, corolla_leaves_max %~~% genetic_diversity_2_log)
summary(model3,  intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")


model4 <- update(model3, reproductive_pathway %~~% corolla_leaves_max)
summary(model4, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



#model5 <- update(model4, corolla_leaves_max %~~% genetic_diversity_2_log)
#(model5, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model5 <- update(model4, corolla_leaves_max %~~% ploidy_embryo_simple)
summary(model5, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")

plot(genetic_diversity_2 ~ sexuality_2, data=dat_summary_without_3n)
summary(glm(genetic_diversity_2_log ~ sexuality_2, data=dat_summary_without_3n, gaussian))


coefs(model6, standardize = "scale")


###### subset exclude triploid sample, with interactions, with genetic groups ######


model_int <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + habitat_simple_ploidy_embryo_simple, random = ~1|genetic_group, family=binomial, data = dat_summary_without_3n), glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group, family=poisson, data = dat_summary_without_3n),  glmmPQL(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway, random = ~1|genetic_group, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no biological sense), goodness of fit --> highly significant

summary(model_int, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#



### specify claims as correlated errors (which make no biological sense)

basisSet(model, direction=NULL)

model_int2 <- update(model_int, habitat_simple %~~% corolla_leaves_max)
summary(model2, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")




model_int3 <- update(model_int2, corolla_leaves_max %~~% genetic_diversity_2_prop)
summary(model_int3, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")


model_int4 <- update(model_int3, reproductive_pathway %~~% corolla_leaves_max)
summary(model_int4, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model_int5 <- update(model_int4, corolla_leaves_max %~~% genetic_diversity_2_prop)
summary(model_int5, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model_int6 <- update(model_int5, corolla_leaves_max %~~% ploidy_embryo_simple)
summary(model_int6, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")















############ tries for correlation matrix based on genetic distances
dat_genetic_distances <- read.xlsx("genetic_distancles_all_97_min30_old_without_preplate.xlsx", sheet="final_rev")
               
dat_genetic_distances$pop_ID <- as.factor(dat_genetic_distances$pop_ID)

str(dat_genetic_distances$pop_ID)

dat_genetic_distances <- as.matrix(dat_genetic_distances[, -c(1)])

#install.packages("corpcor")

library(corpcor)
make.positive.definite()

obj <- cov(dat_genetic_distances)

obj <- cov2cor(dat_genetic_distances)

write.csv(obj, "correlation_matrix.csv")


#average columns and rows per pop manually



# import correlation matrix

dat_genetic_correlations <- read.xlsx("genetic_distancles_all_97_min30_old_without_preplate.xlsx", sheet="final_corr_rev_means_sel_w")

dat_genetic_correlations <- as.matrix(dat_genetic_correlations)

dat_genetic_correlations_pos2 <- make.positive.definite(dat_genetic_correlations)

#normalize matrix (0-1)

#dat_genetic_correlations_normalized = (dat_genetic_correlations-min(dat_genetic_correlations))/(max(dat_genetic_correlations)-min(dat_genetic_correlations))


library(nlme)
?corMatrix
?corCAR1
?corSymm

#obj_corSymm <- corSymm(value=dat_genetic_correlations)

obj_corSymm <- corSymm(value=dat_genetic_correlations[col(dat_genetic_correlations) < row(dat_genetic_correlations)], form = ~ 1)


#obj_corSymm <- corSymm(value=dat_genetic_correlations[col(dat_genetic_correlations) < row(dat_genetic_correlations)], form = ~ 1 | as.factor(pop_ID)) # factor specified here for further modeling



#dat_genetic_correlation_matrix <- data.matrix(dat_genetic_correlations, rownames = " ")


library(nlme)
?gls

#component models

psem(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple,     data = dat_summary_without_3n, family = quasibinomial), glm(corolla_leaves_max ~ sexuality_2, data = dat_summary_without_3n, family = poisson),  glm(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway,     data = dat_summary_without_3n, family = quasibinomial))



model_component_1 <- gls(sexuality_2 ~ ploidy_embryo_simple + habitat_simple, data = dat_summary_without_3n, correlation=obj_corSymm)



model_component_2 <- gls(corolla_leaves_max ~ sexuality_2, data = dat_summary_without_3n, correlation=obj_corSymm)



model_component_3 <- gls(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway, data = dat_summary_without_3n, correlation=obj_corSymm)








model <- psem(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple,     data = dat_summary_without_3n, family = quasibinomial), glm(corolla_leaves_max ~ sexuality_2, data = dat_summary_without_3n, family = poisson),  glm(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + 1 genetic distances,     data = dat_summary_without_3n, family = quasibinomial))

summary(model, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")






#### subset exclude triploid sample, with interactions, without phylogenetic distances

#create interaction terms 

int_ploidy_habitat <- interaction(dat_summary_without_3n$ploidy_embryo_simple:dat_summary_without_3n$habitat_simple)  # create an interaction variable
levels(int_ploidy_habitat)

contrasts(int_ploidy_habitat)



#show all collinearities

mod <- glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple +  habitat_simple_ploidy_embryo_simple
,     data = dat_summary_without_3n, family = quasibinomial)
summary(mod)

mod <- psem(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple +  ploidy_embryo_simple:habitat_simple,     data = dat_summary_without_3n, family = quasibinomial))
summary(mod, test.statistic = "F", test.type="III")



mod2 <- glm(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + ploidy_embryo_simple:habitat_simple + ploidy_embryo_simple:reproductive_pathway + reproductive_pathway:habitat_simple,     data = dat_summary_without_3n, family = quasibinomial)

summary(mod2)


mod2 <- update(mod, ~.  - ploidy_embryo_simple6N:habitat_simpleopen_habitats)
summary(mod2)

#remove collinear level interactions: The easiest way might be to manipulate the model matrix to remove the unwanted columns

int_ploidy_habitat <- model.matrix(sexuality_2 ~ ploidy_embryo_simple:habitat_simple,     data = dat_summary_without_3n)
omit <- grep("[:]fs[^ploidy_embryo_simple6N:habitat_simpleopen_habitats]", colnames(int_ploidy_habitat))
xx <- xx[, -omit]

lm(y ~ 0 + xx)



model <- psem(glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple +  ploidy_embryo_simple:habitat_simple,     data = dat_summary_without_3n, family = quasibinomial), glm(corolla_leaves_max ~ sexuality_2, data = dat_summary_without_3n, family = poisson),  glm(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + ploidy_embryo_simple:habitat_simple + ploidy_embryo_simple:reproductive_pathway + reproductive_pathway:habitat_simple,     data = dat_summary_without_3n, family = quasibinomial))

contrasts(dat_summary_without_3n$reproductive_pathway:dat_summary_without_3n$habitat_simple)

summary(glm(genetic_diversity_2_prop ~ reproductive_pathway:habitat_simple, singular.ok = TRUE,    data =      
dat_summary_without_3n, family = quasibinomial))

summary(model)

model <- psem(glm(genetic_diversity_2_prop ~ reproductive_pathway:habitat_simple, singular.ok = TRUE,    data = dat_summary_without_3n, family = quasibinomial))

summary(model, test.statistic = "F", test.type="III", singular.ok = TRUE)


sum <- contrasts(dat_summary_without_3n$reproductive_pathway:dat_summary_without_3n$habitat_simple)

# subset exclude triploid sample, with interactions, with phylogenetic distances



#### basic regressions among sexuality, ploidy level, repro pathway, corrola leaves and genetic diversity ####



# calculate phylogenetic distances


library(ape)
my_tree <- read.tree('Figure_T13_raxml_min10.tre.txt') # For Newick format trees


my_tree$tip.label # Check the tip labels of your tree

my_tree2<-as.phylo(my_tree)

object <- corBrownian(1, phy = my_tree2)

## S3 method for class 'corBrownian'
coef(object, unconstrained = TRUE)
## S3 method for class 'corBrownian'
corMatrix(object, covariate = getCovariate(object), corr = TRUE)


#install.packages("adephylo")
library(adephylo)

distTips(my_tree2, tips = "all", method = c("patristic"), useC = TRUE)


#rownames(my_data) <- gsub(' ', '_', my_data$species_name_with_spaces)

#rownames(my_tree) <- my_tree$species_name




# read data

#install.packages("openxlsx")
library(openxlsx)

dat_summary<-read.xlsx("00_final_masterfile_final.xlsx", sheet="locations_FC_FCSS_meassurements")


str(dat_summary)



dat_summary$pop_ID<-as.factor(dat_summary$pop_ID)
dat_summary$taxon<-as.factor(dat_summary$taxon)
dat_summary$DNA_ploidy<-as.factor(dat_summary$DNA_ploidy)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FC)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FCSS)
dat_summary$no_seeds<-as.factor(dat_summary$no_seeds)
dat_summary$ploidy_embryo<-as.factor(dat_summary$ploidy_embryo)
dat_summary$ploidy_embryo_simple<-as.factor(dat_summary$ploidy_embryo_simple)
dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)
dat_summary$reproductive_pathway_simple<-as.factor(dat_summary$reproductive_pathway_simple)
dat_summary$habitat<-as.factor(dat_summary$habitat)
dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)
dat_summary$seed_development_in_situ<-as.factor(dat_summary$seed_development_in_situ)


str(dat_summary)

# pops per reproduction mode

tapply(dat_summary$pop_ID, dat_summary$reproductive_pathway, length)



# sexuality per reproduction mode

tapply(dat_summary$sexuality, dat_summary$reproductive_pathway, mean)







sample_size_per_taxon<-tapply(dat_summary$pop_ID, dat_summary$taxon_2020_garden_plants, FUN = function(x) length(unique(x)))#there are no NAs

min(sample_size_per_taxon)
max(sample_size_per_taxon)


####1. genetic diversity####

par(mfrow=c(1,3))




#genetic diversity and ploidy levels

#subset exclude triploid sample

dat_summary_without_3n<-dat_summary[-c(107, 109, 242),]

boxplot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple)




#install.packages("ggplot2")
library(ggplot2)

# Basic violin plot

dat_summary_without_3n$ploidy_embryo_simple<-as.factor(dat_summary_without_3n$ploidy_embryo_simple)# group variable has to be a factor

p <- ggplot(dat_summary_without_3n, aes(x=ploidy_embryo_simple, y=genetic_diversity_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_without_3n, aes(x=ploidy_embryo_simple, y=genetic_diversity_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_without_3n, aes(x=ploidy_embryo_simple, y=genetic_diversity_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot


tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))




dat_summary_2n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="2N")
shapiro.test(dat_summary_2n$genetic_diversity_2)#no

dat_summary_4n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="4N")
shapiro.test(dat_summary_4n$genetic_diversity_2)#no

dat_summary_6n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="6N")
shapiro.test(dat_summary_6n$genetic_diversity_2)#yes

#modxx<-aov(dat_summary_without_3n$genetic_diversity ~ dat_summary_without_3n$ploidy_embryo_simple)
#summary(modxx)#no

kruskal.test(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple)#sign. differences

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity, dat_summary_without_3n$ploidy_embryo_simple, p.adjust.method = "fdr")



plot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple)

tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple, mean, na.rm=TRUE)# small differences 

tapply(dat_summary$genetic_diversity_2, dat_summary$ploidy_embryo_simple, mean, na.rm=TRUE)# small differences 

kruskal.test(dat_summary$genetic_diversity_2 ~ dat_summary$ploidy_embryo_simple)#sign. differences

pairwise.wilcox.test(dat_summary$genetic_diversity, dat_summary$ploidy_embryo_simple)




tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))



#genetic diversity and reproductive pathway


boxplot(dat_summary$genetic_diversity_2 ~ dat_summary$reproductive_pathway)




#install.packages("ggplot2")
library(ggplot2)

# Basic violin plot

dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)# group variable has to be a factor



p <- ggplot(dat_summary, aes(x=reproductive_pathway, y=genetic_diversity_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary, aes(x=reproductive_pathway, y=genetic_diversity_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary, aes(x=reproductive_pathway, y=genetic_diversity_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot









kruskal.test(dat_summary$genetic_diversity_2 ~ dat_summary$reproductive_pathway)# sign. differences

pairwise.wilcox.test(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway, p.adjust.method = "fdr")

tapply(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway, mean, na.rm=TRUE)# small differences between 4n and 2n (suggesting rather autopolyploidization)

tapply(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway, FUN = function(x) length(x[!is.na(x)]))# small differences between 4n and 2n (suggesting rather autopolyploidization)




#genetic diversity and habitat


boxplot(dat_summary$genetic_diversity_2 ~ dat_summary$habitat)

kruskal.test(dat_summary$genetic_diversity_2 ~ dat_summary$habitat)#no sign. differences

pairwise.wilcox.test(dat_summary$genetic_diversity_2, dat_summary$habitat)

tapply(dat_summary$genetic_diversity_2, dat_summary$habitat, mean, na.rm=TRUE)# small differences 







#genetic diversity and habitat simple


boxplot(dat_summary$genetic_diversity_2 ~ dat_summary$habitat_simple)




#install.packages("ggplot2")
library(ggplot2)

# Basic violin plot

dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)# group variable has to be a factor

p <- ggplot(dat_summary, aes(x=habitat_simple, y=genetic_diversity_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary, aes(x=habitat_simple, y=genetic_diversity_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary, aes(x=habitat_simple, y=genetic_diversity_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot



tapply(dat_summary$genetic_diversity_2, dat_summary$habitat_simple, FUN = function(x) length(x[!is.na(x)]))





dat_summary_forest<-subset(dat_summary, dat_summary$habitat_simple=="forest")
shapiro.test(dat_summary_forest$genetic_diversity)#no

dat_summary_open_habitats<-subset(dat_summary, dat_summary$habitat_simple=="open_habitats")
shapiro.test(dat_summary_open_habitats$genetic_diversity_2)#no



wilcox.test(dat_summary$genetic_diversity_2 ~ dat_summary$habitat_simple)#sign. differences



tapply(dat_summary$genetic_diversity_2, dat_summary$habitat_simple, mean, na.rm=TRUE)# small differences 


tapply(dat_summary$genetic_diversity_2, dat_summary$habitat_simple, FUN = function(x) length(x[!is.na(x)]))


####GLMER total ####

#install.packages("lme4")
library(lme4)


mod_genetic_diversity<-glm(genetic_diversity ~ ploidy_embryo_simple + reproductive_pathway + ploidy_embryo_simple:reproductive_pathway+ habitat_simple + ploidy_embryo_simple:habitat_simple + reproductive_pathway:habitat_simple,     data = dat_summary_without_3n, family = quasibinomial)#with taxa probably GLMER
summary(mod_genetic_diversity)



mod_genetic_diversity_bin<-glm(genetic_diversity ~ ploidy_embryo_simple + reproductive_pathway + ploidy_embryo_simple:reproductive_pathway+ habitat_simple + ploidy_embryo_simple:habitat_simple + reproductive_pathway:habitat_simple,     data = dat_summary_without_3n, family = binomial)
AIC(mod_genetic_diversity_bin)#147.06




#rm ploidy_embryo_simple:reproductive_pathway


mod_genetic_diversity2<-glm(genetic_diversity ~ ploidy_embryo_simple + reproductive_pathway + habitat_simple + ploidy_embryo_simple:habitat_simple+ habitat_simple:reproductive_pathway,     data = dat_summary_without_3n, family = quasibinomial)
summary(mod_genetic_diversity2)

anova(mod_genetic_diversity, mod_genetic_diversity2, test="F")# allowed



mod_genetic_diversity2_bin<-glm(genetic_diversity ~ ploidy_embryo_simple + reproductive_pathway + ploidy_embryo_simple:reproductive_pathway+ habitat_simple + habitat_simple:reproductive_pathway,     data = dat_summary_without_3n, family = binomial)
summary(mod_genetic_diversity2_bin)
AIC(mod_genetic_diversity2_bin)#143.67

# account for underdispersion witn quasibinomial (estimation of dispersion parameter!!!!)
#underdispersion leads to conservatism in statistical inference (e.g. decreased power, lowered type I errors), in contrast to overdispersion which leads to optimism (inflated type I error rate etc.), so reviewers etc. tend not to worry about it as much

#https://stats.stackexchange.com/questions/134783/glm-for-proportional-data-and-underdispersion











####GLMER total seperate models####


#install.packages("MuMIn")
#library(MuMIn)
#install.packages("lme4")
#library(lme4)

#install.packages("multcomp")
#library(multcomp)


#install.packages("lsmeans")
library(lsmeans)
lsmeans(mod_sexuality_12, pairwise ~ habitat_simple*reproductive_pathway)
?lsmeans

#install.packages("AICcmodavg")
#library(AICcmodavg)



# with interactions, SCALED, without 3N!!!!


#genetic diversity is also a proportional variable!!!!


mod_sexuality_2 <- glm(genetic_diversity_2_prop ~ corolla_leaves_mean, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_2)
plot(genetic_diversity_2_prop ~ corolla_leaves_mean, data = dat_summary_without_3n)                       



#mod_sexuality_3 <- glm(sexuality_2 ~ genetic_diversity_2_scaled:corolla_leaves_mean_scaled, data = dat_summary_without_3n, family="quasibinomial")
#summary(mod_sexuality_3)# sign 




mod_sexuality_4 <- glm(genetic_diversity_2_prop ~ corolla_leaves_mean:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_4)#sign.







mod_sexuality_5 <- glm(genetic_diversity_2_prop ~ ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_5)#sign.


lsmeans(mod_sexuality_5, pairwise ~ ploidy_embryo_simple)

#summary(glht(mod_sexuality_5, mcp(ploidy_embryo_simple="Tukey")))

tapply(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$ploidy_embryo_simple, mean, na.rm=TRUE)

kruskal.test(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$ploidy_embryo_simple)#sign

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$ploidy_embryo_simple)






mod_sexuality_6 <- glm(genetic_diversity_2_prop ~ ploidy_embryo_simple:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_6)#collinearites


lsmeans(mod_sexuality_6, pairwise ~ ploidy_embryo_simple:reproductive_pathway)

tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway, mean, na.rm=TRUE)

tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway, FUN = function(x) length(x[!is.na(x)]))


kruskal.test(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway)#sign.

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway, p.adjust.method = "fdr")

boxplot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway)





dat_summary_without_3n$ploidy_embryo_simple2 <- factor(dat_summary_without_3n$ploidy_embryo_simple, ordered = TRUE, levels = c("4N", "6N", "2N"))

mod_sexuality_6b <- glm(genetic_diversity_2_prop ~ ploidy_embryo_simple2:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_6b)#collinearites



dat_summary_without_3n$ploidy_embryo_simple2 <- factor(dat_summary_without_3n$ploidy_embryo_simple, ordered = TRUE, levels = c("6N", "4N", "2N"))

mod_sexuality_6c <- glm(genetic_diversity_2_prop ~ ploidy_embryo_simple2:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_6c)#collinearites












mod_sexuality_8 <- glm(genetic_diversity_2_prop ~ reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_8)#sign.


lsmeans(mod_sexuality_8, pairwise ~ reproductive_pathway)

#summary(glht(mod_sexuality_8, mcp(reproductive_pathway="Tukey")))

tapply(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$reproductive_pathway, mean, na.rm=TRUE)





kruskal.test(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$reproductive_pathway)#sign

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$reproductive_pathway)


tapply(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway, mean, na.rm=TRUE)

kruskal.test(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway)#sign

pairwise.wilcox.test(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway)




dat_summary_without_3n$reproductive_pathway_simple2 <- factor(dat_summary_without_3n$reproductive_pathway_simple, ordered = TRUE, levels = c("sexual", "apomictic"))

mod_sexuality_8a <- glm(genetic_diversity_2_prop ~ reproductive_pathway_simple2, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_8a)#sign.


lsmeans(mod_sexuality_8a, pairwise ~ reproductive_pathway_simple2)

#summary(glht(mod_sexuality_8a, mcp(reproductive_pathway_simple2="Tukey")))

tapply(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$reproductive_pathway_simple2, mean, na.rm=TRUE)

tapply(dat_summary$genetic_diversity_2, dat_summary$reproductive_pathway_simple, mean, na.rm=TRUE)



wilcox.test(dat_summary_without_3n$genetic_diversity_2_prop ~ dat_summary_without_3n$reproductive_pathway_simple2)





mod_sexuality_10 <- glm(genetic_diversity_2_prop ~ habitat_simple, data = dat_summary_without_3n, family="quasibinomial")#non-sign.
summary(mod_sexuality_10)#sign. (probably because )

lsmeans(mod_sexuality_10, pairwise ~ habitat_simple)

#summary(glht(mod_sexuality_8, mcp(reproductive_pathway="Tukey")))

tapply(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$habitat_simple, mean, na.rm=TRUE)

wilcox.test(dat_summary_without_3n$genetic_diversity_2_prop ~ dat_summary_without_3n$habitat_simple)






mod_sexuality_12 <- glm(genetic_diversity_2_prop ~ habitat_simple*reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12) 


lsmeans(mod_sexuality_12, pairwise ~ habitat_simple*reproductive_pathway)

kruskal.test(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$habitat_simple_reproductive_pathway)

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$habitat_simple_reproductive_pathway, p.adjust.method = "fdr")



kruskal.test(dat_summary$genetic_diversity_2, dat_summary$habitat_simple_reproductive_pathway)

pairwise.wilcox.test(dat_summary$genetic_diversity_2, dat_summary$habitat_simple_reproductive_pathway, p.adjust.method = "fdr")

tapply(dat_summary$genetic_diversity_2, dat_summary$habitat_simple_reproductive_pathway, mean, na.rm=TRUE)

tapply(dat_summary$genetic_diversity_2, dat_summary$habitat_simple_reproductive_pathway, FUN = function(x) length(x[!is.na(x)]))

boxplot(dat_summary$genetic_diversity_2 ~ dat_summary$habitat_simple_reproductive_pathway)



#summary(glht(mod_sexuality_12, linfct = mcp(habitat_simple="Tukey"))$linfct)

#summary(glht(mod_sexuality_12))

dat_summary_without_3n_open_habitats <- subset(dat_summary_without_3n, habitat_simple=="open_habitats")
dat_summary_without_3n_forest <- subset(dat_summary_without_3n, habitat_simple=="forest")


# open habitats
tapply(dat_summary_without_3n_open_habitats$genetic_diversity_2_prop, dat_summary_without_3n_open_habitats$reproductive_pathway, mean, na.rm=TRUE)


#forest
tapply(dat_summary_without_3n_forest$genetic_diversity_2_prop, dat_summary_without_3n_forest$reproductive_pathway, mean, na.rm=TRUE)








dat_summary_without_3n$reproductive_pathway2 <- factor(dat_summary_without_3n$reproductive_pathway, ordered = TRUE, levels = c("3_apomictic_facultative_sexual", "2_apomictic_obligate_asexual", "1_sexual"))

mod_sexuality_12e <- glm(genetic_diversity_2_prop ~ habitat_simple:reproductive_pathway2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12e)




mod_sexuality_13 <- glm(genetic_diversity_2_prop ~  habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_13)


lsmeans(mod_sexuality_13, pairwise ~ habitat_simple*ploidy_embryo_simple)



kruskal.test(dat_summary_without_3n$genetic_diversity_2_prop, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple)

pairwise.wilcox.test(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, p.adjust.method="fdr")

boxplot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$habitat_simple_ploidy_embryo_simple)

tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, mean, na.rm=TRUE)

tapply(dat_summary_without_3n$genetic_diversity_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))

#summary(glht(mod_sexuality_12, linfct = mcp(habitat_simple="Tukey"))$linfct)

#summary(glht(mod_sexuality_12))

dat_summary_without_3n_open_habitats <- subset(dat_summary_without_3n, habitat_simple=="open_habitats")
dat_summary_without_3n_forest <- subset(dat_summary_without_3n, habitat_simple=="forest")


# open habitats
tapply(dat_summary_without_3n_open_habitats$genetic_diversity_2_prop, dat_summary_without_3n_open_habitats$ploidy_embryo_simple, mean, na.rm=TRUE)


#forest
tapply(dat_summary_without_3n_forest$genetic_diversity_2_prop, dat_summary_without_3n_forest$ploidy_embryo_simple, mean, na.rm=TRUE)









dat_summary_without_3n$ploidy_embryo_simple2 <- factor(dat_summary_without_3n$ploidy_embryo_simple, ordered = TRUE, levels = c("4N", "6N", "2N"))

mod_sexuality_13b <- glm(genetic_diversity_2_prop ~  habitat_simple:ploidy_embryo_simple2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_13b)








#### Bonferoni Corrections


Input = ("
Variable          p_value
Ploidy_4N         5.44e-14
Ploidy_6N         1.65e-14
Repro_fac_sex     5.32e-12
Repro_oblig       8.15e-13
Repro_apom        2.6e-13
Habitat_open      0.0398
2N_forest         0.00223
2N_open           0.00792
4N_forest         1.57e-07
4N_open           5.93e-06
6N_forest         1.20e-11
6N_open           0.00792
forest_sex        1.58e-06
open_sex          0.000439
forest_fac_sex    3.85e-07
open_fac_sex      0.000439
forest_olig_asex  1.25e-06
open_oblig_asex   4.92e-05
")


Data = read.table(textConnection(Input),header=TRUE)



Data$Bonferroni =
  p.adjust(Data$p_value,
           method = "bonferroni")






####2. sexuality####




#sexuality versus genetic diversity


mod_sex_genetic<-glm(dat_summary$sexuality_2~dat_summary$genetic_diversity_2_scaled, family = binomial(link = logit))
summary(mod_sex_genetic)#sign.

#calculate overdispersion: ratio of residual deviance to degrees of freedom
#are error large than binomial errors?

163/221 # no overdispersion (<1)





mod_sex_genetic_quasibinomial<-glm(sexuality_2~genetic_diversity_2, family="quasibinomial", data = dat_summary_without_3n)
summary(mod_sex_genetic_quasibinomial)#sign.







plot(dat_summary$sexuality_2~dat_summary$genetic_diversity_2)


range(dat_summary$genetic_diversity_2, na.rm=TRUE)

xweight <- seq(0, 4, 0.001)

yweight <- predict(mod_sex_genetic_quasibinomial, list(genetic_diversity_2 = xweight),type="response")

lines(xweight, yweight)






#sexuality apomicts versus genetic diversity

#subsets

dat_summary_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway_simple=="apomictic")




mod_sex_genetic2<-glm(dat_summary_apomicts$sexuality_2~dat_summary_apomicts$genetic_diversity_2, family="quasibinomial")
summary(mod_sex_genetic2)#non-sign.


plot(dat_summary_apomicts$sexuality_2~dat_summary_apomicts$genetic_diversity_2)




#facultative sexuality apomicts versus genetic diversity

#subsets

dat_summary_fac_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway=="3_apomictic_facultative_sexual")



mod_sex_genetic3<-glm(sexuality_2~genetic_diversity_2, family="quasibinomial", data = dat_summary_fac_apomicts)
summary(mod_sex_genetic3)#non-sign.


plot(dat_summary_fac_apomicts$sexuality_2~dat_summary_fac_apomicts$genetic_diversity_2)



range(dat_summary_fac_apomicts$genetic_diversity_2, na.rm=TRUE)

xweight <- seq(0, 3, 0.001)

yweight <- predict(mod_sex_genetic3, list(genetic_diversity_2 = xweight),type="response")

lines(xweight, yweight)








# sexuality versus ploidy levels

boxplot(dat_summary$sexuality_2 ~ dat_summary$ploidy_embryo_simple)



#install.packages("ggplot2")
library(ggplot2)


# only own measurements and without 3n

dat_summary_own_without3n<-dat_summary[-c(6, 110, 113:120, 234, 4, 105, 106, 111, 108, 107, 109, 242),]

# Basic violin plot

dat_summary_own_without3n$ploidy_embryo_simple<-as.factor(dat_summary_own_without3n$ploidy_embryo_simple)# group variable has to be a factor

p <- ggplot(dat_summary_own_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_own_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_own_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot

tapply(dat_summary_own_without3n$sexuality_2, dat_summary_own$ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))


#only without 3n


dat_summary_without3n<-dat_summary[-c( 107, 109, 242),]

# Basic violin plot

dat_summary_without3n$ploidy_embryo_simple<-as.factor(dat_summary_without3n$ploidy_embryo_simple)# group variable has to be a factor

p <- ggplot(dat_summary_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_without3n, aes(x=ploidy_embryo_simple, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot

tapply(dat_summary_without3n$sexuality_2, dat_summary_without3n$ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))












dat_summary_2n<-subset(dat_summary_without_3n, dat_summary$ploidy_embryo_simple=="2N")
shapiro.test(dat_summary_2n$sexuality_2)#no


plot(dat_summary_2n$sexuality_2 ~ dat_summary_2n$genetic_diversity_2)



dat_summary_3n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="3N")
shapiro.test(dat_summary_3n$sexuality_2)#no

dat_summary_4n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="4N")
shapiro.test(dat_summary_4n$sexuality_2)#no

dat_summary_6n<-subset(dat_summary, dat_summary$ploidy_embryo_simple=="6N")
shapiro.test(dat_summary_6n$sexuality_2)#no

#modxx<-aov(dat_summary_without_3n$genetic_diversity ~ dat_summary_without_3n$ploidy_embryo_simple)
#summary(modxx)#no

kruskal.test(dat_summary$sexuality_2 ~ dat_summary$ploidy_embryo_simple)#sign. differences

pairwise.wilcox.test(dat_summary$sexuality_2, dat_summary$ploidy_embryo_simple)


tapply(dat_summary$sexuality_2, dat_summary$ploidy_embryo_simple, mean, na.rm=TRUE)



dat_summary_without_3n<-dat_summary[-c( 107, 109, 242),]

kruskal.test(dat_summary_without_3n$sexuality_2 ~ dat_summary_without_3n$ploidy_embryo_simple)#sign. differences


pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple, p.adjust.method = "fdr")




#sexuality and reproductive pathway


boxplot(dat_summary$sexuality_2 ~ dat_summary$reproductive_pathway_simple_without_obligates)


dat_summary_without_NA<-subset(dat_summary, reproductive_pathway_simple_without_obligates >0)





#install.packages("ggplot2")
library(ggplot2)

dat_summary_own<-dat_summary[-c(6, 110, 113:120, 234, 4, 105, 106, 111, 108),]

boxplot(dat_summary_own$sexuality_2 ~ dat_summary_own$reproductive_pathway)

tapply(dat_summary_own$sexuality_2, dat_summary_own$reproductive_pathway, FUN = function(x) length(x[!is.na(x)]))


# Basic violin plot

dat_summary_own$reproductive_pathway<-as.factor(dat_summary_own$reproductive_pathway)# group variable has to be a factor

p <- ggplot(dat_summary_own, aes(x=reproductive_pathway, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_own, aes(x=reproductive_pathway, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_own, aes(x=reproductive_pathway, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot












#install.packages("ggplot2")
library(ggplot2)


#remove 


# Basic violin plot

dat_summary_without_NA$reproductive_pathway_simple_without_obligates<-as.factor(dat_summary_without_NA$reproductive_pathway_simple_without_obligates)# group variable has to be a factor

p <- ggplot(dat_summary_without_NA, aes(x=reproductive_pathway_simple_without_obligates, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_without_NA, aes(x=reproductive_pathway_simple_without_obligates, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_without_NA, aes(x=reproductive_pathway_simple_without_obligates, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot












kruskal.test(dat_summary$sexuality_2 ~ dat_summary$reproductive_pathway)# sign. differences

pairwise.wilcox.test(dat_summary$sexuality_2, dat_summary$reproductive_pathway)

tapply(dat_summary$sexuality_2, dat_summary$reproductive_pathway, mean, na.rm=TRUE)

tapply(dat_summary$sexuality_2, dat_summary$reproductive_pathway, FUN = function(x) length(x[!is.na(x)]))





#sexuality vs. corolla leaves (sexual)


dat_summary_sexual<-subset(dat_summary, reproductive_pathway_simple=="sexual")

plot(dat_summary_sexual$sexuality_2 ~ dat_summary_sexual$corolla_leaves_mean)






# corolla leaves versus sexuality


mod_genetic_div_corolla<-glmmPQL(corolla_leaves_max ~ sexuality_2, family="poisson", random = ~1|genetic_group, data = dat_summary)
summary(mod_genetic_div_corolla)#sign.

mod_genetic_div_corolla<-glm(corolla_leaves_max ~ sexuality_2, family="poisson", data = dat_summary)
summary(mod_genetic_div_corolla)#sign.


plot(corolla_leaves_max ~ sexuality_2, data = dat_summary)


range(dat_summary$sexuality_2, na.rm=TRUE)

xweight <- seq(0, 1, 0.001)

yweight <- predict(mod_genetic_div_corolla, list(sexuality_2 = xweight),type="response")

lines(xweight, yweight)






# sexuality versus max corolla leaves


mod_genetic_div_corolla<-glm(sexuality_2 ~ corolla_leaves_max, family="binomial", data = dat_summary)
summary(mod_genetic_div_corolla)#sign.

plot(dat_summary$sexuality_2~dat_summary$corolla_leaves_max)


range(dat_summary$corolla_leaves_max, na.rm=TRUE)

xweight <- seq(0, 17.5, 0.001)

yweight <- predict(mod_genetic_div_corolla, list(corolla_leaves_max = xweight),type="response")

lines(xweight, yweight)







# sexuality apomicts versus corolla leaves

dat_summary_apomicts<-subset(dat_summary, reproductive_pathway_simple=="apomictic")

mod_genetic_div_corolla_apomicts<-glm(sexuality_2~corolla_leaves_mean, family="quasibinomial", data = dat_summary_apomicts)
summary(mod_genetic_div_corolla_apomicts)#sign.

plot(dat_summary_apomicts$sexuality_2~dat_summary_apomicts$corolla_leaves_mean)



range(dat_summary_apomicts$corolla_leaves_mean, na.rm=TRUE)

xweight <- seq(0, 17.5, 0.001)

yweight <- predict(mod_genetic_div_corolla_apomicts, list(corolla_leaves_mean = xweight),type="response")

lines(xweight, yweight)






# sexuality apomicts facult sexual versus corolla leaves

dat_summary_apomicts_fac<-subset(dat_summary, reproductive_pathway=="3_apomictic_facultative_sexual")

mod_genetic_div_corolla_apomicts_fac<-glm(sexuality_2~corolla_leaves_mean, family="quasibinomial", data = dat_summary_apomicts_fac)
summary(mod_genetic_div_corolla_apomicts_fac)#sign.

plot(dat_summary_apomicts_fac$sexuality_2~dat_summary_apomicts_fac$corolla_leaves_mean)



range(dat_summary_apomicts_fac$corolla_leaves_mean, na.rm=TRUE)

xweight <- seq(0, 17.5, 0.001)

yweight <- predict(mod_genetic_div_corolla_apomicts_fac, list(corolla_leaves_mean = xweight),type="response")

lines(xweight, yweight)














#sexuality and habitat


boxplot(dat_summary$sexuality_2 ~ dat_summary$habitat)

tapply(dat_summary$sexuality_2, dat_summary$habitat, FUN = function(x) length(x[!is.na(x)]))


kruskal.test(dat_summary$sexuality_2 ~ dat_summary$habitat)#sign. differences

pairwise.wilcox.test(dat_summary$sexuality_2, dat_summary$habitat)#only forest and forest edge significant

tapply(dat_summary$sexuality_2, dat_summary$habitat, mean, na.rm=TRUE)# small differences 




#sexuality and habitat simple


boxplot(dat_summary$sexuality_2 ~ dat_summary$habitat_simple)


# only own measurements and without 3n

dat_summary_own<-dat_summary[-c(6, 110, 113:120, 234, 4, 105, 106, 111, 108),]

tapply

#install.packages("ggplot2")
library(ggplot2)

# Basic violin plot

dat_summary_own$habitat_simple<-as.factor(dat_summary_own$habitat_simple)# group variable has to be a factor

p <- ggplot(dat_summary_own, aes(x=habitat_simple, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_own, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_own, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot


tapply()


# all measurements

library(ggplot2)

# Basic violin plot

dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)# group variable has to be a factor

p <- ggplot(dat_summary, aes(x=habitat_simple, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot


tapply(dat_summary$sexuality_2, dat_summary$habitat_simple, FUN = function(x) length(x[!is.na(x)]))









wilcox.test(dat_summary$sexuality_2 ~ dat_summary$habitat_simple)#no sign. differences


tapply(dat_summary$sexuality_2, dat_summary$habitat_simple, mean, na.rm=TRUE)

tapply(dat_summary$sexuality_2, dat_summary$habitat_simple, FUN = function(x) length(x[!is.na(x)]))


#sexuality apomicts and habitat simple

dat_summary_apomicts<-subset(dat_summary_without_3n, dat_summary$reproductive_pathway_simple=="apomictic")

boxplot(dat_summary_apomicts$sexuality_2 ~ dat_summary_apomicts$habitat_simple)


library(ggplot2)

# Basic violin plot

dat_summary_apomicts$habitat_simple<-as.factor(dat_summary_apomicts$habitat_simple)# group variable has to be a factor

p <- ggplot(dat_summary_apomicts, aes(x=habitat_simple, y=sexuality_2)) + 
  geom_violin()
p

# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(dat_summary_apomicts, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE)
p + geom_boxplot(width=0.1)# trimmed with boxplot

p <- ggplot(dat_summary_apomicts, aes(x=habitat_simple, y=sexuality_2)) + geom_violin(trim=FALSE, fill="orange")# if more than one color --> fill=c(col1, col2)
p + geom_boxplot(width=0.1)# not trimmed with boxplot







wilcox.test(dat_summary_apomicts$sexuality_2 ~ dat_summary_apomicts$habitat_simple)#no sign. differences



tapply(dat_summary_apomicts$sexuality_2, dat_summary_apomicts$habitat_simple, mean, na.rm=TRUE)# small differences 




chisq.test(table(dat_summary$reproductive_pathway_simple, dat_summary$habitat_simple))

table(dat_summary$reproductive_pathway_simple, dat_summary$habitat_simple)


chisq.test(table(dat_summary$reproductive_pathway, dat_summary$habitat_simple))

table(dat_summary$reproductive_pathway, dat_summary$habitat_simple)


dat_summary_apomicts<-subset(dat_summary_apomicts, reproductive_pathway_simple="apomictic")
  
chisq.test(table(dat_summary_apomicts$reproductive_pathway, dat_summary_apomicts$habitat_simple))

table(dat_summary_apomicts$reproductive_pathway, dat_summary_apomicts$habitat_simple)

####GLMER total####

#install.packages("MuMIn")
#library(MuMIn)
#install.packages("lme4")
library(lme4)


# with interactions, SCALED, without 3N!!!!



mod_sexuality_int0<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + ploidy_embryo_simple:reproductive_pathway_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:reproductive_pathway + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int0)#sign.






#rm ploidy_embryo_simple:reproductive_pathway_simple


mod_sexuality_int<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:reproductive_pathway + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int)#sign.

anova(mod_sexuality_int0, mod_sexuality_int, test="F")#allowed


mod_sexuality_int_bin<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:reproductive_pathway + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="binomial")
AIC(mod_sexuality_int_bin)#45.91










#rm reproductive_pathway:genetic_diversity_2_scaled

mod_sexuality_int2<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway +  habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:ploidy_embryo_simple + habitat_simple:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int2)#sign.
anova(mod_sexuality_int, mod_sexuality_int2, test="F")# allowed


mod_sexuality_int2_bin<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="binomial")

AIC(mod_sexuality_int2_bin)#47.98





#rm habitat_simple:genetic_diversity_2_scaled

mod_sexuality_int3<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int3)#sign.
anova(mod_sexuality_int3, mod_sexuality_int2, test="F")# allowed




mod_sexuality_int3_bin<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="binomial")
summary(mod_sexuality_int3_bin)
AIC(mod_sexuality_int3_bin)#45.98



#alternatively with repro_simple

#rm habitat_simple:genetic_diversity_2_scaled

mod_sexuality_int3_alt<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int3_alt)#sign.
anova(mod_sexuality_int3_alt, mod_sexuality_int2, test="F")# allowed




mod_sexuality_int3_bin<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="binomial")
summary(mod_sexuality_int3_bin)
AIC(mod_sexuality_int3_bin)#44.09






#rm  corolla_leaves_mean_scaled:reproductive_pathway 

mod_sexuality_int4<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean  + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_int4)#sign.
anova(mod_sexuality_int3, mod_sexuality_int4, test="F")# allowed






mod_sexuality_int4_bin<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + corolla_leaves_mean_scaled:reproductive_pathway + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  reproductive_pathway + reproductive_pathway:genetic_diversity_2_scaled + habitat_simple  + habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="binomial")
summary(mod_sexuality_int4_bin)
AIC(mod_sexuality_int3_bin)#45.98






install.packages("biomod2")
library(biomod2)


response.plot(mod_sexuality_int4)



#install.packages("devtools")
library(devtools)


#devtools::install_github("cardiomoon/ggiraphExtra")
library(ggiraphExtra)



ggPredict(mod_sexuality_int4,colorAsFactor = TRUE,interactive=TRUE)

mod_sexuality_int4_gen<-lapply(mod_sexuality_int4, `[[`, 1)


#### plots predicted


plot(dat_summary_without_3n$sexuality_2~dat_summary_without_3n$genetic_diversity_2_scaled)


range(dat_summary_without_3n$genetic_diversity_2_scaled, na.rm=TRUE)

xweight <- seq(-2, 5, 0.001)

yweight <- predict(mod_sexuality_int4 , list(genetic_diversity_2_scaled = xweight),type="response")

lines(xweight, yweight)







library(ggplot2)
ggplot(dat_summary_without_3n, aes(x=genetic_diversity_2_scaled, y=sexuality_2)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="quasibinomial"), se=FALSE)

par(mar = c(4, 4, 1, 1)) # Reduce some of the margins so that the plot fits better
plot(dat_summary_without_3n$sexuality_2~dat_summary_without_3n$genetic_diversity_2_scaled)
curve(predict(mod_sexuality_int4, data.frame(genetic_diversity_2_scaled=x), type="response"), add=TRUE) 

























mod_sexuality_int4$coefficients


# save the coefficient values so we can use them in the equations
b0 <- mod_sexuality_int4$coefficients[1] # intercept
genetic_diversity_est <- mod_sexuality_int4$coefficients[2]


genetic_diversity_range <- seq(from=min(dat_summary_without_3n$genetic_diversity_2_scaled, na.rm=TRUE), to=max(dat_summary_without_3n$genetic_diversity_2_scaled, na.rm=TRUE), by=.01)


a_logits <- b0 + 
  dat_summary_without_3n$genetic_diversity*genetic_diversity_range



# Compute the probibilities (this is what will actually get plotted):
a_probs <- exp(a_logits)/(1 + exp(a_logits))





plot(genetic_diversity_range, a_probs,ylim=c(0,1))



#https://blogs.uoregon.edu/rclub/2016/04/05/plotting-your-logistic-regression-models/




#ploidy levels and habitat simple

table(dat_summary$ploidy_embryo_simple, dat_summary$habitat_simple)

chisq.test(dat_summary$ploidy_embryo_simple, dat_summary$habitat_simple)#no


barplot(table(dat_summary$habitat_simple, dat_summary$ploidy_embryo_simple))








#corolla leaves and treatment 



boxplot(dat_summary$corolla_leaves_mean)

boxplot(dat_summary$corolla_leaves_mean_2018)


wilcox.test(dat_summary$corolla_leaves_mean, dat_summary$corolla_leaves_mean_2018)

mean(dat_summary$corolla_leaves_mean)
mean(dat_summary$corolla_leaves_mean_2018)


#sexuality apomicts and treatment 

#subsets apomicts

dat_summary_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway_simple=="apomictic")


boxplot(dat_summary_apomicts$sexuality_2 ~ dat_summary_apomicts$seed_development_in_situ)

kruskal.test(dat_summary_apomicts$sexuality_2 ~ dat_summary_apomicts$seed_development_in_situ)

pairwise.wilcox.test(dat_summary_apomicts$sexuality_2, dat_summary_apomicts$seed_development_in_situ)

tapply(dat_summary_apomicts$sexuality_2, dat_summary_apomicts$seed_development_in_situ, mean, na.rm=TRUE)

tapply(dat_summary_apomicts$sexuality_2, dat_summary_apomicts$seed_development_in_situ, median, na.rm=TRUE)




#subset facultative sexuals

dat_summary_fac_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway=="3_apomictic_facultative_sexual")


boxplot(dat_summary_fac_apomicts$sexuality_2 ~ dat_summary_fac_apomicts$seed_development_in_situ_simple)

dat_summary_fac_apomicts_y<-subset(dat_summary_fac_apomicts, seed_development_in_situ_simple=="y")
#dat_summary_fac_apomicts_y_n<-subset(dat_summary_fac_apomicts, seed_development_in_situ=="y/n")
dat_summary_fac_apomicts_n<-subset(dat_summary_fac_apomicts, seed_development_in_situ_simple=="n")

shapiro.test(dat_summary_fac_apomicts_y$sexuality_2)#no

shapiro.test(dat_summary_fac_apomicts_n$sexuality_2)#no

#shapiro.test(dat_summary_fac_apomicts_n$sexuality_2)#no



wilcox.test(dat_summary_fac_apomicts$sexuality_2 ~ dat_summary_fac_apomicts$seed_development_in_situ_simple)#non-sign.

tapply(dat_summary_fac_apomicts$sexuality_2, dat_summary_fac_apomicts$seed_development_in_situ_simple, mean, na.rm=TRUE)



#total

boxplot(dat_summary$sexuality_2 ~ dat_summary$seed_development_in_situ)

kruskal.test(dat_summary$sexuality_2 ~ dat_summary$seed_development_in_situ)#sign

pairwise.wilcox.test(dat_summary$sexuality_2, dat_summary$seed_development_in_situ) #maybe higher sexuality in later sampled Cetral European samples with higher degree of facultative sexuality


tapply(dat_summary$sexuality_2, dat_summary$seed_development_in_situ, mean, na.rm=TRUE)# in "no" treatment all 100% sexuals!!!







#GLMER facult apom

dat_summary_fac_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway=="3_apomictic_facultative_sexual")


# with interactions, SCALED, without 3N!!!!


mod_sexuality_int_fac<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:ploidy_embryo_simple, data = dat_summary_fac_apomicts, family="quasibinomial")
summary(mod_sexuality_int_fac)#sign.


#install.packages("MuMIn")
#library(MuMIn)




mod_sexuality_int_fac<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  habitat_simple + habitat_simple:genetic_diversity_2_scaled + habitat_simple:ploidy_embryo_simple, data = dat_summary_fac_apomicts, family="binomial")
summary(mod_sexuality_int_fac)#AIC=27.03






#rm genetic_diversity_2_scaled:habitat_simpleopen_habitats

mod_sexuality_int_fac_2<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  habitat_simple + habitat_simple:ploidy_embryo_simple, data = dat_summary_fac_apomicts, family="quasibinomial")
summary(mod_sexuality_int_fac_2)#sign.

anova(mod_sexuality_int_fac, mod_sexuality_int_fac_2, test="F")# allowed



#rm genetic_diversity_2_scaled:habitat_simpleopen_habitats

mod_sexuality_int_fac_2<-glm(sexuality_2 ~ genetic_diversity_2_scaled + corolla_leaves_mean_scaled + genetic_diversity_2_scaled:corolla_leaves_mean + ploidy_embryo_simple + genetic_diversity_2_scaled:ploidy_embryo_simple +  habitat_simple + habitat_simple:ploidy_embryo_simple, data = dat_summary_fac_apomicts, family="quasibinomial")
summary(mod_sexuality_int_fac_2)#sign.

anova(mod_sexuality_int_fac, mod_sexuality_int_fac_2, test="F")# allowed



####GLMER total seperate models####


#install.packages("MuMIn")
#library(MuMIn)
#install.packages("lme4")
library(lme4)


# with interactions, SCALED, without 3N!!!!



mod_sexuality_1 <- glm(sexuality_2 ~ genetic_diversity_2, data = dat_summary_without_3n, family="quasibinomial")#sign. (overdispersed)
summary(mod_sexuality_1)

plot(sexuality_2 ~ genetic_diversity_2, data = dat_summary_without_3n)
                  



mod_sexuality_2 <- glm(sexuality_2 ~ corolla_leaves_mean, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_2)
plot(sexuality_2 ~ corolla_leaves_mean, data = dat_summary_without_3n)                       
                       
                       
                       
#mod_sexuality_3 <- glm(sexuality_2 ~ genetic_diversity_2_scaled:corolla_leaves_mean_scaled, data = dat_summary_without_3n, family="quasibinomial")
#summary(mod_sexuality_3)# sign 
                

       
                       
mod_sexuality_4 <- glm(sexuality_2 ~ corolla_leaves_mean:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_4)#sign.
                       

lsmeans(mod_sexuality_4, pairwise ~ corolla_leaves_mean:reproductive_pathway)




mod_sexuality_4c <- glm(sexuality_2 ~corolla_leaves_mean:reproductive_pathway_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_4c)#sign.

lsmeans(mod_sexuality_4c, pairwise ~ corolla_leaves_mean:reproductive_pathway_simple)

                       
                       
                       
mod_sexuality_5 <- glm(sexuality_2 ~ ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_5)#sign.


lsmeans(mod_sexuality_5, pairwise ~ ploidy_embryo_simple)

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple, mean, na.rm=TRUE)

kruskal.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple)#sign

pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple)







dat_summary_without_3n$ploidy_embryo_simple_2 <- factor(dat_summary_without_3n$ploidy_embryo_simple, ordered = TRUE, levels = c("4N", "6N", "2N"))

mod_sexuality_5b <- glm(sexuality_2 ~ ploidy_embryo_simple_2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_5b)#sign.

                       





mod_sexuality_6 <- glm(sexuality_2 ~ ploidy_embryo_simple:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_6)#collinearites





lsmeans(mod_sexuality_6, pairwise ~ ploidy_embryo_simple:reproductive_pathway)

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway, mean, na.rm=TRUE)



kruskal.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway)#sign.

pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple_reproductive_pathway)





#without obligate apomicts (0 sexuality)

dat_summary_without_3n_without_oblig<-subset(dat_summary_without_3n, reproductive_pathway_simple_without_obligates>0)  

mod_sexuality_6a <- glm(sexuality_2 ~ ploidy_embryo_simple:reproductive_pathway, data = dat_summary_without_3n_without_oblig, family="quasibinomial")#sign.
summary(mod_sexuality_6a)#collinearites


dat_summary_without_3n_without_oblig$reproductive_pathway <- factor(dat_summary_without_3n_without_oblig$reproductive_pathway, ordered = TRUE, levels = c("3_apomictic_facultative_sexual", "1_sexual"))




#dat_summary_without_3n_sex<-subset(dat_summary_without_3n, reproductive_pathway=="1_sexual")    
#dat_summary_without_3n_apom<-subset(dat_summary_without_3n, reproductive_pathway=="2_apomictic_obligate_asexual")                     
#dat_summary_without_3n_fac_sex<-subset(dat_summary_without_3n, reproductive_pathway=="3_apomictic_facultative_sexual")                       


#mod_sexuality_6a <- glm(sexuality_2 ~ ploidy_embryo_simple, data = dat_summary_without_3n_sex, family="quasibinomial")#sign.
#summary(mod_sexuality_6a)#collinearites




mod_sexuality_7 <- glm(sexuality_2 ~ genetic_diversity_2:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_7)
                       



lsmeans(mod_sexuality_7, pairwise ~ genetic_diversity_2*ploidy_embryo_simple)

#tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$ploidy_embryo_simple, mean, na.rm=TRUE)




                       
mod_sexuality_8 <- glm(sexuality_2 ~ reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_8)



lsmeans(mod_sexuality_8, pairwise ~ reproductive_pathway)

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$reproductive_pathway, mean, na.rm=TRUE)



kruskal.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$reproductive_pathway)#sign.

pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$reproductive_pathway)



                       
dat_summary_without_3n$reproductive_pathway_2 <- factor(dat_summary_without_3n$reproductive_pathway, ordered = TRUE, levels = c("2_apomictic_obligate_asexual", "3_apomictic_facultative_sexual", "1_sexual"))

str(dat_summary_without_3n$reproductive_pathway_2)

mod_sexuality_8c <- glm(sexuality_2 ~ reproductive_pathway_ordered, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_8c)




dat_summary_without_3n$reproductive_pathway_simple2 <- factor(dat_summary_without_3n$reproductive_pathway_simple, ordered = TRUE, levels = c("sexual", "apomictic"))


mod_sexuality_8a <- glm(sexuality_2 ~ dat_summary_without_3n$reproductive_pathway_simple2, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_8a)

lsmeans(mod_sexuality_8a, pairwise ~ reproductive_pathway_simple2)

wilcox.test(dat_summary_without_3n$sexuality_2 ~ dat_summary_without_3n$reproductive_pathway_simple2)


tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$reproductive_pathway_simple2, mean, na.rm=TRUE)


                       



                       
mod_sexuality_9 <- glm(sexuality_2 ~ reproductive_pathway:genetic_diversity_2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_9)


lsmeans(mod_sexuality_9, pairwise ~ reproductive_pathway:genetic_diversity_2)

                       


mod_sexuality_9b <- glm(sexuality_2 ~ reproductive_pathway_simple:genetic_diversity_2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_9b)

lsmeans(mod_sexuality_9b, pairwise ~ reproductive_pathway_simple:genetic_diversity_2)                       
                       





mod_sexuality_10 <- glm(sexuality_2 ~ habitat_simple, data = dat_summary_without_3n, family="quasibinomial")#non-sign.
summary(mod_sexuality_10)
                       
lsmeans(mod_sexuality_10, pairwise ~ habitat_simple)                      

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple, mean, na.rm=TRUE)



mod_sexuality_11 <- glm(sexuality_2 ~ habitat_simple:genetic_diversity_2, data = dat_summary_without_3n, family="quasibinomial")#sign.
summary(mod_sexuality_11)


lsmeans(mod_sexuality_11, pairwise ~ habitat_simple:genetic_diversity_2) 






                        
                        
mod_sexuality_12 <- glm(sexuality_2 ~ habitat_simple:reproductive_pathway_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12) 


lsmeans(mod_sexuality_12, pairwise ~ habitat_simple:reproductive_pathway_simple) 











mod_sexuality_12b <- glm(sexuality_2 ~ habitat_simple:reproductive_pathway_simple_ordered, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12b) 
                        

mod_sexuality_12a <- glm(sexuality_2 ~ habitat_simple:reproductive_pathway, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12a)


lsmeans(mod_sexuality_12a, pairwise ~ habitat_simple:reproductive_pathway) 











dat_summary_without_3n_open_habitats <- subset(dat_summary_without_3n, habitat_simple=="open_habitats")
dat_summary_without_3n_forest <- subset(dat_summary_without_3n, habitat_simple=="forest")


# open habitats
tapply(dat_summary_without_3n_open_habitats$sexuality_2, dat_summary_without_3n_open_habitats$reproductive_pathway, mean, na.rm=TRUE)


#forest
tapply(dat_summary_without_3n_forest$sexuality_2, dat_summary_without_3n_forest$reproductive_pathway, mean, na.rm=TRUE)


kruskal.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_reproductive_pathway)#sign.


pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_reproductive_pathway)






dat_summary_without_3n$reproductive_pathway_ordered <- as.factor(dat_summary_without_3n$reproductive_pathway_ordered)
mod_sexuality_12d <- glm(sexuality_2 ~ habitat_simple:reproductive_pathway_ordered, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12d)



# or in this way

dat_summary_without_3n$reproductive_pathway2 <- factor(dat_summary_without_3n$reproductive_pathway, ordered = TRUE, levels = c("3_apomictic_facultative_sexual", "2_apomictic_obligate_asexual", "1_sexual"))

mod_sexuality_12e <- glm(sexuality_2 ~ habitat_simple:reproductive_pathway2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_12e)





                        
mod_sexuality_13 <- glm(sexuality_2 ~  habitat_simple:ploidy_embryo_simple, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_13)



kruskal.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple)#sign.


pairwise.wilcox.test(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, p.adjust.method = "fdr")

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, mean, na.rm=TRUE)

tapply(dat_summary_without_3n$sexuality_2, dat_summary_without_3n$habitat_simple_ploidy_embryo_simple, FUN = function(x) length(x[!is.na(x)]))

boxplot(dat_summary_without_3n$sexuality_2~dat_summary_without_3n$habitat_simple_ploidy_embryo_simple)








dat_summary_without_3n_open_habitats <- subset(dat_summary_without_3n, habitat_simple=="open_habitats")
dat_summary_without_3n_forest <- subset(dat_summary_without_3n, habitat_simple=="forest")


# open habitats
tapply(dat_summary_without_3n_open_habitats$sexuality_2, dat_summary_without_3n_open_habitats$ploidy_embryo_simple, mean, na.rm=TRUE)


#forest
tapply(dat_summary_without_3n_forest$sexuality_2, dat_summary_without_3n_forest$ploidy_embryo_simple, mean, na.rm=TRUE)







dat_summary_without_3n$ploidy_embryo_simple2 <- factor(dat_summary_without_3n$ploidy_embryo_simple, ordered = TRUE, levels = c("4N", "6N", "2N"))

mod_sexuality_13b <- glm(sexuality_2 ~  habitat_simple:ploidy_embryo_simple2, data = dat_summary_without_3n, family="quasibinomial")
summary(mod_sexuality_13b)





####mapping of genetic diversity and sexuality ####



# read data

#install.packages("openxlsx")
library(openxlsx)

dat_summary<-read.xlsx("00_final_masterfile_final.xlsx", sheet="locations_FC_FCSS_meassurements")


str(dat_summary)



dat_summary$pop_ID<-as.factor(dat_summary$pop_ID)
dat_summary$taxon<-as.factor(dat_summary$taxon)
dat_summary$DNA_ploidy<-as.factor(dat_summary$DNA_ploidy)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FC)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FCSS)
dat_summary$no_seeds<-as.factor(dat_summary$no_seeds)
dat_summary$ploidy_embryo<-as.factor(dat_summary$ploidy_embryo)
dat_summary$ploidy_embryo_simple<-as.factor(dat_summary$ploidy_embryo_simple)
dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)
dat_summary$reproductive_pathway_simple<-as.factor(dat_summary$reproductive_pathway_simple)
dat_summary$habitat<-as.factor(dat_summary$habitat)
dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)
dat_summary$seed_development_in_situ<-as.factor(dat_summary$seed_development_in_situ)


str(dat_summary)





dat_summary$gps_wgs84_decimal_n<-as.numeric(dat_summary$gps_wgs84_decimal_n)
dat_summary$gps_wgs84_decimal_e<-as.numeric(dat_summary$gps_wgs84_decimal_e)

str(dat_summary)



library(ggplot2)
#install.packages("ggmap")
library(ggmap)       

#install.packages("viridis")
library(viridis)

#install.packages("dplyr")
library(dplyr)


#bubble map


register_google(key = "AIzaSyDVCJN-4VcXwldkuWfxZP72oOwHMkzLrdI")

europe_map2 <- get_map(location = c(lon = 15.2551, lat = 54.5260), maptype = "satellite", zoom = 4)
ggmap(europe_map2)

#install.packages("maps")
library(maps)


#install.packages("mapproj")
library(mapproj)


# Get map data
#USA <- map_data("Austria")

worldMap <- map_data("world", region = ".", exact = FALSE)

# Select only some countries and add values
europe <- data.frame("country"=c("Austria", "Belgium", "Germany", "Spain", "Finland", "France","Greece", "Ireland", "Italy", "Netherlands", "Portugal","Bulgaria","Croatia","Cyprus", "Czech Republic","Denmark","Estonia", "Hungary","Latvia", "Lithuania","Luxembourg","Malta", "Poland", "Romania","Slovakia","Slovenia","Sweden","UK", "Switzerland","Ukraine", "Turkey", "Macedonia", "Norway", "Slovakia", "Serbia", "Montenegro","Moldova", "Kosovo", "Georgia", "Bosnia and Herzegovina", "Belarus","Armenia", "Albania"))



europe_map <- worldMap %>%
  filter(region %in% europe$country)


#data <- dat_summary[, c(1, 25,26,28)]

#simple

ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data = data, aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e)) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map()




#ggmap(europe_map) + 
 # theme_void() + 
  #ggtitle("satellite") +   geom_point( data = data, aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e)) +
 # theme_void() + coord_map()

mybreaks <- c(0, 10, 30, 50, 70, 90, 100)



dat_summary %>%
ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=sexuality_map, color=dat_summary$sexuality), shape=20, stroke=FALSE) + scale_size_continuous(name="Sexuality of Population (%)", trans="log", range=c(1,100), breaks=mybreaks) + scale_alpha_continuous(name="Sexuality of Population (%)", trans="log", range=c(0.1, 1), breaks=mybreaks) + scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (%)" ) + theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() + theme(legend.position="none")



#reordering (command range and mutate out if not)

dat_summary %>%
  arrange(sexuality_map) %>% 
  mutate(name=factor(pop_ID, unique(pop_ID))) %>% 
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=sexuality_map, color=sexuality_map), alpha=0.9) +
  scale_size_continuous(breaks = mybreaks) +
  scale_color_viridis() +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() + theme(legend.position="none")


#genetic diversity
range(dat_summary$genetic_diversity_2, na.rm=TRUE)
hist(dat_summary$genetic_diversity_2)

mybreaks_gen <- c(0.5, 1, 1.5, 2.0, 2.5, 3.0)

dat_summary %>%
  arrange(genetic_diversity_2) %>% 
  mutate(name=factor(pop_ID, unique(pop_ID))) %>% 
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=genetic_diversity_2, color=genetic_diversity_2), alpha=0.9) +
  scale_size_continuous(breaks=mybreaks_gen) +
  scale_color_viridis() +  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() + theme(legend.position="none")

hist(dat_summary$genetic_diversity_2, breaks = mybreaks_gen)

scale_colour_gradient2(genetic_diversity_2, low = "blue", mid = "green",  high = "orange",space = "Lab", na.value = "grey50", guide = "colourbar",aesthetics = "colour")






#### customized map

# Create breaks for the color scale

mybreaks <- c(0, 10, 30, 50, 70 , 90 , 100)

# Reorder data to show biggest cities on top
dat_summary <- dat_summary %>%
  arrange(sexuality_map) %>%
  mutate( name=factor(pop_ID, unique(pop_ID))) %>%
  mutate(sexuality_map) #=sexuality_map/1000000

# Build the map
map %>%
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=sexuality_map, color=sexuality_map), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Sexuality of Population (%)", trans="log", range=c(1,100), breaks=mybreaks) +
  scale_alpha_continuous(name="Sexuality of Population (%)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (%)" ) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")))














data %>%
  arrange(sexuality_map) %>% 
  mutate( name=factor(pop_ID, unique(pop_ID))) %>% 
  ggmap(europe_map) + 
  theme_void() + 
  ggtitle("satellite") +
  geom_point( aes(data = data, aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e), color=sexuality_map), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void()  + coord_map() + theme(legend.position="none")




ggmap(europe_map) + 
  theme_void() + 
  ggtitle("satellite") +   geom_point(aes(data = data, aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e), color = data$sexuality_map), alpha=0.9) +
  theme_void() + coord_map()



#only sexuals and facultative sexuals

dat_summary_sex<-subset(dat_summary, select_sex==1)


ggmap(europe_map, extent = "device") + geom_point(aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n), colour = "red", alpha = 0.3, size = 5, data = dat_summary_sex)
       


europe_map_g_str <- get_map(location = c(lon = 15.2551, lat = 54.5260), maptype = "satellite",zoom = 4)
       

ggmap(europe_map_g_str, extent = "device") + geom_density2d(data = dat_summary_sex, aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n), size = 1) + stat_density2d(data = dat_summary_sex,aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n, fill = ..level.., alpha = ..level..), size = 0.001,bins = 200, geom = "polygon") + scale_fill_gradient(low = "yellow", high = "orangered") + scale_alpha(range = c(0, 1), guide = FALSE)
       


#asex



#dat_seeds_asex<-read.csv("dat_seeds_asex.csv", header=TRUE, sep=";", dec=",")

#dat_seeds_asex$gps_wgs84_decimal_n<-as.numeric(dat_seeds_asex$gps_wgs84_decimal_n)
#dat_seeds_asex$gps_wgs84_decimal_e<-as.numeric(dat_seeds_asex$gps_wgs84_decimal_e)

#str(dat_seeds_asex)
library(ggmap)

register_google(key = "AIzaSyDVCJN-4VcXwldkuWfxZP72oOwHMkzLrdI")

europe_map <- get_map(location = c(lon = 15.2551, lat = 54.5260), maptype = "satellite", zoom = 4)


#ggmap(europe_map, extent = "device") + geom_point(aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n), colour = "red", alpha = 0.3, size = 5, data = dat_seeds_asex)



#europe_map_g_str <- get_map(location = c(lon = 15.2551, lat = 54.5260), maptype = "satellite",zoom = 4)


#ggmap(europe_map_g_str, extent = "device") + geom_density2d(data = dat_seeds_asex, aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n), size = 0.3) + stat_density2d(data = dat_seeds_asex,aes(x = gps_wgs84_decimal_e, y = gps_wgs84_decimal_n, fill = ..level.., alpha = ..level..), size = 0.01,bins = 300, geom = "polygon") + scale_fill_gradient(low = "cadetblue2", high = "darkblue") + scale_alpha(range = c(0, 1), guide = FALSE)



#### assessment of environmental factors ####

#https://rspatial.org/raster/sdm/4_sdm_envdata.html

#install.packages(c('raster', 'rgdal', 'dismo', 'rJava'))

library(raster)
library(rgdal)
library(dismo)
library(rJava)




# read data

#install.packages("openxlsx")
library(openxlsx)

dat_summary<-read.xlsx("00_final_masterfile_final.xlsx", sheet="locations_FC_FCSS_meassurements")


str(dat_summary)



dat_summary$pop_ID<-as.factor(dat_summary$pop_ID)
dat_summary$taxon<-as.factor(dat_summary$taxon)
dat_summary$DNA_ploidy<-as.factor(dat_summary$DNA_ploidy)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FC)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FCSS)
dat_summary$no_seeds<-as.factor(dat_summary$no_seeds)
dat_summary$ploidy_embryo<-as.factor(dat_summary$ploidy_embryo)
dat_summary$ploidy_embryo_simple<-as.factor(dat_summary$ploidy_embryo_simple)
dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)
dat_summary$reproductive_pathway_simple<-as.factor(dat_summary$reproductive_pathway_simple)
dat_summary$habitat<-as.factor(dat_summary$habitat)
dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)
dat_summary$seed_development_in_situ<-as.factor(dat_summary$seed_development_in_situ)


str(dat_summary)





dat_summary$gps_wgs84_decimal_n<-as.numeric(dat_summary$gps_wgs84_decimal_n)
dat_summary$gps_wgs84_decimal_e<-as.numeric(dat_summary$gps_wgs84_decimal_e)

str(dat_summary)




#load environmental variables

library(dismo)
#files <- list.files("F:/01_PhD_Göttingen/data_evaluation/02_fc_fcss/data_analysis/climatic_variables/", pattern='tif$', full.names=TRUE)
#files

#path <- file.path(system.file(package="dismo"), 'ex')
#files <- list.files(path, pattern='grd$', full.names=TRUE )
#files





library(raster)
#bio = getData('worldclim', var='bio', res=0.5, lon=-10, lat=60)


####bioclim


clim <- getData('worldclim', var='bio', res=2.5)# worldwide, 2.5 = 5km resolution!

#clim <- getData('worldclim', var='bio', res=0.5, lon=5, lat=45)# worldwide, 0.5 = 1km resolution would be better!!!


bio_eu <- raster::crop(clim,c(-10,55,30,75)) # cut europe

#par(mfrow=c(1,2))

plot(bio_eu)


#### altitude

alt <- getData('worldclim', var='alt', res=2.5)# worldwide, 2.5 = 5km resolution!

alt_eu <- raster::crop(alt,c(-10,55,30,75))


plot(alt_eu)


#### solar radiation

#average layers

setwd("F:/01_PhD_Göttingen/data_evaluation/02_fc_fcss/data_analysis/maxEnt/world_layer/")

setwd("/Users/kevin/Desktop/world_layer/")

f <- list.files(getwd()) 
ras <- lapply(f,raster) 
STACK1 <- stack(ras)

mean <- calc(STACK1, fun = mean)



rad_eu <- raster::crop(mean,c(-10,55,30,75))

#install.packages("SDMTools")
library(SDMTools)



par(mfrow=c(1,2))

rad_eu <- raster("/Users/kevin/Desktop/world_layer/rad_eu.asc")

plot(rad_eu)



rad_bio3 <- raster("/Users/kevin/Desktop/world_layer/bio_eu_3.asc")

plot(rad_bio3)




rad_bio4 <- raster("/Users/kevin/Desktop/world_layer/bio_eu_4.asc")

plot(rad_bio4)

#?getData



####cut parameters

#GDALinfo("F:/01_PhD_Göttingen/data_evaluation/02_fc_fcss/data_analysis/maxEnt/wc2.0_2.5m_bio/bio1.tif")

#getData("worldclim", download=FALSE, path="F:/01_PhD_Göttingen/data_evaluation/02_fc_fcss/data_analysis/maxEnt/bio", res=2.5, var="bio")



#bio1<-raster("F:/01_PhD_Göttingen/data_evaluation/02_fc_fcss/data_analysis/maxEnt/wc2.0_2.5m_bio/bio1.tif")


#bio1 <- raster::crop(bio1,c(-10,55,30,75)) # cut europe

#plot(bio1, 15)


writeRaster(bio_eu, filename="bio_eu", format="ascii", overwrite=TRUE, bylayer=TRUE)



#writeRaster(alt_eu, filename="alt_eu", format="ascii", overwrite=TRUE, bylayer=TRUE)

writeRaster(rad_eu, filename="rad_eu", format="ascii", overwrite=TRUE, bylayer=TRUE)



#temp1 <- raster(clim,c(-10,55,30,75))



#predictors <- stack(files)
#predictors
#names(predictors)
#plot(predictors)

# we do not need the first column


dat_mapping_all<-read.csv("dat_seeds_three_columns.csv", dec=",", sep=";")

str(dat_mapping_all)

dat_mapping_all  <- dat_mapping_all[,-1]

# first layer of the RasterStack
plot(bio_eu, 1)

#plot(clim, 1)

#data(wrld_simpl)


# with the points function, "add" is implicit
points(dat_mapping_all, col='blue')#ok!







#### extracting values from rasters
presvals_all <- extract(bio_eu, dat_mapping_all)


write.csv(presvals_all, file = "presvals_all_added.csv")


#alt_eu

presvals_sex_alt_eu <- extract(alt_eu, dat_mapping_all)


write.csv(presvals_sex_alt_eu, file = "presvals_alt_eu_added.csv")


#rad_eu

presvals_all_rad_eu <- extract(rad_eu, dat_mapping_all)


write.csv(presvals_all_rad_eu, file = "presvals_all_rad_eu_added.csv")




#STANDARDISIERUNG IN LIBREOFFICE










##### check for correlated predictors


#install.packages("corrplot")
library(corrplot)

dat_summary_envals<-dat_summary[, 28:50]

cors<-cor(dat_summary_envals,use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations

#rm bio 6 (correlated to bio 11), bio 5 (correlated to bio 10), bio 14 (correlated to bio17), bio13 (correlated to bio16)

dat_summary_envals<-dat_summary[, c(28:31, 34:39, 42:50)]

cors<-cor(dat_summary_envals,use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations


#rm bio 16 (correlated to bio 12), gps_wgs84_e (correlated to bio4), gps_wgs84_e (correlated to solar rad)




dat_summary_envals<-dat_summary[, c(28:31, 34:39, 42, 44:47, 50)]

cors<-cor(dat_summary_envals,use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations


#rm bio7 (correlated to bio4), bio11 (correlated to bio1), bio19 (correlated to bio17)




dat_summary_envals<-dat_summary[, c(28:31, 35:37, 39, 42, 44:45, 47, 50)]

cors<-cor(dat_summary_envals,use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations


# rm bio17 (correlated to bio12), bio10 (correlated to bio1)


dat_summary_envals<-dat_summary[, c(28:31, 35:36, 39, 42, 45, 47, 50)]

cors<-cor(dat_summary_envals,use='complete.obs') # evaluate correlations
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6) # plot correlations


#all removed r>0.8!!!





#### modelling sexuality across EUROPE ####

#### review ####


dat_summary$gps_wgs84_decimal_n<-as.numeric(dat_summary$gps_wgs84_decimal_n)
dat_summary$gps_wgs84_decimal_e<-as.numeric(dat_summary$gps_wgs84_decimal_e)

str(dat_summary)

#### all ####

model_a <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary)

summary(model_a)



#### all (interactions) ####

model_a_int <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + I(bio3^2) + I(bio4^2) + I(bio4^2):solar_rad + I(bio4^2):altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int)

#rm altitude_standard:bio12

model_a_int_2 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + I(bio3^2) + I(bio4^2) + I(bio4^2):solar_rad + I(bio4^2):altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_2)


# rm altitude_standard:I(bio4^2)

model_a_int_3 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + I(bio3^2) + I(bio4^2) + I(bio4^2):solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_3)


#rm solar_rad:altitude_standard

model_a_int_4 <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio1:solar_rad + bio1:altitude_standard + I(bio3^2) + I(bio4^2) + I(bio4^2):solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_4)


#rm I(bio3^2)

model_a_int_5 <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio1:solar_rad + bio1:altitude_standard + I(bio4^2) + I(bio4^2):solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_5)


#rm bio18

model_a_int_6 <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio1:solar_rad + bio1:altitude_standard + I(bio4^2) + I(bio4^2):solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_6)



#rm altitude_standard:bio15

model_a_int_7 <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio1:solar_rad + bio1:altitude_standard + I(bio4^2) + I(bio4^2):solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_7)







#### final model old without quadratic terms

model_a_int_13 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)

summary(model_a_int_13)





#### relationships plots

mod_bio3<- glm(sexuality_2 ~ bio3, family=binomial, data = dat_summary)
summary(mod_bio3)

plot(sexuality_2 ~ bio3, data = dat_summary)

plot(sexuality_2 ~ bio1, data = dat_summary)
plot(sexuality_2 ~ bio2, data = dat_summary)
plot(sexuality_2 ~ bio3, data = dat_summary)# rather quadratic

mod_bio3_quadratic <- glm(sexuality_2 ~ I(bio3^2), family=binomial, data = dat_summary)# AIC lower (better)

xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_bio3_quadratic, list(bio3 = xweight),type="response")

lines(xweight, yweight, col="red")



plot(sexuality_2 ~ bio4, data = dat_summary)# rather quadratic
plot(sexuality_2 ~ bio8, data = dat_summary)
plot(sexuality_2 ~ bio12, data = dat_summary)
plot(sexuality_2 ~ bio15, data = dat_summary)
plot(sexuality_2 ~ bio18, data = dat_summary)
plot(sexuality_2 ~ solar_rad, data = dat_summary)#rather linear
plot(sexuality_2 ~ altitude_standard, data = dat_summary)



sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard

xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_bio3, list(bio3 = xweight),type="response")

lines(xweight, yweight, col="red")


text(2500,0.042, "rSP=0.186***" )



### why has bio3 negative estimate

model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n ))#
summary(model)

#without ploidy and habitat
model <- psem(glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n ))#
summary(model)



#### sexuality_2 bio3

bio3_factor <- as.factor(dat_summary$bio3)
hist(table(bio3_factor))

hist(table(bio3_factor), freq=TRUE, xlab = levels(bio3_factor), ylab = "Frequencies")

bio3<-c(-1.85, -1.60, -1.35, -1.10, -0.85, -0.60, -0.34, -0.09, 0.16, 0.41, 0.66, 0.91, 1.17, 1.42, 1.67, 1.92, 2.17, 2.42)
bio3<-as.factor(bio3)

sex<-c(2, 10, 26, 33, 7, 6, 16, 10, 16, 41, 36, 26, 4, 2, 6, 2, 3, 5)

plot(sex~bio3)

hist(sex, xlab = levels(bio3), ylab = "Frequencies")


tapply()

mod <- glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n)

predict(mod, data.frame(bio3=c(-3,3)), type="response")

new.my.model <- expand.grid(Origin=c("Ka","La"), Time=c("mor","eve"))

predict(mod)
xweight <- seq(-3, 3, 0.1)
yweight <- predict(mod, list(bio3 = xweight),type="response")


plot(sexuality_2 ~ bio3, data = dat_summary)

mod_bio3<- glm(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3, family=binomial, data = dat_summary)

summary(mod_bio3)


xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_bio3, list(bio3 = xweight),type="response")

lines(xweight, yweight, col="red")







#### sexuality_2 solar rad
plot(sexuality_2 ~ solar_rad, data = dat_summary)

mod_solar_rad<- glm(sexuality_2 ~ solar_rad, family=binomial, data = dat_summary)

summary(mod_solar_rad)


xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_solar_rad, list(solar_rad = xweight),type="response")

lines(xweight, yweight, col="red")




#summary(psem(glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary)))



#### reduced (without_3n) ####
model_b <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_without_3n)

summary(model_b)



#### fac. apomicts ####

dat_summary_fac_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway=="3_apomictic_facultative_sexual")

model_a_fac <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac)


#### fac. apomicts int ####

model_a_fac_int <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int)


# rm solar_rad:bio12

model_a_fac_int_2 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_2)


# rm altitude_standard:bio15

model_a_fac_int_3 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:altitude_standard + bio15 + bio15:solar_rad + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_3)


# rm solar_rad:bio15

model_a_fac_int_4 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:altitude_standard + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_4)


# rm bio2

model_a_fac_int_5 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard +  bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:altitude_standard + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_5)




# rm altitude_standard:bio12

model_a_fac_int_6 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard +  bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_6)



# rm altitude_standard:bio4 

model_a_fac_int_7 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard +  bio3 + bio4 + bio4:solar_rad + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_7)



# rm bio3 

model_a_fac_int_8 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio4 + bio4:solar_rad + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)

summary(model_a_fac_int_8)


summary(psem(glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio4 + bio4:solar_rad + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts)))

plot(sexuality_2 ~ bio1:solar_rad, data = dat_summary_fac_apomicts)
plot(sexuality_2 ~ bio1, data = dat_summary_fac_apomicts)
plot(sexuality_2 ~ solar_rad:altitude_standard, data = dat_summary_fac_apomicts)

plot(sexuality_2 ~ bio12, data = dat_summary_fac_apomicts)
plot(sexuality_2 ~ bio15, data = dat_summary_fac_apomicts)
plot(sexuality_2 ~ bio18, data = dat_summary_fac_apomicts)


##### exclude both sexuality value outliers around 0.35 (10391, KK166) #####


dat_summary_fac_apomicts_red <- dat_summary_fac_apomicts[-c(15,35), ]

model_a_fac_int_red <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red)


# rm solar_rad:altitude_standard

model_a_fac_int_red_2 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_2)


# rm altitude_standard:bio12

model_a_fac_int_red_3 <- glmmPQL(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_3)


# rm bio1:solar_rad

model_a_fac_int_red_4 <- glmmPQL(sexuality_2 ~ bio1 + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_4)


# rm bio1:altitude_standard

model_a_fac_int_red_5 <- glmmPQL(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_5)


# rm bio1

model_a_fac_int_red_6 <- glmmPQL(sexuality_2 ~ bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_6)



# rm bio18

model_a_fac_int_red_7 <- glmmPQL(sexuality_2 ~ bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_7)


# rm bio8

model_a_fac_int_red_8 <- glmmPQL(sexuality_2 ~ bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_8)


#rm solar_rad:bio15

model_a_fac_int_red_9 <- glmmPQL(sexuality_2 ~ bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_9)


# rm bio2 

model_a_fac_int_red_10 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_10)



# rm bio4:altitude_standard

model_a_fac_int_red_11 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_11)



# rm bio15:altitude_standard

model_a_fac_int_red_12 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + bio15 + solar_rad + altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_12)




# rm altitude_standard

model_a_fac_int_red_13 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + bio15 + solar_rad, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_13)


# rm bio15

model_a_fac_int_red_14 <- glmmPQL(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_14)


# rm bio3

model_a_fac_int_red_15 <- glmmPQL(sexuality_2 ~ bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group, family=binomial, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_15)



model_a_fac_int_red_15_gaus <- glmmPQL(sexuality_2 ~ bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group, family=gaussian, data = dat_summary_fac_apomicts_red)

summary(model_a_fac_int_red_15_gaus)

summary(psem(glmmPQL(sexuality_2 ~ bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group, family=gaussian, data = dat_summary_fac_apomicts_red)))






shapiro.test(log(dat_summary_fac_apomicts_red$sexuality)) #non-normal
qqnorm(dat_summary_fac_apomicts_red$sexuality)
qqline(dat_summary_fac_apomicts_red$sexuality)


#### gen div int (log)####

dat_summary$genetic_diversity_2_log <- log(dat_summary$genetic_diversity_2)

dat_summary$genetic_diversity_2_log <- dat_summary$genetic_diversity_2_log + 10

model_a_gen_int <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int)

# rm altitude_standard:bio4

model_a_gen_int_2 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_2)

# rm bio3

model_a_gen_int_3 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_3)

# rm solar_rad:altitude_standard

model_a_gen_int_4 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_4)


# rm altitude_standard:bio15

model_a_gen_int_5 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio18 + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_5)


# rm bio18

model_a_gen_int_6 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_6)


# rm altitude_standard:bio12

model_a_gen_int_7 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_7)


# rm bio1:solar_rad

model_a_gen_int_8 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_8)


# rm bio1:altitude_standard

model_a_gen_int_9 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad + altitude_standard, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_9)


# rm altitude_standard 

model_a_gen_int_10 <- glmmPQL(genetic_diversity_2_log ~ bio1 + bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_10)



# rm bio1

model_a_gen_int_11 <- glmmPQL(genetic_diversity_2_log ~ bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + bio15:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_11)


# rm solar_rad:bio15

model_a_gen_int_12 <- glmmPQL(genetic_diversity_2_log ~ bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + bio15 + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_12)



# rm bio15

model_a_gen_int_13 <- glmmPQL(genetic_diversity_2_log ~ bio2 + bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_13)



# rm bio2

model_a_gen_int_14 <- glmmPQL(genetic_diversity_2_log ~ bio4 + bio4:solar_rad + bio8 + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_14)


# rm bio8

model_a_gen_int_15 <- glmmPQL(genetic_diversity_2_log ~ bio4 + bio4:solar_rad  + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)

summary(model_a_gen_int_15)


summary(psem(glmmPQL(genetic_diversity_2_log ~ bio4 + bio4:solar_rad  + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary)))



#plot(genetic_diversity_2_log ~ bio1, data = dat_summary)
#plot(genetic_diversity_2_log ~ bio2, data = dat_summary)
plot(genetic_diversity_2_log ~ bio3, data = dat_summary)# rather linear
plot(genetic_diversity_2_log ~ bio4, data = dat_summary)# rather linear
#plot(genetic_diversity_2_log ~ bio8, data = dat_summary)
plot(genetic_diversity_2_log ~ bio12, data = dat_summary)# super linear
summary(lm(genetic_diversity_2_log ~ bio12, data = dat_summary))
#plot(genetic_diversity_2_log ~ bio15, data = dat_summary)
#plot(genetic_diversity_2_log ~ bio18, data = dat_summary)
plot(genetic_diversity_2_log ~ solar_rad, data = dat_summary)# linear
#plot(genetic_diversity_2_log ~ altitude_standard, data = dat_summary)

# line bio4

plot(genetic_diversity_2_log_w10 ~ bio4, data = dat_summary)

mod_bio4 <- lm(genetic_diversity_2_log_w10 ~ bio4, data = dat_summary)

summary(mod_bio4)


xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_bio4, list(bio4 = xweight),type="response")

lines(xweight, yweight, col="red")


# curve bio4

dat_summary$genetic_diversity_2_log_w10 <- log(dat_summary$genetic_diversity_2)


plot(genetic_diversity_2 ~ bio4, data = dat_summary)

mod_bio4 <- lm(genetic_diversity_2_log_w10 ~ bio4, data = dat_summary)

summary(mod_bio4)

exp(0.11174)#intercept
curve(1.118222*exp(0.07526*x), add=T, col="red")



xweight <- seq(-3, 3, 0.1)

yweight <- predict(mod_bio4, list(bio4 = xweight),type="response")

lines(xweight, yweight, col="red")



#### path analysis (total model) ####

### exclude rows with missing data in sexuality_2, ploidy_embryo_simple, habitat_simple, reproductive_pathway, genetic_diversity_2_prop, corolla_leaves_max and exclude triploids

dat_summary_without_3n<-dat_summary[-c(1,2,3,5,87,104,112,166,205,214,215,233, 47, 64, 102, 103, 108, 110, 229, 230, 233, 218, 219, 215, 214, 206, 194, 172, 170, 167, 162, 123, 2, 3, 95, 89, 87, 79, 72, 64, 57, 53, 48, 39, 35, 31, 19, 18, 15, 12, 1, 107, 109, 242),]


boxplot(dat_summary_without_3n$genetic_diversity_2 ~ dat_summary_without_3n$ploidy_embryo_simple)


### without "quasi" (makes only difference in R2 calculations) --> likelihood estimations can handle under- and overdispersion
#glmmPQL

### log genetic_diversity_2_prop (see below)

dat_summary_without_3n$genetic_diversity_2_log <- log(dat_summary_without_3n$genetic_diversity_2)

dat_summary_without_3n$genetic_diversity_2_log <- dat_summary_without_3n$genetic_diversity_2_log + 10

#due to gaussian model, arcsin transformation of sexuality

qqnorm(dat_summary_without_3n$sexuality_2)
qqline(dat_summary_without_3n$sexuality_2)

shapiro.test(dat_summary_without_3n$sexuality_2)

library(base)

shapiro.test(asin(dat_summary_without_3n$sexuality_2))

#why R2 of genetic div response so low?? y standardization??
#?rsquared
#rsquared(model)
#rsquared(model, method = "delta")
#rsquared(model, method = "lognormal")
#rsquared(model, method = "trigamma")

#model <- psem(glmmPQL(genetic_diversity_2_prop ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway, random = ~1|genetic_group, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

#summary(model, test.statistic = "F", test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#

#the issue is the low range of genetic_diversity_2_prop (widely below 0.1), for binomial 0-1 would be optimal (like sexuality)

#shapiro.test(log(dat_summary_without_3n$genetic_diversity_2_prop))#yes
#qqnorm(dat_summary_without_3n$genetic_diversity_2_prop)
#qqline(dat_summary_without_3n$genetic_diversity_2_prop)
#hist(dat_summary_without_3n$genetic_diversity_2_prop)

# model with all climatic env. factors


#model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=binomial, data = dat_summary_without_3n), glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group, family=poisson, data = dat_summary_without_3n),  glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

#summary(model, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#


model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n), glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group_snmf, family=poisson, data = dat_summary_without_3n),  glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4 + bio4:solar_rad  + bio12 + bio12:solar_rad + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

summary(model, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#
coefs(model, standardize = "scale")

# error: 1: In B * (sd.x/sd.y) : Länge des längeren Objektes ist kein Vielfaches der Länge des kürzeren Objektes



#mod_x <- glmer(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3 + bio4 + bio4:solar_rad + bio12 + solar_rad + altitude_standard + solar_rad:altitude_standard + (1|genetic_group_snmf), family=binomial, nAGQ=0, data = dat_summary_without_3n)
#mod_x <- c(mod_x,list(maxit=200))

#summary(mod_x)

#devfun <- do.call(mkGlmerDevfun, mod_x)




####not due to interactions!
model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3 + bio4 +  bio12 + solar_rad + altitude_standard , random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n), glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group_snmf, family=poisson, data = dat_summary_without_3n),  glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4 +  + bio12 + solar_rad, random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))# indep claims --> many significances (relationships have to be removed because make no bioogical sense), goodness of fit --> highly significant

summary(model, standardize.type = "none", standardize="none")

summary(model, test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#

coefs(model, test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#

##### not in seperate glmms!
dat_summary_without_3n$bio4_pos <- dat_summary_without_3n$bio4+10
dat_summary_without_3n$bio3_pos <- dat_summary_without_3n$bio3+10
dat_summary_without_3n$solar_rad_pos <- dat_summary_without_3n$solar_rad+10
dat_summary_without_3n$bio12_pos <- dat_summary_without_3n$bio12+10
dat_summary_without_3n$altitude_standard_pos <- dat_summary_without_3n$altitude_standard+10



model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3_pos + bio4_pos + bio4_pos:solar_rad_pos + bio12_pos + solar_rad_pos + altitude_standard_pos + solar_rad_pos:altitude_standard_pos, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n ))#
summary(model)

model <- psem(glmmPQL(corolla_leaves_max ~ sexuality_2, random = ~1|genetic_group_snmf, family=poisson, data = dat_summary_without_3n))#
summary(model)


#non-standardized bios

model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple + bio3_r + bio4_r + bio4_r:solar_rad_r + bio12_r + solar_rad_r + altitude_standard_r + solar_rad_r:altitude_standard_r, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n ))#

summary(model, test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")#

model <- psem(glmmPQL(sexuality_2 ~ ploidy_embryo_simple + habitat_simple,random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n))
summary(model)
            



model <- psem(glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4 + bio4:solar_rad  + bio12 + bio12:solar_rad + solar_rad , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#

summary(model)


# without interactions


model <- psem(glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4  + bio12 +  solar_rad , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#
summary(model)

model <- psem(glmmPQL(genetic_diversity_2_log ~  bio4  + bio12 +  solar_rad , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#
summary(model)

model <- psem(glmmPQL(sexuality_2 ~  bio1 + bio2, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary_without_3n))#
summary(model)

coefs(model, test.type="II", intercepts = FALSE, standardize.type = "latent.linear", standardize="range")#

### --> variable removal solves the problem!
# rm bio4

model <- psem(glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway   + bio12 +  solar_rad , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#
summary(model)


# rm bio12

model <- psem(glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4  +  solar_rad , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#
summary(model)

# rm solar rad
model <- psem(glmmPQL(genetic_diversity_2_log ~ sexuality_2 + ploidy_embryo_simple + habitat_simple + reproductive_pathway + bio4  + bio12 , random = ~1|genetic_group_snmf, family=gaussian, data = dat_summary_without_3n))#
summary(model)

#not due to missing data!





### psem changes estimates of the previos model (+ --> -)


### specify claims as correlated errors (which make no biological sense)

basisSet(model, direction=NULL)

model2 <- update(model, habitat_simple %~~% corolla_leaves_max)
summary(model2, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")




model3 <- update(model2, corolla_leaves_max %~~% genetic_diversity_2_log)
summary(model3,  intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")


model4 <- update(model3, reproductive_pathway %~~% corolla_leaves_max)
summary(model4, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



#model5 <- update(model4, corolla_leaves_max %~~% genetic_diversity_2_log)
#(model5, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale")



model5 <- update(model4, corolla_leaves_max %~~% ploidy_embryo_simple)
summary(model5, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")


model6 <- update(model5, corolla_leaves_max %~~% bio3)
summary(model6, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")


model7 <- update(model6, corolla_leaves_max %~~% bio4)
summary(model7, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")

model8 <- update(model7, corolla_leaves_max %~~% bio12)
summary(model8, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")


model9 <- update(model8, genetic_diversity_2_log %~~% bio3)
summary(model9, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")

model10 <- update(model9, corolla_leaves_max %~~% solar_rad)
summary(model10, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")

model11 <- update(model10, corolla_leaves_max %~~% altitude_standard)
summary(model11, intercepts = FALSE, standardize.type = "latent.linear", standardize="scale", test.type="II")

model12 <- update(model11, genetic_diversity_2_log %~~% altitude_standard)
summary(model12, intercepts = TRUE, standardize.type = "Menard.OE", standardize="range", test.type="II")

plot(model12)


#bio3

mod_bio3 <- glmmPQL(sexuality_2 ~ bio3, random = ~1|genetic_group_snmf, family=binomial, data = dat_summary)
summary(mod_bio3)

plot(dat_summary_without_3n$sexuality_2 ~ dat_summary_without_3n$bio3)
# save the coefficient values so we can use them in the equations
b0 <- 0 # intercept, 4.109
X1 <- -1.059

X1_range <- seq(from=min(dat_summary_without_3n$bio3), to=max(dat_summary_without_3n$bio3), by=.001)


a_logits <- b0 + 
  X1*X1_range 

# Compute the probibilities (this is what will actually get plotted):
a_probs <- exp(a_logits)/(1 + exp(a_logits))

plot(X1_range, a_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="bio3", ylab="sexuality", main="")


#solar rad


plot(dat_summary_without_3n$sexuality_2 ~ dat_summary_without_3n$solar_rad)
# save the coefficient values so we can use them in the equations
b0 <- 0 # intercept, 4.109
X1 <- 1.2414

X1_range <- seq(from=min(dat_summary_without_3n$solar_rad), to=max(dat_summary_without_3n$solar_rad), by=.001)


a_logits <- b0 + 
  X1*X1_range 

# Compute the probibilities (this is what will actually get plotted):
a_probs <- exp(a_logits)/(1 + exp(a_logits))

plot(X1_range, a_probs, 
     ylim=c(0,1),
     type="l", 
     lwd=3, 
     lty=2, 
     col="gold", 
     xlab="solar_rad", ylab="sexuality", main="")



#heterozygosity
plot(dat_summary_without_3n$genetic_diversity_2_log ~ dat_summary_without_3n$bio4)
# save the coefficient values so we can use them in the equations
b0 <- 0 # intercept, 9.6715
X1 <- 0.0815

curve(0.0815*x + 9.6715, add=T, col="red")




# read data

#install.packages("openxlsx")
library(openxlsx)

dat_summary<-read.xlsx("00_final_masterfile_final.xlsx", sheet="locations_FC_FCSS_meassurements")


str(dat_summary)



dat_summary$pop_ID<-as.factor(dat_summary$pop_ID)
dat_summary$taxon<-as.factor(dat_summary$taxon)
dat_summary$DNA_ploidy<-as.factor(dat_summary$DNA_ploidy)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FC)
dat_summary$no_individuals_FC<-as.factor(dat_summary$no_individuals_FCSS)
dat_summary$no_seeds<-as.factor(dat_summary$no_seeds)
dat_summary$ploidy_embryo<-as.factor(dat_summary$ploidy_embryo)
dat_summary$ploidy_embryo_simple<-as.factor(dat_summary$ploidy_embryo_simple)
dat_summary$reproductive_pathway<-as.factor(dat_summary$reproductive_pathway)
dat_summary$reproductive_pathway_simple<-as.factor(dat_summary$reproductive_pathway_simple)
dat_summary$habitat<-as.factor(dat_summary$habitat)
dat_summary$habitat_simple<-as.factor(dat_summary$habitat_simple)
dat_summary$seed_development_in_situ<-as.factor(dat_summary$seed_development_in_situ)


str(dat_summary)





dat_summary$gps_wgs84_decimal_n<-as.numeric(dat_summary$gps_wgs84_decimal_n)
dat_summary$gps_wgs84_decimal_e<-as.numeric(dat_summary$gps_wgs84_decimal_e)

str(dat_summary)


#### complete model####

mod<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard, "quasibinomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod)#underdispersion!! --> quasibinomial


mod_bin<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard , "binomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod_bin)

AIC(mod_bin)#85.20




#rm bio8


mod2<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio12 + bio15 + bio18 + solar_rad + altitude_standard , "quasibinomial", data=dat_summary)
summary(mod2)

anova(mod, mod2, test="F")#allowed

#rm altitude


mod3<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio12 + bio15 + bio18 + solar_rad , "quasibinomial", data=dat_summary)
summary(mod3)

anova(mod3, mod2, test="F")#allowed


#rm bio 18

mod4<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio12 + bio15 + solar_rad , "quasibinomial", data=dat_summary)
summary(mod4)

anova(mod4, mod3, test="F")#allowed



#rm bio3

mod5<-glm(sexuality_2 ~ bio1 + bio2 + bio4 + bio12 + bio15 + solar_rad , "quasibinomial", data=dat_summary)
summary(mod5)

anova(mod4, mod5, test="F")#allowed


#rm bio2

mod6<-glm(sexuality_2 ~ bio1 + bio4 + bio12 + bio15 + solar_rad , "quasibinomial", data=dat_summary)
summary(mod6)

anova(mod6, mod5, test="F")#allowed




#rm bio15

mod7<-glm(sexuality_2 ~ bio1 + bio4 + bio12 + solar_rad, "quasibinomial", data=dat_summary)
summary(mod7)


mod7_bin<-glm(sexuality_2 ~ bio1 + bio4 + bio12 + solar_rad , "binomial", data=dat_summary)
summary(mod7_bin)

AIC(mod7_bin)#78.31




anova(mod7, mod6, test="F")#allowed


#install.packages("ncf")
library(ncf)
#??ncf




mod_7_cor1<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0,  latlon=T, na.rm=T, resamp = 1000)

#mod_7_cor2<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0.01,  latlon=T, na.rm=T, resamp = 1000)

#mod_7_cor3<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0.1,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor4<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=1,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor5<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=2,  latlon=T, na.rm=T, resamp = 1000)
plot(mod6)


mod_7_cor6<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=5,  latlon=T, na.rm=T, resamp = 1000)


mod_7_cor7<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=100,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor8<-correlog(x=dat_summary$gps_wgs84_decimal_e, y=dat_summary$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=1000,  latlon=T, na.rm=T, resamp = 1000)




#residuals(mod7)
write.csv(mod_7_cor1$p, "mod_7_cor1.csv")
write.csv(mod_7_cor4$p, "mod_7_cor4.csv")
write.csv(mod_7_cor5$p, "mod_7_cor5.csv")
write.csv(mod_7_cor6$p, "mod_7_cor6.csv")
write.csv(mod_7_cor7$p, "mod_7_cor7.csv")
write.csv(mod_7_cor8$p, "mod_7_cor8.csv")




length(mod_7_cor1$p)#0/2 significant
length(mod_7_cor4$p)# 206/2265 significant
length(mod_7_cor5$p)#109/1183 significant
length(mod_7_cor6$p)#48/504 significant
length(mod_7_cor7$p)#5/29 significant
length(mod_7_cor8$p)#0/4 significant


206*100/2265#9.09
109*100/1183#9.21
48*100/504#9.52
5*100/29#17.24
0*100/4#0

x <-c(9.09, 9.21, 9.52, 17.24, 0)
mean(x)#9.01

par(mfrow=c(4,2))

#resample=1,

plot(mod_7_cor1)
plot(mod_7_cor4)#few outliers below -1 and above 1
plot(mod_7_cor5)#few outliers below -1 and above 1
plot(mod_7_cor6)#few outliers below -1 and above 1
plot(mod_7_cor7)
plot(mod_7_cor8)


#few signals of spatial autocorrelation (can be ignored)






#### complete model with interactions ####

mod<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod)#underdispersion!! --> quasibinomial


mod_bin<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "binomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod_bin)

AIC(mod_bin)#93.42




#rm bio12:altitude_standard


mod2<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod2)

anova(mod, mod2, test="F")#allowed



#rm bio4:altitude_standard


mod3<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod3)

anova(mod3, mod2, test="F")#allowed




#rm bio1:solar_rad


mod4<-glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod4)

anova(mod4, mod3, test="F")#allowed


#rm bio2


mod5<-glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod5)

anova(mod4, mod5, test="F")#allowed





#rm bio18


mod6<-glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad + bio15:altitude_standard + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod6)

anova(mod6, mod5, test="F")#allowed





#rm altitude_standard:bio15


mod7<-glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + bio15:solar_rad  + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod7)

anova(mod6, mod7, test="F")#allowed



#rm solar_rad:bio15


mod8<-glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod8)

anova(mod8, mod7, test="F")#allowed



#rm bio1:altitude_standard


mod9<-glm(sexuality_2 ~ bio1 + bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod9)

anova(mod8, mod9, test="F")#allowed



#rm bio1


mod10<-glm(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio12:solar_rad  + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod10)

anova(mod10, mod9, test="F")#allowed






#rm solar_rad:bio12


mod11<-glm(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad  + bio8 + bio12 + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod11)

anova(mod10, mod11, test="F")#allowed





#rm bio8


mod12<-glm(sexuality_2 ~ bio3 + bio4 + bio4:solar_rad  + bio12 + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod12)

anova(mod12, mod11, test="F")#allowed


plot(sexuality_2~altitude_standard, data=dat_summary)





####GLMER total seperate models####



mod<-glm(sexuality_2 ~ bio1, "quasibinomial", data=dat_summary)
summary(mod)#sign.

mod_2<-glm(sexuality_2 ~ bio1:solar_rad, "quasibinomial", data=dat_summary)
summary(mod_2)#marg sig.

mod_3<-glm(sexuality_2 ~ bio1:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_3)#sign.

mod_4<-glm(sexuality_2 ~ bio2, "quasibinomial", data=dat_summary)
summary(mod_4)#sign.



mod_5<-glm(sexuality_2 ~ bio3, "quasibinomial", data=dat_summary)
summary(mod_5)#sign


mod_6<-glm(sexuality_2 ~ bio4, "quasibinomial", data=dat_summary)
summary(mod_6)#non.sign.


mod_7<-glm(sexuality_2 ~ bio4:solar_rad, "quasibinomial", data=dat_summary)
summary(mod_7)#non-sign.


mod_8<-glm(sexuality_2 ~ bio4:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_8)#sign.


mod_9<-glm(sexuality_2 ~ bio8, "quasibinomial", data=dat_summary)
summary(mod_9)#sign.


mod_10<-glm(sexuality_2 ~ bio12, "quasibinomial", data=dat_summary)
summary(mod_10)#sign.

mod_11<-glm(sexuality_2 ~ bio12:solar_rad, "quasibinomial", data=dat_summary)
summary(mod_11)#sign.

mod_13<-glm(sexuality_2 ~ bio12:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_13)#sign.

mod_14<-glm(sexuality_2 ~ bio15, "quasibinomial", data=dat_summary)
summary(mod_14) #marg. sig.

mod_15<-glm(sexuality_2 ~ bio15:solar_rad, "quasibinomial", data=dat_summary)
summary(mod_15)#sign.


mod_16<-glm(sexuality_2 ~ bio15:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_16)#sign.


mod_17<-glm(sexuality_2 ~ bio18, "quasibinomial", data=dat_summary)
summary(mod_17)#sign.


mod_18<-glm(sexuality_2 ~ solar_rad, "quasibinomial", data=dat_summary)
summary(mod_18)#sign.


mod_19<-glm(sexuality_2 ~ altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_19)#sign.


mod_20<-glm(sexuality_2 ~ solar_rad:altitude_standard, "quasibinomial", data=dat_summary)
summary(mod_20)#sign.





#exclude bio1 : solar radiation, bio4, bio4 : solar radiation, bio15 (precipitation seasonality)



#install.packages("relaimpo")
library(relaimpo)

?relaimpo


pAsin <- asin(sqrt(dat_summary$sexuality/100))

qqnorm(dat_summary$sexuality)
qqline(dat_summary$sexuality)

qqnorm(pAsin)
qqline(pAsin)

plot(dat_summary$sexuality, pAsin, type='l', lwd=2, col='blue', las=1, xlab='p', ylab='arcsine(p)')


mod_total <- glm(sexuality_2 ~ bio1 + bio1:altitude_standard + bio2 + bio3 + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, family="quasibinomial", data=dat_summary)
summary(mod_total)



mod_total <- lm(pAsin ~ bio1 + bio1:altitude_standard + bio2 + bio3 + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, data=dat_summary)
summary(mod_total)


res <- calc.relimp(mod_total, type = "lmg")







#### Bonferoni Corrections
Input = ("
        variable                    p_value
        bio1                        0.0135
        bio1:solar_rad              0.0857
        bio1:altitude_standard      0.0284
        bio2                        2.18e-05
        bio3                        0.00293
        bio4                        0.652
        bio4:solar_rad              0.373
        bio4:altitude_standard      0.0156
        bio8                        8.44e-08
        bio12                       2e-16
        bio12:solar_rad             1.79e-08
        bio12:altitude_standard     0.00565
        bio15                       0.0809
        bio15:solar_rad             0.00338
        bio15:altitude_standard     0.0288
        bio18                       6.28e-11
        solar_rad                   3.84e-14
        altitude_standard           3.34e-07
        solar_rad:altitude_standard 0.000369
        ")


Data = read.table(textConnection(Input),header=TRUE)



Data$bonferroni =
  p.adjust(Data$p_value,
           method = "bonferroni")


Data$fdr =
  p.adjust(Data$p_value,
           method = "fdr")












#### facultative apomicts ####

dat_summary_fac_apomicts<-subset(dat_summary, dat_summary$reproductive_pathway=="3_apomictic_facultative_sexual")

mod<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard , "quasibinomial", data=dat_summary_fac_apomicts)#man könnte GLMER draus machen
summary(mod)


mod_bin<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + bio18 + solar_rad + altitude_standard , "binomial", data=dat_summary_fac_apomicts)
summary(mod_bin)

AIC(mod_bin)#32.59




#rm bio18

mod2<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + bio15 + solar_rad + altitude_standard , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod2)

anova(mod, mod2, test="F")#allowed


#rm bio15

mod3<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + bio12 + solar_rad + altitude_standard , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod3)

anova(mod3, mod2, test="F")#allowed




#rm bio12

mod4<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + solar_rad + altitude_standard , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod4)

anova(mod4, mod3, test="F")#allowed



#rm altitude standard

mod5<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + bio8 + solar_rad  , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod5)

anova(mod5, mod4, test="F")#allowed



#rm bio8

mod6<-glm(sexuality_2 ~ bio1 + bio2 + bio3 + bio4 + solar_rad  , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod6)

anova(mod6, mod5, test="F")#allowed


#rm bio1

mod7<-glm(sexuality_2 ~ bio2 + bio3 + bio4 + solar_rad  , "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod7)

anova(mod7, mod6, test="F")#allowed





mod7_bin<-glm(sexuality_2 ~ bio2 + bio3 + bio4 + solar_rad  , "binomial", data=dat_summary_fac_apomicts)
summary(mod7_bin)#AIC=20.57




#### facultative apomicts with interaction #####


mod<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)#man könnte GLMER draus machen
summary(mod)#underdispersion!! --> quasibinomial


mod_bin<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "binomial", data=dat_summary_fac_apomicts)#man könnte GLMER draus machen
summary(mod_bin)

AIC(mod_bin)#50.65



#rm bio1:altitude_standard

mod2<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod2)

anova(mod, mod2, test="F")




#rm altitude_standard:bio15

mod3<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad  + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod3)

anova(mod3, mod2, test="F")




#rm solar_rad:bio15

mod4<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15   + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod4)

anova(mod3, mod4, test="F")


#rm bio3

mod5<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15   + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod5)

anova(mod5, mod4, test="F")


#rm altitude_standard:bio12 

mod6<-glm(sexuality_2 ~ bio1 + bio1:solar_rad + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad  + bio15   + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod6)

anova(mod5, mod6, test="F")




#rm bio1:solar_rad

mod7<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad  + bio15   + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod7)

anova(mod7, mod6, test="F")



#rm solar_rad:bio12

mod8<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12  + bio15   + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod8)

anova(mod7, mod8, test="F")


#rm bio15

mod9<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod9)

anova(mod9, mod8, test="F")



#rm bio12

mod10<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod10)

anova(mod9, mod10, test="F")



#rm bio8

mod11<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod11)

anova(mod11, mod10, test="F")




#rm bio18

mod12<-glm(sexuality_2 ~ bio1 + bio2  + bio4 + bio4:solar_rad + bio4:altitude_standard + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)

summary(mod12)

anova(mod11, mod12, test="F")



#model fac sex apomicts without interactions






#install.packages("ncf")
library(ncf)
#??ncf




mod_7_cor1<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0,  latlon=T, na.rm=T, resamp = 1000)

#mod_7_cor2<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0.09,  latlon=T, na.rm=T, resamp = 1000)

#mod_7_cor3<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=0.1,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor4<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=1,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor5<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=2,  latlon=T, na.rm=T, resamp = 1000)



mod_7_cor6<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=5,  latlon=T, na.rm=T, resamp = 1000)




mod_7_cor7<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=100,  latlon=T, na.rm=T, resamp = 1000)

mod_7_cor8<-correlog(x=dat_summary_fac_apomicts$gps_wgs84_decimal_e, y=dat_summary_fac_apomicts$gps_wgs84_decimal_n, z=as.numeric(residuals(mod7)), increment=1000,  latlon=T, na.rm=T, resamp = 1000)




#residuals(mod7)
write.csv(mod_7_cor1$p, "mod_7_cor1_fac_sex.csv")
write.csv(mod_7_cor4$p, "mod_7_cor4_fac_sex.csv")
write.csv(mod_7_cor5$p, "mod_7_cor5_fac_sex.csv")
write.csv(mod_7_cor6$p, "mod_7_cor6_fac_sex.csv")
write.csv(mod_7_cor7$p, "mod_7_cor7_fac_sex.csv")
write.csv(mod_7_cor8$p, "mod_7_cor8_fac_sex.csv")




#length(mod_7_cor1$p)#1/1 significant
length(mod_7_cor4$p)#104/1100 significant
length(mod_7_cor5$p)#79/739 significant
length(mod_7_cor6$p)#35/363 significant
length(mod_7_cor7$p)#3/22 significant
length(mod_7_cor8$p)#0/3 significant

#0*100/2 #0
104*100/1100#9.45
79*100/739#10.69
35*100/363#9.64
3*100/22#13.64
0*100/4#0

x <-c(9.45, 10.69, 9.64, 13.64, 0)
mean(x)#8.68

par(mfrow=c(4,2))

#resample=1,

plot(mod_7_cor1)
plot(mod_7_cor4)#some outliers below -1 and above 1
plot(mod_7_cor5)#few outliers below -1 and above 1
plot(mod_7_cor6)#few outliers below -1 and above 1
plot(mod_7_cor7)
plot(mod_7_cor8)


#few signals of spatial autocorrelation (can be ignored)





####GLMER total seperate models####


mod<-glm(sexuality_2 ~ bio1, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod)#non-sign.

mod_2<-glm(sexuality_2 ~ bio1:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_2)#sig.

mod_3<-glm(sexuality_2 ~ bio1:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_3)#sign.

mod_4<-glm(sexuality_2 ~ bio2, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_4)#sign.



mod_5<-glm(sexuality_2 ~ bio3, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_5)#non-sign


mod_6<-glm(sexuality_2 ~ bio4, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_6)#sign.


mod_7<-glm(sexuality_2 ~ bio4:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_7)#sign.


mod_8<-glm(sexuality_2 ~ bio4:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_8)#sign.


mod_9<-glm(sexuality_2 ~ bio8, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_9)#marg. sign.


mod_10<-glm(sexuality_2 ~ bio12, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_10)#non-sign.

mod_11<-glm(sexuality_2 ~ bio12:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_11)#sign.

mod_13<-glm(sexuality_2 ~ bio12:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_13)#sign.

mod_14<-glm(sexuality_2 ~ bio15, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_14) #sig.

mod_15<-glm(sexuality_2 ~ bio15:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_15)#non-sign.


mod_16<-glm(sexuality_2 ~ bio15:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_16)#non-sign.


mod_17<-glm(sexuality_2 ~ bio18, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_17)#non-sign.


mod_18<-glm(sexuality_2 ~ solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_18)#sign.


mod_19<-glm(sexuality_2 ~ altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_19)#sign.


mod_20<-glm(sexuality_2 ~ solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_20)#sign.




#exclude bio1, bio3, bio8, bio12, bio15:solar_radiation, bio15:altitude, bio18



install.packages("relaimpo")
library(relaimpo)

?relaimpo



pAsin2 <- asin(sqrt(dat_summary_fac_apomicts$sexuality/100))
summary(pAsin2)
str(pAsin2)

qqnorm(dat_summary_fac_apomicts$sexuality)
qqline(dat_summary_fac_apomicts$sexuality)

qqnorm(pAsin2)
qqline(pAsin2)

hist(pAsin2)



mod_total_fac_sex <-glm(sexuality_2 ~ bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12:solar_rad + bio12:altitude_standard + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, data=dat_summary_fac_apomicts, family="quasibinomial")
summary(mod_total_fac_sex)



mod_total_fac_sex <- lm(pAsin2 ~ bio1:solar_rad + bio1:altitude_standard + bio2 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12:solar_rad + bio12:altitude_standard + bio15 + solar_rad + altitude_standard + solar_rad:altitude_standard, data=dat_summary_fac_apomicts)
summary(mod_total_fac_sex)


res2 <- calc.relimp(mod_total_fac_sex, type = "lmg")






#### Bonferoni Corrections
Input = ("
         variable                    p_value
         bio1                        0.989
         bio1:solar_rad              0.00469
         bio1:altitude_standard      0.00139
         bio2                        0.0109
         bio3                        0.424
         bio4                        1.37e-06
         bio4:solar_rad              0.00127
         bio4:altitude_standard      0.00162
         bio8                        0.0632
         bio12                       0.267
         bio12:solar_rad             0.000202
         bio12:altitude_standard     0.00571
         bio15                       0.000313
         bio15:solar_rad             0.447
         bio15:altitude_standard     0.584
         bio18                       0.653
         solar_rad                   0.0104
         altitude_standard           0.0281
         solar_rad:altitude_standard 0.00971
         ")


Data = read.table(textConnection(Input),header=TRUE)



Data$bonferroni =
  p.adjust(Data$p_value,
           method = "bonferroni")



Data$fdr =
  p.adjust(Data$p_value,
           method = "fdr")







#### modelling genetic diversity across EUROPE ####




mod<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod)#underdispersion!! --> quasibinomial


mod_bin<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:solar_rad + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "binomial", data=dat_summary)#man könnte GLMER draus machen
summary(mod_bin)

AIC(mod_bin)#168.82



#rm solar_rad:bio15

mod2<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio8 + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod2)

anova(mod, mod2, test="F")




#rm bio8

mod2<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio12:altitude_standard + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod2)

anova(mod, mod2, test="F")



#rm altitude_standard:bio12

mod3<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio1:altitude_standard + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod3)

anova(mod3, mod2, test="F")





#rm bio1:altitude_standard

mod4<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio4:solar_rad + bio4:altitude_standard + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod4)

anova(mod3, mod4, test="F")






#rm bio4:altitude_standard

mod5<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio4:solar_rad + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod5)

anova(mod5, mod4, test="F")



#rm solar_rad:bio4 

mod6<-glm(genetic_diversity ~ bio1 + bio1:solar_rad + bio2 + bio3 + bio4 + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod6)

anova(mod5, mod6, test="F")



#rm solar_rad:bio1 

mod7a<-glm(genetic_diversity ~ bio1 + bio2 + bio3 + bio4 + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod7a)

anova(mod7a, mod6, test="F")




#rm bio4

mod8a<-glm(genetic_diversity ~ bio1 + bio2 + bio3 + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod8a)

anova(mod7a, mod8a, test="F")






#rm bio3

mod9a<-glm(genetic_diversity ~ bio1 + bio2 + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod9a)

anova(mod9a, mod8a, test="F")



#rm bio2

mod10a<-glm(genetic_diversity ~ bio1 + bio12 + bio12:solar_rad + bio15 + bio15:altitude_standard + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod10a)

anova(mod9a, mod10a, test="F")




#rm bio15:altitude_standard

mod11a<-glm(genetic_diversity ~ bio1 + bio12 + bio12:solar_rad + bio15 + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod11a)

anova(mod11a, mod10a, test="F")




#rm bio15

mod12a<-glm(genetic_diversity ~ bio1 + bio12 + bio12:solar_rad + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod12a)

anova(mod11a, mod12a, test="F")



#rm bio1

mod13a<-glm(genetic_diversity ~ bio12 + bio12:solar_rad + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "quasibinomial", data=dat_summary)

summary(mod13a)

anova(mod13a, mod12a, test="F")


mod13a_bin<-glm(genetic_diversity ~ bio12 + bio12:solar_rad + bio18 + solar_rad + altitude_standard + solar_rad:altitude_standard, "binomial", data=dat_summary)

summary(mod13a_bin)#AIC=143.32









####GLMER total seperate models####


mod<-glm(genetic_diversity_2_prop ~ bio1, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod)#non-sign.

mod_2<-glm(genetic_diversity_2_prop ~ bio1:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_2)#non-sig.

mod_3<-glm(genetic_diversity_2_prop ~ bio1:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_3)#non-sign.

mod_4<-glm(genetic_diversity_2_prop ~ bio2, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_4)#non-sign.



mod_5<-glm(genetic_diversity_2_prop ~ bio3, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_5)#non-sign


mod_6<-glm(genetic_diversity_2_prop ~ bio4, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_6)#non-sign.


mod_7<-glm(genetic_diversity_2_prop ~ bio4:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_7)#non-sign.


mod_8<-glm(genetic_diversity_2_prop ~ bio4:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_8)#non-sign.


mod_9<-glm(genetic_diversity_2_prop ~ bio8, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_9)#non sign.


mod_10<-glm(genetic_diversity_2_prop ~ bio12, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_10)#non-sign.

mod_11<-glm(genetic_diversity_2_prop ~ bio12:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_11)#non-sign.

mod_13<-glm(genetic_diversity_2_prop ~ bio12:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_13)#non-sign.

mod_14<-glm(genetic_diversity_2_prop ~ bio15, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_14) #sig.

mod_15<-glm(genetic_diversity_2_prop ~ bio15:solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_15)#sign.


mod_16<-glm(genetic_diversity_2_prop ~ bio15:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_16)#non-sign.


mod_17<-glm(genetic_diversity_2_prop ~ bio18, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_17)#non-sign.


mod_18<-glm(genetic_diversity_2_prop ~ solar_rad, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_18)#non-sign.


mod_19<-glm(genetic_diversity_2_prop ~ altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_19)#non-sign.


mod_20<-glm(genetic_diversity_2_prop ~ solar_rad:altitude_standard, "quasibinomial", data=dat_summary_fac_apomicts)
summary(mod_20)#non-sign.






#### Bonferoni Corrections

Input = ("
          variable                    p_value
          bio1                        0.518
          bio1:solar_rad              0.815
          bio1:altitude_standard      0.886
          bio2                        0.693
          bio3                        0.65
          bio4                        0.344
          bio4:solar_rad              0.137
          bio4:altitude_standard      0.553
          bio8                        0.791
          bio12                       0.704
          bio12:solar_rad             0.35
          bio12:altitude_standard     0.803
          bio15                       0.0461
          bio15:solar_rad             0.0392
          bio15:altitude_standard     0.977
          bio18                       0.423
          solar_rad                   0.734
          altitude_standard           0.639
          solar_rad:altitude_standard 0.48
          ")


Data = read.table(textConnection(Input),header=TRUE)




Data$bonferroni =
  p.adjust(Data$p_value,
           method = "bonferroni")


Data$fdr =
  p.adjust(Data$p_value,
           method = "fdr")





####barplot relative importance####





# read data

#install.packages("openxlsx")
library(openxlsx)

dat_summary_rel_imp_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="rel_imp_climate_sex")

str(dat_summary_rel_imp_sex)

dat_summary_rel_imp_sex <- dat_summary_rel_imp_sex[ , c(1,4)]




barplot(dat_summary_rel_imp_sex$rel_cont, ylim=c(0,20))




dat_summary_rel_imp_fac_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="rel_imp_climate_fac_sex")

str(dat_summary_rel_imp_fac_sex)

dat_summary_rel_imp_fac_sex <- dat_summary_rel_imp_fac_sex[ , c(1,4)]



barplot(dat_summary_rel_imp_fac_sex$rel_cont, ylim=c(0,20))








#### Mantel test genetic and geographic distances ####


#install.packages("geosphere")
library(geosphere)



# read data

#install.packages("openxlsx")
library(openxlsx)

dat_locations_gps<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_info_locations")


str(dat_locations_gps)


#create a distance matrix

m <- distm(dat_locations_gps[3:2], dat_locations_gps[3:2], fun = distVincentyEllipsoid)

m <- m/1000 # meters into kilometers


#replace the diagonal with NA

diag(m) <- NA

#make column names for the distance matrix

colnames(m) <- paste0(dat_locations_gps$location)


# bind the distance matrix to the data frame

matrix<-cbind.data.frame(dat_locations_gps, m)

# save distance matrix


write.csv(matrix, "distance_matrix_Ranunculus_auricomus.csv")





# read data

#install.packages("openxlsx")
library(openxlsx)

#genetic distances

dat_dist_gen_tot<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_tot", colNames = FALSE)

str(dat_dist_gen_tot)

dat_dist_gen_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_sex", colNames = FALSE)

str(dat_dist_gen_sex)


dat_dist_gen_fsex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_fsex", colNames = FALSE)

str(dat_dist_gen_fsex)


dat_dist_gen_oapo<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_oapo", colNames = FALSE)

str(dat_dist_gen_oapo)




#geographical distances

dat_dist_geo_tot<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_tot", colNames = FALSE)

str(dat_dist_geo_tot)

dat_dist_geo_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_sex", colNames = FALSE)

str(dat_dist_geo_sex)


dat_dist_geo_fsex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_fsex", colNames = FALSE)

str(dat_dist_geo_fsex)


dat_dist_geo_oapo<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_oapo", colNames = FALSE)

str(dat_dist_geo_oapo)







#install.packages("ape")
library(ape)

par(mfrow=c(2,2))


dat_dist_gen_tot<-as.dist(dat_dist_gen_tot)
dat_dist_geo_tot<-as.dist(dat_dist_geo_tot)

res1<-mantel.test(dat_dist_gen_tot, dat_dist_geo_tot, npermt = 9999, graph = TRUE,
            main = "Mantel test: sexuality",
            xlab = "geographic distance", ylab = "genetic distance")

mod_mantel_sex<-lm(dat_dist_gen_tot~dat_dist_geo_tot)
summary(mod_mantel_sex)#sign.

mod_mantel_tot<-glm(dat_dist_gen_tot~dat_dist_geo_tot, family="quasibinomial")
summary(mod_mantel_tot)#sign.

plot(dat_dist_gen_tot~dat_dist_geo_tot)



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_tot, list(dat_dist_geo_tot = xweight),type="response")

lines(xweight, yweight, col="red")


text(2500,0.042, "rSP=0.186***" )




dat_dist_gen_sex<-as.dist(dat_dist_gen_sex)
dat_dist_geo_sex<-as.dist(dat_dist_geo_sex)

res2<-mantel.test(dat_dist_gen_sex, dat_dist_geo_sex, npermt = 9999, graph = TRUE,
            main = "Mantel test: sexuality",
            xlab = "geographic distance", ylab = "genetic distance")



mod_mantel_sex<-lm(dat_dist_gen_sex~dat_dist_geo_sex)
summary(mod_mantel_sex)#sign.

mod_mantel_sex<-glm(dat_dist_gen_sex~dat_dist_geo_sex, family="quasibinomial")
summary(mod_mantel_sex)#sign.

plot(dat_dist_gen_sex~dat_dist_geo_sex)



xweight <- seq(0, 2500, 0.1)

yweight <- predict(mod_mantel_sex, list(dat_dist_geo_sex = xweight),type="response")

lines(xweight, yweight, col="red")

text(1700,0.042, "rSP=0.441***" )







dat_dist_gen_fsex<-as.dist(dat_dist_gen_fsex)
dat_dist_geo_fsex<-as.dist(dat_dist_geo_fsex)

res3<-mantel.test(dat_dist_gen_fsex, dat_dist_geo_fsex, npermt = 9999, graph = TRUE,
            main = "Mantel test: facultative apomictic sexuality",
            xlab = "geographic distance", ylab = "genetic distance")


mod_mantel_fsex<-glm(dat_dist_gen_fsex~dat_dist_geo_fsex, family="quasibinomial")
summary(mod_mantel_fsex)#sign.

plot(dat_dist_gen_fsex~dat_dist_geo_fsex)



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_fsex, list(dat_dist_geo_fsex = xweight),type="response")

lines(xweight, yweight, col="red")

text(2100,0.038, "rSP=0.174***" )









#dat_dist_gen_oapo<-as.dist(dat_dist_gen_oapo)
#dat_dist_geo_oapo<-as.dist(dat_dist_geo_oapo)

res4<-mantel.test(dat_dist_gen_oapo, dat_dist_geo_oapo, npermt = 9999, graph = TRUE,
                  main = "Mantel test: obligate apomicts",
                  xlab = "geographic distance", ylab = "genetic distance")




mod_mantel_oapo<-glm(dat_dist_gen_oapo~dat_dist_geo_oapo, family="quasibinomial")
summary(mod_mantel_oapo)#sign.

plot(dat_dist_gen_oapo~dat_dist_geo_oapo)



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_oapo, list(dat_dist_geo_oapo = xweight),type="response")

lines(xweight, yweight, col="red")

text(2500,0.038, "rSP=0.256***" )



#### review: ecologcial and genetic distances #####

# read data

#install.packages("openxlsx")
library(openxlsx)


#ecological distances

dat_ecology<-read.xlsx("00_final_masterfile_final.xlsx", sheet="ecological_distances_info_locat")


str(dat_ecology)

#install.packages("ecodist")
library(ecodist)


m <- distance(dat_ecology[, c(3:12)], method = "euclidean")

m <- as.matrix(m)



#replace the diagonal with NA

diag(m) <- NA

#make column names for the distance matrix

colnames(m) <- paste0(dat_ecology$pop_ID)


# bind the distance matrix to the data frame

matrix<-cbind.data.frame(dat_ecology, m)

# save distance matrix


#write.csv(matrix, "ecological_distance_matrix_euclidean.csv")


library(openxlsx)


dat_dist_eco_tot<-read.xlsx("00_final_masterfile_final.xlsx", sheet="ecological_distance_matrix_tot", colNames = FALSE)

str(dat_dist_eco_tot)

dat_dist_eco_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="ecological_distance_matrix_sex", colNames = FALSE)

str(dat_dist_eco_sex)


dat_dist_eco_fsex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="ecological_distance_matrix_fsex", colNames = FALSE)

str(dat_dist_eco_fsex)


dat_dist_eco_oapo<-read.xlsx("00_final_masterfile_final.xlsx", sheet="ecological_distance_matrix_oapo", colNames = FALSE)

str(dat_dist_eco_oapo)





#geographic distances


dat_dist_geo_tot<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_tot", colNames = FALSE)

str(dat_dist_geo_tot)

dat_dist_geo_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_sex", colNames = FALSE)

str(dat_dist_geo_sex)


dat_dist_geo_fsex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_fsex", colNames = FALSE)

str(dat_dist_geo_fsex)


dat_dist_geo_oapo<-read.xlsx("00_final_masterfile_final.xlsx", sheet="geographic_distance_matrix_oapo", colNames = FALSE)

str(dat_dist_geo_oapo)




# genetic distances

dat_dist_gen_tot<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_tot", colNames = FALSE)

str(dat_dist_gen_tot)

dat_dist_gen_sex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_sex", colNames = FALSE)

str(dat_dist_gen_sex)


dat_dist_gen_fsex<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_fsex", colNames = FALSE)

str(dat_dist_gen_fsex)


dat_dist_gen_oapo<-read.xlsx("00_final_masterfile_final.xlsx", sheet="genetic_distance_matrix_oapo", colNames = FALSE)

str(dat_dist_gen_oapo)


#### regressions genetic and geographic distances #####


#install.packages("ape")
library(ape)

par(mfrow=c(2,4))


dat_dist_gen_tot<-as.matrix(dat_dist_gen_tot)
dat_dist_geo_tot<-as.matrix(dat_dist_geo_tot)

#res1<-mantel.test(dat_dist_gen_tot, dat_dist_geo_tot, npermt = 9999, graph = TRUE,
                  main = "Mantel test: sexuality",
                  xlab = "geographic distance", ylab = "genetic distance")


#qqnorm(dat_dist_gen_tot)
#qqline(dat_dist_gen_tot)

dat_dist_gen_tot_2<-as.dist(dat_dist_gen_tot)
dat_dist_geo_tot_2<-as.dist(dat_dist_geo_tot)


mod_mantel_tot<-lm(dat_dist_gen_tot_2~dat_dist_geo_tot_2)

summary(mod_mantel_tot)#sign.

plot(dat_dist_gen_tot ~ dat_dist_geo_tot, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))




xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_tot, list(dat_dist_geo_tot_2 = xweight),type="response")

lines(xweight, yweight, col="red")

cor.test(dat_dist_gen_tot, dat_dist_geo_tot, method="pearson")

text(2500,0.042, "rP=0.206***" )




dat_dist_gen_sex<-as.matrix(dat_dist_gen_sex)
dat_dist_geo_sex<-as.matrix(dat_dist_geo_sex)

#res2<-mantel.test(dat_dist_gen_sex, dat_dist_geo_sex, npermt = 9999, graph = TRUE,
                  main = "Mantel test: sexuality",
                  xlab = "geographic distance", ylab = "genetic distance")




#qqnorm(dat_dist_gen_sex)
#qqline(dat_dist_gen_sex)

dat_dist_gen_sex_2<-as.dist(dat_dist_gen_sex)
dat_dist_geo_sex_2<-as.dist(dat_dist_geo_sex)

mod_mantel_sex<-lm(dat_dist_gen_sex_2~dat_dist_geo_sex_2)
summary(mod_mantel_sex)#sign.

plot(dat_dist_gen_sex~dat_dist_geo_sex, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 2500, 0.1)

yweight <- predict(mod_mantel_sex, list(dat_dist_geo_sex_2 = xweight),type="response")

lines(xweight, yweight, col="red")



cor.test(dat_dist_gen_sex, dat_dist_geo_sex, method="pearson")

text(1700,0.042, "rP=0.433***" )







dat_dist_gen_fsex<-as.matrix(dat_dist_gen_fsex)
dat_dist_geo_fsex<-as.matrix(dat_dist_geo_fsex)

#res3<-mantel.test(dat_dist_gen_fsex, dat_dist_geo_fsex, npermt = 9999, graph = TRUE,
                  main = "Mantel test: facultative apomictic sexuality",
                  xlab = "geographic distance", ylab = "genetic distance")



#qqnorm(dat_dist_gen_fsex)
#qqline(dat_dist_gen_fsex)

dat_dist_gen_fsex_2<-as.dist(dat_dist_gen_fsex)
dat_dist_geo_fsex_2<-as.dist(dat_dist_geo_fsex)


mod_mantel_fsex<-lm(dat_dist_gen_fsex_2~dat_dist_geo_fsex_2)
summary(mod_mantel_fsex)#sign.

plot(dat_dist_gen_fsex~dat_dist_geo_fsex,xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_fsex, list(dat_dist_geo_fsex_2 = xweight),type="response")

lines(xweight, yweight, col="red")


cor.test(dat_dist_gen_fsex, dat_dist_geo_fsex, method="pearson")


text(2100,0.038, "rP=0.260***" )







dat_dist_gen_oapo<-as.matrix(dat_dist_gen_oapo)
dat_dist_geo_oapo<-as.matrix(dat_dist_geo_oapo)

#res4<-mantel.test(dat_dist_gen_oapo, dat_dist_geo_oapo, npermt = 9999, graph = TRUE,
                  main = "Mantel test: obligate apomicts",
                  xlab = "geographic distance", ylab = "genetic distance")



#qqnorm(dat_dist_gen_oapo)
#qqline(dat_dist_gen_oapo)

dat_dist_gen_oapo_2<-as.dist(dat_dist_gen_oapo)
dat_dist_geo_oapo_2<-as.dist(dat_dist_geo_oapo)


mod_mantel_oapo<-lm(dat_dist_gen_oapo_2~dat_dist_geo_oapo_2)
summary(mod_mantel_oapo)#sign.

plot(dat_dist_gen_oapo~dat_dist_geo_oapo, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_oapo, list(dat_dist_geo_oapo_2 = xweight),type="response")

lines(xweight, yweight, col="red")

cor.test(dat_dist_gen_oapo, dat_dist_geo_oapo, method="pearson")


text(2500,0.038, "rP=0.277***" )




#### regressions ecological and geographic distances #####


#install.packages("ape")
library(ape)

#par(mfrow=c(2,2))


dat_dist_eco_tot<-as.matrix(dat_dist_eco_tot)
dat_dist_geo_tot<-as.matrix(dat_dist_geo_tot)

#res1<-mantel.test(dat_dist_eco_tot, dat_dist_geo_tot, npermt = 9999, graph = TRUE,
                  main = "Mantel test: sexuality",
                  xlab = "geographic distance", ylab = "ecological distance")


#qqnorm(dat_dist_eco_tot)
#qqline(dat_dist_eco_tot)

dat_dist_eco_tot_2<-as.dist(dat_dist_eco_tot)
dat_dist_geo_tot_2<-as.dist(dat_dist_geo_tot)


mod_mantel_tot<-lm(dat_dist_eco_tot_2~dat_dist_geo_tot_2)

summary(mod_mantel_tot)#sign.

plot(dat_dist_eco_tot ~ dat_dist_geo_tot, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))




xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_tot, list(dat_dist_geo_tot_2 = xweight),type="response")

lines(xweight, yweight, col="red")

cor.test(dat_dist_eco_tot, dat_dist_geo_tot, method="pearson")

text(2500,0.042, "rP=0.708***" )




dat_dist_eco_sex<-as.matrix(dat_dist_eco_sex)
dat_dist_geo_sex<-as.matrix(dat_dist_geo_sex)

#res2<-mantel.test(dat_dist_eco_sex, dat_dist_geo_sex, npermt = 9999, graph = TRUE,
                  main = "Mantel test: sexuality",
                  xlab = "geographic distance", ylab = "ecological distance")




#qqnorm(dat_dist_eco_sex)
#qqline(dat_dist_eco_sex)

dat_dist_eco_sex_2<-as.dist(dat_dist_eco_sex)
dat_dist_geo_sex_2<-as.dist(dat_dist_geo_sex)

mod_mantel_sex<-lm(dat_dist_eco_sex_2~dat_dist_geo_sex_2)
summary(mod_mantel_sex)#sign.

plot(dat_dist_eco_sex~dat_dist_geo_sex, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 2500, 0.1)

yweight <- predict(mod_mantel_sex, list(dat_dist_geo_sex_2 = xweight),type="response")

lines(xweight, yweight, col="red")



cor.test(dat_dist_eco_sex, dat_dist_geo_sex, method="pearson")

text(1700,0.042, "rP=0.728***" )







dat_dist_eco_fsex<-as.matrix(dat_dist_eco_fsex)
dat_dist_geo_fsex<-as.matrix(dat_dist_geo_fsex)

#res3<-mantel.test(dat_dist_eco_fsex, dat_dist_geo_fsex, npermt = 9999, graph = TRUE,
                  main = "Mantel test: facultative apomictic sexuality",
                  xlab = "geographic distance", ylab = "ecological distance")



#qqnorm(dat_dist_eco_fsex)
#qqline(dat_dist_eco_fsex)

dat_dist_eco_fsex_2<-as.dist(dat_dist_eco_fsex)
dat_dist_geo_fsex_2<-as.dist(dat_dist_geo_fsex)


mod_mantel_fsex<-lm(dat_dist_eco_fsex_2~dat_dist_geo_fsex_2)
summary(mod_mantel_fsex)#sign.

plot(dat_dist_eco_fsex~dat_dist_geo_fsex, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_fsex, list(dat_dist_geo_fsex_2 = xweight),type="response")

lines(xweight, yweight, col="red")


cor.test(dat_dist_eco_fsex, dat_dist_geo_fsex, method="pearson")


text(2100,0.038, "rP=0.653***" )







dat_dist_eco_oapo<-as.matrix(dat_dist_eco_oapo)
dat_dist_geo_oapo<-as.matrix(dat_dist_geo_oapo)

#res4<-mantel.test(dat_dist_eco_oapo, dat_dist_geo_oapo, npermt = 9999, graph = TRUE,
                  main = "Mantel test: obligate apomicts",
                  xlab = "geographic distance", ylab = "ecological distance")



#qqnorm(dat_dist_eco_oapo)
#qqline(dat_dist_eco_oapo)

dat_dist_eco_oapo_2<-as.dist(dat_dist_eco_oapo)
dat_dist_geo_oapo_2<-as.dist(dat_dist_geo_oapo)


mod_mantel_oapo<-lm(dat_dist_eco_oapo_2~dat_dist_geo_oapo_2)
summary(mod_mantel_oapo)#sign.

plot(dat_dist_eco_oapo~dat_dist_geo_oapo, xlab="", ylab="", col=rgb(0,0,0, alpha=0.1))



xweight <- seq(0, 3000, 0.1)

yweight <- predict(mod_mantel_oapo, list(dat_dist_geo_oapo_2 = xweight),type="response")

lines(xweight, yweight, col="red")

cor.test(dat_dist_eco_oapo, dat_dist_geo_oapo, method="pearson")


text(2500,0.038, "rP=0.832***" )





##### review: area per reproduction mode ####

#install.packages("geosphere")
library(geosphere)

#install.packages('tidyverse')
#library(tidyverse)

#subset of coordinates for sexual populations
coord_sex <- subset(dat_summary, reproductive_pathway == "1_sexual", select = gps_wgs84_decimal_n:gps_wgs84_decimal_e)

# Find the convex hull of the points being plotted
hull_sex <- coord_sex %>% slice(chull(gps_wgs84_decimal_n,gps_wgs84_decimal_e))

#calculate area of polygon
areaPolygon(hull_sex) #1.229387e+12 m^2 (1.229.387.000.000 m2, 1.229.387,0 km^2)


# Define the scatterplot & overlay the convex hull
ggplot(coord_sex, aes(gps_wgs84_decimal_e,gps_wgs84_decimal_n)) + geom_point(shape = 25) + geom_polygon(data = hull_fac, alpha = 0.5) + labs(x = "Decimal degrees longitude", y = "Decimal degrees latitude")

ggplot(coord_sex, aes(gps_wgs84_decimal_e,gps_wgs84_decimal_n)) + geom_point(shape = 21) + geom_polygon(data = hull_sex, alpha = 0.5) +
  theme_minimal() + labs(x = "Decimal degrees longitude", y = "Decimal degrees latitude") +
  scale_y_continuous(limits = c(35, 55), breaks = seq(35, 55, by = 5)) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))



#facultative apomicts
coord_fac <- subset(dat_summary, reproductive_pathway == "3_apomictic_facultative_sexual", select = gps_wgs84_decimal_n:gps_wgs84_decimal_e)

hull_fac <- coord_fac %>% slice(chull(gps_wgs84_decimal_n,gps_wgs84_decimal_e))
areaPolygon(hull_fac) #2.842e+12 m^2 (2.842.000.000.000 m2, 2.842.000,000.000 km^2)

ggplot(coord_fac, aes(gps_wgs84_decimal_e,gps_wgs84_decimal_n)) + geom_point(shape = 25) + geom_polygon(data = hull_fac, alpha = 0.5) +
  theme_minimal() + labs(x = "Decimal degrees longitude", y = "Decimal degrees latitude")



#obligate apomicts
coord_obl <- subset(dat_summary, reproductive_pathway == "2_apomictic_obligate_asexual", select = gps_wgs84_decimal_n:gps_wgs84_decimal_e)

hull_obl <- coord_obl %>% slice(chull(gps_wgs84_decimal_n,gps_wgs84_decimal_e))
areaPolygon(hull_obl) #3.097.731.000.000 m^2 (3.097.731,000000 km^2)

ggplot(coord_obl, aes(gps_wgs84_decimal_e,gps_wgs84_decimal_n)) + geom_point(shape = 25) + geom_polygon(data = hull_obl, alpha = 0.5) +
  theme_minimal() + labs(x = "Decimal degrees longitude", y = "Decimal degrees latitude") +
  scale_y_continuous(limits = c(42, 61), breaks = seq(40, 60, by = 5))+
  scale_x_continuous(breaks = seq(-5, 25, by = 5))


# only apomicts


coord_apo <- subset(dat_summary, reproductive_pathway_simple == "apomictic", select = gps_wgs84_decimal_n:gps_wgs84_decimal_e)

hull_apo <- coord_apo %>% slice(chull(gps_wgs84_decimal_n,gps_wgs84_decimal_e))
areaPolygon(hull_apo) #3.661794e+12 m^2 (3.661.794,000000 km^2)

ggplot(coord_apo, aes(gps_wgs84_decimal_e,gps_wgs84_decimal_n)) + geom_point(shape = 25) + geom_polygon(data = hull_apo, alpha = 0.5) +
  theme_minimal() + labs(x = "Decimal degrees longitude", y = "Decimal degrees latitude") +
  scale_y_continuous(limits = c(42, 61), breaks = seq(40, 60, by = 5))+
  scale_x_continuous(breaks = seq(-5, 25, by = 5))






##### review reproduction mode per ploidy level (population) ####

# only own measurements

dat_summary_own<-dat_summary[-c(6, 110, 113:120, 234, 4, 105, 106, 111, 108),]

tapply(dat_summary_own$sexuality_2, dat_summary_own$reproductive_pathway, mean)

table(dat_summary_own$reproductive_pathway, dat_summary_own$ploidy_embryo_simple)

#only own measurements


#ggplot
dat_summary$reproductive_pathway <- ordered(dat_summary_own$reproductive_pathway, levels = c("sexual", "facultative apomictic", "obligate apomictic"))

ggplot(data=dat_summary_own, aes(x=reproduction_mode, y=sexuality, fill=reproduction_mode)) + geom_violin(trim = F) + 
  stat_summary(fun=median, geom="point", size=2) + scale_x_discrete(limits=c("sexual", "facultative_apomict", "obligate_apomict"), labels=c("Sexual", "Facultative apomict", "Obligate apomict")) + 
  scale_fill_manual(values = c("orange", "indianred2", "purple4")) + theme_minimal() + labs(x = "Reproduction mode", y = "Sexuality [%]") + 
  theme(legend.position="none") + scale_y_continuous(limits = c(-13, 110), breaks = seq(0, 100, by = 20)) + 
  annotate("text", x = c(1, 2, 3), y = 110, label = c("a", "b", "c")) + 
  annotate("text", x = c(1, 2, 3), y = -13, label = c("n = 16", "n = 61", "n = 156"), fontface = 'italic')



#### review bubble plots #####

#### mapping of sexuality ####

dat_summary$gps_wgs84_decimal_n<-as.numeric(dat_summary$gps_wgs84_decimal_n)
dat_summary$gps_wgs84_decimal_e<-as.numeric(dat_summary$gps_wgs84_decimal_e)

str(dat_summary)


#install.packages("ggplot2")
library(ggplot2)

#install.packages("ggmap")
library(ggmap)       

#install.packages("viridis")
library(viridis)

#install.packages("dplyr")
library(dplyr)

#install.packages("colorspace")
library(colorspace)



#[load in google map key from Kevin]

#europe_map2 <- get_map(location = c(lon = 15.2551, lat = 54.5260), maptype = "satellite", zoom = 4)
#ggmap(europe_map2)

# --> download satellite map for background





# create bubble map



# Get map data
#USA <- map_data("Austria")

worldMap <- map_data("world", region = ".", exact = FALSE)

# Select only some countries and add values
europe <- data.frame("country"=c("Austria", "Belgium", "Germany", "Spain", "Finland", "France","Greece", "Ireland", "Italy", "Netherlands", "Portugal","Bulgaria","Croatia","Cyprus", "Czech Republic","Denmark","Estonia", "Hungary","Latvia", "Lithuania","Luxembourg","Malta", "Poland", "Romania","Slovakia","Slovenia","Sweden","UK", "Switzerland","Ukraine", "Turkey", "Macedonia", "Norway", "Slovakia", "Serbia", "Montenegro","Moldova", "Kosovo", "Georgia", "Bosnia and Herzegovina", "Belarus","Armenia", "Albania"))



europe_map <- worldMap %>%
  filter(region %in% europe$country)


# Breaks sexuality
mybreaks <- c(0, 10, 30, 50, 70, 90, 100)


# Reordering (command range and mutate out if not)
dat_summary %>%
  arrange(sexuality) %>% 
  mutate(name=factor(pop_ID, unique(pop_ID))) %>% 
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=sexuality, color=sexuality), alpha=0.9) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_size_continuous(breaks = mybreaks, name = "Sexuality [%]") +
  scale_color_continuous_sequential(guide = "legend", "Plasma", rev = F, begin = 0, end = 0.7, breaks = mybreaks, name = "Sexuality [%]")

#no background
dat_summary %>%
  arrange(sexuality) %>% 
  mutate(name=factor(pop, unique(pop))) %>% 
  ggplot() +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=sexuality, color=sexuality), alpha=0.9) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_size_continuous(breaks = mybreaks, name = "Sexuality [%]") +
  scale_color_continuous_sequential(guide = "legend", "Plasma", rev = F, begin = 0, end = 0.7, breaks = mybreaks, name = "Sexuality [%]")



#### mapping of heterozygosity #####

worldMap <- map_data("world", region = ".", exact = FALSE)

# Select only some countries and add values
europe <- data.frame("country"=c("Austria", "Belgium", "Germany", "Spain", "Finland", "France","Greece", "Ireland", "Italy", "Netherlands", "Portugal","Bulgaria","Croatia","Cyprus", "Czech Republic","Denmark","Estonia", "Hungary","Latvia", "Lithuania","Luxembourg","Malta", "Poland", "Romania","Slovakia","Slovenia","Sweden","UK", "Switzerland","Ukraine", "Turkey", "Macedonia", "Norway", "Slovakia", "Serbia", "Montenegro","Moldova", "Kosovo", "Georgia", "Bosnia and Herzegovina", "Belarus","Armenia", "Albania"))



europe_map <- worldMap %>%
  filter(region %in% europe$country)


# Breaks sexuality
mybreaks <- c(0, 0.5, 1, 1.5, 2.0, 2.5, 3.0)


# Reordering (command range and mutate out if not)
dat_summary %>%
  arrange(genetic_diversity_2) %>% 
  mutate(name=factor(pop_ID, unique(pop_ID))) %>% 
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=genetic_diversity_2, color=genetic_diversity_2), alpha=0.9) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_size_continuous(breaks = mybreaks, name = "Heterozygosity [%]") +
  scale_color_continuous_sequential(guide = "legend", "Plasma", rev = T, begin = 0.2, end = 1.0, breaks = mybreaks, name = "Heterozygosity [%]")




#no background
dat_summary %>%
  arrange(genetic_diversity_2) %>% 
  mutate(name=factor(pop, unique(pop))) %>% 
  ggplot() +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, size=genetic_diversity_2, color=genetic_diversity_2), alpha=0.9) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_size_continuous(breaks = mybreaks, name = "Heterozygosity [%]") +
  scale_color_continuous_sequential(guide = "legend", "Plasma", rev = F, begin = 0, end = 0.7, breaks = mybreaks, name = "Heterozygosity [%]")




#### mapping of reproduction mode ####
dat_summary %>%
  arrange(reproduction_mode) %>% 
  mutate(name=factor(pop, unique(pop))) %>% 
  ggplot() +
  geom_polygon(data = europe_map, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, color=reproduction_mode, shape=reproduction_mode), alpha=0.9, size=3) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_color_manual(values = c("sexual"="orange", "facultative_apomict" = "indianred2", "obligate_apomict" ="purple4"), 
                     name = "Reproduction mode", breaks=c("sexual", "facultative_apomict", "obligate_apomict"), labels=c("Sexual", "Facultative apomict", "Obligate apomict")) +
  scale_shape_manual(values=c(17, 16, 20), name = "Reproduction mode", breaks=c("sexual", "facultative_apomict", "obligate_apomict"), labels=c("Sexual", "Facultative apomict", "Obligate apomict"))

#no background
dat_summary %>%
  arrange(reproduction_mode) %>% 
  mutate(name=factor(pop, unique(pop))) %>% 
  ggplot() +
  geom_point(aes(y=gps_wgs84_decimal_n, x = gps_wgs84_decimal_e, color=reproduction_mode, shape=reproduction_mode), alpha=0.9, size=3) +
  theme_void() + ylim(20,65) + xlim(-10,40) + coord_map() +
  scale_color_manual(values = c("sexual"="orange", "facultative_apomict" = "indianred2", "obligate_apomict" ="purple4"), 
                     name = "Reproduction mode", breaks=c("sexual", "facultative_apomict", "obligate_apomict"), labels=c("Sexual", "Facultative apomict", "Obligate apomict")) +
  scale_shape_manual(values=c(17, 16, 20), name = "Reproduction mode", breaks=c("sexual", "facultative_apomict", "obligate_apomict"), labels=c("Sexual", "Facultative apomict", "Obligate apomict"))





##### review snmf analysis #####


#remove all pretest samples from alignment




#BiocManager::install(c("LEA"))

library(LEA)

# Install required packages
#install.packages("fields")

library(fields)

#install.packages("RColorBrewer")

library(RColorBrewer)


source("/Users/kevin/Desktop/00_corrected/POPSutilities.r")


####min30####



#### bar charts ####


# run of pca
# Available options, K (the number of PCs),
# center and scale.
# Create files: genotypes.eigenvalues - eigenvalues,
# genotypes.eigenvectors - eigenvectors,
# genotypes.sdev - standard deviations,
# genotypes.projections - projections,
# Create a pcaProject object: pc.

#convert unlinked STRUCTURE FILE into lfmm format

?struct2geno()


#pc <- pca("all_97_new_class_without_outgroup.geno", scale = TRUE)



#main options
# K = number of ancestral populations
# entropy = TRUE: computes the cross-entropy criterion,
# CPU = 4 the number of CPUs.
project = NULL
project = snmf("all_97_new_class_without_outgroup.geno",
               K = 1:80,
               entropy = TRUE,
               ploidy = 4,
               repetitions = 10,
               project = "continue", CPU=8)


# plot cross-entropy criterion for all runs in the snmf project

project = load.snmfProject("all_97_new_class_without_outgroup.snmfProject")

plot(project, col = "blue", pch = 19, cex = 1.2)

plot(project, col = "blue", pch = 19, cex = 1.2, xlim=c(0,20), ylim=c(0.15,0.30))


# select the best run for K = 3
best = which.min(cross.entropy(project, K = 3))
my.colors <- c("tomato", "lightblue",
               "olivedrab")
barchart(project, K = 3, run = best,
         border = NA, space = 0,
         col = my.colors,
         xlab = "Individuals",
         ylab = "Ancestry proportions",
         main = "Ancestry matrix") -> bp
axis(1, at = 1:length(bp$order),
     labels = bp$order, las=1,
     cex.axis = .4)



#write.table(bp$order, "order_min10_K3.txt") 

bp_order<-read.table("order_min10_K3.txt")

barchart(project, K = 3, run = best,
         border = NA, space = 0,
         col = my.colors,
         xlab = "Individuals",
         ylab = "Ancestry proportions",
         main = "Ancestry matrix")


axis(1, at = 1:length(bp_order$x),
     labels = bp_order$x, las=1,
     cex.axis = .1, las=2)


