## Assignment - Zoltan Kekecs
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("psych")
install.packages("sjPlot")
install.packages("car")
install.packages("sjlabelled")
install.packages("lmtest")
install.packages("lmerTest")
install.packages("sandwich")
install.packages("e1071")
install.packages("modelr")
install.packages("broom")
install.packages("cAIC4")
install.packages("r2glmm")
install.packages("lme4")
install.packages("MuMIn")
install.packages("dplyr")



library(tidyverse)
library(ggplot2)
library(psych)
library(sjPlot)
library(gridExtra)
library(sjmisc)
library(car) 
library(sjlabelled)
library(lmtest)
library(lmerTest)
library(sandwich)
library(RColorBrewer)
library(e1071)
library(modelr)
library(broom)
library(cAIC4)
library(r2glmm)
library(lme4)
library(MuMIn)
library(dplyr)
library(apaTables)

pain_data <- read.csv("https://tinyurl.com/ha-dataset1
")
View(pain_data)

## pain is on scale 0-10 (0 = "no pain"; 10 = "worst pain I can imagine")

## T: State Trait Anxiety Inventory: anxiety scaled from 20-80 (higher score = higher anxiety)
#### STAI_trait --> established correlation btw anxiety and experienced pain

## pain catastrophizing Scale 0-52 (higher score = higher catastrophizing)
#### pain_cat --> well established variable

## Mindful Attention Awareness Scale(MAAS) 1-6 (average of item score)(higher scores = higher dispositional mindfulness)
#### not yet well established to be connected to pain!

## Cortisol(stress hormone) #### cortisol_serum #### cortisol_saliva
#### thought to be positively associated with pain experience

## weight(Kg), IQ, household income(USD) also included - not linked with reported pain

# pain is different within and between individuals
# understand what influences pain/ predict pain
# difference between state and trait psychological measures on pain 

# Research Question 1:
# suggested: age is negatively associated with pain 
# suggested: sex is a predictor more dependent on the type of the procedure

#############################################################################
# Hierarchical regression containing:


## notice the IQ outlier... and maybe don't take it out

pain_data_use <- pain_data%>%
filter(age != "444", STAI_trait > 19, household_income != "-3732")
###### what to do with really low IQ and household income?

summary(pain_data_use)
describe(pain_data_use)

################ model 1 #########################
# model 1: age + sex --> pain

## Scatterplot: linear relationship between age (x) and pain (y)
scatter.smooth(x = pain_data_use$age, y = pain_data_use$pain)

pain_data_use %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

##Scatterplot: linear relationship between sex (x) and pain (y)
scatter.smooth(x = pain_data_use$sex, y = pain_data_use$pain)

pain_data_use %>% 
  ggplot()+
  aes(x = pain, fill = sex)+
  geom_bar(position = "dodge")

##Boxplot: spot outliers 
pain_data_use %>% 
  ggplot()+
  aes(group = pain, y = age)+
      geom_boxplot()


pain_data_use%>%
  ggplot()+
  aes(group = sex, y = pain)+
  geom_boxplot()


pain_data_use %>% 
  ggplot()+
  aes(x = age, y = pain, color = sex)+
  geom_point()+
  geom_smooth()


pain_data_use %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_density()


################ model 2 ##############################
# model 2: age + sex + STAI + pain catastrophizing + mindfulness + cortisol measures

# Scatterplot STAI_trait and pain - linearity
pain_data_use %>% 
  ggplot()+
  aes(x = age, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = pain, fill = sex)+
  geom_bar(position = "dodge")

pain_data_use %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = STAI_trait, y = pain)+
  geom_point()+
  stat_smooth()

# scatterplot pain_cat and pain - linearity
pain_data_use %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = pain_cat, y = pain)+
  geom_point()+
  stat_smooth()

# scatterplot mindfulness and pain
pain_data_use %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = mindfulness, y = pain)+
  geom_point()+
  stat_smooth()


# scatterplot cortisol_saliva and pain
pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_saliva, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_saliva, y = pain)+
  geom_point()+
  stat_smooth()


# scatterplot cortisol_serum and pain
pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_serum, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_serum, y = pain)+
  geom_point()+
  stat_smooth()


##boxplot outliers - Model 2
pain_data_use%>%
  ggplot()+
  aes(group = sex, y = pain)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = age, y = pain)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = STAI_trait, y = pain)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = pain_cat, y = pain)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = pain, y = mindfulness)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = pain, y = cortisol_saliva)+
  geom_boxplot()

pain_data_use%>%
  ggplot()+
  aes(group = pain, y = cortisol_serum)+
  geom_boxplot()

#### density plots of independent variables
pain_data_use %>% 
  ggplot()+
  aes(x = age)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = pain)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = IQ)+
  geom_density()


#### model 1 variables
Vmod1 <- pain_data_use %>% 
  select(pain, age, sex)

plot(Vmod1)

#### Multiple Regression model 1
MRmod1 <- lm(pain ~ age + sex, data = pain_data_use)
summary(MRmod1)
plot(MRmod1)
# R^2, adj R^2, and P-value (sig. <.001) look good - the model is significant 
# age is a significant contributory factor when explaining pain experience (the older you get the less sensitive you are to pain)
# sex is not a significant contributor to explaining experienced pain
# a few influential points (99, 139, 126) but they can stay: seems to be mainly due to high/ low experienced pain values

## cook's distance
plot(MRmod1, which = 4)
CDmod1 <- cooks.distance(MRmod1)
round(CDmod1, 5)%>%
  sort()

####### model 2 variables
Vmod2 <- pain_data_use %>% 
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_saliva, cortisol_serum)

plot(Vmod2)

#### Multiple Regression model 2
MRmod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = pain_data_use)
summary(MRmod2)
plot(MRmod2)
# R^2, adj R^2, and P-value (sig. <.000) look good - the model is significant
# age is now not a significant contributing factor to predict experienced pain
# sex is not a significant contributing factor to predict experienced pain
# STAI_trait is not significant to contribute to the model
# pain_cat is significant at predicting experienced pain
# mindfulness is significant at predicting experienced pain (higher mindfulness - less experienced pain)
# cortisol_saliva is somewhat significant at predicting experienced pain
# cortisol_serum is not significant at predicting experienced pain
# none of the influential points seem to be extreme enough to take out (68, 99, 112)

## cook's distance
plot(MRmod2, which = 4)
CDmod2 <- cooks.distance(MRmod2)
round(CDmod2, 5)%>%
  sort()

  
### model comparison - QQ plot - Normality
MRmod1 %>% plot(which = 2)
MRmod2 %>% plot(which = 2)
## skew and kurtosis: -1; 1 => normality assumption is not violated
describe(residuals(MRmod1))
describe(residuals(MRmod2))
## line needs to be strait and the Turkey test needs to be non significant for linearity
MRmod1 %>% residualPlots()
MRmod2 %>% residualPlots()
## homogeneity
MRmod1 %>% plot(which = 3)
MRmod1 %>% ncvTest()
# not significant = homogeneity assumption is not violated
MRmod2 %>% plot(which = 3)
MRmod2 %>% ncvTest()
# not significant = homogeneity assumption is not violated

## multicollinearity - threshold is 3
MRmod1 %>% vif()
MRmod2 %>% vif()
## high multicollinearity between Cortisol_saliva and cortisol_serum which makes sense, as both measure the same thing. Since serum is more widely used when researching/ measuring cortisol I will take the saliva measurement out of the model


############################################################################
### new model 2 without cortisol_saliva
NewMod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum, data = pain_data_use)

############ Re-check Assumptions
summary(NewMod2)
# R^2, adj R^2, and p-value (sig. <.000) look very good - the model is significant
# age seems to be a significantly contributing factor to predicting pain experience
# sex does not seem to sig. contribute to predicting pain
# STAIT_trait does not seem to conrtibute much to predicting pain
# pain_cat remains a sign predictor to experienced pain
# mindfulness is still negatively associated with pain
# cortisol_serum is now a highly sifnificant predictor of pain
plot(NewMod2)
# Residuals don't seem to violate the assumption for linearity
# Q-Q Plot does not seem to violate the assumption for normality
# Homoscedasticity = fairly equal spread
# cook's distance does not seem to be violated (within acceptable value range)

## cook's distance
plot(NewMod2, which = 4)
CDNewMod2 <- cooks.distance(NewMod2)
round(CDNewMod2, 5)%>%
  sort()
# none of the influential points seem to be extreme enough to take out (95, 99, 112)



### model comparison - QQ plot - Normality
MRmod1 %>% plot(which = 2)
NewMod2 %>% plot(which = 2)

## skew and kurtosis: -1; 1 => normality assumption is not violated
describe(residuals(MRmod1))
describe(residuals(NewMod2))
## line needs to be strait and the Turkey test needs to be non significant for linearity
MRmod1 %>% residualPlots()
NewMod2 %>% residualPlots()
## homogeneity
MRmod1 %>% plot(which = 3)
MRmod1 %>% ncvTest()
# not significant = homogeneity assumption is not violated
NewMod2 %>% plot(which = 3)
NewMod2 %>% ncvTest()
# not significant = homogeneity assumption is not violated
# homoscedasticity does not seem to be violated
## multicollinearity - threshold is 3
MRmod1 %>% vif()
NewMod2 %>% vif()
## Multicollinearity is not violated

### model comparison
AIC(MRmod1) - AIC(NewMod2)


apa.aov.table(lm_output, filename = "Table4_APA.doc", table.number = 4)

 anova(MRmod1, NewMod2)


# the new model is significanlty better at predicting pain experience

summary(MRmod1)
summary(NewMod2)
AIC(MRmod1)
AIC(NewMod2)
### as NewMod2 (494.7841) is more than 2 AIC units lower than MRmod1 (582.9313) it is considered to be a significantly better model to explain experienced pain
AIC(MRmod1) - AIC(NewMod2)
summary(MRmod1) ### adj R^2 = .05291
summary(NewMod2) ### adj R^2 = .5729


## SUM SQUARES################################################

  


## Pain = 1.86 -.04(age) + .27(sexmale) - .02(STAI_trait) + .11(pain_cat) - .28(mindfulness) + .56(cortisol_serum)

tab_model(MRmod1, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

tab_model(NewMod2, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
##### look at bar options: plots : can save as pdf



###################################################################################
## Assignment 2 - backwards regression

full_back_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = pain_data_use)
summary(full_back_model)

full_back_variables <- pain_data_use %>% 
  select(pain, age, sex, STAI_trait, pain_cat, mindfulness, cortisol_serum, weight, IQ, household_income)
plot(full_back_variables)

plot(full_back_model)
### linearity + normality + homoscedasticity + homogeneity do not seem to be violated 

summary(pain_data_use)
describe(pain_data_use)


pain_data_use %>% 
  ggplot()+
  aes(x = weight, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = IQ, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(x = household_income, y = pain)+
  geom_point()+
  stat_smooth(method ="lm", se = FALSE)

pain_data_use %>% 
  ggplot()+
  aes(group = pain, y = weight)+
  geom_boxplot()

pain_data_use %>% 
  ggplot()+
  aes(group = pain, y = IQ)+
  geom_boxplot()

pain_data_use %>% 
  ggplot()+
  aes(group = pain, y = household_income)+
  geom_boxplot()

pain_data_use %>% 
  ggplot()+
  aes(x = weight)+
  geom_density()

pain_data_use %>% 
  ggplot()+
  aes(x = IQ)+
  geom_density()


pain_data_use %>% 
  ggplot()+
  aes(x = household_income)+
  geom_density()



#### Multiple Regression full_back_model
full_back_model
summary(full_back_model)
plot(full_back_model)

## cook's distance
plot(full_back_model, which = 4)
CDFBM <- cooks.distance(full_back_model)
round(CDFBM, 5)%>%
  sort()
### everything is within the acceptable range of cook's distance (-1;1)


#### Backwards Regression
BWRmod = step(full_back_model, direction = "backward")

summary(BWRmod)
plot(BWRmod)
## highly significant model, none of the assumptions seem to be violated

### model comparison - QQ plot - Normality
BWRmod %>% plot(which = 2)

## skew and kurtosis: -1; 1 => normality assumption is not violated
describe(residuals(BWRmod))

## line needs to be strait and the Turkey test needs to be non significant for linearity
BWRmod %>% residualPlots()

## homogeneity
BWRmod %>% plot(which = 3)
BWRmod %>% ncvTest()
# not significant = homogeneity assumption is not violated

## multicollinearity - threshold is 3
BWRmod %>% vif()
### no notable multicollinearity



### model comparison
TB_model <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = pain_data_use)

BW_model <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = pain_data_use)


AIC(TB_model) - AIC(BW_model)

anova(NewMod2, BWRmod)
# no p-value as the models are not nested within eachother

summary(TB_model)
summary(BW_model)
AIC(TB_model)
AIC(BW_model)
### as NewMod2 (494.7841) is around 2 AIC units higher than BWRmod (492.7222) the Backward regression model seems to be a slightly better fit that is significant
AIC(TB_model) - AIC(BW_model)
summary(TB_model) ### adj R^2 = .473
summary(BW_model) ### adj R^2 = .480


### SUM squares
predict_TB_model1= predict(TB_model, pain_data_use)

predict_BW_model1 = predict(BW_model, pain_data_use)

RSS_TB_model1 = sum((pain_data_use[, "pain"] - predict_TB_model1)^2)
RSS_TB_model1

RSS_BW_model1 = sum((pain_data_use[, "pain"] - predict_BW_model1)^2)
RSS_BW_model1

RSS_BW_model1 - RSS_TB_model1

model_TB_mean = lm(pain~1, data = pain_data_use)

TSS_TB_model1 = sum(pain_data_use$pain - predict(model_TB_mean)^2)

R_TB1 = 1 - (RSS_TB_model1/ TSS_TB_model1)
R_TB1

model_BW_mean = lm(pain~1, data = pain_data_use)
TSS_BW_model1 = sum(pain_data_use$pain - predict(model_BW_mean)^2)
R_BW1 = 1 - (RSS_BW_model1/ TSS_BW_model1)
R_BW1
## Pain(TB_model) = 1.86 -.04(age) + .27(sexmale) - .02(STAI_trait) + .11(pain_cat) - .28(mindfulness) + .56(cortisol_serum)
## Pain(BW_model) = 1.95 - .04(age) + .28(sexmale) + .11(pain_cat) - .26(mindfulness) + .52(cortisol_serum) - .00(household_income)


tab_model(TB_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
##### look at bar options: plots : can save as pdf
tab_model(BW_model, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
### smaller model is nested in the bigger model with the bigger model including all of the varieables of the smaller model - else can't do anova

###AIC gets fooled when you take out some of the predictors it makes the model artificially smaller
#### descriptive difference but not significance

##### theory based model is better to use than a statistical based model as it makes more sense

pain_data2 = read.csv("https://tinyurl.com/ha-dataset2")

View(pain_data2)
summary(pain_data2)

pain_data2_use <- pain_data2%>%
  filter(mindfulness <= 6)

View(pain_data2_use)
summary(pain_data2_use)
describe(pain_data2_use)

########### model comparison

AIC(TB_model2)
AIC(BW_model2)

TB_model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = pain_data2_use)

BW_model2 <- lm(pain ~ age + sex + pain_cat + mindfulness + cortisol_serum + household_income, data = pain_data2_use)

tab_model(TB_model2, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)
##### look at bar options: plots : can save as pdf
tab_model(BW_model2, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

 
##### residuals , sum of squares : compares what our model predicted and what the backwards model predicted -- comparing how close they are to the real world 

#### sum of squares and shows that in the realworld our model is better than the backwards model as the backwards model is overfit to the dataset

#################################################################################################################
############################### Assignment 3 ########################################
### need to finish the videos
## load and check data
## remove variables that need to be removed
## mutate data to get hospitals as factors
## mutate(hospital = factor(hospital)) ### can check if factor with str()
## build a mixed model with a intercept (1, hospital)
## r2beta, r2mm

## predict function with random
## 

hospital3.1 <- read.csv("https://tinyurl.com/ha-dataset3")
View(hospital3.1)

summary(hospital3.1)
describe(hospital3.1)

hospital3.1 <- hospital3.1 %>% 
  mutate(sex = replace(sex, sex =="femlae","female")) %>% 
  filter(household_income > 0)

Vhospital3.1 <- hospital3.1 %>% 
  select(pain, sex, age, STAI_trait, pain_cat, cortisol_serum, mindfulness)

plot(Vhospital3.1)

#### hospital = factor & hospital => levels

hospital3.1 = hospital3.1 %>% 
  mutate(hospital = factor(hospital, ordered = FALSE, levels = c("hospital_1", "hospital_2", "hospital_3", "hospital_4", "hospital_5", "hospital_6", "hospital_7", "hospital_8", "hospital_9", "hospital_10")))

hospital3.1$hospital[is.na(hospital3.1$hospital)] <- "hospital_9"

### plots
hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= sex)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)

hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= age)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)

hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= STAI_trait)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)

hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= pain_cat)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)

hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= cortisol_serum)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)

hospital3.1 %>% 
  ggplot()+
  aes(y = pain, x= mindfulness)+
  geom_point(aes(color = hospital) )+
  geom_smooth(method="lm", se =F)



hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=age, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)+
  xlim(0, 80)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)

hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=sex, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)

hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=STAI_trait, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)+
  xlim(0, 80)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)

hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=pain_cat, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)+
  xlim(0, 80)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)

hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=mindfulness, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)+
  xlim(-1, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)

hospital3.1 %>% 
  ggplot()+
  aes(y=pain, x=cortisol_serum, color = hospital)+
  geom_point(size=4)+
  geom_smooth(method="lm", se=F, fullrange=TRUE)+
  xlim(0, 10)+
  geom_hline(yintercept=0)+
  geom_vline(xintercept = 0)


hospital_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = hospital3.1)

hospital_rnd_int

plot(hospital_rnd_int)

tab_model(hospital_rnd_int, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

TB_3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +  cortisol_serum, data = hospital3.1)
tab_model(TB_3, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)

#### compare model NewMod2 with hospital_rnd_int

## model fit and confidence intervals for hospital_rnd_int
summary(TB_3)
summary(hospital_rnd_int)

confint(TB_3)
confint(hospital_rnd_int)


sum(residuals(TB_3)^2)
sum(residuals(hospital_rnd_int)^2)

AIC(TB_3)
cAIC(hospital_rnd_int)$caic

r.squaredGLMM(hospital_rnd_int)
r2beta(hospital_rnd_int, method = "nsj", data = hospital3.1)

######
predict_int_model3= predict(hospital_rnd_int, hospital3.1)

predict_TB_3 = predict(TB_3, hospital3.1)

RSS_int_model3 = sum((hospital3.1[, "pain"] - predict_int_model3)^2)
RSS_int_model3

RSS_TB_3 = sum((hospital3.1[, "pain"] - predict_TB_3)^2)
RSS_TB_3

RSS_NewMod2_3 - RSS_int_model3 


########### using hospital_rnd_int to predict pain in data set 4
hospital3.2 <- read.csv("https://tinyurl.com/ha-dataset4")
View(hospital3.2)

summary(hospital3.2)

hospital3.2 <- hospital3.2 %>% 
  filter(mindfulness < 6, household_income > 0)

summary(hospital3.2)
describe(hospital3.2)

hospital_rnd_int4 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = hospital3.2)


hospital_rnd_int4

summary(hospital_rnd_int4)

confint(hospital_rnd_int4)


sum(residuals(hospital_rnd_int4)^2)

cAIC(hospital_rnd_int4)$caic

r.squaredGLMM(hospital_rnd_int4)
r2beta(hospital_rnd_int4, method = "nsj", data = hospital3.2)

anova(hospital_rnd_int4, hospital_rnd_int) ## not working 


######
predict_int_model4 = predict(hospital_rnd_int, hospital3.2, allow.new.levels = TRUE)

predict_TB_4 = predict(TB_3, hospital3.2)

RSS_int_model4 = sum((hospital3.2[, "pain"] - predict_int_model4)^2)
RSS_int_model4

RSS_TB_Model4 = sum((hospital3.2[, "pain"] - predict_TB_4)^2)
RSS_TB_Model4

 RSS_TB_Model4 - RSS_int_model4 








model_TB_mean = lm(pain~1, data = hospital3.2)
TSS_TB_model = sum(hospital3.2$pain - predict(model_TB_mean)^2)

R_TB2 = 1 - (RSS_TB_model/ TSS_TB_model)
R_TB2

model_BW_mean2 = lm(pain~1, data = pain_data2_use)
TSS_BW_model = sum(pain_data2_use$pain - predict(model_BW_mean2)^2)
R_BW2 = 1 - (RSS_BW_model/ TSS_BW_model)
R_BW2

tab_model(hospital_rnd_int, TB_3, show.est = TRUE, show.std = TRUE, show.p = TRUE, show.se = FALSE, show.stat = FALSE, show.df = FALSE)






