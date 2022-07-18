#LINEAR MIXED MODEL EFFECT:

#Response variable: FCSRTrl free recall
#Fixed effects (explanatory variables): trial number 
#Random effects: iD-subjects (needs to be categorical)
#Covariates: sex, age, education (will be considered as a fixed factor)

library(Matrix)
library(lme4)
library(carData)
library(car)
library(effects)
library(ggplot2)


#MODEL 1: number of trial as fix effect (without covariates) <= THIS IS THE MODEL WE STICK TO!
TrialNumber <-as.numeric(TrialNumber) 
PacienteId <-as.numeric(PacienteId)
ModelLME1 <- lmer(FCSRTrl ~ TrialNumber + (TrialNumber|PacienteId))
summary(ModelLME1)
Anova(ModelLME1, type = 2, test.statisti=c('Chisq')) 
plot(predictorEffects(ModelLME1))
options (max.print = 999999)
TableCoef1 <- coef(ModelLME1)$PacienteId #To extract the slopes for each subject
#TableRanef1 <- ranef (ModelLME1)$PacienteId


pred1a <-predict(ModelLME1, re.form=NA) #population level
pred1b <-predict(ModelLME1) #individual level
(plot1<-ggplot(DataModeloLme, aes(x=TrialNumber, y=FCSRTrl)) + 
    geom_line(aes(y=pred1b, group=PacienteId), alpha = 0.05) + 
    geom_smooth(aes(y=pred1a), method = "glm", size=1.2) + theme_classic() + theme(legend.position = "none") +
    scale_x_continuous(breaks = c(1, 2, 3)) + scale_y_continuous(breaks = seq(0,16,2)) +
    xlab("Trial number") + ylab("FCSRT free recall")) 


#MODEL 2: number of trial as fix effect + covariables (sex, education, age)
TrialNumber <-as.numeric(TrialNumber)  
LevelEduNum <-as.factor(LevelEduNum) 
Sex <- as.factor(Sex)
ModelLME2 <- lmer(FCSRTrl ~ TrialNumber + Sex + LevelEduNum + Age + (TrialNumber|PacienteId))
summary(ModelLME2)
Anova(ModelLME2, type = 2, test.statisti=c('Chisq')) 
plot(predictorEffects(ModelLME2))
TableCoef2 <-coef (ModelLME2)$PacienteId #To extract the slopes for each subject
#TableRanef2 <-ranef (ModelLME2)$PacienteId
sex_pred <- as.data.frame(effect('Sex', ModelLME2))
edu_pred <- as.data.frame(effect('LevelEduNum',ModelLME2))
age_pred <- as.data.frame(effect('Age', ModelLME2))

sex_plot <- ggplot(sex_pred,aes(Sex, fit, ymin = lower, ymax = upper)) +
  geom_pointrange() + xlab('Sex') + ylab('FCSRT free recall')+
  scale_x_discrete(labels=c('Male','Female')) + theme_classic()
edu_plot <- ggplot(edu_pred,aes(LevelEduNum, fit, ymin = lower, ymax = upper)) +
  geom_pointrange() + xlab('Level of Education') + ylab('FCSRT free recall')+
  scale_x_discrete(labels=c('< Primary','Primary', 'Secondary', 'Higher'))+ theme_classic()
age_plot <- ggplot(age_pred, aes(Age, fit, ymin = lower, ymax = upper))+
  geom_line() + geom_ribbon(alpha=0.5)+
  xlab("Age") + ylab("FCSRT free recall")+theme_classic()
library(ggpubr)
ggarrange(plot1, age_plot, sex_plot, edu_plot,ncol = 2, nrow = 2)