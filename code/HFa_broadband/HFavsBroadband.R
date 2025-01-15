
library(lme4)
library(sjPlot)
library(performance)
library(ggplot2)
library(lmerTest)
library(multcomp)
library(effects)
library(jtools)
library(lsmeans)
library(interactions)
library(emmeans)
library(car)

############## MODELLING #####################################################
df = read.csv("data\\HFa_broadband\\hfa_broadband.csv") 
colnames(df)


# declare speed and subject as factors
df$signal_type<- as.factor(df$signal_type)
df$suj<- as.factor(df$suj)

m1<-lmer(scale(amplitude) ~   signal_type +(1|peaks) + (1|suj) + (1|electrode), data=df)

summary(m1)
summ(m1)

plot_model(m1,type='diag')
plot_model(m1,type='est')
plot_model(m1,type='re')
plot_model(m1,type='std2')


