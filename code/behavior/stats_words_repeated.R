library(lme4)
library(sjPlot)
library(performance)
library(ggplot2)
library(lmerTest)
library(multcomp)
library(effects)
library(report) # Load the package every time you start R
library(palmerpenguins)
library(car)
library(rcompanion)
df = read.csv("\\data\\behavior\\repeated_wordsfinal.csv") 
colnames(df)


# declare speed and subject as factors
df$syll<- as.factor(df$syll)
df$suj<- as.factor(df$patient_id)
df$phon<- as.factor(df$phon)
df$conditions<- as.factor(df$conditions)



test<-aov(scores ~  conditions, data=df)

#if stats then homogeneity of variance and normality of residual not meet
shapiro.test(test$residuals)
# so it deviates from normality
leveneTest(scores ~  conditions, data=df)
#  so the homogeneity assumption of the variance is NOT met

kruskal.test(scores ~  conditions, data=df)

par(mfrow = c(1, 2)) # combine plots
plot(test, which = 3)
plot(test, which = 2)

summary(test)
report(test)

boxplot(scores ~  conditions, data=df)


########################

test2 <- scheirerRayHare(scores ~ syll + phon,data = df)


summary(test2)








