
library(car)

library(lsmeans)
library(performance)

df = read.csv("data\\coherence\\broadband\\SpectralFlux\\comparison\\SF-brain-CoherencePeaks.csv")
colnames(df)

# Select first peak data only
dflow <- df[1:99,]

m1 = lm(real ~ syll, data=dflow)
summary(m1)
m2 = lm(real ~ phon, data=dflow)
summary(m2)

m3 = lm(real ~ syll+ phon, data=dflow)
summary(m3)

# compare models
compare_performance(m1, m2,m3, rank = TRUE)
anova(m1,m2,m3)

# check the VIF
check_collinearity(m3)

# Select second peak data only
dfhigh <- df[100:198,]


m4 = lm(real ~ syll, data=dfhigh)
summary(m4)
m5 = lm(real ~ phon, data=dfhigh)
summary(m5)

m6 = lm(real ~ syll+ phon, data=dfhigh)
summary(m6)

# compare models
compare_performance(m4, m5,m6, rank = TRUE)
anova(m3,m4,m5)
