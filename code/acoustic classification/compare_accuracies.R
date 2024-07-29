
library(lme4)
library(sjPlot)
library(performance)
library(ggplot2)
library(lmerTest)
library(multcomp)

#df = read.csv("data\\acoustics\\syllabic_accuracy_scores.csv")
df = read.csv("data\\acoustics\\phonemic_accuracy_scores.csv")


colnames(df)

# declare speed,trial and subject as factors
df$features<- as.factor(df$features)

all_syll<-subset(df,df$conditions == 0)
all_phon<-subset(df,df$conditions == 1)
syll_low<-subset(df,df$conditions == 2)
syll_mid<-subset(df,df$conditions == 3)
syll_hig<-subset(df,df$conditions == 4)
phon_3<-subset(df,df$conditions == 5)
phon_6<-subset(df,df$conditions == 6)
phon_9<-subset(df,df$conditions == 7)


test2<-lm(accuracies~features, data=df)

post.hoc <- glht(test2, linfct = mcp(features = 'Tukey'))

# displaying the result table with summary()
summary(post.hoc)
