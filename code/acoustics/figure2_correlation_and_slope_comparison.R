
library(cocor)

df = read.csv("C:\\Users\\Jeremy\\Desktop\\zotero_folder\\data\\acoustics\\cors_sf.csv") 
colnames(df)


# low peaks

truelow = cor(df$real_peak_low,as.numeric(df$pred_peak_low), method='pearson')
cor.test(df$real_peak_low,df$pred_peak_low, method='pearson')

fakelow = cor(df$real_peak_low,df$pred_peak_high, method='pearson')
cor.test(df$real_peak_low,df$pred_peak_high, method='pearson')

trufak = cor(df$pred_peak_low,df$pred_peak_high, method='pearson')
cocor.dep.groups.overlap(truelow, fakelow,trufak, 9, var.labels=c("pred", "real","entre_eux"))


truehigh = cor(df$real_peak_high,df$pred_peak_high)
cor.test(df$real_peak_high,df$pred_peak_high)

fakehigh = cor(df$real_peak_high,df$pred_peak_low)
cor.test(df$real_peak_high,df$pred_peak_low)
cocor.dep.groups.overlap(truehigh, fakehigh,trufak, 9, var.labels=c("pred", "real","entre_eux"))

library(car)

library(lsmeans)

df = read.csv("C:\\Users\\Jeremy\\Desktop\\zotero_folder\\data\\peak_comparison\\slope_SpectralFlux.csv") 
colnames(df)

dfp1 <- subset(df, peaks == "first")
dfp2 <- subset(df, peaks == "second")

# get correlation and stats 

#######################################
# first peak                          #
#######################################


########################
# look at correlations #
########################

# with syllabic time scale
trulow = cor(subset(dfp1, conds == "syll")$real,subset(dfp1, conds == "syll")$pred, method='pearson')
cor.test(subset(dfp1, conds == "syll")$real,subset(dfp1, conds == "syll")$pred, method='pearson')

# get slope and SE
trulowlm = lm(subset(dfp1, conds == "syll")$pred ~  subset(dfp1, conds == "syll")$real )
summary(trulowlm)




# with phonemic time scale
fakelow = cor(subset(dfp1, conds == "syll")$real,subset(dfp1, conds == "phon")$pred, method='pearson')
cor.test(subset(dfp1, conds == "syll")$real,subset(dfp1, conds == "phon")$pred, method='pearson')

# get slope and SE
fakelowlm = lm(subset(dfp1, conds == "phon")$pred ~  subset(dfp1, conds == "syll")$real )
summary(fakelowlm)





# between syllabic and phonemic time scales
trufak = cor(subset(dfp1, conds == "syll")$pred,subset(dfp1, conds == "phon")$pred, method='pearson')

# are the two correlation strengths different ? 
cocor.dep.groups.overlap(truelow, fakelow,trufak, 99, var.labels=c("pred", "real","entre_eux"))

############################
# look at slopes           #
###########################

# run model first peak
m.interaction <- lm(real ~ pred*conds, data = dfp1)
anova(m.interaction)

# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "conds", var="pred")


# Compare slopes
pairs(m.lst)

##############################################################
# extra to compare slope against unity line
dfsub <- subset(dfp1, conds == "syll")
modi <- lm(real_peak_low ~ pred_peak_high,data = dfsub)
summary(modi)
linearHypothesis(modi,"pred = 1")
##############################################################


#######################################
# second peak                          #
#######################################


########################
# look at correlations #
########################

# with syllabic time scale
truhigh = cor(subset(dfp2, conds == "phon")$real,subset(dfp2, conds == "phon")$pred, method='pearson')
cor.test(subset(dfp2, conds == "phon")$real,subset(dfp2, conds == "phon")$pred, method='pearson')

# get slope and SE
truhilm = lm(subset(dfp2, conds == "phon")$pred ~  subset(dfp2, conds == "phon")$real )
summary(truhilm)



# with phonemic time scale
fakehigh = cor(subset(dfp2, conds == "phon")$real,subset(dfp2, conds == "syll")$pred, method='pearson')
cor.test(subset(dfp2, conds == "phon")$real,subset(dfp2, conds == "syll")$pred, method='pearson')

# get slope and SE
fakehilm = lm(subset(dfp2, conds == "syll")$pred ~  subset(dfp2, conds == "phon")$real )
summary(fakehilm)





# between syllabic and phonemic time scales
trufakhigh = cor(subset(dfp2, conds == "syll")$pred,subset(dfp2, conds == "phon")$pred, method='pearson')

# are the two correlation strengths different ? 
cocor.dep.groups.overlap(truehigh, fakehigh,trufakhigh, 99, var.labels=c("pred", "real","entre_eux"))

############################
# look at slopes           #
###########################

# run model first peak
m.interaction <- lm(real ~ pred*conds, data = dfp2)
anova(m.interaction)

# Obtain slopes
m.interaction$coefficients
m.lst <- lstrends(m.interaction, "conds", var="pred")


# Compare slopes
pairs(m.lst)

##############################################################
# extra to compare slope against unity line
dfsub <- subset(dfp1, conds == "syll")
modi <- lm(real_peak_low ~ pred_peak_high,data = dfsub)
summary(modi)
linearHypothesis(modi,"pred = 1")
##############################################################










