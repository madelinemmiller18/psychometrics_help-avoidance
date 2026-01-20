print(getwd())
path="~/Desktop/psychometrics_foundations/Help_Avoidance"
setwd(path)

#install libraries
library(psych) # library for factor analysis
library(MVN) # library for multivariate normal distributions
library(GPArotation) # library for rotating factors
library(dplyr)
library(car) #use qqplot function
library(lavaan) # library for CFA and structural equation modeling (SEM)
library(semPlot) # library for visualizing SEM path diagrams

#upload data
rawdata <- read.csv("data/Help Avoidance in Group Projects.csv")
rawdata$id <- 1:nrow(rawdata) #Add ID column

dim(rawdata) # dimension
head(rawdata) # first entries
# save the relevant item labels
df <- rawdata[c(3:14)]

#change column names
df <- df %>%
  rename(
    explain_hs = "I.ask.my.group.members.to.explain.concepts.that.confuse.me.",
    quiet_ha = "If.I.do.not.understand..I.will.be.quieter.in.group.project.meetings.",
    support_hs = "If.I.am.struggling.with.my.project.contributions..I.seek.support.from.my.group.members.",
    feedback_ha = "When.I.am.unsure.about.my.work..I.am.reluctant.to.seek.feedback.from.my.group.",
    participation_ha = "If.a.project.is.too.hard.for.me..I.participate.less.rather.than.ask.my.group.for.help.",
    taskhelp_ha = "I.avoid.asking.my.group.members.for.help.even.when.I.find.my.task.difficult.",
    pretunderstand_ha = "I.pretend.to.understand.group.project.discussions.even.when.I.feel.confused..",
    clarification_ha = "I.hesitate.to.ask.my.teammates.for.clarification.during.group.projects.",
    workown_ha = "I.tend.to.keep.working.on.my.own.even.when.collaborating.with.my.group.would.make.the.task.easier.",
    degree_level = "What.is.your.current.student.status.",
    fos = "Which.field.best.describes.your.area.of.study.",
    langlevel = "Self.rate.your.skills.in.the.language.typically.used.for.your.group.projects..e.g...German.or.English.."
  )

#degree level numerical encoding 
# Classify as 1 (Undergraduate or equivalent)
unique(df$degree_level) #return unique values
df$degree_level[df$degree_level == "Undergraduate Student (Bachelor or equivalent)"] <- 1
df$degree_level[df$degree_level  == "MD"] <- 1
df$degree_level[df$degree_level  == "Medical Student"] <- 1
# Classify as 2 (Master or equivalent)
df$degree_level[df$degree_level == "Graduate Student (Master or equivalent)"] <- 2
# Classify as 3 (Doctoral or equivalent)
df$degree_level[df$degree_level == "Doctoral Student (PhD or equivalent)"] <- 3

#language level self rating numerical encoding 
unique(df$langlevel) #return unique values
df[df=="Less than adequate"]<-1
df[df=="Adequate"]<-2
df[df=="Good"]<-3
df[df=="Excellent"]<-4
df[df=="Native Speaker"]<-5

#fos numerical encoding 
unique(df$fos) #return unique values
df[df=="Economics and Social Sciences"]<-1
df[df=="Theology"]<-2
df[df=="Science"]<-3
df[df=="Humanities"]<-4
df[df=="Medicine"]<-5
#assign data science to economics and social sciences
df[df=="Data Science"]<-1
#assign rhetorics, philospophy to humanities
df[df=="Rhetorics, philosophy"]<-4

# save the relevant item labels
item_names <- colnames(df[1:9])

# all your information should be saved in a data frame where the 
# number of rows correspond to the number of participants 
# and the number of columns to the number of items and covariates.
# Use some kind of id as column names (instead of the full questions), 
# e.g. rename a single column: colnames(dat.sub)[1] <- "item1"
# or all columns: colnames(dat.sub) <- c("item1", "item2",...)
# The entries should be the item responses (values between 1 and 6)
# and some numerical encoding for the covariates


###############################################################
# ANALYSIS
###############################################################

# For now, we just use the item information, not the covariates
df1 <- df[,item_names]
#correlation matrix
round(cor(df1), 2)
#explain_hs and support_hs should be reversed (negative).
# So, we re-code by subtracting the response value from 
# the maximum score plus 1 (which is 7 in this case)
df1[,"explain_hs"] <- 7-df1[,"explain_hs"]
df1[,"support_hs"] <- 7-df1[,"support_hs"]



##########################################################################
# Confirmatory factor analysis (CFA)
##########################################################################
# NOTE: typically one should not use the same data for efa and cfa, but
# cross-validate with different samples because using the results from the 
# efa  to define the cfa model may inflate the model fit, creating bias

# one factor model
model1 <- '
f1 =~ explain_hs + quiet_ha + support_hs + feedback_ha + participation_ha 
  + taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha 
'

# two factor model - positively and negatively coded items
# equality constraint added to "nc" in order to identify, as only two items
pn_coding_model <- '
pc =~ quiet_ha + feedback_ha + participation_ha 
  + taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha
nc =~ l1*explain_hs + l1*support_hs 
'
# two factor model - human coded items and ChatGPT items 
ai_model2 <- '
hu =~ explain_hs + quiet_ha + support_hs  + participation_ha 
ai =~ taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha 
'

# two factor model suggested from EFA
model2 <- '
pa1 =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha 
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
'

##########################################################################

# run model 1
cfa1 <- cfa(model1,df1,std.lv=T)

# output
summary(cfa1,standardized=T,fit=T)

# plot path diagram with standardized factor loadings
semPaths(cfa1, "std")
# -> very loadings for items 1 and 2, moderate to high (negative factor
#    loadings for items 3 to 6

# calculate the residual covariance as standardized z-values 
# (check for > +1.96 or < -1.96)
cov1 <- round(resid(cfa1, type="standardized")$cov, 2)
cov1
# -> there is some unwanted residual correlation, 
#    especially between ####

##########################################################################

# run model pn
cfa_pn <- cfa(pn_coding_model2,df1,std.lv=T)

# output
summary(cfa_pn,standardized=T)

# plot path diagram with standardized factor loadings
semPaths(cfa_pn, "std")
# -> moderate to high factor loadings for all items

# calculate the residual covariance as standardized z-values 
# (check for > +1.96 or < -1.96)
cov_pn <- round(resid(cfa_pn,type="standardized")$cov,2)
cov_pn

# run model ai
cfa_ai <- cfa(ai_model2,df1,std.lv=T)

# output
summary(cfa_ai,standardized=T)

# plot path diagram with standardized factor loadings
semPaths(cfa_ai, "std")
# -> moderate to high factor loadings for all items

# calculate the residual covariance as standardized z-values 
# (check for > +1.96 or < -1.96)
cov_ai <- round(resid(cfa_ai,type="standardized")$cov,2)
cov_ai

# run model 2
cfa2 <- cfa(model2,df1,std.lv=T)

# output
summary(cfa2,standardized=T)

# plot path diagram with standardized factor loadings
semPaths(cfa2, "std")
# -> moderate to high factor loadings for all items

# calculate the residual covariance as standardized z-values 
# (check for > +1.96 or < -1.96)
cov_2 <- round(resid(cfa2,type="standardized")$cov,2)
cov_2

# Model comparison to one-factor model
anova(cfa2,cfa1)
# Chi-Squared Difference Test 
#       Df   AIC   BIC  Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
# cfa2  9 2480.4 2513.9 20.238                                    
# cfa1  9 2492.1 2525.5 31.887      11.65     0       0  
# NOTE: models are not nested due to the constraint, so we cannot use the
# the model difference test (see for instance that the degrees of freedom are
# the same for both models), instead use AIC and BIC (smaller is better)
# -> 2 factor model is better

# Model tuning: We can consider to include/free other parameters in the model
# (NOTE: Exploratory)
# Create a list with different changes and the expected decrease in the 
# chi-square statistic (mi) as measure for the model fit improvement
mi <- modindices(cfa2, sort. = T)

mi
# show all mi > 3.84 (significant)
mi[mi$mi>3.84,]
# lhs op rhs     mi    epc sepc.lv sepc.all sepc.nox
# 17  f1 =~  y3  9.896  0.608   0.608    0.390    0.390
# 19  f1 =~  y5 11.771 -0.610  -0.610   -0.429   -0.429
# 24  y1 ~~  y3  5.108  0.353   0.353    0.254    0.254
# -> there is a possibility to add a residual correlation between items 1 and 3
# but it does not make any sense on 2nd glance at the items:
#
# -> ST164Q01IA I concentrate on the parts of the text that are easy to understand.
# ST164Q02IA I quickly read through the text twice.
# -> ST164Q03IA After reading the text, I discuss its content with other people. 
# ST164Q04IA I underline important parts of the text.
# ST164Q05IA I summarise the text in my own words.
# ST164Q06IA I read the text aloud to another person.
#
# This might be due to the length of sentences, but does not seem reasonable 
# enough. Other suggested changes would change the clear factor structure,
# so we keep the original two-factor model.

# Final step: test factor correlation
# Define model with zero correlation
model3 <- '
pa1 =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha 
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha

pa1 ~~ 0*pa2              # this constraint sets the correlation to zero
'
# Run the model
cfa3 <- cfa(model3,df1,std.lv=T)
# Model comparison to correlated factors model (nested models)
anova(cfa2, cfa3)
# -> p-value 0.05244 > 0.05 -> no significant difference between the models
# -> evidence that there is no correlation between the factors (or it is
#    too small to be detected)

# Possible interpretation of the factors: 
# f1 = superficial reading
# f2 = in-depth reading
