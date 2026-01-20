###################
# PISA DATA 2018
###################
# Items
#
# Reading strategy:
# ST164Q01IA - I concentrate on the parts of the text that are easy to understand.
# ST164Q02IA - I quickly read through the text twice.
# ST164Q03IA - After reading the text, I discuss its content with other people. 
# ST164Q04IA - I underline important parts of the text.
# ST164Q05IA - I summarise the text in my own words.
# ST164Q06IA - I read the text aloud to another person.
#
# Reading self-concept:
# ST161Q01HA - I am a good reader.
# ST161Q02HA - I am able to understand difficult texts.
# ST161Q03HA - I read fluently.
# ST161Q06HA - I have always had difficulty with reading.
# ST161Q07HA - I have to read a text several times before completely understanding it.
# ST161Q08HA - I find it difficult to answer questions about a text.

library(mirt) # library for multidimensional item response theory
library(lavaan) #  library for CFA and structural equation modeling (SEM)
library(dplyr)

# Set your working directory
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
setwd(path)

###############################################################
# set up data
###############################################################
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

dfitems <- df[,item_names]

# all your information should be saved in a data frame where the 
# number of rows correspond to the number of participants 
# and the number of columns to the number of items and covariates.
# Use some kind of id as column names (instead of the full questions), 
# e.g. rename a single column: colnames(dat.sub)[1] <- "item1"
# or all columns: colnames(dat.sub) <- c("item1", "item2",...)
# The entries should be the item responses (values between 1 and 6)
# and some numerical encoding for the covariates

###############################################################
# Part 1: Binary data illustration (only for demonstration!)
###############################################################

# Create binary data (to have an equivalent to right and wrong)
dfitems2 <- dfitems
dfitems2[dfitems2 < 4] <- 0
dfitems2[dfitems2 > 3] <- 1
colnames(dfitems2) <- colnames(dfitems)
# Plot to values to check that they are always 0 or 1
plot(unlist(dfitems),unlist(dfitems2))

# Model items with a Rasch (1PL) IRT model
onePL <- mirt(dfitems2, 1, 'Rasch') 
summary(onePL)
#F1     h2
#explain_hs           0.0623
#quiet_ha             0.0623
#support_hs           0.0623
#feedback_ha          0.0623
#participation_ha     0.0623
#taskhelp_ha          0.0623
#pretunderstand_ha    0.0623
#clarification_ha     0.0623
#workown_ha           0.0623
# 
# SS loadings:  0
# Proportion Var:  0

coef(onePL)
# $explain_hs
# a1     d g u
# par  1 1.145 0 1
# 
# $quiet_ha
# a1     d g u
# par  1 0.316 0 1
# 
# $support_hs
# a1     d g u
# par  1 1.442 0 1
# 
# $feedback_ha
# a1      d g u
# par  1 -1.442 0 1
# 
# $participation_ha
# a1    d g u
# par  1 -1.8 0 1
# 
# $taskhelp_ha
# a1      d g u
# par  1 -1.442 0 1
# 
# $pretunderstand_ha
# a1      d g u
# par  1 -0.647 0 1
# 
# $clarification_ha
# a1      d g u
# par  1 -1.288 0 1
# 
# $workown_ha
# a1      d g u
# par  1 -0.424 0 1
# 
# $GroupPars
# MEAN_1 COV_11
# par      0  0.193

plot(onePL, type = 'trace', facet_items=FALSE)

# Model items with a 2PL IRT model
twoPL <- mirt(dfitems2,1, '2PL')
 summary(twoPL)
# F1      h2
# explain_hs        -0.7131 0.50849
# quiet_ha           0.6557 0.42998
# support_hs         0.0366 0.00134
# feedback_ha        0.0606 0.00367
# participation_ha   0.4180 0.17469
# taskhelp_ha        0.3659 0.13390
# pretunderstand_ha  0.9131 0.83369
# clarification_ha   0.8360 0.69891
# workown_ha         0.2830 0.08010
# 
# SS loadings:  2.865 
# Proportion Var:  0.318 

coef(twoPL)
# #$explain_hs
# a1     d g u
# par -1.731 1.655 0 1
# 
# $quiet_ha
# a1     d g u
# par 1.478 0.413 0 1
# 
# $support_hs
# a1     d g u
# par 0.062 1.387 0 1
# 
# $feedback_ha
# a1      d g u
# par 0.103 -1.389 0 1
# 
# $participation_ha
# a1      d g u
# par 0.783 -1.933 0 1
# 
# $taskhelp_ha
# a1      d g u
# par 0.669 -1.512 0 1
# 
# $pretunderstand_ha
# a1      d g u
# par 3.811 -1.715 0 1
# 
# $clarification_ha
# a1      d g u
# par 2.593 -2.383 0 1
# 
# $workown_ha
# a1     d g u
# par 0.502 -0.43 0 1
# 
# $GroupPars
# MEAN_1 COV_11
# par      0      1 

plot(twoPL, type = 'trace', facet_items=FALSE)

###############################################################
# Part 2: EFA-type IRT
###############################################################

# The EFA-type IRT takes the number of factors and the model type
# as input. We use a graded response model, an extension of 2-PL
# for ordered polytomous categories (Likert data).

# One factor solution
model1.1f <- mirt(dfitems, model = 1, 'graded') 
summary(model1.1f)
# F1    h2
# explain_hs        -0.725 0.526
# quiet_ha           0.827 0.684
# support_hs        -0.379 0.144
# feedback_ha        0.477 0.227
# participation_ha   0.480 0.231
# taskhelp_ha        0.540 0.292
# pretunderstand_ha  0.551 0.303
# clarification_ha   0.610 0.372
# workown_ha         0.365 0.133
# 
# SS loadings:  2.913 
# Proportion Var:  0.324 

# Plot the item information functions
# (amount of information provided by the item for given ability theta)
itemplot(model1.1f, 1,type='info')
# -> most informative medium ability
itemplot(model1.1f, 2,type='info')
# -> informative for medium ability
itemplot(model1.1f, 3,type='info')
# -> most informative for large range of low to high, more around medium
itemplot(model1.1f, 4,type='info')
# -> most informative for very medium to higher ability
itemplot(model1.1f, 5,type='info')
# -> most informative for medium ability
itemplot(model1.1f, 6,type='info')
# -> informative a medium ability
itemplot(model1.1f, 7,type='info')
# -> informative a medium ability
itemplot(model1.1f, 8,type='info')
# -> informative a medium ability
itemplot(model1.1f, 9,type='info')
# -> informative a large range of medium ability

# Two factor solution
model1.2f <- mirt(dfitems, model = 2, 'graded') 
summary(model1.2f)
# Rotation:  oblimin 
# Rotated factor loadings: 
#   
#                      F1      F2    h2
# explain_hs        -0.1919 -0.6206 0.513
# quiet_ha          -0.0377  0.9755 0.925
# support_hs        -0.5449 -0.0715 0.332
# feedback_ha        0.5001  0.2056 0.371
# participation_ha   0.2542  0.3125 0.223
# taskhelp_ha        0.9573 -0.0195 0.903
# pretunderstand_ha -0.1028  0.6154 0.341
# clarification_ha   0.1165  0.5309 0.343
# workown_ha         0.6122 -0.0580 0.351
# 
# Rotated SS loadings:  1.965 2.146 
# 
# Factor correlations: 
#   
#   F1 F2
# F1 1.000   
# F2 0.382  1

# Look at model comparison
anova(model1.1f, model1.2f)
# AIC    SABIC       HQ      BIC   logLik     X2 df     p
# model1.1f 1156.152 1081.252 1187.906 1243.974 -526.076                
# model1.2f 1147.525 1061.102 1184.164 1248.858 -513.763 24.627  8 0.002

# Rotation may be performed posthoc
summary(model1.2f, rotate='varimax')
summary(model1.2f, rotate='oblimin')

# Consider reduced model: remove 3,4,5
#UPDATE NEEDED: based on descriptive statistics, which items would be best to remove?
dfitems_filtered <- dfitems[-c(3, 4, 5)]
model1.1f.red <- mirt(dfitems_filtered, model = 1, 'graded') 
summary(model1.1f.red)
# F1     h2
# explain_hs        -0.742 0.5506
# quiet_ha           0.869 0.7555
# taskhelp_ha        0.414 0.1715
# pretunderstand_ha  0.611 0.3735
# clarification_ha   0.611 0.3734
# workown_ha         0.281 0.0791


# Let's have a closer look at a bad item
# Item 1: I concentrate on the parts of the text that are easy to understand.
# 1-factor IRT plot (2D) of item 3
itemplot(model1.1f, 3)
# -> very flat curves
# 2-factor IRT plot (3D) of item 3
itemplot(model1.2f, 3)
# -> looks better when looking at axis theta_2
# Let's have a closer look at a reasonably good item
# Item 3: After reading the text, I discuss its content with other people. 
# 1-factor IRT plot (2D)  of item 1
itemplot(model1.1f, 1)
# -> much more discrimination, especially for extreme answers
# 1-factor IRT plot (2D)  of item 3 in reduced scale (now item 1)
itemplot(model1.1f.red, 1)
# curves slightly sharper than before
# 2-factor IRT plot (3D) of item 3
itemplot(model1.2f, 3)
# -> cut through axis theta_1 very similar to 1-factor result

# Calculate the factor score estimates
EAPscores <- fscores(model1.1f.red)
# Plot them as histogram
hist(EAPscores)
# -> values between -3 and 2, most close to zero
# Compare factor score estimate and average test score per person
testscore <- apply(dat1[,y1nom[3:6]], 1, mean)
plot(testscore, EAPscores)
# Calculate the correlation
cor(testscore, EAPscores)
# 0.9801343 -> very high

# Print the graded response model coefficients
# a1: discrimination parameter
# d1, d2, d3, d4, d5: difficulty parameters (points on the latent scale 
# where a respondent has a 50% chance of picking the next highest score)
coef(model1.1f.red)
# $ST164Q03IA
#     a1   d1    d2    d3     d4     d5
# par 1.283 2.92 1.813 0.625 -0.442 -1.847
# 
# $ST164Q04IA
#     a1    d1    d2    d3    d4     d5
# par 1.551 3.765 2.705 1.943 0.812 -0.597
# 
# $ST164Q05IA
#     a1    d1    d2    d3     d4     d5
# par 1.885 4.849 3.138 1.301 -0.307 -1.443
# 
# $ST164Q06IA
#     a1    d1   d2     d3     d4     d5
# par 0.973 1.321 0.08 -0.756 -2.111 -3.764

###############################################################
# Part 3: CFA-type IRT
###############################################################

# Define the reduced 1-factor cfa model
model1 <- '
f1 =~ y3 + y4 + y5 + y6
'
# Run the reduced cfa model
cfa1 <- cfa(model1, dat2, ,std.lv=T)
# Calculate the factor score estimates
factorscores <- lavPredict(cfa1)
# Compare them with the previously estimated factor scores
plot(factorscores, EAPscores)
# Calculate the correlation
cor(factorscores, EAPscores)
# 0.9889781 -> very high

# Define the 2-factor cfa model 
irt.model1 <- mirt.model('F1 = 1-2
                     F2 = 3-6
                     COV = F1*F2')
model1.2f.cfa <- mirt(dfitems, irt.model1, 'graded')
summary(model1.2f.cfa)
#            F1    F2    h2
# ST164Q01IA 0.903 0.000 0.816
# ST164Q02IA 0.387 0.000 0.149
# ST164Q03IA 0.000 0.605 0.365
# ST164Q04IA 0.000 0.674 0.454
# ST164Q05IA 0.000 0.740 0.547
# ST164Q06IA 0.000 0.498 0.248
# 
# SS loadings:  0.966 1.615 
# Proportion Var:  0.161 0.269 

# 2-factor IRT plot (3D) of item 1
itemplot(model1.2f.cfa, 1)

# Check the categories with a nominal model
model1.2f.cfa2 <- mirt(dfitems, irt.model1, 'nominal')
summary(model1.2f.cfa2)
#            F1    F2     h2
# ST164Q01IA 0.577 0.000 0.3324
# ST164Q02IA 0.878 0.000 0.7708
# ST164Q03IA 0.000 0.217 0.0470
# ST164Q04IA 0.000 0.398 0.1586
# ST164Q05IA 0.000 0.851 0.7248
# ST164Q06IA 0.000 0.177 0.0312
# 
# SS loadings:  1.103 0.962 
# Proportion Var:  0.184 0.16 

# 2-factor IRT plot (3D) of item 1
itemplot(model1.2f.cfa2, 1)
