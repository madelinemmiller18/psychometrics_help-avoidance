#set working directory
print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
setwd(path)

library(psych)
library(lavaan) #  library for CFA and structural equation modeling (SEM)
# needed for the calculation of omega
library(dplyr)

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
dfitems <- df[,item_names]
#correlation matrix
round(cor(dfitems), 2)
#explain_hs and support_hs should be reversed (negative).
# So, we re-code by subtracting the response value from 
# the maximum score plus 1 (which is 7 in this case)
dfitems[,"explain_hs"] <- 7-dfitems[,"explain_hs"]
dfitems[,"support_hs"] <- 7-dfitems[,"support_hs"]

###############################################################
# Internal consistency with Cronbach's alpha
###############################################################

# Calculate for Cronbach's alpha for full scale 1
alpha(dfitems)
# raw_alpha = .74 -> Cronbach's alpha is estimated to be 0.74, acceptable
#if any of the items are dropped, Cronbach's alpha decreases
# Deleting items makes things worse.

###############################################################
# Internal consistency with McDonald's Omega
###############################################################
# we focus on unidimensional constructs. the rest needs 
# too much explanation
?omega

# Calculate omega from efa for scale 1
omegah(dfitems,nfactors = 1) # ignore the warnings
# relevant information: Omega Total = 0.74 
# -> equal to Cronbach's alpha 
# -> effect of different factor loadings
omegah(dfitems,nfactors = 2)
# Omega Total = 0.79 across the two factors
# -> increased as expected according to previous results

# Calculate omega from cfa (see last week) for scale 1 
# Define the one factor model
model1 <- '
f1 =~ explain_hs + quiet_ha + support_hs + feedback_ha + participation_ha 
  + taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha 
'
# Perform cfa
cfa1 <- cfa(model1,dfitems,std.lv=T)
# Calculate omega based on structural equation model (SEM)
omegaFromSem(cfa1) # again ignore the warnings
# Omega Total  from a confirmatory model using sem =  0.82 
# -> this ignores any problems due to misfit, so it is likely
#    to overestimate the reliability

# Define the reduced model without items 1 and 2
model2 <- '
pa1 =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha 
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
'
cfa2 <- cfa(model2,dfitems,std.lv=T)
omegaFromSem(cfa2)
# Omega Total  from a confirmatory model using sem =  0.89 
# -> increased compared to full scale as expected, however
#    likely overestimated due to ignored model misfits

###############################################################
# Split half reliability
###############################################################

# The split half function produces a set of different indices
# by sampling repeatedly, using all possible splits for less 
# than 16 items
splitHalf(dfitems)
# Maximum split half reliability (lambda 4) =  0.84
# Average split half reliability            =  0.73
# Minimum split half reliability  (beta)    =  0.45
