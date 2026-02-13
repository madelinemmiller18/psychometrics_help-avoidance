# 1. I ask my group members to explain concepts that confuse me [-]
# 2. If I do not understand, I will be quieter in group project meetings [+]
# 3. If I am struggling with my project contributions, I seek support from my group members [-]
# 4. When I am unsure about my work, I am reluctant to seek feedback from my group [+]
# 5. If a project is too hard for me, I participate less rather than ask my group for help [+]
# 6. I avoid asking my group members for help even when I find my task difficult. [+]
# 7. I pretend to understand group project discussions even when I feel confused. [+]
# 8. I hesitate to ask my teammates for clarification during group projects. [+]
# 9. I tend to keep working on my own even when collaborating with my group would make the task easier [+]

################################################################
################################################################
################################################################
# IRT based on paper instructions
################################################################
################################################################
################################################################

#set working directory
print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/irt/"
#path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project"
#figures_path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project\\figures\\irt\\"
setwd(path)

library(psych)
library(mirt) # library for multidimensional item response theory
library(lavaan) #  library for CFA and structural equation modeling (SEM)
library(dplyr)

###############################################################
# set up data
###############################################################
# upload data
rawdata <- read.csv("data/Help Avoidance in Group Projects.csv")
df <- rawdata[c(3:14)]

# change column names
df <- df %>%
  rename(
    explain_hs = "I.ask.my.group.members.to.explain.concepts.that.confuse.me.",
    quiet_ha = "If.I.do.not.understand..I.will.be.quieter.in.group.project.meetings.",
    support_hs = "If.I.am.struggling.with.my.project.contributions..I.seek.support.from.my.group.members.",
    feedback_ha = "When.I.am.unsure.about.my.work..I.am.reluctant.to.seek.feedback.from.my.group.",
    participation_ha = "If.a.project.is.too.hard.for.me..I.participate.less.rather.than.ask.my.group.for.help.",
    taskhelp_ha = "I.avoid.asking.my.group.members.for.help.even.when.I.find.my.task.difficult.",
    pretunderstand_ha = "I.pretend.to.understand.group.project.discussions.even.when.I.feel.confused.",
    clarification_ha = "I.hesitate.to.ask.my.teammates.for.clarification.during.group.projects.",
    workown_ha = "I.tend.to.keep.working.on.my.own.even.when.collaborating.with.my.group.would.make.the.task.easier.",
    degree_level = "What.is.your.current.student.status.",
    fos = "Which.field.best.describes.your.area.of.study.",
    langlevel = "Self.rate.your.skills.in.the.language.typically.used.for.your.group.projects..e.g...German.or.English.."
  )


#degree level numerical encoding
# (1 = Bachelor/MD/Medical, 2 = Master, 3 = PhD)
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
df$langlevel[df$langlevel == "Less than adequate"]<-1
df$langlevel[df$langlevel == "Adequate"] <- 2
df$langlevel[df$langlevel == "Good"]<-3
df$langlevel[df$langlevel == "Excellent"]<-4
df$langlevel[df$langlevel == "Native Speaker"]<-5

#fos numerical encoding
unique(df$fos) #return unique values
df$fos[df$fos=="Economics and Social Sciences"]<-1
df$fos[df$fos=="Theology"]<-2
df$fos[df$fos=="Science"]<-3
df$fos[df$fos=="Humanities"]<-4
df$fos[df$fos=="Medicine"]<-5
#assign data science to economics and social sciences
df$fos[df$fos=="Data Science"]<-1
#assign rhetorics, philospophy to humanities
df$fos[df$fos=="Rhetorics, philosophy "]<-4

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

#explain_hs and support_hs should be reversed (negative).
# So, we re-code by subtracting the response value from
# the maximum score plus 1 (which is 7 in this case)
dfitems[,"explain_hs"] <- 7-dfitems[,"explain_hs"]
dfitems[,"support_hs"] <- 7-dfitems[,"support_hs"]


###############################################################
# Part 1: Apply GRM to all items and perform EFA
# Compare the IRT results to the previous EFA: 
# Are the factor structure and item performance similar or different?
###############################################################
# One factor solution
oneFactorModel <- mirt(dfitems, model = 1, 'graded')
summary(oneFactorModel)
coef(oneFactorModel)

#two factor solution
twoFactorModel <- mirt(dfitems, model = 2, 'graded')
summary(twoFactorModel)
coef(twoFactorModel)

# Look at model comparison
anova(oneFactorModel, twoFactorModel)

#rotation
summary(twoFactorModel, rotate='oblimin')

###############################################################
# Part 2: Apply the GRM to the subset of well-performing items. 
# Examine each item using item plots, 
# considering both item characteristic curves and item information.
###############################################################

# Plot the item information functions
# (amount of information provided by the item for given ability theta)
itemplot(oneFactorModel, 1,type='info')
itemplot(oneFactorModel, 1)
itemplot(twoFactorModel, 1,rotate='oblimin')
# -> most informative for very low ability
itemplot(oneFactorModel, 2,type='info')
itemplot(oneFactorModel, 2)
# -> informative a large range of low to medium ability
itemplot(oneFactorModel, 3,type='info')
itemplot(oneFactorModel, 3)
# -> most informative for very medium ability
itemplot(oneFactorModel, 4,type='info')
itemplot(oneFactorModel, 4)
# -> most informative for very medium ability
itemplot(oneFactorModel, 5,type='info')
itemplot(oneFactorModel, 5)
# -> most informative for very medium ability
itemplot(oneFactorModel, 6,type='info')
itemplot(oneFactorModel, 6)
# -> informative a large range of medium to high ability
itemplot(oneFactorModel, 7,type='info')
itemplot(oneFactorModel, 7)
# -> informative a large range of medium ability
itemplot(oneFactorModel, 8,type='info')
itemplot(oneFactorModel, 8)
# -> informative a range of medium ability
itemplot(oneFactorModel, 9,type='info')
itemplot(oneFactorModel, 9)
# -> informative a large range of medium to high ability

###############################################################
# Part 4: Compare self-created and LLM-generated items in terms of 
#IRT parameters (difficulty, discrimination) and item information. 
###############################################################
coef(oneFactorModel, IRTpars = TRUE, simplify = TRUE)$items
#The a column is discrimination. 
#Higher values (roughly: >1.5 = strong, 0.5–1.5 = moderate, <0.5 = weak) 
#mean the item does a better job distinguishing between people at different trait levels.

#Difficulty: b items
#For a 1–6 Likert item you get up to 5 thresholds — 
#each one is the point on the theta scale where a respondent has a 50% chance of scoring above that category boundary. 
#Negative values mean the threshold is easy to cross (endorsed even at low trait levels); 
#positive values mean the threshold is hard (only endorsed at high trait levels).
