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

#split into subgroups for factors
# Item sets
self_items <- c("explain_hs", "quiet_ha", "support_hs", "feedback_ha", "participation_ha")
llm_items  <- c("taskhelp_ha", "pretunderstand_ha", "clarification_ha", "workown_ha")

general_items <- c("explain_hs", "quiet_ha", "participation_ha", "pretunderstand_ha", "clarification_ha")
task_items  <- c("support_hs","feedback_ha","taskhelp_ha","workown_ha")

general <- dfitems[,general_items]
task <- dfitems[,task_items]

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

#general only
genModel <- mirt(general, model = 1, 'graded')
#task only
taskModel <- mirt(task, model = 1, 'graded')

# Look at model comparison between one factor and two factor
anova(oneFactorModel, twoFactorModel)
#p = .002, significant

#rotation
summary(twoFactorModel, rotate='oblimin')
#                       F1      F2    h2
# explain_hs         0.1922  0.6203 0.513. #F2
# quiet_ha          -0.0377  0.9760 0.926. #F2
# support_hs         0.5451  0.0715 0.332. #F1 *
# feedback_ha        0.5003  0.2056 0.371. #F1 *
# participation_ha   0.2544  0.3122 0.223. #F2
# taskhelp_ha        0.9569 -0.0195 0.902. #F1 *
# pretunderstand_ha -0.1025  0.6151 0.341. #F2
# clarification_ha   0.1167  0.5305 0.342. #F2
# workown_ha         0.6125 -0.0583 0.351. #F1 *
# Factor correlations: 
#   F1 F2
# F1 1.000   
# F2 0.381  1

###############################################################
# Part 2: Apply the GRM to the subset of well-performing items. 
# Examine each item using item plots, 
# considering both item characteristic curves and item information.
###############################################################

# Plot the item information functions
# (amount of information provided by the item for given ability theta)

# Highest difficulty: 2 (.54), 9 (.41)
itemplot(genModel, 2,type='info')
itemplot(genModel, 2)

itemplot(taskModel, 4,type='info')
itemplot(taskModel, 4)


# Lowest difficulty: 4, 5 (.26)
#4
itemplot(taskModel, 2,type='info')
itemplot(taskModel, 2)
#5 
itemplot(genModel, 3,type='info')
itemplot(genModel, 3)


#Higher Variance: items 8, 9 (>=2.20)
#8
itemplot(genModel, 5,type='info')
itemplot(oneFactorModel, 8)
#9
itemplot(taskModel, 4,type='info')
itemplot(taskModel, 4)

#worse scale correlation: item 5
itemplot(genModel, 3,type='info')
itemplot(genModel, 3)

########### Self
itemplot(genModel, 1,type='info')
itemplot(twoFactorModel, 1, type='info')
itemplot(twoFactorModel, 1,rotate='oblimin')
# -> most informative for
itemplot(genModel, 2,type='info')
itemplot(twoFactorModel, 2, type='info')
itemplot(twoFactorModel, 2,rotate='oblimin')
# -> most informative for
itemplot(taskModel, 1,type='info')
itemplot(taskModel, 1)
itemplot(twoFactorModel, 3,type='info',rotate='oblimin')
itemplot(twoFactorModel, 3,rotate='oblimin')
# -> most informative for
itemplot(taskModel, 2,type='info')
itemplot(taskModel, 2)
itemplot(oneFactorModel, 4)
# -> most informative for
itemplot(genModel, 3,type='info')
itemplot(oneFactorModel, 5)

########### LLM
# -> most informative for very medium ability
itemplot(taskModel, 3,type='info')
itemplot(oneFactorModel, 6)
# -> most informative for
itemplot(genModel, 4,type='info')
itemplot(oneFactorModel, 7)
# -> most informative for
itemplot(genModel, 5,type='info')
itemplot(oneFactorModel, 8)
# -> most informative for
itemplot(taskModel, 4,type='info')
itemplot(oneFactorModel, 9)
# -> most informative for

# Define the 2-factor cfa model
GenTask<- mirt.model('
     PA1 = 1, 2, 5, 7, 8      # PA1: explain_hs, quiet_ha, participation_ha, pretunderstand_ha, clarification_ha
     PA2 = 3, 4, 6, 9         # PA2: support_hs, feedback_ha, taskhelp_ha, workown_ha
     COV = PA1*PA2')

GenTaskCFA <- mirt(dfitems, irt.model1, 'graded')



# Calculate EAP factor scores
EAPscores <- fscores(GenTaskCFA, method = 'EAP')
# EAPscores is a matrix with two columns (PA1 and PA2)
colnames(EAPscores)  # check factor names
# Plot histograms separately
hist(EAPscores[, "F1"], main="General Factor Scores", xlab="EAP Score")
hist(EAPscores[, "F2"], main="Task Factor Scores", xlab="EAP Score")


# Calculate the factor score estimates
EAPscores <- fscores(twoFactorModel)
# Plot them as histogram
hist(EAPscores)
# -> values between -3 and 2, most close to zero
# Compare factor score estimate and average test score per person
testscore <- apply(dat1[,y1nom[3:6]], 1, mean)
plot(testscore, EAPscores)
# Calculate the correlation
cor(testscore, EAPscores)
# 0.9801343 -> very high

###############################################################
# Part 4: Compare self-created and LLM-generated items in terms of 
#IRT parameters (difficulty, discrimination) and item information. 
###############################################################

coef <- coef(oneFactorModel, IRTpars = TRUE, simplify = TRUE)$items

coef[self_items, ]
#                       a         b1         b2         b3        b4       b5
# explain_hs       1.7915344 -1.2803470  0.3492198  0.9999901 1.8099536 2.307154
# quiet_ha         2.5059875 -1.7629366 -0.6742703 -0.1932783 0.2420704 1.960497
# support_hs       0.6979437 -2.9632289 -0.3596673  2.2498399 3.4654230 5.601548
# feedback_ha      0.9232720 -0.7784672  0.2959237  1.7926589 4.3706300       NA
# participation_ha 0.9321358 -1.5271576  1.2223877  2.1472479 2.3946904       NA
coef[llm_items, ]
#                       a        b1         b2        b3       b4       b5
# taskhelp_ha       1.0930589 -1.543087  0.1275698 1.5706410 3.153114 3.816146
# pretunderstand_ha 1.1232138 -1.717369 -0.2152491 0.7668479 1.789772 3.070443
# clarification_ha  1.3113298 -1.003087  0.2972739 1.2891363 1.802129 2.871991
# workown_ha        0.6669778 -2.507638 -0.4759231 0.6758068 2.296755 4.748062

#The a column is discrimination. 
#Higher values (roughly: >1.5 = strong, 0.5–1.5 = moderate, <0.5 = weak) 
#mean the item does a better job distinguishing between people at different trait levels.

#Difficulty: b items
#For a 1–6 Likert item you get up to 5 thresholds — 
#each one is the point on the theta scale where a respondent has a 50% chance of scoring above that category boundary. 
#Negative values mean the threshold is easy to cross (endorsed even at low trait levels); 
#positive values mean the threshold is hard (only endorsed at high trait levels).

#information


