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
# SESSION 4: Reliability
################################################################
################################################################
################################################################

#set working directory
print(getwd())
 path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
 figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/reliability/"
#path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project"
#figures_path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project\\figures\\reliability\\"
setwd(path)


library(psych)
library(lavaan) #  library for CFA and structural equation modeling (SEM)
# needed for the calculation of omega
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
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
# 0.74      0.74    0.78      0.24 2.8 0.063  2.8 0.77     0.24

# raw_alpha = .74 > .7, acceptable
# -> Cronbach's alpha (raw alpha) is estimated to be 0.74 #CONFIRM WITH SOFIA: whcih is Cronbach's?
# 9 items help avoidance scale has acceptable internal consistency

#if any of the items are dropped, Cronbach's alpha decreases
# Deleting items makes things worse.

###############################################################
# Internal consistency with McDonald's Omega
###############################################################
# we focus on unidimensional constructs. the rest needs
# too much explanation
?omega
# === 1. Test if scale is unidimensional ===
# Calculate omega from efa for scale 1
omegah(dfitems,nfactors = 1) # ignore the warnings
# relevant information: Omega Total = 0.74
# -> equal to Cronbach's alpha, Baseline if we treat scale as unidimensional
# -> effect of different factor loadings (Omega accounts for unequal factor loadings)

# === 2. Test 2-factor model ===
omegah(dfitems,nfactors = 2)
# Omega Total = 0.79 across the two factors
# -> increased as expected according to previous results
# Omega Hierarchical = 0.35 indicates weak general factor


# === 3. CFA-based omega (1-factor) ===
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
#    to overestimate the reliability (is this right)

# === 4. CFA-based omega (2-factor model from EFA) ===
# Calculate omega for the 2-factor model identified in CFA
# PA1: explain_hs, quiet_ha, participation_ha, pretunderstand_ha, clarification_ha
# PA2: support_hs, feedback_ha, taskhelp_ha, workown_ha
model2 <- '
pa1 =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
'
cfa2 <- cfa(model2,dfitems,std.lv=T)
omegaFromSem(cfa2)
# Omega Total  =  0.89 > .70, acceptable
# -> higher than Cronbach's alpha 0.74, higher than 1-factor CFA omega (0.82)

# === 5. CFA-based omega (2-factor model from EFA & reduced FA1: participation_ha removed) ===
model3 <- '
pa1 =~ explain_hs + quiet_ha + pretunderstand_ha + clarification_ha
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
'
cfa3 <- cfa(model3,dfitems,std.lv=T)
omegaFromSem(cfa3)
# Omega Total  =  0.90 > .70, acceptable


###############################################################
# Split half reliability
###############################################################

# The split half function produces a set of different indices
# by sampling repeatedly, using all possible splits for less
# than 16 items
splitHalf(dfitems)
# Maximum split half reliability (lambda 4) =  0.84, good reliability
# Average split half reliability            =  0.73, acceptable
# Minimum split half reliability  (beta)    =  0.45, varies a lot





###############################################################
# Added for Report
###############################################################

###############################################################
# 1. Full Scale Reliability
###############################################################

# Calculate for Cronbach's alpha for full scale 1
alpha(dfitems)
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
# 0.74      0.74    0.78      0.24 2.8 0.063  2.8 0.77     0.24
# raw_alpha = 0.74 > .70 acceptable

# Alpha if item dropped
# There is no improvement in alpha if an item is dropped
# Alpha decrease mostly to 0.69, when item 1 or 2 is dropped
# Alpha stays at the same, when item 3 is dropped

omegah(dfitems,nfactors = 1) # ignore the warnings
# relevant information: Omega Total = 0.74
# -> equal to Cronbach's alpha, Baseline if we treat scale as unidimensional
# -> effect of different factor loadings (Omega accounts for unequal factor loadings)

splitHalf(dfitems)
# Maximum split half reliability (lambda 4) =  0.84, good reliability
# Average split half reliability            =  0.73, acceptable
# Minimum split half reliability  (beta)    =  0.45, varies a lo
# Split-half reliability was acceptable on average (0.73), though the minimum split was low (0.45)



###############################################################
# 2.  Reliability by 2 Factors as suggested by Factor Analysis
###############################################################
# Compute Cronbach’s Alpha, McDonald’s Omega, and split-half reliability
# for each factor with at least 3 items
# FA1: 1,2,5,7,8
# FA2: 3,4,6,9
item_fa1 <- c("explain_hs", "quiet_ha", "participation_ha", "pretunderstand_ha", "clarification_ha")
alpha(dfitems[,item_fa1])
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
#       0.74      0.73    0.71      0.35 2.7 0.064  2.8 0.98     0.37
# raw_alpha = 0.74 > .70 acceptable， same as full scale

# Alpha if item droppeds
# Alpha decrease to 0.66, when item 2 is dropped
# Alpha increase to 0.75, when item 3 is dropped
#                       raw_alpha std.alpha G6(smc) average_r S/N alpha se  var.r
# explain_hs             0.70      0.70    0.65      0.36 2.3    0.076 0.0098
# quiet_ha               0.66      0.65    0.60      0.32 1.9    0.087 0.0126
# participation_ha       0.75      0.75    0.70      0.43 3.0    0.065 0.0046
# pretunderstand_ha      0.67      0.66    0.64      0.33 2.0    0.083 0.0263
# clarification_ha       0.67      0.66    0.63      0.33 1.9    0.084 0.0241


omegah(dfitems[, item_fa1], nfactors = 1)
# Omega Total = 0.74


splitHalf(dfitems[,item_fa1])
# Maximum split half reliability (lambda 4) =  0.77
# Average split half reliability            =  0.7
# Minimum split half reliability  (beta)    =  0.61


item_fa2 <- c("support_hs", "feedback_ha", "taskhelp_ha", "workown_ha")
alpha(dfitems[,item_fa2])
#  raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
#       0.68      0.68    0.63      0.35 2.1 0.083  2.7 0.91     0.32
# raw_alpha = 0.68 < .70 -> slightly below acceptable

# Reliability if an item is dropped:
#             raw_alpha std.alpha G6(smc) average_r S/N alpha se   var.r med.r
# support_hs       0.66      0.67    0.58      0.40 2.0    0.094 0.00951  0.44
# feedback_ha      0.63      0.64    0.55      0.37 1.8    0.099 0.00905  0.36
# taskhelp_ha      0.52      0.53    0.43      0.27 1.1    0.128 0.00042  0.28
# workown_ha       0.62      0.62    0.53      0.35 1.6    0.105 0.00927  0.36
# -> remove any item in this subscale will decrease Cronbach's alpha

omegah(dfitems[, item_fa2], nfactors = 1)
# Omega Total = 0.69
# close to Cronbach's alpha


splitHalf(dfitems[,item_fa2])
# Maximum split half reliability (lambda 4) =  0.7
# Average split half reliability            =  0.68
# Minimum split half reliability  (beta)    =  0.67
# -> more stable than that of FA1
# -> internal consistency??


###############################################################
# 3.  Reliability by Self-created vs LLM-generated
###############################################################
# Compute Cronbach’s Alpha, McDonald’s Omega, and split-half reliability
# for self-created (1-5) and LLM-generated (6-9) subscales.
self_created <- c("explain_hs", "quiet_ha", "support_hs", "feedback_ha", "participation_ha")
alpha(dfitems[,self_created])
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd median_r
#        0.6       0.6    0.57      0.23 1.5 0.098  2.8 0.8     0.19
# raw_alpha = 0.6 < .70, not acceptable

# Reliability if an item is dropped:
#                  raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r
# explain_hs            0.52      0.53    0.46      0.22 1.11     0.12 0.0023
# quiet_ha              0.46      0.46    0.40      0.18 0.87     0.14 0.0026
# support_hs            0.58      0.57    0.54      0.25 1.35     0.11 0.0222
# feedback_ha           0.55      0.53    0.51      0.22 1.15     0.11 0.0234
# participation_ha      0.60      0.59    0.56      0.27 1.45     0.10 0.0194
# -> dropping item 5 doesn't change Cronbach's alpha
# -> no item dropping improves Cronbach's alpha

omegah(dfitems[,self_created], nfactors = 1)
# Omega Total = 0.61, close to Cronbach's alpha

splitHalf(dfitems[,self_created])
# Maximum split half reliability (lambda 4) =  0.67
# Average split half reliability            =  0.57
# Minimum split half reliability  (beta)    =  0.44 -> decrease, a bit instability


llm_generated <- c("taskhelp_ha", "pretunderstand_ha", "clarification_ha", "workown_ha")
alpha(dfitems[,llm_generated])
# raw_alpha std.alpha G6(smc) average_r S/N  ase mean   sd median_r
#       0.52      0.53    0.56      0.22 1.1 0.13  2.8 0.91      0.2
# raw_alpha = 0.52 < .70, not acceptable

# Reliability if an item is dropped:
#                   raw_alpha std.alpha G6(smc) average_r  S/N alpha se var.r
# taskhelp_ha            0.43      0.43    0.39      0.20 0.75     0.16 0.043
# pretunderstand_ha      0.44      0.47    0.44      0.23 0.89     0.16 0.056
# clarification_ha       0.46      0.47    0.43      0.23 0.89     0.15 0.049
# workown_ha             0.48      0.46    0.41      0.22 0.86     0.14 0.037
# -> dropping any item decreases Cronbach's alpha

omegah(dfitems[,llm_generated], nfactors = 1)
# Omega Total = 0.55, close to Cronbach's alpha??


splitHalf(dfitems[,llm_generated])
# Maximum split half reliability (lambda 4) =  0.78
# Average split half reliability            =  0.53
# Minimum split half reliability  (beta)    =  0.26 -> instability

# The two-factor model suggested by EFA/CFA appears to provide better reliability.
# Because FA1 (indicated by alpha?) has high internal consistency
# and FA2 (indicated by splitHalf...) is relatively more stable compared with other.

###############################################################
# 4.  Reliability by 2 Factors (reduced FA1: participation_ha removed)
###############################################################
# Reduced FA1 (remove participation_ha)
item_fa1_re <- c("explain_hs", "quiet_ha",
                 "pretunderstand_ha", "clarification_ha")

# FA1_re reliability
alpha(dfitems[, item_fa1_re])
# raw_alpha std.alpha G6(smc) average_r S/N   ase mean  sd median_r
# 0.75      0.75     0.7      0.43   3 0.065    3 1.1     0.42
# raw_alpha = 0.75 > .70 acceptable，increase 0.1 compared with the one including item5

# Alpha if item droppeds
# Alpha decrease to 0.65, when item 2 (quite_ha) is dropped
#                   raw_alpha std.alpha G6(smc) average_r S/N alpha se  var.r med.r
# explain_hs             0.70      0.70    0.61      0.44 2.3    0.082 0.0013  0.42
# quiet_ha               0.65      0.65    0.55      0.38 1.8    0.096 0.0017  0.39
# pretunderstand_ha      0.71      0.71    0.63      0.45 2.4    0.080 0.0102  0.48
# clarification_ha       0.71      0.71    0.62      0.45 2.4    0.081 0.0050  0.42

omegah(dfitems[, item_fa1_re], nfactors = 1)
# Omega Total = 0.75, increase 0.01 compared with original fa1

splitHalf(dfitems[, item_fa1_re])
# Maximum split half reliability (lambda 4) =  0.79, increased 0.02
# Average split half reliability            =  0.75, increased 0.05
# Minimum split half reliability  (beta)    =  0.71, increased 0.1

