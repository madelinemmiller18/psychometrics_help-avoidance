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
# SESSION 5: IRT
################################################################
################################################################
################################################################

#set working directory
print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/irt/"
setwd(path)
#setwd("E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project")


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
    pretunderstand_ha = "I.pretend.to.understand.group.project.discussions.even.when.I.feel.confused..",
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
# F1    h2
# explain_hs           0.264
# quiet_ha             0.264
# support_hs           0.264
# feedback_ha          0.264
# participation_ha     0.264
# taskhelp_ha          0.264
# pretunderstand_ha    0.264
# clarification_ha     0.264
# workown_ha           0.264
# 
# SS loadings:  0 
# Proportion Var:  0 

coef(onePL)
# $explain_hs
# a1      d g u
# par  1 -1.325 0 1
# 
# $quiet_ha
# a1     d g u
# par  1 0.368 0 1
# 
# $support_hs
# a1      d g u
# par  1 -1.662 0 1
# 
# $feedback_ha
# a1      d g u
# par  1 -1.662 0 1
# 
# $participation_ha
# a1      d g u
# par  1 -2.061 0 1
# 
# $taskhelp_ha
# a1      d g u
# par  1 -1.662 0 1
# 
# $pretunderstand_ha
# a1      d g u
# par  1 -0.753 0 1
# 
# $clarification_ha
# a1      d g u
# par  1 -1.487 0 1
# 
# $workown_ha
# a1      d g u
# par  1 -0.494 0 1
# 
# $GroupPars
# MEAN_1 COV_11
# par      0   1.04
pdf(paste0(figures_path,"onePL.pdf"), width = 7, height = 5)
plot(onePL, type = 'trace', facet_items=FALSE)
dev.off()

# Model items with a 2PL IRT model
twoPL <- mirt(dfitems2,1, '2PL')
 summary(twoPL)
 #                     F1      h2
 # explain_hs         0.7131 0.50848
 # quiet_ha           0.6557 0.42998
 # support_hs        -0.0366 0.00134
 # feedback_ha        0.0606 0.00367
 # participation_ha   0.4180 0.17469
 # taskhelp_ha        0.3659 0.13390
 # pretunderstand_ha  0.9131 0.83370
 # clarification_ha   0.8360 0.69891
 # workown_ha         0.2830 0.08010
 # 
 # SS loadings:  2.865 
 # Proportion Var:  0.318 

coef(twoPL)
# $explain_hs
# a1      d g u
# par 1.731 -1.655 0 1
# 
# $quiet_ha
# a1     d g u
# par 1.478 0.413 0 1
# 
# $support_hs
# a1      d g u
# par -0.062 -1.387 0 1
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

pdf(paste0(figures_path,"twoPL.pdf"), width = 7, height = 5)
plot(twoPL, type = 'trace', facet_items=FALSE)
dev.off()

# Based on CFA results, test if PA2 factor items work better as a separate scale
# PA2 items: support_hs, feedback_ha, taskhelp_ha, workown_ha
pa2_items <- c("support_hs", "feedback_ha", "taskhelp_ha", "workown_ha")
dfitems2_pa2 <- dfitems2[, pa2_items]

# Model PA2 items with Rasch (1PL) IRT model
onePL.red <- mirt(dfitems2_pa2, 1, 'Rasch')
summary(onePL.red)
coef(onePL.red)
pdf(paste0(figures_path,"onePL_PA2items.pdf"), width = 7, height = 5)
plot(onePL.red, type = 'trace', facet_items=FALSE)
dev.off()

# with a 2PL IRT model
twoPL.red <- mirt(dfitems2_pa2, 1, '2PL')
summary(twoPL.red)
coef(twoPL.red)
pdf(paste0(figures_path,"twoPL_PA2items.pdf"), width = 7, height = 5)
plot(twoPL.red, type = 'trace', facet_items=FALSE)
dev.off()

# Compare 1PL vs 2PL for PA2 subscale
anova(onePL.red, twoPL.red)
# H0: 1PL model is sufficient (equal discrimination across items)
# H1: 2PL model is better (items have different discrimination)
#               AIC   SABIC      HQ     BIC  logLik    X2 df     p
# onePL.red 184.008 176.806 187.062 192.453 -87.004
# twoPL.red 174.929 163.406 179.814 188.440 -79.464 15.08  3 0.002
# -> p = 0.002 < 0.05
# -> 2PL is significantly better (items discriminate differently)

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
# -> 2 factors are better since AIC and BIC are smaller

# Rotation may be performed posthoc
summary(model1.2f, rotate='varimax')
# Rotated factor loadings:

#                        F1      F2    h2
# explain_hs        -0.3064 -0.6473 0.513
# quiet_ha           0.1490  0.9502 0.925
# support_hs        -0.5477 -0.1783 0.332
# feedback_ha        0.5294  0.3011 0.371
# participation_ha   0.3087  0.3572 0.223
# taskhelp_ha        0.9345  0.1708 0.903
# pretunderstand_ha  0.0166  0.5837 0.341
# clarification_ha   0.2154  0.5443 0.343
# workown_ha         0.5890  0.0646 0.351

summary(model1.2f, rotate='oblimin')
# Rotated factor loadings:

#                        F1      F2    h2
# explain_hs        -0.1919 -0.6205 0.513
# quiet_ha          -0.0377  0.9756 0.925
# support_hs        -0.5449 -0.0715 0.332
# feedback_ha        0.5001  0.2056 0.371
# participation_ha   0.2542  0.3124 0.223
# taskhelp_ha        0.9573 -0.0195 0.903
# pretunderstand_ha -0.1027  0.6154 0.341
# clarification_ha   0.1166  0.5308 0.343
# workown_ha         0.6122 -0.0580 0.351

# Based on 1-factor IRT results above (see lines 292-304):
#   support_hs (item 3): h² = 0.144, very low
#   workown_ha (item 9): h² = 0.133, very low

# Remove items 3 and 9 (support_hs and workown_ha)
dfitems_filtered <- dfitems[, -c(3, 9)]
model1.1f.red <- mirt(dfitems_filtered, model = 1, 'graded')
summary(model1.1f.red)
#                       F1    h2
# explain_hs        -0.711 0.506
# quiet_ha           0.888 0.788
# feedback_ha        0.410 0.168
# participation_ha   0.456 0.208
# taskhelp_ha        0.436 0.190
# pretunderstand_ha  0.585 0.342
# clarification_ha   0.634 0.402

# SS loadings:  2.604
# Proportion Var:  0.372


# Let's have a closer look at a bad item
# 9. I tend to keep working on my own even when collaborating with my group would make the task easier [+]
# 1-factor IRT plot (2D) of item 9
itemplot(model1.1f, 9)
# -> flat curves and low discrimination
# 2-factor IRT plot (3D) of item 9
itemplot(model1.2f, 9)
# -> loads weakly 

# Let's have a closer look at a good item
# 2. If I do not understand, I will be quieter in group project meetings [+]
# 1-factor IRT plot (2D) of item 2
itemplot(model1.1f, 2)
# -> steeper curves, and better discrimination
# 1-factor IRT plot (2D) of item 2 in reduced scale (still item 2)
itemplot(model1.1f.red, 2)
# -> curves slightly sharper after removing weak items
# 2-factor IRT plot (3D) of item 2
itemplot(model1.2f, 2)
# -> loads strongly on F2 


# Calculate the factor score estimates
EAPscores <- fscores(model1.1f.red)
# Plot them as histogram
hist(EAPscores)
# -> roughly centered near 0 with a slight right skew
# Compare factor score estimate and average test score per person
testscore <- apply(dfitems, 1, mean)
plot(testscore, EAPscores)
# Calculate the correlation
cor(testscore, EAPscores)
# 0.7995625 -> high correlation

# Print the graded response model coefficients
# a1: discrimination parameter
# d1, d2, d3, d4, d5: difficulty parameters (points on the latent scale 
# where a respondent has a 50% chance of picking the next highest score)
coef(model1.1f.red)
# $explain_hs
#         a1    d1    d2    d3    d4     d5
# par -1.721 4.105 3.249 1.808 0.613 -2.282

# $quiet_ha
#        a1    d1    d2    d3     d4     d5
# par 3.283 5.446 2.121 0.624 -0.756 -6.035

# $feedback_ha
#        a1    d1     d2     d3     d4
# par 0.766 0.693 -0.254 -1.577 -3.919

# $participation_ha
#        a1    d1     d2    d3     d4
# par 0.873 1.411 -1.122 -1.98 -2.208

# $taskhelp_ha
#        a1    d1     d2     d3    d4     d5
# par 0.823 1.559 -0.143 -1.609 -3.26 -3.973

# $pretunderstand_ha
#        a1    d1    d2     d3     d4     d5
# par 1.227 1.984 0.231 -0.931 -2.119 -3.566

# $clarification_ha
#        a1    d1     d2     d3     d4     d5
# par 1.395 1.353 -0.387 -1.701 -2.401 -3.854

###############################################################
# Part 3: CFA-type IRT
###############################################################

# Define the reduced 1-factor cfa model
# with 7 items (without support_hs and workown_ha)
model1 <- '
  f1 =~ explain_hs + quiet_ha + feedback_ha + participation_ha + 
        taskhelp_ha + pretunderstand_ha + clarification_ha
'

# Run the reduced cfa model
cfa1 <- cfa(model1, dfitems_filtered, std.lv=T)
# Calculate the factor score estimates
factorscores <- lavPredict(cfa1)
# Compare them with the previously estimated factor scores
plot(factorscores, EAPscores)
# Calculate the correlation
cor(factorscores, EAPscores)
# 0.9776204 -> very high
# confirms that CFA and IRT are measuring the same underlying construct

# Define the 2-factor cfa model 
irt.model1 <- mirt.model('
     PA1 = 1, 2, 5, 7, 8      # PA1: explain_hs, quiet_ha, participation_ha, pretunderstand_ha, clarification_ha
     PA2 = 3, 4, 6, 9         # PA2: support_hs, feedback_ha, taskhelp_ha, workown_ha
     COV = PA1*PA2')

model1.2f.cfa <- mirt(dfitems, irt.model1, 'graded')
summary(model1.2f.cfa)
#                     PA1   PA2    h2
# explain_hs        0.721       0.519
# quiet_ha          0.892       0.796
# support_hs              0.599 0.359
# feedback_ha             0.598 0.357
# participation_ha  0.449       0.202
# taskhelp_ha             0.882 0.779
# pretunderstand_ha 0.611       0.373
# clarification_ha  0.616       0.379
# workown_ha              0.611 0.373

# SS loadings:  2.27 1.869
# Proportion Var:  0.252 0.208 -> together 46% variance explained
# -> participation_ha: weak

# 2-factor IRT plot (3D) of item 1
itemplot(model1.2f.cfa, 1)

#================================
# Check the categories with a nominal model
model1.2f.cfa2 <- mirt(dfitems, irt.model1, 'nominal')
summary(model1.2f.cfa2)
#   PA1 PA2    h2
# explain_hs        0.950     0.903
# quiet_ha          0.999     0.998
# support_hs                0 0.000
# feedback_ha               0 0.000
# participation_ha  0.660     0.435
# taskhelp_ha               0 0.000
# pretunderstand_ha 0.829     0.687
# clarification_ha  0.984     0.969
# workown_ha                0 0.000

# SS loadings:  3.992 0
# Proportion Var:  0.444 0

# 2-factor IRT plot (3D) of item 1
itemplot(model1.2f.cfa2, 1)

# Nominal model needs category-specific slopes
# Nominal IRT is the wrong model for Likert items
# -> Graded CFA-IRT model is preferred