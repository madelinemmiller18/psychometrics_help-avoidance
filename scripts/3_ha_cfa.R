# 1. I ask my group members to explain concepts that confuse me [-]
# 2. If I do not understand, I will be quieter in group project meetings [+]
# 3. If I am struggling with my project contributions, I seek support from my group members [-] efaP2
# 4. When I am unsure about my work, I am reluctant to seek feedback from my group [+] efaP2
# 5. If a project is too hard for me, I participate less rather than ask my group for help [+]
# 6. I avoid asking my group members for help even when I find my task difficult. [+] efaP2
# 7. I pretend to understand group project discussions even when I feel confused. [+]
# 8. I hesitate to ask my teammates for clarification during group projects. [+]
# 9. I tend to keep working on my own even when collaborating with my group would make the task easier [+] efaP2

#PA1: General Understanding Help-Avoidance
# 1. I ask my group members to explain concepts that confuse me [-]
# 2. If I do not understand, I will be quieter in group project meetings [+]
# 5. If a project is too hard for me, I participate less rather than ask my group for help [+]
# 7. I pretend to understand group project discussions even when I feel confused. [+]
# 8. I hesitate to ask my teammates for clarification during group projects. [+]


#PA2: Owned-Task Help-Avoidance ** might be a bifactor
# 3. If I am struggling with my project contributions, I seek support from my group members [-] efaP2 *my project contributions
# 4. When I am unsure about my work, I am reluctant to seek feedback from my group [+] efaP2 *my work
# 6. I avoid asking my group members for help even when I find my task difficult. [+] efaP2 *my task
# 9. I tend to keep working on my own even when collaborating with my group would make the task easier [+] efaP2  *on my own... the task (I am working on)

################################################################
################################################################
################################################################
# SESSION 3: CFA
################################################################
################################################################
################################################################

print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/cfa/"
#path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project"
#figures_path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project\\figures\cfa\\"
setwd(path)


#install libraries
library(psych) # library for factor analysis
library(MVN) # library for multivariate normal distributions
library(GPArotation) # library for rotating factors
library(dplyr)
library(car) #use qqplot function
library(lavaan) # library for CFA and structural equation modeling (SEM)
library(semPlot) # library for visualizing SEM path diagrams
library(tidyr)
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



##########################################################################
# Confirmatory factor analysis (CFA)
##########################################################################
# NOTE: typically one should not use the same data for efa and cfa, but
# cross-validate with different samples because using the results from the
# efa  to define the cfa model may inflate the model fit, creating bias

# one factor model
# All 9 items load on one factor -> unidimensionality
model1 <- '
f1 =~ explain_hs + quiet_ha + support_hs + feedback_ha + participation_ha
  + taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha
'

# two factor model - positively and negatively coded items
# Test if reverse-coded items form a separate factor
pn_coding_model2 <- '
pc =~ quiet_ha + feedback_ha + participation_ha
  + taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha
nc =~ l1*explain_hs + l1*support_hs  # equality constraint
'


# two factor model - human coded items and ChatGPT items # UPDATED to correct ai items (pret and feed)
# Tests if items cluster by who wrote them
ai_model2 <- '
hu =~ explain_hs + quiet_ha + support_hs  + feedback_ha + participation_ha
ai =~ taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha
'

# two factor oblimin_initial_loadings model suggested from EFA
#PA1: General Understanding Help-Avoidance
#PA2: Owned Task Help-Avoidance
model2 <- '
General =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha
OwnTask =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
'

##########################################################################

# run model 1
cfa1 <- cfa(model1,dfitems,std.lv=T)

# output
summary(cfa1,standardized=T,fit=T)

# plot path diagram with standardized factor loadings
pdf(paste0(figures_path,"cfa1_onefactor_sempath.pdf"), width = 7, height = 5)
semPaths(cfa1, "std")
dev.off()
# -> strong loadings for explain_hs (0.66), quiet_ha(0.72) and clarificatin_h (0.59)
# -> moderate: feedback_ha (0.4), taskhelp_ha (0.45), pretundrstnd_h (0.51)
# -> weaker items: support_hs (0.26), participatin_h (0.37), workown_ha  (0.34)


# calculate the residual covariance as standardized z-values
# (check for > +1.96 or < -1.96)
cov1 <- round(resid(cfa1, type="standardized")$cov, 2)
cov1

#                   expln_ quit_h spprt_ fdbck_ prtcp_ tskhl_ prtnd_ clrfc_ wrkwn_
# explain_hs          0.00
# quiet_ha            0.89   0.00
# support_hs         -0.31  -0.06   0.00
# feedback_ha        -0.75   0.21   1.05   0.00
# participation_ha   -1.67  -0.91   0.69   0.30   0.00
# taskhelp_ha        -0.02  -1.64   1.74   1.95   0.60   0.00
# pretunderstand_ha   0.62   0.72  -1.50  -2.73   1.09  -1.90   0.00
# clarification_ha   -0.87   0.85  -1.51   0.02   1.13  -0.46   1.10   0.00
# workown_ha          1.13  -2.15   1.33   1.14  -0.39   2.24   0.13  -1.94  0.00
# -> there is some unwanted residual correlation,
# especially between eg. (pretunderstand_ha & feedback_ha = -2.73) and (taskhelp_ha & feedback_ha = 1.95)...
# -> indicates that the one-factor model does not fit well

##########################################################################

# run model pn (positively and negatively coded items)
cfa_pn <- cfa(pn_coding_model2,dfitems,std.lv=T)

# output
summary(cfa_pn,standardized=T)
# factor correlation = 1.364 -> Impossible, Problematic!!! #CHECK WITH SOFIA?

# plot path diagram with standardized factor loadings
pdf(paste0(figures_path,"cfa_positive-negative-loadings_sempath.pdf"), width = 7, height = 5)
semPaths(cfa_pn, "std")
dev.off()
# pc (7 items): loadings from 0.39 to 0.64
# nc (2 items): weak, 0.38 and 0.37 -> don't form a strong separate factor
# factor correlation: 1.364 -> Impossible, Problematic!!!

# calculate the residual covariance as standardized z-values
# (check for > +1.96 or < -1.96)
cov_pn <- round(resid(cfa_pn,type="standardized")$cov,2)
cov_pn
#                     quit_h fdbck_ prtcp_ tskhl_ prtnd_ clrfc_ wrkwn_ expln_ spprt_
# quiet_ha            0.00
# feedback_ha        -0.01   0.00
# participation_ha   -0.61   0.02   0.00
# taskhelp_ha        -2.15   1.59   0.23   0.00
# pretunderstand_ha   1.48  -2.64   1.22  -1.99   0.00
# clarification_ha    1.55  -0.07   1.19  -0.73   1.62   0.00
# workown_ha         -2.28   0.76  -0.74   1.95   0.10  -2.06   0.00
# explain_hs          1.96  -0.28  -0.80   0.27   1.65   0.76   1.18   1.49
# support_hs         -1.35  -0.07  -0.17   0.41  -2.02  -2.27   0.35  -1.49  -1.49
# diagonal (an item's residual with itself) should be 0
# many residual correlations > |1.96| -> model does not fit well

##########################################################################
# run model ai
cfa_ai <- cfa(ai_model2,dfitems,std.lv=T)

# output
summary(cfa_ai,standardized=T)
# factor correlation = -1.07 -> Impossible, Problematic!!!

# plot path diagram with standardized factor loadings
pdf(paste0(figures_path,"cfa_ai-human-written_sempath.pdf"), width = 7, height = 5)
semPaths(cfa_ai, "std")
dev.off()
# hu: except support_hs (0.28), others are moderate to high
# ai: all items load negatively, weird!


# calculate the residual covariance as standardized z-values
# (check for > +1.96 or < -1.96)
cov_ai <- round(resid(cfa_ai, type="standardized")$cov,2)
cov_ai
#                   expln_ quit_h spprt_ fdbck_ prtcp_ tskhl_ prtnd_ clrfc_ wrkwn_
# explain_hs          0.00
# quiet_ha            1.25   0.00
# support_hs         -0.29   0.00   0.00
# feedback_ha        -0.72   0.29   1.05   0.00
# participation_ha   -1.68  -0.85   0.66   0.25   0.00
# taskhelp_ha        -0.25  -1.87   1.75   1.94   0.45   0.00
# pretunderstand_ha   0.63   0.83  -1.55  -2.79   1.00  -1.90   0.00
# clarification_ha   -0.98   0.94  -1.59  -0.07   1.01  -0.39   1.28   0.00
# workown_ha          1.10  -2.27   1.32   1.11  -0.50   2.21   0.20  -1.94  0.00
# -> also impossible

##########################################################################
# run model 2
cfa2 <- cfa(model2,dfitems,std.lv=T)

# output
summary(cfa2,standardized=T,fit=T)
fitMeasures(cfa2, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
# plot path diagram with standardized factor loadings

# Custom item labels
# Your custom item labels
item_labels <- c(
  "1\nConcepts", 
  "2\nMeetings", 
  "5\nParticpat", 
  "7\nDiscussn", 
  "8\nClarificat",
  "3\nContribut\nSupport", 
  "4\nFeedack\nMy Work", 
  "6\nHelp\nMy Task", 
  "9\nCollabor\nTask"
)

# Latent factor names in the order semPaths expects
latent_names <- c("General", "OwnTask")

# Combine latent + observed labels
node_labels <- c(item_labels,latent_names)

#comment one out if you want a png or pdf returned
pdf(paste0(figures_path,"cfa2_GenOwned.pdf"), width = 7, height = 5)
#png(paste0(figures_path, "cfa2_GenOwned.png"), width = 7, height = 5, units = "in", res = 300)
# 3. Plot with nodeLabels
semPaths(
  cfa2,
  "std",
  nodeLabels = node_labels, #updates to new labels
  nCharNodes = 0,         # show full multi-line labels
  sizeMan = 8, # item size
  sizeLat = 8, #latent variable size
  edge.label.cex = 0.9,  # makes loading numbers bigger/smaller
  node.width = 1, #width of item boxes
  mar = c(4, 4, 4, 4), 
  edge.color = "#000080" #update edge color
)
dev.off()

# -> moderate to high factor loadings for all items

# calculate the residual covariance as standardized z-values
# (check for > +1.96 or < -1.96)
cov_2 <- round(resid(cfa2,type="standardized")$cov,2)
cov_2
#                    expln_ quit_h prtcp_ prtnd_ clrfc_ spprt_ fdbck_ tskhl_ wrkwn_
# explain_hs          0.00
# quiet_ha            0.91   0.00
# participation_ha   -1.44  -1.10   0.00
# pretunderstand_ha   0.18  -0.44   0.99   0.00
# clarification_ha   -1.29   0.09   1.11   0.53   0.00
# support_hs          0.19   0.35   0.85  -1.07  -0.90   0.00
# feedback_ha         0.44   1.10   0.74  -1.66   0.73   0.02   0.00
# taskhelp_ha         0.81  -0.35   0.90  -1.44   0.16  -0.09  -0.05   0.00
# workown_ha          1.45  -0.81  -0.06   0.39  -1.12   0.22  -0.36   0.25   0.00
# item 4 (pretunderstand_ha) and 7 (feedback_ha), -1.66
# -> 2 factor model shows reasonable loading, moderate factor correlation (0.40), and acceptable residuals



##########################################################################
# Till now, only cfa1 and cfa2 possible
##########################################################################
# Model comparison to one-factor model
anova(cfa2,cfa1)
# Chi-Squared Difference Test
#      Df    AIC    BIC  Chisq Chisq diff   RMSEA Df diff Pr(>Chisq)
# cfa2 26 1191.2 1223.3 26.875
# cfa1 27 1207.3 1237.7 44.953     18.079 0.65342       1   2.12e-05 ***
# p < 0.05 -> 2 factor model is significantly better

##########################################################################

# Model tuning: We can consider to include/free other parameters in the model
# (NOTE: Exploratory)
# Create a list with different changes and the expected decrease in the
# chi-square statistic (mi) as measure for the model fit improvement
mi <- modindices(cfa2, sort. = T)
mi
# show all mi > 3.84 (significant)
mi[mi$mi>3.84,]
#                  lhs op         rhs   mi    epc sepc.lv sepc.all sepc.nox
# 54 pretunderstand_ha ~~ feedback_ha 3.96 -0.414  -0.414   -0.355   -0.355
# -> MI=3.96 > 3.84, possible to add a residual correlation between pretunderstand_ha and feedback_ha
# but it is just barely above the threshold 3.84, so keep the original 2 factor model unchanged.



### ADDED, to check based on MI ###
model2_mod <- '
General =~ explain_hs + quiet_ha + participation_ha +
           pretunderstand_ha + clarification_ha
OwnTask =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
pretunderstand_ha ~~ feedback_ha    # Free the residual covariance suggested by MI
'
cfa2_mod <- cfa(model2_mod, dfitems, std.lv = TRUE)
summary(cfa2_mod, standardized = T, fit = T)
# -> TLI = 1.067
# -> keep original model2 without modification

##########################################################################
# Final step: test factor correlation
# Define model with zero correlation
model3 <- '
pa1 =~ explain_hs + quiet_ha + participation_ha + pretunderstand_ha + clarification_ha
pa2 =~ support_hs + feedback_ha + taskhelp_ha + workown_ha
pa1 ~~ 0*pa2         # this constraint sets the correlation to zero
'
# Run the model
cfa3 <- cfa(model3, dfitems, std.lv=TRUE)
# Model comparison to correlated factors model (nested models)
anova(cfa2, cfa3)
#      Df    AIC    BIC  Chisq Chisq diff   RMSEA Df diff Pr(>Chisq)
# cfa2 26 1191.2 1223.3 26.875
# cfa3 27 1193.0 1223.4 30.637     3.7618 0.26276       1    0.05244 .

# p = 0.052 > .05 (marginal) -> no significant loss of fit when fixing correlation to zero
# -> factor correlation is small
# keep correlated model (cfa2) for reporting



##########################################################################
# COMPARISON OF SUBSCALES
# Test unidimensionality of self-created vs LLM items SEPARATELY
##########################################################################

# Define item subsets
self_created <- c("explain_hs", "quiet_ha", "support_hs", "feedback_ha", "participation_ha")
llm_generated <- c("taskhelp_ha", "pretunderstand_ha", "clarification_ha", "workown_ha")

# Create subset datasets
dfitems_self <- dfitems[, self_created]
dfitems_llm <- dfitems[, llm_generated]

##########################################################################
# 1. SELF-CREATED ITEMS: Test unidimensionality
##########################################################################

# One-factor model for self-created items
model_self <- '
  self =~ explain_hs + quiet_ha + support_hs + feedback_ha + participation_ha
'

cfa_self <- cfa(model_self, dfitems_self, std.lv = TRUE)
summary(cfa_self, standardized = T, fit = T)
# forms unidimensionality

##########################################################################
# 2. LLM-GENERATED ITEMS: Test unidimensionality
##########################################################################

# One-factor model for LLM items
model_llm <- '
  llm =~ taskhelp_ha + pretunderstand_ha + clarification_ha + workown_ha
'

cfa_llm <- cfa(model_llm, dfitems_llm, std.lv = TRUE)
summary(cfa_llm, standardized = T, fit = T)
# very poor fit 

#fitmeasures -----------------
fitMeasures(cfa2, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
# Extract fit measures
fit_cfa2 <- fitMeasures(cfa2, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
fit_cfa2_mod <- fitMeasures(cfa2_mod, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
fit_cfa3 <- fitMeasures(cfa3, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
fit_cfa1 <- fitMeasures(cfa1, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
fit_cfa_pn <- fitMeasures(cfa_pn, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))
fit_cfa_ai <- fitMeasures(cfa_ai, c("chisq","df","pvalue","rmsea","srmr","cfi","tli"))

# Combine into data frame
fit_table <- data.frame(
  Model = c("GenOwn_2f", "GenOwn_Mod","GenOwn_Unc","One Factor", "Pos/Neg Coding 2f", "AI vs Human 2f"),
  ChiSq = c(fit_cfa2["chisq"], fit_cfa2_mod["chisq"], fit_cfa3["chisq"], fit_cfa1["chisq"], fit_cfa_pn["chisq"], fit_cfa_ai["chisq"]),
  df = c(fit_cfa2["df"], fit_cfa2_mod["df"], fit_cfa3["df"], fit_cfa1["df"], fit_cfa_pn["df"], fit_cfa_ai["df"]),
  pvalue = c(fit_cfa2["pvalue"], fit_cfa2_mod["pvalue"], fit_cfa3["pvalue"], fit_cfa1["pvalue"], fit_cfa_pn["pvalue"], fit_cfa_ai["pvalue"]),
  RMSEA = c(fit_cfa2["rmsea"], fit_cfa2_mod["rmsea"], fit_cfa3["rmsea"], fit_cfa1["rmsea"], fit_cfa_pn["rmsea"], fit_cfa_ai["rmsea"]),
  SRMR = c(fit_cfa2["srmr"], fit_cfa2_mod["srmr"], fit_cfa3["srmr"], fit_cfa1["srmr"], fit_cfa_pn["srmr"], fit_cfa_ai["srmr"]),
  CFI = c(fit_cfa2["cfi"], fit_cfa2_mod["cfi"], fit_cfa3["cfi"], fit_cfa1["cfi"], fit_cfa_pn["cfi"], fit_cfa_ai["cfi"]),
  TLI = c(fit_cfa2["tli"], fit_cfa2_mod["tli"], fit_cfa3["tli"], fit_cfa1["tli"], fit_cfa_pn["tli"], fit_cfa_ai["tli"])
)

# Round for display
fit_table <- fit_table %>%
  mutate(across(c(ChiSq, RMSEA, SRMR, CFI, TLI), ~round(.x, 3)),
         pvalue = round(pvalue, 4))

print(fit_table)

# Reshape for plotting (exclude chisq, df, pvalue)
fit_long <- fit_table %>%
  select(Model, pvalue, RMSEA, SRMR, CFI, TLI) %>%
  pivot_longer(cols = c(pvalue, RMSEA, SRMR, CFI, TLI), 
               names_to = "Index", 
               values_to = "Value")

# Add thresholds for acceptable fit
thresholds <- data.frame(
  Index = c("pvalue","RMSEA", "SRMR", "CFI", "TLI"),
  Threshold = c(.05, 0.06, 0.08, 0.95, 0.95),
  Direction = c("higher", "lower", "lower", "higher", "higher"),  # lower is better for RMSEA/SRMR
  Label = c("p > .05", "< .06", "< .08", "> .95", "> .95")  
)

# Set factor order to control plot sequence
fit_long$Index <- factor(fit_long$Index, 
                         levels = c("pvalue", "RMSEA", "SRMR", "CFI", "TLI"))
thresholds$Index <- factor(thresholds$Index, 
                           levels = c("pvalue", "RMSEA", "SRMR", "CFI", "TLI"))

fit_long <- fit_long %>%
  left_join(thresholds, by = "Index") %>%
  mutate(Acceptable = case_when(
    Direction == "lower" & Value <= Threshold ~ "Yes",
    Direction == "higher" & Value >= Threshold ~ "Yes",
    TRUE ~ "No"
  ))

# Create plot
p <- ggplot(fit_long, aes(x = Model, y = Value, fill = Acceptable)) +
  geom_col(position = "dodge") +
  geom_hline(data = thresholds, aes(yintercept = Threshold), 
             linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.4) +
  geom_text(data = thresholds, 
            aes(x = 0.4, y = Threshold, label = Label),
            hjust = 0, vjust = -0.5, size = 2.5, fontface = "bold", 
            inherit.aes = FALSE) +
  geom_text(aes(label = sprintf("%.2f", Value)), 
            vjust = -0.5, size = 3.5, fontface = "bold") +
  facet_wrap(~Index, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c("Yes" = "blue", "No" = "orange")) +
  labs(title = "CFA Model Fit Comparison",
       subtitle = "Red dashed lines indicate acceptable fit thresholds",
       x = NULL,
       y = "Fit Index Value") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12)
  )

ggsave(paste0(figures_path, "cfa_fit_comparison.png"),
       p, width = 12, height = 6, dpi = 300)
