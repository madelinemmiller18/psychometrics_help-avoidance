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
# SESSION 1: DESCRIPTIVES
################################################################
################################################################
################################################################

###############################################################
# BASIC
###############################################################

print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/descriptives/"
#path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project"
#figures_path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project\\figures\\descriptives\\"
setwd(path)

#Install libraries
library(foreign) # importing/exporting data from/to different statistical software formats
library(fastDummies) # creating binary indicator variables from categorical variables
library(lubridate) # handling and manipulation of date and time data
library(dplyr)

#upload data
rawdata <- read.csv("data/Help Avoidance in Group Projects.csv")
rawdata$id <- 1:nrow(rawdata) #Add ID column

dim(rawdata) # dimension
head(rawdata) # first entries

# filter to just item data and covariate data
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
    pretunderstand_ha = "I.pretend.to.understand.group.project.discussions.even.when.I.feel.confused.",
    clarification_ha = "I.hesitate.to.ask.my.teammates.for.clarification.during.group.projects.",
    workown_ha = "I.tend.to.keep.working.on.my.own.even.when.collaborating.with.my.group.would.make.the.task.easier.",
    degree_level = "What.is.your.current.student.status.",
    fos = "Which.field.best.describes.your.area.of.study.",
    langlevel = "Self.rate.your.skills.in.the.language.typically.used.for.your.group.projects..e.g...German.or.English.."
  )

# degree level numerical encoding
# (1 = Bachelor/MD/Medical, 2 = Master, 3 = PhD)
# Classify as 1 (Undergraduate or equivalent)
unique(df$degree_level) #return unique values
df$degree_level[df$degree_level == "Undergraduate Student (Bachelor or equivalent)"] <- 1
df$degree_level[df$degree_level  == "MD"] <- 1 #MD = med student, these can be considered undergraduates
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
# use only the item information, not the covariates
dfitems <- df[,item_names]
head(dfitems)

#correlation matrix before reverse coding
round(cor(dfitems), 2)

#explain_hs and support_hs should be reversed (negative).
#this aligns with how questions were written
# So, we re-code by subtracting the response value from
# the maximum score plus 1 = 6 + 1 = 7
dfitems[,"explain_hs"] <- 7-dfitems[,"explain_hs"]
dfitems[,"support_hs"] <- 7-dfitems[,"support_hs"]

#correlation matrix after reverse coding
round(cor(dfitems), 2)

#                    explain_hs  quiet_ha  support_hs  feedback_ha  participation_ha taskhelp_ha pretunderstand_ha clarification_ha workown_ha
# explain_hs              1.00     0.53       0.14        0.20             0.10        0.30              0.39             0.33       0.34
# quiet_ha                0.53     1.00       0.18        0.30             0.20        0.21              0.42             0.48       0.08
# support_hs              0.14     0.18       1.00        0.25             0.19        0.36             -0.05            -0.01       0.28
# feedback_ha             0.20     0.30       0.25        1.00             0.19        0.44             -0.11             0.24       0.29
# participation_ha        0.10     0.20       0.19        0.19             1.00        0.24              0.32             0.35       0.07
# taskhelp_ha             0.30     0.21       0.36        0.44             0.24        1.00              0.03             0.22       0.47
# pretunderstand_ha       0.39     0.42      -0.05       -0.11             0.32        0.03              1.00             0.41       0.19
# clarification_ha        0.33     0.48      -0.01        0.24             0.35        0.22              0.41             1.00       0.00
# workown_ha              0.34     0.08       0.28        0.29             0.07        0.47              0.19             0.00       1.00

# explain_hs ↔ quiet_ha (0.53): Students who avoid asking for explanations also tend to stay quiet
# quiet_ha ↔ clarification_ha (0.48): Staying quiet and hesitating to ask for clarification are strongly linked
# taskhelp_ha ↔ workown_ha (0.47): Avoiding help and preferring to work alone go hand‑in‑hand
# pretunderstand_ha ↔ quiet_ha (0.42)
# and pretunderstand_ha ↔ clarification_ha (0.41): Pretending to understand is related to both silence and hesitation

# negative correlation
# support_hs ↔ pretunderstand_ha (-0.05)
# support_hs ↔ clarification_ha (-0.01)
# feedback_ha ↔ pretunderstand_ha (-0.11)
# clarification_ha ↔ workown_ha (0.00)
# Possible multi-dimensionality ??

###############################################################
# VISUAL INSPECTIONS
###############################################################

# Calculate total scores (sum of all item scores per person) for scale
total_scores <- apply(dfitems, 1, sum)

#################################
# Histograms
#################################
# Plot histogram of single item responses
for (i in item_names) {
  pdf(paste0(figures_path,"histogram_", i, ".pdf"), width = 7, height = 5)
  hist(
    dfitems[[i]],
    main = i,   # title
    xlab = "Response",                          # x-axis label
    ylab = "Frequency",                         # y-axis label
    col = "lightblue",                        # bar color
    border = "white",                         # remove dark borders
    breaks = seq(0.5, 6.5, by = 1),   # produces bins centered at 1,2,3,4,5,6
    xaxt = "n"                    # suppress default x-axis
  )
  axis(1, at = 1:6, labels = 1:6)
  dev.off()
}


# Plot histogram of total scores (here we expect a Gaussian)
#pdf(paste0(figures_path,"histogram_totalscores.pdf"), width = 7, height = 5)
png(paste0(figures_path, "histogram_totalscores.png"), width = 7, height = 5, units = "in", res = 300)
# Find the actual minimum and maximum score for clean plotting
min_score <- min(total_scores)
max_score <- max(total_scores)
mean(total_scores)
hist(
  total_scores,
  main = "Total Scores",   # title
  xlab = "Response",                          # x-axis label
  ylab = "Frequency",                         # y-axis label
  col = "lightblue",                        # bar color
  border = "white",                         # remove dark borders
  breaks = 10
)
dev.off()

#################################
# Density plots
#################################
# Approximate density
pdf(paste0(figures_path,"density_score.pdf"), width = 7, height = 5)
# Compute density first
d <- density(total_scores)
plot(
  d,
  main = "",
  xlab = "Score",
  ylab = "Density",
  lwd = 2,
  col = "steelblue"
)
# Add shaded area under density curve
polygon(d, col = adjustcolor("steelblue", alpha.f = 0.3), border = NA)
# Add normal curve on same x-range
curve(
  dnorm(x, mean = mean(total_scores), sd = sd(total_scores)),
  add = TRUE,
  lwd = 2,
  lty = 3,
  col = "darkred"
)
dev.off()

#################################
# Box plots
#################################
# - Maximum/100th percentile (thin upper horizontal line)
#   the highest data point in the data set excluding any outliers
# - Third quartile/75th percentile (upper end of grey area)
#   the median of the upper half of the dataset
# - Median/50th percentile (thick horizontal line)
#   the middle value in the data set
# - Minimum/0th percentile (thin lower horizontal line)
#   the lowest data point in the data set excluding any outliers
# - First quartile/25th percentile (lower end of grey area)
#   the median of the lower half of the dataset
pdf(paste0(figures_path,"boxplot_total-scores.pdf"), width = 5, height = 7)
boxplot(total_scores)
dev.off()

# If you have some extreme outlier, e.g. one person answering always with
# one option, you can consider to discard it. Please report this!

# We can also include some covariate information to look at different
# subgroups of the sample, here we plot different boxplots for language level
pdf(paste0(figures_path,"boxplot_covariate_language.pdf"), width = 8, height = 7)
boxplot(
  total_scores ~ langlevel,
  data = df,
  main = "",
  xlab = "Language Level",
  ylab = "Help Avoidance Score",
  col = c("red", "orange", "yellow", "green", "lightblue"),  # two soft colors
  border = "gray30",
  boxwex = 0.6,                       # wider boxes
  cex.axis = 1.2,
  cex.lab = 1.3
)
grid(nx = NA, ny = NULL, col = "gray85", lty = "dotted")
dev.off()


#################################
# Scatter Plots
#################################
# We can look at the relationship between a covariant and the total scores
# of scale 1
png(paste0(figures_path, "scatter_covariate_language.png"), width = 7, height = 5, units = "in", res = 300)
#pdf(paste0(figures_path,"scatter_covariate_language.pdf"), width = 7, height = 5)
scatter.smooth(
  x = df$langlevel,
  y = total_scores,
  main = "",
  xlab = "Language Level",
  ylab = "Help Avoidance Score",
  col = adjustcolor("steelblue", alpha.f = 0.5),   # semi-transparent points
  pch = 16,                                        # solid circles
  lpars = list(col = "darkred", lwd = 2)           # smoother line style
)
dev.off()

###############################################################
# DESCRIPTIVE STATISTICS
###############################################################

#################################
# Item difficulty
#################################
#item difficulty: to people tend do have very high endorsements (all very high or all very low)
# This provides information about how strongly persons agreed on average.
# It can be calculated based on the minimum and maximum score that can be
# chosen/achieved on the scale:
# diff := (mean(item) - min_score) / (max_score - min_score)
# The difficulty can take values between 0 to 1 and should not be too extreme,
# otherwise low or high response values are chosen too often and we cannot
# discriminate between participants.

# Calculate the mean of each item
itemdiff <- apply(dfitems, 2, mean)

# Get the min and max score of scale
range(dfitems[,1:9]) #CONFIRM: THIS SHOULD USE REVERSE CODED DATA
minscore1 <- 1 # set min score
maxscore1 <- 6 # set max score
# Calculate the difficulties for scale
itemdiff[1:9] <- (itemdiff[1:9]-minscore1)/(maxscore1-minscore1)

itemdiff
# Check the range of the item difficulties
range(itemdiff)
# 0.265 - 0.540

# Plot the difficulties of items
pdf(paste0(figures_path,"item_difficulty.pdf"), width = 7, height = 5)
plot(itemdiff[1:9], axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item difficulty")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.1,.5,.9),lty=2) # add horizontal lines
dev.off()

#################################
# Item variance
#################################
#item variance: how much do responses differ
# This provides information about the variation in the responses. It is an
# important consideration on top of the item difficulty since the latter
# can take a moderate value for two reasons:  either the responses are well
# distributed between low and high scores (indicating a desirable spread),
# or most responses cluster around medium scores (indicating less discrimination)
# A high variance indicates the first, more desirable scenario. High is
# always relative to the scale range (the variance of a 6-point-scale is
# naturally bigger than the one of a 4-point scale). The theoretical maximum
# variance can be calculated to be: (max_score - min_score)^2 / 4
# Looking at the standard deviation (the square root of the variance) can be
# more intuitive since it has the same "unit" as the measure itself.
##Standard Deviation: want larger/medium STD, concerned about variance that is too small
#too small STD--> too similar, cannot distinguish differences

# For a 6-point scale we get a maximum variance of 6.25 and thus a standard
# deviation of 2.5, so we can interpret the standard deviation as follows:
# < 1: low variance -responses are clustered around a central value
#                   (e.g., most responses are 3 or 4).
# 1-2: moderate variance - responses are somewhat dispersed
#                   but still show clustering
# > 2: high variance - responses are widely spread across the scale,
#                   indicating good discrimination.

# Calculate the variance of the response scores for all items
itemvar <- apply(dfitems, 2, var)
# Print the variances
print(itemvar)
# Print the standard deviations
print(sqrt(itemvar))

# Plot the variances of items
pdf(paste0(figures_path,"item_variance.pdf"), width = 7, height = 5)
plot(itemvar[1:9], axes=F, type="b", ylim=c(0,2.5),
     xlab="item", ylab="item variance")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
dev.off()

# Plot the standard deviation of scale 1 items
pdf(paste0(figures_path,"item_stdev.pdf"), width = 7, height = 5)
plot(sqrt(itemvar[1:9]), axes=F, type="b", ylim=c(0,2.5),
     xlab="item", ylab="item standard deviation")
axis(2) # add y-axis
abline(h=c(1,2),lty=2) # add horizontal lines
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
dev.off()

# Higher variance items: quiet_ha, pretunderstand_ha, clarification_ha, workown_ha (2,7,8,9)→ These items discriminate better between participants
# Middle variance items: explain_hs, support_hs, feedback_ha, participation_ha, taskhelp_ha → Responses are more clustered, meaning participants tended to answer similarly


#################################
# Item-scale correlation
#################################
#item/scale correlation: does this item behave consistently with other items on the scale (high: fits well, low: doesn't fit well)
#want high correlation
# These are the correlations between a single item in a scale and
# the average of all the other items within the same scale.
# They should be high for internal consistency.
# When they are low, check if removing items with particularly low
# correlation to the scale improves the other item-scale correlations.
#
# Calculate the items correlations separately for each scale!

itemcor1 <- c() # empty list
# loop  through the items
for(j in 1:length(item_names)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[,item_names[-j]],1,mean)
  # Calculate the correlation with item j
  itemcor1[j] <- cor(dfitems[,item_names[j]],scale_no_j)
}
# plot the results
#pdf(paste0(figures_path,"item_scale_correlation_1.pdf"), width = 7, height = 5)
png(paste0(figures_path,"item_cor_all.png"), width = 7, height = 5,units = "in", res = 300)
plot(itemcor1, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
text(1:9, itemcor1 + 0.05, round(itemcor1, 2), cex=0.9) 
dev.off()

itemcor1
# item scale correlations all very low,
# not acceptable(below threshold of 0.3) are candidates for deletion
# 0.5322077 0.5392777 0.2703121 0.3764429 0.3561524 0.4889656 0.3677770 0.4459346 0.3490601
# item 3: 0.2703121

# Test correction of scale by removing item 3
item_names_2 <- item_names[-3] # remove the item
itemcor2 <- c() # empty list
# loop  through the items
for(j in 1:length(item_names_2)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[,item_names_2[-j]],1,mean)
  # Calculate the correlation with item j
  itemcor2[j] <- cor(dfitems[,item_names_2[j]],scale_no_j,use="pair")
}
itemcor2 # print the correlations
# 0.5435445 0.5421667 0.3516961 0.3421549 0.4474109 0.4090479 0.4863468 0.3148630


pdf(paste0(figures_path,"item_scale_correlation_2_remove3.pdf"), width = 7, height = 5)
plot(itemcor2, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:8,c(1,2,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()

# After removing item 3, all value is acceptable (above 0.3)

#################################
# Table with all results
#################################
library(xtable)
# since we  only have one scale
itemcor <- itemcor1
# Save all calculated item descriptive statistics in a data frame
tab1 <- data.frame(itemdiff, itemvar, itemcor)
# Add column names
colnames(tab1) <- c("item difficulty","item variance", "item scale correlation")
# Add item codes as row names
rownames(tab1) <- item_names
# Print table with values rounded to two decimal places
round(tab1,2)
#save table png
library(gridExtra)
# Save all calculated item descriptive statistics in a data frame
tab1 <- data.frame(itemdiff, itemvar, itemcor)
# Add column names
colnames(tab1) <- c("item difficulty","item variance", "item scale correlation")
# Replace item names with numbers
rownames(tab1) <- paste0("I", 1:nrow(tab1))  # or just 1:nrow(tab1) for plain numbers

# Round to two decimal places
tab1_rounded <- round(tab1, 2)
# Print to console
print(tab1_rounded)
# Save as PNG
png(paste0(figures_path, "item_descriptives.png"), 
    width = 1200, height = 800, res = 150)
grid.table(tab1_rounded)
dev.off()

# Print latex table
xtable(tab1)
#                       item difficulty item variance item scale correlation
# explain_hs                   0.33          1.82                   0.53
# quiet_ha                     0.54          2.16                   0.54
# support_hs                   0.36          1.41                   0.27 (candidate for removal)
# feedback_ha                  0.26          1.46                   0.38
# participation_ha             0.26          1.51                   0.36
# taskhelp_ha                  0.31          1.43                   0.49
# pretunderstand_ha            0.39          2.15                   0.37
# clarification_ha             0.32          2.20                   0.45
# workown_ha                   0.41          2.25                   0.35



#################################
# DEFINE ITEM CATEGORIES
#################################
self_created <- c("explain_hs", "quiet_ha", "support_hs", "feedback_ha", "participation_ha")
llm_generated <- c("taskhelp_ha", "pretunderstand_ha", "clarification_ha", "workown_ha")
item_type <- c(rep("Self-Created", 5), rep("LLM-Generated", 4))

#################################
# ITEM-SCALE CORRELATIONS FOR SUBSETS
#################################
# Self-created subset
itemcor_self <- c()
for(j in 1:length(self_created)){
  scale_no_j <- apply(dfitems[, self_created[-j]], 1, mean)
  itemcor_self[j] <- cor(dfitems[, self_created[j]], scale_no_j)
}

# LLM-generated subset
itemcor_llm <- c()
for(j in 1:length(llm_generated)){
  scale_no_j <- apply(dfitems[, llm_generated[-j]], 1, mean)
  itemcor_llm[j] <- cor(dfitems[, llm_generated[j]], scale_no_j)
}

# Combine
itemcor_subset <- c(itemcor_self, itemcor_llm)

#################################
# TABLE: FULL SCALE (9 ITEMS)
#################################
tab_full <- data.frame(
  Type = item_type,
  Difficulty = round(itemdiff[1:9], 3),
  Variance = round(itemvar[1:9], 3),
  Cor_Full = round(itemcor1, 3),
  Cor_Subset = round(itemcor_subset, 3)
)
rownames(tab_full) <- item_names
print(tab_full)
xtable(tab_full)

#################################
# TABLE: REVISED SCALE (8 ITEMS, NO ITEM 3)
#################################
tab_revised <- data.frame(
  Type = item_type[-3],
  Difficulty = round(itemdiff[item_names_2], 3),
  Variance = round(itemvar[item_names_2], 3),
  Cor_Revised = round(itemcor2, 3)
)
rownames(tab_revised) <- item_names_2
print(tab_revised)
xtable(tab_revised)

#################################
# TABLE: COMPARISON BY TYPE
#################################
comparison <- data.frame(
  Type = c("Self-Created", "LLM-Generated"),
  N = c(5, 4),
  Avg_Difficulty = round(c(mean(itemdiff[self_created]), mean(itemdiff[llm_generated])), 3),
  Avg_Variance = round(c(mean(itemvar[self_created]), mean(itemvar[llm_generated])), 3),
  Avg_Cor_Full = round(c(mean(itemcor1[1:5]), mean(itemcor1[6:9])), 3),
  Avg_Cor_Subset = round(c(mean(itemcor_self), mean(itemcor_llm)), 3)
)
print(comparison)
xtable(comparison)


#################################
# PLOT: ITEM-SCALE CORRELATIONS BY AI SUBSET
#################################

# Self-created items
# main="Self-created items"
png(paste0(figures_path,"item_cor_self.png"), width = 7, height = 5,units = "in", res = 300)
#pdf(paste0(figures_path,"item_cor_self.pdf"), width = 7, height = 5)
plot(itemcor_self, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item-subset correlation")
axis(2)
axis(1, at=1:5, labels=1:5)
abline(h=0.3, lty=2)
text(1:5, itemcor_self + 0.05, round(itemcor_self, 2), cex=0.9)
dev.off()

# LLM-generated items
# main="LLM-generated items"
png(paste0(figures_path,"item_cor_llm.png"), width = 5, height = 5,units = "in", res = 300)
#pdf(paste0(figures_path,"item_cor_llm.pdf"), width = 7, height = 5)
plot(itemcor_llm, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item subset correlation")
axis(2)
axis(1, at=1:4, labels=6:9)
abline(h=0.3, lty=2)
text(1:4, itemcor_llm + 0.05, round(itemcor_llm, 2), cex=0.9)
dev.off()

#################################
# PLOT: General versus Task SUBSET
#################################
general <- c("explain_hs", "quiet_ha", "participation_ha", "pretunderstand_ha", "clarification_ha")
task <- c("support_hs","feedback_ha","taskhelp_ha","workown_ha")
item_type_gt <- c(rep("General", 5), rep("Task", 4))

#################################
# Calculate item-scale correlations
#################################
itemcor_gen <- c()
for(j in 1:length(general)){
  scale_no_j <- apply(dfitems[, general[-j]], 1, mean)
  itemcor_gen[j] <- cor(dfitems[, general[j]], scale_no_j)
}

itemcor_task <- c()
for(j in 1:length(task)){
  scale_no_j <- apply(dfitems[, task[-j]], 1, mean)
  itemcor_task[j] <- cor(dfitems[, task[j]], scale_no_j)
}

# Combine
itemcor_subset_gt <- c(itemcor_gen, itemcor_task)

#################################
# Calculate total scores - FIX: don't use [-j] here
#################################
total_scores_gen <- apply(dfitems[, general], 1, sum)   # REMOVED [-j]
total_scores_task <- apply(dfitems[, task], 1, sum)     # REMOVED [-j]

#pdf(paste0(figures_path,"histogram_totalscores.pdf"), width = 7, height = 5)
#png(paste0(figures_path, "histogram_totalscores.png"), width = 7, height = 5, units = "in", res = 300)
# Find the actual minimum and maximum score for clean plotting
min_score_g <- min(total_scores_gen)
max_score_gen <- max(total_scores_gen)
total_scores_gen
hist(
  total_scores_gen,
  main = "Total Scores General",   # title
  xlab = "Response",                          # x-axis label
  ylab = "Frequency",                         # y-axis label
  col = "lightblue",                        # bar color
  border = "white",                         # remove dark borders
  breaks = 5
)
#dev.off()

min_score_t <- min(total_scores_task)
max_score_t <- max(total_scores_task)
total_scores_task
hist(
  total_scores_task,
  main = "Total Scores Owned",   # title
  xlab = "Response",                          # x-axis label
  ylab = "Frequency",                         # y-axis label
  col = "lightblue",                        # bar color
  border = "white",                         # remove dark borders
  breaks = 4
)
