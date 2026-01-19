print(getwd())
path="~/Desktop/psychometrics_foundations/Help_Avoidance"
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

#filter to just item data and covariate data
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
df$degree_level[df$degree_level  == "MD"] <- 1 #MD = med student, these can be considered undergraduates
df$degree_level[df$degree_level  == "Medical Student"] <- 1
# Classify as 2 (Master or equivalent)
df$degree_level[df$degree_level == "Graduate Student (Master or equivalent)"] <- 2
# Classify as 3 (Doctoral or equivalent)
df$degree_level[df$degree_level == "Doctoral Student (PhD or equivalent)"] <- 3

#language level self rating numerical encoding 
unique(df$langlevel) #return unique values
df$langlevel[df$langlevel == "Less than adequate"]<-1
df$langlevel[df$langlevel ==" Adequate"]<-2
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
  
# all information should be saved in a data frame where the 
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
#this aligns with how questions were writted
# So, we re-code by subtracting the response value from 
# the maximum score plus 1 = 6 + 1 = 7
dfitems[,"explain_hs"] <- 7-dfitems[,"explain_hs"]
dfitems[,"support_hs"] <- 7-dfitems[,"support_hs"]

#correlation matrix after reverse coding
round(cor(dfitems), 2)

###############################################################
# VISUAL INSPECTIONS
###############################################################

# Calculate total scores (sum of all item scores per person) for scale
total_scores <- apply(dfitems,1,sum) 

#################################
# Histograms
#################################
# Plot histogram of single item responses
for (i in item_names) {
  pdf(paste0("figures/histogram_", i, ".pdf"), width = 7, height = 5)
  hist(
    dfitems[[i]],
    main = i,   # title
    xlab = "Response",                          # x-axis label
    ylab = "Frequency",                         # y-axis label
    col = "lightblue",                        # bar color
    border = "white",                         # remove dark borders
    breaks = seq(0.5, 6.5, by = 1),   # produces bins centered at 1,2,3,4,5,6
    xaxt = "n",                        # suppress default x-axis
  )
  axis(1, at = 1:6, labels = 1:6)
  dev.off()
}

# Plot histogram of total scores (here we expect a Gaussian)
pdf("figures/histogram_strategy.pdf", width = 7, height = 5)
# Find the actual minimum and maximum score for clean plotting
min_score <- min(total_scores) 
max_score <- max(total_scores)
total_scores
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
# Approximate density for scale 1
pdf("figures/density_strategy.pdf", width = 7, height = 5)
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
pdf("figures/boxplot_strategy.pdf", width = 5, height = 7)
boxplot(total_scores)
dev.off()
# If you have some extreme outlier, e.g. one person answering always with 
# one option, you can consider to discard it. Please report this!

# We can also include some covariate information to look at different 
# subgroups of the sample, here we plot different boxplots for language level
pdf("figures/boxplot_strategy_gender.pdf", width = 8, height = 7)
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
pdf("scatter_strategy_age.pdf", width = 7, height = 5)
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
#item difficulty: to people tend to have very high endorsements (all very high or all very low)
# This provides information about how strongly persons agreed on average.
# It can be calculated based on the minimum and maximum score that can be 
# chosen/achieved on the scale:
# diff := (mean(item) - min_score) / (max_score - min_score)
# The difficulty can take values between 0 to 1 and should not be too extreme,
# otherwise low or high response values are chosen too often and we cannot
# discriminate between participants.

# Calculate the mean of each item
itemdiff <- apply(df[1:9],2,mean)

# Get the min and max score of scale
range(df[,1:9])
minscore1 <- 1 # set min score
maxscore1 <- 6 # set max score
# Calculate the difficulties for scale
itemdiff[1:9] <- (itemdiff[1:9]-minscore1)/(maxscore1-minscore1)

# Check the range of the item difficulties
range(itemdiff) 

# Plot the difficulties of scale 1 items
pdf("figures/item_difficulty_strategy.pdf", width = 7, height = 5)
plot(itemdiff[1:9], axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item difficulty")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.1,.5,.9),lty=2) # add horizontal lines
dev.off()
# -> items 2 has high difficulty

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
# > 1: low variance -responses are clustered around a central value 
#                   (e.g., most responses are 3 or 4).
# 1-2: moderate variance - responses are somewhat dispersed 
#                   but still show clustering
# > 2: high variance - responses are widely spread across the scale, 
#                   indicating good discrimination.

# Calculate the variance of the response scores for all items
itemvar <- apply(df[1:9],2,var)
# Print the variances
print(itemvar)
# Print the standard deviations
print(sqrt(itemvar))

# Plot the variances of scale 1 items
pdf("figures/item_variance_strategy.pdf", width = 7, height = 5)
plot(itemvar[1:9], axes=F, type="b", ylim=c(0,2.5),
     xlab="item", ylab="item variance")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
dev.off()

# Plot the standard deviation of scale 1 items
pdf("figures/item_std_strategy.pdf", width = 7, height = 5)
plot(sqrt(itemvar[1:9]), axes=F, type="b", ylim=c(0,2.5),
     xlab="item", ylab="item standard deviation")
axis(2) # add y-axis
abline(h=c(1,2),lty=2) # add horizontal lines
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
dev.off()

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

itemcor <- c() # empty list
# loop  through the items
for(j in 1:length(item_names)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[-j],1,mean)
  # Calculate the correlation with item j
  itemcor[j] <- cor(dfitems[j],scale_no_j)
}
# plot the results
pdf("figures/item_scale_correlation_strategy.pdf", width = 7, height = 5)
plot(itemcor, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:9,c(1,2,3,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()
itemcor
# item scale correlations all very low, 
# not acceptable(below threshold of 0.3) are candidates for deletion

# Test correction of scale by removing item 3
item_names_a <- item_names[-3] # remove the item
itemcora <- c() # empty list
# loop  through the items
for(j in 1:length(item_names_a)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[-j],1,mean)
  # Calculate the correlation with item j
  itemcora[j] <- cor(dfitems[j],scale_no_j,use="pair")
}
itemcora # print the correlations 

pdf("figures/item_scale_correlation_strategy_reduced_A.pdf", width = 7, height = 5)
plot(itemcora, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:8,c(1,2,4,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()

# Test correction of scale by removing items 3 and 4
item_names_b <- item_names[-c(3,4)] # remove the items
itemcorb <- c() # empty list
# loop  through the items
for(j in 1:length(item_names_b)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[-j],1,mean)
  # Calculate the correlation with item j
  itemcorb[j] <- cor(dfitems[j],scale_no_j,use="pair")
}
itemcorb # print the correlations 

pdf("figures/item_scale_correlation_strategy_reduced_B.pdf", width = 7, height = 5)
plot(itemcorb, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:7,c(1,2,5,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()

# Test correction of scale by removing items 3, 4, 5
item_names_c <- item_names[-c(3,4,5)] # remove the items
itemcorc <- c() # empty list
# loop  through the items
for(j in 1:length(item_names_c)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[-j],1,mean)
  # Calculate the correlation with item j
  itemcorc[j] <- cor(dfitems[j],scale_no_j,use="pair")
}
itemcorc # print the correlations 

pdf("figures/item_scale_correlation_strategy_reduced_C.pdf", width = 7, height = 5)
plot(itemcorc, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:6,c(1,2,6,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()

# Test correction of scale by removing items 3, 4, 5, 6
item_names_d <- item_names[-c(3,4,5,6)] # remove the items
itemcord <- c() # empty list
# loop  through the items
for(j in 1:length(item_names_d)){
  # Save the sub scale including all items, but item j
  scale_no_j <- apply(dfitems[-j],1,mean)
  # Calculate the correlation with item j
  itemcord[j] <- cor(dfitems[j],scale_no_j,use="pair")
}
itemcord # print the correlations 

pdf("figures/item_scale_correlation_strategy_reduced_D.pdf", width = 7, height = 5)
plot(itemcord, axes=F, type="b", ylim=c(0,1),
     xlab="item", ylab="item scale correlation")
axis(2) # add y-axis
axis(1,1:5,c(1,2,7,8,9)) # add item labels on x-axis
abline(h=c(.3),lty=2) # add horizontal line
dev.off()

# However, we keep everything for the subsequent analyses 
# because factor analyses may reveal a different structure.

#################################
# Table with all results
#################################
library(xtable)
# Save all calculated item descriptive statistics in a data frame
tab1 <- data.frame(itemdiff, itemvar, itemcor)
# Add column names
colnames(tab1) <- c("item difficulty","item variance", "item scale correlation")
# Add item codes as row names
rownames(tab1) <- item_names
# Print table with values rounded to two decimal places
round(tab1,2)

# Print latex table
xtable(tab1)
