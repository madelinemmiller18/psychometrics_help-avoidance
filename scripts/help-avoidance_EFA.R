print(getwd())
path="~/Desktop/psychometrics_foundations/Help_Avoidance"
setwd(path)

#Install libraries
library(psych) # library for factor analysis
library(MVN) # library for multivariate normal distributions
library(GPArotation) # library for rotating factors
library(dplyr)
library(car) #use qqplot function

#upload data
rawdata <- read.csv("data/Help Avoidance in Group Projects.csv")
rawdata$id <- 1:nrow(rawdata) #Add ID column

dim(rawdata) # dimension
head(rawdata) # first entries
# save the relevant item labels
item_data <- rawdata[c(15,3:11)]
cov_data <- rawdata[c(15,12:14)]
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
# Part 1: Exploratory factor analysis for scales 1 and 2
###############################################################

#################################
# (a) number of factors
#################################

# Plot the scree plot for scale 1
pdf("figures/scree_plot.pdf", width = 7, height = 5)
fa1 <- fa.parallel(dfitems, # data frame of just our items
                   fm="ml", # estimation method maximum likelihood
                   # (allows for hypothesis testing, is based on
                   #  normality assumption)
                   fa="fa") # plot only eigenvalues of factor analysis
# The eigenvalues reflect only the shared variance / communality h^2
# among variables (not the total as in PCA)
# Add mean eigenvalue as horizontal line for the Kaiser Guttman criterion
abline(h=mean(fa1$fa.values),lty=2,col="black")
dev.off()
# As the function outputs, all criteria (ellbow, KG and parallel analysis)
#=elbow: 3
#Kaiser: 3
#PA: 2 --> Parallel Analysis is the most reliably method

# Check this by comparing different factor models
# Save model with one factor
fa1.ml1 <- fa(dfitems,nfactors=1,rotate="none",covar=F,fm="ml")
# Save model with two factors
fa1.ml2 <- fa(dfitems,nfactors=2,rotate="none",covar=F,fm="ml")
# Save model with three factors
fa1.ml3 <- fa(dfitems,nfactors=3,rotate="none",covar=F,fm="ml")
# Perform a model difference test which computes the difference in the models' 
# log-likelihoods (a measure of fit) and evaluates this difference against 
# a chi-square distribution with degrees of freedom corresponding to the 
# difference in the number of estimated parameters between the two models
# -> hypothesis test with H0: The simpler model (with less degrees of freedom) 
# fits the data as well as the more complex model 

# Comparison between one-factor and two-factor model
anova(fa1.ml1,fa1.ml2)
# p-value (PR) is 0.01 -> significant -> reject H0 -> evidence that there is
# improvement in fit when adding the second factor

# Comparison between two-factor and three-factor model
anova(fa1.ml2,fa1.ml3)
# p-value (PR) > 0.17 -> not significant at any relevant significance 
# level -> do not reject H0 -> evidence that there is no improvement in fit 
# when adding the third factor
#This indicates a two factor model

# Check normality which is assumed for maximum likelihood by plotting the QQ-plot
# (compare squared Mahalanobis distances as a measure of how far observations 
# are from the center of the multivariate data with the theoretical quantiles 
# of a chi-square distribution which they are expected to follow if the data 
# are multivariate normal

# Compute covariance and mean
S <- cov(dfitems, use = "pairwise.complete.obs")
mu <- colMeans(dfitems, na.rm = TRUE)

# Compute Mahalanobis distances
MD <- mahalanobis(dfitems, mu, S)

# Compute theoretical chi-square quantiles
q <- qchisq(ppoints(length(MD)), df = length(item_names))

# Plot
qqplot(q, MD,
       main = "Multivariate Q-Q Plot (Mahalanobis)",
       xlab = "Theoretical Chi-square Quantiles",
       ylab = "Mahalanobis Distances")
abline(0, 1, col = "red")

# do data mostly align with the diagonal line? -> largely follow a multivariate 
# if yes --> normal distribution

##########################################################################
# (b) residual matrix: S-Sigma
##########################################################################

# The residual matrix helps assess whether the factor model adequately 
# explains the data. Large residuals indicate the need for more factors 
# or adjustments to the model.
# Specifically, the diagonal entries represent the unique variances 1-h^2 
# of observed item responses not explained by the factors.
# The off-diagonal elements represent the residual correlations between them
# not explained by the factors.

# Run one factor model 
fa1.pfa1 <- fa(dfitems, # data
               nfactors=1, # number of extracted factors
               rotate="none", # no rotation
               covar=F, # treat data as correlation matrix
               fm="pa") # factor extraction method principal axis "pa"
# Print the residual matrix
r1factor<- round(fa1.pfa1$residual,2)
r1factor[abs(r1factor) > 0.3] #prints just residuals over .3
r1factor #prints all
# -> unexplained variances very high, some moderate correlations, especially 
#    high unexplained correlation between item 1 and 2 residuals

# Get residual matrix of two factor model
fa1.pfa2 <- fa(dfitems,nfactors=2,rotate="none",covar=F,fm="pa")
r2factor <- round(fa1.pfa2$residual,2)
r2factor[abs(r2factor) > 0.3]

##########################################################################
# (c) rotation of factors to simple structure
##########################################################################

# Define a function to create a helpful plot for visualizing 2-factor solutions
pl.fa2 <- function(dat, main, fa2, col="black") {
  # Plot the original factor loadings from the factor analysis
  # Each point represents a variable (indicator) in the factor solution
  # `x` and `y` axes correspond to the loadings on Factor 1 and Factor 2 respectively
  plot(fa2$loadings[,1], fa2$loadings[,2], axes=F, xlim=c(-1,1), ylim=c(-1,1),
       xlab="", ylab="", main=main) 
  
  # Add item numbers to the plot, labeling each point with its corresponding variable number
  # The vertical position of the label is adjusted slightly to avoid overlap with the point
  for(j in 1:length(fa2$loadings[,1])) {
    text(fa2$loadings[j,1], fa2$loadings[j,2] + 0.075, paste0(j))
  }
  
  # Draw dashed lines to represent the unrotated factor axes (horizontal and vertical lines)
  abline(h=0, lty=2) # Horizontal line at y = 0
  abline(v=0, lty=2) # Vertical line at x = 0
  
  # Draw solid lines to represent the rotated factor axes
  # Rotated axes are calculated based on the rotation matrix `dat$Th`
  # `dat$Th[1,1]` and `dat$Th[2,1]` define the coordinates for the first factor's axis
  lines(c(-dat$Th[1,1], dat$Th[1,1]), c(-dat$Th[2,1], dat$Th[2,1]), col=col) # Factor 1
  # `dat$Th[1,2]` and `dat$Th[2,2]` define the coordinates for the second factor's axis
  lines(c(-dat$Th[1,2], dat$Th[1,2]), c(-dat$Th[2,2], dat$Th[2,2]), col=col) # Factor 2
  
  # Add text labels to indicate the rotated factor axes
  # These labels ("Factor 1" and "Factor 2") are positioned at the end of the respective axes
  text(dat$Th[1,1], dat$Th[2,1], "Factor 1") # Label for the first rotated factor
  text(dat$Th[1,2], dat$Th[2,2], "Factor 2") # Label for the second rotated factor
}


##########################################################################

# Get factor loadings from analysis without rotation as starting value
fa1.start <- fa(dfitems,nfactors=2,rotate="none",fm="pa")
# Extract factor loadings
initial_loadings <- fa1.start$loadings
print(initial_loadings)

# Perform orthogonal rotation varimax
?Varimax()
vmax1 <- Varimax(A=initial_loadings)
print(vmax1)

# Perform oblique rotation quartimin (delta is here gam)
?oblimin()
obli <- oblimin(A=initial_loadings)
print(obli)
# Loadings:
#PA1 
#explain_hs 
#quiet_ha 
#participation_ha
#pretunderstand_ha
#clarification_ha 

#PA2
#support_hs 
#feedback_ha 
#taskhelp_ha
#workown_ha

# Plot results to easier interpret the solutions
par(mfrow=c(1,2))
pl.fa2(vmax1,"Varimax", fa2=fa1.start)
pl.fa2(obli,"Oblimin", fa2=fa1.start)
# -> variables 1 and 2 close to factor 2 axis and variables 3 to 6 close to
#    factor 1 axis 

# Implement target that reflects the observed structure: 
# Initialize target matrix with same dimension as initial loadings filled with 0
L2 <- matrix(0,dim(initial_loadings)[1],dim(initial_loadings)[2])
# Set entries 1 and 2 in column 1 and entries 3 to 6 in column 2 to null 
# (these are the free positions)
L2[1:2,1] <- L2[3:6,2] <- NA
# Perform orthogonal target rotation minimizing rhw zero loading entries
t1ort <- targetT(A=initial_loadings, Target=L2)
print(t1ort)

t1obl <- targetQ(A=initial_loadings, Target=L2)
print(t1obl)

# Plot results
pl.fa2(t1ort,"Target 1, orthogonal", fa2=fa1.start)
pl.fa2(t1obl,"Target 1, oblique", fa2=fa1.start)
# -> oblique and orthogonal solutions are similar, indicating that the two 
#    factors are orthogonal / uncorrelated 

# Test bifactor solutions:
# Initialize two more target matrices
L31 <- L32 <- matrix(0,dim(initial_loadings)[1],dim(initial_loadings)[2])
# The first target L31 allows factor 1 to load on all items, while factor 2 is 
# restricted to load only on indicators 1 and 2
L31[1:9,1] <- L31[1:2,2] <- NA
# Perform orthogonal rotation with target L31
t2ort1 <- targetT(A=initial_loadings, Target=L31)
print(t2ort1)

# The second target L32 allows factor 1 to load on all items, while factor 2 is 
# restricted to load only on indicators 3 to 6
L32[1:9,1] <- L32[3:6,2] <- NA
# Perform orthogonal rotation with target L32
t2ort2 <- targetT(A=initial_loadings, Target=L32)
print(t2ort2)

# Plot results
pl.fa2(t2ort1,"Target 2, orthogonal",fa2=fa1.start)
pl.fa2(t2ort2,"Target 3, orthogonal",fa2=fa1.start)
# -> looks similar to previous result for two separate factors, so that result
#    seems robust

# All investigations show similar results, indicating two uncorrelated factors
# with a clear structure; possible interpretation:
#Factor 1
#Factor 2
##########################################################################

# Get factor loadings from analysis without rotation as starting value
fa2.start <- fa(dfitems,nfactors=2,rotate="none",fm="pa")
# Extract factor loadings
initial_loadings <- fa2.start$loadings
print(initial_loadings)


# Perform orthogonal rotation varimax
vmax1 <- Varimax(A=initial_loadings)
print(vmax1)

# Perform oblique rotation quartimin
obli <- oblimin(A=initial_loadings)
print(obli)

par(mfrow=c(1,2))
pl.fa2(vmax1,"Varimax",fa2=fa2.start)
pl.fa2(obli,"Oblimin",fa2=fa2.start)
# -> clearly shows how different rotated solution is from the original one

# Test most promising bifactor solutions 
L31 <- L32 <- matrix(0,dim(initial_loadings)[1],dim(initial_loadings)[2])
# let the restricted factor load only on items 1 and 3
L31[1:6,1] <- L31[c(1,3),2] <- NA
t2ort1 <- targetT(A=initial_loadings, Target=L31)
# let the restricted factor load only on the other items
L32[1:6,1] <- L32[c(2,4:6),2] <- NA
t2ort2 <- targetT(A=initial_loadings, Target=L32)
# Plot the results
pl.fa2(t2ort1,"Target 2, orthogonal",fa2=fa2.start)
pl.fa2(t2ort2,"Target 3, orthogonal",fa2=fa2.start)
# -> two cases lead to very different results

# All analyses suggest that there is no way to rotate such that all items are 
# represented well.

# Test three factor solutions:
# Perform orthogonal rotation varimax
fa2.3varimax <- fa(dfitems,nfactors=3,rotate="varimax",fm="pa")
fa2.3varimax

fa2.3oblimin <- fa(dfitems,nfactors=3,rotate="oblimin",fm="pa",max.iter=1000)
fa2.3oblimin
