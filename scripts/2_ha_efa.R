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
# SESSION 2: EFA
################################################################
################################################################
################################################################

print(getwd())
path="~/Desktop/psychometrics_foundations/psychometrics_help-avoidance"
figures_path = "~/Desktop/psychometrics_foundations/psychometrics_help-avoidance/figures/efa/"
#path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project"
#figures_path = "E:\\OneDrive - UT Cloud\\UniLife\\M1 Semester1\\3_Psychometrics\\HA_Project\\figures\\efa\\"
setwd(path)

# Install libraries
library(foreign) # importing/exporting data from/to different statistical software formats
library(fastDummies) # creating binary indicator variables from categorical variables
library(lubridate) # handling and manipulation of date and time data
library(psych) # library for factor analysis
library(MVN) # library for multivariate normal distributions
library(GPArotation) # library for rotating factors
library(dplyr)
library(car) #use qqplot function
#for returning tables
library(gridExtra)
library(grid)
library(tidyr)
library(ggplot2)

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
# Part 1: Exploratory factor analysis for scales 1 and 2
###############################################################

#################################
# (a) number of factors
#################################

# Plot the scree plot
#pdf(paste0(figures_path,"scree_plot.pdf"), width = 7, height = 5)
png(paste0(figures_path, "scree_plot.png"), width = 7, height = 5, units = "in", res = 300)
fa1 <- fa.parallel(dfitems, # data frame of just our items
                   fm="ml", # estimation method maximum likelihood
                   # (allows for hypothesis testing, is based on
                   #  normality assumption)
                   fa="fa") # plot only eigenvalues of factor analysis
# The eigenvalues reflect only the shared variance / communality h^2
# among variables (not the total as in PCA)
# Add mean eigenvalue as horizontal line for the Kaiser Guttman criterion
abline(h=mean(fa1$fa.values),lty=2,col="black")
mean = round(mean(fa1$fa.values),2)
# Add label to the line
text(x = 5, y = mean(fa1$fa.values), 
     labels = paste0("Kaiser Criterion (Mean Eigen = ",mean,")"), 
     pos = 3, # position above the line (1=below, 2=left, 3=above, 4=right)
     cex = 0.8, # text size
     col = "black")
dev.off()
# As the function outputs, all criteria (ellbow, KG and parallel analysis)
#=elbow: 3
#Kaiser: 3
#Parallel Analysis: 2 --> Parallel Analysis is the most reliably method, the following ANOVA confirm it.

# Check this by comparing different factor models
# Save models with 1, 2, 3 factors
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
# p-value (PR) is 0.01 -> significant at alpha=0.05 -> reject H0 -> evidence that there is
# improvement in fit when adding the second factor

# Comparison between two-factor and three-factor model
anova(fa1.ml2,fa1.ml3)
# p-value (PR) > 0.17 -> not significant at any relevant significance
# level -> do not reject H0 -> evidence that there is no improvement in fit
# when adding the third factor (3rd factor not needed)
# This also indicates 2 factors.

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
pdf(paste0(figures_path,"qqplot.pdf"), width = 7, height = 5)
qqplot(q, MD,
       main = "Multivariate Q-Q Plot (Mahalanobis)",
       xlab = "Theoretical Chi-square Quantiles",
       ylab = "Mahalanobis Distances")
abline(0, 1, col = "red")
dev.off()

# do data mostly align with the diagonal line? -> largely follow a multivariate
# if yes --> normal distribution
# From plot, we can see that the data mostly align with the diagonal line,
# indicating that the data largely follow a multivariate normal distribution.

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
round(fa1.pfa1$residual,2)
# Diagonal values are uniquenesses (1 − h²). High values mean the single factor explains little variance in those items.
# Off‑diagonal residuals should be near 0 if the model fits.

r1factor<- round(fa1.pfa1$residual,2)
r1factor[abs(r1factor) > 0.3]
diag(r1factor)
#  explain_hs          quiet_ha        support_hs       feedback_ha
#              0.60              0.56              0.90              0.80
#  participation_ha       taskhelp_ha pretunderstand_ha  clarification_ha
#              0.84              0.71              0.80              0.70
#        workown_ha
#              0.84
# -> 1 factor model leaves a lot of unexplained variance (high diagonals) and some residual correlations.
# -> try 2 factor model

# Get residual matrix of two factor model
fa1.pfa2 <- fa(dfitems,nfactors=2,rotate="none",covar=F,fm="pa",max.iter=1000) #UPDATE: df to dfitems
round(fa1.pfa2$residual,2)
r2factor <- round(fa1.pfa2$residual,2)
r2factor[abs(r2factor) > 0.3]
r2factor
diag(r2factor)
# explain_hs          quiet_ha        support_hs       feedback_ha
#              0.61              0.49              0.76              0.67
#  participation_ha       taskhelp_ha pretunderstand_ha  clarification_ha
#              0.84              0.38              0.50              0.57
#        workown_ha
#              0.70
# still poor overall fit based on diagonal residuals > 0.3

# Run three factor model
fa1.pfa3 <- fa(dfitems, # data
               nfactors=3, # number of extracted factors
               rotate="none", # no rotation
               covar=F, # treat data as correlation matrix
               fm="pa") # factor extraction method principal axis "pa"
# Print the residual matrix
round(fa1.pfa3$residual,2)
# Diagonal values are uniquenesses (1 − h²). High values mean the single factor explains little variance in those items.
# Off‑diagonal residuals should be near 0 if the model fits.

##########################################################################
# (c) rotation of factors to simple structure
##########################################################################

# Define a function to create a helpful plot for visualizing 2-factor solutions
pl.fa2 <- function(dat, main, fa2, col="black") {
  # Plot the original factor loadings from the factor analysis
  # Each point represents a variable (indicator) in the factor solution
  # x and y axes correspond to the loadings on Factor 1 and Factor 2 respectively
  plot(fa2$loadings[,1], fa2$loadings[,2], axes=F, xlim=c(-1,1), ylim=c(-1,1),
       xlab="", ylab="", main=main)

  # Add item numbers to the plot
  for(j in 1:length(fa2$loadings[,1])) {
    text(fa2$loadings[j,1], fa2$loadings[j,2] + 0.075, paste0(j))
  }

  # Draw dashed lines for unrotated factor axes
  abline(h=0, lty=2)
  abline(v=0, lty=2)

  # Draw solid lines for rotated factor axes
  lines(c(-dat$Th[1,1], dat$Th[1,1]), c(-dat$Th[2,1], dat$Th[2,1]), col=col)
  lines(c(-dat$Th[1,2], dat$Th[1,2]), c(-dat$Th[2,2], dat$Th[2,2]), col=col)

  # Add text labels for rotated factor axes
  text(dat$Th[1,1], dat$Th[2,1], "Factor 1")
  text(dat$Th[1,2], dat$Th[2,2], "Factor 2")
}

##########################################################################

# Get factor loadings from analysis without rotation as starting value
fa.start <- fa(dfitems, nfactors=2, rotate="none", fm="pa")
# Extract factor loadings
initial_loadings <- fa.start$loadings
print(initial_loadings)
# Loadings:
#                   PA1    PA2
# explain_hs         0.615 -0.124
# quiet_ha           0.658 -0.274
# support_hs         0.328  0.362
# feedback_ha        0.462  0.343
# participation_ha   0.391
# taskhelp_ha        0.610  0.500
# pretunderstand_ha  0.482 -0.514
# clarification_ha   0.561 -0.342
# workown_ha         0.425  0.340


# Perform orthogonal rotation varimax
vmax1 <- Varimax(A=initial_loadings)
print(vmax1)
# Loadings:
#                       PA1     PA2
# explain_hs        0.54406  0.3123
# quiet_ha          0.67545  0.2273
# support_hs        0.00793  0.4883
# feedback_ha       0.12134  0.5619
# participation_ha  0.34417  0.2000
# taskhelp_ha       0.12966  0.7783
# pretunderstand_ha 0.70125 -0.0695
# clarification_ha  0.64781  0.1126
# workown_ha        0.09583  0.5357
# PA1 strong: pretunderstand_ha (.70), clarification_ha (.65), quiet_ha (.68), explain_hs (.54).
# PA2 strong: taskhelp_ha (.78), feedback_ha (.56), workown_ha (.54), support_hs (.49).
# Varimax: show loadings >= .30

# Perform oblique rotation quartimin
obli <- oblimin(A=initial_loadings)
# factor correlation matrix
obli$Phi
#        PA1       PA2
# PA1 1.0000000 0.2879537
# PA2 0.2879537 1.0000000
print(obli)
# Loadings:
#                      PA1     PA2
# explain_hs        0.5132  0.2422
# quiet_ha          0.6621  0.1347
# support_hs        0.0678 -0.5036
# feedback_ha       0.0378  0.5628
# participation_ha  0.3242  0.1557
# taskhelp_ha       0.0128  0.7852
# pretunderstand_ha 0.7349 -0.1758
# clarification_ha  0.6514  0.0203
# workown_ha        0.0156  0.5395
# -> Same pattern, but slightly cleaner loadings for oblimin.
# !!!
# PA1: explain_hs, quiet_ha, participation_ha, pretunderstand_ha, clarification_ha
# PA2: support_hs, feedback_ha, taskhelp_ha, workown_ha
# -> use Oblimin for rotation
# !!!

#Return table of factor loadings
#nice item names: 
item_labels <- c(
  "I1: Concepts", 
  "I2: Meetings", 
  "I3: Contribut Support", 
  "I4: Feedack My Work", 
  "I5: Particpat", 
  "I6: Help My Task", 
  "I7: Discussn", 
  "I8: Clarificat",
  "I9 CollaborTask"
)

# Build table
loadings_df <- as.data.frame(unclass(obli$loadings))
rownames(loadings_df) <- item_labels
loadings_df <- round(loadings_df, 3)

# Save as PNG
png(paste0(figures_path,"factor_loadings.png"), width = 1200, height = 800, res = 150)
grid.table(loadings_df)
dev.off()
#------------------------------------------ return PNG of loadings
loadings_df <- as.data.frame(unclass(obli$loadings))
rownames(loadings_df) <- item_labels
loadings_df$item <- rownames(loadings_df)

plot_df <- loadings_df %>%
  pivot_longer(c(PA1, PA2), names_to = "Factor", values_to = "Loading") %>%
  group_by(item) %>%
  mutate(is_primary = abs(Loading) == max(abs(Loading))) %>%
  ungroup()

p <- ggplot(plot_df, aes(x = Factor, y = item)) +
  geom_text(
    aes(label = sprintf("%.2f", Loading),
        fontface = ifelse(is_primary, "bold", "plain")),
    size = 7
  ) +
  scale_y_discrete(limits = (item_labels)) +
  scale_x_discrete(position = "top") + 
  labs(
    title = "Oblimin-Rotated Factor Loadings",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

ggsave(
  paste0(figures_path,"factor_loadings.png"),
  p,
  width = 8,
  height = 6,
  dpi = 300
)
#--------------------------------------------------------

# Plot results to easier interpret the solutions
par(mfrow=c(1,2))
pdf(paste0(figures_path,"varimax_initial_loadings.pdf"), width = 7, height = 5)
pl.fa2(vmax1,"Varimax", fa2=fa.start)
dev.off()
pdf(paste0(figures_path,"oblimin_initial_loadings.pdf"), width = 7, height = 5)
pl.fa2(obli,"Oblimin", fa2=fa.start)
dev.off()
# -> Examine which items cluster near which factor axis
assign_factor <- function(loadings) {
  L <- as.matrix(loadings)
  best <- apply(abs(L), 1, which.max)
  data.frame(item = rownames(L), factor = colnames(L)[best], loading = L[cbind(1:nrow(L), best)])
}

af_vmax = assign_factor(vmax1$loadings)
af_obli = assign_factor(obli$loadings)
af_vmax[order(af_vmax$factor, -abs(af_vmax$loading)), ] #UPDATE: add to order
af_obli[order(af_obli$factor, -abs(af_obli$loading)), ] #UPDATE: add to order


##########################################################################
# Implement target that reflects the observed structure:
# Based on theory: Items 1,3 are Help-Seeking (reversed), Items 2,4-9 are Help-Avoidance
# Initialize target matrix (9 items x 2 factors) filled with 0
L2 <- matrix(0, dim(initial_loadings)[1], dim(initial_loadings)[2])
# Set free positions: Items 1,3 load on Factor 1; Items 2,4-9 load on Factor 2
L2[c(1,3), 1] <- L2[c(2,4:9), 2] <- NA
# Perform orthogonal target rotation
t1ort <- targetT(A=initial_loadings, Target=L2)
print(t1ort)

# Perform oblique target rotation
t1obl <- targetQ(A=initial_loadings, Target=L2)
print(t1obl)

# Plot results
par(mfrow=c(1,2))
pdf(paste0(figures_path,"orthogonal_TargetL2.pdf"), width = 7, height = 5)
pl.fa2(t1ort, "Target 1, orthogonal", fa2=fa.start)
dev.off()
pdf(paste0(figures_path,"oblique_TargetL2.pdf"), width = 7, height = 5)
pl.fa2(t1obl, "Target 1, oblique", fa2=fa.start)
dev.off()
# -> Compare if oblique and orthogonal solutions are similar
#    (indicates factors are uncorrelated)


##########################################################################
# Bifactor, just trial
##########################################################################
# Test bifactor solutions:
# Initialize two more target matrices
L31 <- L32 <- matrix(0, dim(initial_loadings)[1], dim(initial_loadings)[2])

# Bifactor 1: Factor 1 loads on ALL items, Factor 2 restricted to items 1,3 (HS items)
L31[1:9, 1] <- L31[c(1,3), 2] <- NA
t2ort1 <- targetT(A=initial_loadings, Target=L31)
print(t2ort1)
# Loadings:
#                     PA1     PA2
# explain_hs        0.620 -0.0962
# quiet_ha          0.669 -0.2448
# support_hs        0.311  0.3762
# feedback_ha       0.446  0.3628
# participation_ha  0.394 -0.0590
# taskhelp_ha       0.588  0.5265
# pretunderstand_ha 0.504 -0.4925
# clarification_ha  0.576 -0.3170
# workown_ha        0.410  0.3582

# Bifactor 2: Factor 1 loads on ALL items, Factor 2 restricted to items
#PA2: Owned-Task Help-Avoidance ** might be a bifactor
# 3. If I am struggling with my project contributions, I seek support from my group members [-] efaP2 *my project contributions
# 4. When I am unsure about my work, I am reluctant to seek feedback from my group [+] efaP2 *my work
# 6. I avoid asking my group members for help even when I find my task difficult. [+] efaP2 *my task
# 9. I tend to keep working on my own even when collaborating with my group would make the task easier [+] efaP2  *on my own... the task (I am working on)
# Test bifactor solutions:
# Initialize two more target matrices
PA1 <- PA2 <- matrix(0,dim(initial_loadings)[1],dim(initial_loadings)[2])
# The first target PA1 allows factor 1 to load on all items, while factor 2 is
# restricted to load only on 2,4-9
PA1[1:9,1] <- PA2[c(3,4,6,9), 2] <- NA
# Perform orthogonal rotation with target L31
t2ort2 <- targetT(A=initial_loadings, Target=PA1)

print(t2ort2)
# Loadings:
#                     PA1     PA2
# explain_hs        0.620 -0.0962
# quiet_ha          0.669 -0.2448
# support_hs        0.311  0.3762
# feedback_ha       0.446  0.3628
# participation_ha  0.394 -0.0590
# taskhelp_ha       0.588  0.5265
# pretunderstand_ha 0.504 -0.4925
# clarification_ha  0.576 -0.3170
# workown_ha        0.410  0.3582

# Plot results
par(mfrow=c(1,2))
pdf(paste0(figures_path,"bifactor_hs.pdf"), width = 7, height = 5)
pl.fa2(t2ort1, "Bifactor (HS specific)", fa2=fa.start)
dev.off()
pdf(paste0(figures_path,"bifactor_ha.pdf"), width = 7, height = 5)
pl.fa2(t2ort2, "Bifactor (HA specific)", fa2=fa.start)
dev.off()
# -> Don't separate cleanly

##########################################################################
# Test three factor solutions (for comparison)
##########################################################################

# Perform 3-factor with varimax rotation
fa.3varimax <- fa(dfitems, nfactors=3, rotate="varimax", fm="pa")
print(fa.3varimax)
#                   PA1   PA2   PA3   h2   u2 com
# explain_hs        0.51  0.19  0.29 0.38 0.62 1.9
# quiet_ha          0.69  0.21  0.01 0.52 0.48 1.2
# support_hs        0.05  0.43  0.17 0.21 0.79 1.3
# feedback_ha       0.20  0.65  0.00 0.46 0.54 1.2
# participation_ha  0.37  0.16  0.04 0.17 0.83 1.4
# taskhelp_ha       0.20  0.64  0.28 0.52 0.48 1.6
# pretunderstand_ha 0.74 -0.36  0.40 0.84 0.16 2.0
# clarification_ha  0.69  0.14 -0.11 0.51 0.49 1.1
# workown_ha        0.02  0.40  0.78 0.77 0.23 1.5

# Perform 3-factor with oblimin rotation
fa.3oblimin <- fa(dfitems, nfactors=3, rotate="oblimin", fm="pa", max.iter=1000)
print(fa.3oblimin)
#                    PA1   PA2   PA3   h2   u2 com
# explain_hs         0.34  0.34  0.23 0.38 0.62 2.7
# quiet_ha           0.59  0.31 -0.07 0.52 0.48 1.6
# support_hs         0.27 -0.17  0.30 0.21 0.79 2.6
# feedback_ha        0.60 -0.29  0.18 0.46 0.54 1.6
# participation_ha   0.34  0.15  0.01 0.17 0.83 1.4
# taskhelp_ha        0.47 -0.16  0.45 0.52 0.48 2.2
# pretunderstand_ha  0.02  0.91  0.09 0.84 0.16 1.0
# clarification_ha   0.59  0.32 -0.21 0.51 0.49 1.8
# workown_ha        -0.04  0.11  0.88 0.77 0.23 1.0

# Compare factor structures - suppress loadings < 0.3 for clarity
print(fa.3oblimin$loadings, cutoff=0.3)
#                     PA1    PA2    PA3
# explain_hs         0.336  0.345
# quiet_ha           0.591  0.315
# support_hs
# feedback_ha        0.605
# participation_ha   0.341
# taskhelp_ha        0.472         0.446
# pretunderstand_ha         0.905
# clarification_ha   0.588  0.316
# workown_ha                       0.882
# -> 3 factor seems unstable and 2 factor model is preferred.




library(xtable)

#################################
# TABLE 1: FACTOR LOADINGS
#################################
loadings_obli <- as.matrix(obli$loadings)
h2 <- fa.start$communality
u2 <- 1 - h2

factor_table <- data.frame(
  PA1 = round(loadings_obli[,1], 3),
  PA2 = round(loadings_obli[,2], 3),
  h2 = round(h2, 3),
  u2 = round(u2, 3)
)
rownames(factor_table) <- paste0("I", 1:9)

print(factor_table)
print(xtable(factor_table,
             caption = "Factor Loadings with Oblimin Rotation",
             label = "tab:efa_loadings"),
      include.rownames = TRUE)

#################################
# TABLE 2: FACTOR CORRELATION
#################################
factor_cor_table <- as.data.frame(round(obli$Phi, 3))
colnames(factor_cor_table) <- c("Factor 1", "Factor 2")
rownames(factor_cor_table) <- c("Factor 1", "Factor 2")

print(xtable(factor_cor_table,
             caption = "Factor Correlation Matrix",
             label = "tab:factor_cor"),
      include.rownames = TRUE)

#################################
# PLOT: FACTOR AXES (OBLIMIN)
#################################
pdf(paste0(figures_path,"efa_oblimin_axes.pdf"), width = 7, height = 5)
pl.fa2(obli, "Two-Factor Solution (Oblimin Rotation)", fa2=fa.start)
dev.off()
