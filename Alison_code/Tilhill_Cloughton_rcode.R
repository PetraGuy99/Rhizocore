## Tilhill: Cloughton Tree Measuring Data
# 17/04/2024
# Alison Stewart
# alison@rhizocore.com 
##########################

# Load packages ----
library(ggplot2)
library(tidyr)
library(tidyverse)
library(sysfonts) #not sure if I need, but to do with font
library(showtext) #not sure if I need, but to do with font
library(agricolae)
library(dplyr)
library(hrbrthemes)
library(geomtextpath)
library(car)
library(rstatix)

# Add Rhizocore font ----
myfont <- "Karla"


# Input data ----
getwd()
setwd("~/Desktop/Rhizocore")

tilhill_site <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/Tilhill_dataset_Master.csv") 


# Check data ----
## we need species to be a character, treatment is a factor , height to be an integer
str(tilhill_site)

# Change treatment from character to factor
tilhill_site$Treatment <- as.factor(tilhill_site$Treatment) 
tilhill_site$Date <- as.factor(tilhill_site$Date) 

# Remove NA
tilhill_site <- na.omit(tilhill_site)

# Create new datasets----

# Make a new data set for each site
tilhill_site1 = subset(tilhill_site, Coordinate == "Site 1")
tilhill_site2 = subset(tilhill_site, Coordinate == "Site 2")

###########################

## Site 1 analysis ----
# Box plot to check for outliers ----
ggplot(tilhill_site1) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(tilhill_site1$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(tilhill_site1$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 89.5
low<- Q[1]-1.5*iqr # Lower Range = 21.5

# I need to remove outliers

# I can remove any outwith the defined range using this line of code
tilhill_site1_removed<- subset(tilhill_site1, tilhill_site1$Height..cm. > 
                       (Q[1] - 1.5*iqr) & tilhill_site1$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(tilhill_site1_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Result: They have, so I will use the new dataset created.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date * Treatment,
                data = tilhill_site1_removed)
r

# Result: The homogeneity of variance hypothesis is rejected, p = 4.119e-10 ***
## If the hypothesis of equal variances is rejected, another version of the ANOVA can be used: 
## the Welch ANOVA, see below
## oneway.test(variable ~ group, data = tilhill_site1, var.equal = FALSE)

# Model ----
# Welch ANOVA
mod <- oneway.test(Height..cm. ~ Date * Treatment, data = tilhill_site1_removed, 
                   var.equal = FALSE)

# Welch ANOVA summary ----
mod # p = < 2.2e-16

# Result: this tells us that height significantly differs according to the date and treatment.
# But it does not tell us exactly how.

# Normal ANOVA: this would tell us more, potentially can force through due to our large dataset
mod1 <- aov(Height..cm. ~ Date * Treatment,
           data = tilhill_site1_removed)

# Testing normality
table(tilhill_site1$Date, tilhill_site1$Treatment)
plot(mod1, which = 2)
plot(mod1, which = 3)
hist(mod1$residuals)
shapiro.test(mod1$residuals)

# Population conformity: The total data points for 2023 + 2024 controls is more than half less 
 # than 2023 + 2024 treatments.
# QQ plot: Points do follow line, pass
# Homogeneity of variance: line is flat, pass
# Histogram: Bell-shaped, pass
# Sharpio-Wilk: p = 0.001965, Failed normality test. 
 # we reject the null hypothesis that the populations are equal.
# BUT!!!
  # We wont do a logarithmic model because of the central limit theorem 
  # that if each group has a large sample, 
  # I can use my dataset even if it fails normality tests.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date * Treatment,
          data = tilhill_site1_removed,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
# 1 2023   Control    49.85463
# 2 2024   Control    63.90411
# 3 2023 Treatment    46.97908
# 4 2024 Treatment    63.89813

# ANOVA summary ----
summary(mod1) # p = significant values, explore further

# HSD test ----
HSD.test(mod1,
         trt = c("Date", "Treatment"),
         console = TRUE)

# Result: 2023 groups are different, 2024 groups are the same but different from 2023.

# Tukey test ----
TukeyHSD(mod1)

# 2023:Treatment-2023:Control p = 0.0004013
# 2024:Treatment-2024:Control p = 0.9999998

# Since the Treatment trees started at a significantly lower height than the control 
# p = 0, and now they are the same height as the control trees p = 0.99.
# We can defer that the treatment trees have grown more than the control trees.

# Results: 


# Plot data height ----
ggplot(tilhill_site1_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C", "#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Tilhill 2023 & 2024 Picea sitchensis Measurements in Plot 1", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

###########################

## Site 2 analysis ----
# Box plot to check for outliers ----
ggplot(tilhill_site2) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(tilhill_site2$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(tilhill_site2$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 73
low<- Q[1]-1.5*iqr # Lower Range = 25

# I need to remove outliers

# I can remove any outwith the defined range using this line of code
tilhill_site2_removed<- subset(tilhill_site2, tilhill_site2$Height..cm. > 
                       (Q[1] - 1.5*iqr) & tilhill_site2$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(tilhill_site2_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Result: They have/been, so I will use the new dataset created.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = tilhill_site2_removed)
r

# Result: The homogeneity of variance hypothesis is accepted p = 0.5346.

# Model ----
mod3 <- aov(Height..cm. ~ Date * Treatment,
           data = tilhill_site2_removed)

# Testing normality
table(tilhill_site2_removed$Date, tilhill_site2_removed$Treatment)
plot(mod3, which = 2)
plot(mod3, which = 3)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Population conformity: The total data points for control 2023 is more than half of 
 # 2023 treatment, in 2023 the difference is not as big.
# QQ plot: Points do follow line.
# Homogeneity of variance: line is flat, pass.
# Histogram: Bell-shaped
# Sharpio-Wilk: p = 1.36e-05, Failed normality test. 
# we reject the null hypothesis.
# We wont do a logarithmic model because of the  central limit theorem 
 # that if each group has a large sample, 
 # I can use my dataset even if it fails normality tests.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date * Treatment,
          data = tilhill_site2_removed,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
 # 1 2023   Control    43.44667
 # 2 2024   Control    49.86508
 # 3 2023 Treatment    47.84383
 # 4 2024 Treatment    51.21581

# ANOVA summary ----
summary(mod3) # p = significant values, explore further.

# HSD test ----
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# Result: 2023 groups are different.

# Tukey test ----
TukeyHSD(mod3)

# Result:
# 2023:Treatment-2023:Control p = 0.0000001
# 2024:Treatment-2024:Control p = 0.1941056

# Since the control trees started at a significantly lower height than the treatment 
# p = 0, and now they are the same height as the control trees p = 0.19.
# We can defer that the control trees have grown more than the treatment trees.


# Plot data height ----
ggplot(tilhill_site2_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment") +                # specifying title of legend
  labs(title = "Tilhill 2023 & 2024 Picea sitchensis Measurements in Plot 2", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff



###########################




