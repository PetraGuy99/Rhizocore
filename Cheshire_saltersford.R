## Cheshire: Saltersford 
# 03/06/2024
# Alison Stewart
# alison@rhizocore.com 
########################

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

saltersford <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/Cheshire_saltersford.csv") 


# Check data ----
str(saltersford)

# Change treatment from character to factor and change height to a interger
saltersford$Treatment <- as.factor(saltersford$Treatment) 
saltersford$Date <- as.factor(saltersford$Date) 
saltersford$Height..cm. <- as.integer(saltersford$Height..cm.) 

# Remove NA
saltersford <- na.omit(saltersford)

# Create new datasets ----

# Aim: Compare 2023 to 2024 in treatment and control plots
# is there a significant height change from one year to the next.

# Make a new dataset for each species
saltersford_oak = subset(saltersford, Species == "Oak")
saltersford_birch = subset(saltersford, Species == "Birch")


########################
## Oak analysis ----
# Box plot to check for outliers ----
ggplot(saltersford_oak) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(saltersford_oak$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(saltersford_oak$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 93
low<- Q[1]-1.5*iqr # Lower Range = 37

# I need to remove outliers

# I can remove any outwith the defined range using this line of code
saltersford_oak_removed<- subset(saltersford_oak, saltersford_oak$Height..cm. > 
                       (Q[1] - 1.5*iqr) & saltersford_oak$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(saltersford_oak_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Result: They have been, so I will  use the new dataset created.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date * Treatment,
                data = saltersford_oak_removed)
r

# Result: The homogeneity of variance hypothesis is rejected, p =6.355e-07 ***
## If the hypothesis of equal variances is rejected, another version of the ANOVA can be used: 
## the Welch ANOVA, see below
## oneway.test(variable ~ group, data = saltersford_oak_removed, var.equal = FALSE)

# Model ----
mod <- aov(Height..cm. ~ Date * Treatment,
           data = saltersford_oak_removed)

mod1 <- oneway.test(Height..cm. ~ Date * Treatment, data = saltersford_oak_removed, var.equal = FALSE)

# Testing normality
table(saltersford_oak_removed$Date, saltersford_oak_removed$Treatment)
plot(mod, which = 2)
plot(mod, which = 3)
hist(mod$residuals)
shapiro.test(mod$residuals)

# Population conformity: The data points from  2023 are uneven with more in the control
 # than treatment, but even in 2024.
# QQ plot: Points do follow line.
# Homogeneity of variance: line is flat, pass.
# Histogram: Bell-shaped
# Sharpio-Wilk: p = 0.1848, passed normality test. 
# we accept the null hypothesis.
# We wont do a logarithmic model

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date * Treatment,
          data = saltersford_oak_removed,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
 # 1 2023   Control    62.36441
 # 2 2024   Control    73.52874
 # 3 2023 Treatment    61.32164
 # 4 2024 Treatment    70.00000

# ANOVA summary ----
summary(mod) # p = significant values, further investigate

# HSD test ----
HSD.test(mod,
         trt = c("Date", "Treatment"),
         console = TRUE)

# Result: 2024 groups are different.

# Tukey test ----
TukeyHSD(mod)

# Result:
# 2023:Treatment-2023:Control p = 0.6321531
# 2024:Treatment-2024:Control p = 0.0404137

# Since the treatment and control trees started at the same height p = 0, 
# and now the control is taller than the treatment p = 0.63.
# We can defer that the control trees have grown more than the treatment trees.

# Welch ANOVA summary ----
mod1 # p = < 2.2e-16

# Games Howell Post-hoc Test ----
saltersford_oak_removed %>%
  group_by(Date) %>%
  games_howell_test(Height..cm. ~ Treatment)

# Results:
# 1 2023  Height..cm. Control Treatment p = 0.181         
# 2 2024  Height..cm. Control Treatment p = 0.032 *    

# These show the same results as "normal" ANOVA.


# Plot data height ----
ggplot(saltersford_oak_removed) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C", "#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Saltersford Quercus Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff


################################
## Birch analysis ----
# Box plot to check for outliers ----
ggplot(saltersford_birch) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot()

# Defining and removing outliers ----

# I'm not going too, because all it will do is remove the tallest trees.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = saltersford_birch)
r

# Result: The homogeneity of variance hypothesis is rejected, p = 1.23e-08 ***.
## If the hypothesis of equal variances is rejected, another version of the ANOVA can be used: 
## the Welch ANOVA, see below
## oneway.test(variable ~ group, data = saltersford_birch, var.equal = FALSE)

# Model ----
mod2 <- aov(Height..cm. ~ Date * Treatment,
           data = saltersford_birch)

# Welch ANOVA
mod3 <- oneway.test(Height..cm. ~ Date * Treatment, data = saltersford_birch, var.equal = FALSE)

# Testing normality
table(saltersford_birch$Date, saltersford_birch$Treatment)
plot(mod2, which = 2)
plot(mod2, which = 3)
hist(mod2$residuals)
shapiro.test(mod2$residuals)

# Population conformity: In 2023 the control data points are half of treatment, in 2024 the same
# QQ plot: Points do follow line.
# Homogeneity of variance: line is flat, pass.
# Histogram: Bell-shaped
# Sharpio-Wilk: p = 0.1122, passed normality test. 
# we accept the null hypothesis.
# We wont do a logarithmic model

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date * Treatment,
          data = saltersford_birch,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
# 1 2023   Control    66.67797
# 2 2024   Control   100.14000
# 3 2023 Treatment    67.90400
# 4 2024 Treatment   107.25926

# ANOVA summary ----
summary(mod2) # p = significant results, further exploration needed

# HSD test ----
HSD.test(mod2,
         trt = c("Date", "Treatment"),
         console = TRUE)

# Result: Year groups are the same.

# Tukey test ----
TukeyHSD(mod2)

# Result:
# 2023:Treatment-2023:Control  p = 0.9575009
# 2024:Treatment-2024:Control  p = 0.0858947
# No difference in height growth between the control and treatment plots.
# Both treatment and control grew the same from 2023 to 2024.

# Welch ANOVA summary ----
mod3 # p = < 2.2e-16

# Games Howell Post-hoc Test ----
saltersford_birch %>%
  group_by(Date) %>%
  games_howell_test(Height..cm. ~ Treatment)

# Results:
# 1 2023  Height..cm. Control Treatment  p =  0.517          
# 2 2024  Height..cm. Control Treatment  p = 0.068    

# Results from Welch ANOVA show the same conclusion as ANOVA.

# Plot data height ----
ggplot(saltersford_birch) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C", "#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Saltersford Betula Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

