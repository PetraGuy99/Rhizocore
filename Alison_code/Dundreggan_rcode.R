## Dundreggan Tree Measuring Data
# 24/04/2024
# Alison Stewart
# alison@rhizocore.com 

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

# Add Rhizocore font ----
myfont <- "Karla"


# Input data ----
getwd()
setwd("~/Desktop/Rhizocore")

dundreggan_data <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/Dundreggan_2024_2023.csv")

# Check data ----
 # We need species to be a character, treatment is a factor , height to be an interger
str(dundreggan_data)

 # Change treatment from character to factor
dundreggan_data$Treatment <- as.factor(dundreggan_data$Treatment) 
dundreggan_data$Date <- as.factor(dundreggan_data$Date) 

##########################

## Betula pubescens analysis ----

 # Aim: Compare control, then fertiliser, then height 2023 to 2024,
  # is there a significant height change from one year to the next.

 # Make a new data set for each treatment
dundreggan_data_control = subset(dundreggan_data, Treatment == "Control")
dundreggan_data_pellet = subset(dundreggan_data, Treatment == "Pellet")
dundreggan_data_pellet1 = subset(dundreggan_data_pellet, Coordinate == "Site 1")
dundreggan_data_pellet2 = subset(dundreggan_data_pellet, Coordinate == "Site 2")
dundreggan_data_fertiliser = subset(dundreggan_data, Treatment == "Fertiliser")
dundreggan_data_fertiliser1 = subset(dundreggan_data_fertiliser, Coordinate == "Site 1")
dundreggan_data_fertiliser2 = subset(dundreggan_data_fertiliser, Coordinate == "Site 2")


## Control analysis ----
# Box plot to check for outliers ----
ggplot(dundreggan_data_control) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

# Defining and removing outliers ----

  # First find the 25th and the 75th percentile of the dataset.
Q <- quantile(dundreggan_data_control$Height..cm., probs=c(.25, .75), na.rm = FALSE)

  # the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(dundreggan_data_control$Height..cm.)

  # Find the cut-off ranges beyond which all data points are outliers.
   # Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 83
low<- Q[1]-1.5*iqr # Lower Range = 27

# It looks like I might need to remove some outliers

# I can remove any outwith the defined range using this line of code
dundreggan_data_control_removed<- subset(dundreggan_data_control, dundreggan_data_control$Height..cm. > 
                                        (Q[1] - 1.5*iqr) & dundreggan_data_control$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(dundreggan_data_control_removed) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Result: They have been, so I will now use the new dataset created.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = dundreggan_data_control_removed)
r

# Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod <- aov(Height..cm. ~ Date,
           data = dundreggan_data_control_removed)

# Testing normality
table(dundreggan_data_control_removed$Date)
plot(mod, which = 2)
plot(mod, which = 3)
hist(mod$residuals)
shapiro.test(mod$residuals)

 # All looks ok apart from the Shapiro-Wilk test, 
  # which shows we reject the null hypothesis. So, we must do a 
   # logarithmic transformation.

 # Population conformity: Data points from both years within range
 # QQ plot: Points follow line, a little off at the right end
 # Homogeneity of variance: line is flat, pass.
 # Histogram: Bell shaped
 # Sharpio-Wilk: p = 0.03213, failed normality test 
  # we reject the null hypothesis.
  # Will do a logarithmic model to find a better fit 

# Logarithmic Model ----
x<-abs(min(dundreggan_data_control_removed$Height..cm.))+1
mod1<-aov(log(Height..cm.+x)~Date, data=dundreggan_data_control_removed)

# Testing normality of log model
plot(mod1, which = 2)
plot(mod1, which = 3)
hist(mod1$residuals)
shapiro.test(mod1$residuals)


# Population conformity: Data points from both years within range
# QQ plot: Points follow line, a little off at the right end
# Homogeneity of variance: line is flat, pass.
# Histogram: Bell shaped
# Sharpio-Wilk: p = 0.02389, failed normality test 
# we reject the null hypothesis.

 # Since this dataset has over 200 values I don't need normality 
  # to continue my analysis. But it should be noted.

 # I will continue with the non logarithimic model as it is a better fit.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date,
          data = dundreggan_data_control_removed,
          FUN = mean)

 # Results:
  #   Date    Height..cm.
  # 1 2023    54.90291
  # 2 2024    55.27778 

# ANOVA summary ----
summary(mod) # p = 0.724

# HSD test ----
HSD.test(mod,
         trt = c("Date"),
         console = TRUE)

# Result: groups are the same.

TukeyHSD(mod,
         which = "Date")

 # Result: p = 0.724, Betula pubescens trees have not increased in height from 2023 to 2024
  # in the control plot.

# Plot data height ----
ggplot(dundreggan_data_control_removed) +
  aes(x = Date, y = Height..cm., fill = Date) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Date") +                # specifying title of legend
  labs(title = " Betula pubescens 2023 & 2024 Tree Measurements in Control Plot", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff



###########################

# Fertiliser stats Site 1 ----
# Box plot to check for outliers ----
ggplot(dundreggan_data_fertiliser1) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

 # Result : no outliers

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = dundreggan_data_fertiliser1)
r

 # Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod2 <- aov(Height..cm. ~ Date,
           data = dundreggan_data_fertiliser1)

 # Testing normality
table(dundreggan_data_fertiliser1$Date)
plot(mod2, which = 2)
plot(mod2, which = 3)
hist(mod2$residuals)
shapiro.test(mod2$residuals)

 # Population conformity: Data points from both years within range
 # QQ plot: Points follow line
 # Homogeneity of variance: line is flat, pass.
 # Histogram: Bell shaped, a little bit bimodal.
 # Sharpio-Wilk: p = 0.1663, passed normality test 
 # we accept the null hypothesis. 

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date,
          data = dundreggan_data_fertiliser1,
          FUN = mean)

# Results:
#   Date    Height..cm.
# 1 2023    69.7381
# 2 2024    70.2500 

# ANOVA summary ----
summary(mod2) # p = 0.794

# HSD test ----
HSD.test(mod2,
         trt = c("Date"),
         console = TRUE)

# Result: groups are the same.

TukeyHSD(mod2,
         which = "Date")

# Result: p = 0.7938912, Betula pubescens trees have not increased in height from 2023 to 2024
# in fertiliser site 1.

# Plot data height ----
ggplot(dundreggan_data_fertiliser1) +
  aes(x = Date, y = Height..cm., fill = Date) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Date") +                # specifying title of legend
  labs(title = " Betula pubescens 2023 & 2024 Tree Measurements in Fertiliser Plot 1", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

###########################

# Fertiliser stats Site 2 ----
# Box plot to check for outliers ----
ggplot(dundreggan_data_fertiliser2) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

# Defining and removing outliers ----

 # First find the 25th and the 75th percentile of the dataset.
Q <- quantile(dundreggan_data_fertiliser2$Height..cm., probs=c(.25, .75), na.rm = FALSE)

 # the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(dundreggan_data_fertiliser2$Height..cm.)

 # Find the cut-off ranges beyond which all data points are outliers.
 # Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 74
low<- Q[1]-1.5*iqr # Lower Range = 14

 # Result: There are no outliers

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = dundreggan_data_fertiliser2)
r

# Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod3 <- aov(Height..cm. ~ Date,
           data = dundreggan_data_fertiliser2)

# Testing normality
table(dundreggan_data_fertiliser2$Date)
plot(mod3, which = 2)
plot(mod3, which = 3)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

 # Population conformity: almost double the data points in 2024
 # QQ plot: Points follow line
 # Homogeneity of variance: line is flat, pass.
 # Histogram: Right skewed
 # Sharpio-Wilk: p = 0.0287, failed normality test 
  # we reject the null hypothesis. So, we must do a logarithmic transformation.

# Logarithmic Model ----
x<-abs(min(dundreggan_data_fertiliser2$Height..cm.))+1
mod4<-aov(log(Height..cm.+x)~Date, data=dundreggan_data_fertiliser2)

# Testing normality of log model
plot(mod4, which = 2)
plot(mod4, which = 3)
hist(mod4$residuals)
shapiro.test(mod4$residuals)

 # QQ plot: Points follow line, vear off at the right side
 # Homogeneity of variance: line is flat, pass.
 # Histogram: Bell shaped, a little bit bimodal.
 # Sharpio-Wilk: p = 0.3131, passed normality test 
  # we accept the null hypothesis. 

 # Result: Use the logarithmic model


# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date,
          data = dundreggan_data_fertiliser2,
          FUN = mean)

# Results:
#   Date    Height..cm.
# 1 2023    44.86667
# 2 2024    46.99145 

# ANOVA summary ----
summary(mod4) # p = 0.135

# HSD test ----
HSD.test(mod4,
         trt = c("Date"),
         console = TRUE)

# Result: groups are the same.

TukeyHSD(mod4,
         which = "Date")

# Result: p = 0.1345143, Betula pubescens trees have not increased in height from 2023 to 2024
# in the 2nd fertiliser plot.

# Plot data height ----
ggplot(dundreggan_data_fertiliser2) +
  aes(x = Date, y = Height..cm., fill = Date) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Date") +                # specifying title of legend
  labs(title = " Betula pubescens 2023 & 2024 Tree Measurements in Fertiliser Plot 2", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

###########################

## Pellet site 1 analysis ----
# Box plot to check for outliers ----
ggplot(dundreggan_data_pellet1) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(dundreggan_data_pellet1$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(dundreggan_data_pellet1$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 116
low<- Q[1]-1.5*iqr # Lower Range = 24

# I don't need to remove outliers

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = dundreggan_data_pellet1)
r

# Result: The homogeneity of variance hypothesis is accepted

# Model ----
mod5 <- aov(Height..cm. ~ Date,
           data = dundreggan_data_pellet1)

# Testing normality
table(dundreggan_data_pellet1$Date)
plot(mod5, which = 2)
plot(mod5, which = 3)
hist(mod5$residuals)
shapiro.test(mod5$residuals)

# Population conformity: Data points from both years are within range.
# QQ plot: Points follow the line in the center but veer off at both ends.
# Homogeneity of variance: line is flat, pass.
# Histogram: Almost Bell-shaped, a bit Bimodal
# Sharpio-Wilk: p = 0.001048, failed normality test. 
# we reject the null hypothesis.
# We will do a logarithmic model

# Logarithmic Model ----
x<-abs(min(dundreggan_data_pellet1$Height..cm.))+1
mod6<-aov(log(Height..cm.+x)~Date, data=dundreggan_data_pellet1)

# Testing normality of log model
plot(mod6, which = 2)
plot(mod6, which = 3)
hist(mod6$residuals)
shapiro.test(mod6$residuals)

# QQ plot: Again, Points follow the line in the center but veer off at both ends.
# Homogeneity of variance: line is flat, pass.
# Histogram: Left-skewed
# Sharpio-Wilk: p = 0.0002211, failed normality test. 
# we reject the null hypothesis.
# The logarithmic model is a fail but better fit.

# The logarithmic model is a better fit, I will continue with it,
# considering the central limit theorem that each group has a large sample, 
# I can use my dataset even if it fails normality tests.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date,
          data = dundreggan_data_pellet1,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
# 1 2023    64.61538
# 2 2024    70.27586 

# ANOVA summary ----
summary(mod6) # p = 0.000606 ***

# HSD test ----
HSD.test(mod6,
         trt = c("Date"),
         console = TRUE)

# Result: groups are different.

TukeyHSD(mod6,
         which = "Date")

# Result: p = 0.0006055
# The Betula pendula trees inoculated with pellets in plot 1 have significantly grown

# Plot data height ----
ggplot(dundreggan_data_pellet1) +
  aes(x = Date, y = Height..cm., fill = Date) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Date") +                # specifying title of legend
  labs(title = "Betula pubescens 2023 & 2024 Tree Measurements in Pellet Plot 1", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff



###########################

## Pellet site 2 analysis ----
# Box plot to check for outliers ----
ggplot(dundreggan_data_pellet2) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(dundreggan_data_pellet2$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(dundreggan_data_pellet2$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 77.8
low<- Q[1]-1.5*iqr # Lower Range = 15.5

# I need to remove outliers

# I can remove any outwith the defined range using this line of code
dundreggan_data_pellet2_removed<- subset(dundreggan_data_pellet2, dundreggan_data_pellet2$Height..cm. > 
                       (Q[1] - 1.5*iqr) & dundreggan_data_pellet2$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(dundreggan_data_pellet2_removed) +
  aes(x = Date, y = Height..cm.) +
  geom_boxplot()

# Result: They have so I will use the new dataset created.

# Levens’s test ----
r <- leveneTest(Height..cm. ~ Date,
                data = dundreggan_data_pellet2_removed)
r

# Result: The homogeneity of variance hypothesis is accepted

# Model ----
mod7 <- aov(Height..cm. ~ Date,
           data = dundreggan_data_pellet2_removed)

# Testing normality
table(dundreggan_data_pellet2_removed$Date)
plot(mod7, which = 2)
plot(mod7, which = 3)
hist(mod7$residuals)
shapiro.test(mod7$residuals)

# Population conformity: Data points from both years are not within range.
# QQ plot: Points do follow line.
# Homogeneity of variance: line is flat, pass.
# Histogram: Right-skewed
# Sharpio-Wilk: p = 0.005302, failed normality test. 
# we reject the null hypothesis.
# We will do a logarithmic model

# Logarithmic Model ----
x<-abs(min(dundreggan_data_pellet2_removed$Height..cm.))+1
mod8<-aov(log(Height..cm.+x)~Date, data=dundreggan_data_pellet2_removed)

# Testing normality of log model
plot(mod8, which = 2)
plot(mod8, which = 3)
hist(mod8$residuals)
shapiro.test(mod8$residuals)

# QQ plot: Points do follow line.
# Homogeneity of variance: line is flat, pass.
# Histogram: Bell-shaped-ish
# Sharpio-Wilk: p = 0.1201, passed normality test. 
# we accept the null hypothesis.
# The logarithmic model is a (better/worse) fit.

# The logarithmic model is a better fit, I will continue with it.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Date,
          data = dundreggan_data_pellet2_removed,
          FUN = mean)

# Results: Group means
#   Date    Height..cm.
# 1 2023    44.83824
# 2 2024    47.50427 

# ANOVA summary ----
summary(mod8) # p = 0.0996

# HSD test ----
HSD.test(mod8,
         trt = c("Date"),
         console = TRUE)

# Result: groups are the same

TukeyHSD(mod8,
         which = "Date")

# Result: p = 0.0996102,
# The Betula pubescens trees inoculated with pellets in Site 2 have not significantly grown 

# Plot data height ----
ggplot(dundreggan_data_pellet2_removed) +
  aes(x = Date, y = Height..cm., fill = Date) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Date") +                # specifying title of legend
  labs(title = "Betula pubescens 2023 & 2024 Tree Measurements in Pellet Plot 2", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff



###########################

