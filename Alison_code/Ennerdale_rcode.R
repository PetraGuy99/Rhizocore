# Ennerdale Tree Measuring Data
 # 10/04/2024
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

ennerdale_data <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/ennerdale_data.csv")


# Check data ----

 # We need species to be a character, treatment is a factor , height to be an integer
str(ennerdale_data)

 # Change treatment from character to factor
ennerdale_data$Treatment <- as.factor(ennerdale_data$Treatment) 

 # Make 3 different datasets for each tree species
ennerdale_data_pine = subset(ennerdale_data, Species == "Scots Pine")
ennerdale_data_birch = subset(ennerdale_data, Species == "Birch")
ennerdale_data_willow = subset(ennerdale_data, Species == "Willow")


############################

# Pinus sylvestris analysis ----

 # Aim: I want to see whether the treatment has an effect on tree height

# Box plots to check for outliers ----
ggplot(ennerdale_data_pine) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Result: Looks OK! 

# Levens’s test ----

 # Levene’s test is an inferential statistic used to check if the variances of a variable 
  # obtained for two or more groups are equal or not when data comes from a non-normal distribution. 
 # Levene’s test is used to check the assumptions that the variances of the populations 
  # from different samples drawn are equal or not before running the test like ANOVA. 
   # It tests the null hypothesis that the population variances are equal or not, It is known as 
    # homoscedasticity.  It’s an alternative to Bartlett’s test that is less sensitive to 
     # departures from normality.


 # If the hypothesis of equal variances is rejected, another version of the ANOVA can be used: the Welch ANOVA 
r <- leveneTest(Height..cm. ~ Treatment,
            data = ennerdale_data_pine)
r

 # Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod <- aov(Height..cm. ~ Treatment,
           data = ennerdale_data_pine)

 # Options for testing normality

 # In the case of large samples, normality is not required.
  # Once source states: If the number of observations in each group/sample is large (usually n≥30)
    # normality is not required
  # Another source states: Central limit theorem states that when sample size has 100 
   # or more observations, violation of the normality is not a major issue.
 # Importantly though, it is always good practice even with a large sample.

table(ennerdale_data_pine$Treatment) # We want populations to be similar in size
plot(mod, which = 2) # Looking for points to follow the reference line
plot(mod, which = 3) # If homogeneity of variances was violated, the red line would not be flat (horizontal).
hist(mod$residuals) # Looking for a normal distribution
shapiro.test(mod$residuals) # Null hypothesis states that data are taken from normal 
                             # distributed population. When P > 0.05, null hypothesis 
                              # accepted and data are called as normally distributed.

 # Though the histogram is slightly off and in the QQ plot the points move off the reference line.
 # The Shapiro-Wilk test shows we can accept the null hypothesis (by the skin of our teeth!).
 # Additionally the homogeneity of variance shows there is no evident relationships 
  # between residuals and fitted values (the mean of each group), so homogeneity of variances is assumed. 

 # In this instance we can check if a logarithmic transformation will
  # transform skewed data to approximately conform to normality. 

# Logarithmic Model ----
x<-abs(min(ennerdale_data_pine$Height..cm.))+1
mod1<-aov(log(Height..cm.+x)~Treatment, data=ennerdale_data_pine)

 # Testing normality of log model
plot(mod1, which = 2)
plot(mod1, which = 3)
hist(mod1$residuals)
shapiro.test(mod1$residuals)

 # The histogram appears to be more "normal" and the Shapiro-Wilk test has a higher p value.
 # Result: the logarithmic model fits better.


# Descriptive stats ----
  # mean by group
aggregate(Height..cm. ~ Treatment,
          data = ennerdale_data_pine,
          FUN = mean)

 # Results:
  #   Treatment Height..cm.
  # 1   Control    46.11628
  # 2 Treatment    36.09091

# Plot data height ----
ggplot(ennerdale_data_pine) +
  aes(x = Treatment, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment") +                # specifying title of legend
  labs(title = "Pinus sylvestris: Height comparison between control and treatment plot", 
       x = "\n Nitrogen Content (mg)", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

# ANOVA summary ----
summary(mod)  # Result: p = 9.99e-07 ***
summary(mod1) # Result: p = 8.64e-07 ***

# HSD test ----
HSD.test(mod,
         trt = c("Treatment"),
         console = TRUE) 

HSD.test(mod1,
         trt = c("Treatment"),
         console = TRUE)
 # Result for both: separate a and b groups.
 # We can accept that the control Pinus sylvestris trees are significantly taller than the treatment trees

############################

# Betula pendula analysis ----

 # Aim: I want to see whether the treatment has an effect on tree height

# Box plot to check for outliers ----
ggplot(ennerdale_data_birch) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Result: There appear to be two in the treatment box plot.

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(ennerdale_data_birch$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(ennerdale_data_birch$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 54.6 
low<- Q[1]-1.5*iqr # Lower Range = 3.62

# It doesn't look like I need to remove the outliers, they both appear to be below 54.6.

# I can remove any outwith the defined range using this line of code
ennerdale_data_birch_removed<- subset(ennerdale_data_birch, ennerdale_data_birch$Height..cm. > 
                                        (Q[1] - 1.5*iqr) & ennerdale_data_birch$Height..cm. < (Q[2]+1.5*iqr))

# Check if outliers have been removed.
ggplot(ennerdale_data_birch_removed) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Result: They have not so they're fine to keep.

# Levens’s test ----
r2 <- leveneTest(Height..cm. ~ Treatment,
                data = ennerdale_data_birch)
r2

 # Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod2 <- aov(Height..cm. ~ Treatment,
            data = ennerdale_data_birch)
# Testing normality
table(ennerdale_data_birch$Treatment)
plot(mod2, which = 2)
plot(mod2, which = 3)
hist(mod2$residuals)
shapiro.test(mod2$residuals)

 # Result: Very much the same conclusions as the Pinus sylvestris model. 
 # Would be beneficial to try the logarithmic transformation. 
 # Need to note the control sample size is extremely low (n=7).

# Logarithmic Model ----
x2<-abs(min(ennerdale_data_birch$Height..cm.))+1
mod3<-aov(log(Height..cm.+x2)~Treatment, data=ennerdale_data_birch)

# Testing normality of log model
plot(mod3, which = 2)
plot(mod3, which = 3)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

 # Result: Very much the same conclusions as the Pinus sylvestris model
 # Histogram is more "normal" and p-value is higher.

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ennerdale_data_birch,
          FUN = mean)

 # Result
  # Treatment Height..cm.
  # 1   Control    29.57143
  # 2 Treatment    30.52941

# Plot data height ----
ggplot(ennerdale_data_birch) +
  aes(x = Treatment, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment") +                # specifying title of legend
  labs(title = "Betula pendula: Height comparison between control and treatment plot", 
       x = "\n Nitrogen Content (mg)", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

# ANOVA summary ----
summary(mod2) # p = 0.851
summary(mod3) # p = 0.652

# HSD test ----
HSD.test(mod2,
         trt = c("Treatment"),
         console = TRUE)
  # Result: No significant difference between groups

HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)
 # Result: No significant difference between groups

 # The Betula pendula trees in the control and treatment plots are not 
  # significantly different in height.

############################

# Salix sp analysis ----

 # Aim: I want to see whether the treatment has an effect on tree height

# Box plots to check for outliers ----
ggplot(ennerdale_data_willow) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Defining and removing outliers ----

# First find the 25th and the 75th percentile of the dataset.
Q <- quantile(ennerdale_data_willow$Height..cm., probs=c(.25, .75), na.rm = FALSE)

# the IQR() function gives me the difference of the 75th and 25th percentiles.
iqr <- IQR(ennerdale_data_willow$Height..cm.)

# Find the cut-off ranges beyond which all data points are outliers.
# Results: found in the global environment
up <-  Q[2]+1.5*iqr # Upper Range = 58
low<- Q[1]-1.5*iqr # Lower Range = 10

# It appears I need to remove the outlier in the treatment box plot.

# I can remove any outwith the defined range using this line of code
ennerdale_data_willow_removed<- subset(ennerdale_data_willow, ennerdale_data_willow$Height..cm. > 
                                         (Q[1] - 1.5*iqr) & ennerdale_data_willow$Height..cm. < (Q[2]+1.5*iqr))

# Check if the outlier has been removed.
ggplot(ennerdale_data_willow_removed) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

 # Result: It has, so I'm going to now use the new data set without it.

# Levens’s test ----
r3 <- leveneTest(Height..cm. ~ Treatment,
                 data = ennerdale_data_willow)
r3

 # Result: The homogeneity of variance hypothesis is accepted.

# Model ----
mod4 <- aov(Height..cm. ~ Treatment,
            data = ennerdale_data_willow_removed)

# Testing normality
table(ennerdale_data_willow_removed$Treatment)
plot(mod4, which = 2)
plot(mod4, which = 3)
hist(mod4$residuals)
shapiro.test(mod4$residuals)

 # Result: histogram is swayed to the left and the Shapiro-Wilk test accepts the null hypothesis.
  # We can use this model but I'm going to do a logarithmic transformation to find a better fit

# Logarithmic Model ----
  x3<-abs(min(ennerdale_data_willow_removed$Height..cm.))+1
mod5<-aov(log(Height..cm.+x2)~Treatment, data=ennerdale_data_willow_removed)

# Testing normality of log model
plot(mod5, which = 2)
plot(mod5, which = 3)
hist(mod5$residuals)
shapiro.test(mod5$residuals)

# Result: Histogram is better, not amazing. Shapiro- Wilk test accepts the null-hypothesis, 
 # p-value is higher. 

# Descriptive stats ----
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ennerdale_data_willow_removed,
          FUN = mean)

 # Results
  # Treatment Height..cm.
  # 1   Control    39.19231
  # 2 Treatment    28.42857

# Plot data height ----
ggplot(ennerdale_data_willow_removed) +
  aes(x = Treatment, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Treatment") +                # specifying title of legend
  labs(title = "Salix sp: Height comparison between control and treatment plot", 
       x = "\n Nitrogen Content (mg)", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

# ANOVA summary ----
summary(mod4) # p = 5.69e-06 ***
summary(mod5) # p = 3.34e-06 ***

# HSD test ----
HSD.test(mod4,
         trt = c("Treatment"),
         console = TRUE)

HSD.test(mod5,
         trt = c("Treatment"),
         console = TRUE)

 # Result for both: separate a and b groups.
 # We can accept that the control Salix sp trees are significantly taller than the treatment trees




###########################

