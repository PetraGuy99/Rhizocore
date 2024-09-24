## Ireland Tree Measuring Data
# 30/04/2024
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

ireland <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/Ireland_2024_master.csv")
irelandgroup <- read.csv("~/Desktop/Rhizocore/R_studio_csv_files/Ireland_groupincrease_master.csv")


# Check data ----
## we need species to be a character, treatment is a factor , height to be an interger
str(ireland)
str(irelandgroup)
# str(ireland_oak_group) not used

# Change treatment and block from character to factor
ireland$Treatment <- as.factor(ireland$Treatment) 
ireland$Block <- as.factor(ireland$Block) 
ireland$Increase_24 <- as.integer(ireland$Increase_24) 

irelandgroup$Treatment <- as.factor(irelandgroup$Treatment) 
irelandgroup$Block <- as.factor(irelandgroup$Block) 
irelandgroup$Date <- as.factor(irelandgroup$Date) 


# Remove NA and 0 values
ireland <- na.omit(ireland)
ireland = subset(ireland, ireland$Increase_24 > 1 ) 

irelandgroup <- na.omit(irelandgroup)

# Make a new data set for each species (not including willow cutting, not enough data points)
ireland_oak = subset(ireland, Species == "Oak")
ireland_birch = subset(ireland, Species == "Birch")
ireland_alder = subset(ireland, Species == "Alder")
ireland_hazel = subset(ireland, Species == "Hazel")
ireland_scotspine = subset(ireland, Species == "Scots pine")
ireland_willownursery = subset(ireland, Species == "Willow Nursery")

# Make a new data set for each species (not including willow cutting, not enough data points)
ireland_oak_group = subset(irelandgroup, Species == "Oak")
ireland_birch_group = subset(irelandgroup, Species == "Birch")
ireland_alder_group = subset(irelandgroup, Species == "Alder")
ireland_hazel_group = subset(irelandgroup, Species == "Hazel")
ireland_scotspine_group = subset(irelandgroup, Species == "Scots pine")
ireland_willownursery_group = subset(irelandgroup, Species == "Willow Nursery")

# Stats: specific tree height increase ----
## Oak ----
# Testing normality
table(ireland_oak$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Treatment,
            data = ireland_oak)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_oak) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Treatment,
          data = ireland_oak,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_oak) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Oak Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Birch ----
# Testing normality
table(ireland_birch$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Treatment,
            data = ireland_birch)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_birch) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Treatment,
          data = ireland_birch,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_birch) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Birch Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Alder ----
# Testing normality
table(ireland_alder$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Treatment,
            data = ireland_alder)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_alder) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Treatment,
          data = ireland_alder,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_alder) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Alder Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Hazel ----
# Testing normality
table(ireland_hazel$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Block * Treatment,
            data = ireland_hazel)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_hazel) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Block * Treatment,
          data = ireland_hazel,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_hazel) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Hazel Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff






## Scots Pine ----
# Testing normality
table(ireland_scotspine$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Treatment,
            data = ireland_scotspine)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_scotspine) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Treatment,
          data = ireland_scotspine,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_scotspine) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Scots pine Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff






## Willow Nursery ----
# Testing normality
table(ireland_willownursery$Treatment)

# Model
mod3 <- aov(Increase_24 ~ Treatment,
            data = ireland_willownursery)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_willownursery) +
  aes(x = Treatment, y = Increase_24) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Increase_24 ~ Treatment,
          data = ireland_willownursery,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test 
HSD.test(mod3,
         trt = c("Treatment"),
         console = TRUE)

TukeyHSD(mod3,
         which = "Treatment")

# Plot data height
ggplot(ireland_willownursery) +
  aes(x = Treatment, y = Increase_24, fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Willow Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff














# Stats: group tree height increase ----
## Oak ----
# Testing normality
table(ireland_oak_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment,
            data = ireland_oak_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_oak_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment * Date,
          data = ireland_oak_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test 
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

TukeyHSD(mod3)

# Plot data height
ggplot(ireland_oak_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Oak Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Birch ----
# Testing normality
table(ireland_birch_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment * Block,
            data = ireland_birch_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_birch_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ireland_birch_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test with blocks
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

# I'd need to do an individual control and treatment dataset for the tukey
# TukeyHSD(mod3,
#         which = c("Date", "Treatment", "Block"), ordered = FALSE, conf.level = 0.95)

# Plot data height
ggplot(ireland_birch_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Birch Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Alder ----
# Testing normality
table(ireland_alder_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment * Block,
            data = ireland_alder_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_alder_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ireland_alder_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test 
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

# TukeyHSD(mod3,
#         which = "Treatment")

# Plot data height
ggplot(ireland_alder_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Alder Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff

## Hazel ----
# Testing normality
table(ireland_hazel_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment * Block,
            data = ireland_hazel_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_hazel_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ireland_hazel_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test 
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

# TukeyHSD(mod3,
#         which = "Treatment")

# Plot data height
ggplot(ireland_hazel_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Hazel Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff






## Scots Pine ----
# Testing normality
table(ireland_scotspine_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment * Block,
            data = ireland_scotspine_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_scotspine_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ireland_scotspine_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test 
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

# TukeyHSD(mod3,
#         which = "Treatment")

# Plot data height
ggplot(ireland_scotspine_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Scots Pine Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff







## Willow Nursery ----
# Testing normality
table(ireland_willownursery_group$Treatment)

# Model
mod3 <- aov(Height..cm. ~ Date + Treatment * Block,
            data = ireland_willownursery_group)

# 3 options for testing normality
plot(mod3, which = 2)
hist(mod3$residuals)
shapiro.test(mod3$residuals)

# Homogeneity of variances
plot(mod3, which = 3)

# Box plots to check for outliers
# For treatment
ggplot(ireland_willownursery_group) +
  aes(x = Treatment, y = Height..cm.) +
  geom_boxplot()

# Descriptive stats
# mean by group
aggregate(Height..cm. ~ Treatment,
          data = ireland_willownursery_group,
          FUN = mean)

# ANOVA summary
summary(mod3)

# HSD test without block
HSD.test(mod3,
         trt = c("Date", "Treatment"),
         console = TRUE)

# HSD test 
HSD.test(mod3,
         trt = c("Date", "Treatment", "Block"),
         console = TRUE)

# TukeyHSD(mod3,
#         which = "Treatment")

# Plot data height
ggplot(ireland_willownursery_group) +
  aes(x = Date, y = Height..cm., fill = Treatment) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "#FAF0D9", # background beige
                                        colour = "white",
                                        size = 0.5, linetype = "solid")) +
  scale_fill_manual(values = c("#F9D23E", "#A9D53C"),     # specifying the colours
                    name = "Key", label = c("Control", "Treatment", "Control", "Treatment")) +                # specifying title of legend
  labs(title = " Ireland Willow Nursery Tree Measurements 2023 to 2024", 
       x = "\n Year", y = "Height (cm)\n") +
  theme(axis.text = element_text(size = 12, family= myfont), 
        axis.title = element_text(size = 12,family= myfont),
        plot.title = element_text(size = 14, family= myfont, hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold", family= myfont),
        legend.box.background = element_rect(color = "grey", size = 0.3),
        legend.text = element_text(family= myfont)) # font stuff
