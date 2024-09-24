## Cheshire dataset
# 30.03.22
# Alison Stewart
# alison@rhizocore.com 

# Download packages ----
  # Tidyverse purpose is for data manipulation in the csv file + viewing data
library(tidyverse)

# Modern Farm Basic ----
# Input data
  # Change this for your laptop by going on the bottom right clicking file - locating file then clicking
  # then selecting import dataset and copy and paste title.
setwd("~/Desktop/rstudio") 
mod_farm <- read.csv("~/Desktop/rstudio/Cheshire_Modern_Farm_Dataset - Master.csv")


# View data
view(mod_farm)
head(mod_farm)
tail(mod_farm)
str(mod_farm) # shows class of each column 

# Manipulating data 
  # changing block, tree number, tree_species and strain to factors
  # because summary function works best when they are.
mod_farm$Block <- as.factor(mod_farm$Block) 
mod_farm$Tree_number <- as.factor(mod_farm$Tree_number)   
mod_farm$Tree_species <- as.factor(mod_farm$Tree_species)   
mod_farm$Strain <- as.factor(mod_farm$Strain)   
str(mod_farm)

# View data - part 2 
summary(mod_farm) # shows all basic data values inc means, total in each sections etc...
summary(mod_farm$Tree_number) # *now fixed* from this we can see that, D29, O29, O30, O78, O79, S5 have duplicates
summary(mod_farm$Tree_species)

# Modern Farm Further Data Manipulation ----
  # Manipulating data 2 
  # Filtering species specific data
# I'm pretty sure we need to create objects for each one, but surely there must be a quicker way when it comes to next part??
Downy_birch <- filter(mod_farm, Tree_species == "Downy_birch")
Hazel <- filter(mod_farm, Tree_species == "Hazel")
Oak <- filter(mod_farm, Tree_species == "Oak")
Silver_birch <- filter(mod_farm, Tree_species == "Silver_birch")

# Filtering for treatments 
Downy_birch_c <- filter(Downy_birch, Strain == "Control")
Downy_birch_7 <- filter(Downy_birch, Strain == "PAX_302_007")
Downy_birch_2 <- filter(Downy_birch, Strain == "PAX_302_002")

Oak_c <- filter(Oak, Strain == "Control")
Oak_7 <- filter(Oak, Strain == "PAX_302_007")
Oak_2 <- filter(Oak, Strain == "PAX_302_002")

Hazel_c <- filter(Hazel, Strain == "Control")
Hazel_7 <- filter(Hazel, Strain == "PAX_302_007")
Hazel_2 <- filter(Hazel, Strain == "PAX_302_002")

Silver_birch_c <- filter(Silver_birch, Strain == "Control")
Silver_birch_7 <- filter(Silver_birch, Strain == "PAX_302_007")
Silver_birch_2 <- filter(Silver_birch, Strain == "PAX_302_002")
# Now we can use summary for each individual species to get min max 
summary(Downy_birch_c)
summary(Downy_birch_7)
summary(Downy_birch_2)

summary(Oak_c)
summary(Oak_7)
summary(Oak_2)

summary(Hazel_c)
summary(Hazel_7)
summary(Hazel_2)

summary(Silver_birch_c)
summary(Silver_birch_7)
summary(Silver_birch_2)

# Woodstock ----
# Input data
woodstock<- read.csv("~/Desktop/rstudio/Cheshire_Woodstock_Dataset - Master_Woodstock.csv")

# View data
view(woodstock)
head(woodstock)
tail(woodstock)
str(woodstock)

# Manipulating data 
# changing block, tree number, tree_species and strain to factors
woodstock$Block <- as.factor(woodstock$Block) 
woodstock$Tree_number <- as.factor(woodstock$Tree_number)   
woodstock$Tree_species <- as.factor(woodstock$Tree_species)   
woodstock$Strain <- as.factor(woodstock$Strain)   
str(woodstock)

# View data - part 2 
summary(woodstock) 
summary(woodstock$Tree_number) # fixed issues in google doc

# Woodstock Farm Further Data Manipulation ----
  # Manipulating data 2 
  # Filtering species specific data
# PLEASE NOTE: For simplicity of code I am reusing objects: meaning they will be overwritten!
Downy_birch <- filter(woodstock, Tree_species == "Downy_birch")
Oak <- filter(woodstock, Tree_species == "Oak")
# No Hazel
Silver_birch <- filter(woodstock, Tree_species == "Silver_birch")

# Filtering for treatments 
Downy_birch_c <- filter(Downy_birch, Strain == "Control")
Downy_birch_7 <- filter(Downy_birch, Strain == "PAX_302_007")
Downy_birch_2 <- filter(Downy_birch, Strain == "LAC_302_031") # again not changing 2 to LAC for quickness

Oak_c <- filter(Oak, Strain == "Control")
Oak_7 <- filter(Oak, Strain == "PAX_302_007")
Oak_2 <- filter(Oak, Strain == "LAC_302_031")

Silver_birch_c <- filter(Silver_birch, Strain == "Control")
Silver_birch_7 <- filter(Silver_birch, Strain == "PAX_302_007")
Silver_birch_2 <- filter(Silver_birch, Strain == "LAC_302_031")
# Now we can use summary for each individual species to get min max 
summary(Downy_birch_c)
summary(Downy_birch_7)
summary(Downy_birch_2)

summary(Oak_c)
summary(Oak_7)
summary(Oak_2)

summary(Silver_birch_c)
summary(Silver_birch_7)
summary(Silver_birch_2)
# Davenham parish 1 ----
# Input data
dav_p1 <- read.csv("~/Desktop/rstudio/Davenham_Parish_site_1_dataset - DavP1_Master.csv")

# View data
view(dav_p1)
head(dav_p1)
tail(dav_p1)
str(dav_p1) 

# Manipulating data 
# changing block, tree number, tree_species and strain to factors
dav_p1$Block <- as.factor(dav_p1$Block) 
dav_p1$Tree.number <- as.factor(dav_p1$Tree.number) # for some reason this needs to be a "." not a "_" 
dav_p1$Tree_species <- as.factor(dav_p1$Tree_species)   
dav_p1$Strain <- as.factor(dav_p1$Strain)   
str(dav_p1)

# View data - part 2 
summary(dav_p1) 
summary(dav_p1$Tree_number) 

# Davenham parish 1 Further Data Manipulation ----
  # Manipulating data 2 
  # Filtering species specific data
# PLEASE NOTE: For simplicity of code I am reusing objects: meaning they will be overwritten!
Downy_birch <- filter(dav_p1, Tree_species == "Downy_birch")
Hazel <- filter(dav_p1, Tree_species == "Hazel")
Oak <- filter(dav_p1, Tree_species == "Oak")
Silver_birch <- filter(dav_p1, Tree_species == "Silver_birch")

# Filtering for treatments 
Downy_birch_c <- filter(Downy_birch, Strain == "Control")
Downy_birch_7 <- filter(Downy_birch, Strain == "HEB-302-003")

Oak_c <- filter(Oak, Strain == "Control")
Oak_7 <- filter(Oak, Strain == "HEB-302-003")

Hazel_c <- filter(Hazel, Strain == "Control")
Hazel_7 <- filter(Hazel, Strain == "HEB-302-003")

# no control data
Silver_birch_7 <- filter(Silver_birch, Strain == "HEB-302-003")

# Now we can use summary for each individual species to get min max 
summary(Downy_birch_c)
summary(Downy_birch_7)

summary(Oak_c)
summary(Oak_7)

summary(Hazel_c)
summary(Hazel_7)

summary(Silver_birch_7)

# Davenham parish 2 ----
# Input data
dav_p2 <- read.csv("~/Desktop/rstudio/Davenham_Parish_site_2_datasetet - DavP2_Master.csv")

# View data
view(dav_p2)
head(dav_p2)
tail(dav_p2)
str(dav_p2) # shows class of each column 

# Manipulating data 
# changing block, tree number, tree_species and strain to factors
dav_p2$Block <- as.factor(dav_p2$Block) 
dav_p2$Tree.number <- as.factor(dav_p2$Tree.number) # Again r has decided to input with a "." instead of "_"
dav_p2$Tree_species <- as.factor(dav_p2$Tree_species)   
dav_p2$Strain <- as.factor(dav_p2$Strain)   
str(dav_p2)

# View data - part 2 
summary(dav_p2) 
summary(dav_p2$Tree.number) 

# Davenham parish 2 Further Data Manipulation ----
  # Manipulating data 2 
  # Filtering species specific data
# PLEASE NOTE: For simplicity of code I am reusing objects: meaning they will be overwritten!
Downy_birch <- filter(dav_p2, Tree_species == "Downy_birch")
Hazel <- filter(dav_p2, Tree_species == "Hazel")
Oak <- filter(dav_p2, Tree_species == "Oak")
Silver_birch <- filter(dav_p2, Tree_species == "Silver_birch")

# Filtering for treatments 
Downy_birch_c <- filter(Downy_birch, Strain == "Control")
Downy_birch_7 <- filter(Downy_birch, Strain == "HEB-302-003")

Oak_c <- filter(Oak, Strain == "Control")
Oak_7 <- filter(Oak, Strain == "HEB-302-003")

Hazel_c <- filter(Hazel, Strain == "Control")
Hazel_7 <- filter(Hazel, Strain == "HEB-302-003")

Silver_birch_c <- filter(Silver_birch, Strain == "Control")
Silver_birch_7 <- filter(Silver_birch, Strain == "HEB-302-003")

# Now we can use summary for each individual species to get min max 
summary(Downy_birch_c)
summary(Downy_birch_7)

summary(Oak_c)
summary(Oak_7)

summary(Hazel_c)
summary(Hazel_7)

summary(Silver_birch_c)
summary(Silver_birch_7)


