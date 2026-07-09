# This code will:
# 1. calculate above ground biomass of each tree
# 2. convert ABG to carbon storage
# 3. summarize data for each plot
# 4. evaluate relationships between plot storage and diversity


library(tidyverse)
install.packages("pak")
pak::pak("ropensci/allodb")
library(allodb) # estimate above ground biomass
# https://github.com/ropensci/allodb
library(corrplot) # evaluate correlations
library(performance) # test model assumptions

#### Calculate Biomass ####

# Prior to calculating tree biomass, users need to provide a table with 
# DBH(cm), parsed species Latin names, and site(s) coordinates. 

# example data with the package
data("scbi_stem1")
# make this dataframe and also include two other columns for plot name and forest type

# to view the species with equations in the package
data("sitespecies")


# this function calculate biomass of each tree and adds a new column with the info
scbi_stem1$agb =
  get_biomass(
    dbh = scbi_stem1$dbh,
    genus = scbi_stem1$genus,
    species = scbi_stem1$species,
    coords = c(-78.2, 38.9))

# Will need to calculate ABG for each plot and then for each forest type

# make a box and whisker plot showing ABG for each forest type
# website with example and code: https://rpubs.com/an-bui/vegan-cheat-sheet
# will need ABG for each plot as input.

#### Calculate Carbon Storage ####
# multiply ABG by 0.5
scbi_stem1$carbon_storage = scbi_stem1$agb*0.5

# do the same totaling of carbon storage as above and generate box and whisker plot

#### Evaluate Relationships ####

# read in diversity calculations
div.dat = read.csv("div.dat.csv")

# need to combine the div.dat and carbon storage dataframes together
# Each row should be a plot
# Columns should be the diveristy values, carbon storage, forest type

# test for correlations among the diversity measures

cor.div.dat = cor(div.dat[,c(4:12)],use = "pairwise") 
corrplot(cor.div.dat, method="number",tl.col = "black", bg = "gray70",is.corr = TRUE,
         col.lim = c(-1,1), col = COL2('BrBG', 200), addgrid.col = "black")

# example from corrplot package
data(mtcars)
M = cor(mtcars)
corrplot(M, method = 'number')

# formerly test the significance of the correlation
cor.test(mtcars$mpg,mtcars$cyl)

# Is there a relationship between carbon storage and plot diversity?
# What is your hypothesis based on the literature?

rich.lm = lm(carbon_storage ~ sppr, data = all.data)
summary(rich.lm)
model_performance(rich.lm)


# linear mixed effects model - accounts for plots being grouped into forest types
rich.lmer = lmer(carbon_storage ~ sppr + (1|forest_type), data = all.data)
summary(rich.lmer)
model_performance(rich.lmer)







