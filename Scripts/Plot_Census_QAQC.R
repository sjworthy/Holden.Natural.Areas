# Code to QAQC plot census data
# code will be adapted to compare data across censuses

library(tidyverse)

# read in the data and inspect
dat = read.csv("./Raw.Data/Plot_Census_2026.csv")
str(dat)
summary(dat)

# detect duplicates
dat %>%
  count(StemID) %>%
  filter(n > 1)

# make sure species codes are entered consistently
table(dat$Species)

# get unique species names
sort(unique(dat$Species))

# check if any value in any column is NA
table(is.na(dat))

# check range of DBH
# no value should be < 1
range(dat$DBH.cm)
# 1.0 to 134.8

# check to see if two decimal places were entered, should only be 1
dat %>%
  filter(DBH.cm != round(DBH.cm, 1))

# look to see if there is species-specific outlier detection
trees.qaqc <- dat %>%
  group_by(Plot,Species) %>%
  mutate(
    mean.dbh = mean(DBH.cm, na.rm = TRUE),
    sd.dbh = sd(DBH.cm, na.rm = TRUE),
    z = (DBH.cm - mean.dbh) / sd.dbh,
    flag.outlier = abs(z) > 3)

table(trees.qaqc$flag.outlier)
# flagging a lot of larger ACESAC

# Visualize DBH for species
ggplot(dat, aes(x = Species, y = DBH.cm)) +
  geom_boxplot()+
  theme_classic(base_size = 15)

ggplot(dat, aes(x = Species, y = DBH.cm)) +
  geom_boxplot()+
  theme_classic(base_size = 40)+
  facet_wrap(~Plot)
