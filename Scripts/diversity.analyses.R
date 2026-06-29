# Code to calculate diversity metrics

library(vegan)

# https://rpubs.com/an-bui/vegan-cheat-sheet for example of community data matrix

# reading
# Morris et al. (2014): https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.1155
# Roswell et al. (2021): https://nsojournals.onlinelibrary.wiley.com/doi/10.1111/oik.07202

# The first step to using vegan is to get the plot data into a new format
# Community Data Matrix: plots are the rows and species are the columns
# Each cell is how many individuals of that species are in that plot

# read in the community data matrix
BCI = read.csv("cdm.csv", header = T, row.names = 1)

# example data from package
simp.div = diversity(cdm, index = "simpson")


# species richness: Number of species present in each plot
# simplest metric used to represent diversity
sppr = specnumber(BCI)

# finds frequencies of species (number of plots with species present)
sppr.freq = specnumber(BCI, MARGIN = 2)

# percent of each species in each plot
sp.percent = BCI / rowSums(BCI)*100
  
# Shannon or Shannon-Weaver or Shannon-Wiener diversity:
# foundations in information theory
# represents the uncertainty about the identify of an unknown individual
# In a highly diverse (evenly distributed) system, an unknown individual
# could belong to any species, leading to a high uncertainty in predictions of its identity
# In a less diverse system, dominated by 1 or a few species, it is easier to predict
# the identify of unknown individuals and there is less uncertainty in the system.
# Metric is equally sensitive to rare and abundant species
# Higher value = More diverse system
shannon.div = diversity(BCI, index = "shannon")

# Simpson diversity:
# Represents the probability that two randomly chosen individuals belong to different species
# sensitive to abundant species
# Higher value = More diverse system
simp.div = diversity(BCI, index = "simpson")

# Inverse Simpson:
# sensitive to abundant species
# Higher value = More diverse system
invsimp.div = diversity(BCI, index = "invsimpson")


# Pielou's evenness (J):
J = shannon.div/log(sppr)

# Simpson's Evenness (E):
# degree to which individuals are split among species with low values indicating
# that relatively equal numbers of individuals belong to each species.
E = invsimp.div/sppr



