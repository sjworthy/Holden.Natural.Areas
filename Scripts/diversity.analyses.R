# Code to calculate diversity metrics

library(vegan)

# https://rpubs.com/an-bui/vegan-cheat-sheet for example of community data matrix

# reading
# Morris et al. (2014): https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.1155
# Roswell et al. (2021): https://nsojournals.onlinelibrary.wiley.com/doi/10.1111/oik.07202

# The first step to using vegan is to get the plot data into a new format
# Community Data Matrix: plots are the rows and species are the columns
# Each cell is how many individuals of that species are in that plot

# fake data from vegan package if you want to play around with it. 
data(BCI)

# read in the community data matrix
# plots
plot.cdm = read.csv("Formatted.Data/plot_cdm.csv", header = T, row.names = 1)

# forest types
forest.type.cdm = read.csv("Formatted.Data/forest_type_cdm.csv", header = T, row.names = 1)

# interesting things
# Oak-Hickory only type with Amelanchier arborea, Cornus florida (1), Quercus alba, Carya ovata
# Beech-Maple only type with Juglans nigra (6 in one plot)
# Hemlock-Hardwood only type with Aesculus glabra (2 in one plot), Betula alleghaniensis (1), Ulmus americana (6 in one plot)
# Mixed-Meso only type with Fraxinus spp. (2, need to be IDed)

# species richness: Number of species present in each plot
# simplest metric used to represent diversity
sppr.plot = specnumber(plot.cdm)
sppr.forest.type = specnumber(forest.type.cdm)

# finds frequencies of species (number of plots with species present)
sppr.plot.freq = specnumber(plot.cdm, MARGIN = 2)
sppr.plot.percent = sppr.plot.freq/12

# Acer saccharum and Fagus grandifolia are the only species in all 12 plots

sppr.forest.type.freq = specnumber(forest.type.cdm, MARGIN = 2)
sppr.forest.type.percent = sppr.forest.type.freq/4

# percent of each species in each plot
sp.percent.plot = plot.cdm/rowSums(plot.cdm)*100
colMeans(sp.percent.plot)
# Acer saccharum is on average 49% of all trees in a plot
# Fagus grandifolia is on average 15% of all trees in a plot
  
# Shannon or Shannon-Weaver or Shannon-Wiener diversity:
# foundations in information theory
# represents the uncertainty about the identify of an unknown individual
# In a highly diverse (evenly distributed) system, an unknown individual
# could belong to any species, leading to a high uncertainty in predictions of its identity
# In a less diverse system, dominated by 1 or a few species, it is easier to predict
# the identify of unknown individuals and there is less uncertainty in the system.
# Metric is equally sensitive to rare and abundant species
# Higher value = More diverse system
shannon.div.plot = diversity(plot.cdm, index = "shannon")
shannon.div.forest.type = diversity(forest.type.cdm, index = "shannon")
# Hemlock Hardwood highest diversity

# Simpson diversity:
# Represents the probability that two randomly chosen individuals belong to different species
# sensitive to abundant species
# Higher value = More diverse system
simp.div.plot = diversity(plot.cdm, index = "simpson")
simp.div.forest.type = diversity(forest.type.cdm, index = "simpson")
# Hemlock Hardwood highest diversity

# Inverse Simpson:
# sensitive to abundant species
# Higher value = More diverse system
invsimp.div.plot = diversity(plot.cdm, index = "invsimpson")
invsimp.div.forest.type = diversity(forest.type.cdm, index = "invsimpson")
# Hemlock Hardwood highest diversity

# Pielou's evenness (J):
J.plot = shannon.div.plot/log(sppr.plot)
J.forest.type = shannon.div.forest.type/log(sppr.forest.type)
# Hemlock Hardwood highest evenness (1 plot less even)

# Simpson's Evenness (E):
# degree to which individuals are split among species with high values indicating
# that relatively equal numbers of individuals belong to each species.
E.plot = invsimp.div.plot/sppr.plot
E.forest.type = invsimp.div.forest.type/sppr.forest.type
# Hemlock Hardwood highest evenness, but mix of forest types with plots with highest evenness

# combine all the metrics into one data frame and save
all.div.plot = as.data.frame(cbind(sppr.plot,
                                   shannon.div.plot,simp.div.plot,
                                   invsimp.div.plot,J.plot,E.plot))
all.div.forest.type = as.data.frame(cbind(sppr.forest.type,
                                   shannon.div.forest.type,simp.div.forest.type,
                                   invsimp.div.forest.type,J.forest.type,E.forest.type))

write.csv(all.div.plot, file = "./Formatted.Data/all.div.plot.csv")
write.csv(all.div.forest.type, file = "./Formatted.Data/all.div.forest.type.csv")

