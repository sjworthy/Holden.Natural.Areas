# This code will:
# 1. calculate above ground biomass of each tree
# 2. convert ABG to carbon storage
# 3. summarize data for each plot
# 4. evaluate relationships between plot storage and diversity


library(tidyverse)
#install.packages("pak")
#pak::pak("ropensci/allodb")
library(allodb) # estimate above ground biomass
# https://github.com/ropensci/allodb
library(corrplot) # evaluate correlations
library(performance) # test model assumptions
library(lmerTest)

#### Calculate Biomass ####

# Prior to calculating tree biomass, users need to provide a table with 
# DBH(cm), parsed species Latin names, and site(s) coordinates. 
dat = read.csv("Raw.Data/Plot_Census_2026.csv")

# this function calculate biomass of each tree and adds a new column with the info
dat$agb =
  get_biomass(
    dbh = dat$DBH.cm,
    genus = dat$Genus,
    species = dat$Sp,
    coords = c(-81.3, 41.6))

# Will need to calculate ABG for each plot and then for each forest type

plot.agb = dat %>% 
  group_by(Plot) %>% 
  summarise(total.abg = sum(agb))

plot.agb$ForestType = c("HH","OH","OH","MM","OH","MM","MM","BM","BM","HH","HH","BM")

forest.type.agb = dat %>% 
  group_by(ForestType) %>% 
  summarise(total.abg = sum(agb))

# mixed meso has highest above ground biomass

ggplot(plot.agb, aes(y = total.abg, x = ForestType))+
  geom_boxplot()

#### Calculate Carbon Storage ####
# multiply ABG by 0.5
dat$carbon_storage = dat$agb*0.5

plot.carbon = dat %>% 
  group_by(Plot) %>% 
  summarise(total.carbon.storage = sum(carbon_storage))

plot.carbon$ForestType = c("HH","OH","OH","MM","OH","MM","MM","BM","BM","HH","HH","BM")

forest.type.carbon = dat %>% 
  group_by(ForestType) %>% 
  summarise(total.carbon.storage = sum(carbon_storage))
# mixed mesophytic highest stored carbon

ggplot(plot.carbon, aes(y = total.carbon.storage, x = ForestType))+
  geom_boxplot()

# checking for total carbon differs between forest types

carbon.forest.type.aov = aov(total.carbon.storage ~ ForestType, data = plot.carbon)
summary(carbon.forest.type.aov) # nonsignificant

all.carbon.plot = merge(plot.agb,plot.carbon)
all.carbon.forest.type = merge(forest.type.agb,forest.type.carbon)

write.csv(all.carbon.plot, file = "./Formatted.Data/all.carbon.plot.csv")
write.csv(all.carbon.forest.type, file = "./Formatted.Data/all.carbon.forest.type.csv")

#### Evaluate Relationships ####

# read in all data
all.plot.final = read.csv("Formatted.Data/all.plot.final.csv")
all.plot.final$ForestType = as.factor(all.plot.final$ForestType)
all.forest.type.final = read.csv("Formatted.Data/all.forest.type.final.csv")

# test for correlations among the diversity measures
cor.div.dat = cor(all.plot.final[,c(2:7)],use = "pairwise") 
corrplot(cor.div.dat, method="number",tl.col = "black", bg = "gray70",is.corr = TRUE,
         col.lim = c(-1,1), col = COL2('BrBG', 200), addgrid.col = "black")

# formerly test the significance of the correlation
cor.test(all.plot.final$shannon.div.plot,all.plot.final$E.plot)
# not significantly correlated, p = 0.08

# Is there a relationship between carbon storage and species diversity?

# linear mixed effects model - accounts for plots being grouped into forest types
rich.lmer = lmer(total.carbon.storage ~ sppr.plot + (1|ForestType), data = all.plot.final)
summary(rich.lmer)
model_performance(rich.lmer)

rich.effects = allEffects(rich.lmer)
rich.effects.2 = rich.effects$sppr.plot

ggplot(all.plot.final, aes(y = total.carbon.storage, x = sppr.plot, color = ForestType))+
  geom_point()+
  geom_line(data = rich.effects.2, aes(x = sppr.plot, y = fit), inherit.aes = FALSE)+
  geom_ribbon(data = rich.effects.2, aes(x = sppr.plot, ymin = lower, ymax = upper),
              inherit.aes = FALSE, alpha = 0.2)+
  theme_classic(base_size = 15)

shannon.lmer = lmer(total.carbon.storage ~ shannon.div.plot + (1|ForestType), data = all.plot.final)
summary(shannon.lmer)
model_performance(shannon.lmer)

simp.lmer = lmer(total.carbon.storage ~ simp.div.plot + (1|ForestType), data = all.plot.final)
summary(simp.lmer)
model_performance(simp.lmer)

invsimp.lmer = lmer(total.carbon.storage ~ invsimp.div.plot + (1|ForestType), data = all.plot.final)
summary(invsimp.lmer)
model_performance(invsimp.lmer)

J.lmer = lmer(total.carbon.storage ~ J.plot + (1|ForestType), data = all.plot.final)
summary(J.lmer)
model_performance(J.lmer)

E.lmer = lmer(total.carbon.storage ~ E.plot + (1|ForestType), data = all.plot.final)
summary(E.lmer)
model_performance(E.lmer)

