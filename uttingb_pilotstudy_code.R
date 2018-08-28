#Code for first year pilot study

library(dplyr)
library(ggplot2)
library(HDInterval)
library(lme4)
library(MCMCpack)
library(multcomp)
library(readxl)
library(reshape2)

mydata <- read_excel("uttingb_pilotstudydata.xlsx")
View(mydata)

#######################################
#Exploratory Data Analysis
#######################################

mydata %>% 
  count(dataclass)

mydata %>%
  count(inventory)

mydata %>%
  count(coreNumber)

#######################################
#Subsetting data
#######################################

mydata$coreNumber <- factor(mydata$coreNumber)

core1 <- subset(mydata, coreNumber == "1")
core2 <- subset(mydata, coreNumber == "2")
core3 <- subset(mydata, coreNumber == "3")
core4 <- subset(mydata, coreNumber == "4")

freeHand <- subset(mydata, inventory == "freeHand")
biPolar <- subset(mydata, inventory == "biPolar")

compFlake <- subset(mydata, dataclass == "compFlake")
microFact <- subset(mydata, dataclass == "shatter" | dataclass == "splinter")
angularChunk <- subset(mydata, dataclass == "angularChunk")
pieceEsquille <- subset(mydata, dataclass == "pieceEsquille")
flakeFrag <- subset(mydata, dataclass == "flakeFrag")
core <- subset(mydata, dataclass =="core")

freeHand.compFlake <- subset(freeHand, dataclass == "compFlake")
biPolar.compFlake <- subset(biPolar, dataclass == "compFlake")

freeHand.microFact <- subset(freeHand, dataclass == "shatter" | dataclass == "splinter")
biPolar.microFact <- subset(biPolar, dataclass == "shatter" | dataclass == "splinter")

#####################################################
#Qualitative analyses: bipolar v freehand
#####################################################

#######################################
#Dataclass
#######################################

#Frequentist
xtab <- table(mydata$inventory, mydata$dataclass)
chisq.test(xtab,simulate.p.value=TRUE,B=10000)

#Bayesian
table(mydata$inventory, mydata$dataclass)

#Here, I use the Dirichlet distribution as a conjugate prior to derive 95% credibility intervals for frequencies of each dataclass

freeHand.dataclass <- c(1, 23, 2, 2, 2, 0, 10, 5)
biPolar.dataclass <- c(12, 21, 0, 1, 5, 7, 18, 4)

#angular chunk, complete flake, core, core frag, flake frag, piece esquille, shatter, splinter

res.freeHand.DC=rdirichlet(1000, alpha = freeHand.dataclass + 1)
res.biPolar.DC=rdirichlet(1000, alpha = biPolar.dataclass + 1)

#angular chunk
hdi(res.biPolar.DC[,1], credMass = 0.95)
#0.08801851, 0.25106176 
hdi(res.freeHand.DC[,1], credMass = 0.95)
#0.002082471, 0.092410569 

#complete flake
hdi(res.biPolar.DC[,2], credMass = 0.95)
#0.2000960, 0.4046125
hdi(res.freeHand.DC[,2], credMass = 0.95)
#0.3246097, 0.5902484

#core
hdi(res.biPolar.DC[,3], credMass = 0.95)
#2.247418e-05 4.059156e-02 
hdi(res.freeHand.DC[,3], credMass = 0.95)
#0.008671009 0.118258565 

#corefrag
hdi(res.biPolar.DC[,4], credMass = 0.95)
#0.001387619 0.063417982 
hdi(res.freeHand.DC[,4], credMass = 0.95)
#0.004154494 0.120439338

#flake frag
hdi(res.biPolar.DC[,5], credMass = 0.95)
#0.02623053 0.14346086
hdi(res.freeHand.DC[,5], credMass = 0.95)
#0.005093803 0.113178957

#pieceEsquillee
hdi(res.biPolar.DC[,6], credMass = 0.95)
#0.04428908 0.17098082
hdi(res.freeHand.DC[,6], credMass = 0.95)
#5.325402e-05 5.801998e-02 

#shatter
hdi(res.biPolar.DC[,7], credMass = 0.95)
#0.1541764 0.3426509 
hdi(res.freeHand.DC[,7], credMass = 0.95)
#0.1050915 0.3077477

#splinter
hdi(res.biPolar.DC[,8], credMass = 0.95)
#0.1541764 0.3426509 
hdi(res.freeHand.DC[,8], credMass = 0.95)
#0.1050915 0.3077477

#barplot
ggplot(mydata, aes(x = dataclass, group = inventory)) +
  geom_bar(aes(y = ..prop.., fill = inventory), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -.5) +
  scale_fill_manual(name = "Reduction Method", values = c("goldenrod3", "darkolivegreen4")) +
  ggtitle("Dataclass by Reduction Method") +
  xlab("Dataclass") +
  ylab("Percent") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~inventory) +
  theme_minimal()


#######################################
#Platform type 
#######################################

#Frequentist
xtab <- table(mydata$inventory, mydata$platformType)
chisq.test(xtab,simulate.p.value=TRUE,B=10000)

#Bayesian
table(mydata$inventory, mydata$platformType)

biPolar.platformType <- c(5, 8, 12, 0, 1, 4)
freeHand.platformType <- c(6, 0, 12, 1, 5, 1)

#cortical, crushed, flat, lipped, pointed, pointed+crushed

res.freeHand.plat=rdirichlet(1000, alpha = freeHand.platformType + 1)
res.biPolar.plat=rdirichlet(1000, alpha = biPolar.platformType + 1)

#cortical
hdi(res.biPolar.plat[,1], credMass = 0.95)
#0.06376183 0.29827619 
hdi(res.freeHand.plat[,1], credMass = 0.95)
#0.09630261 0.37151297

#crushed
hdi(res.biPolar.plat[,2], credMass = 0.95)
#0.1213218 0.3790378 
hdi(res.freeHand.plat[,2], credMass = 0.95)
#0.0000123399 0.0923066689 

#flat
hdi(res.biPolar.plat[,3], credMass = 0.95)
#0.2085698 0.5081046
hdi(res.freeHand.plat[,3], credMass = 0.95)
#0.2565483 0.6097997

#lipped
hdi(res.biPolar.plat[,4], credMass = 0.95)
#1.099229e-05 7.998083e-02
hdi(res.freeHand.plat[,4], credMass = 0.95)
#0.002008741 0.151924080 

#pointed
hdi(res.biPolar.plat[,5], credMass = 0.95)
#0.0008989339 0.1299069618 
hdi(res.freeHand.plat[,5], credMass = 0.95)
#0.0642424 0.3274469 

#pointed+ crushed
hdi(res.biPolar.plat[,6], credMass = 0.95)
#0.04390645 0.25003839 
hdi(res.freeHand.plat[,6], credMass = 0.95)
#0.001985497 0.148193797

#barplot
platforms <- subset(mydata, platformType == "cortical" | platformType == "crushed" | platformType == "flat" | platformType == "lipped" | platformType == "pointed" | platformType == "pointed+crushed")

ggplot(platforms, aes(x = platformType, group = inventory)) +
  geom_bar(aes(y = ..prop.., fill = inventory), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -.5) +
  scale_fill_manual(name = "Inventory", values = c("goldenrod3", "darkolivegreen4")) +
  ggtitle("Platform Type by Reduction Method") +
  xlab("Platform Type") +
  ylab("Percent") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~inventory) +
  theme_minimal()

#######################################
#Termination type
#######################################

xtab <- table(mydata$inventory, mydata$termination)
chisq.test(xtab,simulate.p.value=TRUE,B=10000)

table(mydata$inventory, mydata$termination)

biPolar.termination <- c(3, 9, 2, 4, 0, 2, 10)
freeHand.termination <- c(0, 1, 13, 0, 2, 0, 7)

#axial, crushed, feather, hinge, overshot, perverse, step

res.biPolar.term=rdirichlet(1000, alpha = biPolar.termination + 1)
res.freeHand.term=rdirichlet(1000, alpha = freeHand.termination + 1)

#axial
hdi(res.biPolar.term[,1], credMass = 0.95)
#0.02413218 0.20514024
hdi(res.freeHand.term[,1], credMass = 0.95)
#1.918177e-06 9.576440e-02

#crushed
hdi(res.biPolar.term[,2], credMass = 0.95)
#0.1293825 0.4026441 
hdi(res.freeHand.term[,2], credMass = 0.95)
#0.002830953 0.149734805

#feather
hdi(res.biPolar.term[,3], credMass = 0.95)
#0.01255765 0.17415888
hdi(res.freeHand.term[,3], credMass = 0.95)
#0.3004294 0.6206338 

#hinge
hdi(res.biPolar.term[,4], credMass = 0.95)
#0.0331761 0.2312492
hdi(res.freeHand.term[,4], credMass = 0.95)
#6.708198e-06 1.055280e-01 

#overshot
hdi(res.biPolar.term[,5], credMass = 0.95)
#1.104665e-05 8.370399e-02
hdi(res.freeHand.term[,5], credMass = 0.95)
#0.0118348 0.2150066 

#perverse
hdi(res.biPolar.term[,6], credMass = 0.95)
#0.01565661 0.17085446 
hdi(res.freeHand.term[,6], credMass = 0.95)
#0.0001967013 0.1059101086 

#step
hdi(res.biPolar.term[,7], credMass = 0.95)
#0.1623581 0.4448766 
hdi(res.freeHand.term[,7], credMass = 0.95)
#0.1261946 0.4276979 

#barplot
terminations <- subset(mydata, termination == "axial" | termination == "crushed" | termination == "feather" | termination == "hinge" | termination == "overshot" | termination == "perverse" | termination == "step")

ggplot(terminations, aes(x = termination, group = inventory)) +
  geom_bar(aes(y = ..prop.., fill = inventory), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y = ..prop..), stat = "count", vjust = -.5) +
  scale_fill_manual(name = "Inventory", values = c("goldenrod3", "darkolivegreen4")) +
  ggtitle("Termination Type by Reduction Method") +
  xlab("Termination Type") +
  ylab("Percent") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = scales::percent) +
  facet_grid(~inventory) +
  theme_minimal()

#####################################################
#Quantitative analyses: bipolar v freehand
#####################################################

#######################################
#Maximum length
#######################################

#Frequentist
wilcox.test(freeHand$maxLength, biPolar$maxLength)

#Bayesian
BEST_out_maxLength <- BESTmcmc(freeHand$maxLength, biPolar$maxLength)
plot(BEST_out_maxLength)

summary(BEST_out_maxLength)
plot(BEST_out_maxLength, "effect")

plot(BEST_out_maxLength, ROPE = c(-0.1,0.1))

#######################################
#Maximum width
#######################################

#Frequentist
wilcox.test(freeHand$maxWidth, biPolar$maxWidth)

#Bayesian
BEST_out_maxWidth <- BESTmcmc(freeHand$maxWidth, biPolar$maxWidth)
plot(BEST_out_maxWidth)

summary(BEST_out_maxWidth)
plot(BEST_out_maxWidth, "effect")

plot(BEST_out_maxWidth, ROPE = c(-0.1,0.1))

#######################################
#Maximum thickness
#######################################

#Frequentist
wilcox.test(freeHand$maxThickness, biPolar$maxThickness)

#Bayesian
BEST_out_maxThickness <- BESTmcmc(freeHand$maxThickness, biPolar$maxThickness)
plot(BEST_out_maxThickness)
plotAll(BEST_out_maxThickness)

summary(BEST_out_maxThickness)
plot(BEST_out_maxThickness, "effect")

plot(BEST_out_maxThickness, ROPE = c(-0.1,0.1))

#######################################
#Surface Area
#######################################

#Frequentist
wilcox.test(freeHand$SG1Area, biPolar$SG1Area)

#Bayesian
BEST_out_surfaceArea <- BESTmcmc(freeHand$SG1Area, biPolar$SG1Area)
plot(BEST_out_surfaceArea)
plotAll(BEST_out_surfaceArea)

summary(BEST_out_surfaceArea)
plot(BEST_out_surfaceArea, "effect")

plot(BEST_out_surfaceArea, ROPE = c(-0.1,0.1))

#######################################
#Perimeter
#######################################

#Frequentist
wilcox.test(freeHand$SG1Perimeter, biPolar$SG1Perimeter)

#Bayesian
BEST_out_perimeter <- BESTmcmc(freeHand$SG1Perimeter, biPolar$SG1Perimeter)
plot(BEST_out_perimeter)
plotAll(BEST_out_perimeter)

summary(BEST_out_perimeter)
plot(BEST_out_perimeter, "effect")

plot(BEST_out_perimeter, ROPE = c(-0.1,0.1))

#######################################
#Mass
#######################################

#Frequentist
wilcox.test(freeHand$mass, biPolar$mass)

#Bayesian
BEST_out_mass <- BESTmcmc(freeHand$mass, biPolar$mass)
plot(BEST_out_mass)
plotAll(BEST_out_mass)

summary(BEST_out_mass)
plot(BEST_out_mass, "effect")

plot(BEST_out_mass, ROPE = c(-0.1,0.1))

#######################################
#Technological length
#######################################

#Frequentist
wilcox.test(freeHand$techLength, biPolar$techLength)

#Bayesian
BEST_out_techLength <- BESTmcmc(na.omit(freeHand$techLength), na.omit(biPolar$techLength))
plot(BEST_out_techLength)
plotAll(BEST_out_techLength)

summary(BEST_out_techLength)
plot(BEST_out_techLength, "effect")

plot(BEST_out_techLength, ROPE = c(-0.1,0.1))

#######################################
#Proximal width
#######################################

#Frequentist
wilcox.test(freeHand$proxWidth, biPolar$proxWidth)

#Bayesian
BEST_out_proxWidth <- BESTmcmc(na.omit(freeHand$proxWidth), na.omit(biPolar$proxWidth))
plot(BEST_out_proxWidth)
plotAll(BEST_out_proxWidth)

summary(BEST_out_proxWidth)
plot(BEST_out_proxWidth, "effect")

plot(BEST_out_proxWidth, ROPE = c(-0.1,0.1))

#######################################
#Mesial width
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$mesWidth), na.omit(biPolar$mesWidth))

#Bayesian
BEST_out_mesWidth <- BESTmcmc(na.omit(freeHand$mesWidth), na.omit(biPolar$mesWidth))
plot(BEST_out_mesWidth)
plotAll(BEST_out_mesWidth)

summary(BEST_out_mesWidth)
plot(BEST_out_mesWidth, "effect")

plot(BEST_out_mesWidth, ROPE = c(-0.1,0.1))

#######################################
#Distal width
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$distWidth), na.omit(biPolar$distWidth))

#Bayesian
BEST_out_distWidth <- BESTmcmc(na.omit(freeHand$distWidth), na.omit(biPolar$distWidth))
plot(BEST_out_distWidth)
plotAll(BEST_out_distWidth)

summary(BEST_out_distWidth)
plot(BEST_out_distWidth, "effect")

plot(BEST_out_distWidth, ROPE = c(-0.1,0.1))

#######################################
#Proximal thickness
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$proxThickness), na.omit(biPolar$proxThickness))

#Bayesian
BEST_out_proxThick <- BESTmcmc(na.omit(freeHand$proxThickness), na.omit(biPolar$proxThickness))
plot(BEST_out_proxThick)
plotAll(BEST_out_proxThick)

summary(BEST_out_proxThick)
plot(BEST_out_proxThick, "effect")

plot(BEST_out_proxThick, ROPE = c(-0.1,0.1))

#######################################
#Mesial thickness
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$mesThickness), na.omit(biPolar$mesThickness))

#Bayesian
BEST_out_mesThick <- BESTmcmc(na.omit(freeHand$mesThickness), na.omit(biPolar$mesThickness))
plot(BEST_out_mesThick)
plotAll(BEST_out_mesThick)

summary(BEST_out_mesThick)
plot(BEST_out_mesThick, "effect")

plot(BEST_out_mesThick, ROPE = c(-0.1,0.1))

#######################################
#Distal thickness
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$distThickness), na.omit(biPolar$distThickness))

#Bayesian
BEST_out_distThick <- BESTmcmc(na.omit(freeHand$distThickness), na.omit(biPolar$distThickness))
plot(BEST_out_distThick)
plotAll(BEST_out_distThick)

summary(BEST_out_distThick)
plot(BEST_out_distThick, "effect")

plot(BEST_out_distThick, ROPE = c(-0.1,0.1))

#######################################
#Platform width
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$platformWidth), na.omit(biPolar$platformWidth))

#Bayesian
BEST_out_platWidth <- BESTmcmc(na.omit(freeHand$platformWidth), na.omit(biPolar$platformWidth))
plot(BEST_out_platWidth)
plotAll(BEST_out_platWidth)

summary(BEST_out_platWidth)
plot(BEST_out_platWidth, "effect")

plot(BEST_out_platWidth, ROPE = c(-0.1,0.1))

#######################################
#Platform thickness
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$platformThickness), na.omit(biPolar$platformThickness))

#Bayesian
BEST_out_platThick <- BESTmcmc(na.omit(freeHand$platformThickness), na.omit(biPolar$platformThickness))
plot(BEST_out_platThick)
plotAll(BEST_out_platThick)

summary(BEST_out_platThick)
plot(BEST_out_platThick, "effect")

plot(BEST_out_platThick, ROPE = c(-0.1,0.1))

#######################################
#Interior Platform Angle (IPA)
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$interiorPlatformAngle), na.omit(biPolar$interiorPlatformAngle))

#Bayesian
BEST_out_IPA <- BESTmcmc(na.omit(freeHand$interiorPlatformAngle), na.omit(biPolar$interiorPlatformAngle))
plot(BEST_out_IPA)
plotAll(BEST_out_IPA)

summary(BEST_out_IPA)
plot(BEST_out_IPA, "effect")

plot(BEST_out_IPA, ROPE = c(-0.1,0.1))

#######################################
#Exterior Platform Angle (EPA)
#######################################

#Frequentist
wilcox.test(na.omit(freeHand$exteriorPlatformAngle), na.omit(biPolar$exteriorPlatformAngle))

#Bayesian
BEST_out_EPA <- BESTmcmc(na.omit(freeHand$exteriorPlatformAngle), na.omit(biPolar$exteriorPlatformAngle))
plot(BEST_out_EPA)
plotAll(BEST_out_EPA)

summary(BEST_out_EPA)
plot(BEST_out_EPA, "effect")

plot(BEST_out_EPA, ROPE = c(-0.1,0.1))

#probability density histogram
ggplot(mydata, aes(x=exteriorPlatformAngle, fill=inventory)) + geom_density(alpha=0.5, position="identity") +
  ggtitle("Differences in Exterior Platform Angle between Freehand and Bipolar Flakes") + 
  geom_vline(xintercept = 78.0, color = "darkolivegreen4", linetype = "dashed") + 
  geom_vline(xintercept = 89.1, color = "goldenrod3", linetype = "dashed") +
  xlab("Exterior Platform Angle (EPA)") + 
  ylab("Density of Observations") +
  scale_fill_manual("Reduction method", values = c("biPolar" = alpha("goldenrod3", 0.5), "freeHand"=alpha("darkolivegreen4",0.5))) +
  theme_minimal()

#######################################
#Maximum length to maximum width
#######################################

#Frequentist
wilcox.test((freeHand$maxLength/freeHand$maxWidth), (biPolar$maxLength/biPolar$maxWidth))

#Bayesian
BEST_out_mLmW <- BESTmcmc((freeHand$maxLength/freeHand$maxWidth), (biPolar$maxLength/biPolar$maxWidth))
plot(BEST_out_mLmW)
plotAll(BEST_out_mLmW)

summary(BEST_out_mLmW)
plot(BEST_out_mLmW, "effect")

plot(BEST_out_mLmW, ROPE = c(-0.1,0.1))

#######################################
#EPA to maximum length
#######################################

#Frequentist
wilcox.test((na.omit(freeHand$exteriorPlatformAngle/freeHand$maxLength)), na.omit((biPolar$exteriorPlatformAngle/biPolar$maxLength)))

#Bayesian
BEST_out_EPAmL <- BESTmcmc((na.omit(freeHand$exteriorPlatformAngle/freeHand$maxLength)), na.omit((biPolar$exteriorPlatformAngle/biPolar$maxLength)))
plot(BEST_out_EPAmL)
plotAll(BEST_out_EPAmL)

summary(BEST_out_EPAmL)
plot(BEST_out_EPAmL, "effect")

plot(BEST_out_EPAmL, ROPE = c(-0.1,0.1))

#Graph
ggplot(mydata, aes(x = exteriorPlatformAngle, y = maxLength, color = inventory)) + 
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = lm) + 
  xlab("Exterior Platform Angle") + 
  ylab("Maximum Length") +
  scale_color_manual("Reduction method", values = c("biPolar" = alpha("goldenrod3", 0.5), "freeHand"=alpha("darkolivegreen4",0.5))) +
  ggtitle("Relationship between Exterior Platform Angle and Maximum Length") +
  theme_minimal()

#######################################
#EPA to technological length
#######################################

#Frequentist
wilcox.test((na.omit(freeHand$exteriorPlatformAngle/freeHand$techLength)), na.omit((biPolar$exteriorPlatformAngle/biPolar$techLength)))

#Bayesian
BEST_out_EPAtL <- BESTmcmc((na.omit(freeHand$exteriorPlatformAngle/freeHand$techLength)), na.omit((biPolar$exteriorPlatformAngle/biPolar$techLength)))
plot(BEST_out_EPAtL)
plotAll(BEST_out_EPAtL)

summary(BEST_out_EPAtL)
plot(BEST_out_EPAtL, "effect")

plot(BEST_out_EPAtL, ROPE = c(-0.1,0.1))

#Graph
ggplot(mydata, aes(x = exteriorPlatformAngle, y = techLength, color = inventory)) + 
  geom_point(shape = 16, size = 3) +
  geom_smooth(method = lm) + 
  xlab("Exterior Platform Angle") + 
  ylab("Technological Length") +
  scale_color_manual("Reduction method", values = c("biPolar" = alpha("goldenrod3", 0.5), "freeHand"=alpha("darkolivegreen4",0.5))) +
  ggtitle("Relationship between Exterior Platform Angle and Technological Length") +
  theme_minimal()

#####################################################
#Microfacts: bipolar versus freehand
#####################################################

#######################################
#Maximum length
#######################################

#Frequentist
wilcox.test(freeHand.microFact$maxLength, biPolar.microFact$maxLength)

#Bayesian
BEST_out_micro.length <- BESTmcmc(freeHand.microFact$maxLength, biPolar.microFact$maxLength)
summary(BEST_out_micro.length)
plot(BEST_out_micro.length, ROPE = c(-0.1,0.1))
plot(BEST_out_micro.length, "effect")

#######################################
#Maximum width
#######################################

#Frequentist
wilcox.test(freeHand.microFact$maxWidth, biPolar.microFact$maxWidth)

#Bayesian
BEST_out_micro.width <- BESTmcmc(freeHand.microFact$maxWidth, biPolar.microFact$maxWidth)
summary(BEST_out_micro.width)
plot(BEST_out_micro.width, ROPE = c(-0.1,0.1))
plot(BEST_out_micro.width, "effect")

#######################################
#Maximum thickness
#######################################

#Frequentist
wilcox.test(freeHand.microFact$maxThickness, biPolar.microFact$maxThickness)

#Bayesian
BEST_out_micro.thick <- BESTmcmc(freeHand.microFact$maxThickness, biPolar.microFact$maxThickness)
summary(BEST_out_micro.thick)
plot(BEST_out_micro.thick, ROPE = c(-0.1,0.1))
plot(BEST_out_micro.thick, "effect")

#######################################
#Mass
#######################################

#Frequentist
wilcox.test(freeHand.microFact$mass, biPolar.microFact$mass)

#Bayesian
BEST_out_micro.mass <- BESTmcmc(freeHand.microFact$mass, biPolar.microFact$mass)
summary(BEST_out_micro.mass)
plot(BEST_out_micro.mass, ROPE = c(-0.1,0.1))
plot(BEST_out_micro.mass, "effect")

#####################################################
#Descriptive statistics: pieces esquillees & cores
#####################################################

#Pieces esquillees
summary(pieceEsquille$maxLength)
summary(pieceEsquille$maxWidth)
summary(pieceEsquille$maxThickness)
summary(pieceEsquille$mass)

#Cores
summary(core$maxLength)
summary(core$maxWidth)
summary(core$maxThickness)
summary(core$mass)

#####################################################
#Measurement technique analyses
#####################################################

TestMeasures_Length <- read_excel("TestMeasures_Length.xlsx")
View(TestMeasures_Length)
TestMeasures_Length

TestMeasures_Width <- read_excel("TestMeasures_Width.xlsx")
View(TestMeasures_Width)
TestMeasures_Width

TestMeasures_Area <- read_excel("TestMeasures_Area.xlsx")
View(TestMeasures_Area)
TestMeasures_Area

TestMeasures_Perimeter <- read_excel("TestMeasures_Perimeter.xlsx")
View(TestMeasures_Perimeter)
TestMeasures_Perimeter

#######################################
#Length
#######################################

#Frequentist

#Melt data
melted_data_length <- melt(TestMeasures_Length, id.vars="artifactID")

length_model <- lmer(value ~ variable + (1|artifactID), data = melted_data_length)
anova(length_model)

#post-hoc analysis
summary(glht(length_model,linfct=mcp(variable="Tukey")))

#Bayesian
#myself v external 
BEST_out_R.EO.L <- BESTmcmc(TestMeasures_Length$maxLength, TestMeasures_Length$EOMaxLength)
summary(BEST_out_R.EO.L)
plot(BEST_out_R.EO.L, ROPE = c(-0.1,0.1))
plot(BEST_out_R.EO.L, "effect")

#myself v sg1 length
BEST_out_R.SG1.L <- BESTmcmc(TestMeasures_Length$maxLength, TestMeasures_Length$SG1Length)
summary(BEST_out_R.SG1.L)
plot(BEST_out_R.SG1.L, ROPE = c(-0.1,0.1))
plot(BEST_out_R.SG1.L, "effect")

#myself v sg2 length
BEST_out_R.SG2.L <- BESTmcmc(TestMeasures_Length$maxLength, TestMeasures_Length$SG2Length)
summary(BEST_out_R.SG2.L)
plot(BEST_out_R.SG2.L, ROPE = c(-0.1,0.1))
plot(BEST_out_R.SG2.L, "effect")

#external v sg1 length
BEST_out_EO.SG1.L <- BESTmcmc(TestMeasures_Length$EOMaxLength, TestMeasures_Length$SG1Length)
summary(BEST_out_EO.SG1.L)
plot(BEST_out_EO.SG1.L, ROPE = c(-0.1,0.1))
plot(BEST_out_EO.SG1.L, "effect")

#external v sg2 length
BEST_out_EO.SG2.L <- BESTmcmc(TestMeasures_Length$EOMaxLength, TestMeasures_Length$SG2Length)
summary(BEST_out_EO.SG2.L)
plot(BEST_out_EO.SG2.L, ROPE = c(-0.1,0.1))
plot(BEST_out_EO.SG2.L, "effect")

#sg1 v sg2 length
BEST_out_SG1.SG2.L <- BESTmcmc(TestMeasures_Length$SG1Length, TestMeasures_Length$SG2Length)
summary(BEST_out_SG1.SG2.L)
plot(BEST_out_SG1.SG2.L, ROPE = c(-0.1,0.1))
plot(BEST_out_SG1.SG2.L, "effect")

#Probability density histogram
ggplot(melted_data_length) + geom_density(aes(x = value, color = variable), show.legend = FALSE) +
  stat_density(aes(x = value, color = variable),
               geom = "line", position = "identity") +
  ggtitle("Maximum Length Measurements Probability Density Plot") + 
  xlab("Maximum Length") + 
  ylab("Density of Observations") +
  scale_color_manual("Measurement Technique", values = c("maxLength" = alpha("mediumblue", 0.5), "EOMaxLength"=alpha("dodgerblue3",0.5), "SG1Length"=alpha("forestgreen",0.5), "SG2Length"=alpha("lightgreen",0.5))) +
  theme_minimal()

#Boxplot
melted_data_length$variable <- factor(melted_data_length$variable, levels = c("maxLength", "EOMaxLength", "SG1Length", "SG2Length"))

ggplot(melted_data_length, aes(x = variable, y = value, fill = variable), show.legend = FALSE) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual("Measurement Technique", values = c("maxLength" = alpha("mediumblue", 0.5), "EOMaxLength"=alpha("dodgerblue3",0.5), "SG1Length"=alpha("forestgreen",0.5), "SG2Length"=alpha("lightgreen",0.5))) +
  xlab("Measurement Technique") +
  ylab("Value") +
  ggtitle("Lengths by Measurement Technique")

#######################################
#Width
#######################################

#Frequentist

#Melt data
melted_data_width <- melt(TestMeasures_Width, id.vars="artifactID")

width_model <- lmer(value ~ variable + (1|artifactID), data = melted_data_width)
anova(width_model)

#post-hoc analysis
summary(glht(width_model,linfct=mcp(variable="Tukey")))

#Bayesian

#myself v external 
BEST_out_R.EO.W <- BESTmcmc(TestMeasures_Width$maxWidth, TestMeasures_Width$EOMaxWidth)
summary(BEST_out_R.EO.W)
plot(BEST_out_R.EO.W, ROPE = c(-0.1,0.1))
plot(BEST_out_R.EO.W, "effect")

#myself v sg1 
BEST_out_R.SG1.W <- BESTmcmc(TestMeasures_Width$maxWidth, TestMeasures_Width$SG1Width)
summary(BEST_out_R.SG1.W)
plot(BEST_out_R.SG1.W, ROPE = c(-0.1,0.1))
plot(BEST_out_R.SG1.W, "effect")

#myself v sg2 
BEST_out_R.SG2.W <- BESTmcmc(TestMeasures_Width$maxWidth, TestMeasures_Width$SG2Width)
summary(BEST_out_R.SG2.W)
plot(BEST_out_R.SG2.W, ROPE = c(-0.1,0.1))
plot(BEST_out_R.SG2.W, "effect")

#external v sg1 
BEST_out_EO.SG1.W <- BESTmcmc(TestMeasures_Width$EOMaxWidth, TestMeasures_Width$SG1Width)
summary(BEST_out_EO.SG1.W)
plot(BEST_out_EO.SG1.W, ROPE = c(-0.1,0.1))
plot(BEST_out_EO.SG1.W, "effect")

#external v sg2 
BEST_out_EO.SG2.W <- BESTmcmc(TestMeasures_Width$EOMaxWidth, TestMeasures_Width$SG2Width)
summary(BEST_out_EO.SG2.W)
plot(BEST_out_EO.SG2.W, ROPE = c(-0.1,0.1))
plot(BEST_out_EO.SG2.W, "effect")

#sg1 v sg2 
BEST_out_SG1.SG2.W <- BESTmcmc(TestMeasures_Width$SG1Width, TestMeasures_Width$SG2Width)
summary(BEST_out_SG1.SG2.W)
plot(BEST_out_SG1.SG2.W, ROPE = c(-0.1,0.1))
plot(BEST_out_SG1.SG2.W, "effect")

#Probability density histogram
ggplot(melted_data_width) + geom_density(aes(x = log(value), color = variable), show.legend = FALSE) +
  stat_density(aes(x = log(value), color = variable),
               geom = "line", position = "identity") +
  ggtitle("Maximum Width Measurements Probability Density Plot") + 
  xlab("Maximum Width (log transform)") + 
  ylab("Density of Observations") +
  scale_color_manual("Measurement Technique", values = c("maxWidth" = alpha("mediumblue", 0.5), "EOMaxWidth"=alpha("dodgerblue3",0.5), "SG1Width"=alpha("forestgreen",0.5), "SG2Width"=alpha("lightgreen",0.5))) +
  theme_minimal()

#Boxplot
melted_data_width$variable <- factor(melted_data_width$variable, levels = c("maxWidth", "EOMaxWidth", "SG1Width", "SG2Width"))

ggplot(melted_data_width, aes(x = variable, y = value, fill = variable), show.legend = FALSE) + 
  geom_boxplot() +
  theme_minimal() +
  scale_fill_manual("Measurement Technique", values = c("maxWidth" = alpha("mediumblue", 0.5), "EOMaxWidth"=alpha("dodgerblue3",0.5), "SG1Width"=alpha("forestgreen",0.5), "SG2Width"=alpha("lightgreen",0.5))) +
  xlab("Measurement Technique") +
  ylab("Value") +
  ggtitle("Widths by Measurement Technique")

#######################################
#Surface Area
#######################################

#Frequentist
t.test(TestMeasures_Area$SG1Area, TestMeasures_Area$SG2Area, paired = T)

#Bayesian
BEST_out_SG1.SG2.Area <- BESTmcmc(TestMeasures_Area$SG1Area, TestMeasures_Area$SG2Area)
summary(BEST_out_SG1.SG2.Area)
plot(BEST_out_SG1.SG2.Area, ROPE = c(-0.1,0.1))
plot(BEST_out_SG1.SG2.Area, "effect")

#Probability density histogram
#melt data
melted_data_area <- melt(TestMeasures_Area, id.vars="artifactID")

ggplot(melted_data_area) + geom_density(aes(x = log(value), color = variable), show.legend = FALSE) +
  stat_density(aes(x = log(value), color = variable),
               geom = "line", position = "identity") +
  ggtitle("Surface Area Measurements Probability Density Plot") + 
  xlab("Surface Area (log transform)") + 
  ylab("Density of Observations") +
  scale_color_manual("Measurement Technique", values = c("SG1Area"=alpha("forestgreen",0.5), "SG2Area"=alpha("lightgreen",0.5))) +
  theme_minimal()

#######################################
#Perimeter
#######################################

#Frequentist
t.test(TestMeasures_Perimeter$SG1Perimeter, TestMeasures_Perimeter$SG2Perimeter, paired = T)

#Bayesian
BEST_out_SG1.SG2.perim <- BESTmcmc(TestMeasures_Perimeter$SG1Perimeter, TestMeasures_Perimeter$SG2Perimeter)
summary(BEST_out_SG1.SG2.perim)
plot(BEST_out_SG1.SG2.perim, ROPE = c(-0.1,0.1))
plot(BEST_out_SG1.SG2.Area, "effect")

#Probability density histogram
melted_data_perimeter <- melt(TestMeasures_Perimeter, id.vars="artifactID")

ggplot(melted_data_perimeter) + geom_density(aes(x = log(value), color = variable), show.legend = FALSE) +
  stat_density(aes(x = log(value), color = variable),
               geom = "line", position = "identity") +
  ggtitle("Perimeter Measurements Probability Density Plot") + 
  xlab("Perimeter (log transform)") + 
  ylab("Density of Observations") +
  scale_color_manual("Measurement Technique", values = c("SG1Perimeter"=alpha("forestgreen",0.5), "SG2Perimeter"=alpha("lightgreen",0.5))) +
  theme_minimal()

#####################################################
#Impact of measurement technique on interpretation
#####################################################

#######################################
#Maximum length
#######################################

#Frequentist
wilcox.test(freeHand$maxLength, biPolar$maxLength)
wilcox.test(freeHand$eoMaxLength, biPolar$eoMaxLength)
wilcox.test(freeHand$SG1Length, biPolar$SG1Length)
wilcox.test(freeHand$SG2Length, biPolar$SG2Length)

#Bayesian: myself
BEST_out_maxLength <- BESTmcmc(freeHand$maxLength, biPolar$maxLength)
plot(BEST_out_maxLength)

summary(BEST_out_maxLength)
plot(BEST_out_maxLength, "effect")

plot(BEST_out_maxLength, ROPE = c(-0.1,0.1))

#Bayesian: external participant
BEST_out_maxLength.EO <- BESTmcmc(freeHand$eoMaxLength, biPolar$eoMaxLength)
plot(BEST_out_maxLength.EO)

summary(BEST_out_maxLength.EO)
plot(BEST_out_maxLength.EO, "effect")

plot(BEST_out_maxLength.EO, ROPE = c(-0.1,0.1))

#Bayesian: SG1
BEST_out_maxLength.SG1 <- BESTmcmc(freeHand$SG1Length, biPolar$SG1Length)
plot(BEST_out_maxLength.SG1)

summary(BEST_out_maxLength.SG1)
plot(BEST_out_maxLength.SG1, "effect")

plot(BEST_out_maxLength.SG1, ROPE = c(-0.1,0.1))

#Bayesian: SG2
BEST_out_maxLength.SG2 <- BESTmcmc(freeHand$SG2Length, biPolar$SG2Length)
plot(BEST_out_maxLength.SG2)

summary(BEST_out_maxLength.SG2)
plot(BEST_out_maxLength.SG2, "effect")

plot(BEST_out_maxLength.SG2, ROPE = c(-0.1,0.1))

#######################################
#Maximum Width
#######################################

#Frequentist
wilcox.test(freeHand$maxWidth, biPolar$maxWidth)
wilcox.test(freeHand$eoMaxWidth, biPolar$eoMaxWidth)
wilcox.test(freeHand$SG1Width, biPolar$SG1Width)
wilcox.test(freeHand$SG2Width, biPolar$SG2Width)

#Bayesian: myself
BEST_out_maxWidth <- BESTmcmc(freeHand$maxWidth, biPolar$maxWidth)
plot(BEST_out_maxWidth)

summary(BEST_out_maxWidth)
plot(BEST_out_maxWidth, "effect")

plot(BEST_out_maxWidth, ROPE = c(-0.1,0.1))

#Bayesian: external participant
BEST_out_maxWidth.EO <- BESTmcmc(freeHand$eoMaxWidth, biPolar$eoMaxWidth)
plot(BEST_out_maxWidth.EO)

summary(BEST_out_maxWidth.EO)
plot(BEST_out_maxWidth.EO, "effect")

plot(BEST_out_maxWidth.EO, ROPE = c(-0.1,0.1))

#Bayesian: SG1
BEST_out_maxWidth.SG1 <- BESTmcmc(freeHand$SG1Width, biPolar$SG1Width)
plot(BEST_out_maxWidth.SG1)

summary(BEST_out_maxWidth.SG1)
plot(BEST_out_maxWidth.SG1, "effect")

plot(BEST_out_maxWidth.SG1, ROPE = c(-0.1,0.1))

#Bayesian: SG2
BEST_out_maxWidth.SG2 <- BESTmcmc(freeHand$SG2Width, biPolar$SG2Width)
plot(BEST_out_maxWidth.SG2)

summary(BEST_out_maxWidth.SG2)
plot(BEST_out_maxWidth.SG2, "effect")

plot(BEST_out_maxWidth.SG2, ROPE = c(-0.1,0.1))

#######################################
#Surface Area
#######################################

#Frequentist
wilcox.test(freeHand$SG1Area, biPolar$SG1Area)
wilcox.test(freeHand$SG2Area, biPolar$SG2Area)

#Bayesian: SG1
BEST_out_SA.SG1 <- BESTmcmc(freeHand$SG1Area, biPolar$SG1Area)
plot(BEST_out_SA.SG1)

summary(BEST_out_SA.SG1)
plot(BEST_out_SA.SG1, "effect")

plot(BEST_out_SA.SG1, ROPE = c(-0.1,0.1))

#Bayesian: SG2
BEST_out_SA.SG2 <- BESTmcmc(freeHand$SG2Area, biPolar$SG2Area)
plot(BEST_out_SA.SG2)

summary(BEST_out_SA.SG2)
plot(BEST_out_SA.SG2, "effect")

plot(BEST_out_SA.SG2, ROPE = c(-0.1,0.1))

#######################################
#Perimeter
#######################################

#Frequentist
wilcox.test(freeHand$SG1Perimeter, biPolar$SG1Perimeter)
wilcox.test(freeHand$SG2Perimeter, biPolar$SG2Perimeter)

#Bayesian: SG1
BEST_out_P.SG1 <- BESTmcmc(freeHand$SG1Perimeter, biPolar$SG1Perimeter)
plot(BEST_out_P.SG1)

summary(BEST_out_P.SG1)
plot(BEST_out_P.SG1, "effect")

plot(BEST_out_P.SG1, ROPE = c(-0.1,0.1))

#Bayesian: SG2
BEST_out_P.SG2 <- BESTmcmc(freeHand$SG2Perimeter, biPolar$SG2Perimeter)
plot(BEST_out_P.SG2)

summary(BEST_out_P.SG2)
plot(BEST_out_P.SG2, "effect")

plot(BEST_out_P.SG2, ROPE = c(-0.1,0.1))
