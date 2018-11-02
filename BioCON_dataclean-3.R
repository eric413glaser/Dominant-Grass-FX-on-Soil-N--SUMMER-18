library(tidyverse)
library(plyr)
library(ggplot2)

ndata0 <- read.csv("e141_soilN.csv") # read in file

ndata <- ndata0 %>%
 filter(CountOfSpecies == 1)  %>%  ## Select # of specie per plot
 filter(CO2 == "Camb") %>%   ## Select CO2 Ambient plots
 filter(Nitrogen == "Namb")  %>%   ## Select N Ambient plots
 filter(Depth == "0-20") %>%  ## Select depth 0-20 cm
 select(Date, Sampling, NO2NO3, NH4, monospecies, Monogroup) #Chooses columns
ndata <- na.omit(ndata) #throw out n/a's

data4 <- ndata0 %>%
  filter(CountOfSpecies == 4)  %>%  ## Select # of specie per plot
  filter(CO2 == "Camb") %>%   ## Select CO2 Ambient plots
  filter(Nitrogen == "Namb")  %>%   ## Select N Ambient plots
  filter(Depth == "0-20") %>%  ## Select depth 0-20 cm
  select(Date, Sampling, NO2NO3, NH4, Plot) #Chooses columns
data4 <- na.omit(data4) #throw out n/a's

ndataSPP <- ndata %>%
  filter(monospecies %in% c("Bromus inermis", 
         "Agropyron repens",
         "Andropogon gerardi",
         "Sorghastrum nutans"))

test1 <- aov(NO2NO3~monospecies, data=ndataSPP) #average N level of all years
summary(test1)
TukeyHSD(test1)

test5 <- lm(NO2NO3~monospecies*Date, data=ndataSPP)
summary(test5)

test7 <- lm(NO2NO3~monospecies+Date, data=ndataSPP)
summary(test7)

test6 <- lm(NH4~monospecies*Date, data=ndataSPP)
summary(test6)

test2 <- aov(NO2NO3~Monogroup, data=ndata)
summary(test2)
TukeyHSD(test2)

test3 <- aov(NH4~monospecies, data=ndataSPP)
summary(test3)
TukeyHSD(test3)

test4 <- aov(NH4~Monogroup, data=ndata)
summary(test4)
TukeyHSD(test4)

## Grouping for making plots
nitrate_spp <- ddply(ndata, c("monospecies"), summarise,
                   N    = length(NO2NO3),
                   mean = mean(NO2NO3),
                   sd   = sd(NO2NO3),
                   se   = sd / sqrt(N)
)
nitrate_spp 

nitrate_group <- ddply(ndata, c("Monogroup"), summarise,
                     N    = length(NO2NO3),
                     mean = mean(NO2NO3),
                     sd   = sd(NO2NO3),
                     se   = sd / sqrt(N)
)
nitrate_group


NH4_spp <- ddply(ndataSPP, c("monospecies"), summarise,
                     N    = length(NH4),
                     mean = mean(NH4),
                     sd   = sd(NH4),
                     se   = sd / sqrt(N)
)
NH4_spp 

NH4_group <- ddply(ndata, c("Monogroup"), summarise,
                       N    = length(NH4),
                       mean = mean(NH4),
                       sd   = sd(NH4),
                       se   = sd / sqrt(N)
)
NH4_group

#### Plots
theme.t2g<-theme_bw()+theme(text=element_text(size=16), rect=element_rect(colour=
                                                                            "black", size=0.5), 
                            plot.title=element_text(face="bold", vjust=2),
                            axis.title.x=element_text(size = rel(0.9), face="bold", vjust=-0.5),
                            axis.title.y=element_text(size=rel(0.9), face="bold", vjust=1),
                            axis.text=element_text(size=rel(0.9)), strip.text=element_text(size=
                                                                                             rel(0.9), face = "italic"),
                            legend.key=element_rect(colour="black"), panel.grid.major = element_blank(),
                            panel.grid.minor=element_blank(), axis.line=element_line(colour="black",
                                                                                     size = 0.5),
                            strip.background = element_rect(fill = "white", colour = "white"))


nitratespp_plot <- ggplot(data=nitrate_spp, aes(x=monospecies, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(0.9), fill="lightgreen")+
  ylab("Nitrate mg/kg soil") +
  xlab(" ") +
 theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se), width = 0.1, position = position_dodge(0.9))

nitratespp_plot

nitrategroup_plot <- ggplot(data=nitrate_group, aes(x=Monogroup, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(0.9), fill="lightgreen")+
  ylab("NO2 + NO3 mg/kg soil") +
  xlab(" ") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se), width = 0.1, position = position_dodge(0.9))

nitrategroup_plot

NH4spp_plot <- ggplot(data=NH4_spp, aes(x=monospecies, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(0.9), fill="lightgreen")+
  ylab("NH4 mg/kg soil") +
  xlab(" ") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se), width = 0.1, position = position_dodge(0.9))

NH4spp_plot

NH4group_plot <- ggplot(data=NH4_group, aes(x=Monogroup, y=mean)) +
  geom_bar(stat="identity", position=position_dodge(0.9), fill="lighytgreen")+
  ylab("NH4 mg/kg soil") +
  xlab(" ") +
  theme_bw() + theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     axis.line = element_line(colour = "black"))+
  scale_y_continuous(expand = c(0,0)) +
  geom_errorbar(aes(ymax = mean+se, ymin = mean-se), width = 0.1, position = position_dodge(0.9))

NH4group_plot
