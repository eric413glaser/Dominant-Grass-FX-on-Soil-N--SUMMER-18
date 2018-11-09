library(tidyverse)
library(vegan)

data <- read.csv("e141_Cover.csv")   # Read in data

str(data)  # checks how R is reading in each column

# right now the data is in long format
# let's make it wide format so we can calculate diversity index

data_wide0 <- data  %>%
  group_by(Species, Year, Plot) %>%
  mutate(ind = row_number()) %>%
  spread(Species, Cover, fill=0) %>%
  tbl_df()

#data_wide <- subset(data_wide0, data_wide0$ind != 2)
#rm(data_wide0)

data_div <- data_wide0

sp.mat <- data_wide0[c(17:ncol(data_wide0))]

D <- diversity(sp.mat, "invsimpson")

## Add column for Species Richness in df
SR <- NA
for (n in 1:nrow(data_wide0)) { 
  SR[n] = sum(data_wide0[n, 17:ncol(data_wide0)] > 0)
}
data_div$sr <- SR
rm(SR)

## Add Column for Total Cover in df
TB <- NA
for (n in 1:nrow(data_wide0)) { 
  TB[n] = sum(data_wide0[n, 17:(ncol(data_wide0)-1)] )
}
data_div$totcover <- TB
rm(TB)

data_div$D <- D
data_div$E <- data_div$D /data_div$sr 

#data_ready <- data_div %>%
  dplyr::select(plot, BurnTrt, BrunFreq, year, D, sr, totbio, E)

#write.csv(data_ready, file="FireData.csv")
