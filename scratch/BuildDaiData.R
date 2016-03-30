#In each .txt file, there are 8 lines corresponding to 8 replicates. The 11 values of optical densities
#on each line correspond to 11 different initial cell densities on day 0.
#The different initial cell densities on day 0 (after divided by dilution factor) are differed by a
#factor of 2, with the highest one at 0.05. 

library(dplyr)
library(tidyr)
library(ggplot2)

dilutions <- c(250, 500, 750, 1000, 1133, 1266, 1400, 1600)
days <- 1:7
replicates <- paste("rep_", 1:8, sep = "")
densities <- paste("dens_", 1:11, sep = "")

all_data <- data.frame()

for (day in days){
  for (dilution in dilutions){
    # read the file
    a <- as.matrix(
      read.table(
        paste("DaiData/dilution", dilution, "_day", day, ".txt", sep = "")
        , header = FALSE)
      )
    a <- as.data.frame(a)
    colnames(a) <- densities
    a$replicate <- replicates
    a$day <- day
    a$dilution <- dilution
    all_data <- rbind(all_data, a)
  }
}

all_data <- all_data %>% gather(initial_density, value, 1:11)
check_extinction <- all_data %>% 
  filter(day == 7) %>% 
  mutate(extinction = value < 0.06) %>% 
  select(replicate, dilution, extinction, initial_density)

all_data <- left_join(all_data, check_extinction)

write.table(all_data, "DaiEtAl_Science2012_data.csv", row.names = FALSE, col.names = TRUE)
