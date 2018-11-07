# Plots for publication
# Wild vs. farmed wrasse and acclimation paper
# Adam Brooker 9th November 2017

# Plot list
# 1a. wild vs. farmed temperature
# 1b. wild vs. farmed salinity
# 1c. wild vs. farmed DO
# 2a. acclimated vs. non-acclimated temperature
# 2b. acclimated vs. non-acclimated salinity
# 2c. acclimated vs. non-acclimated DO
# 3. Pen layout and segmentation plot
# 4. code to draw top view of pen 7 with colour-coded locations
# 5. code to draw side view of pen 7 with colour-coded locations
# 6. Locations for wild vs. farmed and acclimated vs. non-acclimated B
# 7. Hourly pen coverage night vs. day for wild vs. farmed and acclimated vs. non-acclimated B
# 8. Polar plots of headings for wild vs. farmed and acclimated vs. non acclimated B
# 9. Individual fish activity by time of day for wild, hatchery acclimated and hatchery and pen acclimated
# 10. Line plots of night and day depth and activity for non-acclimated and hatchery and pen-acclimated
# 11. Survival for wild vs. farmed and both acclimation trials
# 12. Heatmap of depth over time
# 13. Line plots of night and day depth for all three trials
# 14. Bar plots of individual fish night and day depth for all three trials



# 1. Environmental probe plots

library(rJava)
library(xlsxjars)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(zoo)
library(grid)
library(reshape2)
library(cowplot)
library(data.table)



# LOAD ENVIRONMENTAL PROBE READINGS

masterfileloc = "H:/Data processing/AcousticTagFile_2015.xlsx" # 2015 wild vs. farmed wrasse

load.DO <- function(probename, colnums) {
  pn <- probename
  probename <- read.xlsx(masterfileloc, sheet = 13, startRow = 3, cols = colnums)
  colnames(probename) <- c('Time', 'DO', 'Temp')
  probename$Time <- as.POSIXct(strptime(probename$Time, "%m/%d/%y %I:%M:%S %p", tz = 'UTC'))
  probename$Time <- probename$Time - as.difftime(1, unit = 'hours') 
  probename <- probename %>% mutate_at(.vars = vars(DO, Temp), .funs = funs(round(.,2)))
  assign(pn, probename, envir = globalenv())
}

load.sal <- function(probename, colnums) {
  pn <- probename
  probename <- read.xlsx(masterfileloc, sheet = 13, startRow = 3, cols = colnums)
  colnames(probename) <- c('Time', 'Sal')
  probename$Time <- as.POSIXct(strptime(probename$Time, "%m/%d/%y %I:%M:%S %p", tz = 'UTC'))
  probename$Time <- probename$Time - as.difftime(1, unit = 'hours')
  probename <- probename %>% mutate(Sal = round(Sal, 2))
  assign(pn, probename, envir = globalenv())
}


# load wild vs. farmed data

load.DO('probe.DOT1', colnums = c(1, 2, 3))
load.DO('probe.DOT4', colnums = c(6, 7, 8))
load.DO('probe.DOT8', colnums = c(11, 12, 13))
load.DO('probe.DOT12', colnums = c(16, 17, 18))
load.sal('probe.sal1', colnums = c(4, 5))
load.sal('probe.sal4', colnums = c(9, 10))
load.sal('probe.sal8', colnums = c(14, 15))
load.sal('probe.sal12', colnums = c(19, 20))

# combine all data into one data frame
probes <- cbind(probe.DOT1, probe.sal1, probe.DOT4, probe.sal4, probe.DOT8, probe.sal8, probe.DOT12, probe.sal12)
colnames(probes) <- c('do.time.1m', 'do.1m', 'temp.1m', 'sal.time.1m', 'sal.1m', 'do.time.4m', 'do.4m', 'temp.4m', 'sal.time.4m', 'sal.4m', 'do.time.8m', 'do.8m', 'temp.8m', 'sal.time.8m', 'sal.8m','do.time.12m', 'do.12m', 'temp.12m', 'sal.time.12m', 'sal.12m')

# subset for study start and end dates
probes <- subset(probes, do.time.1m > '2015-06-05 00:00:00' & do.time.1m < '2015-08-20 00:00:00')

# calculate rolling 6h-means for data
probes$rolldo1m <- c(rep(NA,23), rollapply(probes$do.1m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo4m <- c(rep(NA,23), rollapply(probes$do.4m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo8m <- c(rep(NA,23), rollapply(probes$do.8m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolldo12m <- c(rep(NA,23), rollapply(probes$do.12m, width = 24, FUN = mean, na.rm = T, align = 'right'))

probes$rollt1m <- c(rep(NA,23), rollapply(probes$temp.1m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rollt4m <- c(rep(NA,23), rollapply(probes$temp.4m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rollt8m <- c(rep(NA,23), rollapply(probes$temp.8m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rollt12m <- c(rep(NA,23), rollapply(probes$temp.12m, width = 24, FUN = mean, na.rm = T, align = 'right'))

probes$rolls1m <- c(rep(NA,23), rollapply(probes$sal.1m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolls4m <- c(rep(NA,23), rollapply(probes$sal.4m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolls8m <- c(rep(NA,23), rollapply(probes$sal.8m, width = 24, FUN = mean, na.rm = T, align = 'right'))
probes$rolls12m <- c(rep(NA,23), rollapply(probes$sal.12m, width = 24, FUN = mean, na.rm = T, align = 'right'))


# 1a. wild vs. farmed temperature

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$do.time.1m))) + 
  scale_y_continuous(expression(paste('Temperature (', ~degree,'C)', sep='')), limits = c(8,15)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$do.time.1m), probes$rollt1m, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.4m), probes$rollt4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$do.time.8m), probes$rollt8m, colour = ' 8m', linetype = ' 8m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.12m), probes$rollt12m, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 4m' = 'gray', ' 8m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 4m' = 'solid', ' 8m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(a)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))

# 1b. wild vs. farmed salinity

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$do.time.1m))) + 
  scale_y_continuous('Salinity (PSU)', limits = c(0,35)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$do.time.1m), probes$rolls1m, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.4m), probes$rolls4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$do.time.8m), probes$rolls8m, colour = ' 8m', linetype = ' 8m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.12m), probes$rolls12m, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 4m' = 'gray', ' 8m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 4m' = 'solid', ' 8m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(b)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))



# 1c. wild vs. farmed DO

ggplot(probes) +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probes$do.time.1m))) + 
  scale_y_continuous('Dissolved oxygen (mg/L)', limits = c(0,15)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probes$do.time.1m), probes$rolldo1m, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.4m), probes$rolldo4m, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probes$do.time.8m), probes$rolldo8m, colour = ' 8m', linetype = ' 8m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probes$do.time.12m), probes$rolldo12m, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 4m' = 'gray', ' 8m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 4m' = 'solid', ' 8m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(c)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))



# load wrasse acclimation data

masterfileloc = "H:/Data processing/AcousticTagFile_2016.xlsx" # 2015 wild vs. farmed wrasse

load.DO('probe.DOT1', colnums = c(1, 2, 3))
load.DO('probe.DOT2', colnums = c(6, 7, 8))
load.DO('probe.DOT4', colnums = c(11, 12, 13))
load.DO('probe.DOT12', colnums = c(16, 17, 18))
load.sal('probe.sal1', colnums = c(4, 5))
load.sal('probe.sal2', colnums = c(9, 10))
load.sal('probe.sal4', colnums = c(14, 15))
load.sal('probe.sal12', colnums = c(19, 20))

# combine all data into one data frame
#probes <- cbind(probe.DOT1, probe.sal1, probe.DOT2, probe.sal2, probe.DOT4, probe.sal4, probe.DOT12, probe.sal12)
#colnames(probes) <- c('do.time.1m', 'do.1m', 'temp.1m', 'sal.time.1m', 'sal.1m', 'do.time.2m', 'do.2m', 'temp.2m', 'sal.time.2m', 'sal.2m', 'do.time.4m', 'do.4m', 'temp.4m', 'sal.time.4m', 'sal.4m','do.time.12m', 'do.12m', 'temp.12m', 'sal.time.12m', 'sal.12m')


# calculate rolling 6h-means for data
probe.DOT1$rolldo <- c(rep(NA,11), rollapply(probe.DOT1$DO, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT2$rolldo <- c(rep(NA,11), rollapply(probe.DOT2$DO, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT4$rolldo <- c(rep(NA,11), rollapply(probe.DOT4$DO, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT12$rolldo <- c(rep(NA,11), rollapply(probe.DOT12$DO, width = 12, FUN = mean, na.rm = T, align = 'right'))

probe.DOT1$rollt <- c(rep(NA,11), rollapply(probe.DOT1$Temp, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT2$rollt <- c(rep(NA,11), rollapply(probe.DOT2$Temp, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT4$rollt <- c(rep(NA,11), rollapply(probe.DOT4$Temp, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.DOT12$rollt <- c(rep(NA,11), rollapply(probe.DOT12$Temp, width = 12, FUN = mean, na.rm = T, align = 'right'))

probe.sal1$rolls <- c(rep(NA,11), rollapply(probe.sal1$Sal, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.sal2$rolls <- c(rep(NA,11), rollapply(probe.sal2$Sal, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.sal4$rolls <- c(rep(NA,11), rollapply(probe.sal4$Sal, width = 12, FUN = mean, na.rm = T, align = 'right'))
probe.sal12$rolls <- c(rep(NA,11), rollapply(probe.sal12$Sal, width = 12, FUN = mean, na.rm = T, align = 'right'))


# subset for study start and end dates
trial1 <- c('2016-06-18 19:00:00', '2016-07-16 00:00:00')
trial2 <- c('2016-09-14 00:00:00', '2016-10-14 00:00:00')

#trial 1 subset
probe.DOT1 <- subset(probe.DOT1, Time > trial1[[1]] & Time < trial1[[2]])
probe.DOT2 <- subset(probe.DOT2, Time > trial1[[1]] & Time < trial1[[2]])
probe.DOT4 <- subset(probe.DOT4, Time > trial1[[1]] & Time < trial1[[2]])
probe.DOT12 <- subset(probe.DOT12, Time > trial1[[1]] & Time < trial1[[2]])
probe.sal1 <- subset(probe.sal1, Time > trial1[[1]] & Time < trial1[[2]])
probe.sal2 <- subset(probe.sal2, Time > trial1[[1]] & Time < trial1[[2]])
probe.sal4 <- subset(probe.sal4, Time > trial1[[1]] & Time < trial1[[2]])
probe.sal12 <- subset(probe.sal12, Time > trial1[[1]] & Time < trial1[[2]])

#trial 2 subset
probe.DOT1 <- subset(probe.DOT1, Time > trial2[[1]] & Time < trial2[[2]])
probe.DOT2 <- subset(probe.DOT2, Time > trial2[[1]] & Time < trial2[[2]])
probe.DOT4 <- subset(probe.DOT4, Time > trial2[[1]] & Time < trial2[[2]])
probe.DOT12 <- subset(probe.DOT12, Time > trial2[[1]] & Time < trial2[[2]])
probe.sal1 <- subset(probe.sal1, Time > trial2[[1]] & Time < trial2[[2]])
probe.sal2 <- subset(probe.sal2, Time > trial2[[1]] & Time < trial2[[2]])
probe.sal4 <- subset(probe.sal4, Time > trial2[[1]] & Time < trial2[[2]])
probe.sal12 <- subset(probe.sal12, Time > trial2[[1]] & Time < trial2[[2]])

# 2a. acclimated vs. non-acclimated temperature

ggplot() +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probe.DOT1$Time))) + 
  scale_y_continuous(expression(paste('Temperature (', ~degree,'C)', sep='')), limits = c(8,15)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probe.DOT1$Time), probe.DOT1$rollt, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.DOT2$Time), probe.DOT2$rollt, colour = ' 2m', linetype = ' 2m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probe.DOT4$Time), probe.DOT4$rollt, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.DOT12$Time), probe.DOT12$rollt, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 2m' = 'gray', ' 4m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 2m' = 'solid', ' 4m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(a)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))

# 2b. acclimated vs. non-acclimated salinity

ggplot() +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probe.sal1$Time))) + 
  scale_y_continuous('Salinity (PSU)', limits = c(0,38)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probe.sal1$Time), probe.sal1$rolls, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.sal2$Time), probe.sal2$rolls, colour = ' 2m', linetype = ' 2m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probe.sal4$Time), probe.sal4$rolls, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.sal12$Time), probe.sal12$rolls, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 2m' = 'gray', ' 4m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 2m' = 'solid', ' 4m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(b)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))



# 2c. acclimated vs. non-acclimated DO

ggplot() +  
  scale_x_datetime('Date', limits = as.POSIXct(range(probe.DOT1$Time))) + 
  scale_y_continuous('Dissolved oxygen (mg/L)', limits = c(0,15)) +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 18), legend.position = c(0.90, 0.2)) +
  #geom_line(aes(as.POSIXct(probes$do.time.1m), probes$do.1m), linetype = 'dashed') +
  geom_line(aes(as.POSIXct(probe.DOT1$Time), probe.DOT1$rolldo, colour = ' 1m', linetype = ' 1m')) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.DOT2$Time), probe.DOT2$rolldo, colour = ' 2m', linetype = ' 2m')) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  geom_line(aes(as.POSIXct(probe.DOT4$Time), probe.DOT4$rolldo, colour = ' 4m', linetype = ' 4m')) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  geom_line(aes(as.POSIXct(probe.DOT12$Time), probe.DOT12$rolldo, colour = '12m', linetype = '12m')) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c(' 1m' = 'gray', ' 2m' = 'gray', ' 4m' = 'black', '12m' = 'black')) +
  scale_linetype_manual(name = '', values = c(' 1m' = 'longdash', ' 2m' = 'solid', ' 4m' = 'longdash', '12m' = 'solid')) +
  annotation_custom(grobTree(textGrob('(c)', x = 0.05, y = 0.95, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))


# 3. Pen layout and segmentation plot

#LOAD LOCATIONS CODING DATA
locations.lookup <- read.xlsx(masterfileloc, sheet = 12, startRow = 1, cols = seq(1,7)) # read in codes from Locations Coding spreadsheet
rownames(locations.lookup) <- locations.lookup$Code

pen.col <- 'black'
pen.size <- 1.5

#create hide and hydrophone location table
pen.sym <- c(x = locations.lookup['7WHNW', 'xmin']+2, y = locations.lookup['7WHNW', 'ymin']+2)
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7WHSE', 'xmin']+2, y = locations.lookup['7WHSE', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8WHSW', 'xmin']+2, y = locations.lookup['8WHSW', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8WHNE', 'xmin']+2, y = locations.lookup['8WHNE', 'ymin']+2))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7FBSE', 'xmin']+1.5, y = locations.lookup['7FBSE', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['7FBNW', 'xmin']+1.5, y = locations.lookup['7FBNW', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8FBSW', 'xmin']+1.5, y = locations.lookup['8FBSW', 'ymin']+1.5))
pen.sym <- rbind(pen.sym, c(x = locations.lookup['8FBNE', 'xmin']+1.5, y = locations.lookup['8FBNE', 'ymin']+1.5))
rownames(pen.sym) <- c('7WHNW', '7WHSE', '8WHSW', '8WHNE', '7FBSE', '7FBNW', '8FBSW', '8FBNE')
pen.sym <- as.data.frame(pen.sym)
pen.sym <- rbind(pen.sym, c(40, 15.7), c(66, 40), c(14.3, 14.3), c(40, 40), c(66, 14.3), c(40, 14.23), c(14.3, 40), c(40, 38.6))
pen.sym$type <- as.factor(c(rep('hide', 4), rep('feed block', 4), rep('deep hydrophone', 4), rep('shallow hydrophone', 4)))
pen.sym$type <- factor(pen.sym$type, levels = c('deep hydrophone', 'shallow hydrophone', 'hide', 'feed block'))
rownames(pen.sym) <- c('7WHNW', '7WHSE', '8WHSW', '8WHNE', '7FBSE', '7FBNW', '8FBSW', '8FBNE', 'd1', 'd2', 'd3', 'd4', 's1', 's2', 's3', 's4')
pen.sym <- as.data.frame(pen.sym)

blank <- c(rep(' ', 9)) # blank sequence for no labels on axis tick marks

ggplot() +
  geom_point(aes(x = 15, y = 15), colour = 'white') + #scale_fill_gradientn(colours=plot.col, space = 'Lab', limits = c(0, pingmax), na.value = plot.col[length(plot.col)], name = 'No. pings') +
  theme_classic() + theme(text = element_text(family = 'Times New Roman', size = 20), legend.position = c(0.15, 0.9), axis.title.x = element_text(size = 18, face = 'bold'), axis.title.y = element_text(size = 18, face = 'bold')) +
  scale_x_continuous('x-axis (m)', limits = c(10,70), breaks = seq(10, 70, 1), labels = c('10', blank, '20', blank, '30', blank, '40', blank, '50', blank, '60', blank, '70')) +
  scale_y_continuous('y-axis (m)', limits = c(10,50), breaks = seq(10, 50, 1), labels = c('10', blank, '20', blank, '30', blank, '40', blank, '50')) +
  annotate('rect', xmin = locations.lookup['8CSW', 'xmin'], xmax = locations.lookup['8CSE', 'xmax'], ymin = locations.lookup['8CSW', 'ymin'], ymax = locations.lookup['8CNE', 'ymax'], size = pen.size, colour = pen.col, alpha = 0) +
  annotate('rect', xmin = locations.lookup['7CSW', 'xmin'], xmax = locations.lookup['7CSE', 'xmax'], ymin = locations.lookup['7CSW', 'ymin'], ymax = locations.lookup['7CNE', 'ymax'], size = pen.size, colour = pen.col, alpha = 0) +
  geom_point(data = pen.sym, mapping = aes(x = x, y = y, size = type, colour = type, fill = type, shape = type, stroke = 1)) +
  annotate('text', x = 27.5, y = 27.5, label = 'Pen 7', family = 'Times New Roman', size = 6) +
  annotate('text', x = 53.5, y = 27.5, label = 'Pen 8', family = 'Times New Roman', size = 6) +
  scale_size_manual(name = '', values = c('hide' = 15, 'feed block' = 5, 'deep hydrophone' = 5, 'shallow hydrophone' = 5)) +
  scale_shape_manual(name = '', values = c('hide' = 21, 'feed block' = 13, 'deep hydrophone' = 22, 'shallow hydrophone' = 24)) +
  scale_colour_manual(name = '', values = c('hide' = 'black', 'feed block' = 'black', 'deep hydrophone' = 'black', 'shallow hydrophone' = 'black')) +
  scale_fill_manual(name = '', values = c('hide' = 'grey70', 'feed block' = 'grey70', 'deep hydrophone' = 'grey35', 'shallow hydrophone' = 'grey35')) +
  guides(size = guide_legend(override.aes = list(size = 5))) +
  annotation_custom(grobTree(linesGrob(x = c(0.305, 0.325), y = c(0.81, 0.9), arrow = arrow(length = unit(5, 'mm')), gp = gpar(lwd = 2.5)))) +
  annotation_custom(grobTree(textGrob('N', x = 0.330, y = 0.93, gp = gpar(fontsize = 18, fontfamily = 'Times New Roman'))))


# 4. code to draw top view of pen 7 with colour-coded locations

par(mfrow=c(1,1))

plot(50, 50, xlab = 'X (m)', ylab = 'Z (m)', pch = 20, cex = 1, xlim = c(10, 45), ylim = c(10, 45), type = 'l', col = '#26b426') # tight plot
polygon(c(15, 21, 21, 15), c(15, 15, 21, 21), lty = 1, lwd = 2, col = rgb(248, 203, 173, maxColorValue = 255)) # SW corner
polygon(c(15, 21, 21, 15), c(33, 33, 39, 39), lty = 1, lwd = 2, col = rgb(255, 153, 153, maxColorValue = 255)) # NW corner
polygon(c(33, 39, 39, 33), c(15, 15, 21, 21), lty = 1, lwd = 2, col = rgb(255, 153, 153, maxColorValue = 255)) # SE corner
polygon(c(33, 39, 39, 33), c(33, 33, 39, 39), lty = 1, lwd = 2, col = rgb(248, 203, 173, maxColorValue = 255)) # NE corner
polygon(c(21, 33, 33, 21), c(15, 15, 21, 21), lty = 1, lwd = 2, col = rgb(118, 113, 113, maxColorValue = 255)) # S edge
polygon(c(33, 39, 39, 33), c(21, 21, 33, 33), lty = 1, lwd = 2, col = rgb(118, 113, 113, maxColorValue = 255)) # E edge
polygon(c(21, 33, 33, 21), c(33, 33, 39, 39), lty = 1, lwd = 2, col = rgb(118, 113, 113, maxColorValue = 255)) # N edge
polygon(c(15, 21, 21, 15), c(21, 21, 33, 33), lty = 1, lwd = 2, col = rgb(118, 113, 113, maxColorValue = 255)) # W edge
polygon(c(21, 33, 33, 21), c(21, 21, 33, 33), lty = 1, lwd = 2, col = rgb(192, 0, 0, maxColorValue = 255)) # centre
polygon(c(33.35, 37.35, 37.35, 33.35), c(15.13, 15.13, 19.13, 19.13), lty = 1, lwd = 2, col = rgb(146, 208, 80, maxColorValue = 255)) # SE hide
polygon(c(15.49, 19.49, 19.49, 15.49), c(35.10, 35.10, 39.10, 39.10), lty = 1, lwd = 2, col = rgb(146, 208, 80, maxColorValue = 255)) # NW hide
polygon(c(36, 39, 39, 36), c(13.5, 13.5, 16.5, 16.5), lty = 1, lwd = 2, col = rgb(0, 176, 240, maxColorValue = 255)) # SE feed block
polygon(c(13.5, 16.5, 16.5, 13.5), c(36, 36, 39, 39), lty = 1, lwd = 2, col = rgb(0, 176, 240, maxColorValue = 255)) # NW feed block

# 5. code to draw side view of pen 7 with colour-coded locations

par(mfrow=c(1,1))
plot(50, 50, xlab = 'X (m)', ylab = 'Z (m)', pch = 20, cex = 1, xlim = c(10, 43), ylim = c(25, -5), type = 'l', col = '#26b426') # tight plot
polygon(c(15, 21, 21, 15), c(15, 15, 0, 0), lty = 1, lwd = 2, col = rgb(248, 203, 173, maxColorValue = 255)) # left edge
polygon(c(21, 33, 33, 21), c(15, 15, 0, 0), lty = 1, lwd = 2, col = rgb(118, 113, 113, maxColorValue = 255)) # edge
polygon(c(33, 39, 39, 33), c(15, 15, 0, 0), lty = 1, lwd = 2, col = rgb(255, 153, 153, maxColorValue = 255)) # right edge
polygon(c(15, 27, 39), c(15, 20, 15), lwd = 2, col = rgb(208, 206, 206, maxColorValue = 255)) # bottom cone
polygon(c(33.35, 37.35, 37.35, 33.35), c(12, 12, 8, 8), lwd = 2, col = rgb(146, 208, 80, maxColorValue = 255)) # hide SE
#polygon(c(15.49, 19.49, 19.49, 15.49), c(9.98, 9.98, 13.9, 13.9), lty = 1, lwd = 2, col = rgb(146, 208, 80, maxColorValue = 255)) # NW hide
polygon(c(36, 39, 39, 36), c(4.5, 4.5, 7.5, 7.5), lty = 1, lwd = 2, col = rgb(0, 176, 240, maxColorValue = 255)) # SE feed block
#polygon(c(13.5, 16.5, 16.5, 13.5), c(4.5, 4.5, 7.5, 7.5), lty = 1, lwd = 2, col = rgb(0, 176, 240, maxColorValue = 255)) # NW feed block


# 6. Locations for wild vs. farmed and acclimated vs. non-acclimated B

# load wild vs. farmed locations data, reorganise and recalculate as proportions

setwd('H:/Data processing/2015 Wild vs. Farmed/Cropped data/Coded Day CSV/Outputs')
wflocs <- read.csv('LocationsOutput.csv')
wflocs <- as.data.frame(wflocs)
wflocs$X <- as.character(wflocs$X)
wflocs[1,1] <- 'P7_lt15m'
wflocs[2,1] <- 'P7_gt15m'
wflocs[10,1] <- 'P8_lt15m'
wflocs[11,1] <- 'P8_gt15m'
rownames(wflocs) <- wflocs$X
wflocs$X <- NULL
colnames(wflocs) <- seq(1, 37)
wflocs <- wflocs[,1:37]
wflocs <- as.data.frame(t(wflocs))
wflocs$P7_totedge <- wflocs$P7_outer + wflocs$P7_edge
wflocs$P8_totedge <- wflocs$P8_outer + wflocs$P8_edge
wflocs$P7_hidecor_nohid <- wflocs$P7_hidecorner - wflocs$P7_hides - wflocs$P7_feedblock
wflocs$P8_hidecor_nohid <- wflocs$P8_hidecorner - wflocs$P8_hides - wflocs$P8_feedblock
#wflocs <- wflocs[c(1, 19, seq(6, 9), 21, 10, 20, seq(15, 18), 22)]
wflocs <- wflocs[c(1, 9, 8, 7, 21, 6, 19, 10, 18, 17, 16, 22, 15, 20)]
wflocs[wflocs <0] <- 0
wflocs$P7_tot <- wflocs[,1] + wflocs[,2] + wflocs[,3] + wflocs[,4] + wflocs[,5] + wflocs[,6] + wflocs[,7]
wflocs$P8_tot <- wflocs[,8] + wflocs[,9] + wflocs[,10] + wflocs[,11] + wflocs[,12] + wflocs[,13] + wflocs[,14]
wflocs$P7_lt15m <- (wflocs$P7_lt15m / wflocs$P7_tot)*100
wflocs$P7_totedge <- (wflocs$P7_totedge / wflocs$P7_tot)*100
wflocs$P7_emptycorner <- (wflocs$P7_emptycorner / wflocs$P7_tot)*100
wflocs$P7_centre <- (wflocs$P7_centre / wflocs$P7_tot)*100
wflocs$P7_hides <- (wflocs$P7_hides / wflocs$P7_tot)*100
wflocs$P7_feedblock <- (wflocs$P7_feedblock / wflocs$P7_tot)*100
wflocs$P7_hidecor_nohid <- (wflocs$P7_hidecor_nohid / wflocs$P7_tot)*100
wflocs$P8_lt15m <- (wflocs$P8_lt15m / wflocs$P8_tot)*100
wflocs$P8_totedge <- (wflocs$P8_totedge / wflocs$P8_tot)*100
wflocs$P8_emptycorner <- (wflocs$P8_emptycorner / wflocs$P8_tot)*100
wflocs$P8_centre <- (wflocs$P8_centre / wflocs$P8_tot)*100
wflocs$P8_hides <- (wflocs$P8_hides / wflocs$P8_tot)*100
wflocs$P8_feedblock <- (wflocs$P8_feedblock / wflocs$P8_tot)*100
wflocs$P8_hidecor_nohid <- (wflocs$P8_hidecor_nohid / wflocs$P8_tot)*100
wlocs <- wflocs[c(1:7)]
flocs <- wflocs[c(8:14)]
wlocs <- melt(wlocs, variable.name = 'locs', value.name = 'props')
wlocs$day <- c(seq(1, 14), seq(19, 26), seq(29, 43))
flocs <- melt(flocs, variable.name = 'locs', value.name = 'props')
flocs$day <- c(seq(1, 14), seq(19, 26), seq(29, 43))

# load acclimated vs. non-acclimated locations data, reorganise and recalculate as proportions

setwd('H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Day CSV/Outputs')
aclocs <- read.csv('LocationsOutput.csv')
aclocs <- as.data.frame(aclocs)
aclocs$X <- as.character(aclocs$X)
aclocs[1,1] <- 'P7_lt15m'
aclocs[2,1] <- 'P7_gt15m'
aclocs[10,1] <- 'P8_lt15m'
aclocs[11,1] <- 'P8_gt15m'
rownames(aclocs) <- aclocs$X
aclocs$X <- NULL
colnames(aclocs) <- seq(1, 30)
#wflocs <- wflocs[,1:37]
aclocs <- as.data.frame(t(aclocs))
aclocs$P7_totedge <- aclocs$P7_outer + aclocs$P7_edge
aclocs$P8_totedge <- aclocs$P8_outer + aclocs$P8_edge
aclocs$P7_hidecor_nohid <- aclocs$P7_hidecorner - aclocs$P7_hides - aclocs$P7_feedblock
aclocs$P8_hidecor_nohid <- aclocs$P8_hidecorner - aclocs$P8_hides - aclocs$P8_feedblock
#wflocs <- wflocs[c(1, 19, seq(6, 9), 21, 10, 20, seq(15, 18), 22)]
aclocs <- aclocs[c(1, 9, 8, 7, 21, 6, 19, 10, 18, 17, 16, 22, 15, 20)]
aclocs[aclocs <0] <- 0
aclocs$P7_tot <- aclocs[,1] + aclocs[,2] + aclocs[,3] + aclocs[,4] + aclocs[,5] + aclocs[,6] + aclocs[,7]
aclocs$P8_tot <- aclocs[,8] + aclocs[,9] + aclocs[,10] + aclocs[,11] + aclocs[,12] + aclocs[,13] + aclocs[,14]
aclocs$P7_lt15m <- (aclocs$P7_lt15m / aclocs$P7_tot)*100
aclocs$P7_totedge <- (aclocs$P7_totedge / aclocs$P7_tot)*100
aclocs$P7_emptycorner <- (aclocs$P7_emptycorner / aclocs$P7_tot)*100
aclocs$P7_centre <- (aclocs$P7_centre / aclocs$P7_tot)*100
aclocs$P7_hides <- (aclocs$P7_hides / aclocs$P7_tot)*100
aclocs$P7_feedblock <- (aclocs$P7_feedblock / aclocs$P7_tot)*100
aclocs$P7_hidecor_nohid <- (aclocs$P7_hidecor_nohid / aclocs$P7_tot)*100
aclocs$P8_lt15m <- (aclocs$P8_lt15m / aclocs$P8_tot)*100
aclocs$P8_totedge <- (aclocs$P8_totedge / aclocs$P8_tot)*100
aclocs$P8_emptycorner <- (aclocs$P8_emptycorner / aclocs$P8_tot)*100
aclocs$P8_centre <- (aclocs$P8_centre / aclocs$P8_tot)*100
aclocs$P8_hides <- (aclocs$P8_hides / aclocs$P8_tot)*100
aclocs$P8_feedblock <- (aclocs$P8_feedblock / aclocs$P8_tot)*100
aclocs$P8_hidecor_nohid <- (aclocs$P8_hidecor_nohid / aclocs$P8_tot)*100
nalocs <- aclocs[c(8:14)]
aclocs <- aclocs[c(1:7)]
aclocs <- melt(aclocs, variable.name = 'locs', value.name = 'props')
aclocs$day <- c(seq(1, 30))
nalocs <- melt(nalocs, variable.name = 'locs', value.name = 'props')
nalocs$day <- c(seq(1, 30))


# draw stacked bar plots of wild vs. farmed

plotfont <- 'Arial'

wplot <- ggplot(wlocs, aes(x = day, y = props, fill = locs)) 
wplot <- wplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt'))
wplot <- wplot + geom_bar(stat = 'identity') +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('below 15m', 'feed block', 'hides', 'centre', 'hide corners', 'empty corners', 'edge'), values = c('P7_lt15m' = 'gray90', 'P7_feedblock' = 'gray35', 'P7_hides' = 'gray50', 'P7_centre' = 'gray0', 'P7_hidecor_nohid' = 'gray70', 'P7_emptycorner' = 'gray80', 'P7_totedge' = 'gray20'))
wplot <- wplot + scale_y_continuous(name = 'Time (%)', expand = c(0,0), breaks = seq(0, 100, 10)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 43, 1), labels = c('5', '10', '15', '20', '25', '30', '35', '40'), breaks = seq(5, 40, 5))
wplot <- wplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
wplot

fplot <- ggplot(flocs, aes(x = day, y = props, fill = locs))
fplot <- fplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt'))
fplot <- fplot + geom_bar(stat = 'identity') +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('below 15m', 'feed block', 'hides', 'centre', 'hide corners', 'empty corners', 'edge'), values = c('P8_lt15m' = 'gray90', 'P8_feedblock' = 'gray35', 'P8_hides' = 'gray50', 'P8_centre' = 'gray0', 'P8_hidecor_nohid' = 'gray70', 'P8_emptycorner' = 'gray80', 'P8_totedge' = 'gray20'))
fplot <- fplot + scale_y_continuous(name = 'Time (%)', expand = c(0,0), breaks = seq(0, 100, 10)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 43, 1), labels = c('5', '10', '15', '20', '25', '30', '35', '40'), breaks = seq(5, 40, 5))
fplot <- fplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
fplot

# draw stacked bar plots of acclimated vs. non-acclimated

acplot <- ggplot(aclocs, aes(x = day, y = props, fill = locs)) 
acplot <- acplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), plot.margin = margin(10, 1, 10, 10, 'pt'))
acplot <- acplot + geom_bar(stat = 'identity') +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('below 15m', 'feed block', 'hides', 'centre', 'hide corners', 'empty corners', 'edge'), values = c('P7_lt15m' = 'gray90', 'P7_feedblock' = 'gray35', 'P7_hides' = 'gray50', 'P7_centre' = 'gray0', 'P7_hidecor_nohid' = 'gray70', 'P7_emptycorner' = 'gray80', 'P7_totedge' = 'gray20'))
acplot <- acplot + scale_y_continuous(name = 'Time (%)', expand = c(0,0), breaks = seq(0, 100, 10)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 30, 1), labels = c('5', '10', '15', '20', '25', '30'), breaks = seq(5, 30, 5))
acplot <- acplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
acplot

naplot <- ggplot(nalocs, aes(x = day, y = props, fill = locs))
naplot <- naplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 132, 10, 10, 'pt'))
naplot <- naplot + geom_bar(stat = 'identity') +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('below 15m', 'feed block', 'hides', 'centre', 'hide corners', 'empty corners', 'edge'), values = c('P8_lt15m' = 'gray90', 'P8_feedblock' = 'gray35', 'P8_hides' = 'gray50', 'P8_centre' = 'gray0', 'P8_hidecor_nohid' = 'gray70', 'P8_emptycorner' = 'gray80', 'P8_totedge' = 'gray20'))
naplot <- naplot + scale_y_continuous(name = 'Time (%)', expand = c(0,0), breaks = seq(0, 100, 10)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 30, 1), labels = c('5', '10', '15', '20', '25', '30'), breaks = seq(5, 30, 5))
naplot <- naplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
naplot


plot_grid(wplot, acplot, fplot, naplot, labels = c('(a)', '(c)', '(b)', '(d)'), rel_widths = c(1,1), hjust = c(-0.0, -0.5, -0.0, -0.5))



# 7. Hourly pen coverage night vs. day for wild vs. farmed and acclimated vs. non-acclimated

# load wild vs. farmed hourly coverage data

wildn <- 16
farmedn <- 10
accn <- 10
naccn <- 14

setwd('H:/Data processing/2015 Wild vs. Farmed/Cropped data/Coded Fish CSV/Outputs')
covday <- read.csv('CoverageOutput_hmeanperfish_day.csv')
covday <- as.data.frame(covday)
covday <- covday[,-c(1, 2, 4,5)]
#covday$ID <- as.character(covday$ID)
#rownames(covday) <- covday$ID
#covday$ID <- NULL
#covday <- covday[,c(2, seq(3, 76, 2))]
#colnames(covday) <- c('pen', seq(1, 37))
#covday <- do.call(data.frame, aggregate(.~pen, data = covday, FUN = function(x) c(mn = mean(x), sd = sd(x))))
covday <- do.call(data.frame, aggregate(.~pen, data = covday, FUN = function(x) c(mn = mean(x))))
covday <- cbind(as.data.frame(t(covday[,seq(2, 75, 2)])), as.data.frame(t(covday[,seq(3, 75, 2)])))
colnames(covday) <- c('wild_cov_day', 'farmed_cov_day', 'wild_day_sd', 'farmed_day_sd')
covday$day <- c(seq(1, 14), seq(19, 26), seq(29, 43))
rownames(covday) <- seq(1, 37, 1)


covnight <- read.csv('CoverageOutput_hmeanperfish_night.csv')
covnight <- as.data.frame(covnight)
covnight[,c(1, 2, 4,5)] <- NULL
#covnight$ID <- as.character(covnight$ID)
#rownames(covnight) <- covnight$ID
#covnight$ID <- NULL
#covnight <- covnight[,c(2, seq(3, 74, 2))]
#colnames(covnight) <- c('pen', seq(1, 36))
#covnight <- do.call(data.frame, aggregate(.~pen, data = covnight, FUN = function(x) c(mn = mean(x), sd = sd(x))))
covnight <- do.call(data.frame, aggregate(.~pen, data = covnight, FUN = function(x) c(mn = mean(x)), na.action = na.omit))
covnight[,seq(3, 74, 2)] <- NA
covnight <- cbind(as.data.frame(t(covnight[,seq(2, 73, 2)])), as.data.frame(t(covnight[,seq(3, 73, 2)])))
colnames(covnight) <- c('wild_cov_night', 'farmed_cov_night', 'wild_night_sd', 'farmed_night_sd')
covnight <- covnight[-c(14, 22),]
covnight$day <- c(seq(2, 14), seq(20, 26), seq(30, 43))
rownames(covnight) <- seq(1, 34, 1)


coverage <- left_join(covday, covnight, by = 'day')
wcov <- coverage[c(5, 1, 3, 6, 8)]
colnames(wcov) <- c('day', 'day_cov', 'day_sd', 'night_cov', 'night_sd')
fcov <- coverage[c(5, 2, 4, 7, 9)]
colnames(fcov) <- c('day', 'day_cov', 'day_sd', 'night_cov', 'night_sd')

covs <- melt(wcov[,c('day', 'day_cov', 'night_cov')], measure.vars = c('day_cov', 'night_cov'), variable.name = 'time', value.name = 'cov')
csd <- melt(wcov[,c('day', 'day_sd', 'night_sd')], measure.vars = c('day_sd', 'night_sd'), variable.name = 'time', value.name = 'sd')
csd <- csd[,3]
csd <- csd/wildn # divide sd by n of fish
wcov <- cbind(covs, csd)
wcov[,c(3, 4)] <- wcov[,c(3, 4)]*100
#wcov[38:74,4] <- NA
rm(covs, csd)

covs <- melt(fcov[,c('day', 'day_cov', 'night_cov')], measure.vars = c('day_cov', 'night_cov'), variable.name = 'time', value.name = 'cov')
csd <- melt(fcov[,c('day', 'day_sd', 'night_sd')], measure.vars = c('day_sd', 'night_sd'), variable.name = 'time', value.name = 'sd')
csd <- csd[,3]
csd <- csd/farmedn # divide sd by n of fish
fcov <- cbind(covs, csd)
fcov[,c(3, 4)] <- fcov[,c(3, 4)]*100
rm(covs, csd)


# load acclimated vs. non-acclimated hourly coverage data

setwd('H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Day CSV/Outputs')
covday <- read.csv('CoverageOutput_hmeanperfish_day.csv')
covday <- as.data.frame(covday)
#covday$ID <- as.character(covday$ID)
#rownames(covday) <- covday$ID
#covday$ID <- NULL
#colnames(covday) <- seq(1, 30)
covday[,c(1, 2)] <- NULL
covday <- do.call(data.frame, aggregate(.~pen, data = covday, FUN = function(x) c(mn = mean(x)), na.action = na.omit))
covday <- cbind(as.data.frame(t(covday[,seq(2, 61, 2)])), as.data.frame(t(covday[,seq(3, 61, 2)])))
#covday <- as.data.frame(t(covday))
colnames(covday) <- c('acc_cov_day', 'naacc_cov_day', 'acc_day_sd', 'nacc_day_sd')
covday$day <- seq(1, 30)
rownames(covday) <- seq(1, 30, 1)

covnight <- read.csv('CoverageOutput_hmeanperfish_night.csv')
covnight <- as.data.frame(covnight)
#covnight$ID <- as.character(covnight$ID)
#rownames(covnight) <- covnight$ID
#covnight$ID <- NULL
#colnames(covnight) <- seq(1, 30)
covnight[,c(1, 2)] <- NULL
covnight <- do.call(data.frame, aggregate(.~pen, data = covnight, FUN = function(x) c(mn = mean(x)), na.action = na.omit))
covnight <- cbind(as.data.frame(t(covnight[,seq(2, 61, 2)])), as.data.frame(t(covnight[,seq(3, 61, 2)])))
#covnight <- as.data.frame(t(covnight))
colnames(covnight) <- c('acc_cov_night', 'nacc_cov_night', 'acc_night_sd', 'nacc_night_sd')
covnight$day <- seq(1, 30)
covnight <- covnight[-c(1, 18),]
rownames(covnight) <- seq(1, 28, 1)


coverage <- left_join(covday, covnight, by = 'day')
acov <- coverage[c(5, 1, 2, 6, 7)]
colnames(acov) <- c('day', 'day_cov', 'day_sd', 'night_cov', 'night_sd')
nacov <- coverage[c(5, 3, 4, 8, 9)]
colnames(nacov) <- c('day', 'day_cov', 'day_sd', 'night_cov', 'night_sd')

covs <- melt(acov[,c('day', 'day_cov', 'night_cov')], measure.vars = c('day_cov', 'night_cov'), variable.name = 'time', value.name = 'cov')
csd <- melt(acov[,c('day', 'day_sd', 'night_sd')], measure.vars = c('day_sd', 'night_sd'), variable.name = 'time', value.name = 'sd')
csd <- csd[,3]
csd <- csd/accn # divide sd by n of fish
acov <- cbind(covs, csd)
acov[,c(3, 4)] <- acov[,c(3, 4)]*100
#acov[38:74,4] <- NA
rm(covs, csd)

covs <- melt(nacov[,c('day', 'day_cov', 'night_cov')], measure.vars = c('day_cov', 'night_cov'), variable.name = 'time', value.name = 'cov')
csd <- melt(nacov[,c('day', 'day_sd', 'night_sd')], measure.vars = c('day_sd', 'night_sd'), variable.name = 'time', value.name = 'sd')
csd <- csd[,3]
csd <- csd/naccn # divide sd by n of fish
nacov <- cbind(covs, csd)
nacov[,c(3, 4)] <- nacov[,c(3, 4)]*100
rm(covs, csd)


# draw bar plots of wild vs. farmed hourly coverage

plotfont <- 'Arial'

wplot <- ggplot(wcov, aes(x = day, y = cov, fill = time)) 
wplot <- wplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt'))
wplot <- wplot + geom_bar(stat = 'identity', position = 'dodge') + geom_errorbar(aes(ymin = cov-csd, ymax = cov+csd), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_cov' = 'gray60', 'night_cov' = 'gray30'))
wplot <- wplot + scale_y_continuous(name = 'Coverage (%)', expand = c(0,0), limits = c(0, 4), breaks = seq(0, 4, 0.5)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 43, 1), labels = c('5', '10', '15', '20', '25', '30', '35', '40'), breaks = seq(5, 40, 5))
wplot <- wplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
wplot


fplot <- ggplot(fcov, aes(x = day, y = cov, fill = time)) 
fplot <- fplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt'))
fplot <- fplot + geom_bar(stat = 'identity', position = 'dodge') + geom_errorbar(aes(ymin = cov-csd, ymax = cov+csd), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_cov' = 'gray60', 'night_cov' = 'gray30'))
fplot <- fplot + scale_y_continuous(name = 'Coverage (%)', expand = c(0,0), limits = c(0, 4), breaks = seq(0, 4, 0.5)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 43, 1), labels = c('5', '10', '15', '20', '25', '30', '35', '40'), breaks = seq(5, 40, 5))
fplot <- fplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
fplot


acplot <- ggplot(acov, aes(x = day, y = cov, fill = time)) 
acplot <- acplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = c(0.9, 0.9), plot.margin = margin(10, 1, 10, 10, 'pt'))
acplot <- acplot + geom_bar(stat = 'identity', position = 'dodge') + geom_errorbar(aes(ymin = cov-csd, ymax = cov+csd), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_cov' = 'gray60', 'night_cov' = 'gray30'))
acplot <- acplot + scale_y_continuous(name = 'Coverage (%)', expand = c(0,0), limits = c(0, 4), breaks = seq(0, 4, 0.5)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 30, 1), labels = c('5', '10', '15', '20', '25', '30'), breaks = seq(5, 30, 5))
acplot <- acplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
acplot


naplot <- ggplot(nacov, aes(x = day, y = cov, fill = time)) 
naplot <- naplot + theme_classic() + theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 1, 10, 10, 'pt'))
naplot <- naplot + geom_bar(stat = 'identity', position = 'dodge') + geom_errorbar(aes(ymin = cov-csd, ymax = cov+csd), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_cov' = 'gray60', 'night_cov' = 'gray30'))
naplot <- naplot + scale_y_continuous(name = 'Coverage (%)', expand = c(0,0), limits = c(0, 4), breaks = seq(0, 4, 0.5)) + scale_x_discrete(name = 'Exp. day', expand = c(0,0), limits = seq(0, 30, 1), labels = c('5', '10', '15', '20', '25', '30'), breaks = seq(5, 30, 5))
naplot <- naplot +  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
naplot

plot_grid(wplot, acplot, fplot, naplot, labels = c('(a)', '(c)', '(b)', '(d)'), rel_widths = c(1,1), hjust = c(-3, -3.5, -3, -3.5))


# 8. Polar plots of headings

# load wild vs. farmed dataset


#new dayfile classes
dayfile.classes <- c('NULL', 'numeric', 'factor', 'factor', 'POSIXct', 'double', 'double', 
                     'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor',
                     'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                     'double', 'double', 'double', 'double', 'double', 'double', 'double',
                     'double', 'double', 'double', 'double', 'double', 'double', 'double',
                     'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
                     'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor',
                     'factor', 'factor', 'double', 'double', 'double', 'double', 'double', 
                     'double', 'double', 'double', 'double', 'double', 'double', 'double', 'factor'
)

# Load wild vs. farmed data
workingdir <- 'H:/Data processing/2015 Wild vs. Farmed/Cropped data/Coded Day CSV'
setwd(workingdir)
  files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
  dayfile <- data.frame()
  
  for(i in 1:length(files)){
    daytemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)
    dayfile <- rbind(dayfile, daytemp)
  }

wfdata <- dayfile[c(1, 3, 4, 10, 13, 47, 48)]

hadjconst <- 24.88

# readjust headings from local to global coordinates
wfdata$HEAD <- ifelse(wfdata$HEAD-hadjconst <0, 360-(hadjconst-wfdata$HEAD), wfdata$HEAD-hadjconst)

wfdata$HEIGHT <- factor(wfdata$HEIGHT, levels(wfdata$HEIGHT)[c(2, 1, 3)]) # reorder factor levels

threshold <- 0.1

whead <- subset(wfdata, PEN == 7 & MSEC >= threshold)
whead <- subset(whead, HEIGHT == 'S' | HEIGHT == 'N')
fhead <- subset(wfdata, PEN == 8 & MSEC >= threshold)
fhead <- subset(fhead, HEIGHT == 'S' | HEIGHT == 'N')


# Load acclimated vs. non-acclimated data

workingdir <- 'H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Day CSV'
setwd(workingdir)
files <- list.files(path = workingdir, pattern = '*.csv', all.files = FALSE, recursive = FALSE)
dayfile <- data.frame()

for(i in 1:length(files)){
  daytemp <- read.csv(files[[i]], header = TRUE, sep = ",", colClasses = dayfile.classes)
  dayfile <- rbind(dayfile, daytemp)
}

accdata <- dayfile[c(1, 3, 4, 10, 13, 47, 48)]

threshold <- 0.1

ahead <- subset(accdata, PEN == 7 & MSEC >= threshold)
ahead <- subset(ahead, HEIGHT == 'S' | HEIGHT == 'N')
nahead <- subset(accdata, PEN == 8 & MSEC >= threshold)
nahead <- subset(nahead, HEIGHT == 'S' | HEIGHT == 'N')


# polar plots of wild and farmed fish

pplotw <- ggplot(whead, aes(x = HEAD, fill = HEIGHT))
pplotw <- pplotw + geom_histogram(breaks = seq(0, 360, 10), size = 0.5, closed = 'left', color = 'black') + #, alpha = 0) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank(), legend.key.size = unit(1.5, 'lines'), legend.key.height = unit(1.5, 'lines')) + 
  theme(text = element_text(family = plotfont, size = 14), plot.margin = margin(0, 0, 0, 0, 'pt'), legend.position = c(1, 0.1)) +
  scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 330, 30), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330'), minor_breaks = seq(0, 360, 10)) +
  scale_fill_manual(guide = guide_legend(title = 'Tide',  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Spring', 'Neap'), values = c('S' = 'gray70', 'N' = 'gray40')) +
  scale_y_continuous(breaks = NULL) + #, limits = c(0, 40000)) +
  theme(axis.text.x = element_text(size = 14)) +
  coord_polar(theta = 'x', start = 0)

pplotf <- ggplot(fhead, aes(x = HEAD, fill = HEIGHT))
pplotf <- pplotf + geom_histogram(breaks = seq(0, 360, 10), size = 0.5, closed = 'left', color = 'black') + #, alpha = 0) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(text = element_text(family = plotfont, size = 14), plot.margin = margin(0, 0, 0, 0, 'pt'), legend.position = 'none') +
  scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 330, 30), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330'), minor_breaks = seq(0, 360, 10)) +
  scale_fill_manual(guide = guide_legend(title = 'Tide',  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Spring', 'Neap'), values = c('S' = 'gray70', 'N' = 'gray40')) +
  scale_y_continuous(breaks = NULL) + #, limits = c(0, 40000)) +
  theme(axis.text.x = element_text(size = 14)) +
  coord_polar(theta = 'x', start = 0)

# polar plots of acclimated and non-acclimated fish

pplota <- ggplot(ahead, aes(x = HEAD, fill = HEIGHT))
pplota <- pplota + geom_histogram(breaks = seq(0, 360, 10), size = 0.75, closed = 'left', color = 'black') + #, alpha = 0) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(text = element_text(family = plotfont, size = 14), plot.margin = margin(0, 0, 0, 0, 'pt'), legend.position = 'none') +
  scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 330, 30), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330'), minor_breaks = seq(0, 360, 10)) +
  scale_fill_manual(guide = guide_legend(title = 'Tide',  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Spring', 'Neap'), values = c('S' = 'gray70', 'N' = 'gray40')) +
  scale_y_continuous(breaks = NULL) + #, limits = c(0, 40000)) +
  theme(axis.text.x = element_text(size = 14)) +
  coord_polar(theta = 'x', start = 0)

pplotna <- ggplot(nahead, aes(x = HEAD, fill = HEIGHT))
pplotna <- pplotna + geom_histogram(breaks = seq(0, 360, 10), size = 0.75, closed = 'left', color = 'black') + #, alpha = 0) + 
  theme_minimal() + theme(axis.text.y = element_blank(), axis.title.y = element_blank()) + 
  theme(text = element_text(family = plotfont, size = 14), plot.margin = margin(0, 0, 0, 0, 'pt'), legend.position = 'none') +
  scale_x_continuous('', limits = c(0, 360), expand = c(0, 0), breaks = seq(0, 330, 30), labels = c('0', '30', '60', '90', '120', '150', '180', '210', '240', '270', '300', '330'), minor_breaks = seq(0, 360, 10)) +
  scale_fill_manual(guide = guide_legend(title = 'Tide',  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Spring', 'Neap'), values = c('S' = 'gray70', 'N' = 'gray40')) +
  scale_y_continuous(breaks = NULL) + #, limits = c(0, 40000)) +
  theme(axis.text.x = element_text(size = 14)) +
  coord_polar(theta = 'x', start = 0)

# plot four polar plots and label

plot_grid(pplotw, pplota, pplotf, pplotna, labels = c('(a)', '(c)', '(b)', '(d)'), rel_widths = c(1,1), hjust = c(-2, -2, -2, -2), vjust = c(5, 5, 5, 5))
plot_grid(pplotw, pplotf, labels = c('(a)', '(b)'), rel_widths = c(1,1), hjust = c(-2, -2), vjust = c(5, 5))


# 9. Activity by time of day for wild, hatchery acclimated and hatchery and pen acclimated

# Read in wild vs. farmed activity data

detach('package:xlsx')
library(openxlsx)

setwd('H:/Data processing/2015 Wild vs. Farmed/Cropped data/Coded Fish CSV/Outputs')
wfact <- read.xlsx('ActivityTotOutput2.xlsx', sheet = 1)
rownames(wfact) <- wfact$ID
wfact$ID <- NULL
wfact <- t(wfact)
wact <- wfact[,1:8]
fact <- wfact[,9:16]
wact <- as.data.frame(wact[!(rowSums(is.na(wact))),])
fact <- as.data.frame(fact[!(rowSums(is.na(fact))),])
wact <- setDT(wact, keep.rownames = T)[]
colnames(wact)[1] <- 'fishID'
fact <- setDT(fact, keep.rownames = T)[]
colnames(fact)[1] <- 'fishID'

wact <- wact[order(P7_day_mean),]
wact$fishID <- as.factor(wact$fishID)
flevels <- wact$fishID
wact$fishID <- factor(wact$fishID, levels = flevels)

# Read in acclimated A activity data

setwd('H:/Data analysis/Acoustic tag - Preconditioning A/Filtered Data')
hacact <- read.xlsx('PreconA_analysisbyfish_filtered.xlsx', sheet = 4, rows = seq(2, 10, 1), cols = seq(1, 14, 1), colNames = T, rowNames = T)
hacact <- as.data.frame(t(hacact))
hacact <- setDT(hacact, keep.rownames = T)[]
colnames(hacact)[1] <- 'fishID'
hacact <- hacact[order(Day_mean),]
hacact$fishID <- as.factor(hacact$fishID)
flevels <- hacact$fishID
hacact$fishID <- factor(hacact$fishID, levels = flevels)

colnames(wact) <- colnames(hacact)
colnames(fact) <- colnames(hacact)

# Read in acclimated B activity data

setwd('H:/Data analysis/Acoustic tag - Preconditioning B/Filtered Data')
hpacact <- read.xlsx('PreconB_analysisbyfish_filtered.xlsx', sheet = 4, rows = seq(2, 10, 1), cols = seq(1, 11, 1), colNames = T, rowNames = T)
hpacact <- as.data.frame(t(hpacact))
hpacact <- setDT(hpacact, keep.rownames = T)[]
colnames(hpacact)[1] <- 'fishID'
hpacact <- hpacact[order(Day_mean),]
hpacact$fishID <- as.factor(hpacact$fishID)
flevels <- hpacact$fishID
hpacact$fishID <- factor(hpacact$fishID, levels = flevels)

mact <- melt(wact[,c('fishID', 'Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean')], measure.vars = c('Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean'), variable.name = 'time', value.name = 'act')
actse <- melt(wact[,c('fishID', 'Dawn_se', 'Day_se', 'Dusk_se', 'Night_se')], measure.vars = c('Dawn_se', 'Day_se', 'Dusk_se', 'Night_se'), variable.name = 'time', value.name = 'se')
actse <- actse[,3]
wact <- cbind(mact, actse)
rm(mact, actse)
mact <- melt(hacact[,c('fishID', 'Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean')], measure.vars = c('Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean'), variable.name = 'time', value.name = 'act')
actse <- melt(hacact[,c('fishID', 'Dawn_se', 'Day_se', 'Dusk_se', 'Night_se')], measure.vars = c('Dawn_se', 'Day_se', 'Dusk_se', 'Night_se'), variable.name = 'time', value.name = 'se')
actse <- actse[,3]
hacact <- cbind(mact, actse)
rm(mact, actse)
mact <- melt(hpacact[,c('fishID', 'Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean')], measure.vars = c('Dawn_mean', 'Day_mean', 'Dusk_mean', 'Night_mean'), variable.name = 'time', value.name = 'act')
actse <- melt(hpacact[,c('fishID', 'Dawn_se', 'Day_se', 'Dusk_se', 'Night_se')], measure.vars = c('Dawn_se', 'Day_se', 'Dusk_se', 'Night_se'), variable.name = 'time', value.name = 'se')
actse <- actse[,3]
hpacact <- cbind(mact, actse)
rm(mact, actse)

# hpacact <- as.data.frame(hpacact)

# Create dataframes of significance levels

hpacsig <- data.frame(fishID = unique(hpacact$fishID), act = c(0.48, 0.5, 0.6, 0.61, 0.75, 0.82, 0.9, 0.99, 1.06, 1.05))
wsig <- data.frame(fishID = unique(wact$fishID), act = c(0.31, 0.42, 0.43, 0.43, 0.44, 0.45, 0.45, 0.45, 0.47, 0.49, 0.5, 0.58, 0.59, 0.59))
hacsig <- data.frame(fishID = unique(hacact$fishID), act = c(0.6, 0.6, 0.61, 0.63, 0.64, 0.64, 0.64, 0.7, 0.7, 0.72, 0.85, 0.85, 1.01))

# Draw barplots of activity

plotfont <- 'Arial'

wpact <- ggplot(wact, aes(x = fishID, y = act)) +
  theme_classic() + 
  theme(text = element_text(family = plotfont, size = 14), legend.position = c(0.2, 0.9), plot.margin = margin(10, 5, 10, 1, 'pt')) + 
  geom_bar(aes(fill = time), stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = act-se, ymax = act+se, fill = time), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Dawn', 'Day', 'Dusk', 'Night'), values = c('Dawn_mean' = 'gray40', 'Day_mean' = 'gray80', 'Dusk_mean' = 'gray60', 'Night_mean' = 'gray20')) +
  scale_y_continuous(name = 'Activity (BL/s)', expand = c(0,0), limits = c(0, 1.1), breaks = seq(0, 1.1, 0.1)) + 
  scale_x_discrete(name = 'fish ID', expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  geom_text(data = wsig, label = c(' ', '*', '*', '**', '*', '*', '*', '*', '*', '*', '*', '**', '*', '**'), size = 5, hjust = 'centre')
wpact

hacpact <- ggplot(hacact, aes(x = fishID, y = act)) +
  theme_classic() + 
  theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt')) +
  geom_bar(aes(fill = time), stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = act-se, ymax = act+se, fill = time), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Dawn', 'Day', 'Dusk', 'Night'), values = c('Dawn_mean' = 'gray40', 'Day_mean' = 'gray80', 'Dusk_mean' = 'gray60', 'Night_mean' = 'gray20')) +
  scale_y_continuous(name = 'Activity (BL/s)', expand = c(0,0), limits = c(0, 1.1), breaks = seq(0, 1.1, 0.1)) + 
  scale_x_discrete(name = 'fish ID', expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  geom_text(data = hacsig, label = c(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*'), size = 5, hjust = 'centre')
hacpact


hpacpact <- ggplot(hpacact, aes(x = fishID, y = act)) + #, fill = time)) +
  theme_classic() + 
  theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt')) +
  geom_bar(aes(fill = time), stat = 'identity', position = 'dodge') + 
  geom_errorbar(aes(ymin = act-se, ymax = act+se, fill = time), width = 0.4, position = position_dodge(1)) +
  scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Dawn', 'Day', 'Dusk', 'Night'), values = c('Dawn_mean' = 'gray40', 'Day_mean' = 'gray80', 'Dusk_mean' = 'gray60', 'Night_mean' = 'gray20')) +
  scale_y_continuous(name = 'Activity (BL/s)', expand = c(0,0), limits = c(0, 1.1), breaks = seq(0, 1.1, 0.1)) + 
  scale_x_discrete(name = 'fish ID', expand = c(0,0)) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14)) +
  geom_text(data = hpacsig, label = c(' ', ' ', ' ', ' ', '*', ' ', '*', '*', '*', '**'), size = 5, hjust = 'centre')
hpacpact

actleg <- get_legend(wpact)
wpact <- wpact + theme(legend.position = 'none')

plot_grid(wpact, actleg, hacpact, hpacpact, labels = c('(a)', ' ', '(b)', '(c)'), rel_widths = c(1,1), hjust = c(-4, -4, -4, -4), vjust = c(3, 3, 3, 3))

plot_grid(plot_grid(wpact, actleg, labels = c('(a)', ' '), hjust = c(-4, -4), vjust = c(3, 3), rel_widths = c(1.2, 0.8), scale = c(1, 0)), plot_grid(hacpact, hpacpact, labels = c('(b)', '(c)'), hjust = c(-4, -4), vjust = c(3, 3), rel_widths = c(1.13, 0.87)), nrow = 2, ncol = 1)



# 10. Line plots of night and day depth and activity for non-acclimated and hatchery and pen-acclimated

detach('package:xlsx')
library(openxlsx)
library(data.table)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(cowplot)

# Read and modify depth data

setwd('H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Day CSV')
mdepth <- read.xlsx('DepthTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mdepth <- as.data.frame(t(mdepth))
mdepth <- setDT(mdepth, keep.rownames = T)[]
colnames(mdepth)[1] <- 'Day'
mdepth$Day <- as.numeric(mdepth$Day)
accdepth <- mdepth[,c(1, 4, 5, 8, 9)]
#accdepth$P7_day_mean <- as.numeric(levels(accdepth$P7_day_mean))[accdepth$P7_day_mean]
#accdepth$P7_day_se <- as.numeric(levels(accdepth$P7_day_se))[accdepth$P7_day_se]
#accdepth$P7_night_mean <- as.numeric(levels(accdepth$P7_night_mean))[accdepth$P7_night_mean]
#accdepth$P7_night_se <- as.numeric(levels(accdepth$P7_night_se))[accdepth$P7_night_se]

mdep <- melt(accdepth[,c('Day', 'P7_day_mean', 'P7_night_mean')], measure.vars = c('P7_day_mean', 'P7_night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(accdepth[,c('Day', 'P7_day_se', 'P7_night_se')], measure.vars = c('P7_day_se', 'P7_night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
accdepth <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(accdepth) <- c('daynum', 'tod', 'mdepth', 'se')
#accdepth$daynum <- as.factor(accdepth$daynum)

naccdepth <- mdepth[,c(1, 12, 13, 16, 17)]
#naccdepth$P8_day_mean <- as.numeric(levels(naccdepth$P8_day_mean))[naccdepth$P8_day_mean]
#naccdepth$P8_day_se <- as.numeric(levels(naccdepth$P8_day_se))[naccdepth$P8_day_se]
#naccdepth$P8_night_mean <- as.numeric(levels(naccdepth$P8_night_mean))[naccdepth$P8_night_mean]
#naccdepth$P8_night_se <- as.numeric(levels(naccdepth$P8_night_se))[naccdepth$P8_night_se]

mdep <- melt(naccdepth[,c('Day', 'P8_day_mean', 'P8_night_mean')], measure.vars = c('P8_day_mean', 'P8_night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(naccdepth[,c('Day', 'P8_day_se', 'P8_night_se')], measure.vars = c('P8_day_se', 'P8_night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
naccdepth <- cbind(mdep, sedep)
colnames(naccdepth) <- c('daynum', 'tod', 'mdepth', 'se')

rm(mdep, sedep)

# read and modify activity data for non-acclimated and hatchery and pen-acclimated

setwd('H:/Data processing/2016 Conditioning study B/Filtered Data/Recoded Day CSV')
mact <- read.xlsx('ActivityTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mact <- as.data.frame(t(mact))
mact <- setDT(mact, keep.rownames = T)[]
colnames(mact)[1] <- 'Day'
mact$Day <- as.numeric(mact$Day)
accact <- mact[,c(1, 4, 5, 8, 9)]
#accact$P7_day_mean <- as.numeric(levels(accact$P7_day_mean))[accact$P7_day_mean]
#accact$P7_day_se <- as.numeric(levels(accact$P7_day_se))[accact$P7_day_se]
#accact$P7_night_mean <- as.numeric(levels(accact$P7_night_mean))[accact$P7_night_mean]
#accact$P7_night_se <- as.numeric(levels(accact$P7_night_se))[accact$P7_night_se]

mac <- melt(accact[,c('Day', 'P7_day_mean', 'P7_night_mean')], measure.vars = c('P7_day_mean', 'P7_night_mean'), variable.name = 'time', value.name = 'mean')
seac <- melt(accact[,c('Day', 'P7_day_se', 'P7_night_se')], measure.vars = c('P7_day_se', 'P7_night_se'), variable.name = 'time', value.name = 'se')
seac <- seac[,3]
accact <- cbind(mac, seac)
colnames(accact) <- c('daynum', 'tod', 'act', 'se')
rm(mac, seac)

naccact <- mact[,c(1, 12, 13, 16, 17)]
#naccact$P8_day_mean <- as.numeric(levels(naccact$P8_day_mean))[naccact$P8_day_mean]
#naccact$P8_day_se <- as.numeric(levels(naccact$P8_day_se))[naccact$P8_day_se]
#naccact$P8_night_mean <- as.numeric(levels(naccact$P8_night_mean))[naccact$P8_night_mean]
#naccact$P8_night_se <- as.numeric(levels(naccact$P8_night_se))[naccact$P8_night_se]

mac <- melt(naccact[,c('Day', 'P8_day_mean', 'P8_night_mean')], measure.vars = c('P8_day_mean', 'P8_night_mean'), variable.name = 'time', value.name = 'mean')
seac <- melt(naccact[,c('Day', 'P8_day_se', 'P8_night_se')], measure.vars = c('P8_day_se', 'P8_night_se'), variable.name = 'time', value.name = 'se')
seac <- seac[,3]
naccact <- cbind(mac, seac)
colnames(naccact) <- c('daynum', 'tod', 'act', 'se')
rm(mac, seac)

#accdpsig <- data.frame(daynum = unique(accdepth$daynum), mdepth = c(15, 12, 11, 8, 7, 6, 7, 7, 7, 6, 5, 7, 7, 8, 8, 7, 7, 6, 6, 7, 8, 9, 10, 10, 9, 10, 10, 10, 7, 7))
accdepth$sig <- rep(c(15, 12.4, 10.5, 8.2, 7, 6, 8, 7.6, 7.4, 6.6, 4.6, 7.3, 7, 8.5, 8, 7.6, 7.9, 6.5, 6.1, 7.4, 8, 9, 10, 10, 9, 7.7, 9.9, 9.6, 7.6, 8), 2)
naccdepth$sig <- rep(c(1, 14.9, 15.4, 1, 1, 14.9, 11.5, 1, 12.3, 12.3, 10.5, 1, 1, 11.1, 1, 1, 1, 10.8, 10.6, 1, 1, 1, 1, 10.9, 1, 1, 11.3, 1, 1, 1), 2)
accact$sig <- rep(c(0.1, 0.1, 0.82, 0.86, 0.85, 1.13, 0.8, 0.74, 0.76, 0.81, 0.75, 0.8, 0.1, 0.8, 0.98, 0.1, 0.79, 0.82, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.64, 0.1, 0.74, 0.1), 2)
naccact$sig <- rep(c(0.1, 0.67, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.73, 0.8, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.72, 0.1, 0.1, 0.1, 0.1, 0.1), 2)


# draw depth plots for acclimated and non acclimated groups

plotfont <- 'Arial'

accdp <- ggplot(accdepth, aes(x = daynum, y = mdepth, group = tod)) +
  theme_classic() +
  theme(text = element_text(family = plotfont, size = 14), legend.position = c(0.9, 0.2), plot.margin = margin(10, 5, 10, 1, 'pt')) +
  scale_y_reverse(name = 'Mean depth (m)', limits = c(25, 0), breaks = seq(25, 0, -5), expand = c(0,0)) +
  geom_line(aes(colour = tod), stat = 'identity', size = 1.2) + 
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  geom_point(aes(colour = tod), size = 3) +
  geom_errorbar(aes(ymin = mdepth-se, ymax = mdepth+se, group = tod, colour = tod), width = 0.2, size = 1) +
  #scale_y_continuous(name = 'Mean depth (m)', expand = c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + 
  scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, 30, 5)) +
  scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('P7_day_mean' = 'gray70', 'P7_night_mean' = 'gray20')) +
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))  +
  geom_text(aes(x = daynum, y = sig), label = rep(c(' ', '*', '*', '*', '**', ' ', ' ', '***', '*', '*', '*', '**', ' ', '*', ' ', '*', '*', '*', '**', '**', ' ', ' ', ' ', ' ', ' ', '*', '*', '*', '*', '*'), 2), size = 5, hjust = 'centre')
accdp

naccdp <- ggplot(naccdepth, aes(x = daynum, y = mdepth, group = tod)) +
  theme_classic() +
  theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt')) +
  scale_y_reverse(name = 'Mean depth (m)', limits = c(25, 0), breaks = seq(25, 0, -5), expand = c(0,0)) +
  geom_line(aes(colour = tod), stat = 'identity', size = 1.2) + 
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  geom_point(aes(colour = tod), size = 3) +
  geom_errorbar(aes(ymin = mdepth-se, ymax = mdepth+se, group = tod, colour = tod), width = 0.2, size = 1) +
  #scale_y_continuous(name = 'Mean depth (m)', expand = c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + 
  scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, 30, 5)) +
  scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('P8_day_mean' = 'gray70', 'P8_night_mean' = 'gray20')) +
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))  +
  geom_text(aes(x = daynum, y = sig), label = rep(c(' ', '*', '*', ' ', ' ', '*', '*', ' ', '*', '*', '*', ' ', ' ', '*', ' ', ' ', ' ', '*', '*', ' ', ' ', ' ', ' ', '*', ' ', ' ', '*', ' ', ' ', ' '), 2), size = 5, hjust = 'centre')
naccdp



# draw activity plots for acclimated and non acclimated groups

accap <- ggplot(accact, aes(x = daynum, y = act, group = tod)) +
  theme_classic() +
  theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt')) +
  scale_y_continuous(name = 'Mean activity (BL/s)', limits = c(0, 1.2), breaks = seq(0, 1.2, 0.1), expand = c(0,0)) +
  geom_line(aes(colour = tod), stat = 'identity', size = 1.2) + 
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  geom_point(aes(colour = tod), size = 3) +
  geom_errorbar(aes(ymin = act-se, ymax = act+se, group = tod, colour = tod), width = 0.2, size = 1) +
  #scale_y_continuous(name = 'Mean depth (m)', expand = c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + 
  scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, 30, 5)) +
  scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('P7_day_mean' = 'gray70', 'P7_night_mean' = 'gray20')) +
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))  +
  geom_text(aes(x = daynum, y = sig), label = rep(c(' ', ' ', '*', '**', '*', '*', '*', '*', '*', '*', '*', '*', ' ', '*', '*', ' ', '*', '*', '*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*', ' ', '*', ' '), 2), size = 5, hjust = 'centre')
accap

naccap <- ggplot(naccact, aes(x = daynum, y = act, group = tod)) +
  theme_classic() +
  theme(text = element_text(family = plotfont, size = 14), legend.position = 'none', plot.margin = margin(10, 5, 10, 1, 'pt')) +
  scale_y_continuous(name = 'Mean activity (BL/s)', limits = c(0, 1.2), breaks = seq(0, 1.2, 0.1), expand = c(0,0)) +
  geom_line(aes(colour = tod), stat = 'identity', size = 1.2) + 
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  geom_point(aes(colour = tod), size = 3) +
  geom_errorbar(aes(ymin = act-se, ymax = act+se, group = tod, colour = tod), width = 0.2, size = 1) +
  #scale_y_continuous(name = 'Mean depth (m)', expand = c(0,0), limits = c(0, 25), breaks = seq(0, 25, 5)) + 
  scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, 30, 5)) +
  scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('P8_day_mean' = 'gray70', 'P8_night_mean' = 'gray20')) +
  #scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = 14, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('Day' = 'gray80', 'Night' = 'gray20')) +
  theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))  +
  geom_text(aes(x = daynum, y = sig), label = rep(c(' ', '*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*', '*', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '*', ' ', ' ', ' ', ' ', ' '), 2), size = 5, hjust = 'centre')
naccap

plot_grid(naccdp, accdp, naccap, accap, labels = c('(a)', '(b)', '(c)', '(d)'), rel_widths = c(1,1), hjust = c(-4, -4, -4, -4), vjust = c(3, 3, 3, 3))



# 11. Survival for wild vs. farmed and both acclimation trials

t1tot <- 18
t2atot <- 20
t2btot <- 17

detach('package:xlsx')
library(openxlsx)
setwd('H:/Data processing')
mortsdf <- read.xlsx('TaggingSurgeryMorts.xlsx', sheet = 1, rows = c(3, seq(6, 48)), cols = c(1, 4, 5, 6, 7, 8, 9))
colnames(mortsdf)[1] <- 'Days_postdeployment'
mortsdf$Days_postdeployment <- seq(1, 43)
mortsdf$T2_Wild <- ((t1tot - mortsdf$T2_Wild)/t1tot)*100
mortsdf$T2_Farmed <- ((t1tot - mortsdf$T2_Farmed)/t1tot)*100
mortsdf$T3_acclimated <- ((t2atot - mortsdf$T3_acclimated)/t2atot)*100
mortsdf$T3_nonacclimated <- ((t2atot - mortsdf$T3_nonacclimated)/t2atot)*100
mortsdf$T4_acclimated <- ((t2btot - mortsdf$T4_acclimated)/t2btot)*100
mortsdf$T4_nonacclimated <- ((t2btot - mortsdf$T4_nonacclimated)/t2btot)*100
mortsdf[31:43,4:7] <- NA
mortsdf <- melt(mortsdf, id.var = 1)
mortsdf$variable <- c(rep('Trial 1: wild', 43), rep('Trial 1: farmed', 43), rep('Trial 2a: acclimated', 43), rep('Trial 2a: non-acclimated', 43), rep('Trial 2b: acclimated', 43), rep('Trial 2b: non-acclimated', 43))
mortsdf$variable <- factor(mortsdf$variable, levels(as.factor(mortsdf$variable))[c(2, 1, 3, 4, 5, 6)])
mortsdf$morts <- 100-mortsdf$value

# draw line plot of survival

ggplot(mortsdf) +  
  scale_x_continuous('Trial Day', limits = c(1, 43)) + 
  scale_y_continuous('Survival (%)', limits = c(40,100)) +
  theme_classic() + theme(text = element_text(family = plotfont, size = 12), legend.position = c(0.15, 0.2)) +
  geom_line(aes(x = Days_postdeployment, y = value, group = variable, color = variable, linetype = variable), size = 1) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  #geom_line(aes(Days_postdeployment, T2_Farmed, colour = 'Trial 1: farmed', linetype = 'Trial 1: farmed'), size = 1) + #, size = 0.7, color = 'gray', linetype = 'solid') + 
  #geom_line(aes(Days_postdeployment, T3_acclimated, colour = 'Trial 2a: acclimated', linetype = 'Trial 2a: acclimated'), size = 1) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  #geom_line(aes(Days_postdeployment, T3_nonacclimated, colour = 'Trial 2a: non-acclimated', linetype = 'Trial 2a: non-acclimated'), size = 1) + #, size = 0.7, color = 'black', linetype = 'solid') +
  #geom_line(aes(Days_postdeployment, T4_acclimated, colour = 'Trial 2b: acclimated', linetype = 'Trial 2b: acclimated'), size = 1) + #, size = 0.7, color = 'black', linetype = 'longdash') +
  #geom_line(aes(Days_postdeployment, T4_nonacclimated, colour = 'Trial 2b: non-acclimated', linetype = 'Trial 2b: non-acclimated'), size = 1) + #, size = 0.7, color = 'black', linetype = 'solid') +
  scale_colour_manual(name = '', values = c('Trial 1: wild' = 'black', 'Trial 1: farmed' = 'gray', 'Trial 2a: acclimated' = 'black', 'Trial 2a: non-acclimated' = 'gray', 'Trial 2b: acclimated' = 'black', 'Trial 2b: non-acclimated' = 'gray')) +
  scale_linetype_manual(name = '', values = c('Trial 1: wild' = 'solid', 'Trial 1: farmed' = 'solid', 'Trial 2a: acclimated' = 'dashed', 'Trial 2a: non-acclimated' = 'dashed', 'Trial 2b: acclimated' = 'dotted', 'Trial 2b: non-acclimated' = 'dotted')) +
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) # +

# draw bar plot of survival

ggplot(mortsdf, aes(x = Days_postdeployment, y = 100-value, group = variable, fill = variable, colour = variable, linetype = variable)) +  
  scale_x_continuous('Trial Day', limits = c(1, 43), expand = c(0,0)) + 
  scale_y_continuous('Survival (%)', limits = c(0,100), expand = c(0,0)) +
  theme_classic() + theme(text = element_text(family = plotfont, size = 12), legend.position = c(0.25, 0.8)) +
  geom_bar(stat = 'identity', position = 'dodge', width = 1.4) + #, size = 0.7, color = 'gray', linetype = 'longdash') +
  scale_fill_manual(name = '',
                      labels = c('Trial 1: wild', 'Trial 1: farmed', 'Trial 2a: acclimated', 'Trial 2a: non-acclimated', 'Trial 2b: acclimated', 'Trial 2b: non-acclimated'),
                      values = c('black', 'black',  'gray45', 'gray45', 'gray75', 'gray75')) +
  scale_colour_manual(name = '',
                        labels = c('Trial 1: wild', 'Trial 1: farmed', 'Trial 2a: acclimated', 'Trial 2a: non-acclimated', 'Trial 2b: acclimated', 'Trial 2b: non-acclimated'),
                        values = c('black', 'black',  'black', 'black', 'black', 'black')) +
  scale_linetype_manual(name = '',
                    labels = c('Trial 1: wild', 'Trial 1: farmed', 'Trial 2a: acclimated', 'Trial 2a: non-acclimated', 'Trial 2b: acclimated', 'Trial 2b: non-acclimated'),
                    values = c('solid', 'dotted',  'solid', 'dotted', 'solid', 'dotted')) + 
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12)) # +


# 12. Heatmap of depth over time--------------------------------

dayfile <- wildvsfarmed[sample(nrow(wildvsfarmed), 10000),] # random sample to reduce plot drawing time

# draw heatmap of time vs. depth

dm <- ggplot(dayfile[dayfile$PEN == '7',], aes(x = EchoTime, y = PosZ)) +
  stat_density_2d(geom = 'raster', aes(fill = stat(density)), contour = F) +
  geom_density_2d(aes(colour = PosZ)) + #ylim(25, 0) +
  #scale_y_continuous(expand = c(0, 0), limits = c(0, 25)) + 
  scale_y_reverse(name = 'Depth (m)', expand = c(0, 0), limits = c(25, 0)) +
  scale_x_datetime(name = 'Date', expand = c(0, 0))
dm



# 13. Line plots of night and day depth for all three trials

detach('package:xlsx')
library(openxlsx)
library(data.table)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(cowplot)

# Read and modify trial 1 depth data

setwd('H:/Acoustic tag - Wild vs. Farmed/Data processing/Cropped data/Coded Day CSV/Outputs')
#mdepth <- read.xlsx('DepthTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mdepth <- read.csv('DepthTotOutput.csv')
rownames(mdepth) <- mdepth$X
mdepth[,c(1, 2, 40, 41, 42, 43, 44, 45)] <- NULL
colnames(mdepth) <- c(seq(1, 14, 1), seq(19, 26, 1), seq(29, 43, 1))
mdepth <- as.data.frame(t(mdepth))
mdepth <- setDT(mdepth, keep.rownames = T)[]
colnames(mdepth)[1] <- 'Day'
mdepth$Day <- as.numeric(mdepth$Day)

wdepth <- mdepth[,c(1, 4, 5, 8, 9)]
colnames(wdepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(wdepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(wdepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
wdepth <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(wdepth) <- c('daynum', 'tod', 'mdepth', 'se')
wdepth <- filter(wdepth, !is.na(mdepth))
#wdepth$daynum <- as.factor(wdepth$daynum)

fdepth <- mdepth[,c(1, 12, 13, 16, 17)]
colnames(fdepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(fdepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(fdepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
fdepth <- cbind(mdep, sedep)
colnames(fdepth) <- c('daynum', 'tod', 'mdepth', 'se')
fdepth <- filter(fdepth, !is.na(mdepth))

rm(mdep, sedep)


# Read and modify trial 2a depth data

setwd('H:/Acoustic tag - Preconditioning A/Data processing/Filtered Data/Recoded Day CSV/Outputs')
#mdepth <- read.xlsx('DepthTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mdepth <- read.csv('DepthTotOutput.csv')
rownames(mdepth) <- mdepth$X
mdepth[,c(1, 2)] <- NULL
colnames(mdepth) <- seq(1, 30, 1)
mdepth <- as.data.frame(t(mdepth))
mdepth <- setDT(mdepth, keep.rownames = T)[]
colnames(mdepth)[1] <- 'Day'
mdepth$Day <- as.numeric(mdepth$Day)

acc2adepth <- mdepth[,c(1, 4, 5, 8, 9)]
colnames(acc2adepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(acc2adepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(acc2adepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
acc2adepth <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(acc2adepth) <- c('daynum', 'tod', 'mdepth', 'se')

nacc2adepth <- mdepth[,c(1, 12, 13, 16, 17)]
colnames(nacc2adepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(nacc2adepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(nacc2adepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
nacc2adepth <- cbind(mdep, sedep)
colnames(nacc2adepth) <- c('daynum', 'tod', 'mdepth', 'se')

rm(mdep, sedep)


# Read and modify trial 2b depth data

setwd('H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Day CSV/Outputs')
mdepth <- read.xlsx('DepthTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mdepth <- as.data.frame(t(mdepth))
mdepth <- setDT(mdepth, keep.rownames = T)[]
colnames(mdepth)[1] <- 'Day'
mdepth$Day <- as.numeric(mdepth$Day)

acc2bdepth <- mdepth[,c(1, 4, 5, 8, 9)]
colnames(acc2bdepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(acc2bdepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(acc2bdepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
acc2bdepth <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(acc2bdepth) <- c('daynum', 'tod', 'mdepth', 'se')

nacc2bdepth <- mdepth[,c(1, 12, 13, 16, 17)]
colnames(nacc2bdepth) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(nacc2bdepth[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(nacc2bdepth[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
nacc2bdepth <- cbind(mdep, sedep)
colnames(nacc2bdepth) <- c('daynum', 'tod', 'mdepth', 'se')

rm(mdep, sedep)

# vectors to position effect size asterisks

#accdpsig <- data.frame(daynum = unique(accdepth$daynum), mdepth = c(15, 12, 11, 8, 7, 6, 7, 7, 7, 6, 5, 7, 7, 8, 8, 7, 7, 6, 6, 7, 8, 9, 10, 10, 9, 10, 10, 10, 7, 7))
accdepth$sig <- rep(c(15, 12.4, 10.5, 8.2, 7, 6, 8, 7.6, 7.4, 6.6, 4.6, 7.3, 7, 8.5, 8, 7.6, 7.9, 6.5, 6.1, 7.4, 8, 9, 10, 10, 9, 7.7, 9.9, 9.6, 7.6, 8), 2)
naccdepth$sig <- rep(c(1, 14.9, 15.4, 1, 1, 14.9, 11.5, 1, 12.3, 12.3, 10.5, 1, 1, 11.1, 1, 1, 1, 10.8, 10.6, 1, 1, 1, 1, 10.9, 1, 1, 11.3, 1, 1, 1), 2)
accact$sig <- rep(c(0.1, 0.1, 0.82, 0.86, 0.85, 1.13, 0.8, 0.74, 0.76, 0.81, 0.75, 0.8, 0.1, 0.8, 0.98, 0.1, 0.79, 0.82, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.64, 0.1, 0.74, 0.1), 2)
naccact$sig <- rep(c(0.1, 0.67, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.73, 0.8, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.72, 0.1, 0.1, 0.1, 0.1, 0.1), 2)

# draw depth plots for acclimated and non acclimated groups

plotfont <- 'Arial'
fs <- 12

mdepplot <- function(df, leg, el){

dplot <- ggplot(df, aes(x = daynum, y = mdepth, group = tod)) +
  theme_classic() +
  theme(text = element_text(family = plotfont, size = fs), plot.margin = margin(10, 5, 10, 1, 'pt')) +
  scale_y_reverse(name = 'Mean depth (m)', limits = c(25, 0), breaks = seq(25, 0, -5), expand = c(0,0)) +
  geom_line(aes(colour = tod), stat = 'identity', size = 1) + 
  geom_point(aes(colour = tod), size = 2) +
  geom_errorbar(aes(ymin = mdepth-se, ymax = mdepth+se, group = tod, colour = tod), width = 0.2, size = 1) +
  scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, el, 5)) +
  scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = fs, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_mean' = 'gray70', 'night_mean' = 'gray20')) +
  theme(axis.text.x = element_text(size = fs), axis.text.y = element_text(size = fs))  #+
#geom_text(aes(x = daynum, y = sig), label = rep(c(' ', '*', '*', '*', '**', ' ', ' ', '***', '*', '*', '*', '**', ' ', '*', ' ', '*', '*', '*', '**', '**', ' ', ' ', ' ', ' ', ' ', '*', '*', '*', '*', '*'), 2), size = 5, hjust = 'centre')

if(leg == T){
  dplot <- dplot + theme(legend.position = c(0.9, 0.8))
} else {
  dplot <- dplot + theme(legend.position = 'none')
}

dplot <<- dplot

}

mdepplot(wdepth, F, 47)
wdp <- dplot

mdepplot(fdepth, T, 47)
fdp <- dplot

mdepplot(acc2adepth, F, 30)
acc2adp <- dplot

mdepplot(nacc2adepth, F, 30)
nacc2adp <- dplot

mdepplot(acc2bdepth, F, 30)
acc2bdp <- dplot

mdepplot(nacc2bdepth, F, 30)
nacc2bdp <- dplot


# draw depth plots for all studies
plot_grid(wdp, fdp, acc2adp, nacc2adp, acc2bdp, nacc2bdp, labels = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'), rel_widths = c(1,1), hjust = c(rep(-3, 5), -4), vjust = c(3, 3, 3, 3, 3, 3), nrow = 3, ncol = 2, label_size = fs)



# 14. Bar plots of individual fish night and day depth for all three trials

detach('package:xlsx')
library(openxlsx)

# Read in wild vs. farmed activity data

setwd('H:/Acoustic tag - Wild vs. Farmed/Data processing/Cropped data/Coded Fish CSV/Outputs')
wfdep <- read.csv('DepthTotOutput.csv')
rownames(wfdep) <- wfdep$ID
wfdep$ID <- NULL
wfdep <- t(wfdep)
wdep <- as.data.frame(wfdep[,1:8])
fdep <- as.data.frame(wfdep[,9:16])
wdep <- setDT(wdep, keep.rownames = T)[]
colnames(wdep)[1] <- 'fishID'
fdep <- setDT(fdep, keep.rownames = T)[]
colnames(fdep)[1] <- 'fishID'
wdep <- filter(wdep, P7_dawn_mean != '#N/A')
fdep <- filter(fdep, P8_dawn_mean != '#N/A')
wdep$fishID <- substring(wdep$fishID, 2)
fdep$fishID <- substring(fdep$fishID, 2)
wdep[,2:9] <- lapply(wdep[,2:9], function(x) {as.numeric(as.character(x))}) # convert factors to numeric
fdep[,2:9] <- lapply(fdep[,2:9], function(x) {as.numeric(as.character(x))})

wdep <- arrange(wdep, desc(P7_day_mean))
wdep$fishID <- as.factor(wdep$fishID)
flevels <- wdep$fishID
wdep$fishID <- factor(wdep$fishID, levels = flevels)

fdep <- arrange(fdep, desc(P8_day_mean))
fdep$fishID <- as.factor(fdep$fishID)
flevels <- fdep$fishID
fdep$fishID <- factor(fdep$fishID, levels = flevels)

mdep <- melt(wdep[,c('fishID', 'P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean')], measure.vars = c('P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(wdep[,c('fishID', 'P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se')], measure.vars = c('P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
wdep <- cbind(mdep, depse)
wdep$time <- substring(wdep$time, 4)
rm(mdep, depse)

mdep <- melt(fdep[,c('fishID', 'P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean')], measure.vars = c('P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(fdep[,c('fishID', 'P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se')], measure.vars = c('P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
fdep <- cbind(mdep, depse)
fdep$time <- substring(fdep$time, 4)
rm(mdep, depse)


# Read in acclimated A activity data

setwd('H:/Acoustic tag - Preconditioning A/Data processing/Filtered Data/Recoded Day CSV/Outputs')
acc2adep <- read.csv('DepthTotOutput-byfish.csv')
rownames(acc2adep) <- acc2adep$ID
acc2adep$ID <- NULL
acc2adep <- t(acc2adep)
hacc2adep <- as.data.frame(acc2adep[,1:8])
nacc2adep <- as.data.frame(acc2adep[,9:16])
hacc2adep <- setDT(hacc2adep, keep.rownames = T)[]
colnames(hacc2adep)[1] <- 'fishID'
nacc2adep <- setDT(nacc2adep, keep.rownames = T)[]
colnames(nacc2adep)[1] <- 'fishID'
hacc2adep <- filter(hacc2adep, P7_dawn_mean != '#N/A')
nacc2adep <- filter(nacc2adep, P8_dawn_mean != '#N/A')
hacc2adep$fishID <- substring(hacc2adep$fishID, 2)
nacc2adep$fishID <- substring(nacc2adep$fishID, 2)
hacc2adep[,2:9] <- lapply(hacc2adep[,2:9], function(x) {as.numeric(as.character(x))}) # convert factors to numeric
nacc2adep[,2:9] <- lapply(nacc2adep[,2:9], function(x) {as.numeric(as.character(x))})

hacc2adep <- arrange(hacc2adep, desc(P7_day_mean))
hacc2adep$fishID <- as.factor(hacc2adep$fishID)
flevels <- hacc2adep$fishID
hacc2adep$fishID <- factor(hacc2adep$fishID, levels = flevels)

nacc2adep <- arrange(nacc2adep, desc(P8_day_mean))
nacc2adep$fishID <- as.factor(nacc2adep$fishID)
flevels <- nacc2adep$fishID
nacc2adep$fishID <- factor(nacc2adep$fishID, levels = flevels)

mdep <- melt(hacc2adep[,c('fishID', 'P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean')], measure.vars = c('P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(hacc2adep[,c('fishID', 'P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se')], measure.vars = c('P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
hacc2adep <- cbind(mdep, depse)
hacc2adep$time <- substring(hacc2adep$time, 4)
rm(mdep, depse)

mdep <- melt(nacc2adep[,c('fishID', 'P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean')], measure.vars = c('P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(nacc2adep[,c('fishID', 'P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se')], measure.vars = c('P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
nacc2adep <- cbind(mdep, depse)
nacc2adep$time <- substring(nacc2adep$time, 4)
rm(mdep, depse)


# Read in acclimated B activity data

setwd('H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Day CSV/Outputs')
acc2bdep <- read.csv('DepthTotOutput-byfish.csv')
rownames(acc2bdep) <- acc2bdep$ID
acc2bdep$ID <- NULL
acc2bdep <- t(acc2bdep)
hacc2bdep <- as.data.frame(acc2bdep[,1:8])
nacc2bdep <- as.data.frame(acc2bdep[,9:16])
hacc2bdep <- setDT(hacc2bdep, keep.rownames = T)[]
colnames(hacc2bdep)[1] <- 'fishID'
nacc2bdep <- setDT(nacc2bdep, keep.rownames = T)[]
colnames(nacc2bdep)[1] <- 'fishID'
hacc2bdep <- filter(hacc2bdep, P7_dawn_mean != '#N/A')
nacc2bdep <- filter(nacc2bdep, P8_dawn_mean != '#N/A')
hacc2bdep$fishID <- substring(hacc2bdep$fishID, 2)
nacc2bdep$fishID <- substring(nacc2bdep$fishID, 2)
hacc2bdep[,2:9] <- lapply(hacc2bdep[,2:9], function(x) {as.numeric(as.character(x))}) # convert factors to numeric
nacc2bdep[,2:9] <- lapply(nacc2bdep[,2:9], function(x) {as.numeric(as.character(x))})

hacc2bdep <- arrange(hacc2bdep, desc(P7_day_mean))
hacc2bdep$fishID <- as.factor(hacc2bdep$fishID)
flevels <- hacc2bdep$fishID
hacc2bdep$fishID <- factor(hacc2bdep$fishID, levels = flevels)

nacc2bdep <- arrange(nacc2bdep, desc(P8_day_mean))
nacc2bdep$fishID <- as.factor(nacc2bdep$fishID)
flevels <- nacc2bdep$fishID
nacc2bdep$fishID <- factor(nacc2bdep$fishID, levels = flevels)

mdep <- melt(hacc2bdep[,c('fishID', 'P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean')], measure.vars = c('P7_dawn_mean', 'P7_day_mean', 'P7_dusk_mean', 'P7_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(hacc2bdep[,c('fishID', 'P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se')], measure.vars = c('P7_dawn_se', 'P7_day_se', 'P7_dusk_se', 'P7_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
hacc2bdep <- cbind(mdep, depse)
hacc2bdep$time <- substring(hacc2bdep$time, 4)
rm(mdep, depse)

mdep <- melt(nacc2bdep[,c('fishID', 'P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean')], measure.vars = c('P8_dawn_mean', 'P8_day_mean', 'P8_dusk_mean', 'P8_night_mean'), variable.name = 'time', value.name = 'mean')
depse <- melt(nacc2bdep[,c('fishID', 'P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se')], measure.vars = c('P8_dawn_se', 'P8_day_se', 'P8_dusk_se', 'P8_night_se'), variable.name = 'time', value.name = 'se')
depse <- depse[,3]
nacc2bdep <- cbind(mdep, depse)
nacc2bdep$time <- substring(nacc2bdep$time, 4)
rm(mdep, depse)

# Create dataframes of significance levels

#hpacsig <- data.frame(fishID = unique(hpacact$fishID), act = c(0.48, 0.5, 0.6, 0.61, 0.75, 0.82, 0.9, 0.99, 1.06, 1.05))
#wsig <- data.frame(fishID = unique(wact$fishID), act = c(0.31, 0.42, 0.43, 0.43, 0.44, 0.45, 0.45, 0.45, 0.47, 0.49, 0.5, 0.58, 0.59, 0.59))
#hacsig <- data.frame(fishID = unique(hacact$fishID), act = c(0.6, 0.6, 0.61, 0.63, 0.64, 0.64, 0.64, 0.7, 0.7, 0.72, 0.85, 0.85, 1.01))

# Draw barplots of activity


plotfont <- 'Arial'
fs <- 12

fishdepplot <- function(df, leg){
  
  fdplot <- ggplot(df, aes(x = fishID, y = mean)) +
    theme_classic() + 
    theme(text = element_text(family = plotfont, size = fs), plot.margin = margin(10, 5, 10, 1, 'pt')) + 
    geom_bar(aes(fill = time), stat = 'identity', position = 'dodge') + 
    geom_errorbar(aes(ymin = mean-depse, ymax = mean+depse, fill = time), width = 0.4, position = position_dodge(1)) +
    scale_fill_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = fs, angle = 0, family = plotfont)), labels = c('Dawn', 'Day', 'Dusk', 'Night'), values = c('dawn_mean' = 'gray40', 'day_mean' = 'gray80', 'dusk_mean' = 'gray60', 'night_mean' = 'gray20')) +
    scale_y_reverse(name = 'Depth (m)', expand = c(0,0), limits = c(25, 0), breaks = seq(0, 25, 5)) + 
    scale_x_discrete(name = 'fish ID', expand = c(0,0), position = 'top') +
    theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = fs)) # +
  #geom_text(data = wsig, label = c(' ', '*', '*', '**', '*', '*', '*', '*', '*', '*', '*', '**', '*', '**'), size = 5, hjust = 'centre')
  
  if(leg == T){
    fdplot <- fdplot + theme(legend.position = c(0.85, 0.2))
  } else {
    fdplot <- fdplot + theme(legend.position = 'none')
  }
  
  fdplot <<- fdplot
}

fishdepplot(wdep, T)
wfdp <- fdplot

fishdepplot(fdep, F)
ffdp <- fdplot

fishdepplot(hacc2adep, F)
h2afdp <- fdplot

fishdepplot(nacc2adep, F)
n2afdp <- fdplot

fishdepplot(hacc2bdep, F)
h2bfdp <- fdplot

fishdepplot(nacc2bdep, F)
n2bfdp <- fdplot


#actleg <- get_legend(wpact)
#wpact <- wpact + theme(legend.position = 'none')

plot_grid(wfdp, ffdp, h2afdp, n2afdp, h2bfdp, n2bfdp, labels = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'),  rel_widths = c(1,1), hjust = c(rep(-4, 5), -5), vjust = c(rep(27, 6)), nrow = 3, ncol = 2, label_size = fs)

#plot_grid(plot_grid(wpact, actleg, labels = c('(a)', ' '), hjust = c(-4, -4), vjust = c(3, 3), rel_widths = c(1.2, 0.8), scale = c(1, 0)), plot_grid(hacpact, hpacpact, labels = c('(b)', '(c)'), hjust = c(-4, -4), vjust = c(3, 3), rel_widths = c(1.13, 0.87)), nrow = 2, ncol = 1)



# 15. Line plots of night and day activity for all three trials

detach('package:xlsx')
library(openxlsx)
library(data.table)
library(ggplot2)
library(extrafont)
library(extrafontdb)
library(cowplot)

# Read and modify trial 1 activity data

setwd('H:/Acoustic tag - Wild vs. Farmed/Data processing/Cropped data/Coded Day CSV/Outputs')
#mdepth <- read.xlsx('DepthTotOutput.xlsx', sheet = 1, rows = seq(1, 17, 1), cols = seq(2, 32, 1), colNames = T, rowNames = T)
mact <- read.csv('ActivityTotOutput.csv')
rownames(mact) <- mact$X
mact[,c(1, 2, 3)] <- NULL
colnames(mact) <- c(seq(1, 14, 1), seq(19, 26, 1), seq(29, 43, 1))
mact <- as.data.frame(t(mact))
mact <- setDT(mact, keep.rownames = T)[]
colnames(mact)[1] <- 'Day'
mact$Day <- as.numeric(mact$Day)

wact <- mact[,c(1, 4, 5, 8, 9)]
colnames(wact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(wact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(wact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
wact <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(wact) <- c('daynum', 'tod', 'mean', 'se')
wact <- filter(wact, !is.na(mean))
#wact$daynum <- as.factor(wact$daynum)

fact <- mact[,c(1, 12, 13, 16, 17)]
colnames(fact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(fact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(fact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
fact <- cbind(mdep, sedep)
colnames(fact) <- c('daynum', 'tod', 'mean', 'se')
fact <- filter(fact, !is.na(mean))

rm(mdep, sedep)


# Read and modify trial 2a depth data

setwd('H:/Acoustic tag - Preconditioning A/Data processing/Filtered Data/Recoded Day CSV/Outputs')
mact <- read.csv('ActivityTotOutput-days.csv')
rownames(mact) <- mact$X
mact[,c(1, 2, 3, 4)] <- NULL
colnames(mact) <- seq(1, 30, 1)
mact <- as.data.frame(t(mact))
mact <- setDT(mact, keep.rownames = T)[]
colnames(mact)[1] <- 'Day'
mact$Day <- as.numeric(mact$Day)

acc2aact <- mact[,c(1, 4, 5, 8, 9)]
colnames(acc2aact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(acc2aact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(acc2aact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
acc2aact <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(acc2aact) <- c('daynum', 'tod', 'mean', 'se')

nacc2aact <- mact[,c(1, 12, 13, 16, 17)]
colnames(nacc2aact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(nacc2aact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(nacc2aact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
nacc2aact <- cbind(mdep, sedep)
colnames(nacc2aact) <- c('daynum', 'tod', 'mean', 'se')

rm(mdep, sedep)


# Read and modify trial 2b depth data

setwd('H:/Acoustic tag - Preconditioning B/Data processing/Filtered Data/Recoded Day CSV/Outputs')
mact <- read.csv('ActivityTotOutput-days.csv')
rownames(mact) <- mact$X
mact[,c(1, 2, 3)] <- NULL
colnames(mact) <- seq(1, 30, 1)
mact <- as.data.frame(t(mact))
mact <- setDT(mact, keep.rownames = T)[]
colnames(mact)[1] <- 'Day'
mact$Day <- as.numeric(mact$Day)

acc2bact <- mact[,c(1, 4, 5, 8, 9)]
colnames(acc2bact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(acc2bact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(acc2bact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
acc2bact <- cbind(mdep, sedep)
rm(mdep, sedep)
colnames(acc2bact) <- c('daynum', 'tod', 'mean', 'se')
acc2bact <- filter(acc2bact, !is.na(mean))

nacc2bact <- mact[,c(1, 12, 13, 16, 17)]
colnames(nacc2bact) <- c('Day', 'day_mean', 'day_se', 'night_mean', 'night_se')
mdep <- melt(nacc2bact[,c('Day', 'day_mean', 'night_mean')], measure.vars = c('day_mean', 'night_mean'), variable.name = 'time', value.name = 'mean')
sedep <- melt(nacc2bact[,c('Day', 'day_se', 'night_se')], measure.vars = c('day_se', 'night_se'), variable.name = 'time', value.name = 'se')
sedep <- sedep[,3]
nacc2bact <- cbind(mdep, sedep)
colnames(nacc2bact) <- c('daynum', 'tod', 'mean', 'se')
nacc2bact <- filter(nacc2bact, !is.na(mean))

rm(mdep, sedep)

# vectors to position effect size asterisks

#accdpsig <- data.frame(daynum = unique(accdepth$daynum), mdepth = c(15, 12, 11, 8, 7, 6, 7, 7, 7, 6, 5, 7, 7, 8, 8, 7, 7, 6, 6, 7, 8, 9, 10, 10, 9, 10, 10, 10, 7, 7))
#accdepth$sig <- rep(c(15, 12.4, 10.5, 8.2, 7, 6, 8, 7.6, 7.4, 6.6, 4.6, 7.3, 7, 8.5, 8, 7.6, 7.9, 6.5, 6.1, 7.4, 8, 9, 10, 10, 9, 7.7, 9.9, 9.6, 7.6, 8), 2)
#naccdepth$sig <- rep(c(1, 14.9, 15.4, 1, 1, 14.9, 11.5, 1, 12.3, 12.3, 10.5, 1, 1, 11.1, 1, 1, 1, 10.8, 10.6, 1, 1, 1, 1, 10.9, 1, 1, 11.3, 1, 1, 1), 2)
#accact$sig <- rep(c(0.1, 0.1, 0.82, 0.86, 0.85, 1.13, 0.8, 0.74, 0.76, 0.81, 0.75, 0.8, 0.1, 0.8, 0.98, 0.1, 0.79, 0.82, 0.7, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.64, 0.1, 0.74, 0.1), 2)
#naccact$sig <- rep(c(0.1, 0.67, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.73, 0.8, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.72, 0.1, 0.1, 0.1, 0.1, 0.1), 2)

# draw depth plots for acclimated and non acclimated groups

plotfont <- 'Arial'
fs <- 12

mactplot <- function(df, leg, el){
  
  aplot <- ggplot(df, aes(x = daynum, y = mean, group = tod)) +
    theme_classic() +
    theme(text = element_text(family = plotfont, size = fs), plot.margin = margin(10, 5, 10, 1, 'pt')) +
    scale_y_continuous(name = 'Mean activity (BL/s)', limits = c(0, 1), breaks = seq(0, 1, 0.1), expand = c(0,0)) +
    geom_line(aes(colour = tod), stat = 'identity', size = 1) + 
    geom_point(aes(colour = tod), size = 2) +
    geom_errorbar(aes(ymin = mean-se, ymax = mean+se, group = tod, colour = tod), width = 0.2, size = 1) +
    scale_x_continuous(name = 'Exp. day', expand = c(0.01,0), breaks = seq(0, el, 5)) +
    scale_colour_manual(guide = guide_legend(title = NULL,  label.theme = element_text(size = fs, angle = 0, family = plotfont)), labels = c('Day', 'Night'), values = c('day_mean' = 'gray70', 'night_mean' = 'gray20')) +
    theme(axis.text.x = element_text(size = fs), axis.text.y = element_text(size = fs))  #+
  #geom_text(aes(x = daynum, y = sig), label = rep(c(' ', '*', '*', '*', '**', ' ', ' ', '***', '*', '*', '*', '**', ' ', '*', ' ', '*', '*', '*', '**', '**', ' ', ' ', ' ', ' ', ' ', '*', '*', '*', '*', '*'), 2), size = 5, hjust = 'centre')
  
  if(leg == T){
    aplot <- aplot + theme(legend.position = c(0.9, 0.8))
  } else {
    aplot <- aplot + theme(legend.position = 'none')
  }
  
  aplot <<- aplot
  
}

mactplot(wact, F, 47)
wap <- aplot

mactplot(fact, T, 47)
fap <- aplot

mactplot(acc2aact, F, 30)
acc2aap <- aplot

mactplot(nacc2aact, F, 30)
nacc2aap <- aplot

mactplot(acc2bact, F, 30)
acc2bap <- aplot

mactplot(nacc2bact, F, 30)
nacc2bap <- aplot


# draw depth plots for all studies
plot_grid(wap, fap, acc2aap, nacc2aap, acc2bap, nacc2bap, labels = c('(a)', '(b)', '(c)', '(d)', '(e)', '(f)'), rel_widths = c(1,1), hjust = c(rep(-3, 5), -4), vjust = c(3, 3, 3, 3, 3, 3), nrow = 3, ncol = 2, label_size = fs)

