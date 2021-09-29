## Set up modelling environment

# Clear memory
rm(list=ls())

# do not use scientific notation
options(scipen=999) 

# for calculating the process time
ptm <- proc.time()

# Import libraries: Although some of them are not required

library(stats); library(minpack.lm); library(MuMIn);library(AICcmodavg); library(Metrics); library(plyr); library(zoo)
library(pbapply); library(forecast); library(parallel); library(snow); library(pbmcapply); 
library(readxl); library(zoo); library(Hmisc); library(ggplot2); library(deepslm); library(data.table)

## set working, data, output directories

# set working directory
working.dir <- c('/home/shong/Documents/Projects/UTAS/Projects/Working/PGRA_2017/codes')
setwd(working.dir)

# set data directory
data.dir <- paste(working.dir, '/data/', sep='')

# import files
df <- importcsvs(dr = data.dir, type = c('hist', 'proj'))

# country.list <- intersect(as.character(unique(df$hist$country)), as.character(unique(df$proj$country)))
country.list <- c('ZWE', 'LSO', 'BWA', 'GNQ', 'MUS', 'SYC')
hist.years <- seq(1961, 2015, 1)
proj.years <- seq(2020, 2100, 10)

df$hist <- df$hist[order(df$hist$country, decreasing = F), ]
df$proj <- df$proj[order(df$proj$country, decreasing = F), ]

plot.new()
par(mfrow = c(3, 3))
for(ct in country.list){
  
  test <- df$hist[df$hist$country %in% ct, ]
   plot(test$fertility ~ test$year, main = ct)
  
}

plot.new()
par(mfrow = c(3, 3))
for(ct in country.list){
  
  test <- df$hist[df$hist$country %in% ct, ]
  plot(test$fertility ~ test$GDP, main = ct)
  
}
