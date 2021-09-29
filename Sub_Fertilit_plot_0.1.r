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

sel.year <- 2050

load(file.choose())
st1 <- stoch.output
load(file.choose())
st2 <- stoch.output
load(file.choose())
st3 <- stoch.output


out <- lapply(c(1:3), function(j){

  if(j == 1)stoch.output <- st1
  if(j == 2)stoch.output <- st2
  if(j == 3)stoch.output <- st3
  
  out2 <- lapply(stoch.output, function(st){
    
    # st <- stoch.output[[1]]
    
    out <- sapply(st, function(stt){
      
      stt[stt$year %in% 2050, ]$fertility
      
    })
    
    # output <- c(median = median(out), sd = sd(out))
    
    
    
  })
  
  out2 <- do.call(rbind, out2)
  return(out2)
})

out <- melt(out)
names(out) <- c('country', 'variation', 'fertility', 'id')

out2 <- lapply(c(1:3), function(j){
  
  if(j == 1)stoch.output <- st1
  if(j == 2)stoch.output <- st2
  if(j == 3)stoch.output <- st3
  
  
  out2 <- lapply(stoch.output, function(st){
    
    # st <- stoch.output[[1]]
    
    out <- sapply(st, function(stt){
      
      stt[stt$year %in% 2050, ]$fertility
      
    })
    
    c(median = median(out), sd = sd(out))

  })
  
  out2 <- do.call(rbind, out2)
  return(out2)
})

out2 <- melt(out2)
names(out2) <- c('country', 'variation', 'fertility', 'id')

sel.ct <- lapply(as.character(unique(out2$country)), function(ct){

  if(length(unique(out2[out2$country %in% ct & out2$variation %in% 'median', ]$fertility)) != 1) return(ct) else NA

})

sel.ct <- do.call(c, sel.ct)
# sel.ct <- unique(out$country)

med1 <- round(median(out[out$id %in% 1 , ]$fertility), digits = 3) # & out$variation %in% 0
med2 <- round(median(out[out$id %in% 2 , ]$fertility), digits = 3)
med3 <- round(median(out[out$id %in% 3 , ]$fertility), digits = 3)

ggplot(data = out[out$country %in%sel.ct, ]) + 
  geom_boxplot(aes(x = country, y = fertility, fill = factor(id)), outlier.shape = NA) +
  scale_y_continuous(limits=c(0,7), breaks=seq(0, 6, by = 1), expand = c(0, 0)) +
  scale_fill_manual(name = 'GDP threshold variations', values = c(1, 2, 3), 
                    labels = c(paste('low (USD 10,000):', med1), paste('mid (USD 20,000):', med2), paste('high (USD 30,000):', med3)))+
  # labs(fill = 'Fertility variations')+
  xlab("Countries") + ylab('Fertility in 2050') +
  theme_bw() +
  theme(axis.text.x=element_text(size=8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y=element_text(size = 10),
        axis.title=element_text(size=12), 
        legend.position = 'bottom',
        legend.text = element_text(size=10), 
        legend.title = element_text(size=12))



