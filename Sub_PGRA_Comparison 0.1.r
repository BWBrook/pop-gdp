## SRAMW output handler

# Clear memory
rm(list=ls())

library(parallel); library(RJSONIO)
library(ggplot2)
library(gridExtra)
library(plyr)
library(lhs)
library(RColorBrewer)
library(pbapply)
library(dplyr)
library(gridExtra)
library(grid)
library(ggExtra)
library(ggplot2)
library(zoo)

for(i in 1: 2){
  
  load(file.choose())
  
  gdp.hist <- read.csv('data/IMHEGDP_ID.csv', check.names = F)
  gdp.hist <- gdp.hist[, c('GDP.hist', '2015')]
  names(gdp.hist)[1] <- 'country'
  
  pop.hist <- read.csv('data/population_hist.csv', check.names = F)
  pop.hist <- pop.hist[, c('pop.hist', '2015')]
  names(pop.hist)[1] <- 'country'
  
  cl <- intersect(gdp.hist$country, pop.hist$country)
  gdp.hist <- gdp.hist[gdp.hist$country %in% cl, ]
  pop.hist <- pop.hist[pop.hist$country %in% cl, ]
  
  gdp.hist <- gdp.hist[order(gdp.hist$country), ]
  pop.hist <- pop.hist[order(pop.hist$country), ]
  
  cl <- intersect(cl, names(rt2))
  
  years <- seq(2020, 2100, by = 1)
  
  pr <- rt2
  remove(rt2)
  
  pn <- pblapply(cl, function(ci){
    
    # ci <- cl[1]
    
    pt <- lapply(1: length(pr[[ci]]), function(i){
      
      # ci <- cl[i]
      
      pk <- pr[[ci]][[i]]
      pk$male <- pk$male[rownames(pk$male) %in% years, ]
      pk$female <- pk$female[rownames(pk$female) %in% years, ]
      pk$gdp <- pk$gdp[(length(pk$gdp) - 8):length(pk$gdp)]  #pk$gdp[56:length(pk$gdp)]
      names(pk$gdp) <- seq(2020, 2100, by = 10)
      
      gdp.temp <- rep(NA, length(years))
      names(gdp.temp) <- years
      gdp.temp[names(gdp.temp) %in% names(pk$gdp)] <- pk$gdp
      gdp.temp <- na.approx(gdp.temp)
      names(gdp.temp) <- years
      pk$gdp <- gdp.temp
      
      pop <- pk$male + pk$female
      pop <- rowSums(pop)
      
      ph <- pop.hist[pop.hist$country == ci, '2015']
      gh <- gdp.hist[gdp.hist$country == ci, '2015']
      gt <- ph * gh
      
      rt <- data.frame(year = c('2015', years), pop = c(ph, pop), gdpc = c(gh, pk$gdp), country = ci)
      rt$gdpt <- rt$gdp * rt$pop
      
      rt$popr <- log(rt$pop / ph) / (as.numeric(as.character(rt$year)) - 2015)
      rt$gdpcr <- log(rt$gdpc / gh) / (as.numeric(as.character(rt$year)) - 2015)
      rt$gdptr <- log(rt$gdpt / gh) / (as.numeric(as.character(rt$year)) - 2015)
      
      rt$id <- i
      
      return(rt)
      
    })
    
    pt <- do.call(rbind, pt)
    
  })
  
  names(pn) <- cl
  
  if(i == 1) df1 <- do.call(rbind, pn) else df2 <- do.call(rbind, pn)
  
}

df <- df1
df$pop <- df2$pop - df1$pop
df <- data.frame(year = df$year, pop = df$pop, gdpc = df$gdpc, country = df$country, id = df$id, gdpt = df$pop * df$gdpc)

glob.df <- plyr::ddply(df, .(year, id), summarize, gdpt = sum(gdpt), pop = sum(pop))

ggplot() +
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, 2100, by = 5)) , ],
            aes(x = year, y = pop/10^6, group = id, col = as.factor(id)), show.legend=T)+
  labs(col = 'Growth scenarios') +
  scale_x_discrete(breaks = seq(2015, 2100, by =5))+
  xlab("Year") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))

group <- read.csv('additional_data/countries_groups.csv')

df$economic <- NA
df$regional <- NA
for(c in group$country){
  
  df[df$country %in% c, 'economic'] <- as.character(group[group$country %in% c, 'economic'])
  df[df$country %in% c, 'regional'] <- as.character(group[group$country %in% c, 'regional'])
  
}

new.df <- df[df$year %in% 2050, ]
new.df <- new.df[order(new.df$economic), ]

ggplot(data = new.df[new.df$economic == 'Low', ],aes(x = country, y = pop/10^6)) +
  geom_boxplot(outlier.shape = NA, show.legend = F, size = 0.5)+
  scale_y_continuous(limits=c(-10,10), breaks=seq(-10,10,5), expand = c(0, 0)) +
  labs(fill = 'Economic group')+
  ylab("Countries") + xlab('Population (million)') +
  theme_bw() +
  theme(axis.text.x=element_text(size=8, angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y=element_text(size = 10),
        axis.title=element_text(size=12), 
        legend.text = element_text(size=10), 
        legend.title = element_text(size=12)) +
  coord_flip()
