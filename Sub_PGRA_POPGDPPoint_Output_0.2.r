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
# do not use scientific notation
options(scipen=999) 

output <- c('national', 'global', 'regional')
# ln_lhs <- 1000 # number of latin hypercube
# exc <- F

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

years <- seq(2020, 2050, by = 1)

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

df <- do.call(rbind, pn)

group <- read.csv('additional_data/countries_groups.csv')

df$economic <- NA
df$regional <- NA
for(c in group$country){
  
  df[df$country %in% c, 'economic'] <- as.character(group[group$country %in% c, 'economic'])
  df[df$country %in% c, 'regional'] <- as.character(group[group$country %in% c, 'regional'])
  
}

df$econid <- NA

df[df$economic == 'High', 'econid'] <- 4
df[df$economic == 'Uppermiddle', 'econid'] <- 3
df[df$economic == 'Lowermiddle', 'econid'] <- 2
df[df$economic == 'Low', 'econid'] <- 1

df[is.na(df)] <- 0
df[df == 'Inf'] <- 0

df$economic <- NA
df$regional <- NA
for(c in group$country){
  
  df[df$country %in% c, 'economic'] <- as.character(group[group$country %in% c, 'economic'])
  df[df$country %in% c, 'regional'] <- as.character(group[group$country %in% c, 'regional'])
  
}

econ <- as.character(unique(df$economic))
df$pop2015 <- NA

for(c in cl){
  
  df[df$country %in% c, 'pop2015'] <- df[(df$country %in% c) & (df$year ==2015), 'pop']
  
}

df$popinc <- df$pop - df$pop2015

blank <- grid.rect(gp=gpar(col="white"))
weird <- scales::trans_new("signed_log",
                           transform = function(x) sign(x)*log(abs(x) + 1, base = 10),
                           inverse = function(x) sign(x)*log(abs(x) + 1, base = 10))
x <- c(-1000, -100, -10, -1, 0, 1, 10, 100, 1000)
xk <- sign(x)*log(abs(x) + 1, base = 10)
df$y <- sign(df$popinc/10^6)*log(abs(df$popinc/10^6) + 1, base = 10)
reg <- as.character(unique(df$regional))

new.df <- df[df$year == max(years) & df$id == 11, ]
ggplot(data = new.df)+
  geom_point(aes(x = popr, y = y, size = pop2015/10^6, 
                 colour = factor(econid), fill = factor(econid)), alpha = 0.8,  pch = 21, stroke = 1, show.legend = T) + 
  scale_size_continuous(range = c(1, 20)) +
  scale_x_continuous(limits = c(-0.01, 0.03)) +
  scale_y_continuous(limits = c(min(xk), max(xk)), breaks = xk, labels = x)+
  theme_bw() +
  geom_vline(aes(xintercept=0), size=0.7, linetype = 'twodash', show.legend = F)+
  geom_hline(aes(yintercept=0), size=0.7, linetype = 'twodash', show.legend = F)+
  scale_fill_brewer(palette = 'Dark2', labels = c('Low', 'Lower middle',  'Upper middle', 'High'))+
  scale_colour_brewer(palette = 'Dark2', labels = c('Low', 'Lower middle',  'Upper middle', 'High'))+
  xlab('Annual population growth rate by 2050') +
  ylab('Population increase by 2050 (million)') +
  labs(colour = 'Income group', fill = 'Income group', size = 'Population (million)') +
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1), fill = guide_legend(override.aes = list(size = 5), order = 1), size = guide_legend(order = 2))+
  theme(panel.grid.major = element_blank(), legend.position = 'bottom', panel.grid.minor = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

# ggsave(q1, filename='/home/shong/Documents/Projects/UTAS/1_Projects/SML/PGRA_2017/codes/output/popr_popinc_economic.tiff', width = 10, height=10, dpi=300, device='tiff')

ggplot(data = new.df)+
  geom_point(aes(x = popr, y = y, size = pop2015/10^6, 
                 colour = regional, fill = regional), alpha = 0.8,  pch = 21, stroke = 1, show.legend = T) + 
  scale_size_continuous(range = c(1, 20)) +
  scale_x_continuous(limits = c(-0.01, 0.05)) +
  scale_y_continuous(limits = c(min(xk), max(xk)), breaks = xk, labels = x)+
  theme_bw() +
  geom_vline(aes(xintercept=0), size=0.7, linetype = 'twodash', show.legend = F)+
  geom_hline(aes(yintercept=0), size=0.7, linetype = 'twodash', show.legend = F)+
  scale_color_brewer(palette='Dark2') +
  scale_fill_brewer(palette='Dark2') +
  # scale_color_manual(values=c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A"), labels = c('High', 'Upper middle', 'Lower middle', 'Low'))+
  # scale_fill_manual(values=c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A"), labels = c('High', 'Upper middle', 'Lower middle', 'Low'))+                     
  xlab('Annual population growth rate by 2050') +
  ylab('Population increase by max(years) (million)') +
  labs(colour = 'Regions', fill = 'Regions', size = 'Population (million)') +
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1), 
         fill = guide_legend(override.aes = list(size = 5), order = 1), size = guide_legend(order = 2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=12), axis.title=element_text(size=12), aspect.ratio=1, 
        legend.text = element_text(size=10), legend.title = element_text(size=12), legend.position = 'bottom')  

# box plot
# by ecoonmic group
econ.df <- plyr::ddply(df, .(year, id, economic), summarize, gdpt = sum(gdpt), pop = sum(pop))
econ.df$gdpc <- econ.df$gdpt / econ.df$pop
econ.df$econid <- NA
econ.df[econ.df$economic %in% 'High', ]$econid <- 4
econ.df[econ.df$economic %in% 'Uppermiddle', ]$econid <- 3
econ.df[econ.df$economic %in% 'Lowermiddle', ]$econid <- 2
econ.df[econ.df$economic %in% 'Low', ]$econid <- 1
  
ggplot(data = econ.df[econ.df$year %in% c(2050, seq(2020, max(years), by = 10)), ], 
       aes(x = year, y = pop/10^6, fill = factor(econid))) + 
  stat_boxplot(aes(color = factor(econid)), geom = 'errorbar', width = 1, shape = 1, position = position_dodge(width = 0), show.legend = F)+
  geom_boxplot(width = 3, size = 0.3, shape = 2, outlier.size = 0.1, position = position_dodge(width = 0), outlier.colour = NA, show.legend = F) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000), position = 'left') +
  scale_fill_brewer(palette = 'Dark2', labels = c('Low', 'Lower middle',  'Upper middle', 'High'))+
  scale_colour_brewer(palette = 'Dark2', labels = c('Low', 'Lower middle',  'Upper middle', 'High'))+
  labs(fill = 'Income groups', colour = 'Income groups') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=14))

reg.df <- plyr::ddply(df, .(year, id, regional), summarize, gdpt = sum(gdpt), pop = sum(pop))
reg.df$gdpc <- reg.df$gdpt / reg.df$pop

ggplot(data = reg.df[reg.df$year %in% c(2050, seq(2020, max(years), by = 10)), ], 
       aes(x = year, y = pop/10^6, fill = regional)) + 
  stat_boxplot(aes(color = regional), geom = 'errorbar', width = 1, shape = 1, position = position_dodge(width = 0), show.legend = F)+
  geom_boxplot(width = 3, size = 0.3, shape = 2, outlier.size = 0.1, show.legend = F,  position = position_dodge(width = 0),outlier.colour = NA) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4000)) +
  scale_fill_brewer(palette = 'Dark2')+
  scale_colour_brewer(palette = 'Dark2')+
  labs(fill = 'Regions', colour = 'Regions') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=14), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=14))

# lapply(unique(reg.df$regional), function(rn){reg.df[reg.df$pop == min(reg.df[reg.df$year == 2050 & reg.df$regional == rn & reg.df$gdpc <= 62375, ]$pop), ]})

## plot

# sel.df <- df[df$id == 11, ]
glob.df <- plyr::ddply(df, .(year, id), summarize, gdpt = sum(gdpt), pop = sum(pop), pop2015 = sum(pop2015))

glob.df$gdpc <- glob.df$gdpt / glob.df$pop
glob.df$popr <- log(glob.df$pop / glob.df$pop2015) / ((as.numeric(as.character(glob.df$year)) - 2015))
glob.df$gdpr <- log(glob.df$gdpc / glob.df[glob.df$year == 2015, ]$gdpc) / ((as.numeric(as.character(glob.df$year)) - 2015))

glob.df[glob.df == 'NaN'] <- 0
glob.text <- glob.df[glob.df$id %in% 31 & glob.df$year %in% c(2015, seq(2020, max(years), by = 10)), ]
ggplot() + 
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 5)), ], 
            aes(x = gdpc, y = pop/10^6, group = as.factor(year)), col = 'black', linetype = 2, size = 0.1, show.legend=F)+
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 1)) , ],
            aes(x = gdpc, y = pop/10^6, group = id, col = as.factor(id)), show.legend=F)+
  geom_smooth(data = glob.df[glob.df$year == max(years), ], aes(x = gdpc, y = pop/10^6), 
              method = 'glm', col = 'black', show.legend = F)+
  geom_text(data = glob.text[as.numeric(as.character(glob.text$year)) <= max(years), ], 
            aes(x = gdpc, y = pop/10^6, label=year), col = 'black', 
            hjust = -0.5, vjust = 1, size = 4, show.legend = F)+
  labs(col = 'Year') +
  scale_x_continuous(expand = c(0, 20000), limits = c(0, 300000))+
  xlab("GDP (per-capita in 2005 USD)") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

# sel.df <- df[df$region == 'Sub-Saharan Africa', ]
sel.df <- df[df$economic == 'Low', ]
# sel.df <- df
glob.df <- plyr::ddply(sel.df, .(year, id), summarize, gdpt = sum(gdpt), pop = sum(pop), pop2015 = sum(pop2015))

glob.df$gdpc <- glob.df$gdpt / glob.df$pop
glob.df$popr <- log(glob.df$pop / glob.df$pop2015) / ((as.numeric(as.character(glob.df$year)) - 2015))
glob.df$gdpr <- log(glob.df$gdpc / glob.df[glob.df$year == 2015, ]$gdpc) / ((as.numeric(as.character(glob.df$year)) - 2015))

glob.df[glob.df == 'NaN'] <- 0
glob.text <- glob.df[glob.df$id %in% 31 & glob.df$year %in% c(2015, seq(2020, max(years), by = 10)), ]
ggplot() + 
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 5)), ], 
            aes(x = gdpc, y = pop/10^6, group = as.factor(year)), col = 'black', linetype = 2, size = 0.1, show.legend=F)+
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 1)) , ],
            aes(x = gdpc, y = pop/10^6, group = id, col = as.factor(id)), show.legend=F)+
  geom_smooth(data = glob.df[glob.df$year == max(years), ], aes(x = gdpc, y = pop/10^6), 
              method = 'glm', col = 'black', show.legend = F)+
  geom_text(data = glob.text[as.numeric(as.character(glob.text$year)) <= max(years), ], 
            aes(x = gdpc, y = pop/10^6, label=year), col = 'black', 
            hjust = -0.5, vjust = 1, size = 4, show.legend = F)+
  labs(col = 'Year') +
  scale_x_continuous(expand = c(0, 20000), limits = c(0, 300000))+
  xlab("GDP (per-capita in 2005 USD)") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

require(viridis)
ggplot() +
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 5)) & glob.df$id %in% seq(1, 31, by = 1) , ],
            aes(x = year, y = pop/10^6, group = id, col = as.factor(id)), show.legend=F)+
  labs(col = 'Growth variations') +
  scale_color_viridis(discrete=TRUE, labels = seq(0, 3, by = 0.1))+
  scale_x_discrete(breaks = c(2015, seq(2020, max(years), by = 10)))+
  xlab("year") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=10))

# glob.df <- glob.df[glob.df$id %in% glob.df[glob.df$year == 2050 & glob.df$gdpc < 62375, ]$id, ]

ggplot() +
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 5)) & glob.df$id %in% seq(1, 31, by = 1) , ],
            aes(x = gdpc, y = pop/10^6, group = id, col = as.factor(id)), show.legend=F)+
  labs(col = 'Growth variation') +
  scale_color_viridis(discrete=TRUE, labels = seq(0, 3, by = 0.1))+
  # scale_x_continuous(breaks = seq(0, 250000, by = 10000), expand = c(0, 0))+
  # coord_cartesian(xlim=c(0, 70000))+
  scale_x_continuous(breaks = seq(0, 250000, by =25000))+
  geom_vline(aes(xintercept=62375), size=0.2, linetype = 'twodash', show.legend = F)+
  # scale_y_continuous(position = 'right')+
  xlab("GDP (per-capita in 2005 USD)") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))

ggplot() +
  geom_line(data = glob.df[glob.df$year %in% c(2015, seq(2020, max(years), by = 5)) & glob.df$id %in% seq(1, 31, by = 1) , ],
            aes(x = gdpc, y = pop/10^6, group = id, col = as.factor(id)), show.legend=T)+
  labs(col = 'Growth variation') +
  scale_color_viridis(discrete=TRUE, labels = seq(0, 3, by = 0.1))+
  # scale_x_discrete(breaks = seq(0, 250000, by =50000))+
  # scale_y_continuous(position = 'right')+
  xlab("GDP (per-capita in 2005 USD)") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))

reg2.df <- reg.df[reg.df$regional %in% c('East Asia and Pacific', 'South Asia', 'Sub-Saharan Africa'), ]

reg2.df <- reg.df
ggplot(data = reg2.df[reg2.df$year %in% c(2015, seq(2020, max(years), by = 10)), ], 
       aes(x = year, y = pop/10^6, fill = regional)) + 
  stat_boxplot(aes(color = regional), show.legend = F, 
               position = position_dodge(width = 1), alpha = 1, geom = 'errorbar')+
  geom_boxplot(width = 1, size = 0.3, shape = 2, outlier.size = 0.1,  show.legend = T,
               position = position_dodge(width = 1),outlier.colour = NA, alpha = 1) +
  geom_vline(xintercept = 0.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 1.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 2.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 3.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 4.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 5.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 6.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 7.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 8.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 9.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 10.5, linetype = 2, size = 0.1)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  scale_fill_brewer(palette = 'Dark2')+
  scale_colour_brewer(palette = 'Dark2')+
  labs(fill = 'Regions', colour = 'Regions') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=14), axis.title=element_text(size=14), aspect.ratio=0.5, legend.text = element_text(size=10), legend.title = element_text(size=14))

reg3.df <- reg.df[!(reg.df$regional %in% c('East Asia and Pacific', 'South Asia', 'Sub-Saharan Africa')), ]

ggplot(data = reg3.df[reg3.df$year %in% seq(2015, max(years), by = 10), ], 
       aes(x = year, y = pop/10^6, fill = regional)) + 
  stat_boxplot(aes(color = regional), show.legend = F, geom = 'errorbar', width = 1, shape = 1, 
               position = position_dodge(width = 0.5), alpha = 1)+
  geom_boxplot(width = 0.8, size = 0.3, shape = 2, outlier.size = 0.1,  show.legend = F, 
               position = position_dodge(width = 0.5),outlier.colour = NA, alpha = 1) +
  scale_y_continuous(expand = c(0, 0), limits = c(300, 1000), position = 'right') +
  # scale_fill_brewer(palette = 'Dark2')+
  # scale_colour_brewer(palette = 'Dark2')+
  labs(fill = 'Regions', colour = 'Regions') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=14), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=14))


difference <- lapply(as.character(unique(reg.df$regional)), function(ct){
  
  temp.df <- reg.df[reg.df$regional %in% ct & reg.df$year %in% 2050, ]
  base.df <- reg.df[reg.df$regional %in% ct & reg.df$year %in% 2015 & reg.df$id %in% 11, ]
  
  df2050 <- reg.df[reg.df$regional %in% ct & reg.df$year %in% 2050 & reg.df$id == 11, ]
  df2015 <- reg.df[reg.df$regional %in% ct & reg.df$year %in% 2015 & reg.df$id == 11, ]
  
  data.frame(regional = ct, pop = df2050$pop - df2015$pop, pop.r = (df2050$pop - df2015$pop) / df2015$pop, 
             uncertainty.rate = (max(temp.df$pop) - min(temp.df$pop))/base.df$pop,
             uncertainty.abs = (max(temp.df$pop) - min(temp.df$pop)))
  
})

difference <- do.call(rbind, difference)
write.csv(difference, file = 'reg.uncertainty.csv')

uncertainty <- lapply(as.character(unique(df$country)), function(ct){
  
  temp.df <- df[df$country %in% ct & df$year %in% 2050, ]
  base.df <- df[df$country %in% ct & df$year %in% 2015 & df$id %in% 11, ]
  data.frame(country = ct, uncertainty = (max(temp.df$pop) - min(temp.df$pop))/base.df$pop, growth = temp.df[temp.df$id == 11, ]$pop - base.df$pop)
  
})
uncertainty <- do.call(rbind, uncertainty)
write.csv(uncertainty, file = 'uncertainty.csv')

base.df <-  df[df$regional %in% 'Sub-Saharan Africa' & df$year == 2015, ]
sub.df <- df[df$regional %in% 'Sub-Saharan Africa' & df$year == 2050, ]
sub.df <- data.frame(country = sub.df$country, pop = sub.df$pop, gdp = sub.df$gdpc, id = sub.df$id)
View(sub.df)

out2 <- lapply(as.character(unique(sub.df$country)), function(ct){
  
  temp.df <- sub.df[sub.df$country %in% ct, ]
  gap <- temp.df[temp.df$id == 11, ]$pop - base.df[base.df$id == 11 & base.df$country %in% ct, ]$pop
  base <- temp.df[temp.df$id == 11, ]
  max.df <- temp.df[temp.df$pop == max(temp.df$pop), ]
  min.df <- temp.df[temp.df$pop == min(temp.df$pop), ]
  first.df <- temp.df[temp.df$id == 1, ]
  last.df <- temp.df[temp.df$id == 31, ]
  
  data.frame(country = ct, 'Base' = as.numeric(base$pop), 'Max' = max.df$pop, 'Min' = min.df$pop,
    'Max-Base' = max.df$pop - base$pop,
    'Max-Min' = base$pop - min.df$pop,
    'Growth' = gap, 'Max Var' = (max.df$id-1)/10, 'Min Var' = (min.df$id-1)/10)

})
out2 <- do.call(rbind, out2)
write.csv(out2, file = 'subafrica.csv')

base.df <-  df[df$year == 2015, ]
sub.df <- df[df$year == 2050, ]
sub.df <- data.frame(country = sub.df$country, pop = sub.df$pop, gdp = sub.df$gdpc, id = sub.df$id)
View(sub.df)

out2 <- lapply(as.character(unique(sub.df$country)), function(ct){
  
  temp.df <- sub.df[sub.df$country %in% ct, ]
  gap <- temp.df[temp.df$id == 11, ]$pop - base.df[base.df$id == 11 & base.df$country %in% ct, ]$pop
  base <- temp.df[temp.df$id == 11, ]
  max.df <- temp.df[temp.df$pop == max(temp.df$pop), ]
  min.df <- temp.df[temp.df$pop == min(temp.df$pop), ]
  first.df <- temp.df[temp.df$id == 1, ]
  last.df <- temp.df[temp.df$id == 31, ]
  
  data.frame(country = ct, 'Base' = as.numeric(base$pop), 'Max' = max.df$pop, 'Min' = min.df$pop,
             'Max-Base' = max.df$pop - base$pop,
             'Max-Min' = base$pop - min.df$pop,
             'Growth' = gap, 'Max Var' = (max.df$id-1)/10, 'Min Var' = (min.df$id-1)/10)
  
})
out2 <- do.call(rbind, out2)
write.csv(out2, file = 'globalbynation.csv')


ggplot(data = econ.df[econ.df$year %in% c(2015, seq(2020, max(years), by = 10)), ], 
       aes(x = year, y = pop/10^6, fill = economic)) + 
  stat_boxplot(aes(color = economic), show.legend = F, 
               position = position_dodge(width = 1), alpha = 1, geom = 'errorbar')+
  geom_boxplot(width = 1, size = 0.3, shape = 2, outlier.size = 0.1,  show.legend = T,
               position = position_dodge(width = 1),outlier.colour = NA, alpha = 1) +
  geom_vline(xintercept = 0.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 1.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 2.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 3.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 4.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 5.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 6.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 7.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 8.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 9.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 10.5, linetype = 2, size = 0.1)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5000)) +
  scale_fill_brewer(palette = 'Dark2')+
  scale_colour_brewer(palette = 'Dark2')+
  labs(fill = 'Regions', colour = 'Regions') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=10), axis.title=element_text(size=10), aspect.ratio=0.5, legend.text = element_text(size=10), legend.title = element_text(size=10))


ggplot(data = reg.df[reg.df$year %in% c(2015, seq(2020, max(years), by = 10)), ], 
       aes(x = year, y = pop/10^6, fill = regional)) + 
  stat_boxplot(aes(color = regional), show.legend = F, 
               position = position_dodge(width = 1), alpha = 1, geom = 'errorbar')+
  geom_boxplot(width = 1, size = 0.3, shape = 2, outlier.size = 0.1,  show.legend = T,
               position = position_dodge(width = 1),outlier.colour = NA, alpha = 1) +
  geom_vline(xintercept = 0.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 1.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 2.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 3.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 4.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 5.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 6.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 7.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 8.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 9.5, linetype = 2, size = 0.1)+
  geom_vline(xintercept = 10.5, linetype = 2, size = 0.1)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000)) +
  scale_fill_brewer(palette = 'Dark2')+
  scale_colour_brewer(palette = 'Dark2')+
  labs(fill = 'Regions', colour = 'Regions') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() + 
  theme(plot.margin = unit(c(1,0.5,0.5,0.5),"lines"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.text=element_text(size=10), axis.title=element_text(size=10), aspect.ratio=0.5, legend.text = element_text(size=10), legend.title = element_text(size=10))

out <- lapply(1: max(unique(glob.df$id)), function(i){
  
  temp.df <- reg.df[reg.df$id == i & reg.df$regional == 'South Asia', ]
  data.frame(id = i, peakyear = as.numeric(as.character(temp.df[temp.df$pop == max(temp.df$pop), ]$year)), 
    peakpop = as.numeric(as.character(temp.df[temp.df$pop == max(temp.df$pop), ]$pop)),
    peakgdp = as.numeric(as.character(temp.df[temp.df$pop == max(temp.df$pop), ]$gdpc)),
    pop2100 = as.numeric(as.character(temp.df[temp.df$year == 2100, ]$pop)),
    gdp2100 = as.numeric(as.character(temp.df[temp.df$year == 2100, ]$gdpc))
  )
  
})
out <- do.call(rbind, out)
View(out)
