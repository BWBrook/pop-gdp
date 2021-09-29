# Clear memory
rm(list=ls())
load(file.choose())
# rt.df <- melt(rt2)
options(scipen=999) 
st <- seq(0, 3, by = 0.1)
country.list <- names(rt2)
group <- read.csv('additional_data/countries_groups.csv')

national.gdp <- lapply(1: length(st), function(s) {pop <- lapply(country.list, function(ct){
  gdp <- rt2[[ct]][[s]]$gdp
  names(gdp) <- seq(2020, 2100, by = 5)
  gdp2 <- c(1961: 2100)
  names(gdp2) <- gdp2
  gdp2[1: length(gdp2)] <- NA
  gdp2[names(gdp2) %in% names(gdp)] <- gdp
  gdp2[1: length(gdp2)] <- zoo::na.approx(gdp2)
  gdp2 <- gdp2[names(gdp2) %in% c(2016:2100)]
  return(gdp2)
  })
names(pop) <- country.list
return(pop)
})
names(national.gdp) <- st

national.pop <- lapply(1: length(st), function(s) {pop <- lapply(country.list, function(ct)
  rowSums(rt2[[ct]][[s]]$male) + rowSums(rt2[[ct]][[s]]$female))
names(pop) <- country.list
return(pop)
})
names(national.pop) <- st

gdp.df <- melt(national.gdp)
pop.df <- melt(national.pop)

df <- data.frame(gdp.df, pop = pop.df$value, year = c(2016: 2100))
names(df) <- c('gdp', 'country', 'set', 'pop', 'year')
df$gdpt <- df$gdp * df$pop

for(c in group$country){
  
  df[df$country %in% c, 'economic'] <- as.character(group[group$country %in% c, 'economic'])
  df[df$country %in% c, 'regional'] <- as.character(group[group$country %in% c, 'regional'])
  
}

for(c in country.list){
  
  df[df$country %in% c, 'pop2015'] <- median(df[(df$country %in% c) & (df$year == 2016), 'pop'])
  
}

df$econid <- NA

df[df$economic == 'High', 'econid'] <- 1
df[df$economic == 'Uppermiddle', 'econid'] <- 2
df[df$economic == 'Lowermiddle', 'econid'] <- 3
df[df$economic == 'Low', 'econid'] <- 4

library(ggplot2)

# ggplot(data = df[df$country %in% 'AUS' & df$year <= 2050, ], aes(x = year, y = pop, col = set)) + 
#   geom_line()
# 
# ggplot(data = df[df$country %in% 'AUS' & df$year <= 2050, ], aes(x = gdp, y = pop, col = set)) + 
#   geom_line()


glob.df <- plyr::ddply(df, .(set, year), summarise, pop = sum(pop), gdpt = sum(gdpt), gdp = sum(gdpt)/sum(pop))
# glob.df <- glob.df[order(as.numeric(glob.df$set)), ]

econ.df <- lapply(as.character(unique(group$economic)), function(ec){
  
  new.df <- df[df$country %in% group[group$economic %in% ec, ]$country, ]
  data.frame(plyr::ddply(new.df, .(set, year), summarise, pop = sum(pop), gdpt = sum(gdpt), gdp = sum(gdpt)/sum(pop)), country = ec)
  
})
econ.df <- do.call(rbind, econ.df)

reg.df <- lapply(as.character(unique(group$regional)), function(ec){
  
  new.df <- df[df$country %in% group[group$regional %in% ec, ]$country, ]
  data.frame(plyr::ddply(new.df, .(set, year), summarise, pop = sum(pop), gdpt = sum(gdpt), gdp = sum(gdpt)/sum(pop)), country = ec)
  
})
reg.df <- do.call(rbind, reg.df)


library(ggplot2)
# glob.df <- glob.df[glob.df$year <= 2050, ]

ggplot(data = glob.df[glob.df$year <= 2050, ], aes(x = year, y = pop/10^6, col = set)) + 
  geom_line() + 
  labs(col = 'Economic growth') +
  xlab("Year") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

glob.text <- glob.df[glob.df$set %in% 3 & glob.df$year %in% c(2016, seq(2020, 2050, by = 10)), ]
ggplot() + 
  geom_line(data = glob.df[glob.df$year %in% c(2016, seq(2020, 2050, by = 5)), ], 
            aes(x = gdp, y = pop/10^6, group = as.factor(year)), col = 'black', linetype = 2, size = 0.1, show.legend=F)+
  geom_line(data = glob.df[glob.df$year %in% c(2016, seq(2020, 2050, by = 1)) , ],
            aes(x = gdp, y = pop/10^6, group = set, col = as.factor(set)), show.legend=F)+
  geom_smooth(data = glob.df[glob.df$year == 2050, ], aes(x = gdp, y = pop/10^6), 
              method = 'glm', col = 'black', show.legend = F)+
  geom_text(data = glob.text[glob.text$year <= 2050, ], aes(x = gdp, y = pop/10^6, label=year), col = 'black', 
            hjust = -0.5, vjust = 1, size = 4, show.legend = F)+
  labs(col = 'Year') +
  scale_x_continuous(expand = c(0, 20000), limits = c(0, 300000))+
  xlab("GDP (per-capita in 2005 USD)") + ylab('Population (million)') +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

weird <- scales::trans_new("signed_log",
                           transform=function(x) sign(x)*log(abs(x)),
                           inverse=function(x) sign(x)*exp(abs(x)))


ggplot(data = df[df$year == 2050 & df$set == 1, ])+
  geom_point(aes(x = gdp, y = pop, size = pop2015, colour = factor(econid), fill = factor(econid)), 
             alpha = 0.8,  pch = 21, stroke = 1, show.legend = T) + 
  scale_size_continuous(range = c(1, 20)) +
  scale_x_continuous( limits = c(-0.03, 0.05)) +
  scale_y_continuous(limits = c(-1000, 1000), trans = weird, breaks = c(-1000, -500, -100, -10, -5, -1, 0, 1,5, 10, 100, 500, 1000)) +
  theme_bw() +
  # stat_ellipse(aes(x = popr, y = popinc, colour = factor(econid), fill = factor(econid)), alpha = 1, show.legend = F)+
  # geom_vline(aes(xintercept=0), size=0.7, linetype = 'twodash', show.legend = F)+
  # geom_hline(aes(yintercept=1), size=0.7, linetype = 'twodash', show.legend = F)+
  # geom_vline(aes(xintercept=medpopre, colour=factor(econid)), size=0.5, linetype = 'longdash', show.legend = F)+
  # geom_hline(aes(yintercept=medpopince, colour=factor(econid)), size=0.5, linetype = 'longdash', show.legend = F)+
  # scale_color_brewer(palette='Dark2') +
  # scale_fill_brewer(palette='Dark2') +
  scale_color_manual(values=c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A"), labels = c('High', 'Upper middle', 'Lower middle', 'Low'))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02" ,"#7570B3", "#E7298A"), labels = c('High', 'Upper middle', 'Lower middle', 'Low'))+                     
  xlab('Population increase by 2070 (population in 2015 = 1)') +
  ylab('Population increase by 2070 (million)') +
  labs(colour = 'Income groups', fill = 'Income groups', size = 'Population (million)') +
  guides(colour = guide_legend(override.aes = list(size = 5), order = 1), fill = guide_legend(override.aes = list(size = 5), order = 1), size = guide_legend(order = 2))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text=element_text(size=12), axis.title=element_text(size=12), aspect.ratio=1, legend.text = element_text(size=10), legend.title = element_text(size=12))  

library(plotly)
p <- plot_ly(glob.df[glob.df$year <= 2050, ], y = ~year, x = ~gdp, z = ~pop, color = ~set,
             type = 'scatter3d', mode = 'lines', line = list(width = 6)) %>%
  # add_markers() %>%
  layout(scene = list(yaxis = list(title = 'Year'),
                      xaxis = list(title = 'per-capita GDP (2005 USD)'),
                      zaxis = list(title = 'Population')))
p
  