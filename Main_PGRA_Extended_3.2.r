# Import libraries: Although some of them are not required
library(stats); library(minpack.lm); library(MuMIn);library(AICcmodavg); library(Metrics); library(plyr); library(zoo)
library(pbapply); library(forecast); library(parallel); library(snow); library(pbmcapply); 
library(readxl); library(zoo); library(Hmisc); library(ggplot2); library(deepslm); library(data.table)

# set data directory
data.dir <- paste(working.dir, '/data/', sep='')

# import files
df <- importcsvs(dr = data.dir, type = c('hist', 'proj'))

country.list <- intersect(as.character(unique(df$hist$country)), as.character(unique(df$proj$country)))
hist.years <- seq(1961, 2015, 1)
proj.years <- seq(2020, 2100, 10)

df$hist <- df$hist[order(df$hist$country, decreasing = F), ]
df$proj <- df$proj[order(df$proj$country, decreasing = F), ]

dep.vars <- c('fertility', 'male_0', 'male_1', 'male_5', 'male_10', 'male_15', 'male_20', 'male_25', 'male_30', 'male_35', 'male_40',
              'male_45', 'male_50', 'male_55', 'male_60', 'male_65', 'male_70', 'male_75', 'male_80', 
              'female_0', 'female_1', 'female_5', 'female_10', 'female_15', 'female_20', 'female_25', 'female_30', 'female_35', 'female_40',
              'female_45', 'female_50', 'female_55', 'female_60', 'female_65', 'female_70', 'female_75', 'female_80')

# set thresholds
thre.glob <- sapply(dep.vars, function(dp) quantile(df$hist[, dp], na.rm=T, probs = seq(0.01, 0.99, 0.01)))
names(thre.glob) <- dep.vars

# stochastic GDP
hist.gdp = read.csv('./data/IMHEGDP_ID.csv', check.names = F)
rownames(hist.gdp) <- hist.gdp$GDP.hist
hist.gdp <- hist.gdp[, -1]
hist.gdp <- data.frame(t(hist.gdp))
hist.gdp <- hist.gdp[as.character(hist.years), ]
hist.growth <- array(NA, c(ncol(hist.gdp), nrow(hist.gdp)))
colnames(hist.growth) <- rownames(hist.gdp)
rownames(hist.growth) <- colnames(hist.gdp)

for(i in 1: ncol(hist.gdp)) hist.growth[i, ] <- c(NA, exp(diff(log(hist.gdp[[i]]))) - 1)

hist.gdp <- t(hist.gdp)

ng <- quantile(hist.growth, na.rm=T, probs=seq(0, 1, 0.01))[6]
mg <- quantile(hist.growth, na.rm=T, probs=seq(0, 1, 0.01))[100]

range.bp <- 3
# dep.vars <- 'fertility'
# ct <- 'IND'
st <- 1
include.target <- F
# excluded.countries <- c('MAC', 'HKG', 'QAT', 'KWT', 'ARE', 'TTO', 'ABW', 'IRL', 'BWA', 'GNQ')
group <- read.csv(paste(working.dir, '/additional_data/countries_groups.csv', sep=''))
low.countries <- as.character(group[group$economic %in% 'Low', ]$country)
lowermiddle.countries <- as.character(group[group$economic %in% 'Lowermiddle', ]$country)
small.countries <- as.character(df$hist[df$hist$year == max(df$hist$year) & df$hist$pop < 500000, ]$country)
middleeast.countries <- as.character(group[group$regional %in% 'Middle East and North Africa', ]$country)
subsahara.countries <- as.character(group[group$regional %in% 'Sub-Saharan Africa', ]$country)
latin.countries <- as.character(group[group$regional %in% 'Latin America and Caribbean', ]$country)
# excluded.countries <- c('MAC', 'HKG', 'QAT', 'KWT', 'ARE', 'TTO', 'ABW', 'IRL')

excluded.countries <- as.character(unique(c(low.countries, lowermiddle.countries, latin.countries, middleeast.countries, subsahara.countries, small.countries)))
recentdata.countries <- as.character(unique(c('MDA', 'MMR', 'MNE', 'BRN','NIC', latin.countries, low.countries, middleeast.countries, subsahara.countries))) #, 'BRN', lowermiddle.countries
exp.countries <- country.list[!(country.list %in% excluded.countries)]

st <- seq(-1, 2, by = 0.1)
# st <- c(0)
target.gdp <- 10000
target.year <- 2000
# dep.vars <- 'fertility'
stoch.output <- pblapply(country.list, function(ct){

  rk <- lapply(st, function(s){
    
    # s <- st[[1]]
    proj.df <- df$proj[df$proj$country == ct, ]  
    proj.df <- proj.df[proj.df$year %in% proj.years, ]  
    
    hist.df <- df$hist[df$hist$country == ct, ]  
    hist.df <- hist.df[hist.df$year %in% hist.years, ]  
    
    if(proj.df[proj.df$year == 2020, ]$GDP < hist.df[hist.df$year == max(hist.years), ]$GDP){ proj.df$GDP <- proj.df$GDP + (hist.df[hist.df$year == max(hist.years), ]$GDP - proj.df[proj.df$year == 2020, ]$GDP) + (proj.df[proj.df$year == 2030, ]$GDP - proj.df[proj.df$year == 2020, ]$GDP)/2}
    growth.rate <- sapply(1: nrow(proj.df), function(pk) {if(pk == 1) ((proj.df[pk,]$GDP/hist.df[hist.df$year == max(hist.years), ]$GDP) ^ (1/(min(proj.years) - max(hist.years))))-1 else ((proj.df[pk,]$GDP/proj.df[pk-1,]$GDP) ^ (1/10))-1})
    growth.rate <- growth.rate * (1 + s)
    
    new.gdp <- list()
    for(pk in 1: (nrow(proj.df))){
      
      if(pk == 1) new.gdp[[pk]] <- hist.df[hist.df$year == max(hist.years), ]$GDP * (1 + growth.rate[pk])^(proj.df[pk,]$year-hist.df[hist.df$year == max(hist.years), ]$year) else new.gdp[[pk]] <- new.gdp[[pk-1]] * (1 + growth.rate[pk])^10
        
    }
    new.gdp <- do.call(c, new.gdp)
    proj.df$GDP <- new.gdp # new gdp
    
    exp.df <- df$hist[!(df$hist$country %in% ct), ]
    exp.df <- exp.df[complete.cases(exp.df$GDP), ]

    test.df <- exp.df
    test.df <- test.df[test.df$year >= target.year, ]

    if(ct %in% recentdata.countries) hist.df <- hist.df[hist.df$year > target.year, ]
    
    # multiple regression analysis with multiple dependant variables
    rt.dp <- mclapply(dep.vars, mc.cores = length(dep.vars), function(dp){
      
      if(dp == 'fertility'){
        
        # for gdp < target gdp
        if(min(proj.df$GDP) <= target.gdp){
          
          pf.df <- proj.df[proj.df$GDP < target.gdp, ]
          
          ck.out <- lapply(as.character(unique(test.df$country)), function(ck) 
            if(max(test.df[test.df$country %in%ck, ]$GDP) > pf.df$GDP & 
               min(test.df[test.df$country %in%ck, ]$GDP) < max(pf.df$GDP, 5000)) return(ck)) #  & min(test.df[test.df$country %in%ck, ]$GDP) < max(hist.df$GDP)
          
          sel.countries <- do.call(c, ck.out)
          # exp.df <- exp.df[exp.df$country %in% sel.countries, ]
          exp.df <- test.df[(test.df$country %in% sel.countries) & !(test.df$country %in% excluded.countries), ]
          
          # reorganise data to apply linear regression on each dependant variable
          ht.df <- hist.df[, c(dp, 'GDP', 'year')]
          pj.df <- pf.df[, c('GDP', 'year')]
          ht.df <- ht.df[complete.cases(ht.df), ]
          
          ep.df <- exp.df[, c('country', dp, 'GDP', 'year')]
          ep.df <- ep.df[complete.cases(ep.df), ]
          
          if(include.target == T) { if(length(as.character(unique(ep.df$country))) == 0) sel.exp <- ct else sel.exp <- c(as.character(unique(ep.df$country)), ct) }else{
            
            if(length(as.character(unique(ep.df$country))) == 0) sel.exp <- ct else sel.exp <- as.character(unique(ep.df$country))  
          }
          
          
          rc.ep <- lapply(sel.exp, function(ec){
            # message(ec)
            
            if(ct != ec) {
              
              ec.df <- ep.df[ep.df$country %in% ec, ]
              ec.df <- ec.df[, c(dp, 'GDP', 'year')]
              ec.df <- rbind(ht.df, ec.df) # ec.df[ec.df$GDP > ht.df[ht.df$year == max(ht.df$year), ]$GDP, ]
              
            } else{
              
              ec.df <- ht.df  
            } 
            
            if(nrow(ec.df) > 5){
              
              # transform historical data to fit models
              train.lt <- list(lm.fit = data.frame(dp = ec.df[, dp], GDP = ec.df$GDP),
                               div.fit = data.frame(dp = ec.df[, dp], GDP = (1/ec.df$GDP)),
                               nloge.fit = data.frame(dp = ec.df[, dp], GDP = log(1/ec.df$GDP)),
                               npowe.fit = data.frame(dp = ec.df[, dp], GDP = (ec.df$GDP^(-2.718282))))
              
              # transform projection data to predict
              test.lt <- list(lm.fit = data.frame(dp = NA, GDP = ht.df$GDP),
                              div.fit = data.frame(dp = NA, GDP = (1/ht.df$GDP)),
                              nloge.fit = data.frame(dp = NA, GDP = log(1/ht.df$GDP)),
                              npowe.fit = data.frame(dp = NA, GDP = (ht.df$GDP^(-2.718282))))
              
              
              proj.lt <- list(lm.fit = data.frame(dp = NA, GDP = pj.df$GDP),
                              div.fit = data.frame(dp = NA, GDP = (1/pj.df$GDP)),
                              nloge.fit = data.frame(dp = NA, GDP = log(1/pj.df$GDP)),
                              npowe.fit = data.frame(dp = NA, GDP = (pj.df$GDP^(-2.718282))))
              
              md.names <- names(train.lt)  
              
              # fit 'null' model
              rc.null <- lm(ec.df[, dp] ~ 1)
              rc.null$predict <- rep(rc.null$fitted.values[1], length(ht.df$year))
              rc.null$proj <- rep(rc.null$fitted.values[1], length(pj.df$year))
              
              # fit linear models to the given data
              rc.lm <- fit(lt = train.lt, pl=test.lt, threshold = F, replace.eq = rc.null) #
              
              proj.lm <- lapply(1: length(rc.lm), function(rl) predict(rc.lm[[rl]], newdata = proj.lt[[rl]]))
              
              for(k in 1: length(rc.lm)){rc.lm[[k]]$proj <- proj.lm[[k]]}
              
              trainsp.lt <- list(sp1 = data.frame(dp = ec.df[, dp], GDP = ec.df$GDP))
              testsp.lt <- list(sp1 = data.frame(dp = NA, GDP = ht.df$GDP))
              projsp.lt <- list(sp1 = data.frame(dp = NA, GDP = pj.df$GDP))
              
              if((nrow(trainsp.lt[[1]]) - range.bp*2) > 2) {
                
                rc.sp <- lmspfit2(lt = trainsp.lt, pl = testsp.lt, threshold = F, tl = thre.df, 
                                  selection = c('righthinge')
                                  , range.bp = range.bp, replace.eq = rc.null)
                
              } else{
                
                rc.sp <- list()
                
                # rc.sp$sp1$lmspline <- rc.null
                # rc.sp$sp1$lefthinge <- rc.null
                rc.sp$sp1$righthinge <- rc.null
                
              }
              
              # plot(rc.sp$sp1$righthinge$predict ~ testsp.lt$sp1$GDP)
              
              proj.sp <- lapply(1: length(rc.sp$sp1), function(rl) predict(rc.sp$sp1[[rl]], newdata = projsp.lt$sp1))
              
              for(k in 1: length(rc.sp$sp1)){rc.sp$sp1[[k]]$proj <- proj.sp[[k]]}
              
              fit.models <- list(#lm = rc.lm$lm.fit,
                                 div = rc.lm$div.fit,
                                 nloge = rc.lm$nloge.fit,
                                 npowe = rc.lm$npowe.fit,
                                 null = rc.null,
                                 # lefthinge = rc.sp$sp1$lefthinge,
                                 # lmspline = rc.sp$sp1$lmspline,
                                 righthinge = rc.sp$sp1$righthinge)
              
              if(dp == 'fertility') fit.models <- lapply(fit.models, function(fm){if(any(fm$proj < 0)) fit.models$null else fm}) else{
                
                fit.models <- lapply(fit.models, function(fm){if(any(fm$proj < 0)) fit.models$null else fm})
                
              }
              
              for(k in 1: length(fit.models)) fit.models[[k]]$predictor <- ec.df
              names(fit.models) <- paste(ec, names(fit.models))
              
              return(fit.models)
            }
            
          })
          rc.ep <- do.call(c, rc.ep)
          
          # need to calculate wAIC using the prediction
          
          n <- dim(ht.df)[1]; k <- 4
          aicc.out <- lapply(rc.ep, function(rp) {2 * k * (n / (n - k - 1)) + n * log(sum((ht.df[[dp]] - rp$predict)^2, na.rm=T) / n)})
          w.aic <- round(Weights(as.numeric(aicc.out)), digits = 5)
          
          rc.np <- rc.ep[which(w.aic > 0)]
          wei <- w.aic[which(w.aic > 0)]
          
          proj.out <- sapply(1: length(rc.np), function(rp) rc.np[[rp]]$proj * wei[[rp]])
          # proj.out <- data.frame(c(proj.out), avg = sum(proj.out))
          # colnames(proj.out) <- c(names(rc.np), 'avg')
          
          if(dim(pf.df)[1] > 1) output <- data.frame(pf.df, avg = rowSums(proj.out)) else output <- data.frame(pf.df, avg = sum(proj.out))
          
          # return(output)
          
          if(max(output$year) < max(proj.years)){
            
            output <- merge(data.frame(year = proj.df$year), output[, c('year', 'avg')], by = 'year', all=T)
            output <- na.locf(output)
            
          }
          
        } else output <- data.frame(avg = rep(hist.df[hist.df$year == max(hist.df$year), dp], nrow(proj.df)))

        return(output)
        
      } else{
        
        ck.out <- lapply(as.character(unique(test.df$country)), function(ck) if(max(test.df[test.df$country %in%ck, ]$GDP) > min(proj.df$GDP) & min(test.df[test.df$country %in%ck, ]$GDP) < max(proj.df$GDP)) return(ck)) #  & min(test.df[test.df$country %in%ck, ]$GDP) < max(hist.df$GDP)
        sel.countries <- do.call(c, ck.out)
        # exp.df <- exp.df[exp.df$country %in% sel.countries, ]
        exp.df <- test.df[(test.df$country %in% sel.countries) & !(test.df$country %in% excluded.countries), ]

        # reorganise data to apply linear regression on each dependant variable
        ht.df <- hist.df[, c(dp, 'GDP', 'year')]
        pj.df <- proj.df[, c('GDP', 'year')]
        ht.df <- ht.df[complete.cases(ht.df), ]
        
        ep.df <- exp.df[, c('country', dp, 'GDP', 'year')]
        ep.df <- ep.df[complete.cases(ep.df), ]
        
        if(include.target == T) { if(length(as.character(unique(ep.df$country))) == 0) sel.exp <- ct else sel.exp <- c(as.character(unique(ep.df$country)), ct) }else{
          
          if(length(as.character(unique(ep.df$country))) == 0) sel.exp <- ct else sel.exp <- as.character(unique(ep.df$country))  
        }
        
        
        rc.ep <- lapply(sel.exp, function(ec){
          

          if(ct != ec) {
            
            ec.df <- ep.df[ep.df$country %in% ec, ]
            ec.df <- ec.df[, c(dp, 'GDP', 'year')]
            ec.df <- rbind(ht.df, ec.df) # ec.df[ec.df$GDP > ht.df[ht.df$year == max(ht.df$year), ]$GDP, ]
            
          } else{
            
            ec.df <- ht.df  
          } 

          
          if(nrow(ec.df) > 5){
            
            # transform historical data to fit models
            train.lt <- list(lm.fit = data.frame(dp = ec.df[, dp], GDP = ec.df$GDP),
                             div.fit = data.frame(dp = ec.df[, dp], GDP = (1/ec.df$GDP)),
                             nloge.fit = data.frame(dp = ec.df[, dp], GDP = log(1/ec.df$GDP)),
                             npowe.fit = data.frame(dp = ec.df[, dp], GDP = (ec.df$GDP^(-2.718282))))
            
            # transform projection data to predict
            test.lt <- list(lm.fit = data.frame(dp = NA, GDP = ht.df$GDP),
                            div.fit = data.frame(dp = NA, GDP = (1/ht.df$GDP)),
                            nloge.fit = data.frame(dp = NA, GDP = log(1/ht.df$GDP)),
                            npowe.fit = data.frame(dp = NA, GDP = (ht.df$GDP^(-2.718282))))
            
            
            proj.lt <- list(lm.fit = data.frame(dp = NA, GDP = pj.df$GDP),
                            div.fit = data.frame(dp = NA, GDP = (1/pj.df$GDP)),
                            nloge.fit = data.frame(dp = NA, GDP = log(1/pj.df$GDP)),
                            npowe.fit = data.frame(dp = NA, GDP = (pj.df$GDP^(-2.718282))))
            
            md.names <- names(train.lt)  
            
            # fit 'null' model
            rc.null <- lm(ec.df[, dp] ~ 1)
            rc.null$predict <- rep(rc.null$fitted.values[1], length(ec.df$year))
            rc.null$proj <- rep(rc.null$fitted.values[1], length(pj.df$year))
            
            # fit linear models to the given data
            rc.lm <- fit(lt = train.lt, pl=test.lt, threshold = F, replace.eq = rc.null) #
            
            proj.lm <- lapply(1: length(rc.lm), function(rl) predict(rc.lm[[rl]], newdata = proj.lt[[rl]]))
            
            for(k in 1: length(rc.lm)){rc.lm[[k]]$proj <- proj.lm[[k]]}
            
            trainsp.lt <- list(sp1 = data.frame(dp = ec.df[, dp], GDP = ec.df$GDP))
            testsp.lt <- list(sp1 = data.frame(dp = NA, GDP = ht.df$GDP))
            projsp.lt <- list(sp1 = data.frame(dp = NA, GDP = pj.df$GDP))
            
            if((nrow(trainsp.lt[[1]]) - range.bp*2) > 2) {
              
              rc.sp <- lmspfit2(lt = trainsp.lt, pl = testsp.lt, threshold = F, tl = thre.df, 
                                selection = c('righthinge')
                                , range.bp = range.bp, replace.eq = rc.null)
              
            } else{
              
              rc.sp <- list()
              
              # rc.sp$sp1$lmspline <- rc.null
              # rc.sp$sp1$lefthinge <- rc.null
              rc.sp$sp1$righthinge <- rc.null
              
            }
            
            # plot(rc.sp$sp1$righthinge$predict ~ testsp.lt$sp1$GDP)
            
            proj.sp <- lapply(1: length(rc.sp$sp1), function(rl) predict(rc.sp$sp1[[rl]], newdata = projsp.lt$sp1))
            
            for(k in 1: length(rc.sp$sp1)){rc.sp$sp1[[k]]$proj <- proj.sp[[k]]}
            
            fit.models <- list(#lm = rc.lm$lm.fit,
                               div = rc.lm$div.fit,
                               nloge = rc.lm$nloge.fit,
                               npowe = rc.lm$npowe.fit,
                               null = rc.null,
                               # lefthinge = rc.sp$sp1$lefthinge,
                               # lmspline = rc.sp$sp1$lmspline,
                               righthinge = rc.sp$sp1$righthinge)
            
            if(dp == 'fertility') fit.models <- lapply(fit.models, function(fm){if(any(fm$proj < 0)) fit.models$null else fm}) else{
              
              fit.models <- lapply(fit.models, function(fm){if(any(fm$proj < 0)) fit.models$null else fm})
              
            }
            
            for(k in 1: length(fit.models)) fit.models[[k]]$predictor <- ec.df
            names(fit.models) <- paste(ec, names(fit.models))
            
            return(fit.models)
          }
          
        })
        rc.ep <- do.call(c, rc.ep)
        
        # need to calculate wAIC using the prediction
        
        n <- dim(ht.df)[1]; k <- 4
        aicc.out <- lapply(rc.ep, function(rp) {2 * k * (n / (n - k - 1)) + n * log(sum((ht.df[[dp]] - rp$predict)^2, na.rm=T) / n)})
        w.aic <- round(Weights(as.numeric(aicc.out)), digits = 5)
        
        rc.np <- rc.ep[which(w.aic > 0)]
        wei <- w.aic[which(w.aic > 0)]
        
        proj.out <- sapply(1: length(rc.np), function(rp) rc.np[[rp]]$proj * wei[[rp]])
        proj.out <- data.frame(proj.out, avg = rowSums(proj.out))
        colnames(proj.out) <- c(names(rc.np), 'avg')
        
        output <- list(models = rc.np, comp = w.aic[which(w.aic>0)], avg = proj.out)
        
        return(output)

      }

    })
    
    names(rt.dp) <- dep.vars
    
    rt.dp <- list(hist = hist.df, proj = proj.df, rt.dp)
    names(rt.dp) <- c('hist', 'proj', 'output')
    
    rcp <- data.frame(rt.dp$proj, sapply(dep.vars, function(dp) if(dp == 'fertility') rt.dp$output[[dp]]$avg else rt.dp$output[[dp]]$avg[, 'avg']))
    rk <- rbind.fill(rt.dp$hist, rcp)

    return(rk)
  })
  
  names(rk) <- st
  
  return(rk)
})

names(stoch.output) <- country.list

plot.new()
par(mfrow = c(4, 4))
for(i in 1: length(stoch.output)){
  plot(stoch.output[[i]][[11]]$fertility ~ stoch.output[[i]][[11]]$GDP, main= names(stoch.output[i]))
  plot(stoch.output[[i]][[11]]$fertility ~ stoch.output[[i]][[11]]$year, main= names(stoch.output[i]))
  
}

require(readxl)

male.lt <- data.frame(read_xlsx(paste(working.dir, '/additional_data/popbyagemale.xlsx', sep = '')))
female.lt <- data.frame(read_xlsx(paste(working.dir, '/additional_data/popbyagefemale.xlsx', sep = '')))

male.lt[, 3:ncol(male.lt)] <- male.lt[, 3: ncol(male.lt)]*1000
female.lt[, 3:ncol(female.lt)] <- female.lt[, 3: ncol(female.lt)]*1000

male.lt <- male.lt[male.lt$Date == '2015', ]
female.lt <- female.lt[female.lt$Date == '2015', ]

rownames(male.lt) <- male.lt$Country
rownames(female.lt) <- female.lt$Country

male.lt <- male.lt[, 3: ncol(male.lt)]
female.lt <- female.lt[, 3: ncol(female.lt)]

colnames(male.lt) <- c(0: 100)
colnames(female.lt) <- c(0: 100)

pop.lt <- list(male = male.lt, female = female.lt)

ln <- length(st)
no.countries <- length(country.list)
min.a <- 15
max.a <- 49

# for stochastic probability model
stc <- lapply(1: ln, function(x){
  
  st <- lapply(1: no.countries, function(y){
    
    stoch.output[[y]][[x]]
    
  })
  
  names(st) <- names(stoch.output)
  
  return(st)
})

# for deterministic probability model
# stc <- deter.output

source('Sub_SRAMW_Popbyage 0.0.2.r')

rt <- pblapply(stc, function(k){
  
  popbyage(df = k, proj.year = c(2016:max(proj.years)), hist.pop = pop.lt, max.age = 150)
  
})

countries <- names(rt[[1]])

rt2 <-   lapply(countries, function(ct) lapply(1: ln, function(i) rt[[i]][[ct]])) 
names(rt2) <- countries    

save(stoch.output, file = 'raw_inf_10000_pre.RDATA')
save(rt2, file = 'reor_inf_10000_pre.RDATA')
