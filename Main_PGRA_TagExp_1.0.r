# do not use scientific notation
options(scipen=999) 

# Import libraries: Although some of them are not required
library(stats); library(minpack.lm); library(MuMIn);library(AICcmodavg); library(Metrics); library(plyr); library(zoo)
library(pbapply); library(forecast); library(parallel); library(snow); library(pbmcapply); 
library(readxl); library(zoo); library(Hmisc); library(ggplot2); library(deepslm); library(readxl)

data.dir <- paste(working.dir, '/data/', sep='') # set data directory
df <- importcsvs(dr = data.dir, type = c('hist', 'proj')) # import files

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

# now we need to select models based on wAIC
# Generate experience models
exp.models <- pbmcapply::pbmclapply(exp.countries, mc.cores = 30, function(ct){
  
  hist.df <- df$hist[df$hist$country == ct, ]  
  proj.df <- df$proj[df$proj$country == ct, ] 
  
  hist.df <- hist.df[hist.df$year %in% hist.years, ]  
  proj.df <- proj.df[proj.df$year %in% proj.years, ]  
  
  hist.df <- hist.df[hist.df$year > 1990, ] # limit years to recent data
  
  # multiple regression analysis with multiple dependant variables
  rt.dp <- lapply(dep.vars, function(dp){
    
    # reorganise data to apply linear regression on each dependant variable
    ht.df <- hist.df[, c(dp, 'GDP', 'year')]
    pj.df <- proj.df[, c('GDP', 'year')]
    
    ht.df <- ht.df[complete.cases(ht.df), ]
    
    # transform historical data to fit models
    train.lt <- list(lm.fit = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP),
                     div.fit = data.frame(dp = ht.df[, dp], GDP = (1/ht.df$GDP)),
                     nloge.fit = data.frame(dp = ht.df[, dp], GDP = log(1/ht.df$GDP)),
                     npowe.fit = data.frame(dp = ht.df[, dp], GDP = (ht.df$GDP^(-2.718282))))
    
    md.names <- names(train.lt)  
    
    # fit 'null' model
    rc.null <- lm(ht.df[, dp] ~ 1)
    # rc.null$predict <- rep(rc.null$fitted.values[1], length(ht.df$year))
    
    # fit linear models to the given data
    rc.lm <- fit(lt = train.lt, threshold = F, replace.eq = rc.null) #
    
    trainsp.lt <- list(sp1 = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP))
    testsp.lt <- list(sp1 = data.frame(dp = NA, GDP = pj.df$GDP))
    
    if((nrow(trainsp.lt[[1]]) - range.bp*2) > 2) {
      
      rc.sp <- lmspfit2(lt = trainsp.lt, pl = NA, threshold = F, tl = thre.df, method = c('lmspline'), range.bp = range.bp, replace.eq = rc.null)
      #'lefthinge', 'righthinge'
    } else{
      
      rc.sp <- list()
      
      rc.sp$sp1$lmspline <- rc.null
      # rc.sp$sp1$lefthinge <- rc.null
      # rc.sp$sp1$righthinge <- rc.null
      
    }
    
    fit.models <- list(lm = rc.lm$lm.fit,
                       div = rc.lm$div.fit,
                       nloge = rc.lm$nloge.fit,
                       npowe = rc.lm$npowe.fit,
                       null = rc.null,
                       # lefthinge = rc.sp$sp1$lefthinge,
                       # righthinge = rc.sp$sp1$righthinge,
                       lmspline = rc.sp$sp1$lmspline) #
    
    return(fit.models)
    
  })
  
  names(rt.dp) <- dep.vars
  
  return(rt.dp)
  
})
names(exp.models) <- exp.countries

# Generate target models
target.models <- pbmcapply::pbmclapply(country.list, mc.cores = 30, function(ct){
  
  hist.df <- df$hist[df$hist$country == ct, ]  
  proj.df <- df$proj[df$proj$country == ct, ] 
  
  hist.df <- hist.df[hist.df$year %in% hist.years, ]  
  proj.df <- proj.df[proj.df$year %in% proj.years, ]  
  
  # hist.df <- hist.df[hist.df$year > 1990, ] # limit years to recent data
  
  # multiple regression analysis with multiple dependant variables
  rt.dp <- lapply(dep.vars, function(dp){
    
    # reorganise data to apply linear regression on each dependant variable
    ht.df <- hist.df[, c(dp, 'GDP', 'year')]
    pj.df <- proj.df[, c('GDP', 'year')]
    
    ht.df <- ht.df[complete.cases(ht.df), ]
    
    # transform historical data to fit models
    train.lt <- list(lm.fit = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP),
                     div.fit = data.frame(dp = ht.df[, dp], GDP = (1/ht.df$GDP)),
                     nloge.fit = data.frame(dp = ht.df[, dp], GDP = log(1/ht.df$GDP)),
                     npowe.fit = data.frame(dp = ht.df[, dp], GDP = (ht.df$GDP^(-2.718282))))
    
    md.names <- names(train.lt)  
    
    # fit 'null' model
    rc.null <- lm(ht.df[, dp] ~ 1)
    # rc.null$predict <- rep(rc.null$fitted.values[1], length(ht.df$year))
    
    # fit linear models to the given data
    rc.lm <- fit(lt = train.lt, threshold = F, replace.eq = rc.null) #
    
    trainsp.lt <- list(sp1 = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP))
    testsp.lt <- list(sp1 = data.frame(dp = NA, GDP = pj.df$GDP))
    
    if((nrow(trainsp.lt[[1]]) - range.bp*2) > 2) {
      
      rc.sp <- lmspfit2(lt = trainsp.lt, pl = NA, threshold = F, tl = thre.df, method = c('lmspline'), range.bp = range.bp, replace.eq = rc.null)
      #'lefthinge', 'righthinge'
    } else{
      
      rc.sp <- list()
      
      rc.sp$sp1$lmspline <- rc.null
      # rc.sp$sp1$lefthinge <- rc.null
      # rc.sp$sp1$righthinge <- rc.null
      
    }
    
    fit.models <- list(lm = rc.lm$lm.fit,
                       div = rc.lm$div.fit,
                       nloge = rc.lm$nloge.fit,
                       npowe = rc.lm$npowe.fit,
                       null = rc.null,
                       # lefthinge = rc.sp$sp1$lefthinge,
                       # righthinge = rc.sp$sp1$righthinge,
                       lmspline = rc.sp$sp1$lmspline) #
    
    return(fit.models)
    
  })
  
  names(rt.dp) <- dep.vars
  
  return(rt.dp)
  
})
names(target.models) <- country.list

# dep.vars <- 'fertility'
stoch.output <- pblapply(country.list, function(ct){
  
  message(ct)
  
  rk <- lapply(st, function(s){
    
    hist.df <- df$hist[df$hist$country == ct, ]  
    proj.df <- df$proj[df$proj$country == ct, ]  
    
    hg <- hist.growth[ct, ]
    gdp <- hist.df$GDP[hist.df$year == max(hist.years)]
    
    rc <- lapply((max(hist.years) + 1): max(proj.years), function(k){
      
      vec <- hg[as.character(c((k-5): (k-1)))]
      avg <- mean(vec)
      std <- sd(vec)
      
      vec2 <- hg
      
      vt <- vec2 > 0
      
      if(vt[length(vt)] == T) ldr <- as.numeric(names(vt[vt == FALSE])[length(vt[vt == FALSE])]) + 1 else ldr <- as.numeric(names(vt[vt == TRUE])[length(vt[vt == TRUE])]) + 1
      pen <- sqrt(k - ldr) * 0.01
      
      if(all(vec > 0) | all(vec < 0)) avg <- avg - avg * pen
      
      test <- FALSE
      while(test == FALSE){ # check this part to have all values within the max and min
        
        hn <- rnorm(1, avg, std)  
        if((hn <= mg) & (hn >= ng)) test <- TRUE
        
      }
      
      hg <<- c(hg, hn)
      names(hg)[length(hg)] <<- as.character(k)
      
    })
    remove(rc)
    
    gdp.rate <- hg[names(hg) %in% c(max(hist.years)+1 : max(proj.years))]
    
    proj.gdp <- sapply(gdp.rate, function(x) gdp <<- gdp * (1+ x))
    proj.gdp <- data.frame(year = c((max(hist.years)+1):max(proj.years)), GDP = proj.gdp)
    
    proj.df <- merge(proj.df[, c('year', 'country')], proj.gdp, by = 'year')
    
    hist.df <- hist.df[hist.df$year %in% hist.years, ]  
    proj.df <- proj.df[proj.df$year %in% proj.years, ]  
    
    exp.df <- df$hist[!(df$hist$country %in% ct), ]
    exp.df <- exp.df[complete.cases(exp.df$GDP), ]
    
    # exp.df <- exp.df[exp.df$GDP >= min(proj.df$GDP)*0.95, ]
    # exp.df <- exp.df[exp.df$GDP >= max(hist.df$GDP)*0.8, ]
    # exp.df <- exp.df[exp.df$GDP <= max(proj.df$GDP)*1.05, ]
    
    test.df <- exp.df
    test.df <- test.df[test.df$year >= 2010, ]
    test.df <- test.df[!(test.df$country %in% excluded.countries), ]
    # test.df <- test.df[test.df$GDP >= max(hist.df$GDP) * 0.8, ]
    # test.df <- test.df[test.df$GDP > min(proj.df$GDP) * 1, ]
    test.df <- test.df[test.df$GDP > min(proj.df$GDP) * 1, ]
    sel.countries <- as.character(unique(test.df$country))
    
    # select experience models
    
    tar.models <- target.models[[ct]]
    sel.models <- exp.models[intersect(names(exp.models), sel.countries)]
    
    # calculate prediction during the GDP projection and target country's historical period
    
    hist.df <- df$hist[df$hist$country == ct, ]  
    proj.df <- df$proj[df$proj$country == ct, ] 
    
    hist.df <- hist.df[hist.df$year %in% hist.years, ]  
    proj.df <- proj.df[proj.df$year %in% proj.years, ]  
    
    # hist.df <- hist.df[hist.df$year > 1990, ] # limit years to recent data
    
    # multiple regression analysis with multiple dependant variables
    rt.dp <- mclapply(dep.vars, mc.cores = length(dep.vars), function(dp){
      
      tm <- tar.models[[dp]]
      
      # reorganise data to apply linear regression on each dependant variable
      ht.df <- hist.df[, c(dp, 'GDP', 'year')]
      pj.df <- proj.df[, c('GDP', 'year')]
      
      ht.df <- ht.df[complete.cases(ht.df), ]
      
      # transform historical data to fit models
      hist.lt <- list(lm = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP),
                      null = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP),
                      lmspline = data.frame(dp = ht.df[, dp], GDP = ht.df$GDP),
                      div = data.frame(dp = ht.df[, dp], GDP = (1/ht.df$GDP)),
                      nloge = data.frame(dp = ht.df[, dp], GDP = log(1/ht.df$GDP)),
                      npowe = data.frame(dp = ht.df[, dp], GDP = (ht.df$GDP^(-2.718282))))
      
      proj.lt <- list(lm = data.frame(dp = NA, GDP = pj.df$GDP),
                      null = data.frame(dp = NA, GDP = pj.df$GDP),
                      lmspline = data.frame(dp = NA, GDP = pj.df$GDP),
                      div = data.frame(dp = NA, GDP = (1/pj.df$GDP)),
                      nloge = data.frame(dp = NA, GDP = log(1/pj.df$GDP)),
                      npowe = data.frame(dp = NA, GDP = (pj.df$GDP^(-2.718282))))
      
      md.names <- names(hist.lt)  
      
      # find target model and estimate hist and proj
      tm.list <- names(tm)
      
      target.hist <- lapply(tm.list, function(tl) {
        pr <- predict(tm[[tl]], newdata = hist.lt[[tl]])
        if(any(pr < 0)) tl <- 'null'
        pr <- predict(tm[[tl]], newdata = hist.lt[[tl]])
      })
      names(target.hist) <- tm.list
      
      target.proj <- lapply(tm.list, function(tl) {
        pr <- predict(tm[[tl]], newdata = proj.lt[[tl]])
        if(any(pr < 0)) tl <- 'null'
        pr <- predict(tm[[tl]], newdata = proj.lt[[tl]])
      })
      names(target.proj) <- tm.list
      
      if(length(sel.models) > 0){
        
        exp.hist <- lapply(names(sel.models), function(sc){
          sm <- sel.models[[sc]][[dp]]
          rt <- lapply(tm.list, function(tl) {
            pr <- predict(sm[[tl]], newdata = hist.lt[[tl]])
            if(any(pr < 0)) tl <- 'null'
            pr <- predict(sm[[tl]], newdata = hist.lt[[tl]])
          })
          names(rt) <- tm.list
          return(rt)
          
        })
        names(exp.hist) <- names(sel.models)
        exp.hist <- do.call(c, exp.hist)
        
        exp.proj <- lapply(names(sel.models), function(sc){
          sm <- sel.models[[sc]][[dp]]
          rt <- lapply(tm.list, function(tl){
            pr <- predict(sm[[tl]], newdata = proj.lt[[tl]])
            if(any(pr < 0)) tl <- 'null'
            pr <- predict(sm[[tl]], newdata = proj.lt[[tl]])
          })
          names(rt) <- tm.list
          return(rt)
          
        })
        names(exp.proj) <- names(sel.models)
        exp.proj <- do.call(c, exp.proj)
        
        n <- dim(ht.df)[1]; k <- 4
        aicc.tar <- lapply(target.hist, function(rp) {2 * k * (n / (n - k - 1)) + n * log(sum((ht.df[[dp]] - rp)^2, na.rm=T) / n)})
        aicc.exp <- lapply(exp.hist, function(rp) {2 * k * (n / (n - k - 1)) + n * log(sum((ht.df[[dp]] - rp)^2, na.rm=T) / n)})
        
        w.aic.target <- round(Weights(as.numeric(aicc.tar)), digits = 5)
        w.aic.examplar <- round(Weights(as.numeric(aicc.exp)), digits = 5)
        
        w.rate <- length(which(w.aic.target > 0)) / (length(which(w.aic.target > 0)) + length(which(w.aic.examplar > 0)))
        
        w.aic.target <- w.aic.target * w.rate
        w.aic.examplar <- w.aic.examplar * (1-w.rate)
        
        sel.tar <- tm[w.aic.target > 0]
        
        sel.exp <- lapply(names(sel.models), function(sc){
          sel.models[[sc]][[dp]]
        })
        
        names(sel.exp) <- names(sel.models)
        sel.exp <- do.call(c, sel.exp)
        sel.exp <- sel.exp[w.aic.examplar > 0]
        
        w.aic <- c(w.aic.target[w.aic.target>0], w.aic.examplar[w.aic.examplar>0])
        names(sel.tar) <- paste(ct, '.', names(sel.tar), sep = '')
        models <- list(sel.tar, sel.exp)
        models <- do.call(c, models)
        
        proj.out <- cbind(do.call(cbind, target.proj[which(w.aic.target>0)]), do.call(cbind, exp.proj[which(w.aic.examplar>0)]))
        colnames(proj.out) <- c(names(sel.tar), names(sel.exp))
        
        weighted.out <- t(t(proj.out) * w.aic)
        weighted.out <- cbind(weighted.out, avg=rowSums(weighted.out))
        
        
      } else{
        
        n <- dim(ht.df)[1]; k <- 4
        aicc.out <- lapply(target.hist, function(rp) {2 * k * (n / (n - k - 1)) + n * log(sum((ht.df[[dp]] - rp)^2, na.rm=T) / n)})
        w.aic <- round(Weights(as.numeric(aicc.out)), digits = 5)
        
        sel.tar <- tm[w.aic > 0]
        names(sel.tar) <- paste(ct, '.', names(sel.tar), sep = '')
        models <- do.call(c, list(sel.tar))
        
        proj.out <- cbind(do.call(cbind, target.proj[which(w.aic>0)]))
        colnames(proj.out) <- c(names(sel.tar))
        
        w.aic <- w.aic[w.aic>0]
        weighted.out <- t(t(proj.out) * w.aic)
        weighted.out <- cbind(weighted.out, avg=rowSums(weighted.out))
        
      }
      
      output <- list(models = models, comp = w.aic, avg = weighted.out)
      
    })
    
    names(rt.dp) <- dep.vars
    
    rt.dp <- list(hist = hist.df, proj = proj.df, rt.dp)
    names(rt.dp) <- c('hist', 'proj', 'output')
    
    rcp <- data.frame(rt.dp$proj, sapply(dep.vars, function(dp) rt.dp$output[[dp]]$avg[, 'avg']))
    rk <- rbind.fill(rt.dp$hist, rcp)
    
    # plot(rk$fertility~rk$GDP)
    # plot(rk$female_0~rk$GDP)
    
    return(rk)
  })
  
  return(rk)
})
names(stoch.output) <- country.list

range.bp <- 3

excluded.countries <- c('MAC', 'HKG', 'QAT', 'KWT', 'ARE', 'TTO', 'ABW', 'IRL')
exp.countries <- country.list[!(country.list %in% excluded.countries)]
st <- c(1)

male.lt <- data.frame(read_xlsx(paste(working.dir, '/data/popbyagemale.xlsx', sep = '')))
female.lt <- data.frame(read_xlsx(paste(working.dir, '/data/popbyagefemale.xlsx', sep = '')))

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

ct <- country.list
plot(rowSums(rt2[[ct]][[1]]$male) + rowSums(rt2[[ct]][[1]]$female))
