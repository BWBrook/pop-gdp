popbyage <- function(df, proj.year, hist.pop, max.age){

   # df = stc[[1]]; proj.year = c(2016:2070); hist.pop = pop.lt; max.age = 150
   # require(zoo)
  
  countries <- names(df)
  
  rd <- lapply(countries, function(ct){
    
    d <- df[[ct]]
    
    if(class(d) == 'data.frame'){
      
      male.pl <- pop.lt$male[rownames(pop.lt$male) == ct, ]
      female.pl <- pop.lt$male[rownames(pop.lt$female) == ct, ]
      
      dt <- data.frame(array(NA, c(length(min(d$year):max(proj.year)), length(0: max.age))))
      rownames(dt) <- as.numeric(min(d$year):max(proj.year))
      colnames(dt) <- as.numeric(c(0: max.age))
      dt <- data.frame(dt, year = rownames(dt), check.names = F)
      
      # female population
      
      fd <- data.frame(d[ , grepl('female_' , colnames(d))])
      colnames(fd) <- as.numeric(sub('female_*', '', colnames(fd)))
      fd <- data.frame(fd, year = d$year, check.names = F)
      
      dt[dt$year %in% fd$year, colnames(dt) %in% colnames(fd)] <- fd
      
      dt <- na.approx(dt)
      rownames(dt) <- as.numeric(min(d$year):max(proj.year))
      
      dt <- dt[, -ncol(dt)]
      dt[, 2] <- dt[, 2]/4
      dt[, 6: 81] <- dt[, 6: 81]/5
      #####
      d2 <- sapply(1: nrow(dt), function(i){

        k <- lm(y ~ I(x^2.718282), data = data.frame(y = as.numeric(dt[i, 41:81]), x = c(41: 81)))
        k2 <- predict(k, newdata = data.frame(x = seq(86, max.age, 5)))

        names(k2) <- seq(86, max.age, 5)
        return(k2)
      })
      d2 <- t(d2)
      dt[,colnames(dt) %in% colnames(d2)] <- d2
      dt[dt > 1] <- 1
      #####
      
      dt <- t(na.locf(t(dt)))
      
      dt <- dt[rownames(dt) %in% proj.year, ]
      
      fert<- df[[ct]][, c('year', 'fertility')]
      ft <- data.frame(year = c(min(d$year):max(proj.year)), fertility = NA)
      ft[ft$year %in% fert$year, 'fertility'] <- fert$fertility
      
      ft$fertility[ft$year > 2014] <- na.approx(ft$fertility[ft$year > 2014])
      ft <- ft[ft$year %in% proj.year, ]
      
      fr <- array(0, c(length(proj.year), (max.age + 1)))
      colnames(fr) <- c(0: max.age)
      rownames(fr) <- proj.year
      
      rt <- lapply(proj.year, function(x){
        
        fy <- ft[ft$year == x, 'fertility']
        
        if(x == min(proj.year)) {
          
          pp <- c(as.numeric(female.pl), c(rep(0, max.age - 100)))
          
          pn <- floor((sum(pp[(min.a + 1):(max.a + 1)]) * fy / (max.a - min.a + 1)) * 100 / 205)
          # pn <- floor((sum(pp) * fy / (length(female.pl[which(female.pl > 0)])) * (100 / 205)))
          
        } else {
          
          pp <- as.numeric(fr[rownames(fr) == (x-1), ])
          pn <- floor((sum(fr[rownames(fr) == (x-1), (min.a + 1):(max.a + 1)]) * fy / (max.a - min.a + 1)) * 100 / 205)
          # po <- fr[rownames(fr) == (x-1), ]
          # pn <- floor((sum(po) * fy / (length(female.pl[which(female.pl > 0)]))) * (100 / 205))
          
        }
        
        mt <- (1- dt[rownames(dt) == x, ])
        pp.new <- c(pn, floor(mt * pp)[1: max.age])
        names(pp.new) <- c(0: max.age)
        
        fr[rownames(fr) == x, ] <<- pp.new
        
      })
      
      female.pop <- fr
      
      # male population
      
      dt <- data.frame(array(NA, c(length(min(d$year):max(proj.year)), length(0: max.age))))
      rownames(dt) <- as.numeric(min(d$year):max(proj.year))
      colnames(dt) <- as.numeric(c(0: max.age))
      dt <- data.frame(dt, year = rownames(dt), check.names = F)
      
      fd <- data.frame(d[ , grepl('male_' , colnames(d)) & !grepl("female_", colnames(d))])
      colnames(fd) <- as.numeric(sub('male_*', '', colnames(fd)))
      fd <- data.frame(fd, year = d$year, check.names = F)
      
      dt[dt$year %in% fd$year, colnames(dt) %in% colnames(fd)] <- fd
      
      dt <- na.approx(dt)
      rownames(dt) <- as.numeric(min(d$year):max(proj.year))
      
      dt <- dt[, -ncol(dt)]
      dt[, 2] <- dt[, 2]/4
      dt[, 6: 81] <- dt[, 6: 81]/5
      #####
      d2 <- sapply(1: nrow(dt), function(i){

        k <- lm(y ~ I(x^2.718282), data = data.frame(y = as.numeric(dt[i, 41:81]), x = c(41: 81)))
        k2 <- predict(k, newdata = data.frame(x = seq(86, max.age, 5)))

        names(k2) <- seq(86, max.age, 5)
        return(k2)
      })
      d2 <- t(d2)
      dt[,colnames(dt) %in% colnames(d2)] <- d2
      dt[dt > 1] <- 1
      #####
      
      dt <- t(na.locf(t(dt)))
      
      dt <- dt[rownames(dt) %in% proj.year, ]
      
      fr <- array(0, c(length(proj.year), (max.age + 1)))
      colnames(fr) <- c(0: max.age)
      rownames(fr) <- proj.year
      
      rt <- lapply(proj.year, function(x){
        
        fp <- female.pop[rownames(female.pop) == x, 1]
        pn <- floor(fp * (205 / 100) * (105 / 205))
        
        mt <- (1- dt[rownames(dt) == x, ])
        
        
        if(x == min(proj.year)) {
          
          pp <- c(as.numeric(male.pl), c(rep(0, max.age - 100)))
          
        } else {
          
          pp <- as.numeric(fr[rownames(fr) == (x-1), ])
          
        }
        
        pp.new <- c(pn, floor(mt * pp)[1: max.age])
        names(pp.new) <- c(0: max.age)
        
        fr[rownames(fr) == x, ] <<- pp.new
        
      })
      
      male.pop <- fr
      
      ret <- list(male = male.pop, female = female.pop, gdp = d$GDP)
      
      return(ret)
    }
    
  })
  
  names(rd) <- countries
  
  return(rd)
}