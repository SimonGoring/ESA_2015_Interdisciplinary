# Here we pass in 'all.messages'

source('Scrape_ECOLOG.R')

summarize.msg <- function(y){
    if(!length(y) == 2){
      return(data.frame(subject = NA,
                        interdisip = NA, 
                        tenure = NA, 
                        application = NA, 
                        position = NA))
    }
    if(!(length(y[[2]])>0|length(y[[2]])>0)){
      return(data.frame(subject = NA,
                        interdisip = NA, 
                        tenure = NA, 
                        application = NA, 
                        position = NA))
    }
    
    if(length(y[[1]]) == 0) y[[1]] <- NA
    
    output <- data.frame(subject = y[[1]],
                         interdisip  = regexpr('interdisciplinary', y[[2]])>0,
                         tenure      = regexpr('tenure track', y[[2]])>0,
                         application = regexpr('application', y[[2]])>0,
                         position    = regexpr('position', y[[2]])>0)
    
    output
}

msg.sum <- lapply(all.messages, function(x){
        do.call(rbind.data.frame,lapply(x, summarize.msg))})

aa <- do.call(rbind.data.frame, lapply(1:length(msg.sum), function(x)data.frame(msg.sum[[x]], myw2[x,])))

aa <- na.omit(aa)

values.by.year <- data.frame(myw2[1:length(msg.sum),],do.call(rbind.data.frame,lapply(msg.sum, function(x)apply(x[,-1], 2, mean, na.rm=TRUE))))
colnames(values.by.year)[4:7] <- colnames(msg.sum[[1]])[-1]

aa$ymw <- (aa$year + (aa$month + aa$week / 4)/12)

jobs <- aa[aa$tenure,]

model.tenure <- glm(tenure ~ I(year + (month + week / 4)/12), 
                    data = aa[aa$week < 5,], family = binomial)
model.pos    <- glm(position ~ I(year + (month + week / 4)/12), 
                    data = aa[aa$week < 5,], family = binomial)
model.tt     <- gam(tt.job ~ s(ymw, by = as.factor(interdisip)), 
                    data = aa[aa$week < 5,], family = binomial)
model.inter  <- glm(interdisip ~ I(year + (month + week / 4)/12), 
                    data = aa[aa$week < 5,], family = binomial)



plot(aa$ymw[aa$week < 5], model.tt$fitted.values, 
     ylim=c(0, 0.1), pch = 19, cex = 0.5, col = 4)
points(aa$ymw[aa$week < 5], model.tt$fitted.values, 
       ylim=c(0, 0.1), pch = 19, cex = 0.5, col = 2)
points(aa$ymw[aa$week < 5], model.inter$fitted.values, 
       ylim=c(0, 0.1), pch = 19, cex = 0.5, col = 3)
points(aa$ymw[aa$week < 5], model.pos$fitted.values, 
       ylim=c(0, 0.1), pch = 19, cex = 0.5, col = 5)
