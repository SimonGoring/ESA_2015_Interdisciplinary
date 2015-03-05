# Here we pass in 'all.messages'
library(tm)
library(SnowballC)

if('all.messages.RDS' %in% list.files('data')){
  all.messages <- readRDS(file = 'data/all.messages.RDS') 
} else {
  source('Scrape_ECOLOG.R')
}

#  The purpose of the following commands is to use the functionality of the `tm` 
#  to parse all the messages.  The list contains two items, a subject line and
#  the message text proper, so we need to select every other object:
aa <- unlist(lapply(all.messages, function(x)lapply(x, function(x)ifelse(length(x)>1,x[[2]],NA))))

tm.corp <- Corpus(VectorSource(aa))

#  Remove full URLs:
tm.corp <- tm_map(tm.corp, toSpace, "(ht|f)tp(s?)\\:\\/\\/[0-9a-zA-Z]([-.\\w]*[0-9a-zA-Z])*(:(0-9)*)*(\\/?)([a-zA-Z0-9\\-\\.\\?\\,\\'\\/\\\\+&amp;%\\$#_]*)?")
#  Remove partial URLs:
tm.corp <- tm_map(tm.corp, toSpace, "[0-9a-zA-Z]*\\.*\\.[0-9a-zA-Z]*")
tm.corp <- tm_map(x=tm.corp, FUN='removeNumbers')

# There is a pattern used to mask identifying information.  It only works after the
#  numbers are removed because the numbers specifically point to a date (or something)
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed = FALSE))
#  This gets rid of all href tags, and any hyperlinked text.
#  This should help reduce the memory overhead by about 58mb.
tm.corp <- tm_map(tm.corp, toSpace, "<a.+/a>")

# before we remove punctuation we have to make sure that we replace some objects 
# with spaces, this includes HTML entities like &lt;:
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
tm.corp <- tm_map(tm.corp, toSpace, "\\&[a-z0-9A-Z]+\\;")
tm.corp <- tm_map(tm.corp, 'removePunctuation')
tm.corp <- tm_map(tm.corp, 'stripWhitespace')
tm.corp <- tm_map(tm.corp, content_transformer(tolower))
tm.corp <- tm_map(tm.corp, removeWords, stopwords("english"))

#  Note, I tried stemming, but it does a pretty rotten job on the data.

dtm   <- DocumentTermMatrix(tm.corp)
dtm.s99 <- removeSparseTerms(dtm, 0.99)

#  all.messages is structured by week, it is a list of lists.

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
    
    #  Interdiscip definitions from Eigenbrode et al 2007.
    
    dictionary <- c('interdisciplinary', 'tenure track', 'multidisciplinary',
                    'transdisciplinary', 'application', 'CV', 'curriculum vitae', 'c.v.',
                    'seeking graduate students', 'salary', '0:9 references', 'transcripts',
                    'GRE', 'funding', 'All qualified applicants',
                    'or closely realted field', 'active research program')
    
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
