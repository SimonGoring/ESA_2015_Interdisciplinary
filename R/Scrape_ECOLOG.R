#  Scraping ECOLOG-L for job ads:
#  This is the format of an ecolog URL:
#  https://listserv.umd.edu/cgi-bin/wa?A1=ind1412e&L=ecolog-l
#
#  most of it is pretty common.  The key portion is A1=ind[YY][MM][a-e] where we
#  insert the YY year and MM month, and then cycle a - e by week.
#
#  This then returns an HTML page where the unique messages are 
library(plyr)

get_msg <- function(URL){
  
  message.page <- try(readLines(URL))
  
  if(class(message.page) == 'try-error'){
    return(NA)
  }
  
  which.msg <- which(regexpr('MSGHDR-Content-Type-PRE', message.page)>0) + 2
  which.end <- which(regexpr('<a href=\"#TOP\">Top of message</a>', message.page) > 0) - 1

  if(length(which.msg) == 0 | length(which.end) == 0){
    # Failed to find the message:
    return(NA)
  }
  
  which.subj <- message.page[which(regexpr('MSGHDR-Subject-PRE', message.page)>0)]
  
  subj <- substr(which.subj, 
                 regexpr('<span ID=\"MSGHDR-Subject-PRE\">', which.subj) + 30,
                 nchar(which.subj))
  
  message <- paste(message.page[which.msg:which.end], collapse = ' ')
  
  return(list(subj, message))
}

form_url <- function(x, year, month, week){

  if(all(colnames(x) %in% c('year', 'month', 'week'))){
    month <- x['month']
    year  <- x['year']
    week   <- x['week']
  }
  
  if(nchar(month) == 1) month <- paste0('0', month)
  if(nchar(year) == 4) year  <- substr(year, 3, 4)
  
  paste0("https://listserv.umd.edu/cgi-bin/wa?A1=ind",
          year, month, letters[week], "&L=ecolog-l")

}

get_posts <- function(control.URL){
  
  control.page <- try(readLines(control.URL))
  
  if(class(control.page) == 'try-error'){
    return(NA)
  }
  
  if(any(regexpr('The archive files could not be accessed', control.page)>0)){
    #  The combination doesn't work:
    return(NA)
  }
  
  find.posts <- regexpr('&L=ecolog-l&P=[0-9]+\"', control.page)
  
  good.pages <- control.page[find.posts > 0]
  start      <- find.posts[find.posts > 0] + 14
  end <- (find.posts + attributes(find.posts)$match.length)[find.posts > 0] - 1
  pages <- substr(good.pages,
                  start, 
                  end)
  
  control.URL <- gsub('A1=', 'A2=', control.URL)
  paste0(control.URL, '&P=', pages)
}

myw2 <- expand.grid(week = 1:5, month = 1:12, year = 2000:2015)

big.pages <- apply(myw2, 1, form_url)

page.posts <- lapply(big.pages,get_posts)

#all.messages <- unlist(lapply(page.posts, function(x)lapply(x,function(y)try(get_msg(y)))),recursive=FALSE)
all.messages <- list()

for(i in i:length(page.posts)){
  all.messages[[i]] <- list()
  for(j in 1:length(page.posts[[i]])){
    all.messages[[i]][[j]] <- get_msg(page.posts[[i]][[j]])
    cat(paste0(i, ', ', j, ': ', page.posts[[i]][[j]], 
        ': ', class(all.messages[[i]][[j]]), '\n'))
  }
}

