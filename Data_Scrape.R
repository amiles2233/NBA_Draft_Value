setwd("~/Analyses/NBA/Draft Value")

library(rvest)
library(dplyr)


## Scraping Draft------------------

##----Base Data Frame----

url <- "http://www.basketball-reference.com/draft/NBA_2000.html"

## Scrape Website
draft <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="stats"]') %>%
    html_table(header=FALSE)

## Exract Data Frame
a <- draft[[1]]

## Remove Irrelvant Rows
a <- a[!is.na(as.numeric(a$X1)),c(2,4)]

## Assign Draft Year
a$draft <- 2000


for(i in c(2001:2015)){
    ## Get URL
    url <- paste("http://www.basketball-reference.com/draft/NBA_",i,".html", sep="")
    ## Scrape Website
    draft <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="stats"]') %>%
        html_table(header=FALSE)
    
    ## Exract Data Frame
    b <- draft[[1]]
    
    ## Remove Irrelvant Rows
    b <- b[!is.na(as.numeric(b$X1)),c(2,4)]
    
    ## Assign Draft Year
    b$draft <- i
    
    a <- bind_rows(a,b)
}

rm(b)
names(a) <- c("Pick", "Player", "Draft")

draft <- a

write.csv(draft, "draft.csv",row.names = FALSE)


##----Scrape Win Shares by Season
url <- "http://www.basketball-reference.com/leagues/NBA_2001_advanced.html"

adv <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="advanced"]') %>%
    html_table(header=FALSE)

a <- adv[[1]]
a <- a[!is.na(as.numeric(a$X1)),c(2,23)]
a$year <- 2001



for(i in c(2002:2016)){
    url <- paste("http://www.basketball-reference.com/leagues/NBA_",i,"_advanced.html", sep="")
    adv <- url %>%
        read_html() %>%
        html_nodes(xpath='//*[@id="advanced"]') %>%
        html_table(header=FALSE)
    b <- adv[[1]]
    b <- b[!is.na(as.numeric(b$X1)),c(2,23)]
    b$year <- i
    a <- bind_rows(a,b)
}

names(a) <- c("Player", "WS", "Season")

a <- a %>%
    group_by(Season, Player) %>%
    summarize(WS=first(WS)) %>%
    mutate(WS=as.numeric(WS))

season <- a
write.csv(season ,file="season.csv",row.names=FALSE)


