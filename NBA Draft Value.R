library(dplyr)
library(ggplot2)
library(RColorBrewer)

setwd("~/Analyses/NBA/Draft Value")

season <- read.csv("season.csv", stringsAsFactors = FALSE)
draft <- read.csv("draft.csv", stringsAsFactors = FALSE)

dat <- left_join(draft, season, by="Player") %>%
    arrange(Draft, Pick, Season) %>%
    group_by(Player) %>%
    mutate(ws_sum=cumsum(WS)) %>%
    ungroup() %>%
    mutate(lt_season=Season-Draft) %>%
    filter(Draft<Season, !is.na(lt_season))


ws_lt <- dat %>%
    group_by(Draft, lt_season) %>%
    summarize(ws=sum(WS)) %>%
    filter(!is.na(lt_season), lt_season>0) %>%
    mutate(draft=factor(Draft)) %>%
    ggplot(aes(x=lt_season, y=Draft, fill=ws, label=round(ws,1))) +
    geom_tile() +
    geom_text() +
    xlab("Year in League") +
    ylab("Draft Year") +
    scale_y_continuous(breaks=2000:2015) +
    scale_x_continuous(breaks=1:16) +
    scale_fill_gradient(low="blue", high="red", name="Win Shares") +
    theme_minimal() +
    ggtitle("Plot 1: Draft Win Shares by Year in League")

ws_cum_lt <- dat %>%
    group_by(Draft, lt_season) %>%
    summarize(ws=sum(WS)) %>%
    mutate(ws=cumsum(ws)) %>%
    filter(!is.na(lt_season), lt_season>0) %>%
    mutate(draft=factor(Draft)) %>%
    ggplot(aes(x=lt_season, y=Draft, fill=ws, label=round(ws,1))) +
    geom_tile() +
    geom_text() +
    xlab("Year in League") +
    ylab("Draft Year") +
    scale_y_continuous(breaks=2000:2015) +
    scale_x_continuous(breaks=1:16) +
    scale_fill_gradient(low="blue", high="red", name="Cumulative\nWin Shares") +
    theme_minimal() +
    ggtitle("Plot 3: Cumulative Draft Win Shares by Year in League Heatmap")

ws_season <- dat %>%
    group_by(Draft, Season) %>%
    summarize(ws=sum(WS)) %>%
    filter(!is.na(Season), Season>Draft) %>%
    mutate(draft=factor(Draft)) %>%
    ggplot(aes(x=Season, y=Draft, fill=ws, label=round(ws,1))) +
    geom_tile() +
    geom_text() +
    xlab("NBA Season") +
    ylab("Draft Year") +
    scale_y_continuous(breaks=2000:2015) +
    scale_x_continuous(breaks=2001:2016) +
    scale_fill_gradient(low="blue", high="red", name="Win Shares") +
    theme_minimal() +
    ggtitle("Draft Win Shares by Season")


ws_cum_line <- dat %>%
    group_by(Draft, lt_season) %>%
    summarize(ws=sum(WS)) %>%
    mutate(ws=cumsum(ws)) %>%
    ungroup() %>%
    mutate(Draft=as.character(Draft)) %>%
    ggplot(aes(x=lt_season, y=ws, color=Draft)) +
    geom_line() +
    xlab("Year in League") +
    ylab("Win Shares") +
    scale_x_continuous(breaks=1:16) +
    theme_minimal() +
    ggtitle(" Plot 4: Cumulative Draft Win Shares by Year in League Line")

ws_lt_line <- dat %>%
    group_by(Draft, lt_season) %>%
    summarize(ws=sum(WS)) %>%
    ungroup() %>%
    mutate(Draft=as.character(Draft)) %>%
    ggplot(aes(x=lt_season, y=ws, color=Draft)) +
    geom_line() +
    xlab("Year in League") +
    ylab("Win Shares") +
    scale_x_continuous(breaks=1:16) +
    theme_minimal() +
    ggtitle("Plot 2: Draft Win Shares by Year in League Line")

ws_lt_smooth <- dat %>%
    group_by(lt_season) %>%
    summarize(ws=sum(WS)) %>%
    ggplot(aes(x=lt_season, y=ws)) +
    geom_smooth() +
    xlab("Year in League") +
    ylab("Win Shares") +
    scale_x_continuous(breaks=1:16) +
    theme_minimal() +
    ggtitle("Draft Win Shares by Year in League (Smoothed)")

