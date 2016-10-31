# Residence at barriers

library(ybp)
library(fishtrackr)
library(tidyverse)

a <- alltags
groups <- split(a, a$`Tagging Group`)
g <- map(groups, select, TagID) # selects only the TagID column of each dataframe
tags <- unlist(g, recursive = FALSE) # have to unlist the first level to be able to manipulate with these as vectors

d <- all69khz_grouped # all detections, with grouped gated receivers

d <- filter(d, TagID %in% tags$wst_2014.TagID)
d <- filter(d, DateTimeUTC > "2014-07-01", DateTimeUTC < "2015-06-30") # d now contains white sturgeon in detyear 2014

d2 <- fishpaths(d, d$TagID, d$Station)

# want to compare residence at each station
d2$residence = d2$departure - d2$arrival
units(d2$residence) <- "hours"

resbystation <- d2 %>% 
  filter(residence < 150) %>% 
  group_by(TagID, Station) %>% 
  mutate(meanres = as.numeric(mean(residence))) %>% 
  ungroup() %>% 
  ggplot(aes(x = Station, y = meanres)) + geom_boxplot(aes(color = Station)) + geom_jitter(aes(color = Station), alpha = 0.5, width = 0.5) + theme(legend.position = "none") + labs(title = "White Sturgeon Mean Residency in Hours by Station")

resbystation

# chn
d <- all69khz_grouped # all detections, with grouped gated receivers

d <- filter(d, TagID %in% tags$fca_2014.TagID)
d <- filter(d, DateTimeUTC > "2014-07-01", DateTimeUTC < "2015-06-30") # d now contains white sturgeon in detyear 2014

d2 <- fishpaths(d, d$TagID, d$Station)

# want to compare residence at each station
d2$residence = d2$departure - d2$arrival
units(d2$residence) <- "hours"

resbystation_chn <- d2 %>% 
  filter(residence < 150) %>% 
  group_by(TagID, Station) %>% 
  mutate(meanres = as.numeric(mean(residence))) %>% 
  ungroup() %>% 
  ggplot(aes(x = Station, y = meanres)) + geom_boxplot(aes(color = Station)) + geom_jitter(aes(color = Station), alpha = 0.5, width = 0.5) + theme(legend.position = "none") + labs(title = "Chinook Salmon Mean Residency in Hours by Station")

resbystation_chn

par(mfrow = c(2, 1))
resbystation
resbystation_chn

# Need to facet this by station for each fish.
range(d2$residence)
d2 %>% 
  filter(residence < 150, residence > 0, Station == "Lisbon" | Station == "Swanston") %>% 
  ggplot(aes(x = Station, y = residence)) + geom_boxplot(aes(color = factor(TagID)), alpha = 0.5) + geom_jitter(aes(color = factor(TagID)), alpha = 0.5, size = 2) + facet_wrap(~TagID) + labs(title = "Residence Time (hrs) of Chinook at locations with barriers", x = "2014")

# re-run with wst data
d2 %>% 
  filter(residence < 150, residence > 0, Station == "Lisbon" | Station == "Swanston") %>% 
  ggplot(aes(x = Station, y = residence)) + geom_boxplot(aes(color = factor(TagID)), alpha = 0.5) + geom_jitter(aes(color = factor(TagID)), alpha = 0.5, size = 2) + facet_wrap(~TagID) + labs(title = "Residence Time (hrs) of White Sturgeon at locations with barriers", x = "2014")

# Both Species
d <- all69khz_grouped # all detections, with grouped gated receivers

d <- filter(d, TagID %in% tags$wst_2014.TagID | TagID %in% tags$fca_2014.TagID)
len(d$TagID)

d <- filter(d, DateTimeUTC > "2014-07-01", DateTimeUTC < "2015-06-30") # d now contains white sturgeon in detyear 2014

d2 <- fishpaths(d, d$TagID, d$Station)
d2$residence = d2$departure - d2$arrival
units(d2$residence) <- "hours"


d2 %>% 
  filter(residence < 150, residence > 0, Station == "Lisbon" | Station == "Swanston") %>% 
  ggplot(aes(x = Station, y = residence)) + geom_boxplot(aes(color = Station), alpha = 0.5) + geom_jitter(aes(color = factor(TagID)), alpha = 0.5, size = 2) + facet_wrap(~Sp) + labs(title = "Residence Time (hrs) of Chinook and White Sturgeon at locations with barriers")

# spot=checking ID46640:

w46440 <- d2 %>% 
  filter(residence < 150, residence > 0, TagID == 46640)
