library(ybp)
library(fishtrackr)
library(tidyverse)
library(lubridate)
d <- all69khz_grouped
head(d)

d <- fishpaths(d, d$TagID, d$Station)

head(d)
d$year <- year(d$DateTagged)

p <- filter(d, TagID == 2841)

p$residence = p$departure - p$arrival
units(p$residence) <- "days"
p <- filter(p, arrival > "2012-04-04 00:00:01" & departure < "2012-04-06 00:00:00")
p$obs <- c(1:5)

ggplot(p, aes(x = arrival, y = Station, group = TagID)) + 
  geom_point(alpha = 0.5, size = 3) +
  geom_point(aes(x = departure, y = Station), color = "green", alpha = 0.5, size = 3) + 
  geom_path(aes(x = departure, group = obs))
