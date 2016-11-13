# Arrivals at monitors in groups

load("data_tidy/fishpaths11-15.Rdata")
head(d3)

d3$hour_arr <- hour(d3$arrival)
d3$day <- as.Date(d3$arrival)


grp <- d3 %>% 
  group_by(detyear, Station, Sp, day, hour_arr) %>% 
  summarise(grps = n()) %>% 
  filter(grps > 2)

head(grp)
tail(grp)
range(grp$grps)

ggplot(grp, aes(x = Station, y = factor(grps))) + geom_point(aes(color = Sp), position = "dodge") + facet_wrap(~detyear)

# might be something there, but have to filter for the detections on the day they were tagged