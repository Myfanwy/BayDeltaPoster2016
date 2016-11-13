# Residence at barriers

library(ybp)
library(fishtrackr)
library(tidyverse)
load("data_tidy/fishpaths11-15.Rdata")
head(d3)
d3$residence <- d3$departure - d3$arrival
units(d3$residence) = "hours"


# Residence at Swanston Weir in 2014 - still closed 12/3/14 boards pulled 12/9/14, large culvert opened 12/10/2014
# Only salmon data points available :( - Sturgeon did not visit until Spring '15
sw14bef <- d3 %>% 
  filter(detyear == "2014", 
         Station == "Swanston",
         arrival > "2014-10-01 00:00:00", departure < "2014-12-08 23:00:00")
len(sw14bef$TagID)
len(sw14bef$Sp)
sw14aft <- d2 %>% 
  filter(detyear == "2014", 
         Station == "Swanston",
         arrival > "2014-12-11 00:00:00")
len(sw14aft$TagID)

ggplot(sw14bef) + geom_point(aes(x = arrival, y = as.numeric(residence), color = factor(TagID)), size = 3) + scale_color_viridis(discrete = TRUE, option = "D") + theme_minimal() + geom_vline(xintercept = "2014-12-08 00:00:00") + 
  scale_x_datetime(limits = c(as.POSIXct("2014-11-01 00:00:00"), as.POSIXct("2015-01-01 00:00:00")))

ggplot(sw14aft) + geom_point(aes(x = arrival, y = as.numeric(residence), color = factor(TagID)), size = 3) + scale_color_viridis(discrete = TRUE, option = "D") + theme_minimal() + geom_vline(xintercept = "2014-12-08 00:00:00") + 
  scale_x_datetime(limits = c(as.POSIXct("2014-11-01 00:00:00"), as.POSIXct("2015-01-01 00:00:00")))


sw14 <- d2 %>% 
  filter(detyear == "2014", 
         Station == "Swanston",
         residence < 150,
         arrival < "2015-02-01")


str(sw14)
lim1 = as.Date("2014-11-01", format="%Y-%m-%d")
lim2 = as.Date("2015-01-01")

ggplot(sw14) + geom_point(aes(x = arrival, y = as.numeric(residence), color = factor(TagID)), size = 3) + scale_color_viridis(discrete = TRUE, option = "D") + theme_minimal() + geom_vline(xintercept = "2014-12-08 00:00:00") + 
  scale_x_datetime(limits = c(as.POSIXct("2014-11-01 00:00:00"), as.POSIXct("2015-01-01 00:00:00")))

# Basically we don't have any data.  No salmon arrived after the gates were open.

# Swanson 2015 - don't have exact dates but they started catching lots of salmon in ww fyke on 12/7/15
s15b <- d3 %>% 
  filter(detyear == "2015",
         Station == "Swanston",
         arrival < "2015-12-05 00:00:00")
len(s15b$TagID)

summary(as.numeric(s15b$residence))

s15a <- d3 %>% 
  filter(detyear == "2015",
         Station == "Swanston",
         departure > "2015-12-05 00:00:00")

s15a %>% 
  group_by(Sp, TagID) %>% 
  ggplot(aes(x = Sp, y = residence)) + geom_boxplot(alpha = 0.5) + geom_jitter(aes(color = Sp), alpha = 0.5)

sw15 <- d3 %>% 
  filter(detyear == "2015",
         TagID == 37835,
  Station == "Swanston")

ggplot(sw15) + geom_segment(aes(x=arrival, xend=departure, y=factor(TagID), yend=factor(TagID), color = factor(TagID)), alpha = 0.8,
                         size = 8) + scale_color_viridis(discrete = TRUE, option = "D") + theme_minimal()


## Try again with lisbon, just summarizing the differences between Species

lis <- d3 %>% 
  filter(Station == "Lisbon", residence !=0, residence < 50, detyear != "2011")

lis$month <- month(lis$arrival)

lis$season <- ifelse(lis$month == 9 | lis$month == 10, "early", ifelse(
  lis$month == 11 | lis$month == 12, "late", "other")
  )

lis <- filter(lis, season != "other")
ggplot(lis, aes(x = season, y = as.numeric(residence), color = Sp)) + 
  geom_boxplot(alpha = 0.4, width = 1) +
  geom_jitter(size = 3, alpha = 0.3, width = 0.75) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  facet_wrap(~detyear)

# So we basically have no data for sturgeon in the upper Bypass before like, January, ever

