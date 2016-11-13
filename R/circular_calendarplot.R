# Circular bar plot
library(ybp)
library(fishtrackr)
library(viridis)
library(tidyverse)
library(beepr)
# data has to look like: group = chn or wst, value = percentage of the year that they're present.

# most white sturgeon present from late October - early April.

# wst
julian(as.Date("2016-10-25"), origin = as.Date("2016-01-01")) #298
julian(as.Date("2016-04-08"), origin = as.Date("2016-01-01")) #98

298-98 # missing for 200 days out of the summer
365.25-298 # 67.25

67.25+98 # present for 165.25 days

165.25/365.25 # present for 45% of the year

#chn
julian(as.Date("2016-01-08"), origin = as.Date("2016-01-01")) #7
julian(as.Date("2016-09-05"), origin = as.Date("2016-01-01")) #98

365.25-248 # present for 117.25 + 7 days = 124.25 days
124.25/365.25

# present for 34% of the year

d <- data.frame(group = c("chn", "wst"), percentage = c(34, 45))

ggplot(d, aes(x = group, y = percentage ,fill = group)) + 
  geom_bar(width = 0.85, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y") +    
  
  #Remove useless labels of axis
  xlab("") + ylab("")  + ylim(c(0, 100))

# Replace with Real Data:
library(lubridate)
d <- all69khz_grouped

# add detyear column
d2 <- d %>% 
  arrange(DateTimeUTC) %>% 
  mutate(detyear = 
           ifelse(
             DateTimeUTC > "2011-07-01 00:00:00" & DateTimeUTC < "2012-06-30 00:00:00", "2011",
             ifelse(
               DateTimeUTC > "2012-07-01 00:00:00" & DateTimeUTC < "2013-06-30 00:00:00", "2012",
               ifelse(
                 DateTimeUTC > "2013-07-01 00:00:00" & .$DateTimeUTC < "2014-06-30 00:00:00", "2013",
                 ifelse(
                   DateTimeUTC > "2014-07-01 00:00:00" & DateTimeUTC < "2015-06-30 00:00:00", "2014", "2015"
                 )
               )
             )
           ) )

# filter for false dets
d2 <- d2 %>% 
  group_by(TagID) %>% 
  filter(DateTimeUTC >= DateTagged) %>% 
  arrange(DateTimeUTC)

# filter for shed tags

d3 <- fishpaths(d2, d2$TagID, d2$Station)
d3$residence <- d3$departure - d3$arrival
units(d3$residence) = "days"
sheds <- filter(d3, residence > 7) # filter out encounters equal to or longer than 7 consecutive days
len(sheds$TagID)

d3 <- filter(d2, !(TagID %in% sheds$TagID))
len(d3$TagID) # should be 221-6 = 215

d3 <- d3 %>% 
  group_by(detyear, Sp) %>% 
  summarise(mindet = min(DateTimeUTC), maxdet = max(DateTimeUTC))
head(d3)

d3$resdays = d3$maxdet - d3$mindet
d3$percyear <- (as.numeric(d3$resdays)/365.25)*100
d3 <- filter(d3, detyear != "2011") # don't count the spring of 2012


# Still not what we want.  Back to ggplot2:
d3$detyear <- factor(d3$detyear, labels = c(  "2012 - 2013",
                                                      "2013 - 2014",
                                                      "2014 - 2015",
                                                      "2015 - 2016"))
calplot <- d3 %>% 
  ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=9,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b-%y") +
  facet_wrap(~detyear, nrow = 4, scales = "free_x", labeller = label_value) +
  labs(x = "", y = "", title = "Seasonal Residence, Fall 2012 - Spring 2016")

# Poster Plot -------------------------------------------------------------


calplot + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                plot.title = element_text(hjust = 0.5),
                text = element_text(size = 20),
                axis.text.x = element_text(size = 14),
                axis.title.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.position = "none")

ggsave(filename = "figures/calendarplot.jpg", width = 8, height = 10, units = "in")
# - ------- Individual year plots ------------------------------------
d3 %>% 
  filter(detyear == "2012") %>% 
ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=8,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%y", limits = c(as.POSIXct("2012-07-01"),
                                                                               as.POSIXct("2013-06-01"))) 
d3 %>% 
  filter(detyear == "2012") %>% 
  ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=8,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2012-07-01"),as.POSIXct("2013-06-01"))) +
  theme(axis.text.y = element_blank(),
             axis.ticks.y = element_blank()) +
  labs(x = "Month", title = "2012 - 2013", y = " ")

d3 %>% 
  filter(detyear == "2013") %>% 
  ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=8,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2013-07-01"), as.POSIXct("2014-06-01"))) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Month", title = "2013 - 2014", y = " ")

d3 %>% 
  filter(detyear == "2014") %>% 
  ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=8,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2014-07-01"), as.POSIXct("2015-06-01"))) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Month", title = "2014 - 2015", y = " ")

d3 %>% 
  filter(detyear == "2015") %>% 
  ggplot(.) +
  geom_segment(aes(x=mindet, xend=maxdet, y=Sp, yend=Sp, color = Sp), size=8,
               lineend = "round", show.legend = FALSE) + 
  scale_color_viridis(discrete = TRUE, option = "D") + 
  scale_x_datetime(date_breaks = "2 months", date_labels = "%b-%y", 
                   limits = c(as.POSIXct("2015-07-01"), as.POSIXct("2016-06-01"))) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(x = "Month", title = "2015 - 2016", y = " ")


# One last shot at the calendar heatmap in case it's better


d2$day <- as.Date(d2$DateTimeUTC)
densmap <- transform(d2,
                     week = as.POSIXlt(day)$yday %/% 7 + 1,
                     wday = as.POSIXlt(day)$wday,
                     year = as.POSIXlt(day)$year + 1900)

# plot by species
library(scales)

densmap <- filter(densmap, detyear != "2011")
densmap$x <- as.Date(densmap$week, origin = "2011-01-01") # does not work
densmap$x <- as.POSIXct(densmap$x)
densmap$detyear <- factor(densmap$detyear, labels = c("2012-2013",
                                                      "2013-2014",
                                                      "2014-2015",
                                                      "2015-2016"))
p <- ggplot(densmap, aes(x = x, y = wday, fill = Sp)) + 
  
  geom_tile(colour = "white", alpha = 0.5) + 
  
  scale_fill_manual(values = c("blue", "yellow")) +
  
  facet_wrap(~detyear, ncol = 1, labeller = label_value, scales = "free_x")

p + labs(title = "Presence in the Yolo Bypass By Species and Detection Year", x = " ",
         y = "Weekday") + theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank())

p + scale_x_datetime(date_breaks = "1 month", labels = date_format("%b"))

# plot by number of fish in the system:

ggplot(densmap, aes(x = week, y = wday, group = Sp, fill = nfish)) + 
  
  geom_tile(aes(color = Sp), colour = "white", alpha = 0.6) + 
  
  #  scale_fill_viridis() +
  
  facet_wrap(~detyear, ncol = 1, labeller = label_value )
