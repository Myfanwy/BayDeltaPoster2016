# Circular bar plot

library(ggplot2)

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
           )
  )

# filter for false dets
d2 <- d2 %>% 
  group_by(TagID) %>% 
  filter(DateTimeUTC >= DateTagged) %>% 
  arrange(DateTimeUTC)

d3 <- d2 %>% 
  group_by(detyear, Sp) %>% 
  summarise(mindet = min(DateTimeUTC), maxdet = max(DateTimeUTC))
head(d3)

d3$resdays = d3$maxdet - d3$mindet
ggplot(d3, aes(x = detyear, y = as.numeric(resdays), group = Sp)) + geom_line(aes(color = Sp))

d3$percyear <- (as.numeric(d3$resdays)/365.25)*100

ggplot(d3, aes(x = detyear, y = percyear ,fill = Sp)) + 
  geom_bar(width = 0.75, stat="identity") +    
  
  # To use a polar plot and not a basic barplot
  coord_polar(theta = "y", start = 3) +    
  
  #Remove useless labels of axis
  xlab("") + ylab("")  + ylim(c(0, 100)) +
  
  # facet by detyear
  facet_wrap(~Sp) + scale_fill_manual(values = viridis(2, option = "C", alpha = 0.8))

# Do Separately to get the offsets correctly aligned with the year
library(ggthemes)
monthsabb <- month.abb
breaks <- seq(0, 100, by = 8.34)

d3 %>% 
  filter(Sp == "chn", detyear != "2011") %>% 
  ggplot(aes(x = detyear, y = percyear, fill = Sp)) + 
  geom_bar(width = 0.75, stat="identity", alpha = 0.8) +    
    coord_polar(theta = "y", start = 3) +    
    xlab("") + ylab("")  + ylim(c(0, 100)) +
    scale_fill_manual(values = "#0D0887FF") + ggtitle("Chinook Salmon Residence By Year") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none")

test = data.frame(months = 1:12, values = runif(12))
test

ggplot(test, aes(x = months, y = values)) + geom_bar(width = 0.75, stat = "identity", alpha = 0.8) + 
  coord_polar(theta = "y")
