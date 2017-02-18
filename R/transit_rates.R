# Transit Speed
library(tidyverse)
library(ybp)
library(fishtrackr)
library(lubridate)
library(viridis)
library(beepr)

d <- all69khz_grouped
head(d)
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
d2 <- d %>% 
  group_by(TagID) %>% 
  filter(DateTimeUTC >= DateTagged) %>% 
  arrange(DateTimeUTC)

# filter for shed tags

d3 <- fishpaths(d2, d2$TagID, d2$Station)
beep()
d3$residence <- d3$departure - d3$arrival
units(d3$residence) = "days"
sheds <- filter(d3, residence > 7) # filter out encounters equal to or longer than 7 consecutive days

len(sheds$TagID)

# Now filter out those long-res IDs:
d3 <- filter(d2, !(TagID %in% sheds$TagID))
len(d3$TagID) # should be 221-6 = 215

# now call fishpaths on d3, add detyear later:
d3 <- fishpaths(d3, d3$TagID, d3$Station)
beep(2)
head(d3)
dups <- filter(d3, duplicated(DateTimeUTC)) # all duplicate values are at BC_joint2
duparrs <- filter(d3, duplicated(arrival))
identical(dups, duparrs) # same ones

d3 <- d3 %>% 
  arrange(arrival) %>% 
  mutate(detyear = 
           ifelse(
             departure > "2011-07-01 00:00:00" & departure < "2012-06-30 00:00:00", "2011",
             ifelse(
               departure > "2012-07-01 00:00:00" & departure < "2013-06-30 00:00:00", "2012",
               ifelse(
                 departure > "2013-07-01 00:00:00" & .$departure < "2014-06-30 00:00:00", "2013",
                 ifelse(
                   departure > "2014-07-01 00:00:00" & departure < "2015-06-30 00:00:00", "2014", "2015"
                 )
               )
             )
           ) )
head(select(d3, arrival, departure, detyear))
tail(select(d3, arrival, departure, detyear))

# Calculate transit time
# or just 

## Try on whole df:
load("data_tidy/fishpaths11-15.Rdata")
d3$residence <- d3$departure - d3$arrival
units(d3$residence) <- "days"
library(dplyr)

dp <- d3 %>% 
  filter(residence != 0) %>% # filter out single detections and BC_joint
  filter(!duplicated(DateTimeUTC)) %>% 
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  mutate(transit_time = difftime(lead(arrival), departure, units = "hours")) %>% 
  ungroup()
range(dp$transit_time, na.rm = TRUE)

dp %>% 
  select(detyear, TagID, arrival, departure, Station, Rkm, transit_time) %>% 
  group_by(TagID) %>% 
  arrange(transit_time) %>% 
  head(.)

chk <- filter(d3, TagID == 13642, detyear == 2014)

dp$transit_time <- as.numeric(dp$transit_time)
range(dp$transit_time, na.rm = TRUE)


# Filtering criteria: remove those transit_times that are greater than 50 days.

# make grouping ids and slice rows by them
dp <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(arrival) %>% 
  filter(transit_time < 50*24, transit_time > 0) %>% # filters out transits greater than 50 days
  mutate(move_num = cumsum(!duplicated(Station))) %>%
  ungroup() %>% 
  group_by(detyear, TagID, move_num) %>% 
  slice(length(move_num)) %>% 
  ungroup()
range(dp$transit_time) # good enough for first poster

head(dp)


dp2 <- dp %>% 
  group_by(detyear, TagID) %>% 
  arrange(move_num) %>% 
  mutate(diffRkm = abs(lead(Rkm) - Rkm),
         RkmAdj = diffRkm + 0.4, # 0.4 for the detection range adjustment (very generous adjustment)
         rate = RkmAdj/transit_time) %>% 
  ungroup %>% 
  filter(rate < 10)
range(dp2$rate, na.rm = TRUE)

dp2 %>% 
  select(TagID, arrival, departure, Station, RkmAdj, rate, Sp) %>% 
  arrange(-rate) %>% 
  head(.)

dp2$detyear <- factor(dp2$detyear, labels = c(  "2011 - 2012",
                                              "2012 - 2013",
                                              "2013 - 2014",
                                              "2014 - 2015",
                                              "2015 - 2016"))
dp2 <- filter(dp2, detyear != "2011 - 2012")
rateplot <- ggplot(dp2, aes(x = Sp, y = rate)) + 
  geom_violin(scale = "count", alpha = 0.6) +
  geom_jitter(aes(color = Sp), size = 2.5, width= 0.3,    alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  facet_wrap(~detyear, nrow = 1, labeller = label_value)

# Poster Plot -------------------------------------------------------------

rateplot <- rateplot + labs(x = "", y = "Movement Rate (km/hr)", title = "Movement Rate by Species and Year")

rateplot + theme( text = element_text(size = 20),
              axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.position = "none")

ggsave(filename = "figures/rateplot.jpg", width = 9, height = 4, units = "in")

# - ------- Individual year plots ------------------------------------
ggplot(dp2) +
  geom_density(aes(x = rate, color = Sp)) + facet_wrap(~detyear, scales = "free_x")

# begin modeling 

detach("package:dplyr", unload=TRUE)
library(rethinking) 
d1 <- dp2
d1$species <- ifelse(d1$Sp == "chn", 1, 0)
d1 <- as.data.frame(d1)
head(d1)
range(d1$rate)

summary(lm(rate ~ Sp, data=d1)) # sanity check

chn_rate <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a + bSp*species,
  a ~ dnorm(0, 10) ,
  bSp ~ dnorm(0, 1),
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1 )

precis(chn_rate, prob = 0.95) # shows an increase of 0.53km/hour for chinook than for white sturgeon.  But that's just in 2013, when there were many more chn than white sturgeon.


m1int <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a,
  a ~ dnorm(0, 10) ,
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1 )


compare(chn_rate, m1int)

# summarise movement rates:
library(dplyr)
dp2 %>% 
  group_by(Sp) %>% 
  filter(rate > 0) %>% 
  summarise(maxrate = max(rate), minrate = min(rate), meanrate = mean(rate), sdrate = sd(rate))
