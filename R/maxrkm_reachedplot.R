library(tidyverse)
library(ybp)
library(fishtrackr)
library(lubridate)
library(viridis)
library(beepr)

d <- all69khz_grouped

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

# Find max rkm reached for each fish

maxrkms <- d2 %>% 
  group_by(detyear, TagID) %>% 
  mutate(maxrkm = max(Rkm)) %>% 
  ungroup()

head(maxrkms)

maxrkms$detyear <- factor(maxrkms$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))
maxrkms <- filter(maxrkms, detyear != "2011 - 2012")

maxsum <- maxrkms %>% 
  group_by(detyear, Sp, TagID) %>% 
  filter(!duplicated(TagID)) %>% 
  summarise(maxrkm_reached = maxrkm)

rkmplot <- ggplot(maxsum, aes(x = Sp, y = maxrkm_reached)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(aes(color = Sp), size = 3, width= 0.5,    alpha = 0.65) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "River Kilometer", title = "Maximum River Kilometer Reached")

rkmplot + theme(text = element_text(size = 18),
                axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.position = "none")

ggsave(filename = "figures/maxrkmplot.jpg", width = 8, height = 5, units = "in")

head(maxsum)

ggplot(maxsum, aes(x = maxrkm_reached)) + geom_density(aes(color = Sp)) + facet_wrap(~detyear)

# Begin modeling #-----------------------------
detach("package:tidyverse", unload=TRUE)
detach("package:dplyr", unload=TRUE)
detach("package:purrr", unload = TRUE)

library(rethinking)
d1 <- maxsum
d1$dSp <- ifelse(d1$Sp == "chn", 1, 0)
d1 <- as.data.frame(d1)
head(d1)

# --  Before running models: need to ask Matt how to handle data constratined above 105?  Center?

m1a <- map(flist = alist(
  maxrkm_reached ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a + bSp*dSp,
  a ~ dnorm(0, 10) ,
  bSp ~ dnorm(0, 1),
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1 )

precis(m1a) # shows an increase of 0.53km/hour for chinook than for white sturgeon.  But that's just in 2013, when there were many more chn than white sturgeon.

m1int <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a,
  a ~ dnorm(0, 10) ,
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1 )


compare(m1a, m1int)

