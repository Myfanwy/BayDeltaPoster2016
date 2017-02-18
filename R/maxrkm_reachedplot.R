library(tidyverse)
library(dplyr)
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

rkmplot + theme(text = element_text(size = 20),
                axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                plot.title = element_text(hjust = 0.5),
                legend.position = "none")

ggsave(filename = "figures/maxrkmplot.jpg", width = 9, height = 4, units = "in")

head(maxsum)

ggplot(maxsum, aes(x = maxrkm_reached)) + geom_density(aes(color = Sp)) + facet_wrap(~detyear)

## For IEP ##
library(ggalt)
ms <- filter(maxsum, Sp == "wst")
head(ms)
ggplot(ms, aes(x = factor(maxrkm_reached))) +
  geom_bar(stat = 'count') +
  facet_wrap(~detyear)

ggplot(ms, aes(x = detyear, y = maxrkm_reached)) +
  geom_boxplot( alpha = 0.6) +
  geom_jitter(aes(color = detyear), alpha = 0.8, size = 2, width = 0.1)

ms2 <- filter(maxrkms, Sp == 'wst')

ms2 <- ms2 %>% 
  group_by(detyear, TagID) %>% 
  filter(!duplicated(Rkm))

ggplot(ms, aes(x = factor(maxrkm_reached))) + 
  geom_density(aes(color = detyear), stat = 'count') +
  facet_wrap(~detyear)

ggplot(ms, aes(x = factor(maxrkm_reached))) +
  geom_bar(aes(fill = detyear)) +
  scale_fill_viridis(discrete = TRUE, option = "C") +
  facet_wrap(~detyear, nrow = 1) +
  coord_flip()

ggplot(ms, aes(x = detyear, y = maxrkm_reached)) +
  geom_dotplot(aes(fill = detyear), binaxis = "y", stackdir = "centerwhole", 
               binwidth = 1, stackratio = 0.5, alpha = 0.9, show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) 

library(ggbeeswarm)
ggplot(ms, aes(x = Sp, y = factor(maxrkm_reached), color = detyear)) +
  geom_beeswarm(cex = 3.5) +
  scale_color_viridis(discrete = TRUE, option = 'D') +
  theme_minimal() +
  facet_wrap(~detyear, nrow = 1) + theme(legend.position = 'none')

ggplot(ms, aes(x = Sp, y = factor(maxrkm_reached), color = detyear)) +
  geom_quasirandom(cex = 3.5, method = 'tukeyDense') +
  scale_color_viridis(discrete = TRUE, option = 'D') +
  theme_minimal() +
  facet_wrap(~detyear, nrow = 1) + theme(legend.position = 'none')

library(ggforce)

ggplot(ms, aes(x = detyear, y = maxrkm_reached, color = detyear)) +
  geom_sina(size = 0.5) 
  facet_wrap(~detyear)
  

# See if same fish are reaching same river kms each year
  
head(maxrkms)
g <- ms$detyear
comtags <- split(ms$TagID, g )
coms <- Reduce(intersect, comtags)

coms <- filter(ms, TagID %in% coms)
head(coms)


ggplot(coms, aes(x = detyear, y = maxrkm_reached)) +
  geom_point(aes(color = factor(TagID))) +
  geom_path(aes(color = factor(TagID), group = 1)) +
  facet_grid(~factor(TagID))

coms2 <- Reduce(intersect, comtags[2:5])
coms2 <- filter(ms, TagID %in% coms2)


yup <- unlist(comtags[2:5])

names(yup) <- NULL
yup <- yup[!duplicated(yup)]
yup <- filter(ms, TagID %in% yup)

# Find common fish in each year:
pair_intersect <- outer(comtags, comtags, Vectorize(intersect))
pair_intersect
count <- matrix(lengths(pair_intersect), nrow = length(comtags),
                dimnames = dimnames(pair_intersect))
count

# to find tags that appear at least twice, we just need to find and filter out the tags that appear only once:

## First get a table of the number of times each fish occurs
tt = table(unlist(comtags))

## The ones to exclude have a value == 1
oneFish = names(tt)[tt == 1]

## Loop over the list, exclude the tags that are in oneFish
ans = lapply(comtags, function(x) {
  x[!(x %in% oneFish)]
})

## Sanity checking
sapply(comtags, length)
sapply(comtags, function(x) sum(!x %in% oneFish))
sapply(ans, length)
## Should not be any < 1
table(unlist(ans))


onefish <- as.numeric(oneFish)
yup <- filter(ms, !(TagID %in% onefish))
ggplot(yup, aes(x = detyear, y = maxrkm_reached)) +
  geom_point(aes(color = factor(TagID))) +
  geom_path(aes(color = factor(TagID), group = TagID)) +
  facet_wrap(~factor(TagID)) +
  labs(title = 'Maximum River Kilometer Reached in Consecutive Years of Returns') +
  theme(legend.position = 'none') +
  scale_x_discrete(labels = c("1", "2", "3", "4", "5"))


  coms1 <- Reduce(intersect, comtags[4:5])
coms345 <- Reduce(intersect, c(list(coms1), comtags[3])) 
coms2345 <- Reduce(intersect, c(list(coms345), comtags[2]))

chk <- Reduce(intersect, comtags[2:5])
identical(coms2345, chk)









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

