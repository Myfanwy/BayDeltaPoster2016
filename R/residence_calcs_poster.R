#residence calcs
detach("package:rethinking", unload = TRUE)
library(tidyverse)
library(ybp)
library(fishtrackr)
library(lubridate)
library(viridis)
library(beepr)

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

# Save d3 - this is fishpaths for all years, with detyear added, shed tag IDs removed, no false dets 
save(d3, file = "data_tidy/fishpaths11-15.Rdata")

# Residence Time for Each fish, by year:
load(file = "data_tidy/fishpaths11-15.Rdata")
head(d3)
d3$residence <- d3$departure - d3$arrival
units(d3$residence) = "days"
library(dplyr)
res <- d3 %>% 
  group_by(detyear, TagID, Sp) %>% 
  summarise(firstarrival = min(arrival), lastdeparture = max(departure)) %>% 
  mutate(totalres = lastdeparture - firstarrival) 

units(res$totalres) <- "days"
res$totalres <- as.numeric(res$totalres)

res <- filter(res, TagID != 13728) # mortality

res$detyear <- factor(res$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))
res <- filter(res, detyear != "2011 - 2012")

resplot <- ggplot(res, aes(x = Sp, y = totalres)) + 
  geom_violin(alpha = 0.4, scale = "count") + 
  geom_jitter(aes(color = Sp), size = 3, width= 0.5,    alpha = 0.7) + 
  scale_color_viridis(discrete = TRUE, option = "D") +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "Residence in Days", title = "Residence Time by Species and Year")

resplot + theme(text = element_text(size = 18),
                axis.text.x = element_blank(), axis.ticks = element_blank(),
                 plot.title = element_text(hjust = 0.5),
                 legend.position = "none")

ggsave(filename = "figures/resplot.jpg", width = 8, height = 5, units = "in")


# Summary table for poster
restab <- res %>% 
  group_by(detyear, Sp) %>% 
  summarise(meanres = mean(totalres), sdres = sd(totalres))

write.csv(restab, file = "data_tidy/restab.csv")

# begin modeling
head(res)
d1 <- filter(res, detyear != "2011", totalres > 0)
detach("package:dplyr", unload = TRUE)
library(rethinking)  

d1$species <- ifelse(d1$Sp == "chn", 1, 0)

dens(d1$totalres) # See shape of raw data - skewed

# compile relevant predictors/variables into list for running models


d1 <- as.data.frame(d1)
head(d1)
#d1$totalres <- round(d1$totalres)
range(d1$totalres)


# Trying with normal distributions
d1 <- dplyr::select(d1, detyear, TagID, Sp, species, totalres)
head(d1)
save(d1, file = "data_raw/totalres.Rdata")

chn_model <- map(
  alist(
  totalres ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a + bSp*species,
  a ~ dnorm(0, 100) ,
  bSp ~ dnorm(0, 50),
  sigma ~ dcauchy(0,25)
), data = d1, start = list(a = 50, sigma = 20))

intercept_model <- map(
  alist(
    totalres ~ dnorm(mean = mu, sd = sigma) ,
    mu <- a,
    a ~ dnorm(0, 100) ,
    sigma ~ dcauchy(0, 25)
  ), data = d1, start = list(a = 50, sigma = 20)
)

precis(m2)
precis(m)
compare(intercept_model, chn_model)

