#load("../data_tidy/fishpaths11-15.Rdata") #generates object d3; this is just all the Yolo Bypass detections for all tagged fish through fall chn 2015
library(lubridate)
library(dplyr)
## filter out tag day detections #-------------
d3 <- all69khz_grouped
head(d3)
d3$DateTagged_hms <- "01:00:00"
d3$Datepaste <- format(as.POSIXct(paste(d3$DateTagged, d3$DateTagged_hms)), "%Y-%m-%d %H:%M:%S")
d3$DateTagged_full <- ymd_hms(d3$Datepaste)
d3$DateTagged <- d3$DateTagged_full

# filter out the detections that happened the day they were tagged
f2 <- d3 %>% 
  group_by(TagID) %>%
  mutate(tagdetthreshold = DateTagged + 60*60*24) %>% 
  filter(DateTimeUTC > tagdetthreshold) #3600 less detections
#----------------------------------------------------
f <- select(f2, -DateTagged_full, -Datepaste, -DateTagged_hms, -tagdetthreshold) # get back to the columns we want
head(f)
# Can now run fishpaths function
f <- fishpaths(f, f$TagID, f$Station)
head(f)
# fix time zones of arrival and departure columns
f <- f %>% 
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
f$arrival <- format(f$arrival, tz="Pacific/Pitcairn", usetz=TRUE)
f$departure <- format(f$departure, tz="Pacific/Pitcairn", usetz=TRUE)
f$hour_arr <- hour(f$arrival)
f$hour_dep <- hour(f$departure)

f$detyear <- factor(f$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))

chn_deps <- f %>% 
  filter(Sp == "chn") %>% 
  group_by(detyear, hour_dep, Station) %>% 
  tally()

chn_arrs <- f %>% 
  filter(Sp == "chn") %>% 
  group_by(detyear, hour_arr, Station) %>% 
  tally()


chn_arrivals <- ggplot(chn_arrs, aes(x = hour_arr, y = n)) + 
  geom_bar(stat = "identity", fill = "#440154FF") + 
  coord_polar(start = 0) + 
  ylim(c(0, 125)) +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = NULL, y = NULL)

chn_arrivals <- chn_arrivals + theme(text = element_text(size = 24),
                     plot.title = element_text(hjust = 0.5),
                     axis.text.x = element_text(size = 26),
                     legend.position = "none",
                     plot.margin = unit( c(0,0,0,0), "in" ),
                     panel.spacing = unit(0.25, "in"))

chn_arrivals

ggsave(chn_arrivals, filename = "figures/chnarrs_plot.jpg", width = 12, units = "in", limitsize = FALSE)

chn_departures <- ggplot(chn_deps, aes(x = hour_dep, y = n)) + 
  geom_bar(stat = "identity", fill = "#440154FF") + 
  coord_polar(start = 0) +
  ylim(c(0, 125)) +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

chn_departures <- chn_departures + theme(text = element_text(size = 24),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "none",
                       axis.text.x = element_text(size = 26),
                       plot.margin = unit( c(0,0,0,0), "in" ),
                       panel.spacing = unit(0.25, "in"))

ggsave(chn_departures, filename = "figures/chndeps_plot.jpg", width = 12, units = "in", limitsize = FALSE)


# wst ----------------------------------
wst_deps <- f %>% 
  filter(Sp == "wst", detyear != "2011 - 2012") %>% 
  group_by(detyear, hour_dep, Station) %>% 
  tally()

wst_arrs <- f %>% 
  filter(Sp == "wst", detyear != "2011 - 2012") %>% 
  group_by(detyear, hour_arr, Station) %>% 
  tally()


wst_arrivals <- ggplot(wst_arrs, aes(x = hour_arr, y = n)) + 
  geom_bar(stat = "identity", fill = "#FDE725FF") + 
  coord_polar(start = 0) + 
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

wst_arrivals <- wst_arrivals + theme(text = element_text(size = 24),
                                     axis.text.x = element_text(size = 26),
                     plot.title = element_text(hjust = 0.5),
                     legend.position = "none",
                     plot.margin = unit( c(0,0,0,0), "in" ),
                     panel.spacing = unit(0.25, "in"))

ggsave(wst_arrivals, filename = "figures/wstarrs_plot.jpg", width = 12, units = "in", limitsize = FALSE)



wst_departures <- ggplot(wst_deps, aes(x = hour_dep, y = n)) + 
  geom_bar(stat = "identity", fill = "#FDE725FF") + 
  coord_polar(start = 0) +
  ylim(c(0, 125)) +
  facet_wrap(~detyear, nrow = 1, labeller = label_value) +
  labs(x = "", y = "")

wst_departures <- wst_departures + theme(text = element_text(size = 24),
                                         axis.text.x = element_text(size = 26),
                       plot.title = element_text(hjust = 0.5),
                       legend.position = "none",
                       plot.margin = unit( c(0,0,0,0), "in" ),
                       panel.spacing = unit(0.25, "in"))

ggsave(wst_departures, filename = "figures/wstdeps_plot.jpg", width = 12, units = "in", limitsize = FALSE)


# do white sturgeon just have more arrivals/departures?
head(f)
comps <- f %>%  # run the model on this one
  group_by(Sp, detyear) %>% 
  mutate(nfish = len(TagID)) %>% 
  filter(!duplicated(detyear)) %>% 
  select(detyear, Sp, nfish)
comps

dets <- f %>%
  filter(detyear != "2011 - 2012") %>% 
  group_by(TagID, Sp) %>% 
  summarise(narrs = n())


# -------------------- begin modeling 
detach("package:dplyr", unload = TRUE)
library(rethinking)  
d1 <- dets
d1$dSp <- ifelse(d1$Sp == "chn", 1, 0)
d1 <- as.data.frame(d1)
range(d1$narrs)
str(d1)
head(d1)

chn_model <- map(
  alist(
    narrs ~ dnorm(mean = mu, sd = sigma) ,
    mu <- a + bSp*dSp,
    a ~ dnorm(0, 100) ,
    bSp ~ dnorm(0, 50),
    sigma ~ dcauchy(0,25)
  ), data = d1, start = list(a = 50, sigma = 30))

precis(chn_model, prob = 0.95)

intc_model <- map(
  alist(
    narrs ~ dnorm(mean = mu, sd = sigma) ,
    mu <- a,
    a ~ dnorm(0, 100) ,
    sigma ~ dcauchy(0,25)
  ), data = d1, start = list(a = 50, sigma = 30))

compare(chn_model, intc_model)











precis(m, prob = 0.95) # shows an increase of 0.53km/hour for chinook than for white sturgeon.  But that's just in 2013, when there were many more chn than white sturgeon.

d1all <- as.data.frame(dp3)
d1all <- dplyr::filter(d1all, transit_time > 0)
d1all$dSp <- ifelse(d1all$Sp == "chn", 1, 0)

m1a <- map(flist = alist(
  rate ~ dnorm(mean = mu, sd = sigma) ,
  mu <- a + bSp*dSp,
  a ~ dnorm(0, 10) ,
  bSp ~ dnorm(0, 1),
  sigma ~ dunif(0,10)
),
start = list(a=1, sigma = 5), data = d1all )
precis(m1a) # holy shit - when you take all the data into account, being a chn corresponds to a 0.89km/hr increase.



