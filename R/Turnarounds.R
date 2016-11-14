load("data_tidy/fishpaths11-15.Rdata") #generates object d3; this is just all the Yolo Bypass detections for all tagged fish through fall chn 2015
library(lubridate)
library(dplyr)
## filter out tag day detections #-------------
# d3 <- all69khz_grouped
head(d3)
f <- d3
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

# don't run until you're ready to plot:
f$detyear <- factor(f$detyear, labels = c(  "2011 - 2012",
                                                "2012 - 2013",
                                                "2013 - 2014",
                                                "2014 - 2015",
                                                "2015 - 2016"))

head(f)

fl <- split(f, as.factor(f$detyear))


f1 <- f %>% 
 # add 1 chn to 2015, when BC_joint picked up > BC2
  group_by(Sp, detyear, TagID) %>% 
  arrange(DateTimeUTC) %>%
  filter(!duplicated(DateTimeUTC)) %>%
  summarise(first = paste(Station[DateTimeUTC == min(DateTimeUTC)]),
            last = paste(Station[DateTimeUTC == max(DateTimeUTC)])) %>% 
  ungroup()
head(f1)

comps <- f %>%  # run the model on this one
  group_by(Sp, detyear) %>% 
  mutate(nfish = len(TagID)) %>% 
  filter(!duplicated(detyear)) %>% 
  select(detyear, Sp, nfish)
comps

esc <- read.csv("data_tidy/escapement.csv", stringsAsFactors = FALSE, header = TRUE)
str(esc)
esc$detyear <- as.numeric(esc$detyear)

esc2 <- reshape2::melt(esc, id.vars = c("detyear", "species", "sample_size"), value.name = "count")
esc2 <- filter(esc2, detyear != 2011)
esc2$species <- factor(esc2$species, labels = c("Chinook salmon", "white sturgeon"))
  
ggplot(esc2, aes(x = detyear, y = count, fill = variable)) + 
  facet_wrap(~species, labeller = label_value) + 
  geom_rect(aes(fill = species), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = FALSE) +
   geom_bar(stat = 'identity', color = "black", width = 0.5, position = "stack") +
  scale_y_continuous(expand = c(0, 0.3), limits = c(0, 55)) +
  scale_fill_manual(values = c("#440154FF" , "white", "gray40", "#FDE725FF")) +
  labs(y = "Count") +
  coord_flip() +
   theme(text = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.spacing = unit(15, "points"),
        legend.position = "none")

ggsave(filename = "figures/escapement.jpg", width = 12, height = 5.5)

# Wrap by year instead
ggplot(esc2, aes(x = species, y = count, fill = variable)) + 
  facet_wrap(~detyear, labeller = label_value) + 
  geom_rect(aes(fill = species), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2, show.legend = FALSE) +
  geom_bar(stat = 'identity', color = "black") +
  scale_fill_manual(values = c("#440154FF" , "white", "gray40", "#FDE725FF")) +
  labs(y = "Count") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Try w/ geom_ribbon
ggplot(esc2, aes(x = detyear, y = count, fill = variable)) + 
  geom_ribbon(aes(fill = variable, color = variable, ymin = 0, ymax = count), stat = "identity", alpha = 0.2) +
  facet_wrap(~species, scales = "free_y") +
  labs(y = "Count") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5))



#  Begin modeling
head(f1)
f1 <- left_join(f1, comps)
f1$escapement <- ifelse(f1$last == "BC_joint" | f1$last   == "BC_joint2" | f1$last == "Putah_creek" | f1$last == "Base_TD", 1, 0)
f1$species <- ifelse(f1$Sp == "chn", 1, 0)
f1$spp <- coerce_index(f1$Sp)

head(f1)
f1 <- as.data.frame(f1)
data(chimpanzees)
head(chimpanzees)
# TagID = actor, 

# escapement[i] ~ Binomial(1, pi)
# logit(pi) ~ a + bSpp*species

m <- map(
  alist(
    escapement ~ dbinom(nfish, p),
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ),
  data = f1)
precis(m, prob = 0.95)

logistic(c(-3.87, 0.06)) # before considering species, most fish escape.

head(f1)

m2 <- map(
  alist(
    escapement ~ dbinom(nfish, p),
    logit(p) <- a + bSpp*species,
    a ~ dnorm(0,10),
    bSpp ~ dnorm(0, 10)
  ),
  data = f1)
precis(m2, prob = 0.95)

compare(m, m2)

post <- extract.samples(m2)
p.escape.salmon <- logistic(post$a + post$bSpp)
p.escape.wst <- logistic(post$a)
diff.esc <- p.escape.salmon - p.escape.wst
quantile(diff.esc, c(0, 0.025, 0.5, 0.975))  # means that the median estimate of escapement for salmon is about -23%, with a 95% CI of between -40% and -14%.
dens(diff.esc)



logistic(c(-1.72, 0.36, -2.41, -1.02)) # salmon estimates
exp(-1.72) # proportional change in odds of escapement for being a salmon; odds decrease by 18%
logistic(c(2.57, 0.3, 2.09, 3.04))
logistic(2.57 + (-1.72)) # difference on the absolute scale of 22% between white sturgeon and Chinook salmon, despite being only an 18% increase in proportional odds.
plot(precis(m2))

d.pred <- data.frame(species = c(0, 1))
fish.ensemble <- ensemble(m, m2, data = d.pred)
pred.p <- apply(fish.ensemble$link, 2, mean)
pred.p.PI <- apply(fish.ensemble$link, 2, PI)
plot(0, 0, type = "n", xlab = "species",
     ylab = "Proportion escaped", ylim = c(0, 1), xaxt = "n" ,
     xlim = c(0, 1))
axis(1, at = 0:1, labels = c("white sturgeon", "chinook"))

p <- by(f1$escapement, f1$species, mean)
chn <- c(1, 0.7007874)
wst <- c(0, 0.9289941)
points(x = 0, y = wst, col=rangi2)
points(x = 1, y = chn, col=rangi2)
plot(0, 0, type = "n")
lines(wst, chn, col=rangi2)

lines(1:2, pred.p)
shade(pred.p.PI, 1:2)

## Using the UCB admit template:
library(rethinking)
head(esc)
esc$chn <- ifelse(esc$species == "chn", 1, 0)
esc <- as.data.frame(esc)
esc
m <- map(
  alist(
  escaped ~ dbinom(sample_size, p) ,
  logit(p) <- a + bchn*chn ,
  a ~ dnorm(0, 10),
  bchn ~ dnorm(0, 10)
), 
data = esc)

m2 <- map(
  alist(
    escaped ~ dbinom(sample_size, p) ,
    logit(p) <- a ,
    a ~ dnorm(0, 10)
  ), 
  data = esc)

compare(m, m2)

precis(m, prob = 0.95)
precis(m2, prob = 0.95)
exp(-2.65)*100 # a salmon's odds of escapement were only 7% that of a white sturgeon's.  That's relative, though.  What matters is absolute difference:

post <- extract.samples(m)
p.escape.salmon <- logistic(post$a + post$bchn)
p.escape.wst <- logistic(post$a)
diff.esc <- p.escape.salmon - p.escape.wst
quantile(diff.esc, c(0.025, 0.5, 0.975))  # means that the median estimate of escapement disadvantage for salmon is about -34%, with a 95% CI of between -43% and -25%.
dens(diff.esc)
plot(post)
postcheck(m) # blue points are observed proportions escaped for each row in the data, and open points are expected proportions.

## Allowing escapement to vary by year:
str(esc)
esc$detyear_id <- coerce_index(esc$detyear)
m3 <- map2stan(
  alist(
    escaped ~ dbinom(sample_size, p) ,
    logit(p) <- a[detyear_id] + bchn*chn,
    a[detyear_id] ~ dnorm(3, 20),
    bchn ~ dnorm(3, 10)
  ), 
  data = esc, start = list(a=3),
  chains = 2, warmup = 500, iter = 1e4)
