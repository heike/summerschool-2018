## ------------------------------------------------------------------------
iowa <- read.csv("https://raw.githubusercontent.com/heike/summerschool-2018/master/02-graphics/data/brfss-iowa-2012.csv")

## ------------------------------------------------------------------------
library(ggplot2)
qplot(data = iowa, x = HTIN4, y = WTKG3)

# use some alpha blending to alleviate over-plotting
qplot(data = iowa, x = HTIN4, y = WTKG3, alpha = I(0.5))

# Generally there is a positive relationship, but it's not very strong, and there are lots of outliers on the upper end of the weight.
# The stripes come from height being reported in inch. We can use jittering to add some random noise

qplot(data = iowa, x = HTIN4, y = WTKG3, alpha = I(0.5), geom="jitter")


## ------------------------------------------------------------------------
qplot(data = iowa, x = HTIN4, y = WTKG3/100, colour = SEX)

# the colour is strange - let's change the SEX variable to a factor permanently:
iowa$SEX <- factor(iowa$SEX)

qplot(data=iowa, x = HTIN4, y = WTKG3, colour = SEX)

## ------------------------------------------------------------------------
qplot(data = iowa, x = HTIN4, y = WTKG3, colour = SEX) +
  ggtitle("Relationship of weight and height among the Iowa population in 2012") +
  xlab("Height in inch") +
  ylab("Weight in kg")

## ------------------------------------------------------------------------
iowa$bmi <- (iowa$WTKG3/100)/(iowa$HTIN4*2.54/100)^2
summary(iowa$bmi)

## ------------------------------------------------------------------------
qplot(data = iowa, x = bmi, geom="histogram") 

## ------------------------------------------------------------------------
qplot(data = iowa, x = bmi, geom="histogram", binwidth=1) 

## ------------------------------------------------------------------------
qplot(data = iowa, x = bmi, geom="histogram", facets = SEX~.)

## ------------------------------------------------------------------------
qplot(data = iowa, x = SEX, y = DRNK3GE5, geom="boxplot") 

## ------------------------------------------------------------------------
qplot(data = subset(iowa, DRNK3GE5<77), x = SEX, y = DRNK3GE5, geom="boxplot") 

## ------------------------------------------------------------------------
qplot(data = subset(iowa, DRNK3GE5<77), x = SEX, y = DRNK3GE5, geom="boxplot") +
  geom_jitter()

## ------------------------------------------------------------------------
qplot(data = iowa, x = SEATBELT, geom="bar")
qplot(data = iowa, x = factor(SEATBELT), geom="bar")

## ------------------------------------------------------------------------
qplot(data = iowa, x = factor(SEATBELT), geom="bar", facets=~SEX)
# proportion of women always using a seatbelt is higher than proportion of men.
qplot(data = iowa, x = factor(SEATBELT), geom="bar", fill=factor(SEX))

qplot(data = iowa, x = SEX, geom="bar", fill=SEATBELT==1)


## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
states <- map_data("state")

states.sex.stats <- read.csv("http://heike.github.io/rwrks/02-r-graphics/data/states.sex.stats.csv", stringsAsFactors = FALSE)
states.sex.map <- left_join(states, states.sex.stats, by = c("region" = "state.name"))

qplot(long, lat, geom = "polygon", data = states.sex.map, 
      group = group, fill = avg.drnk) + coord_map() + 
    facet_grid(. ~ sex)

## ------------------------------------------------------------------------
qplot(long, lat, geom = "polygon", data = states.sex.map, 
      group = group, fill = avg.drnk) + coord_map() + 
    facet_grid(. ~ sex) +
  scale_fill_gradient2(midpoint=median(states.sex.stats$avg.drnk), low = "steelblue", high = "darkorange") + 
  ggthemes::theme_map()  +
  ggtitle("Map of Average Number of Drinks by State") +
  theme(legend.position="right")

## ---- fig.height=4, fig.width=7, warning=FALSE---------------------------
load("../data/noaa.rdata")
ggplot(aes(x = Depth, y = Temperature, colour = callSign), 
       data = floats) + 
  geom_point()

## ------------------------------------------------------------------------
animal <- read.csv("https://raw.githubusercontent.com/heike/summerschool-2018/master/02-graphics/data/animal.csv")

## ------------------------------------------------------------------------

ggplot() +
    geom_path(data = states, aes(x = long, y = lat, group = group)) +   geom_point(data = animal, aes(x = Longitude, y = Latitude)) +   
    geom_point(aes(x, y), shape = "x", size = 5, data = rig) + 
    geom_text(aes(x, y), label = "BP Oil Rig", size = 5, data = rig, hjust = -0.1) + 
    xlim(c(-91, -80)) + 
    ylim(c(22, 32)) + coord_map()


