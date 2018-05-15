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


### Layers
#--------------------------------------------

## Your turn 1 - Deepwater horizon 

	# recreate the map shown
	load("../../02-r-graphics/data/noaa.rdata")
	ls()
	head(floats)
	ggplot(data = floats, aes(x = Depth, y = Temperature)) + 
		geom_point(aes(color = callSign))

## Your turn 2 - animal

	# 1. Read in the animal.csv data
	
	animal <- read.csv("https://heike.github.io/rwrks/02-r-graphics/data/animal.csv")
	
	# 2. Plot the location of animal sightings on a map of the region
	
	ggplot() + 
		geom_path(data = states, aes(x = long, y = lat, group = group)) + 
		geom_point(data = animal, aes(x = Longitude, y = Latitude)) + 
		xlim(c(-91, -80)) + ylim(c(24,32)) + coord_map()
	
	# 3. On this plot, try to color points by class of animal and/or 
		# status of animal
		
	ggplot() + 
		geom_path(data = states, aes(x = long, y = lat, group = group)) + 
		geom_point(data = animal, aes(x = Longitude, y = Latitude, 	
					color = class)) + 
		xlim(c(-91, -80)) + ylim(c(24,32)) + coord_map()		
		
	ggplot() + 
		geom_path(data = states, aes(x = long, y = lat, group = group)) + 
		geom_point(data = animal, aes(x = Longitude, y = Latitude, 	
					color = Condition)) + 
		xlim(c(-91, -80)) + ylim(c(24,32)) + coord_map()			
		
	# 4. Advanced: Could we indicate time somehow?
		
	library(lubridate)
	animal$month <- month(as.Date(animal$Date_))
		
	ggplot() + 
		geom_path(data = states, aes(x = long, y = lat, group = group)) + 
		geom_point(data = animal, aes(x = Longitude, y = Latitude, 	
					color = Condition), alpha = .5) +
		xlim(c(-91, -80)) + ylim(c(24,32)) +
		facet_wrap(~month) + coord_map() 		
			
### Perception
#-------------------------------------------------

## Your turn 1 - interaction

	frame <- read.csv("http://heike.github.io/rwrks/02-r-graphics/data/frame.csv") 
	qplot(x, y, data = frame, shape = g1, colour = g2, size = I(4))
	
	# Make sure the "oddball" stands out while keeping the information on the groups
	frame$inter <- interaction(frame$g1, frame$g2)
	ggplot(frame, aes(x, y)) +  
	geom_point(aes(shape = g1, fill = g2, color = inter), size = I(4), stroke = I(2)) + 
	scale_shape_manual(values = c(21,22)) + 
	scale_colour_manual(values = c("red", "black", "green")) + 
	scale_fill_manual(values = c("red", "green")) + 
	theme(legend.position = 'none')
	
## Your turn 2 - View Many Variables at Once

	# In the diamonds data, clarity and cut are ordinal, while price and carat are continuous. Find a graphic that gives an overview of these four variables while respecting their types. Hint: start with the following code: 
	
	qplot(carat, price, shape = cut, colour = clarity, data = diamonds, 
	size = carat, alpha = price) # ?? 
	
## Your turn 3 - Movies 	

	# Explore the differences in length, rating, etc. in movie genres over time
	
	movies <- read.csv("http://heike.github.io/rwrks/02-r-graphics/data/MovieSummary.csv")

	ggplot(movies, aes(x = year, y = budget, group = genre, color = genre)) + 
	geom_point()
	
	ggplot(movies, aes(x = year, y = length, group = genre, color = genre)) +
	geom_smooth()	
	 
	ggplot(movies, aes(x = budget, y = rating, color = genre, group = genre)) + 
	geom_point() +
	geom_smooth()  + 
	facet_wrap(~mpaa)
	
	ggplot(movies, aes(x = log(budget+1), y = rating, color = genre, group = genre)) + 
	geom_point() +
	geom_smooth() 
	
	ggplot(movies, aes(x = genre, fill = mpaa)) + geom_bar() 
	
	 ggplot(movies, aes(x = rating, group = mpaa, fill = mpaa)) + 
	 geom_density(alpha = .4) + facet_wrap(~genre, nrow = 2)

### Polishing your plots
#------------------------------------------------------

## Your turn 1 - Saving files

	# 1. Save a PDF of a scatterplot of price vs carat
	
	qplot(price, carat, data = diamonds)
	
	ggsave("diamonds.pdf")
	
	# 2. Open up the pdf in Adobe Acrobat (or another PDF reader)
	# 3. Save a png of the same scatterplot 
	
	ggsave("diamonds.png")
	
	
	# 4. Embed the png into MS word or another editor.	 
