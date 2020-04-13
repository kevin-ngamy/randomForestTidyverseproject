# Loading the tidyverse
# .... YOUR CODE FOR TASK 1 ....
library(tidyverse)
library(dplyr)
library(ggplot2)
# Reading in the taxi data
taxi <- read_csv("taxi.csv")

# Taking a look at the first few rows in taxi
head(taxi)
# .... YOUR CODE FOR TASK 1 ....

# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>%
  rename(lat = pickup_latitude, long = pickup_longitude) %>%
  filter(fare_amount > 0 | tip_amount > 0) %>%
  mutate(total = log(fare_amount + tip_amount))

# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93
taxi <- taxi  %>% 
  filter(between(lat, 40.70, 40.83) & between(long, -74.025, -73.93))

# Loading in ggmap and viridis for nice colors
library(ggmap)
library(viridis)
# .... YOUR CODE FOR TASK 4 ....

# Retrieving a stored map object which originally was created by
# manhattan <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("datasets/manhattan.rds")

# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  geom_bin2d(data = taxi, aes(y = lat, x = long), bins = 60, alpha = 0.6) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Journeys')
# .... YOUR CODE FOR TASK 4 .... 

# Loading in the tree package
library(tree)
# .... YOUR CODE FOR TASK 5 HERE ....

# Fitting a tree to lat and long
fitted_tree <- tree(formula = total ~ lat + long, data = taxi)

# Draw a diagram of the tree structure
# .... YOUR CODE FOR TASK 5 HERE ....

# Loading in the lubridate package
library(lubridate)
# .... YOUR CODE FOR TASK 6 HERE ....

# Generate the three new time variables
taxi <- taxi %>% 
  mutate(hour = hour(pickup_datetime),
         wday = wday(pickup_datetime, label = TRUE),
         month = month(pickup_datetime, label = TRUE))

# Fitting a tree with total as the outcome and 
# lat, long, hour, wday, and month as predictors
fitted_tree <- tree(formula = total ~ lat + long + hour + wday + month, data = taxi)

# draw a diagram of the tree structure
# .... YOUR CODE FOR TASK 7 HERE ....
plot(fitted_tree)
text(fitted_tree)
# Summarizing the performance of the tree
summary(fitted_tree)
# .... YOUR CODE FOR TASK 7 HERE ....

# Loading in the randomForest package
library(randomForest)
# .... YOUR CODE FOR TASK 8 HERE ....

# Fitting a random forest
fitted_forest <- randomForest(formula = total ~ lat + long + hour + wday + month,
                              ntree = 80,
                              sampsize = 10000,
                              data = taxi)

# Printing the fitted_forest object
summary(fitted_forest)
print(fitted_forest)
# .... YOUR CODE FOR TASK 8 HERE ....

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

# Plotting the predicted mean trip prices from according to the random forest
# .... COPY CODE FROM TASK 4 AND MODIFY HERE ....
library(ggmap)
library(viridis)

ggmap(manhattan, darken = 0.5) +
  scale_fill_viridis(option = 'plasma') +
  stat_summary_2d(data = taxi, aes(y = lat, x = long, z = pred_total),
                  bins = 60, alpha = 0.6, fun = mean) +
  labs(x = 'Longitude', y = 'Latitude', fill = 'Prediction')

# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
  ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
# .... COPY CODE FROM TASK 9 AND MODIFY HERE ....



















































