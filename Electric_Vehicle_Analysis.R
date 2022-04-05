# ---
#   title: "Which Electric Vehicle (EV) is Worth the Drive?"
# author: "Ty Rawls"
# date: "`r format(Sys.time(), '%A, %B %d, %Y')`"
# output:
#   html_document:
#   number_sections: false
# toc: true
# ---
#   
#   [![forthebadge](https://forthebadge.com/images/badges/made-with-markdown.svg)](https://forthebadge.com)
# 
# # Context
# 
# This is a dataset of EVs.
# 
# Though there is growth in this market, there are very few datasets on EVs. 
# One of the more popular data science datasets is the `mtcars` dataset, which is 
# known for its simplicity when running analysis and visualizations. Below I will 
# answer a few questions to better understand EVs using a small dataset.
# 
# # Questions
# 
# 1. Which vehicle has the fastest 0-100 acceleration?
#   2. Which vehicle has the highest efficiency?
#   3. Does a difference in power train effect the range, top speed, efficiency?
#   4. Which manufacturer has the most number of vehicles?
#   5. How does price relate to rapid charging?
#   
#   # Data Source
#   
#   This dataset originated from an [Electric Vehicle Databse](https://ev-database.org/).


library('tidyverse')
library('lubridate')
library('ggforce')
library('ggplot2')

# Read data
electric_cardata <- read.csv("C:/Users/prade/OneDrive/Documents/Kmeans clustering and DBscan/EVClean.csv")

# Convert datatype from character to integer and inspect data
glimpse(electric_cardata)


# Which car has the fastest 0-100 acceleration?

electric_cardata_accel <-  # Sort vehicles in ascending order by acceleration 
  electric_cardata[,1:3][order(electric_cardata$AccelSec), ]

head(electric_cardata_accel) # View first 6 rows of dataframe

electric_cardata_fastest_accel <- electric_cardata_accel[1, ] # Grab first row

# Output vehicle with the fastest 0-100 acceleration
writeLines(paste0("Fastest Acceleration: ", 
                  electric_cardata_fastest_accel[1], 
                  electric_cardata_fastest_accel[2],                   
                  "at ", 
                  electric_cardata_fastest_accel[3], 
                  " secs"))


# Which Vehicle Has the Highest Efficiency?

electric_cardata_efficiency <- # Sort vehicles in ascending order by efficiency 
  electric_cardata[,c(1:2,6)][order(electric_cardata$Efficiency_WhKm), ]

head(electric_cardata_efficiency) # View first 6 rows of dataframe

electric_cardata_highest_efficiency <- # Grab first row
  electric_cardata_efficiency[1, ]

# Output vehicle with the highest efficiency
writeLines(paste0("High Efficiency: ", 
                  electric_cardata_highest_efficiency[1],  
                  electric_cardata_highest_efficiency[2],                    
                  "at ", 
                  electric_cardata_highest_efficiency[3], " Wh/Km"))

# Does A Difference in Power Train Effect the Range, Top Speed, and Efficiency?

# Create dataframe with range, top speed, efficiency & power train

electric_cardata_powertrain <- electric_cardata[ , c(4:6, 9)]

## Power Train vs Range (km)

# Calculate the average range for each power train
avg_ranges <- aggregate(electric_cardata_powertrain$Range_Km ~ 
                          electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_ranges <- 
  avg_ranges %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_Range_Km = `electric_cardata_powertrain$Range_Km`)

avg_ranges

# Plot Power Train vs Range and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=Range_Km, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Range?",
       subtitle = "Power Train vs Range (km)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) +
  # Mark average range for AWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[1], y = 1.5, 
                   xend = avg_ranges$Avg_Range_Km[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average range for FWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[2], y = 2.5, 
                   xend = avg_ranges$Avg_Range_Km[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average range for RWD power train
  geom_segment(aes(x = avg_ranges$Avg_Range_Km[3], y = 3.5, 
                   xend = avg_ranges$Avg_Range_Km[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))
# ```
# 
# The AWD power train has highest average range in comparison to the other power 
# trains. RWD comes in 2nd and FWD comes in last.

## Power Train vs Top Speed (km/h)


# Calculate the average top speed for each power train
avg_topspeed <- aggregate(electric_cardata_powertrain$TopSpeed_KmH ~ 
                            electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_topspeed <- 
  avg_topspeed %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_TopSpeed_KmH = `electric_cardata_powertrain$TopSpeed_KmH`)

avg_topspeed

# Plot Power Train vs Top Speed and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=TopSpeed_KmH, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Top Speed?",
       subtitle = "Power Train vs Top Speed (km/h)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) +
  # Mark average top speed for AWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[1], y = 1.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average top speed for FWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[2], y = 2.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average top speed for RWD power train
  geom_segment(aes(x = avg_topspeed$Avg_TopSpeed_KmH[3], y = 3.5, 
                   xend = avg_topspeed$Avg_TopSpeed_KmH[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))
# ```
# 
# The AWD power train has highest average top speed in comparison to the other 
# power trains. RWD comes in 2nd and FWD comes in last again.


## Power Train vs Efficiency (Wh/km)


# Calculate the average efficiency for each power train
avg_efficiency <- aggregate(electric_cardata_powertrain$Efficiency_WhKm ~ 
                              electric_cardata_powertrain$PowerTrain, FUN = mean)

# Rename columns
avg_efficiency <- 
  avg_efficiency %>% 
  rename(PowerTrain = `electric_cardata_powertrain$PowerTrain`,
         Avg_Efficiency_WhKm = `electric_cardata_powertrain$Efficiency_WhKm`)

avg_efficiency


# Plot Power Train vs Efficiency and mark the average range for each power train
ggplot(electric_cardata_powertrain, 
       aes(x=Efficiency_WhKm, y=PowerTrain)) + 
  geom_point(aes(color=PowerTrain)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Power Train") +
  labs(title = "Does a Difference in Power Train Effect the Efficiency?",
       subtitle = "Power Train vs Efficiency (Wh/km)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) + 
  # Mark average efficiency for AWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[1], y = 1.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average efficiency for FWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[2], y = 2.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average efficiency for RWD power train
  geom_segment(aes(x = avg_efficiency$Avg_Efficiency_WhKm[3], y = 3.5, 
                   xend = avg_efficiency$Avg_Efficiency_WhKm[3], yend = 3),
               arrow = arrow(length = unit(0.5, "cm")))


# The AWD power train uses the least amount of energy. Therefore, it is more 
# efficient when compared to the other power trains. RWD comes in 2nd and FWD 
# comes in last once again. 
# 
# I think we can conclude that AWD is more superior than the other power trains 
# when comparing range, top speed, and efficiency.

# Which Manufacturer Has the Most Number of Vehicles?


# Calculate number of vehicles for each brand/manufacturer
num_vehicles_per_brand <- 
  electric_cardata %>% 
  group_by(Brand) %>% 
  tally(n="num_vehicles")

# Sort vehicles in ascending order by number of vehicles 
num_vehicles_per_brand <- 
  num_vehicles_per_brand[order(-num_vehicles_per_brand$num_vehicles), ]

head(num_vehicles_per_brand)

# Output manufacturer with the most vehicles
writeLines(paste0("Manufacturer with Most Vehicles: ", 
                  num_vehicles_per_brand[1,1], 
                  "with ", 
                  num_vehicles_per_brand[1,2], 
                  " vehicles"))


# How Does Price Relate to Rapid Charging?

```{r}
# Create dataframe that shows rapid charge availability and price
electric_cardata_rapidcharge_price <- electric_cardata[, c(8, 14)]
```
```{r}
# Sort vehicles in ascending order by price 
electric_cardata_rapidcharge_price <- 
  electric_cardata_rapidcharge_price[order(
    electric_cardata_rapidcharge_price$PriceEuro), ]

# Calculate the average price for vehicles WITH and WITHOUT rapid charging
avg_rapidcharge_price <- 
  aggregate(electric_cardata_rapidcharge_price$PriceEuro ~ 
              electric_cardata_rapidcharge_price$RapidCharge, FUN = mean)

# Rename columns
avg_rapidcharge_price <- 
  avg_rapidcharge_price %>% 
  rename(RapidCharge = `electric_cardata_rapidcharge_price$RapidCharge`,
         Avg_Price = `electric_cardata_rapidcharge_price$PriceEuro`)


avg_rapidcharge_price

# Plot Rapid Charging Availability vs Price (Euro)
ggplot(electric_cardata_rapidcharge_price, 
       aes(x=PriceEuro, y=RapidCharge), fill = RapidCharge) + 
  geom_point(aes(color=RapidCharge)) +
  # Add plot title, subtitle, legend title and caption. Remove x-axis and y-axis 
  # titles. Centers titles and makes main title bold.
  scale_colour_discrete("Rapid Charging") +
  labs(title = "How Does Price Relate to Rapid Charging?",
       subtitle = "Rapid Charging Availabilty vs Price (Euro)",
       caption = "Data collected from ev-database.org") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) + 
  # Mark average price of vehicles WITH rapid charging
  geom_segment(aes(x = avg_rapidcharge_price$Avg_Price[1], y = 1.30, 
                   xend = avg_rapidcharge_price$Avg_Price[1], yend = 1),
               arrow = arrow(length = unit(0.5, "cm"))) +
  # Mark average price of vehicles WITHOUT rapid charging
  geom_segment(aes(x = avg_rapidcharge_price$Avg_Price[2], y = 2.30, 
                   xend = avg_rapidcharge_price$Avg_Price[2], yend = 2),
               arrow = arrow(length = unit(0.5, "cm"))) 


##You practically have a "100% chance" of getting a EV with rapid charging if the 
#price is at or above **???57,324.68** (average price of vehicles with rapid 
  #                                   charging), but what if you're looking to budget? What if you are looking at 
#v3ehicles below the average price of vehicles with rapid charging?

## Chance of Getting a Vehicle with Rapid Charging If Price Is Less Than ???57,324.68


# Filter dataset for vehicles below rapid charge average price
less_than_rapidcharge_price_avg <- 
  electric_cardata_rapidcharge_price %>% 
  filter(PriceEuro < avg_rapidcharge_price$Avg_Price[2])


# Calculate percentage of vehicles WITH rapid charging 
rapidcharge_yes_percentage_01 <- 
  round((nrow(
    less_than_rapidcharge_price_avg %>% 
      filter(RapidCharge == "Yes")) /
      nrow(less_than_rapidcharge_price_avg)) * 100, 2)

# Calculate percentage of vehicles WITHOUT rapid charging 
rapidcharge_no_percentage_01 <- 
  round(100 - rapidcharge_yes_percentage_01, 2)

# Creates a dataframe with rapid charge and percentage 
rapidcharge_percentages_01 <-
  data.frame(rapidcharge = c("Yes", "No"), 
             percentage = rbind("1" = rapidcharge_yes_percentage_01,
                                "2" = rapidcharge_no_percentage_01))

# Plots the likelihood (%) you'll get a vehicle WITH or WITHOUT rapid charging
# based on a price constraint.
ggplot(rapidcharge_percentages_01,
       aes(x = "", y = percentage, fill = rapidcharge)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Removes background, grid, numeric labels.
  guides(fill = guide_legend(title = "Rapid Charge")) +
  labs(title = "How Does Price Relate to Rapid Charging?",
       subtitle = "Analyzing Vehicles Below ???57,324.68",
       caption = "") +
  geom_text(aes(y = c(50, 96), label = c(
    paste(rapidcharge_yes_percentage_01, "%", sep = ""),
    paste(rapidcharge_no_percentage_01, "%", sep = ""))),
    color = "white", size = 6) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic")) 

# Prints the likelihood (%) you'll get a vehicle WITH or WITHOUT rapid charging
# based on a price constraint.
writeLines(paste0("You have a ", 
                  rapidcharge_yes_percentage_01, 
                  "% chance of getting a vehicle with rapid charging."))


## Chance of Getting a Vehicle with Rapid Charging If Price Is Less Than ???25,000



# Filter dataset for vehicles below ???25,000  
less_than_25K <- 
  electric_cardata_rapidcharge_price %>% 
  filter(PriceEuro < 25000)

# Calculate percentage of vehicles WITH rapid charging 
rapidcharge_yes_percentage_02 <- 
  round((nrow(
    less_than_25K %>% 
      filter(RapidCharge == "Yes")) /
      nrow(less_than_25K)) * 100, 2)

# Calculate percentage of vehicles WITHOUT rapid charging 
rapidcharge_no_percentage_02 <- 
  round(100 - rapidcharge_yes_percentage_02, 2)

# Creates a dataframe with rapid charge and percentage 
rapidcharge_percentages_02 <-
  data.frame(rapidcharge = c("Yes", "No"), 
             percentage = rbind("1" = rapidcharge_yes_percentage_02,
                                "2" = rapidcharge_no_percentage_02))

# Plots the likelihood (%) you'll get a vehicle WITH or WITHOUT rapid charging
# based on a price constraint.
ggplot(rapidcharge_percentages_02,
       aes(x = "", y = percentage, fill = rapidcharge)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() + # Removes background, grid, numeric labels.
  guides(fill = guide_legend(title = "Rapid Charge")) +
  labs(title = "How Does Price Relate to Rapid Charging?",
       subtitle = "Analyzing Vehicles Below ???25,000",
       caption = "") +
  geom_text(aes(y = c(25, 75), label = c(
    paste(rapidcharge_yes_percentage_02, "%", sep = ""),
    paste(rapidcharge_no_percentage_02, "%", sep = ""))),
    color = "white", size = 6) +
  theme(plot.title = element_text(color = "black", size = 12, 
                                  face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 10, hjust = 0.5),
        plot.caption = element_text(color = "black", face = "italic"))

# Prints the likelihood (%) you'll get a vehicle WITH or WITHOUT rapid charging
# based on a price constraint.
# writeLines(paste0("You have a ", 
#                   rapidcharge_yes_percentage_02, 
#                   "% chance of getting a vehicle with rapid charging."))
# ```
# 
# It appears that there is a correlation between price and the availability of the 
# rapid charging feature. When the price increases, the likelihood of getting an 
# EV with rapid charging increases. When the price decreases, the likelihood of 
# getting an EV with rapid charging also decreases.
# 
# # In Conclusion
# 
# * The Tesla Roadster has the fastest 0-100 acceleration, which was 2.1 secs
# * The Lightyear One is the most efficient with 104 Wh/km
# * The power train has an effect on range, top speed and efficiency 
# - AWS is the most superior
# - RWD is 2nd best 
# - FWD was last in ALL 3 categories
# * Tesla has the most EVs than any other manufacturer in this dataset
# * The likelihood of getting an EV with rapid charging decreases as the price
# drops
# ...