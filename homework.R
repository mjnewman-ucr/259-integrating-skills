# 259 Homework - integrating skills
# For full credit, answer at least 8/10 questions
# List students working with below:
# Serena, kinda :)

library(tidyverse)
library(lubridate)
library(DataExplorer)

#> These data are drawn from the fivethirtyeight article:
#> http://fivethirtyeight.com/features/what-12-months-of-record-setting-temperatures-looks-like-across-the-u-s/
#> The directory us-weather-history contains a data file for each of 10 cities, labelled by their station name
#> Each data file contains:
#> `date` | The date of the weather record, formatted YYYY-M-D
#> `actual_mean_temp` | The measured average temperature for that day
#> `actual_min_temp` | The measured minimum temperature for that day
#> `actual_max_temp` | The measured maximum temperature for that day
#> `average_min_temp` | The average minimum temperature on that day since 1880
#> `average_max_temp` | The average maximum temperature on that day since 1880
#> `record_min_temp` | The lowest ever temperature on that day since 1880
#> `record_max_temp` | The highest ever temperature on that day since 1880
#> `record_min_temp_year` | The year that the lowest ever temperature occurred
#> `record_max_temp_year` | The year that the highest ever temperature occurred
#> `actual_precipitation` | The measured amount of rain or snow for that day
#> `average_precipitation` | The average amount of rain or snow on that day since 1880
#> `record_precipitation` | The highest amount of rain or snow on that day since 1880

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")


# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

# Answer:

library(readr)

#ds <- read_csv("us-weather-history/KCLT.csv")
#ds$date <- as.Date(ds$date, formate = "%Y-%m-%d", na.rm = T)
#glimpse(ds)
#ds

read_weather <- function(x, y) {
  ds <- read_csv(x)
  ds <- mutate(ds, station = y)
}

#>I know I didn't change the date to date format. Whenever I added it in my function
#>kept returning a value instead of a tibble

KCLT <- read_weather("us-weather-history/KCLT.csv", "KCLT")
KCQT <- read_weather("us-weather-history/KCQT.csv", "KCQT")


# QUESTION 2
#> Use map_dfr() and your new function to read in all 10 stations
#> map_dfr() will take each dataframe and automatically bind them.
#> Save the resulting dataset to "ds"

(stations <- list.files(path = "us-weather-history", all.files = F, 
           pattern = ".csv", full.names = T))

ds <- stations %>% map_dfr(read_weather)

#can't get it to work uuuugh... I asked Serena and she provided me with her function, but it didn't help


ds <- map_dfr(stations,read_weather)

#at some point this was kinda working, but the station column just read as "y" for every row.
#But now I can't even get ds, which makes my below answers not work


# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use fct_count to check that there are 365 days of data for each city 

ds$city <- factor(ds$station, levels = stations, labels = cities)
#had to rerun to "stations" code at the beginning, to replace mine
fct_count(ds$city)

# QUESTION 4
#> Since we're scientists, let's convert all the temperatures to C
#> Write a function to convert F to C, and then use mutate across to 
#> convert all of the temperatures, rounded to a tenth of a degree

ftoc <- function(f){
  celsius <- (f - 32)*(5/9)
}

temp_columns <- c("actual_mean_temp","actual_min_temp","actual_max_temp","average_min_temp","average_max_temp","record_min_temp","record_max_temp")
#Tried to do this as a list. Couldn't figure it out
ds <- ds %>% mutate(across(.cols = temp_columns,~ftoc(.x)))
ds <- ds %>% mutate(across(.cols = temp_columns,~round(.x,digits=1)))

### CHECK YOUR WORK
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.
ds <- read_csv("data-clean/compiled_data.csv")

# QUESTION 5
#> Write a function that counts the number of extreme temperature days,
#> where the actual min or max was equal to the (i.e., set the) record min/max
#> A piped function starting with '.' is a good strategy here.
#> Group the dataset by city to see how many extreme days each city experienced,
#> and sort in descending order to show which city had the most:
#> (Seattle, 20, Charlotte 12, Phoenix 12, etc...)
#> Don't save this summary over the original dataset!



# QUESTION 6
#> Pull out the month from the date and make "month" a factor
#> Split the tibble by month into a list of tibbles 

ds$date <- as.Date (ds$date, format = "%Y-%m-%d")
ds$date <- format(ds$date, "%m")
as.factor(ds$date)
splitds <- split(ds, ds$date)

# QUESTION 7
#> For each month, determine the correlation between the actual_precipitation
#> and the average_precipitation (across all cities), and between the actual and average mins/maxes
#> Use a for loop, and print the month along with the resulting correlation
#> Look at the documentation for the ?cor function if you've never used it before



# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this




# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month




# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> Note, just add geom_line() to your ggplot call to get the lines
#> use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


