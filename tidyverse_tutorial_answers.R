################################################################################
###
### Working with data in the tidyverse
###
### Sara Gottlieb-Cohen, Manager of Statistical Support Services
### Center for Science and Social Science Information
### Yale University
###
################################################################################


library(tidyverse)

# The first thing you will notice is that we use a "pipe" (%>%) notation in the tidyverse. 
# The object being "piped" into the function is used as the first argument (sometimes called the)
# "subject," and the function its being piped into is called the "verb." Examples:

x <- c(1:10)
sum(x)

x %>% sum(x)
  
# Why is the advantage of piping? You can chain several pipes together to create code that
# does sophisticated work without all the complicated nesting. It's easier to see what the code 
# is doing at a glance. We'll see some nice examples later in this tutorial.


################################################################################
### Example 1: Iris ###

head(iris)

# It is easy to select only certain cases using the pipe:

setosa_only <- iris %>%
  filter(Species == "setosa")

setosa_only

# It is also easy to select only certain columns:

lengths <- iris %>%
  select(Species, contains("Length"))

# And we can create new variables using the "mutate" verb:

iris2 <- iris %>%
  mutate(petal_ratio = Petal.Width/Petal.Length,
         sepal_ratio = Sepal.Width/Sepal.Length)

iris2

# It is remarkably easier to reshape data using the pipe. Iris is in long format; we are 
# going to make it into long format, with one row per observation.

iris_long <- iris %>%
  gather(key = "variable", value = "number", -Species) %>%
  separate(variable, into = c("part","measurement")) 

iris_long

# Once data is in tidy/long format, it is really easy to reduce our data by summarizing
# across obsevations.

iris_summary <- iris_long %>%
  group_by(Species, part, measurement) %>%
  summarize(average = mean(number))

iris_summary

# Summary tables allow us to easily visualize the data:

ggplot(iris_summary, aes(x = Species, y = average, fill = measurement)) +
  geom_bar(stat = "identity", position = "dodge") 

################################################################################
### Example 2: Flight schedules ###

# We ultimately want to plot the percentage of delayed flights by city and airline.

flight_schedule <- read.csv(file="https://raw.githubusercontent.com/raghu74us/607_1/master/flt.csv", 
                            header=TRUE, sep=",")

str(flight_schedule)
head(flight_schedule)

# Complete the following exercises to manipulate and tidy this data

# 1. Rename X and X.1 "Airline" and "Status," respectively

flight_schedule_tidy <- flight_schedule %>%
  rename(Airline = X,
         Status = X.1)

# 2. Tidy the data by forcing it into a long format. Each line will represent
# one observation, and you should have three columns: Airline, Departure_city, and Status and Flights

flight_schedule_tidy <- flight_schedule_tidy %>%
  gather(key = "Departure_City", value = "Flights", -Airline, -Status)

# 3. The data look good! But the format of the city names are inconsistent; next, remove the periods.

flight_schedule_tidy <- flight_schedule_tidy %>%
  mutate(Departure_City = str_replace(Departure_City, "\\.", " "))

# 4. We are almost there! We want to create a new variable called "Percent_delayed," but we need
# to make a final change to the data format in order to do that. We want "Status" to actually be 
# two variables - one column will contain the number of delayed flights, and the other will contain the 
# number of on time flights. We use "spread" to do this, which is the opposite of "gather."

flight_schedule_tidy <- flight_schedule_tidy %>%
  spread(Status, Flights)

# 5. Rename "on time" so it does not have a space in it.

flight_schedule_tidy <- flight_schedule_tidy %>%
  rename(on_time = `on time`)

# 6. Our data is in the disired format! 
# Use mutate to create a new variable called "percent_delayed."

flight_schedule_tidy <- flight_schedule_tidy %>%
  mutate(percent_delayed = delayed/(delayed + on_time))
         
# 7. Now we can plot the data. Put city on the x-axis, percentage on the y-axis,
# and use different color bars for airline.

ggplot(flight_schedule_tidy, aes(x = Departure_City, y = percent_delayed, fill = Airline)) +
  geom_bar(stat = "identity", position = "dodge")

################################################################################
### Example 3: Covid-19 data from California ###

# Load NYT data about the number of cases in counties across the country.
counties <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

head(counties)
str(counties)

# Load an additional data set that contains populations for each county.
populations <- read.csv("/Users/sgc/Downloads/populations.csv")

# Our goal is to create a bar chart with data from the three counties with the most cases in California
# on the most recent day of the data set. We will put date on the x-axis, and percent of the population infected
# on the y-axis. We will create separate plots for the three different counties.

# There is a lot to do. We will break it down into different steps.

# 1. Filter the data to include only observations in California.

cal_covid <- counties %>%
  filter(state == "California")

# 2. The tidyverse can handle dates really well. Date is currently being treated as a factor; transform it
# into a date variable instead.

cal_covid <- cal_covid %>%
  mutate(date = as.Date(date))

str(cal_covid)

# 3. To keep things tidy, select on the columns we need and get rid of what we don't need (state, fips, deaths).

cal_covid <- cal_covid %>%
  select(date, county, cases)

# 4. We want to match population data to this data set. We are introducing a new verb here called "join." It will
# append population data if we match rows based on county name. However, we have an issue we need to tackle first.
# In the population data set, each county name has "County" pasted after it. We are going to edit the county names in 
# the cal_covid data set to match this format so that we can correctly join our two data sets.

cal_covid <- cal_covid %>%
  rename(CTYNAME = county) %>%
  mutate(CTYNAME = paste(CTYNAME, "County", sep = " "))

# 5. Now we can join them.

cal_covid <- cal_covid %>%
  left_join(populations, by = "CTYNAME") %>%
  select(-GrowthRate)

# 6. Use mutate to create a new variable called "percent_infected."

cal_covid <- cal_covid %>%
  mutate(percent_infected = (cases/Pop)*100)

# 7. We now have all the data we need! However, we want to only graph the number of cases
# in the top counties with the most cases. We can find this out by selecting for the most 
# recent day, and then looking at which counties have the most cases.

sickest_counties <- cal_covid %>%
  filter(date == max(date)) %>%
  arrange(desc(cases))
  
head(sickest_counties)

# 8. Now that we have the sickest counties, we are going to filter cal_covid to only include observations
# from those counties.

selected_counties <- c("Los Angeles County", "Santa Clara County", "San Diego County")

cal_covid <- cal_covid %>%
  filter(CTYNAME %in% selected_counties)

# 9. Now we can finally graph our data!

ggplot(cal_covid, aes(x = date, y = percent_infected)) +
  geom_bar(stat = "identity") +
  facet_wrap(~CTYNAME)

# Below you will see how we can compress all this into one pipe.

cal_covid2 <- counties %>%
  filter(state == "California") %>%
  mutate(date = as.Date(date)) %>%
  select(date, county, cases) %>%
  rename(CTYNAME = county) %>%
  mutate(CTYNAME = paste(CTYNAME, "County", sep = " ")) %>%
  left_join(populations, by = "CTYNAME") %>%
  select(-GrowthRate) %>%
  mutate(percent_infected = cases/Pop) %>%
  filter(CTYNAME %in% selected_counties)

ggplot(cal_covid2, aes(x = date, y = percent_infected)) +
  geom_bar(stat = "identity") +
  facet_wrap(~CTYNAME)
