library(tidyverse)

# The functions might be useful for A4
source("/Users/jiajialin/Documents/info201/assignments/a4-LinJ12/source/a4-helpers.R")
#data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
data <- get_data()
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#data summary
summary_info <- list()

#The average number of black jail population in the northeast(1985-2018).
get_N_B <- data %>%
  group_by(year, region) %>%
  filter(region == "Northeast" & year > 1984) %>%
  summarise(ave = ave(sum(black_jail_pop, na.rm = TRUE), na.rm = TRUE)) %>%
  pull(ave)
summary_info$N_ave_black_pop <- round(mean(get_N_B), 1)

#The average number of white jail population in the northeast(1985-2018).
get_N_W <- data %>%
  group_by(year, region) %>%
  filter(region == "Northeast" & year > 1984) %>%
  summarise(ave = ave(sum(white_jail_pop, na.rm = TRUE), na.rm = TRUE)) %>%
  pull(ave)
summary_info$N_ave_white_pop <- round(mean(get_N_W), 1)

#The average number of black jail population in the south(1985-2018).
get_S_B <- data %>%
  group_by(year, region) %>%
  filter(region == "South" & year > 1984) %>%
  summarise(ave = ave(sum(black_jail_pop, na.rm = TRUE), na.rm = TRUE)) %>%
  pull(ave)
summary_info$S_ave_black_pop <- round(mean(get_S_B), 1)

#The average number of white jail population in the south(1985-2018).
get_N_B <- data %>%
  group_by(year, region) %>%
  filter(region == "South" & year > 1984) %>%
  summarise(ave = ave(sum(white_jail_pop, na.rm = TRUE), na.rm = TRUE)) %>%
  pull(ave)
summary_info$S_ave_white_pop <- round(mean(get_N_B), 1)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
library(dplyr)
library(ggplot2)
#----------------------------------------------------------------------------#
# This function wrangles the data, it selects the year and total_jail_pop columns from the original data frame.
get_year_jail_pop <- function() {
  df <- data %>%
    group_by(year) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}
# This function create a visualization of total jail Population from 1970 to 2018.
plot_jail_pop_for_us <- function()  {
  labels <- labs(
    x = "Year",
    y = "Total Jail Population",
    title = "Increase of Jail Population in U.S. (1970-2018)",
    caption = "A visualization of total jail population from 1970 to 2018."
  )
  chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) + 
    scale_y_continuous(labels = scales::comma) +
    labels
  return(chart)   
}

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# This function wrangles the data, it selects total_jail_pop of input state.
get_jail_pop_by_states <- function(states) {
  df <- data %>% 
    group_by(state, year) %>%
    filter(state %in% states) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(df)
}
# This function create a visualization of total jail Population from 1970 to 2018 by states.
AL_jail_pop <- get_jail_pop_by_states(c("CA", "WA"))
plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x=year, y=total_jail_pop, color=state)) +
    labs(
      x = "Year",
      y = "Total Jail Population",
      title = "Growth of Prison Population by State(1970-2018)",
      caption = "A visualization of total jail population from 1970 to 2018 by states."
    )
  return(chart)
}


## Section 5  ---- 
#----------------------------------------------------------------------------#
# White Jail Population and Black Jail Population in South of the U.S.
#----------------------------------------------------------------------------#
# This function wrangles the data, it selects the black and white jail population in south region of the U.S..
get_jail_pop_by_race <- function() {
  df <- data %>%
    group_by(year, region) %>%
    filter(region == "South") %>%
    summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE), white_jail_pop = sum(white_jail_pop, na.rm = TRUE))
  return(df)
}

#"This function creates a visualization of the comparison of black and white jail population in south region of the U.S..
plot_jail_pop_by_race <- function() {
  chart <- ggplot(get_jail_pop_by_race(), aes(x = year)) +
    geom_line(aes(y = black_jail_pop, colour = "Black_Population"), size = 1.5) +
    geom_line(aes(y = white_jail_pop, colour = "White_Population"), size = 1.5) + 
    labs(
      x = "Year",
      y = "Jail Population",
      title = "Black jail population and White jail population from 1970 to 2018 in the South region",
      caption = "A visiualization compares the total jail population between black population and white population in the south region",
      color = "Population"
    ) 
  return(chart)
}


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
library(leaflet)
library(dplyr)
library(tigris)

#Data wrangling
get_black_jail_pop <- function() {
  df_pop <- data %>%
    #filter and calculate the total black jail population of each states.6
    group_by(state) %>%
    summarise(black_jail_pop = sum(black_jail_pop, na.rm = TRUE))
  #Get the state map.
  df_states_map <- states(cb=TRUE)
  #combine two data frame.
  df_combine <- geo_join(df_states_map, df_pop, "STUSPS", "state")
  df_combine <- subset(df_combine, !is.na(black_jail_pop))
  return(df_combine)
}

#Plot the visualization.
plot_black_jail_pop <- function() {
  pal <- colorNumeric("Blues", domain = get_black_jail_pop()$black_jail_pop)
  pop_up_text <- paste0("Black jail Population = ", as.character(get_black_jail_pop()$black_jail_pop))
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(-98.483330, 38.712046, zoom = 4) %>%
    addPolygons(data = get_black_jail_pop(),
                fillColor = ~pal(get_black_jail_pop()$black_jail_pop),
                fillOpacity = 0.7,
                weight = 0.2,
                smoothFactor = 0.2,
                popup = ~pop_up_text) %>%
    addLegend(pal = pal, 
              values = get_black_jail_pop()$black_jail_pop,
              position = "bottomright",
              title = "Black jail Population")
}





