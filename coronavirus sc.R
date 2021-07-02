library(httr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

cases <-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")


sc <- c("Argentina","Brazil","Chile","Paraguay","Uruguay","Colombia","Peru") 

sc_cases <- filter(cases, Country.Region %in% sc)

sc_cases <- sc_cases[ -c(1, 3:56) ]

removeX = function(es) {
  f = es
  for (col in c(1:ncol(f))){ #for each column in dataframe
    if (startsWith(colnames(f)[col], "X") == TRUE)  { #if starts with 'X' ..
      colnames(f)[col] <- substr(colnames(f)[col], 2, 100) #get rid of it
    }
  }
  assign(deparse(substitute(es)), f, inherits = TRUE) #assign corrected data to original name
}

sc_cases <- removeX(sc_cases)

# Change the date below to obtain the chart with the latest data------
# E.g, change "11.7.20" to "7.1.21" ---------------------------------

long_sc_cases <- sc_cases %>% gather(Date, Cases, "3.14.20":"7.1.21")

long_sc_cases <- long_sc_cases %>%
  group_by(Country.Region) %>%
  mutate(Daily_Cases = Cases - lag(Cases)) %>%
  ungroup


long_sc_cases$Date <- mdy(long_sc_cases$Date)
  
ggplot(long_sc_cases, aes(x = Date, y = Daily_Cases)) +
  geom_bar(stat='identity', colour = "steelblue4" ) +
  facet_wrap( ~ Country.Region, ncol=3, scales = "free_y") +
  theme_minimal() +
  labs(y = "Daily Cases",
       title = "Daily Coronavirus Cases in Selected Countries", 
       caption = "Source: John Hopkins Univeristy")+
  theme(plot.title = element_text(size = 14),
        legend.position = "none") 


deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

sc_deaths <- filter(deaths, Country.Region %in% sc)

sc_deaths <- sc_deaths[ -c(1, 3:56) ]

sc_deaths <- removeX(sc_deaths)

# Change the date below to obtain the chart with the latest data------
# E.g, change "11.7.20" to "11.8.20" ---------------------------------

long_sc_deaths <- sc_deaths %>% 
  gather(Date, Deaths, "3.14.20":"11.7.20")

long_sc_deaths <- long_sc_deaths %>%
  group_by(Country.Region) %>%
  mutate(Daily_Deaths = Deaths - lag(Deaths)) %>%
  ungroup

long_sc_deaths$Date <- mdy(long_sc_deaths$Date)


long_sc_deaths <- long_sc_deaths %>%
    mutate(pop = ifelse(Country.Region == "Argentina", 45.53,
           ifelse(Country.Region == "Brazil", 212.56,
                  ifelse(Country.Region == "Chile", 19.11,
                         ifelse(Country.Region == "Colombia", 50.88,
                                ifelse(Country.Region == "Uruguay", 3.47,
                                       ifelse(Country.Region == "Paraguay", 7.13, 
                                              ifelse(Country.Region == "Peru", 32.97,0)))))))) %>%
     mutate(cdm = Deaths/pop)


long_sc_deaths <- long_sc_deaths %>%
  rename(Country = Country.Region) 

ggplot(long_sc_deaths, aes(x = Date, y = cdm)) +
  geom_path(aes(group = Country, colour = Country)) +
  theme_minimal() +
  scale_color_manual(values=c("deepskyblue1", "forestgreen", "navy", "yellow2", "#E69F00", "red2", "#999999")) +
  labs(y = "Cumulative Deaths per Million Population",
       title = "Cumulative Deaths per Million Population", 
       caption = "Source: John Hopkins Univeristy")+
  theme(plot.title = element_text(size = 14),
        legend.position = "bottom") 
