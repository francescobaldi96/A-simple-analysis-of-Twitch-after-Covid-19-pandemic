library(tidyverse)
library(magrittr)
library(lubridate)
library(scales)

# Obiettivo: capire l'andamento di Twitch dopo il Covid-19

df <- read.csv("Twitch_global_data.csv")
View(df)
head(df)
glimpse(df)

# Data Wrangling----

df <- df   |> 
  mutate(Date = as.Date(
    paste("01", Month, year, sep = "-"), format ="%d-%m-%Y")
    ) |> 
  mutate(year = as.factor(year))

# Data Analysis----
## Hours watched on Twitch from 2016-2023
df |> 
  ggplot(aes(Date, Hours_watched/1E6)) + 
  geom_col(aes(fill = year)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs(title = "Hours watched on Twitch from 2016-2023",
       subtitle = "In millions of hours", 
       x = "", 
       y = "")

## Average viewers on Twitch from 2016-2023
df |> 
  ggplot(aes(Date, Avg_viewers/1E6)) +
  geom_col(aes(fill = year)) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal() +
  labs(title = "Average viewers on Twitch from 2016-2023",
       subtitle = "in millions of viewers",
       x = "",
       y = "")


## Average channels on Twitch from 2016-2023
df |> 
  ggplot(aes(Date, Avg_channels/1E3)) +
  geom_col(aes(fill = year)) +
  theme_minimal() +
  scale_fill_brewer(palette = "Spectral") +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") +
  labs(title = "Average channels on Twitch from 2016-2023",
       subtitle = "in thousand of channels",
       x = "",
       y = "")
