df_games <- read.csv("Twitch_game_data.csv")

View(df_games)

# Data Wrangling----

top5_games <- df_games |> 
  filter(between(Rank,1 ,5)) |> 
  group_by(Game) %>% 
  summarize(Count_Months_Top5 = n()) |> 
  arrange(desc(Count_Months_Top5))

hourswatched_games <-  df_games |> 
  group_by(Game) |> 
  summarize(Tot_Hours_watched = sum(Hours_watched)) |> 
  arrange(desc(Tot_Hours_watched)) |> 
  slice(1:10)

streamers_games <- df_games |> 
  group_by(Game) |> 
  summarize(Avg_streamers = mean(Streamers)) |> 
  arrange(desc(Avg_streamers)) |> 
  slice(1:10)
  
# Data Analysis----

top5_games |> 
  ggplot(aes(Count_Months_Top5, reorder(Game, Count_Months_Top5))) +
  geom_col(fill = "purple3") +
  theme_minimal() +
  labs(title = "Months in most watched categories from 2016-2023 - Top 5",
       subtitle = "30 different categories in top 5 most watched categories",
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60, 75, 90))

df_games |> 
  filter(Year == 2023) |> 
  filter(between(Rank,1 ,5)) |> 
  group_by(Game) %>% 
  summarize(Count_Months_Top5 = n()) |> 
  arrange(desc(Count_Months_Top5)) |> 
  ggplot(aes(Count_Months_Top5, reorder(Game, Count_Months_Top5))) +
  geom_col(fill = "purple3") +
  theme_minimal() +
  labs(title = "Months in most watched categories from Jan2023-Mar2023 - Top 5",
       x = "",
       y = "")

hourswatched_games |> 
  ggplot(aes(Tot_Hours_watched/1E6, reorder(Game, Tot_Hours_watched))) +
  geom_col(fill = "purple3") +
  theme_minimal() +
  labs(title = "Hours watched by categories from 2016-2023",
       subtitle = "In millions of hours",
       x = "",
       y = "") +
  scale_x_continuous(labels = comma)

streamers_games |> 
  ggplot(aes(Avg_streamers, reorder(Game, Avg_streamers))) +
  geom_col(fill = "purple3") +
  theme_minimal() +
  labs(title = "Average monthly streamers by categories",
       x = "",
       y = "") +
  scale_x_continuous(breaks = seq(0, 600000, by = 150000),
                     labels = comma)



  
  