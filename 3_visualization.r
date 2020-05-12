fifa = read.csv("1_cleaned_data.csv")
library("ggplot2")
library("ggthemes")
library("magrittr")
library("dplyr")
library("scales")
options(scipen = 999)

#FREQUENCY PLOT OF FOOT PREFERRENCE
print(ggplot(fifa, aes(Preferred.Foot)) +geom_bar(fill = "#0073C2FF") + ggtitle("Most of the players are right footed") + theme_fivethirtyeight())

#HISTOGRAM OF PLAYER RATING
print(fifa %>%
  ggplot(aes(x= Overall)) +
  geom_histogram(color = "white", fill = "darkgrey", binwidth = 1) +
  ggtitle("Player Ratings Are Normally Distributed", subtitle = "The mean can be used as a measure of central tendancy") +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_blank()))

#BOXPLOT OF AGE VS VALUE
fifa <- fifa %>%
  mutate(AgeGroup = ifelse(Age <= 20, "20 and under", ifelse(Age > 20 & Age <=25, "21 to 25", ifelse(Age > 25 & Age <= 30, "25 to 30", ifelse(Age > 30 & Age <= 35, "31 to 35", "Over 35")))))
print(fifa %>%
        ggplot(aes(x= AgeGroup, y= Value)) +
        geom_boxplot(fill = "darkgrey") +
        scale_y_log10(labels = dollar_format(prefix = "€")) +
        ggtitle("Players Are In High Demand In Their Mid-20s", subtitle = "Valuation on a log scale, so differences \nbetween the age groups are significant") +
        theme_fivethirtyeight())


#SCATTERPLOT OF VALUE VS OVERALL RATING
print(fifa %>%
        ggplot(aes(x= Overall, y= Value)) +
        geom_point(position = "jitter", color = "darkgrey") +
        ggtitle("Higher Ratings Cost More Money") +
        scale_y_continuous(labels = dollar_format(prefix = "€")) +
        theme_fivethirtyeight())

# FREQUENCY PLOT OF TEAM VS TOTAL WAGE
print(fifa %>% 
  group_by(Club) %>% 
  summarise(TotalWages = sum(Wage, na.rm = T),) %>%
  arrange(desc(TotalWages)) %>% head(n= 20) %>%
  ggplot(aes(x= reorder(Club, TotalWages), y= TotalWages)) +
  geom_col(colour = "black") +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  coord_flip() +
  ggtitle("The 20 highest wage bills in FIFA19 and how much one rating point costs in wages") +
  theme_fivethirtyeight() +
  theme(legend.position = "none"))