#1 install and load needed packages

install.packages("trelliscopejs",
                 "tidyverse",
                 "lubridate",
                 "tidymodels",
                 "ggpmisc",
                 "skimr",
                 "rpivotTable",
                 "ggrepel")


library(trelliscopejs)
library(tidyverse)
library(lubridate)
library(tidymodels)
library(ggpmisc)
library(skimr)
library(ggrepel)

#2 read the data file

temp_df <- read_csv("https://raw.githubusercontent.com/Ammar-K/1000_cities_temp_analysis/main/temp_df.csv")



#3 explore the data

str(temp_df)

summary(temp_df)

skim(temp_df)

rpivotTable::rpivotTable(temp_df)

# you can also check Dataexplorer https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html

#4 quick viz

##A selected cities

cities <- c("Riyadh", "Tokyo", "Cairo", "New York")



temp_df %>%
  filter(city %in% cities) %>%
  ggplot(aes(x= index, y= temp)) +
  geom_point() +
  facet_wrap(~city) +
  stat_poly_line()+
  stat_poly_eq(aes(label = after_stat(eq.label)))

##B all cities

temp_df %>%
  # filter(city %in% cities) %>%
  ggplot(aes(x= index, y= temp)) +
  geom_point() +
  #facet_wrap(~city) +
  facet_trelliscope(~city + country)+ # We use facet_trelliscope to visualize all the data, you can comment this line and uncomment facet_wrap and  filter to visualize a selected data set
  stat_poly_line()+
  stat_poly_eq(aes(label = after_stat(eq.label)))

#5 create the model
temp_model <-
temp_df %>%
  select(-population, -year) %>%
  nest(temp_year = c(index, temp)) %>%
  mutate(model = map(temp_year, ~ lm(temp ~ index, data = .x)))



##A get the slopes

slopes <-
  temp_model %>%
  mutate(cofes = map(model , tidy)) %>%
  unnest(cofes) %>%
  filter(term == "index") %>%
  mutate(p.value = p.adjust(p.value))



##B plot the slopes

slopes %>%
  filter(country %in% c("Saudi Arabia", "Japan", "Egypt", "Kuwait")) %>%
  # slice_max(order_by = estimate, n = 100) %>%
  ggplot(aes(estimate, p.value, label = city, color = country)) +
  geom_vline(
    xintercept = 0, lty = 2,
    size = 1, alpha = 0.7, color = "gray50") +
  geom_point(alpha = 0.2, size = 2.5, show.legend = FALSE) +
  scale_y_log10() +
  geom_text_repel(size = 3, family = "IBMPlexSans") +
  #facet_wrap(~ country)+
  theme_light(base_family = "IBMPlexSans") +
  theme(strip.text = element_text(family = "IBMPlexSans-Bold", size = 12)) +
  labs(x = "increase in temp per year")


slopes %>% slice_max(estimate, n = 10)











