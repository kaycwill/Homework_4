## Section 1 code (read in data) ###
####################################

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(stats)
library(broom)
library(scales)
library(purrr)
library(forcats)
library(ggplot2)

homicides <- read.csv("data/homicide-data.csv")

head(homicides)

## Section 2 code (create city_name) ##
#######################################

homicides <- homicides %>%
  unite(city_name, city, state, sep = ", ", remove = FALSE)

head(homicides)

## Section 3 code (create dataframe) ##
#######################################

homicides <- homicides %>% 
  select(city_name, disposition) %>% 
  mutate(unsolved_homicides = str_detect(disposition, 
                                          c("Closed without arrest|Open/No arrest"))) %>%
  rename(total_homicides = disposition)
           

unsolved <- homicides %>%  
  group_by(city_name) %>% 
  summarise(total_homicides = sum(!is.na(total_homicides)),
            unsolved_homicides = sum((unsolved_homicides == "TRUE")))

## Section 4 code (prop.test for Baltimore) ##
##############################################

homicide_prop <- unsolved %>% 
  filter(city_name == "Baltimore, MD")

baltimore_homicides <- prop.test(x = homicide_prop$unsolved_homicides, 
                                 n = homicide_prop$total_homicides)

tidy(baltimore_homicides)

## Section 5 code (prop.test for all cities) ##
###############################################

all_homicides <- map2(unsolved$unsolved_homicides, unsolved$total_homicides, .f = prop.test)

all_homicides2 <- map_df(all_homicides, tidy)  

unnest(all_homicides2, .drop = TRUE)

## Section 6 code (creating a graph) ##
#######################################

unsolved2 <- unsolved %>% 
  mutate(estimate = unsolved_homicides/total_homicides)

prop_df <- full_join(unsolved2, all_homicides2)

prop_df %>% 
  mutate(city_name = fct_reorder(city_name, estimate)) %>%
  filter(city_name != "Tulsa, AL") %>% 
  ggplot(aes(x = city_name, y = estimate)) + 
  geom_point(color = "white") +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high), width = 0, color = "white", alpha = 0.5) + 
  coord_flip() + 
  ggtitle("Unsolved homicides by city", "Bars show 95% confidence interval") +
  labs(x = "", y = "Percents of homicides that are unsolved") + 
  scale_y_continuous(labels = percent) + 
  theme_dark()