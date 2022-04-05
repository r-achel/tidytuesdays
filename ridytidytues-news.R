# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2022-04-05')
#tuesdata <- tidytuesdayR::tt_load(2022, week = 14)

news_orgs <- tuesdata$news_orgs

# Or read in the data manually

news_orgs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-05/news_orgs.csv')

#######################################################

library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)



news_orgs_tidy <- news_orgs %>%
  select(year_founded, primary_language) %>%
  mutate(age = case_when(year_founded > 2019 ~ "0-2 years",
                         year_founded >= 2016 & year_founded < 2019 ~ "3-5 years",
                         year_founded >= 2011 & year_founded < 2016 ~ "6-10 years",
                         year_founded <2011 ~ "11+ years")
  ) %>%
  mutate(age = factor(age, levels = c("11+ years", "6-10 years", "3-5 years", "0-2 years"))) %>%
  mutate(tidy_language = case_when(
    primary_language == "English" ~ "English",
    primary_language == "Spanish" ~ "Spanish",
    primary_language %in% c("Bilingual (Spanish & English)", "Spanish, English") ~ "Bilingual"
  )) %>%
  mutate(tidy_language = factor(tidy_language, levels = c("Bilingual", "Spanish", "English"))) %>%
  filter(!is.na(tidy_language)) %>%
  filter(!is.na(age)) %>%
  group_by(age, tidy_language)%>%
  count() %>%
  ungroup() %>%
  group_by(age) %>%
  arrange(desc(tidy_language))%>%
  mutate(percentage = round(n/sum(n), 3)*100,
         lab.pos = cumsum(percentage)-.5*percentage)

  ggplot(news_orgs_tidy, aes(age, percentage, fill = tidy_language)) +
  geom_bar(stat="identity") +
    scale_fill_brewer(palette = "Accent") +
    labs(fill = "Language", title = "Digital Futures: Bilingual News Organisations Are On The Rise", 
         subtitle = "Data: Project Oasis | Graphic: Rachel Williams (twitter.com/sciwithrach)", 
         x= "Organisation Age", y="Percentage (%)") +
    geom_text(aes(y=c(50, 50, 50, 50, 98, 96.5, 97, 90, 101, 101, 101, 101), 
                  label = paste(percentage, "%", sep = "")), col=c("darkorange4", "darkorange4", "darkorange4", "darkorange4", "mediumorchid4", "mediumorchid4", "mediumorchid4", "mediumorchid4", "green4", "green4", "green4", "green4"),
              size = 4, family = "Montserrat") +
 #     theme_void() +
    theme(
      text = element_text(family="Montserrat", color = "darkorange4"),
      plot.background = element_rect(fill = "linen"),
      axis.title.x = element_text(),
      axis.ticks = element_blank(),
      axis.text.x = element_text(size=rel(1.2), color = "darkorange4", vjust=5),
      axis.text.y = element_blank(),
      axis.title.y = element_text(vjust = 0.1),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major = element_line(color="linen", linetype = "dashed"),
      panel.grid.minor = element_blank(),
      axis.line = element_blank(),
      legend.background = element_rect(color="bisque")
    )
      
    )
    

#################### CODE GRAVEYARD ####################
distribution_split <- unique(unlist(str_split(news_orgs$distribution, ", ")))

ypos = c(0.5, 0.9, 1.1)
ypos= c(50, 50, 50, 50, 98.15 97.35 97.55 91.70, 100, 100, 100, 100)

    coord_polar(theta="y") +
  xlim(.2, 2.5) +
  facet_grid(cols=vars(age), switch="x") +
  theme_void() +
  scale_fill_brewer(palette = "Accent") +
  geom_text(aes(y=lab.pos, label = paste(percentage, "%", sep = "")), col="black")
  
  mutate(dist_split = strsplit(distribution, ", ")) %>%
  unnest(dist_split) %>%
  pivot_wider(names_from = dist_split, names_prefix = "distribution_", values_from = dist_split, values_fill=0, values_fn=length) %>%
  group_by(age) %>%
  summarise(email_distribution = sum(distribution_Email))
  



  separate(distribution, sep = ", ", into = distribution_split) %>%
  colnames()

news_orgs %>%
  mutate(distribution_n = str_count(distribution, ",")) %>%
  group_by(year_founded) %>%
  mutate(mean_distribution_n = mean(distribution_n, na.rm=T)) %>%
  ggplot(aes(year_founded, mean_distribution_n)) +
  geom_point() +
  facet_wrap(vars(total_employees))

  ggplot(aes(distribution_n)) +
  geom_histogram() +
  facet_wrap(vars(year))


head(news_orgs)
head(news_orgs$paywall_or_gateway)

news_orgs %>%
  ggplot(aes(year_founded)) +
  geom_histogram() +
  facet_wrap(vars(paywall_or_gateway))

news_orgs %>%
  ggplot(aes(as.numeric(total_employees))) +
  geom_histogram() +
  facet_wrap(vars(primary_language))

news_orgs %>%
  count(owner) %>%
  arrange(desc(n))

news_orgs %>%
  count(is_owner_founder)

news_orgs %>%
  separate(col=distribution, into = c(), sep=", ")