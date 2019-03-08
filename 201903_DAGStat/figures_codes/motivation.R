
library(coalitions)
library(tidyverse)
theme_set(theme_bw())


# 2013 last FORSA pre-election poll ---------------------------------------
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-09-20") %>%
  unnest()

gg_survey(survey, annotate_bars = FALSE) +
  ylab("Voter share in %") +
  ggtitle("Forsa opinion poll, 20.09.2013") +
  theme_bw(base_size = 20) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa.pdf", height = 5, width = 7)
  