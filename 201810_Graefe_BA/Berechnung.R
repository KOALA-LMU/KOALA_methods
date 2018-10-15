
library(dplyr); library(tidyr); library(magrittr)

n_votes <- 1000
nsim <- 10000

stimmen <- data.frame("A" = seq(50, 46, by = -0.5),
                      "E" = seq(3,  7,  by = 0.5),
                      "B" = 22,
                      "C" = 13,
                      "D" = 12,
                      "pollster" = "mockup",
                      "date" = seq.Date(as.Date("2018-10-15"), as.Date("2018-10-24"), length.out = 9),
                      "start" = "2018-10-15",
                      "end" = "2018-10-15",
                      "respondents" = n_votes)

stimmen %<>% gather("party", "percent", -pollster, -date, -start, -end, -respondents) %>%
  mutate(votes = n_votes * percent / 100) %>%
  nest(-pollster, -date, -start, -end, -respondents, .key = "survey")

for (i in 1:nrow(stimmen)) {
  set.seed(2018)
  message(paste0("Wahrscheinlichkeit für Szenario ",i,":"))
  prob <- stimmen %>% slice(i) %>%
    coalitions::get_probabilities(nsim = nsim, coalitions = list(c("A","E"),c("A"))) %>%
    unnest() %>%
    filter(coalition == "A_E") %>%
    pull(probability)
  message(paste0(prob, "%"))
}
