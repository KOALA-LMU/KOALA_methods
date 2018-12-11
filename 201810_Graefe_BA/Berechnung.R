
library(dplyr); library(tidyr); library(magrittr)

n_votes <- 1000
nsim <- 10000
n_seats <- 598

# Version 1 ---------------------------------------------------------------
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


# Version 2 ---------------------------------------------------------------
stimmen <- data.frame("A" = seq(46, 42, by = -0.5),
                      "E" = seq(3,  7,  by = 0.5),
                      "B" = 22,
                      "C" = 13,
                      "D" = 12,
                      "others" = 6,
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



# Version 3 ---------------------------------------------------------------
stimmen <- data.frame("A" = seq(46, 42, by = -0.5),
                      "E" = seq(3,  7,  by = 0.5),
                      "B" = 22,
                      "C" = 11,
                      "D" = 10,
                      "others" = 8,
                      "pollster" = "mockup",
                      "date" = seq.Date(as.Date("2018-10-15"), as.Date("2018-10-24"), length.out = 9),
                      "start" = "2018-10-15",
                      "end" = "2018-10-15",
                      "respondents" = n_votes)

stimmen %<>% gather("party", "percent", -pollster, -date, -start, -end, -respondents) %>%
  mutate(votes = n_votes * percent / 100) %>%
  nest(-pollster, -date, -start, -end, -respondents, .key = "survey")

# Probabilities for a majority of A_E
for (i in 1:nrow(stimmen)) {
  set.seed(2018)
  message(paste0("Wahrscheinlichkeit für Szenario ",i,":"))
  prob <- stimmen %>% slice(i) %>%
    coalitions::get_probabilities(nsim = nsim, coalitions = list(c("A","E"),c("A"))) %>%
    unnest() %>%
    pull(probability)
  message(paste(paste0(c("A_E","A"), ": ", prob, "%"), collapse = ", "))
}

# Credibility intervals
for (i in 3:7) {
  set.seed(2018)
  message(paste0("Wahrscheinlichkeit für Szenario ",i,":"))
  draws <- stimmen %>% slice(i) %>% unnest() %>%
    coalitions::draw_from_posterior(nsim = nsim, correction = 0.005)
  seats <- coalitions::get_seats(draws, distrib.fun = sls, samplesize = n_votes, n_seats = n_seats) %>%
    mutate(seats = seats / n_seats) %>%
    spread(party, seats)
  if (!("E" %in% colnames(seats))) {
    seats$E <- 0
  } else
    seats$E[is.na(seats$E)] <- 0
  message("Stimmanteil A:")
  message(paste(round(quantile(draws$A, probs = c(0.025, 0.975)), 3), collapse = " - "))
  message("Sitzanteil A:")
  message(paste(round(quantile(seats$A, probs = c(0.025, 0.975)), 3), collapse = " - "))
  message("Stimmanteil A+E:")
  message(paste(round(quantile(draws$A + draws$E, probs = c(0.025, 0.975)), 3), collapse = " - "))
  message("Sitzanteil A+E:")
  message(paste(round(quantile(seats$A + seats$E, probs = c(0.025, 0.975)), 3), collapse = " - "))
}



