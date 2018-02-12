
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)


# Scrape data -------------------------------------------------------------
# scrape surveys
data <- get_surveys_by() %>%
  mutate(pollster = replace(pollster, pollster == "infratestdimap", "infratest")) %>%
  filter(pollster != "pollytix") %>%
  arrange(pollster)

# pool surveys
dates <- as.Date("2018-01-10")
party_order <- coalishin::lookup_parties %>% filter(id_election == "ltw_by") %>% arrange(position) %>% pull(id_party)
survey_pooled <- lapply(dates, function(date) pool_surveys(data, last_date = as.Date(date))) %>%
  bind_rows() %>%
  slice(order(match(party, party_order))) %>%
  nest(-pollster, -date, -start, -end, -respondents) %>%
  rename(survey = data) %>%
  nest(-pollster) %>%
  rename(surveys = data)


# Estimate probabilities --------------------------------------------------
survey <- survey_pooled %>% unnest() %>% unnest()

# simulate seat distributions
nsim <- 1000
correction <- 0.005
dirichlet.draws <- coalitions::draw_from_posterior(survey = survey, nsim = nsim, correction = correction)
seat.distributions <- coalitions::get_seats(dirichlet.draws, survey = survey, 
                                            distrib.fun = get(lookup_elections$distribution_function[lookup_elections$id == "ltw_by"]),
                                            n_seats = lookup_elections$parl_seats[lookup_elections$id == "ltw_by"])
# retrieve coalitions for which the party ordering is relevant (i.e. coalitions in lookup_coalitions that are stated multiple times in different ordering, e.g. "cdu|spd" and "spd|cdu")
coals <- lookup_coalitions %>% filter(id_election == "ltw_by") %>% pull(id_coalition)
coals_sorted <- sapply(coals, function(x) paste(sort(strsplit(x, "\\|")[[1]]), collapse = "|"), USE.NAMES = FALSE)
if (any(table(coals_sorted) > 1)) {
  name <- names(table(coals_sorted))[table(coals_sorted) > 1]
  strongest_party_coals <- coals[which(coals_sorted == name)]
} else {
  strongest_party_coals <- NULL
}
# calculate coalition probabilities
parties <- c("csu","fdp")
res_all <- calc_allCoalProbs(seat.distributions, parties, dirichlet.draws, strongest_party_coals = strongest_party_coals)
shares <- res_all$shares_perSimulation
shares <- shares %>% mutate(pollster = "pooled", date = dates) %>% select(pollster, date, everything())


# Plot --------------------------------------------------------------------
pdf("../figures/bauer_seatDist.pdf", width = 20, height = 4)
plot_seatDist_density(shares, "pooled", coal = "csu|fdp", parl_seats = lookup_elections$parl_seats[lookup_elections$id == "ltw_by"],
                      mark_CI = FALSE, base_size = 40)
dev.off()