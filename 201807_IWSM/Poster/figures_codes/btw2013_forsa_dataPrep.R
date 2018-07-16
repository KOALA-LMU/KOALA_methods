
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)


# Create data -------------------------------------------------------------
### Scrape data from before the German federal election 2013
address_forsa2013 <- "http://www.wahlrecht.de/umfragen/forsa/2013.htm"
surveys <- coalitions:::.pollster_df %>% filter(pollster == "forsa") %>% mutate(address = address_forsa2013) %>%
  mutate(surveys = map(address, scrape_wahlrecht), surveys = map(.x = surveys, collapse_parties)) %>% 
  select(-one_of("address"))
surveys <- surveys %>% unnest() %>% filter(date >= "2013-01-01" & date <= "2013-09-22")

### Simulate shares
nsim <- 10000
# Helper function
sim_oneDate <- function(dat) {
  print(dat)
  survey <- surveys %>% filter(date == dat) %>%
    filter(row_number() == 1) %>% # for the case when two surveys of one institute are published on the same day
    unnest()
  dirichlet.draws <- coalitions::draw_from_posterior(survey = survey, nsim = nsim)
  seat.distributions <- coalitions::get_seats(dirichlet.draws, survey = survey, 
                                              distrib.fun = coalitions::sls, n_seats = 598)
  seat.shares <- sapply(1:nsim, function(i) {
    sum(seat.distributions$seats[seat.distributions$sim == i & seat.distributions$party %in% c("cdu","fdp")]) / 598 * 100
  })
  return(data.frame("coal_percent" = seat.shares,
                    "fdp_rawPercent" = 100 * dirichlet.draws$fdp,
                    "date" = as.POSIXct(dat)))
}

set.seed(2018)
shares_list <- lapply(surveys$date, function(dat) sim_oneDate(dat))
shares <- bind_rows(shares_list)


### Data preparation for comparing observed shares to the probabilities
# function to get redistributed shares per date
get_redistributed_partyShares_oneDate <- function(dat, parties, hurdle = 5) {
  dat <- surveys %>% filter(date == as.Date(dat)) %>% unnest() %>%
    filter(percent >= hurdle & party != "others") %>%
    mutate(percent = percent / sum(percent) * 100) %>%
    filter(party %in% parties) %>% select(party,percent)
  if (!all(parties %in% dat$party)) # if any party didn't pass the 5% hurdle
    for (p in parties[!(parties %in% dat$party)]) {
      dat <- bind_rows(dat,
                       data.frame("party" = p, "percent" = 0, stringsAsFactors = FALSE))
    }
  dat
}
# function to extract coalition majority probability
get_maj_prob <- function(dat, coal_column) {
  sh <- shares %>% filter(date == dat)
  100 * sum(sh[,coal_column] > 50) / nrow(sh)
}
# function to extract the probability of a party passing the 5% hurdle
get_pass_prob <- function(dat, party_column) {
  sh <- shares %>% filter(as.Date(date) == as.Date(dat))
  100 * sum(sh[,party_column] >= 5) / nrow(sh)
}
# function call
dates <- unique(shares$date)
plot_dat <- data.frame("date" = dates,
                       "fdp_share_raw" = sapply(dates, function(dat) surveys %>% unnest() %>% filter(date == as.Date(dat) & party == "fdp") %>% pull(percent)),
                       "cdufdp_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, c("cdu","fdp")) %>% pull(percent) %>% sum()),
                       "cdufdp_majority_prob" = sapply(dates, function(dat) { get_maj_prob(dat, "coal_percent") }),
                       "fdp_passing_prob" = sapply(dates, function(dat) get_pass_prob(dat, "fdp_rawPercent")))


### Save prepared data
save(list = ls(), file = "data_prepared/btw2013_forsa_data.RData")
