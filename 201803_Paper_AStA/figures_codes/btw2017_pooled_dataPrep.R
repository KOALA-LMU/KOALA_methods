
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)


# Create data -------------------------------------------------------------
### Scrape data from before the German federal election 2017
surveys <- coalitions:::.pollster_df %>%
  mutate(surveys = map(address, scrape_wahlrecht), surveys = map(.x = surveys, collapse_parties)) %>% 
  select(-one_of("address"))
date_range <- as.Date(c("2016-09-25","2017-09-24"))
surveys <- surveys %>% unnest() %>% filter(date >= date_range[1] - 14 & date <= date_range[2]) %>%
  nest(-pollster, .key = "surveys")

dates <- surveys %>% unnest() %>% filter(date >= date_range[1]) %>% pull(date) %>% unique() %>% sort(decreasing = TRUE)
pooled_list <- lapply(dates, function(dat) {
  pool_surveys(surveys, last_date = as.Date(dat)) %>%
    nest(-pollster, -date, -start, -end, -respondents, .key = "survey")
})

pool <- bind_rows(pooled_list)

### Simulate shares
nsim <- 10000
# Helper function
sim_oneDate <- function(dat) {
  print(dat)
  pool_dat <- pool %>% filter(date == as.Date(dat)) %>% unnest()
  dirichlet.draws <- coalitions::draw_from_posterior(survey = pool_dat, nsim = nsim)
  seat.distributions <- coalitions::get_seats(dirichlet.draws, survey = pool_dat, 
                                              distrib.fun = coalitions::sls, n_seats = 598)
  get_seatShares_coal <- function(i, parties) { sum(seat.distributions$seats[seat.distributions$sim == i & seat.distributions$party %in% parties]) / 598 * 100 }
  
  return(data.frame("coal_percent_cdufdp" = sapply(1:nsim, function(i) { get_seatShares_coal(i, c("cdu","fdp")) }),
                    "coal_percent_spdleftgreens" = sapply(1:nsim, function(i) { get_seatShares_coal(i, c("spd","left","greens")) }),
                    "fdp_rawPercent" = 100 * dirichlet.draws$fdp,
                    "afd_rawPercent" = 100 * dirichlet.draws$afd,
                    "left_rawPercent" = 100 * dirichlet.draws$left,
                    "greens_rawPercent" = 100 * dirichlet.draws$greens,
                    "date" = as.POSIXct(as.Date(dat))))
}

set.seed(2018)
shares_list <- lapply(pool$date, function(dat) sim_oneDate(dat))
shares <- bind_rows(shares_list)


### Data preparation for comparing observed shares to the probabilities
# function to get redistributed shares per date
get_redistributed_partyShares_oneDate <- function(dat, parties, hurdle = 5) {
  dat <- pool %>% filter(date == as.Date(dat)) %>% unnest() %>%
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
  sh <- shares %>% filter(as.Date(date) == as.Date(dat))
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
                       "cdu_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "cdu") %>% pull(percent)),
                       "spd_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "spd") %>% pull(percent)),
                       "fdp_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "fdp") %>% pull(percent)),
                       "afd_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "afd") %>% pull(percent)),
                       "left_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "left") %>% pull(percent)),
                       "greens_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == as.Date(dat) & party == "greens") %>% pull(percent)),
                       "cdu_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "cdu") %>% pull(percent)),
                       "spd_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "spd") %>% pull(percent)),
                       "fdp_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "fdp") %>% pull(percent)),
                       "afd_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "afd") %>% pull(percent)),
                       "left_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "left") %>% pull(percent)),
                       "greens_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "greens") %>% pull(percent)),
                       "cdufdp_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, c("cdu","fdp")) %>% pull(percent) %>% sum()),
                       "spdleftgreens_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, c("spd","left","greens")) %>% pull(percent) %>% sum()),
                       "cdufdp_majority_prob" = sapply(dates, function(dat) { get_maj_prob(as.Date(dat), "coal_percent_cdufdp") }),
                       "spdleftgreens_majority_prob" = sapply(dates, function(dat) { get_maj_prob(as.Date(dat), "coal_percent_spdleftgreens") }),
                       "fdp_passing_prob" = sapply(dates, function(dat) get_pass_prob(as.Date(dat), "fdp_rawPercent")),
                       "afd_thirdParty_prob" = sapply(dates, function(dat) {
                         sh <- shares %>% filter(as.Date(date) == as.Date(dat))
                         afd_thirdParty <- apply(sh, 1, function(x) which.max(x[c("afd_rawPercent","fdp_rawPercent","left_rawPercent","greens_rawPercent")]) == 1)
                         100 * sum(afd_thirdParty) / length(afd_thirdParty)
                       }))


### Save prepared data
save(list = ls(), file = "data_prepared/btw2017_pooled_data.RData")
