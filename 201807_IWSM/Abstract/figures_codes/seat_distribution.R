
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
theme_set(theme_bw())

# Data and code source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55

# Create data -------------------------------------------------------------
### Scrape data from before the German federal election 2013
address_forsa2013 <- "http://www.wahlrecht.de/umfragen/forsa/2013.htm"
surveys <- coalitions:::.pollster_df %>% filter(pollster == "forsa") %>% mutate(address = address_forsa2013) %>%
  mutate(surveys = map(address, scrape_wahlrecht), surveys = map(.x = surveys, collapse_parties)) %>% 
  select(-one_of("address"))
surveys <- surveys %>% unnest() %>% filter(date >= "2013-01-01" & date <= "2017-09-24")

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
  return(data.frame("percent" = seat.shares,
                    "date" = as.POSIXct(dat)))
}

set.seed(2018)
shares_list <- lapply(surveys$date, function(dat) sim_oneDate(dat))
shares <- bind_rows(shares_list)

# Helper function for plot over time --------------------------------------
## define function to reverse date time scale
# source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55
library(scales)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inv, breaks, format)
}


# Plot over time ----------------------------------------------------------
library(ggridges)

# revert time axis
rev_date <- c_trans("reverse", "time")

## plot
gg_shares <- ggplot(shares,
       aes(x = percent, y = date, group = date, # basic aesthetics
           fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
           frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(trans  = rev_date, breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-09-22")),
                     labels = c("Jan 2013","Apr 2013","Jul 2013","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom")

pdf("../figures/bauer_seatDist_time.pdf", width = 7, height = 6)
gg_shares
dev.off()
