
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)
source("helpers.R")
theme_set(theme_bw())

# revert time axis
rev_date <- c_trans("reverse", "time")


# Data and code source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55


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
  pool_dat <- pool %>% filter(date == dat) %>% unnest()
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
                    "date" = as.POSIXct(dat)))
}

set.seed(2018)
shares_list <- lapply(pool$date, function(dat) sim_oneDate(dat))
shares <- bind_rows(shares_list)


### Data preparation for comparing observed shares to the probabilities
# function to get redistributed shares per date
get_redistributed_partyShares_oneDate <- function(dat, parties, hurdle = 5) {
  dat <- pool %>% filter(date == dat) %>% unnest() %>%
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
  sh <- shares %>% filter(date == dat)
  100 * sum(sh[,party_column] >= 5) / nrow(sh)
}
# function call
dates <- unique(shares$date)
plot_dat <- data.frame("date" = dates,
                       "cdu_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "cdu") %>% pull(percent)),
                       "spd_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "spd") %>% pull(percent)),
                       "fdp_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "fdp") %>% pull(percent)),
                       "afd_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "afd") %>% pull(percent)),
                       "left_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "left") %>% pull(percent)),
                       "greens_share_raw" = sapply(dates, function(dat) pool %>% unnest() %>% filter(date == dat & party == "greens") %>% pull(percent)),
                       "cdu_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "cdu") %>% pull(percent)),
                       "spd_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "spd") %>% pull(percent)),
                       "fdp_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "fdp") %>% pull(percent)),
                       "afd_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "afd") %>% pull(percent)),
                       "left_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "left") %>% pull(percent)),
                       "greens_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, "greens") %>% pull(percent)),
                       "cdufdp_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, c("cdu","fdp")) %>% pull(percent) %>% sum()),
                       "spdleftgreens_share_redist" = sapply(dates, function(dat) get_redistributed_partyShares_oneDate(dat, c("spd","left","greens")) %>% pull(percent) %>% sum()),
                       "cdufdp_majority_prob" = sapply(dates, function(dat) { get_maj_prob(dat, "coal_percent_cdufdp") }),
                       "spdleftgreens_majority_prob" = sapply(dates, function(dat) { get_maj_prob(dat, "coal_percent_spdleftgreens") }),
                       "fdp_passing_prob" = sapply(dates, function(dat) get_pass_prob(dat, "fdp_rawPercent")),
                       "afd_thirdParty_prob" = sapply(dates, function(dat) {
                         sh <- shares %>% filter(date == dat)
                         afd_thirdParty <- apply(sh, 1, function(x) which.max(x[c("afd_rawPercent","fdp_rawPercent","left_rawPercent","greens_rawPercent")]) == 1)
                         100 * sum(afd_thirdParty) / length(afd_thirdParty)
                       }))

### prepare skewed probability scale for the majority probability plots
axis_ticks <- c(0,20,40,50,60,80,100)
axis_ticks_minor <- c(10,30,70,90)
axis_labels <- c("0%","","20%","","40%","50%","60%","","80%","","100%")
# define color vector for ticks (to gray out ticks without labels)
ticks <- sort(c(axis_ticks, axis_ticks_minor))
skewed_ticks <- 100 * coalishin:::transform_cps(ticks / 100)
ticks_col <- rep("black", length(ticks))
ticks_col[ticks %in% axis_ticks_minor] <- gray(0.7)




# Development of (pooled) raw poll shares ---------------------------------
# prepare data for using the plotting function
plot_dat_raw <- surveys %>% unnest() %>% bind_rows(pool)

# plot
lk <- coalishin::lookup_parties %>% filter(id_election == "btw")
partycols <- lk$col
names(partycols) <- lk$id_party
partycols["fdp"] <- lk %>% filter(id_party == "fdp") %>% pull(colDark)

gg <- coalishin::plot_pooledSurvey_byTime(plot_dat_raw, election = "btw", plot_intervals = FALSE, partycols = partycols,
                                          hline = NULL)
pdf("../figures/2017_pooled_rawShares.pdf", width = 7.5, height = 3.5)
gg + 
  scale_y_continuous(name = "Raw voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","5%","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_date(breaks = as.Date(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
               minor_breaks = NULL,
               labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  scale_color_manual(name = "Parties", values = partycols,
                     labels = c("Union","SPD","AfD","FDP","Left","Greens","Others")) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_hline(yintercept = 5, lty = 2, lwd = 1, col = "gray") +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank())
dev.off()



# CDU/FDP majority --------------------------------------------------------
### 1) Redistributed raw shares
gg <- ggplot(plot_dat, aes(x = date, y = cdufdp_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(name = "Redistributed voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 25, 5.5, 15), units = "pt"))
pdf("../figures/2017_pooled_cdufdp_rawSharesRedist.pdf", width = 9, height = 3)
gg
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$cdufdp_majority_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "Seat majority probability") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 25, 5.5, 4), units = "pt"))
pdf("../figures/2017_pooled_cdufdp_prob.pdf", width = 9, height = 3)
gg
dev.off()

### 3) ridgeline plot
gg_shares <- ggplot(shares,
                    aes(x = coal_percent_cdufdp, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(35,55)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2017_pooled_cdufdp_ridgeline.pdf", width = 10, height = 8)
gg_shares
dev.off()





# FDP passing hurdle-------------------------------------------------------
# ### 1) Raw voter shares
# gg <- ggplot(plot_dat, aes(x = date, y = fdp_share_raw)) +
#   geom_hline(yintercept = 5, lty = 2, lwd = 1.2, col = "gray") +
#   geom_line(lwd = 1.3) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,10), name = "Raw voter share") +
#   scale_x_datetime(breaks = as.POSIXct(c("2016-10-01","2017-01-01","2017-04-01","2017-07-01","2017-09-24")),
#                    labels = c("Oct 2016","Jan 2017","Apr 2017","Jul 2017","Election day")) +
#   theme_bw(base_size = 20) +
#   theme(axis.title.x = element_blank())
# pdf("../figures/2017_pooled_fdp_rawShares.pdf", width = 7.5, height = 3.5)
# gg
# dev.off()
# 
# ### 2) Majority probabilities
# plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$fdp_passing_prob / 100)
# gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
#   geom_line(lwd = 1.3) +
#   scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
#                      labels = axis_labels, name = "seat majority probability") +
#   scale_x_datetime(breaks = as.POSIXct(c("2016-10-01","2017-01-01","2017-04-01","2017-07-01","2017-09-24")),
#                    labels = c("Oct 2016","Jan 2017","Apr 2017","Jul 2017","Election day")) +
#   theme_bw(base_size = 20) +
#   theme(axis.title.x = element_blank())
# pdf("../figures/2017_pooled_fdp_PassingProb.pdf", width = 7.5, height = 3.5)
# gg
# dev.off()
# 
# ### 3) ridgeline plot
# gg_shares <- ggplot(shares,
#                     aes(x = fdp_rawPercent, y = date, group = date, # basic aesthetics
#                         fill = ifelse(..x..>5, "yes", "no"), # "cut-off" gradient
#                         frame = date, cumulative = TRUE)) + # aesthetics for animation
#   geom_density_ridges_gradient(
#     scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
#   scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
#   geom_vline(xintercept = 5, lty = 1, lwd = 1.2, col = "grey90") +
#   scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,10)) +
#   scale_y_continuous(trans  = rev_date, breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-09-22")),
#                      labels = c("Jan 2013","Apr 2013","Jul 2013","Election day")) +
#   xlab("Simulated raw voter share") + ylab("") +
#   theme_bw(base_size = 25) +
#   theme(legend.position = "bottom")
# 
# pdf("../figures/2017_pooled_fdp_ridgeline.pdf", width = 7.5, height = 10)
# gg_shares
# dev.off()



# SPD/Left/Greens majority --------------------------------------------------
### 1) Redistributed raw shares
gg <- ggplot(plot_dat, aes(x = date, y = spdleftgreens_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(name = "Redistributed voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 25, 5.5, 15), units = "pt"))
pdf("../figures/2017_pooled_spdleftgreens_rawSharesRedist.pdf", width = 9, height = 3)
gg
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$spdleftgreens_majority_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "Seat majority probability") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 25, 5.5, 4), units = "pt"))
pdf("../figures/2017_pooled_spdleftgreens_prob.pdf", width = 9, height = 3)
gg
dev.off()

### 3) ridgeline plot
gg_shares <- ggplot(shares,
                    aes(x = coal_percent_spdleftgreens, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(35,55)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2017_pooled_spdleftgreens_ridgeline.pdf", width = 10, height = 8)
gg_shares
dev.off()


# AfD third biggest party -------------------------------------------------
### 1) Raw voter shares
parties <- c("afd","fdp","left","greens")
plot_dat_raw <- surveys %>% unnest() %>% bind_rows(pool) %>% unnest() %>%
  filter(party %in% parties) %>% nest(-pollster, -date, -start, -end, -respondents, .key = "survey")
lk <- coalishin::lookup_parties %>% filter(id_election == "btw")
partycols <- lk$col
names(partycols) <- lk$id_party
partycols["fdp"] <- lk %>% filter(id_party == "fdp") %>% pull(colDark)
partycols <- partycols[names(partycols) %in% parties]
gg <- coalishin::plot_pooledSurvey_byTime(plot_dat_raw, election = "btw", plot_intervals = FALSE, partycols = partycols,
                                          hline = NULL)
pdf("../figures/2017_pooled_afd_rawShares.pdf", width = 9, height = 4)
gg + 
  scale_y_continuous(name = "Raw voter share",
                     limits = c(0,15),
                     breaks = seq(0,15,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","5%","10%","15%")) +
  scale_x_date(breaks = as.Date(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
               minor_breaks = NULL,
               labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  scale_color_manual(name = "Parties", values = partycols,
                     labels = c("AfD","FDP","Left","Greens")) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_hline(yintercept = 5, lty = 2, lwd = 1, col = "gray") +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        legend.position = "bottom",
        plot.margin = unit(c(0, 25, 5.5, 15), units = "pt"))
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$afd_thirdParty_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "Seat majority probability") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0, 25, 5.5, 4), units = "pt"))
pdf("../figures/2017_pooled_afd_thirdPartyProb.pdf", width = 9, height = 3)
gg
dev.off()
