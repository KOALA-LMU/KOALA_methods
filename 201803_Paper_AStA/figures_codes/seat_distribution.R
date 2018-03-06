
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)
library(png)
library(grid)
theme_set(theme_bw())

# Data and code source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55


# Prepare KOALA logo for branding all plots -------------------------------
img <- readPNG("../figures_codes/Koala_Logo_Schrift.png", info = FALSE)
grob_img <- grobTree(rasterGrob(img, vjust = 0))

# Helper function to brand a ggplot-based plot
# NOTE: This worked fine as one didn't apply the rev_date y axis in the
#       ridgeline plot!
brand_plot <- function(gg, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf) {
  gg + annotation_custom(grob_img, xmax = xmax, ymax = ymax, xmin = xmin, ymin = ymin)
}




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
  return(data.frame("coal_percent" = seat.shares,
                    "fdp_rawPercent" = 100 * dirichlet.draws$fdp,
                    "date" = as.POSIXct(dat)))
}

set.seed(2018)
shares_list <- lapply(surveys$date, function(dat) sim_oneDate(dat))
shares <- bind_rows(shares_list)



# 1) Plot for the last poll before election day ---------------------------
shares_short <- shares %>% filter(date == max(date))

gg_shares <- ggplot(shares_short,
                    aes(x = coal_percent, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(40,57)) +
  xlab("Share of parliament seats") + ylab("Density") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

pdf("../figures/bauer_2013_cdufdp_lastPreelectionPoll.pdf", width = 7, height = 4)
gg_shares
# brand_plot(gg_shares, xmin = 53, xmax = 57.75, ymin = shares_short$date[1] + 0.65)
dev.off()


# 2) Helper function for plot over time -----------------------------------
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


# 2) Plots over time ------------------------------------------------------
# revert time axis
rev_date <- c_trans("reverse", "time")

### plot the CDU/FDP coalition
gg_shares <- ggplot(shares,
       aes(x = coal_percent, y = date, group = date, # basic aesthetics
           fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
           frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(40,57)) +
  scale_y_continuous(trans  = rev_date, breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-09-22")),
                     labels = c("Jan 2013","Apr 2013","Jul 2013","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom")

pdf("../figures/bauer_2013_cdufdp_time.pdf", width = 7.5, height = 6)
gg_shares
dev.off()


### plot only FDP
gg_shares <- ggplot(shares,
                    aes(x = fdp_rawPercent, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>5, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 5, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,10)) +
  scale_y_continuous(trans  = rev_date, breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-09-22")),
                     labels = c("Jan 2013","Apr 2013","Jul 2013","Election day")) +
  xlab("Raw voter share") + ylab("") +
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom")

pdf("../figures/bauer_2013_fdp_rawShare.pdf", width = 7.5, height = 6)
gg_shares + ggtitle("No ned mit 10000 sims")
dev.off()




# Data preparation for comparing observed shares to the probabilities ----
# function to get redistributed shares per date
get_redistributed_CDUFDPshare_oneDate <- function(dat, hurdle = 5) {
  dat <- surveys %>% filter(date == dat) %>% unnest() %>%
    filter(percent >= hurdle & party != "others") %>%
    mutate(percent = percent / sum(percent) * 100) %>%
    filter(party %in% c("cdu","fdp")) %>% select(party,percent)
  if (!("fdp" %in% dat$party))
    dat <- bind_rows(dat,
                     data.frame("party" = "fdp", "percent" = 0, stringsAsFactors = FALSE))
  dat
}

dates <- unique(shares$date)
plot_dat <- data.frame("date" = dates,
                       "cdu_share_raw" = sapply(dates, function(dat) surveys %>% unnest() %>% filter(date == dat & party == "cdu") %>% pull(percent)),
                       "fdp_share_raw" = sapply(dates, function(dat) surveys %>% unnest() %>% filter(date == dat & party == "fdp") %>% pull(percent)),
                       "afd_pir_meanDistToHurdle" = sapply(dates, function(dat) surveys %>% unnest() %>% filter(date == dat & party %in% c("afd","pirates")) %>% mutate(diff = percent - 5) %>% pull(diff) %>% mean()),
                       "cdu_share_redist" = sapply(dates, function(dat) get_redistributed_CDUFDPshare_oneDate(dat) %>% filter(party == "cdu") %>% pull(percent)),
                       "fdp_share_redist" = sapply(dates, function(dat) get_redistributed_CDUFDPshare_oneDate(dat) %>% filter(party == "fdp") %>% pull(percent)),
                       "coal_share_redist" = sapply(dates, function(dat) get_redistributed_CDUFDPshare_oneDate(dat) %>% pull(percent) %>% sum()),
                       "coal_majority_prob" = sapply(dates, function(dat) {
                         sh <- shares %>% filter(date == dat)
                         100 * sum(sh$coal_percent > 50) / nrow(sh)
                       }))



# Plots for the BTW 2013 --------------------------------------------------
### Plot redistributed coalition shares
# a) as a line plot
ggplot(plot_dat, aes(x = date, y = coal_share_redist)) +
  geom_line() +
  geom_hline(yintercept = 50, lty = 2) +
  scale_y_continuous(limits = c(0,100))
# b) as stacked area plot
plot_dat2 <- data.frame("date" = rep(plot_dat$date, times = 2),
                        "share" = c(plot_dat$cdu_share_redist, plot_dat$fdp_share_redist),
                        "party" = rep(c("cdu","fdp"), each = nrow(plot_dat)))
ggplot(plot_dat2, aes(x = date, y = share, fill = party)) +
  geom_area() +
  scale_fill_manual(values = c("black","yellow")) +
  scale_y_continuous(limits = c(0,100)) +
  geom_hline(yintercept = 50, lty = 2, col = "gray")


### Plot coalition probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$coal_majority_prob / 100)
axis_ticks <- c(0,20,40,50,60,80,100)
axis_ticks_minor <- c(10,30,70,90)
axis_labels <- c("0","","20","","40","50","60","","80","","100")
# define color vector for ticks (to gray out ticks without labels)
ticks <- sort(c(axis_ticks, axis_ticks_minor))
skewed_ticks <- 100 * coalishin:::transform_cps(ticks / 100)
ticks_col <- rep("black", length(ticks))
ticks_col[ticks %in% axis_ticks_minor] <- gray(0.7)

ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line() +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks,
                     labels = axis_labels) +
  scale_x_datetime(breaks = as.POSIXct(c("2013-01-01","2013-04-01","2013-07-01","2013-09-22")),
                     labels = c("January","April","July","October")) +
  theme(axis.title.x = element_blank())
