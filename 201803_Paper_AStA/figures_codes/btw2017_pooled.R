
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)
source("helpers.R")
theme_set(theme_bw())
library(patchwork)

load("data_prepared/btw2017_pooled_data.RData")

# revert time axis
rev_date <- c_trans("reverse", "time")

# Data and code source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55

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
pdf("../figures/2017_pooled_rawShares.pdf", width = 8, height = 3.5)
gg +
  scale_y_continuous(name = "Reported party share",
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
gg_cdufdp_raw <- ggplot(plot_dat, aes(x = date, y = cdufdp_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(name = "Joint voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","","","","Apr 2017","","","","","","Election day")) +
  # theme_bw(base_size = 24) +
  theme(
    axis.title.x = element_blank(),
    plot.margin = unit(c(0, 37, 5.5, 20.5), units = "pt"),
    axis.text    = element_text(size = rel(1.2)),
    axis.title   = element_text(size = rel(1.3)))

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$cdufdp_majority_prob / 100)
gg_cdufdp_prob <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "POE") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","","","","Apr 2017","","","","","","Election day")) +
  # theme_bw(base_size = 24) +
  theme(
    axis.title.x = element_blank(),
    plot.margin = unit(c(0, 37, 5.5, 20.5), units = "pt"),
    axis.text    = element_text(size = rel(1.2)),
    axis.title   = element_text(size = rel(1.3)))

### 3) ridgeline plot
gg_cdufdp_ridge <- ggplot(shares,
                    aes(x = coal_percent_cdufdp, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(35,55)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  # theme_bw(base_size = 29) +
  theme(
    legend.position = "right",
    plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"),
    axis.text    = element_text(size = rel(1.3)),
    axis.title   = element_text(size = rel(1.4)),
    legend.title = element_text(size = rel(1.4)),
    legend.text = element_text(size = rel(1.3)))


p_cdufdp <- ((gg_cdufdp_raw + gg_cdufdp_prob + plot_layout(ncol=1)) -
  gg_cdufdp_ridge)  + plot_layout(ncol = 2, widths=c(2, 3))

ggsave("../figures/cdufdp_2017_joint.pdf", p_cdufdp, width=13, height=6)



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
### 1) Redistributed party shares
gg_spd_raw <- ggplot(plot_dat, aes(x = date, y = spdleftgreens_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(name = "Joint voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  # theme_bw(base_size = 24) +
  theme(
    axis.title.x = element_blank(),
    plot.margin = unit(c(0, 37, 5.5, 20.5), units = "pt"),
    axis.text    = element_text(size = rel(1.2)),
    axis.title   = element_text(size = rel(1.3)))

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$spdleftgreens_majority_prob / 100)
gg_spd_prob <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "POE") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  # theme_bw(base_size = 24) +
  theme(
    axis.title.x = element_blank(),
    plot.margin = unit(c(0, 37, 5.5, 20.5), units = "pt"),
    axis.text    = element_text(size = rel(1.2)),
    axis.title   = element_text(size = rel(1.3)))

### 3) ridgeline plot
gg_spd_ridge <- ggplot(shares,
                    aes(x = coal_percent_spdleftgreens, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(35,55)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  # theme_bw(base_size = 29) +
  theme(
    legend.position = "right",
    plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"),
    axis.text    = element_text(size = rel(1.3)),
    axis.title   = element_text(size = rel(1.4)),
    legend.title = element_text(size = rel(1.4)),
    legend.text = element_text(size = rel(1.3)))


p_spd <- ((gg_spd_raw + gg_spd_prob + plot_layout(ncol=1)) -
  gg_spd_ridge)  + plot_layout(ncol = 2, widths=c(2, 3))

ggsave("../figures/spd_2017_joint.pdf", p_spd, width=13, height=6)


# AfD third biggest party -------------------------------------------------
### 1) Reported party shares
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
pdf("../figures/2017_pooled_afd_rawShares.pdf", width = 9, height = 3)
gg +
  scale_y_continuous(name = "Reported party share",
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
        legend.position = "right",
        plot.margin = unit(c(0, 25, 5.5, 11.9), units = "pt"))
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$afd_thirdParty_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks, minor_breaks = NULL,
                     labels = axis_labels, name = "POE") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2016,3),rep(2017,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"24"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2016","","","Jan 2017","","","Apr 2017","","","Jul 2017","","","Election day")) +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 145, 5.5, 3), units = "pt"))
pdf("../figures/2017_pooled_afd_thirdPartyProb.pdf", width = 9, height = 3)
gg
dev.off()
