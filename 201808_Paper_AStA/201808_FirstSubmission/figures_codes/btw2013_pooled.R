
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)
source("helpers.R")
theme_set(theme_bw())

load("data_prepared/btw2013_pooled_data.RData")

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
partycols <- partycols[names(partycols) != "afd"]
partycols["fdp"] <- lk %>% filter(id_party == "fdp") %>% pull(colDark)

gg <- coalishin::plot_pooledSurvey_byTime(plot_dat_raw, election = "btw", partycols = partycols, plot_variation = FALSE,
                                          hline = NULL)
pdf("../figures/2013_pooled_rawShares.pdf", width = 8, height = 3.5)
gg + 
  scale_y_continuous(name = "Reported party share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","5%","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_date(breaks = as.Date(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
               minor_breaks = NULL,
               labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  scale_color_manual(name = "Parties", values = partycols,
                     labels = c("Union","SPD","FDP","Left","Greens","Others")) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_hline(yintercept = 5, lty = 2, lwd = 1, col = "gray") +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank())
dev.off()



# CDU/FDP majority --------------------------------------------------------
### 1) Redistributed party shares
gg <- ggplot(plot_dat, aes(x = date, y = cdufdp_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  scale_y_continuous(name = "Joint voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 15), units = "pt"))
pdf("../figures/2013_pooled_cdufdp_rawSharesRedist.pdf", width = 9, height = 4)
gg
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$cdufdp_majority_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks,
                     labels = axis_labels, name = "POE",
                     minor_breaks = NULL) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 4), units = "pt"))
pdf("../figures/2013_pooled_cdufdp_prob.pdf", width = 9, height = 4)
gg
dev.off()

### 3) ridgeline plot
gg_shares <- ggplot(shares,
                    aes(x = coal_percent_cdufdp, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(38,62)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 29) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2013_pooled_cdufdp_ridgeline.pdf", width = 10, height = 10)
gg_shares
dev.off()





# FDP passing hurdle-------------------------------------------------------
### 1) Reported party shares
gg <- ggplot(plot_dat, aes(x = date, y = fdp_share_raw)) +
  geom_hline(yintercept = 5, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3) +
  scale_y_continuous(limits = c(0,10), name = "Reported party share",
                     breaks = c(0,2,4,5,6,8,10),
                     minor_breaks = c(1,3,7,9),
                     labels = c("0%","2%","4%","5%","6%","8%","10%")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 11.9), units = "pt"))
pdf("../figures/2013_pooled_fdp_rawShares.pdf", width = 9, height = 4)
gg
dev.off()

### 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$fdp_passing_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3) +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks,
                     labels = axis_labels, name = "POE",
                     minor_breaks = NULL) +
  scale_x_datetime(breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                   minor_breaks = NULL,
                   labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 3), units = "pt"))
pdf("../figures/2013_pooled_fdp_PassingProb.pdf", width = 9, height = 4)
gg
dev.off()

### 3) ridgeline plot
gg_shares <- ggplot(shares,
                    aes(x = fdp_rawPercent, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>5, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 5, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,10)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(c(rep(2012,3),rep(2013,10)), "-", c("10","11","12","01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",12),"22"))),
                     minor_breaks = NULL,
                     labels = c("Oct 2012","","","Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 29) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2013_pooled_fdp_ridgeline.pdf", width = 10, height = 10)
gg_shares
dev.off()



# SPD/Greens majority --------------------------------------------------
# ### 1) Redistributed raw shares
# gg <- ggplot(plot_dat, aes(x = date, y = spdgreens_share_redist)) +
#   geom_hline(yintercept = 50, lty = 2, lwd = 1.2, col = "gray") +
#   geom_line(lwd = 1.3, col = "gray30") +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(40,60), name = "redistributed voter share") +
#   scale_x_datetime(breaks = as.POSIXct(c("2012-10-01","2013-01-01","2013-04-01","2013-07-01","2013-09-24")),
#                    labels = c("Oct 2012","Jan 2013","Apr 2013","Jul 2013","Election day")) +
#   theme_bw(base_size = 20) +
#   theme(axis.title.x = element_blank())
# pdf("../figures/2013_pooled_spdgreens_rawSharesRedist.pdf", width = 7.5, height = 3.5)
# gg
# dev.off()
# 
# ### 2) Majority probabilities
# plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$spdgreens_majority_prob / 100)
# gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
#   geom_line(lwd = 1.3, col = "gray30") +
#   scale_y_continuous(limits = c(0,100), breaks = skewed_ticks,
#                      labels = axis_labels, name = "seat majority probability",
#                      minor_breaks = NULL) +
#   scale_x_datetime(breaks = as.POSIXct(c("2012-10-01","2013-01-01","2013-04-01","2013-07-01","2013-09-24")),
#                    labels = c("Oct 2012","Jan 2013","Apr 2013","Jul 2013","Election day")) +
#   theme_bw(base_size = 20) +
#   theme(axis.title.x = element_blank())
# pdf("../figures/2013_pooled_spdgreens_prob.pdf", width = 7.5, height = 3.5)
# gg
# dev.off()
# 
# ### 3) ridgeline plot
# gg_shares <- ggplot(shares,
#                     aes(x = coal_percent_spdgreens, y = date, group = date, # basic aesthetics
#                         fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
#                         frame = date, cumulative = TRUE)) + # aesthetics for animation
#   geom_density_ridges_gradient(
#     scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
#   scale_fill_manual(values = c("grey80","steelblue"), na.value = "grey80", guide = guide_legend(title = "Seat majority")) +
#   geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
#   scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(35,60)) +
#   scale_y_continuous(trans  = rev_date, breaks = as.POSIXct(c("2012-10-01","2013-01-01","2013-04-01","2013-07-01","2013-09-24")),
#                      labels = c("Oct 2012","Jan 2013","Apr 2013","Jul 2013","Election day")) +
#   xlab("Share of parliament seats") + ylab("") +
#   theme_bw(base_size = 25) +
#   theme(legend.position = "bottom")
# 
# pdf("../figures/2013_pooled_spdgreens_ridgeline.pdf", width = 7.5, height = 10)
# gg_shares
# dev.off()