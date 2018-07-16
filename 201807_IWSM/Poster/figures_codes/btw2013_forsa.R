
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ggridges)
source("helpers.R")
theme_set(theme_bw())

load("data_prepared/btw2013_forsa_data.RData")

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


# 1) Density plot for the last poll before election day -------------------
shares_short <- shares %>% filter(date == max(date))

gg_shares <- ggplot(shares_short,
                    aes(x = coal_percent, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey90") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(40,60)) +
  xlab("Share of parliament seats") + ylab("Density") +
  theme_bw(base_size = 35) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

pdf("../figures/vis_seatDist.pdf", width = 8, height = 5.5)
gg_shares
dev.off()


# 2) Ridgeline plots ------------------------------------------------------
### plot the CDU/FDP coalition
gg_shares <- ggplot(shares,
       aes(x = coal_percent, y = date, group = date, # basic aesthetics
           fill = ifelse(..x..>50, "yes", "no"), # "cut-off" gradient
           frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(40,60)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(rep(2013,10), "-", c("01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",9),"22"))),
                     minor_breaks = NULL,
                     labels = c("Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 45) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,20,5.5,-30), units = "pt"))

pdf("../figures/vis_seatDist_time.pdf", width = 10, height = 10)
gg_shares
dev.off()
