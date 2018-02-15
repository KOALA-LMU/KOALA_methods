
library(coalitions)
library(coalishin)
library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

# Data and code source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55

# Read data ---------------------------------------------------------------
shares <- readRDS(url("https://ndownloader.figshare.com/files/9375607"))


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
  scale_y_continuous(trans  = rev_date) +
  xlab("Share of votes") + ylab("") +
  theme_bw(base_size = 18) +
  theme(legend.position = "bottom")

pdf("../figures/bauer_seatDist_time.pdf", width = 6, height = 8)
gg_shares
dev.off()