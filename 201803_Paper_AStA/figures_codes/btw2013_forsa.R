
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


# 0) Development of (pooled) raw poll shares ------------------------------
# prepare data for using the plotting function
plot_dat_raw <- surveys %>% unnest()# %>% bind_rows(pool)
# merge the Pirates party into the Others category
for (dat in unique(plot_dat_raw$date)) {
  row_pir <- which(plot_dat_raw$date == dat & plot_dat_raw$party == "pirates")
  row_others <- which(plot_dat_raw$date == dat & plot_dat_raw$party == "others")
  plot_dat_raw[row_others, c("percent","votes")] <- plot_dat_raw[row_others, c("percent","votes")] + plot_dat_raw[row_pir, c("percent","votes")]
  plot_dat_raw <- plot_dat_raw[-row_pir,]
}

# plot
lk <- coalishin::lookup_parties %>% filter(id_election == "btw")
partycols <- lk$col
names(partycols) <- lk$id_party
partycols["fdp"] <- lk %>% filter(id_party == "fdp") %>% pull(colDark)

gg <- coalishin::plot_rawSurvey_byTime(plot_dat_raw, institute = "forsa", election = "btw", hline = NULL)
pdf("../figures/2013_forsa_rawShares.pdf", width = 8, height = 3.5)
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
                     labels = c("Union","SPD","AfD","FDP","Left","Greens","Others")) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_hline(yintercept = 5, lty = 2, lwd = 1, col = "gray") +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank())
dev.off()


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
  theme_bw(base_size = 25) +
  theme(legend.position = "bottom",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

pdf("../figures/2013_forsa_cdufdp_lastPreelectionPoll.pdf", width = 8, height = 4)
gg_shares
# brand_plot(gg_shares, xmin = 53, xmax = 57.75, ymin = shares_short$date[1] + 0.65)
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
  theme_bw(base_size = 29) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2013_forsa_cdufdp_ridgeline.pdf", width = 10, height = 10)
gg_shares
dev.off()


### plot only FDP
gg_shares <- ggplot(shares,
                    aes(x = fdp_rawPercent, y = date, group = date, # basic aesthetics
                        fill = ifelse(..x..>5, "yes", "no"), # "cut-off" gradient
                        frame = date, cumulative = TRUE)) + # aesthetics for animation
  geom_density_ridges_gradient(
    scale=10, size = 0.25, rel_min_height = 0.03, calc_ecdf=TRUE) +
  scale_fill_manual(values = c("grey90","#7294C9"), na.value = "grey90", guide = guide_legend(title = "Seat majority")) +
  geom_vline(xintercept = 50, lty = 1, lwd = 1.2, col = "grey30") +
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,10)) +
  scale_y_continuous(trans  = rev_date,
                     breaks = as.POSIXct(paste0(rep(2013,10), "-", c("01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",9),"22"))),
                     minor_breaks = NULL,
                     labels = c("Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  xlab("Share of parliament seats") + ylab("") +
  theme_bw(base_size = 29) +
  theme(legend.position = "bottom",
        plot.margin = unit(c(0,5.5,5.5,5.5), units = "pt"))

pdf("../figures/2013_forsa_fdp_ridgeline.pdf", width = 10, height = 10)
gg_shares
dev.off()




# 3) Line plot of CDU-FDP probabilities -----------------------------------
### 1) Redistributed party shares
gg <- ggplot(plot_dat, aes(x = date, y = cdufdp_share_redist)) +
  geom_hline(yintercept = 50, lty = 2, lwd = 1, col = "gray") +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(name = "Joint voter share",
                     limits = c(0,55),
                     breaks = seq(0,55,by = 5),
                     minor_breaks = NULL,
                     labels = c("0%","","10%","","20%","","30%","","40%","","50%","")) +
  scale_x_datetime(breaks = as.POSIXct(paste0(rep(2013,10), "-", c("01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",9),"22"))),
                   minor_breaks = NULL,
                   labels = c("Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 11.9), units = "pt"))
pdf("../figures/2013_forsa_cdufdp_rawSharesRedist.pdf", width = 9, height = 4)
gg
dev.off()

# 2) Majority probabilities
plot_dat$probs_skewed <- 100 * coalishin:::transform_cps(plot_dat$cdufdp_majority_prob / 100)
gg <- ggplot(plot_dat, aes(x = date, y = probs_skewed)) +
  geom_line(lwd = 1.3, col = "gray30") +
  scale_y_continuous(limits = c(0,100), breaks = skewed_ticks,
                     labels = axis_labels, name = "POE",
                     minor_breaks = NULL) +
  scale_x_datetime(breaks = as.POSIXct(paste0(rep(2013,10), "-", c("01","02","03","04","05","06","07","08","09","09"), "-", c(rep("01",9),"22"))),
                   minor_breaks = NULL,
                   labels = c("Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  theme_bw(base_size = 24) +
  theme(axis.title.x = element_blank(),
        plot.margin = unit(c(0, 37, 5.5, 3), units = "pt"))
pdf("../figures/2013_forsa_cdufdp_prob.pdf", width = 9, height = 4)
gg
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
        plot.margin = unit(c(0, 37, 5.5, 20.5), units = "pt"))
pdf("../figures/2013_forsa_fdp_rawShares.pdf", width = 9, height = 4)
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
        plot.margin = unit(c(0, 37, 5.5, 0), units = "pt"))
pdf("../figures/2013_forsa_fdp_passingProb.pdf", width = 9, height = 4)
gg
dev.off()
