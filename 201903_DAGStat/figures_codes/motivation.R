
library(coalitions)
library(tidyverse)
theme_set(theme_bw())
library(gganimate)


# 2013 FORSA pre-election polls -------------------------------------------

### first 2013 poll
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-01-09") %>%
  unnest()
# focus on Union and FDP
bw_cols <- coalitions::party_colors_de
bw_cols[!(names(bw_cols) %in% c("cdu","fdp"))] <- "gray80"
bw_cols <- bw_cols[names(bw_cols) != "afd"]
gg_survey(survey, annotate_bars = FALSE, colors = bw_cols) +
  ylab("Voter share in %") +
  ggtitle("Forsa, 09.01.2013") +
  theme_bw(base_size = 28) +
  ylim(c(0,43)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130109_bw.pdf", height = 5, width = 6)

### second 2013 poll
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-01-16") %>%
  unnest()
# focus on Union and FDP
bw_cols <- coalitions::party_colors_de
bw_cols[!(names(bw_cols) %in% c("cdu","fdp"))] <- "gray80"
bw_cols <- bw_cols[names(bw_cols) != "afd"]
gg_survey(survey, annotate_bars = FALSE, colors = bw_cols) +
  ylab("Voter share in %") +
  ggtitle("Forsa, 16.01.2013") +
  theme_bw(base_size = 28) +
  ylim(c(0,43)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130116_bw.pdf", height = 5, width = 6)

### first poll in 2013 where there's a bimodal posterior distribution
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-03-26") %>%
  unnest()
# focus on Union and FDP
bw_cols <- coalitions::party_colors_de
bw_cols[!(names(bw_cols) %in% c("cdu","fdp"))] <- "gray80"
bw_cols <- bw_cols[names(bw_cols) != "afd"]
gg_survey(survey, annotate_bars = FALSE, colors = bw_cols) +
  ylab("Voter share in %") +
  ggtitle("Forsa, 26.03.2013") +
  theme_bw(base_size = 28) +
  ylim(c(0,43)) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130326_bw.pdf", height = 5, width = 6)

### last pre-election poll
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-09-20") %>%
  unnest()
gg_survey(survey, annotate_bars = FALSE) +
  ylab("Voter share in %") +
  ggtitle("Forsa opinion poll, 20.09.2013") +
  theme_bw(base_size = 19) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130920.pdf", height = 5, width = 7)
# focus on Union and FDP
bw_cols <- coalitions::party_colors_de
bw_cols[!(names(bw_cols) %in% c("cdu","fdp"))] <- "gray80"
gg_survey(survey, annotate_bars = FALSE, colors = bw_cols) +
  ylab("Voter share in %") +
  ggtitle("Forsa opinion poll, 20.09.2013") +
  theme_bw(base_size = 19) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130920_bw.pdf", height = 5, width = 7)




# Ridgeline plot ----------------------------------------------------------
surveys <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date >= "2013-01-01")

set.seed(2018)
dat_list <- lapply(seq_len(nrow(surveys)), function(i) {
  print(paste0("Do calculations for survey ",i," of ",nrow(surveys)))
  survey <- surveys %>% slice(i) %>% unnest()
  coal_shares <- survey %>%
    draw_from_posterior(nsim = 1000, correction = 0.005) %>%
    get_seats(survey = survey, hurdle = 0.05) %>%
    filter(party %in% c("cdu","fdp")) %>%
    group_by(sim) %>%
    summarize(coal_share = sum(seats) / 598) %>%
    pull(coal_share)
  dat_seatShares <- data.frame("pollster"  = "forsa",
                               "date"      = survey$date[1],
                               "coalition" = "cdu|fdp",
                               stringsAsFactors = FALSE)
  dat_seatShares <- cbind(dat_seatShares, matrix(coal_shares, nrow = 1))
  dat_seatShares
})
dat <- dplyr::bind_rows(dat_list)

# posterior distribution for latest pre-election poll
# This plot was copied from the AStA paper folder

# animated ridgeline plot
rev_date <- coalishin:::c_trans("reverse", "time")
gg <- coalishin::plot_cp_ridgeline(dat, "forsa", "cdu|fdp") +
  xlab("Share of parliament seats") +
  scale_y_continuous(trans = rev_date,
                     minor_breaks = NULL,
                     breaks = as.POSIXct(paste0("2013-0",c(1:9,9),c(rep("-01",9),"-22"))),
                     labels = c("Jan 2013","","","Apr 2013","","","Jul 2013","","","Election day")) +
  scale_fill_manual(values = c("#7294C9","grey90"),
                    labels = c("yes","no"),
                    guide = guide_legend(title = "Seat majority", reverse = TRUE)) +
  theme(legend.position = "right") +
  theme_bw(base_size = 24) +
  # transition_time(date) +
  transition_manual(date, cumulative = TRUE)
  # transition_states(date, transition_length = 5, state_length = 1)
  # enter_fade() + exit_fade()
# anim_save("../figures/ridgeline_forsa.mp4", animation = gg, width = 800, height = 600,
#           renderer = ffmpeg_renderer(options = list(vf = "setpts=7*PTS",
#                                                     crf = "10")))
anim_save("../figures/ridgeline_forsa.gif", animation = gg, width = 800, height = 500)
