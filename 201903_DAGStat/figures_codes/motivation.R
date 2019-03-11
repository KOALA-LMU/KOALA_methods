
library(coalitions)
library(tidyverse)
theme_set(theme_bw())
library(gganimate)


# 2013 last FORSA pre-election poll ---------------------------------------
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



# first 2013 poll of FORSA ------------------------------------------------
survey <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date == "2013-01-09") %>%
  unnest()

gg_survey(survey, annotate_bars = FALSE) +
  ylab("Voter share in %") +
  ggtitle("Forsa opinion poll, 09.01.2013") +
  theme_bw(base_size = 19) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130109.pdf", height = 5, width = 7)

# focus on Union and FDP
bw_cols <- coalitions::party_colors_de
bw_cols[!(names(bw_cols) %in% c("cdu","fdp"))] <- "gray80"
bw_cols <- bw_cols[names(bw_cols) != "afd"]
gg_survey(survey, annotate_bars = FALSE, colors = bw_cols) +
  ylab("Voter share in %") +
  ggtitle("Forsa opinion poll, 09.01.2013") +
  theme_bw(base_size = 19) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("../figures/motivation_forsa_130109_bw.pdf", height = 5, width = 7)



# Ridgeline plot ----------------------------------------------------------
surveys <- scrape_wahlrecht("http://www.wahlrecht.de/umfragen/forsa/2013.htm") %>%
  collapse_parties() %>% filter(date > "2013-01-01")

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

coalishin::plot_cp_ridgeline(dat, "forsa", "cdu|fdp") +
  # transition_time(date) +
  transition_manual(date, cumulative = TRUE) +
  # transition_states(date, transition_length = 5, state_length = 1) +
  # enter_fade() + exit_fade() +
  anim_save("ridgeline_forsa.gif")
