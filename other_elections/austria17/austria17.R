library(magrittr)
library(tidyr)
library(purrr)
library(dplyr)
library(coalitions)
library(ggplot2)
theme_set(theme_bw())
library(png)
library(grid)
library(gridExtra)
library(coalishin)


# Calculate newest probabilities ------------------------------------------
surveys_at <- get_surveys(country="AT")
surveys_at

pooled_at <- pool_austria(surveys_at)
pooled_at

coalitions=list(
  c("ÖVP", "GRÜNE", "NEOS", "PILZ"),
  c("ÖVP", "FPÖ"),
  c("ÖVP", "SPÖ"),
  c("SPÖ", "FPÖ"),
  c("SPÖ", "GRÜNE", "NEOS"),
  c("SPÖ", "GRÜNE", "NEOS", "PILZ"))
res_df <- pooled_at %>%
  collapse_parties() %>%
  mutate(draws = map(survey, draw_from_posterior, nsim=100, correction=0.005)) %>%
  ## note distrib.fun, n_seats and hurdle arguments, specified for Austria
  mutate(seats = map2(draws, survey, get_seats, distrib.fun = dHondt, n_seats = 183,
                      hurdle = 0.04))  %>%
  mutate(majorities = map(seats, have_majority, coalitions=coalitions,
                          seats_majority = 92)) %>%
  mutate(probabilities = map(majorities, calculate_probs,coalitions=coalitions))

res_df

res_einzug <- res_df %>% pull(draws) %>% flatten_dfc() %>% apply(2, function(party) mean(party > 0.04))
res_cp <- res_df %>% pull(probabilities)



# Daten aufbereiten -------------------------------------------------------
election <- "AT"
source("R/zzz.R")
data_path <- get("data_path", envir=.coalEnv)
party_order <- get("party_order_AT", envir = .coalEnv)

pooled_nested <- nest(pooled_at, -pollster, -respondents, -date, -start, -end, .key = "survey")
pooled_nested <- nest(pooled_nested, -pollster, .key = "surveys")
surveys_byTime <- bind_rows(surveys_at, pooled_nested) %>% unnest()
# set order of party levels
surveys_byTime <- surveys_byTime %>%
  unnest() %>%
  mutate(party = factor(party, levels = party_order)) %>%
  nest(-pollster, -date, -start, -end, -respondents) %>%
  rename(survey = data)
# most current surveys
surveys <- surveys_byTime %>%
  group_by(pollster) %>%
  slice(which.max(date)) %>%
  ungroup()

# reformat the tibbles into lists with one element per survey
pollsters <- surveys$pollster
surveys <- lapply(pollsters, function(ins) {
  surveys %>%
    filter(pollster == ins) %>%
    select(-pollster)
})
names(surveys) <- pollsters
surveys_byTime <- lapply(pollsters, function(ins) {
  surveys_byTime %>%
    filter(pollster == ins) %>%
    select(-pollster)
})
names(surveys_byTime) <- pollsters





# Twittern ----------------------------------------------------------------
tweet_type <- 3

### Step 1: Prepare message and image
# Get the newest survey(s)
newest_date <- max(pooled_at$date)
newest_date <- format(newest_date, "%d.%m.%Y")
# get the election label
elections <- get("elections", envir = .coalEnv)
selection <- which(sapply(elections, function(x) x$id == election))
election_label <- paste0("zur ", elections[[selection]]$label)

election_hashtag <- get("elections_hashtags", envir = .coalEnv)[[election]]
at_mentions <- get("elections_at_mentions", envir = .coalEnv)[[election]]
at_mentions <- unname(at_mentions)
at_mentions <- paste(sample(at_mentions, length(at_mentions)), collapse=" ")
message <- paste0("Hier die aktuellen KoalitionsW'keiten zur ",election_hashtag, " ", at_mentions)
message2 <- message
message <- paste0("Aktueller Umfragenstand zur ",election_hashtag," @neuwalcom ", at_mentions)
message3 <- paste0("Schaffen es die kleinen Parteien bei der ",election_hashtag," über die 4%-Hürde? ", at_mentions)

### Prepare the image for the tweet
# Create grobs for header of plot
img <- readPNG(paste0(data_path, "../images/Koala_Logo_Schrift.png"), info = FALSE)
img_election <- readPNG(paste0(data_path, "../images/election_flags/", election, ".png"), info = FALSE)
grob_koala <- grobTree(rasterGrob(img, hjust = 0.5))
grob_election <- grobTree(rasterGrob(img_election, hjust = 0.5))
election_label_grob <- grobTree(textGrob(election_label))
freespace_g <- grobTree(textGrob(" "))

# Check if only one survey was published in the last 14 days
latest_dates <- max(pooled_at$date)
max_date <- max(latest_dates)
onlyOneCurrentSurvey <- FALSE
# Prepare plots
date <- max_date
date <- format(strptime(date, "%Y-%m-%d"), "%d.%m.%Y")
start <- format(as.Date(date, format = "%d.%m.%Y") - 14, "%d.%m.%Y")
datum <- paste0(substring(start, 1, 5), " - ", date)

partycols <- get("partycols_AT", envir=.coalEnv)
ins <- "pooled"
gg_ps <- plot_rawSurvey(surveys, ins, partycols, main = "Gepoolte Umfrage", election = election, hurdle = 4)
gg_pooledSurvey <- gg_ps +
  ggtitle(paste0("Aktuelle Ergebnisse\n(gepoolt auf Basis der aktuellsten Umfragen\nim Zeitraum ",datum,")\n")) +
  theme(plot.title = element_text(hjust = 0, size = 14)) +
  labs(caption = "Datenquelle: www.neuwal.com") +
  theme(plot.caption = element_text(hjust = 0, size = 14, color = "gray40"))
### home coals
coals_sort <- c("ÖVP|GRÜNE|NEOS|PILZ", "ÖVP|FPÖ", "ÖVP|SPÖ", "SPÖ|FPÖ", "SPÖ|GRÜNE|NEOS","SPÖ|GRÜNE|NEOS|PILZ")
cols <- sapply(coals_sort, function(x) ifelse(substr(x,1,3) == "ÖVP", partycols["ÖVP"], partycols["SPÖ"]))
dat <- res_cp[[1]]
names(dat)[2] <- "prob"
dat$coalition <- coals_sort
dat$labels <- get("coalLabels_AT", envir = .coalEnv)[dat$coalition]
gg_cp <- plot_percBar(round(dat$prob, 1), cols = cols, labels = dat$labels)
gg_coalProbs <- gg_cp +
  ggtitle(paste0("Wahrscheinlichkeit, dass Mehrheit für Koalition X\nzustande kommt (wenn heute Wahl wäre)\n",ifelse(tweet_type==1,"\n",""))) +
  theme(plot.title = element_text(hjust = 0, size = 14))
### inOut
dat <- as.data.frame(t(as.matrix(res_einzug, nrows = 1)))
probs <- c("Grüne" = dat$GRÜNE, "NEOS" = dat$NEOS, "PILZ" = dat$PILZ)
partyLabel_order <- names(partyLabels)[order(match(partyLabels, party_order))]
probs <- probs[order(match(names(probs), partyLabel_order))]
probs <- round(probs * 100, 1)
color_names <- gsub("Grüne","GRÜNE",partyLabels[names(probs)])
gg_inOut <- plot_percBar(round(probs, 1), cols = partycols[color_names], labels = names(probs))
gg_inOut <- gg_inOut +
  ggtitle("Schaffen es die kleinen Parteien über die 4%-Hürde?") +
  theme(plot.title = element_text(hjust = 0, size = 14),
        plot.margin = unit(c(20,40,20,40),"pt"))
# caption
gg_coalProbs <- gg_coalProbs + labs(caption = paste0("\n\nStand: ",newest_date," | Datenquelle: www.neuwal.com")) +
  theme(plot.caption = element_text(hjust = 0, size = 14, color = "gray40"))
png("temp_tweet.png", width = ifelse(tweet_type == 1,1850,1400), 1600, type = "cairo", res = 175) # use type 'cairo' as this is the best one on Linux
header <- grid.arrange(freespace_g, grob_koala, grob_election, freespace_g, ncol = 4, widths = c(3,4,1,3))
grid.arrange(freespace_g, header, freespace_g, election_label_grob, freespace_g,
             gg_pooledSurvey, heights = c(2,8,1,2,2,50))
dev.off()


header <- grid.arrange(freespace_g, grob_koala, grob_election, freespace_g, ncol = 4, widths = c(3,4,1,3))
main_plots <- grid.arrange(freespace_g, gg_coalProbs, freespace_g, widths = c(1,8,1))
png("temp_tweet2.png", width = 1500, height = 1600, type = "cairo", res = 175) # use type 'cairo' as this is the best one on Linux
grid.arrange(freespace_g, header, freespace_g, election_label_grob, freespace_g,
             main_plots, heights = c(2,8,1,2,4,50))
dev.off()

header <- grid.arrange(freespace_g, grob_koala, grob_election, freespace_g, ncol = 4, widths = c(3,4,1,3))
png("temp_tweet3.png", width = 1200, height = 1200, type = "cairo", res = 175)
grid.arrange(freespace_g, header, freespace_g, election_label_grob, freespace_g,
             gg_inOut, heights = c(2,8,1,2,8,40))
dev.off()

### Step 2: Tweet
# source: http://blog.eoda.de/2017/02/02/tutorial-so-richten-sie-mit-r-einen-twitter-bot-ein/
# set up twitter api
api_keys <- suppressWarnings(read.csv(paste0(data_path,"../web_services/twitter_accessTokens.csv"), sep = ";", stringsAsFactors = FALSE))
library(twitteR)
setup_twitter_oauth(
  consumer_key    = api_keys$consumer_key,
  consumer_secret = api_keys$consumer_secret,
  access_token    = api_keys$access_token,
  access_secret   = api_keys$access_secret)
# send tweet(s)
tweet(message, mediaPath = "temp_tweet.png")
tweet(message2, mediaPath = "temp_tweet2.png")
tweet(message3, mediaPath = "temp_tweet3.png")

# Delete image file again
file.remove(file = "temp_tweet.png")
file.remove(file = "temp_tweet2.png")
file.remove(file = "temp_tweet3.png")
