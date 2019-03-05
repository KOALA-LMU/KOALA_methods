
library(coalitions)
library(tidyverse)



# German federal elections 2013 -------------------------------------------
time_frame <- c("2013-09-01","2013-09-22")
nsim <- 10000
surveys <- get_surveys() %>%
  unnest() %>%
  filter(date >= time_frame[1], date <= time_frame[2]) %>%
  nest(-pollster, .key = "surveys")
last_date <- surveys %>% unnest() %>% pull(date) %>% max()
survey <- surveys %>% pool_surveys(last_date = last_date)
set.seed(2019)
draws <- survey %>% draw_from_posterior(nsim = nsim, correction = 0.005)
seats <- draws %>% get_seats(survey = survey)

# Union-FDP majority
round(quantile(draws$cdu + draws$fdp, probs = c(0.025,0.975)), 3)
prop.table(table(seats %>% have_majority(coalitions = list(c("cdu","fdp")))))

# SPD-Left-Greens majority
round(quantile(draws$spd + draws$left + draws$greens, probs = c(0.025,0.975)), 3)
prop.table(table(seats %>% have_majority(coalitions = list(c("spd","left","greens")))))

# Greens third strongest party
round(quantile(draws$greens, probs = c(0.025,0.975)), 3)
prop.table(table(draws$greens > draws$left))

# Left third strongest party
round(quantile(draws$left, probs = c(0.025,0.975)), 3)
prop.table(table(draws$left > draws$greens))

# FDP passing into parliament
round(quantile(draws$fdp, probs = c(0.025,0.975)), 3)
prop.table(table(draws$fdp >= 0.05))

# AfD passing into parliament
round(quantile(draws$afd, probs = c(0.025,0.975)), 3)
prop.table(table(draws$afd >= 0.05))


# German federal elections 2017 -------------------------------------------
time_frame <- c("2017-09-01","2017-09-24")
nsim <- 10000
surveys <- get_surveys() %>%
  unnest() %>%
  filter(date >= time_frame[1], date <= time_frame[2]) %>%
  nest(-pollster, .key = "surveys")
last_date <- surveys %>% unnest() %>% pull(date) %>% max()
survey <- surveys %>% pool_surveys(last_date = last_date)
set.seed(2019)
draws <- survey %>% draw_from_posterior(nsim = nsim, correction = 0.005)
seats <- draws %>% get_seats(survey = survey)

# Union-FDP majority
round(quantile(draws$cdu + draws$fdp, probs = c(0.025,0.975)), 3)
prop.table(table(seats %>% have_majority(coalitions = list(c("cdu","fdp")))))

# SPD-Left-Greens majority
round(quantile(draws$spd + draws$left + draws$greens, probs = c(0.025,0.975)), 3)
prop.table(table(seats %>% have_majority(coalitions = list(c("spd","left","greens")))))

# AfD third strongest party
round(quantile(draws$afd, probs = c(0.025,0.975)), 3)
prop.table(table(draws$afd > draws$fdp & draws$afd > draws$left))

# Left third strongest party
round(quantile(draws$left, probs = c(0.025,0.975)), 3)
prop.table(table(draws$left > draws$fdp & draws$left > draws$afd))

# FDP third strongest party
round(quantile(draws$fdp, probs = c(0.025,0.975)), 3)
prop.table(table(draws$fdp > draws$afd & draws$fdp > draws$left))
