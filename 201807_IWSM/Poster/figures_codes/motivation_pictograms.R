
library(ggplot2)



# Survey chart ------------------------------------------------------------
dat3 <- data.frame("party" = LETTERS[1:7],
                   "share" = c(33.3,18.3,13.6,8.9,10.3,11.7,4.1),
                   "col" = c("black","#E3000F","skyblue2","#ffed00","deeppink3","#46962b","grey"))
ggplot(dat3, aes(x = party, weight = share, fill = party)) +
  geom_bar() +
  scale_fill_manual(values = as.character(dat3$col)) +
  scale_y_continuous("Share in %") +
  theme_bw(base_size = 43) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin=unit(c(0,0,0,0),"mm")) +
  ggsave("../figures/motivation_survey.pdf", height = 5, width = 8)

# Bar chart ---------------------------------------------------------------
dat <- data.frame("party" = LETTERS[1:5],
                  "share" = c(30,20,12,14,11))
gg <- ggplot(dat, aes(x = party, weight = share)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

gg + geom_bar(fill = "gray40") +
  ggsave("../figures/motivation_pictoBar.pdf", height = 4, width = 5)
gg + geom_bar(fill = "firebrick1") +
  ggsave("../figures/motivation_pictoBar_col.pdf", height = 4, width = 5)


# Density chart -----------------------------------------------------------
set.seed(2018)
dat2 <- data.frame("x" = c(rnorm(20000,50,4), rnorm(13000,40,3.5)))
dat2$x[dat2$x < 31] <- 31
dat2$x[dat2$x > 61.5] <- 61.5
gg2 <- ggplot(dat2, aes(x = x)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

gg2 + geom_density(fill = "gray50", col = "gray40") +
  ggsave("../figures/motivation_pictoDens.pdf", height = 4, width = 5)
gg2 + geom_density(fill = "#33CC00", col = "#3CA518") +
  ggsave("../figures/motivation_pictoDens_col.pdf", height = 4, width = 5)
