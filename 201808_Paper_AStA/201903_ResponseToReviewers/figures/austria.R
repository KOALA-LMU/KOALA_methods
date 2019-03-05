
library(coalishin)
library(tidyverse)

# plot of the latest pooled results before the 2017 Austria election

perc <- c("spo" = 25.3, "ovp" = 33.3, "fpo" = 25.5, "grune" = 4.4,
          "neos" = 4.9, "pilz" = 4.6, "others" = 2.0)
plot_dat <- data.frame("party" = names(perc),
                       "percent" = unname(perc))
plot_rawSurvey(plot_dat, election = "at", hurdle = 4) +
  ylab("Party share in %") +
  ggtitle("Pooled party shares\n(based on time period 26.09. - 10.10.2017)") +
  scale_x_discrete(labels = c("SPÖ","ÖVP","FPÖ","Greens","NEOS","PILZ","Others")) +
  ggsave("austria.pdf", width = 6, height = 5)
