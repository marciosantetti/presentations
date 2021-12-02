library(tidyverse)
library(hrbrthemes)
library(extrafont)
library(patchwork)
library(ggrepel)
library(tsibble)

theme_set(theme_ipsum_rc())


red_pink <- "#e64173"
met_slate <- "#23373b"
grey_mid <- "grey50"
red <- "#E02C05"
turquoise <- "#20B2AA"


series <- read_csv("hamilton.csv")

series <- series %>%
  mutate(period = yearquarter(period)) 


h1 <- series %>% ggplot(aes(x=period, y=y)) + geom_line(color = grey_mid, size=1) +
  labs(title = "Original series",
       x = NULL,
       y = "Real GDP") +
  theme(axis.title.y = element_text(size = 13)) 

h2 <- series %>% ggplot(aes(x=period, y=y_trend)) + geom_line(color = turquoise, size=1) +
  labs(title = "Trend Component",
       x = NULL,
       y = "Real GDP - Trend") +
  theme(axis.title.y = element_text(size = 13)) 

h3 <- series %>% ggplot(aes(x=period, y=y_cycle)) + geom_line(color = red_pink, size=1) +
  labs(title = "Cyclical component",
       x = NULL,
       y = "Real GDP - Cycle") +
  theme(axis.title.y = element_text(size = 13)) 



(h1 | h2) / h3
