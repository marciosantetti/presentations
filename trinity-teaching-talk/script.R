## @knitr load_packages

library(tidyverse)
library(hrbrthemes)
library(ggrepel)



##== Colors:

red_pink <- "#e64173"
met_slate <- "#23373b"
grey_mid <- "grey50"
red <- "#E02C05"
turquoise <- "#20B2AA"



## @knitr load_data

##== Load data:

data <- read_csv('data_set.csv')



##== Split data into three periods: 1948-1970, 1970-2000, and 2000-2020:


data_filter1 <- data %>% filter(DATE <= 1970)

data_filter2 <- data %>% filter(DATE >= 1970 & DATE <= 2000)

data_filter3 <- data %>% filter(DATE >= 2000)


##== Create a new column (CPI_CHANGE):


data_filter2 <- data_filter2 %>% mutate(CPI_CHANGE = c(NA, diff(CPI)))



## @knitr pc_1


##== The 'original Phillips Curve (1948-1970, with the inflation rate on the y-axis):


data_filter1 %>% ggplot(aes(y=CPI, x=UNRATE)) + geom_point(shape = 24, fill = red_pink,
                                                          color = red_pink,
                                                          size=2)  +
  geom_smooth(formula = 'y ~ x + I(x^2)', method = 'lm', se=F, color = turquoise) + 
  geom_hline(yintercept = 0) + theme_ipsum_rc() +
  geom_text_repel(aes(label = DATE), family = 'Roboto Condensed', size = 4.5) +
  labs(title = 'Phillips Curve, US, 1948—1970',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15)) 

  


## @knitr pc_2


##== The 'original' Phillips Curve (1970-2000, with the inflation rate on the y-axis):


years <- c("1970", "1980", "1990", "2000")


data_filter2 %>% ggplot(aes(y=CPI, x=UNRATE, label = ifelse(DATE %in% years, DATE, ""))) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink,
             size=2)  +
  geom_hline(yintercept = 0) + theme_ipsum_rc() +
  geom_text_repel(family = 'Roboto Condensed', size = 4.5) +
  labs(title = 'Phillips Curve, US, 1970—2000',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15)) 


## @knitr pc_3

##== The 'Accelerationist' Phillips Curve (1970-2020, 
##== with the change in the inflation rate on the y-axis):




data_filter2 %>% ggplot(aes(y=CPI_CHANGE, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink, color = red_pink, size=2) +
  geom_smooth(formula = 'y ~ x', method = 'lm', se = FALSE, color = turquoise) +
  theme_ipsum_rc() + geom_hline(yintercept = 0) +
  labs(title = 'Accelerationist Phillips Curve, US, 1970—2000',
       y = 'Change in inflation rate (%)',
       x = 'Unemployment rate (%)') +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15)) 


## @knitr pc_4


data_filter3 %>% ggplot(aes(y=CPI, x=UNRATE)) + 
  geom_point(shape = 24, fill = red_pink,
             color = red_pink,
             size=2)  +
  geom_hline(yintercept = 0) + theme_ipsum_rc() +
  geom_smooth(formula = 'y ~ x', method = 'lm', se=F, color = turquoise) +
  geom_text_repel(aes(label = DATE), family = 'Roboto Condensed', size = 4.5) +
  labs(title = 'Phillips Curve, US, 2000—2020',
       y = 'Inflation rate (%)',
       x = 'Unemployment rate (%)') +
  theme(axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15)) 


