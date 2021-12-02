
## @knitr load_packages

library(tidyverse)
library(ggthemr)
library(extrafont)
library(patchwork)
library(ggrepel)
library(tsibble)

ggthemr(palette = "greyscale")


red_pink <- "#e64173"
met_slate <- "#23373b"
grey_mid <- "grey50"
red <- "#E02C05"
turquoise <- "#20B2AA"


## @knitr ham

series <- read_csv("hamilton.csv")

series <- series %>%
  mutate(period = yearquarter(period)) 


## @knitr hamfig

h1 <- series %>% ggplot(aes(x=period, y=y)) + geom_line(color = grey_mid, size=1) +
  labs(title = "Original series",
       x = NULL,
       y = "Real GDP") +
  theme_ipsum_rc() +
  theme(axis.title.y = element_text(size = 13)) 

h2 <- series %>% ggplot(aes(x=period, y=y_trend)) + geom_line(color = turquoise, size=1) +
  labs(title = "Trend Component",
       x = NULL,
       y = "Real GDP - Trend") +
  theme_ipsum_rc() +
  theme(axis.title.y = element_text(size = 13)) 

h3 <- series %>% ggplot(aes(x=period, y=y_cycle)) + geom_line(color = red_pink, size=1) +
  labs(title = "Cyclical component",
       x = NULL,
       y = "Real GDP - Cycle") +
  theme_ipsum_rc() +
  theme(axis.title.y = element_text(size = 13)) 



(h1 | h2) / h3


#--------------------------------------------------------------------------------#

## @knitr set

set4 <- read_csv('irfs_svar_4d_org.csv')
red_point <- set4 %>% filter(period == 1)



######### ##=================== u, g:

## @knitr irf1


## IRFs:

a11 <- set4 %>% ggplot(aes(x=period, y=u_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se1), lty=2, alpha=0.8) +
  geom_line(aes(y = se2), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a21 <- set4 %>% ggplot(aes(x=period, y=g_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se9), lty=2, alpha=0.8) +
  geom_line(aes(y = se10), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% g[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a22 <- set4 %>% ggplot(aes(x=period, y=g_to_g)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se11), lty=2, alpha=0.8) +
  geom_line(aes(y = se12), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t] %->% g[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


a12 <- set4 %>% ggplot(aes(x=period, y=u_to_g)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se3), lty=2, alpha=0.8) +
  geom_line(aes(y = se4), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles1

## Cycles:


# 1. Output shock:

u_shock_g <- set4 %>% ggplot(aes(y = g_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(g_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = g_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(g[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


# 2. Investment shock:

g_shock_u <- set4 %>% ggplot(aes(y = g_to_g, x = u_to_g)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_g, n=-1), NA),
                                                          yend = c(tail(g_to_g, n=-1), NA))) +
  geom_point(data = red_point, aes(y = g_to_g, x = u_to_g), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Investment shock", x = expression(u[t]), y = expression(g[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr demo1

a11

## @knitr demo2

a21


## @knitr demo3

u_shock_g



## @knitr fig1


(a11 | a21 | a22 | a12) / (u_shock_g | g_shock_u)


## @knitr fig1_1

(u_shock_g | g_shock_u)






##=================== u, psi:

## @knitr irf2

## IRFs:


a14 <- set4 %>% ggplot(aes(x=period, y=u_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se7), lty=2, alpha=0.8) +
  geom_line(aes(y = se8), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


a41 <- set4 %>% ggplot(aes(x=period, y=psi_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se25), lty=2, alpha=0.8) +
  geom_line(aes(y = se26), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))



a44 <- set4 %>% ggplot(aes(x=period, y=psi_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se31), lty=2, alpha=0.8) +
  geom_line(aes(y = se32), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## knitr cycles2

## Cycles:

u_shock_psi <- set4 %>% ggplot(aes(y = psi_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(psi_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


psi_shock_u <- set4 %>% ggplot(aes(y = psi_to_psi, x = u_to_psi)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_psi, n=-1), NA),
                                                          yend = c(tail(psi_to_psi, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_psi, x = u_to_psi), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Labor share shock", x = expression(u[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig2

(a11 | a41 | a44 | a14) / (u_shock_psi | psi_shock_u)

## @knitr fig2_1

(u_shock_psi | psi_shock_u)




#---------------------------------------------------------------------------------------------------##


##=================== e, psi:

## @knitr irf3


## IRFs:

a34 <- set4 %>% ggplot(aes(x=period, y=e_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se23), lty=2, alpha=0.8) +
  geom_line(aes(y = se24), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a43 <- set4 %>% ggplot(aes(x=period, y=psi_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se29), lty=2, alpha=0.8) +
  geom_line(aes(y = se30), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a33 <- set4 %>% ggplot(aes(x=period, y=e_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se21), lty=2, alpha=0.8) +
  geom_line(aes(y = se22), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles3

## Cycles:


e_shock_psi <- set4 %>% ggplot(aes(y = psi_to_e, x = e_to_e)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(e_to_e, n=-1), NA),
                                                          yend = c(tail(psi_to_e, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_e, x = e_to_e), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Employment shock", x = expression(e[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

# 2. Labor share shock:

psi_shock_e <- set4 %>% ggplot(aes(y = psi_to_psi, x = e_to_psi)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(e_to_psi, n=-1), NA),
                                                          yend = c(tail(psi_to_psi, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_psi, x = e_to_psi), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Labor share shock", x = expression(e[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


## @knitr fig3

(a33 | a43 | a44 | a34) / (e_shock_psi | psi_shock_e)

## @knitr fig3_1

(e_shock_psi | psi_shock_e)

#---------------------------------------------------------------------------------------------------##


##=================== u, e:

## @knitr irf4

## IRFs:


a13 <- set4 %>% ggplot(aes(x=period, y=u_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se5), lty=2, alpha=0.8) +
  geom_line(aes(y = se6), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a31 <- set4 %>% ggplot(aes(x=period, y=e_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se17), lty=2, alpha=0.8) +
  geom_line(aes(y = se18), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles4

## Cycles:


# 1. Output shock:

u_shock_e <- set4 %>% ggplot(aes(y = e_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(e_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = e_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(e[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


# 2. Employment shock:

e_shock_u <- set4 %>% ggplot(aes(y = e_to_e, x = u_to_e)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_e, n=-1), NA),
                                                          yend = c(tail(e_to_e, n=-1), NA))) +
  geom_point(data = red_point, aes(y = e_to_e, x = u_to_e), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Employment shock", x = expression(u[t]), y = expression(e[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig4

(a11 | a31 | a33 | a13) / (u_shock_e | e_shock_u)

## @knitr fig4_1

(u_shock_e | e_shock_u)






###=======================================================##


## @knitr set_2

set5 <- read_csv('irfs_svar_5d_org.csv')
red_point <- set5 %>% filter(period == 1)


##=================== u, psi:


## @knitr irf_5


## IRFs:

a33_1 <- set5 %>% ggplot(aes(x=period, y=u_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se25), lty=2, alpha=0.8) +
  geom_line(aes(y = se26), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a53_1 <- set5 %>% ggplot(aes(x=period, y=psi_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se45), lty=2, alpha=0.8) +
  geom_line(aes(y = se46), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a55_1 <- set5 %>% ggplot(aes(x=period, y=psi_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se49), lty=2, alpha=0.8) +
  geom_line(aes(y = se50), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a35_1 <- set5 %>% ggplot(aes(x=period, y=u_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se29), lty=2, alpha=0.8) +
  geom_line(aes(y = se30), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


## @knitr cycles_5

## Cycles:

u_shock_psi_1 <- set5 %>% ggplot(aes(y = psi_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(psi_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


# 2. Labor share shock:

psi_shock_u_1 <- set5 %>% ggplot(aes(y = psi_to_psi, x = u_to_psi)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_psi, n=-1), NA),
                                                          yend = c(tail(psi_to_psi, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_psi, x = u_to_psi), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Labor share shock", x = expression(u[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig5

(a33_1 | a53_1 | a55_1 | a35_1) / (u_shock_psi_1 | psi_shock_u_1)

## @knitr fig5_1

(u_shock_psi_1 | psi_shock_u_1)


##=================== e, psi:

## @knitr irf_6


## IRFs:

a44_1 <- set5 %>% ggplot(aes(x=period, y=e_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se37), lty=2, alpha=0.8) +
  geom_line(aes(y = se38), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a54_1 <- set5 %>% ggplot(aes(x=period, y=psi_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se47), lty=2, alpha=0.8) +
  geom_line(aes(y = se48), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% psi[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a45_1 <- set5 %>% ggplot(aes(x=period, y=e_to_psi)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se39), lty=2, alpha=0.8) +
  geom_line(aes(y = se40), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(psi[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles_6

## Cycles:

# 1. Employment shock:

e_shock_psi_1 <- set5 %>% ggplot(aes(y = psi_to_e, x = e_to_e)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(e_to_e, n=-1), NA),
                                                          yend = c(tail(psi_to_e, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_e, x = e_to_e), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Employment shock", x = expression(e[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

# 2. Labor share shock:

psi_shock_e_1 <- set5 %>% ggplot(aes(y = psi_to_psi, x = e_to_psi)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(e_to_psi, n=-1), NA),
                                                          yend = c(tail(psi_to_psi, n=-1), NA))) +
  geom_point(data = red_point, aes(y = psi_to_psi, x = e_to_psi), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Labor share shock", x = expression(e[t]), y = expression(psi[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig6

(a44_1 | a54_1 | a55_1 | a45_1) / (e_shock_psi_1 | psi_shock_e_1)

## @knitr fig6_1

(e_shock_psi_1 | psi_shock_e_1)

##=================== u, e:

## @knitr irf_7


## IRFs:

a43_1 <- set5 %>% ggplot(aes(x=period, y=e_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se35), lty=2, alpha=0.8) +
  geom_line(aes(y = se36), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% e[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a34_1 <- set5 %>% ggplot(aes(x=period, y=u_to_e)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se27), lty=2, alpha=0.8) +
  geom_line(aes(y = se28), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(e[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


## Cycles:

## @knitr cycles_7


# 1. Output shock:

u_shock_e_1 <- set5 %>% ggplot(aes(y = e_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(e_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = e_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Output shock", x = expression(u[t]), y = expression(e[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


# 2. Employment shock:

e_shock_u_1 <- set5 %>% ggplot(aes(y = e_to_e, x = u_to_e)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_e, n=-1), NA),
                                                          yend = c(tail(e_to_e, n=-1), NA))) +
  geom_point(data = red_point, aes(y = e_to_e, x = u_to_e), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Employment shock", x = expression(u[t]), y = expression(e[t])) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig7

(a33_1 | a43_1 | a44_1 | a34_1) / (u_shock_e_1 | e_shock_u_1)

## @knitr fig7_1

(u_shock_e_1 | e_shock_u_1)



##=================== u, gr:

## @knitr irf_8


## IRFs:




a31_1 <- set5 %>% ggplot(aes(x=period, y=u_to_gr)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se21), lty=2, alpha=0.8) +
  geom_line(aes(y = se22), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t]^R %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


a13_1 <- set5 %>% ggplot(aes(x=period, y=gr_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se5), lty=2, alpha=0.8) +
  geom_line(aes(y = se6), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% g[t]^R)) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))


a11_1 <- set5 %>% ggplot(aes(x=period, y=gr_to_gr)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se1), lty=2, alpha=0.8) +
  geom_line(aes(y = se2), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t]^R %->% g[t]^R)) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles_8

## Cycles:

# 1. Output shock:

u_shock_gr_1 <- set5 %>% ggplot(aes(y = gr_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(gr_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = gr_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(g[t]^R)) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

# 2. Residential Investment shock:

gr_shock_u_1 <- set5 %>% ggplot(aes(y = gr_to_gr, x = u_to_gr)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_gr, n=-1), NA),
                                                          yend = c(tail(gr_to_gr, n=-1), NA))) +
  geom_point(data = red_point, aes(y = gr_to_gr, x = u_to_gr), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Residential investment shock", x = expression(u[t]), y = expression(g[t]^R)) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))


## @knitr fig8


(a33_1 | a13_1 | a11_1 | a31_1) / (u_shock_gr_1 | gr_shock_u_1)


## @knitr fig8_1

(u_shock_gr_1 | gr_shock_u_1)


## @knitr irf_9


## IRFs:

a33_1 <- set5 %>% ggplot(aes(x=period, y=u_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se25), lty=2, alpha=0.8) +
  geom_line(aes(y = se26), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a23_1 <- set5 %>% ggplot(aes(x=period, y=gn_to_u)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se15), lty=2, alpha=0.8) +
  geom_line(aes(y = se16), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(u[t] %->% g[t]^N)) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a22_1 <- set5 %>% ggplot(aes(x=period, y=gn_to_gn)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se13), lty=2, alpha=0.8) +
  geom_line(aes(y = se14), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t]^N %->% g[t]^N)) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

a32_1 <- set5 %>% ggplot(aes(x=period, y=u_to_gn)) + geom_line(size=0.6, alpha=0.9) + geom_point(size=2, alpha=0.5) +
  geom_line(aes(y = se23), lty=2, alpha=0.8) +
  geom_line(aes(y = se24), lty=2, alpha=0.8) +
  geom_hline(yintercept = 0) +
  labs(y="", x = "Quarters", title = expression(g[t]^N %->% u[t])) + scale_x_continuous(breaks = seq(0,32, by=8)) +
  theme(text = element_text(family="Fira Sans"))

## @knitr cycles_9

## Cycles:

u_shock_gn_1 <- set5 %>% ggplot(aes(y = gn_to_u, x = u_to_u)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_u, n=-1), NA),
                                                          yend = c(tail(gn_to_u, n=-1), NA))) +
  geom_point(data = red_point, aes(y = gn_to_u, x = u_to_u), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Demand shock", x = expression(u[t]), y = expression(g[t]^N)) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

# 2. Nonresidential Investment shock:

gn_shock_u_1 <- set5 %>% ggplot(aes(y = gn_to_gn, x = u_to_gn)) + geom_point() +
  geom_text_repel(aes(label = period), family = 'Roboto Condensed') + geom_segment(aes(xend = c(tail(u_to_gn, n=-1), NA),
                                                          yend = c(tail(gn_to_gn, n=-1), NA))) +
  geom_point(data = red_point, aes(y = gn_to_gn, x = u_to_gn), color="red", size = 3) + 
  # geom_point(data = blue_point, aes(y = h_to_h, x = u_to_h), color="blue", size=3) +
  labs(title = "Nonres. investment shock", x = expression(u[t]), y = expression(g[t]^N)) +
  geom_vline(xintercept = 0, lty = 2) + geom_hline(yintercept = 0, lty = 2) +
  theme(text = element_text(family="Fira Sans"))

## @knitr fig9

(a33_1 | a23_1 | a22_1 | a32_1) / (u_shock_gn_1 | gn_shock_u_1)

## @knitr fig9_1

(u_shock_gn_1 | gn_shock_u_1)

