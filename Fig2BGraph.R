library(ggplot2)
library(tidyr)

test_data <-
  data.frame(
    xd_direct = 100 + c(0, cumsum(runif(17, -20, 20))),
    xd_mediate = 150 + c(0, cumsum(runif(17, -10, 10))),
    year = seq(from=1980, to=2015)
  )

test_data %>%
  gather(key, fraction, xd_direct, xd_mediate) %>%
  ggplot(aes(x=year, y=fraction, colour=key)) +
  geom_rect(aes(xmin = 1990, ymin = 0, xmax= 2003, ymax = 180, color="#f7e2ae", fill="#f7e2ae")) +
  geom_text(aes(label = "HGP (1990-2003)", x=1996.5, y=175)) +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits = c(1980,2015), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,180), expand = c(0, 0)) +
  xlab("") +
  ylab("Fraction of collaborations that are cross-disciplinary") +
  theme(axis.text.x = element_text(color="#000000", size=14, angle=0),
        axis.text.y = element_text(color="#000000", size=14, angle=0),
        legend.justification=c(1,0), 
        legend.position=c(1,0)) +
  scale_x_continuous(breaks=seq(1980,2015,2), labels=c())
  
