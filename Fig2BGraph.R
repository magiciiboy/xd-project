# Author: Tung Huynh
# Ref: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

library(ggplot2)
library(tidyr)

# fraction_data <-
#  data.frame(
#    xd_direct = seq(from=0.1, to=0.15),
#    xd_mediate = seq(from=0.075, to=0.125),
#    year = seq(from=1980, to=2015)
#  )
fraction_data <- read.csv("./preprocessed/collaborations_xd_fraction.csv")

max_xd = max(c(max(fraction_data$xd_direct), max(fraction_data$xd_mediate)))
axis_x_seq = seq(1980,2015,2)

fraction_data %>%
  gather(key, fraction, xd_direct, xd_mediate) %>%
  ggplot(aes(x=year, y=fraction, colour=key)) +
  annotate("rect", xmin = 1990, ymin = 0, xmax= 2003, ymax = 0.3, fill="#f7e2ae") +
  annotate("text", label="HGP (1990-2003)", x=1996.5, y=0.29) +
  geom_line(size=1) +
  scale_color_manual(values=c('#0000FF','#FF0000'), 
                     labels=c("Direct XD links - Career data", "Mediated XD links - Career data"), 
                     name="",
                     aesthetics = "colour") +
  theme_classic() +
  scale_x_continuous(limits = c(1980,2015), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,0.3), expand = c(0, 0)) +
  xlab("") +
  ylab(expression(paste("Fraction of collaborations \nthat are cross-disciplinary"))) +
  theme(axis.text.x = element_text(color="#000000", size=14, angle=0),
        axis.text.y = element_text(color="#000000", size=14, angle=0),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        legend.justification=c(0,0), 
        legend.position=c(0.01,0.6),
        legend.background = element_rect(fill=alpha('white', 1.0))) +
  scale_x_continuous(breaks=seq(1980,2015,2), 
                     labels=ifelse(axis_x_seq %in% c(1980, 1990, 2000, 2010), axis_x_seq, "")) + 
  scale_fill_discrete(name="Test")
  

ggsave("./output/Fig2B.png", width = 7.0, height = 4.5, units="in")
