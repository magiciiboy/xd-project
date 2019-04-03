library(ggplot2)
library(grid)



df <- data.frame(
  trt = factor(c(1, 2, 3, 4, 5, 6, 7)),
  resp = c(1, 2, 3, 4, 3, 4, 5),
  upper = c(1.3, 3, 3.3, 4.2, 3, 3.3, 4.2),
  lower = c(0.8, 1.4, 2.4, 3.6, 1.4, 2.4, 3.6)
)

x_labs <- c( expression( bolditalic( beta[paste("r")] ) ), #1
             expression( bolditalic( beta[paste("$1")] ) ), #2
             expression( bolditalic( beta[paste("N1")] ) ), #3
             expression( bolditalic( beta[paste("$2")] ) ), #4
             expression( bolditalic( beta[paste("N2")] ) ), #5
             expression( bolditalic( beta[paste("C") ^ "PR"] ) ), #6
             expression( bolditalic( beta[paste(chi)] ) )  #7
             )

p <- ggplot(df, aes(x=trt, y=resp))
p = p + geom_pointrange(aes(ymin = lower, ymax = upper), fatten = 5) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept=0, linetype=2) +
  xlab("") + ylab("Standardized regression coefficients") +
  scale_x_discrete(breaks=1:7, labels=x_labs ) + theme_light() +
  theme(axis.text.x = element_text(size=20, face="bold",family="serif" ),
        axis.text.y = element_text(size=20, face="bold" ),
        axis.title = element_text(size=20, face="bold" ),
        plot.margin = unit(c(6,2,2,2), "lines") )

lg = linesGrob(gp=gpar(lwd=3))

p = p +
  annotation_custom(grob = textGrob("CV"),  xmin = 3, xmax = 3, ymin = 6.3, ymax = 6.3) +
  annotation_custom(grob = lg, xmin = 0.7, xmax = 5.3, ymin = 6, ymax = 6) +
  annotation_custom(grob = lg, xmin = 0.7, xmax = 0.7, ymin = 5.8, ymax = 6) +
  annotation_custom(grob = lg, xmin = 5.3, xmax = 5.3, ymin = 5.8, ymax = 6)

p = p +
  annotation_custom(grob = textGrob("Network"),  xmin = 6.5, xmax = 6.5, ymin = 6.3, ymax = 6.3) +
  annotation_custom(grob = lg, xmin = 5.7, xmax = 7.3, ymin = 6, ymax = 6) +
  annotation_custom(grob = lg, xmin = 5.7, xmax = 5.7, ymin = 5.8, ymax = 6) +
  annotation_custom(grob = lg, xmin = 7.3, xmax = 7.3, ymin = 5.8, ymax = 6)

tg = textGrob("***", rot = 90)

p = p + 
  annotation_custom(grob = tg,  xmin = 1, xmax = 1, ymin = 5.5, ymax = 5.5) +
  annotation_custom(grob = tg,  xmin = 4, xmax = 4, ymin = 5.5, ymax = 5.5) +
  annotation_custom(grob = tg,  xmin = 5, xmax = 5, ymin = 5.5, ymax = 5.5) + 
  annotation_custom(grob = tg,  xmin = 7, xmax = 7, ymin = 5.5, ymax = 5.5)

  
gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)
