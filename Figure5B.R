### Plot figure 6B
library(ggplot2)
library(grid)

coeffAllFE <- c(0.2061, -0.0971, 0.1453)
sdAllFE <- c(0.00173, 0.01601, 0.01578)

coeffXDFE <- c(0.252, -0.0663, 0.112)
sdXDFE <- c(0.00282, 0.0271, 0.0162)

coeffXDMatchFE <- c(0.373, 0.491, 0.135)
sdXDMatchFE <- c(0.0333, 0.0519, 0.0471)

coeffAll <- c(coeffAllFE, coeffXDFE, coeffXDMatchFE)
sdAll <- c(sdAllFE, sdXDFE, sdXDMatchFE)

df <- data.frame(
    x = c(c(4, 14, 24), c(5, 15, 25), c(6, 16, 26)),
    coeff = coeffAll,
    group = factor(c(0, 0, 0, 1, 1, 1, 2, 2, 2), levels=c(0, 1, 2)),
    color = factor(rep("blue", 9), levels=c("blue")),
    upper = coeffAll + sdAll,
    lower = coeffAll - sdAll
)

plotB <- ggplot(df)

# Pointranges
plotB <- plotB + geom_point(aes(x=x, y=coeff, ymin = lower, ymax = upper, group=group, colour=color, shape=group), color="blue", fatten = 5)
plotB <- plotB +
    geom_errorbar(aes(x=x, ymin = lower, ymax = upper, group=group, colour=color), width = 0.2) +
    geom_hline(yintercept=0, linetype=2) 

# Axis Label
x_breaks <- c(5.0, 15.0, 25.0)
x_labs <- c(expression("Coauthor," ~ bolditalic( beta[paste(alpha)] )),
            expression("Author Age," ~ bolditalic( beta[paste(tau)] )),
            expression("Cross disc.," ~ bolditalic( beta[paste("I")] ))
)
plotB <- plotB + xlab("") + 
    ylab((expression(atop("Regression", "coefficients")))) +
    scale_x_continuous(breaks=x_breaks, labels=x_labs)

# Themes
plotB <- plotB + theme_bw() +
    theme(axis.text.x = element_text(size=12, face="bold" ),
          axis.text.y = element_text(size=14, face="bold" ),
          axis.title = element_text(size=14, face="bold" ),
          plot.margin = unit(c(6,2,2,2), "lines") ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Annotation: Significant level
tg1 <- textGrob("*", rot = 90)
tg2 <- textGrob("**", rot = 90)
tg3 <- textGrob("***", rot = 90)

plotB <- plotB + 
    annotation_custom(grob = tg3,  xmin = 4, xmax = 4, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg3,  xmin = 5, xmax = 5, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg3,  xmin = 6, xmax = 6, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg3,  xmin = 14, xmax = 14, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg1,  xmin = 15, xmax = 15, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg3,  xmin = 16, xmax = 16, ymin = 0.65, ymax = 0.65) + 
    annotation_custom(grob = tg3,  xmin = 24, xmax = 24, ymin = 0.65, ymax = 0.65) +
    annotation_custom(grob = tg3,  xmin = 25, xmax = 25, ymin = 0.65, ymax = 0.65) + 
    annotation_custom(grob = tg2,  xmin = 26, xmax = 26, ymin = 0.65, ymax = 0.65)

# Legend
plotB <- plotB + theme(legend.position=c(.5, .6), legend.title=element_text(size=10))


plotB <- plotB + scale_color_manual(values=c("blue", "black"),
                                    name=NULL,
                                    breaks=c("blue", "black"),
                                    labels=c("Fixed effects: Standardized vars.", 
                                             "Pooled: Standardized variables"))

plotB <- plotB + scale_shape_manual(values=c(16, 15, 18),
                                    name=NULL,
                                    breaks=c(0, 1, 2),
                                    labels=c("All Fi", 
                                             "Only XD Fi",
                                             "Only XD Fi: matched"))

gt <- ggplot_gtable(ggplot_build(plotB))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

