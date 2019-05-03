### Plot figure 6A
library(ggplot2)
library(grid)

coeffAllNoFE <- c(0.1872, -0.0560, 0.1259)
sdAllNoFE <- c(0.0016, 0.0018, 0.0157)

coeffAllFE <- c(0.2061, -0.0971, 0.1453)
sdAllFE <- c(0.00173, 0.01601, 0.01578)

coeffAll <- c(coeffAllFE, coeffAllNoFE)
sdAll <- c(sdAllFE, sdAllNoFE)

df <- data.frame(
    x = c(c(4, 14, 24), c(6, 16, 26)),
    coeff = coeffAll,
    group = factor(c(1, 1, 1, 0, 0 ,0), levels=c(0, 1)),
    color = factor(c("blue", "blue", "blue", "black", "black", "black"), levels=c("blue", "black")),
    upper = coeffAll + sdAll,
    lower = coeffAll - sdAll
)

plotA <- ggplot(df)

# Pointranges
plotA <- plotA + geom_point(aes(x=x, y=coeff, ymin = lower, ymax = upper, group=group, colour=color), fatten = 5)
plotA <- plotA +
    geom_errorbar(aes(x=x, ymin = lower, ymax = upper, group=group, colour=color), width = 0.2) +
    geom_hline(yintercept=0, linetype=2) 

# Axis Label
x_breaks <- c(5.0, 15.0, 25.0)
x_labs <- c(expression("Coauthor," ~ bolditalic( beta[paste(alpha)] )),
            expression("Author Age," ~ bolditalic( beta[paste(tau)] )),
            expression("Cross disc.," ~ bolditalic( beta[paste("I")] ))
)
plotA <- plotA + xlab("") + 
        ylab((expression(atop("Regression", "coefficients")))) +
        scale_x_continuous(breaks=x_breaks, labels=x_labs)

# Themes
plotA <- plotA + theme_bw() +
    theme(axis.text.x = element_text(size=12, face="bold" ),
          axis.text.y = element_text(size=14, face="bold" ),
          axis.title = element_text(size=14, face="bold" ),
          plot.margin = unit(c(6,2,2,2), "lines") ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Annotation: Significant level
tg <- textGrob("***", rot = 90)
plotA <- plotA + 
    annotation_custom(grob = tg,  xmin = 4, xmax = 4, ymin = 0.25, ymax = 0.25) +
    annotation_custom(grob = tg,  xmin = 6, xmax = 6, ymin = 0.25, ymax = 0.25) +
    annotation_custom(grob = tg,  xmin = 14, xmax = 14, ymin = 0.25, ymax = 0.25) + 
    annotation_custom(grob = tg,  xmin = 16, xmax = 16, ymin = 0.25, ymax = 0.25) +
    annotation_custom(grob = tg,  xmin = 24, xmax = 24, ymin = 0.25, ymax = 0.25) + 
    annotation_custom(grob = tg,  xmin = 26, xmax = 26, ymin = 0.25, ymax = 0.25)

plotA <- plotA +
    xlab("") + ylab("Regression coeffcients")

# Legend
plotA <- plotA + theme(legend.position=c(.5, .75), legend.title=element_text(size=10))


plotA <- plotA + scale_color_manual(values=c("blue", "black"),
                                     name=expression("All faculty" ~ italic("F"[paste("i")])),
                                     breaks=c("blue", "black"),
                                     labels=c("Fixed effects: Standardized variables", 
                                                "Pooled: Standardized variables"))

gt <- ggplot_gtable(ggplot_build(plotA))
gt$layout$clip[gt$layout$name=="panel"] <- "off"
grid.draw(gt)

ggsave('./output/Figure5A.png', plot=gt, width = 9, height = 6, dpi = 120)
