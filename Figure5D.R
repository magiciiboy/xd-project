### Plot figure 5C
library(data.table)
library(ggplot2)
library(grid)

library(sjstats)
library(plm)
library(dplyr)


# dat <- fread('./preprocessed/S4S5/panel_model_paper_citations_data_xd_A.csv')
# 
# dat <- na.omit(dat)
# dat <- dat %>% filter(PR > 0)
# 
# coef = numeric(1000)
# 
# for (i in 1:1000) {
#     dat$I = sample(dat$I)
#     model_FE <- plm(z ~ ln_a + tau + I + factor(t), data=dat, index=c("i"), model="within", effect="individual")
#     coef[i] = model_FE$coefficients["I"]
# }
# write.table(coef, "./preprocessed/Figure5D.csv", sep = ",")

coef = read.table("./preprocessed/Figure5D.csv", sep=",")

plotC <- ggplot(coef)

plotC = plotC + geom_histogram( aes(x=x, stat(ndensity)), bins = 40, color="black", fill="gray" ) + 
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0.112, color="blue", linetype = "dashed") +
        geom_vline(xintercept = 0.07, color="blue") +
        geom_vline(xintercept = 0.16, color="blue") +
    coord_cartesian(ylim = c(0, 1.2), xlim = c(-0.08, 0.22), expand = FALSE) +
    xlab("Cross-disciplinarity coefficient, BI") + ylab("Prob. dist. P(BI)")


plotC = plotC +
    annotate("text", x = -0.06, y = 0.5, label = "Placebo Model", size = 6, angle = 90) +
    annotate("text", x = -0.05, y = 0.5, label = "95% CI=(-0.028, 0.028)", size = 5, angle = 90) +
    annotate("text", x = 0.07, y = 1.1, label = "Only XD Fi", size = 5) +
    annotate("text", x = 0.105, y = 0.5, label = expression(beta[i]==0.112), size = 6, angle = 90, color="blue") +
    annotate("text", x = 0.119, y = 0.5, label = "95% CI=(0.07, 0.16)", size = 5, angle = 90, color="blue")

    
plotC = plotC + theme_light() +
        theme(axis.text.x = element_text(size=16, face="bold", margin=unit(c(20,20,20,20), "pt") ),
              axis.text.y = element_text(size=16, face="bold", margin=unit(c(20,20,20,20), "pt") ),
              axis.title = element_text(size=16, face="bold" ),
              plot.margin = unit(c(6,2,2,2), "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.length=unit(-10, "pt")
              ) 

plotC

ggsave('./output/Figure5D.png', plot=plotC, width = 9, height = 6, dpi = 120)

