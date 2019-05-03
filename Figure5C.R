### Plot figure 5C
library(data.table)
library(ggplot2)
library(grid)

library(sjstats)
library(plm)
library(dplyr)


# dat <- fread('./preprocessed/S4S5/panel_model_paper_citations_data_all_A.csv')
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
# write.table(coef, "./preprocessed/Figure5C.csv", sep = ",")

coef = read.table("./preprocessed/Figure5C.csv", sep=",")

# coef = data.frame(coef)

plotC <- ggplot(coef)

plotC = plotC + geom_histogram( aes(x=x, stat(ndensity)), bins = 40, color="black", fill="gray" ) + 
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_vline(xintercept = 0.145, color="blue", linetype = "dashed") +
        geom_vline(xintercept = 0.1, color="blue") +
        geom_vline(xintercept = 0.19, color="blue") +
    coord_cartesian(ylim = c(0, 1.2), xlim = c(-0.08, 0.22), expand = FALSE) +
    xlab("Cross-disciplinarity coefficient, BI") + ylab("Prob. dist. P(BI)")


plotC = plotC +
    annotate("text", x = -0.06, y = 0.5, label = "Placebo Model", size = 4.5, angle = 90) +
    annotate("text", x = -0.05, y = 0.5, label = "95% CI=(-0.027, 0.029)", size = 3, angle = 90) +
    annotate("text", x = 0.07, y = 1.1, label = "All Fi", size = 5) +
    annotate("text", x = 0.14, y = 0.5, label = expression(beta[i]==0.145), size = 4, angle = 90, color="blue") +
    annotate("text", x = 0.15, y = 0.5, label = "95% CI=(0.10, 0.19)", size = 3, angle = 90, color="blue")

    
plotC = plotC + theme_bw() +
        theme(axis.text.x = element_text(size=12, face="bold", margin=unit(c(14,14,0,10), "pt") ),
              axis.text.y = element_text(size=12, face="bold", margin=unit(c(14,14,0,10), "pt") ),
              axis.title = element_text(size=12, face="bold" ),
              plot.margin = unit(c(2,2,2,2), "lines"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.length=unit(-10, "pt")
              ) 


plotC

ggsave('./output/Figure5C.png', plot=plotC, width = 9, height = 6, dpi = 120)
