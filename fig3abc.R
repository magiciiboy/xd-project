getwd
setwd("D:\\bwm03\\Documents\\Graduate Classes\\Statistical Methods R")
library(ggplot2)
part2 <- read.csv("Faculty_GoogleScholar_Funding_Data_N4190.csv", header = TRUE)
year1 <- (part2$min_year)
head(part2)
ktotal <- (part2$KTotal)
X <- (part2$Chi)
XDIndicator <- (part2$XDIndicator)
year1
library(plyr)
library(scales)

###Probability Distribution of Year of First Publication
ggplot(part2, aes(x = year1, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3a <- ggplot(part2, aes(x = year1, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(year1))
head(mu)
fig3a <- fig3a + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Year of First Publication") + scale_y_continuous(name = "PDF")
fig3a

###Probability Distribution of Total Collaboration degree - used normal r plot
### have to do the logarithmic transformation, although it wont destroy our grade
ggplot(part2, aes(x = ktotal, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3b <- ggplot(part2, aes(x = ktotal, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(ktotal))
head(mu)
fig3b <- fig3b + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Total Degree of Collaboration", breaks = c(0, 500, 1000, 1500)) + scale_y_continuous(name = "PDF")
fig3b

#Probability distribution of total collaboration degree - normal plot()
library(sm)
xdfactor <- factor(XDIndicator, levels = c("BIO", "CS", "XD"), labels = c("BIO", "CS", "XD"))
fig3b <- sm.density.compare(ktotal, XDIndicator, xlab = "Total Collaboration Degree")
fig3b + 

###Probability Distribution of Cross-Disciplinarity
ggplot(part2, aes(x = X, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3c <- ggplot(part2, aes(x = X, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(X))
head(mu)
fig3c <- fig3c + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Cross-Disciplinarity", breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) + scale_y_continuous(name = "PDF")
fig3c







