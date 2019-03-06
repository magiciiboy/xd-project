###Probability Distribution of Year of First Publication
ggplot(part2, aes(x = year1, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3a <- ggplot(part2, aes(x = year1, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(year1))
head(mu)
fig3a <- fig3a + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Year of First Publication", breaks = c(1960, 1980, 2000, 2010)) + scale_y_continuous(name = "PDF (yi0)")
fig3a
fig3ap <- ggplotly(fig3a)
fig3ap

###Probability Distribution of Total Collaboration degree - used normal r plot
ggplot(part2, aes(x = ktotal, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3b <- ggplot(part2, aes(x = ktotal, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(ktotal))
head(mu)
fig3b <- fig3b + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Total Degree of Collaboration, Ki", breaks = c(0, 500, 1000, 1500)) + scale_y_continuous(name = "PDF (Ki)")
fig3b
fig3bp <- ggplotly(fig3b)
fig3bp

###Probability Distribution of Cross-Disciplinarity
ggplot(part2, aes(x = X, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
fig3c <- ggplot(part2, aes(x = X, fill = XDIndicator)) + geom_density(col = NA, alpha = 0.5)
df <- data.frame(year1, ktotal, X, XDIndicator)
mu <- ddply(df, "XDIndicator", summarise, grp.mean=mean(X))
head(mu)
fig3c <- fig3c + geom_vline(data = mu, aes(xintercept=grp.mean, color = XDIndicator), linetype = "dashed") + scale_x_continuous(name = "Cross-Disciplinarity", breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) + scale_y_continuous(name = "PDF (??i)")
fig3c
fig3cp <- ggplotly(fig3c)
fig3cp