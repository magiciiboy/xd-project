# install.packages('plm')
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_xd.csv')

print(colnames(dat))
print(nrow(dat))

bins = (max(dat$t) - min(dat$t)) / 5
dat$Dt <- cut(dat$t, bins)

# summary(dat)
# str(dat)

model <- plm(z ~ ln_a + tau + I + factor(Dt), data=dat, index=c("s", "X"), model="within")
summary(model)

model2 <- plm(z ~ ln_a + tau + I + factor(Dt), data=dat, index=c("s"), model="within")
summary(model2)

model3 <- plm(z ~ ln_a + tau + I + factor(Dt), data=dat, index=c("s"), model="random")
summary(model3)

model4 <- plm(z ~ ln_a + tau + I + factor(Dt), data=dat, index=c("X", "s"), model="within")
summary(model4)

