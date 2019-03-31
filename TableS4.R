# install.packages('plm')
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_all.csv')

print(colnames(dat))
print(nrow(dat))

bins = (max(dat$t) - min(dat$t)) / 5
dat$Dt <- cut(dat$t, bins)

# summary(dat)
# str(dat)

model <- plm(z ~ ln_a + tau + I + factor(Dt), data=dat, index=c("i", "X"), model="within", effect="time")
summary(model)

