# install.packages('plm')
# install.packages('sjstats')
library(sjstats)
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_all.csv')

print(colnames(dat))
print(nrow(dat))
head(dat)

# Remove row having NA (Ex: pagerank)
dat <- na.omit(dat)

print(nrow(dat))

# summary(dat)
# str(dat)
# Model: No FE
model_NoFE <- lm(z ~ ln_a + tau + I + factor(t) + PR + lamda + factor(dept), data=dat)
summary(model_NoFE)

# Model: No FE (Standardized)
model_NoFE_std <- lm(scale(z) ~ scale(ln_a) + scale(tau) + scale(I) + factor(t) + scale(PR) + scale(lamda) + factor(dept), data=dat)
summary(model_NoFE_std)

# Model: FE
model_FE <- plm(z ~ ln_a + tau + I + factor(t), data=dat, index=c("i", "X"), model="within", effect="individual")
summary(model_FE)

# Model: FE (Standardized)
model_FE_std <- plm(scale(z) ~ scale(ln_a) + scale(tau) + scale(I), data=dat, index=c("i", "X"), model="within", effect="individual")
summary(model_FE_std)
