# install.packages('plm')
# install.packages('sjstats')
# install.packages("robustHD")

library(sjstats)
library(plm)
library(dplyr)
library(robustHD)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_xd_matched_A.csv')

print(colnames(dat))
print(nrow(dat))
head(dat)

# Remove row having NA (Ex: pagerank)
dat <- na.omit(dat)

# summary(dat)
str(dat)
nrow(dat)

# Model: No FE
model_NoFE <- lm(z ~ log(a) + tau + I + factor(t) + log(PR) + log(lambda) + factor(dept), data=dat)
summary(model_NoFE)
anova(model_NoFE)
coff <- coefficients(model_NoFE)
print(coff)
print(coff)

# Model: No FE (Standardized)
model_NoFE_std <- std_beta(model_NoFE)
# Or:
model_NoFE_std <- lm(z ~ scale(log(a)) + scale(tau) + I + factor(t) + scale(log(PR)) + scale(log(lambda)) + factor(dept), data=dat)
summary(model_NoFE_std)

# Model: FE
# lmer 
model_FE <- plm(z ~ ln_a + tau + I + factor(t), data=dat, index=c("i"), model="within", effect="individual")
summary(model_FE)
within_intercept(model_FE)

# Model: FE (Standardized)
dat <- dat %>% mutate(ln_a_scaled=scale(dat$ln_a),
                      tau_scaled=scale(dat$tau),
                      I_scaled=scale(dat$I))

model_FE_std <- plm(z ~ ln_a_scaled + tau_scaled + I + factor(t), data=dat, index=c("i"), model="within", effect="individual")
summary(model_FE_std)
within_intercept(model_FE_std)
