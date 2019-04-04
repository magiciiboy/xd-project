# install.packages('plm')
# install.packages('sjstats')
library(sjstats)
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_xd.csv')

print(colnames(dat))
print(nrow(dat))
head(dat)

# Remove row having NA (Ex: pagerank)
dat <- na.omit(dat)

# summary(dat)
# str(dat)
nrow(dat)

# Model: No FE
model_NoFE <- lm(z ~ ln_a + tau + I + factor(t) + PR + lamda, data=dat)
summary(model_NoFE)
coff <- coefficients(model_NoFE)
print(coff['PR'])
print(coff['lamda'])

# Model: No FE (Standardized)
std_beta(model_NoFE)
# Or:
# model_NoFE_std <- lm(scale(z) ~ scale(ln_a) + scale(tau) + scale(I) + factor(t) + scale(PR) + scale(lamda) + factor(dept), data=dat)
# summary(model_NoFE_std)

# Model: FE
# lmer 
# model_FE <- plm(z ~ ln_a + tau + I + factor(t), data=dat, index=c("i", "X"), model="within", effect="individual")
model_FE <- lm(z ~ ln_a + tau + I + factor(t) + factor(i), data=dat)
summary(model_FE)
print(paste0('Interception: ', coefficients(model_FE)['(Intercept)']))

# Model: FE (Standardized)
std_beta(model_FE)


# RESULTS:
# Model: Non FE
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -0.820509   0.750937  -1.093 0.274550    
# ln_a                   0.351202   0.003920  89.590  < 2e-16 ***
# tau                    0.034904   0.020317   1.718 0.085806 .  
# I                      0.112352   0.016287   6.898 5.28e-12 ***


# Model: FE
# Coefficients: (1 dropped because of singularities)
#                 Estimate Std. Error t-value  Pr(>|t|)    
# ln_a           0.3512021  0.0039201 89.5899 < 2.2e-16 ***
# tau           -0.0061721  0.0025318 -2.4378 0.0147768 *  
# I              0.1123525  0.0162866  6.8985 5.276e-12 ***
# (Rows of coefficients of D(t) were removed for shortenning the results)
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Total Sum of Squares:    156870
# Residual Sum of Squares: 149220
# R-Squared:      0.048815
# Adj. R-Squared: 0.041376
# F-statistic: 180.524 on 47 and 165327 DF, p-value: < 2.22e-16

# With Dummy
# Coefficients: (1 not defined because of singularities)
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)            0.830825   0.451095   1.842 0.065507 .  
# ln_a                   0.351202   0.003920  89.590  < 2e-16 ***
# tau                    0.063095   0.019555   3.227 0.001253 ** 
# I                      0.112352   0.016287   6.898 5.28e-12 ***

# Model: Fixed Effects (Standardized)

