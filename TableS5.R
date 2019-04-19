# install.packages('plm')
# install.packages('sjstats')
# install.packages("robustHD")

library(sjstats)
library(plm)
library(dplyr)
library(robustHD)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_xd.csv')

print(colnames(dat))
print(nrow(dat))
head(dat)

# Remove row having NA (Ex: pagerank)
dat <- na.omit(dat)

# summary(dat)
str(dat)
nrow(dat)

# Model: No FE
model_NoFE <- lm(z ~ ln_a + tau + I + factor(t) + PR + lamda + factor(dept), data=dat)
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

# RESULTS:
# Model: Non FE
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           -0.820509   0.750937  -1.093 0.274550    
# ln_a                   0.351202   0.003920  89.590  < 2e-16 ***
# tau                    0.034904   0.020317   1.718 0.085806 .  
# I                      0.112352   0.016287   6.898 5.28e-12 ***


# MODEL: FE
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


# MODEL: Fixed Effects (Standardized)
# Oneway (individual) effect Within Model

# Call:
# plm(formula = z ~ ln_a_scaled + tau_scaled + I + factor(t), data = dat, 
#     effect = "individual", model = "within", index = c("i", "X"))

# Unbalanced Panel: n = 1247, T = 3-1388, N = 166621

# Residuals:
#      Min.   1st Qu.    Median   3rd Qu.      Max. 
# -3.612039 -0.630061  0.031691  0.635723  5.373274 

# Coefficients: (1 dropped because of singularities)
#                 Estimate Std. Error t-value  Pr(>|t|)    
# ln_a_scaled    0.2523839  0.0028171 89.5899 < 2.2e-16 ***
# tau_scaled    -0.0662938  0.0271938 -2.4378 0.0147768 *  
# I              0.1123525  0.0162866  6.8985 5.276e-12 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Total Sum of Squares:    156870
# Residual Sum of Squares: 149220
# R-Squared:      0.048815
# Adj. R-Squared: 0.041376
# F-statistic: 180.524 on 47 and 165327 DF, p-value: < 2.22e-16

