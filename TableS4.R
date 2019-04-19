# install.packages('plm')
# install.packages('sjstats')
library(sjstats)
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data_all.csv')

print(colnames(dat))
print(nrow(dat))
head(dat)

# Remove row having NA (Ex: pagerank). Only keep 3900 nodes in the network.
dat <- na.omit(dat)

print(nrow(dat))

# summary(dat)
# str(dat)
# Model: No FE
model_NoFE <- lm(z ~ ln_a + tau + I + factor(t) + PR + lamda + factor(dept), data=dat)
summary(model_NoFE)

# Model: No FE (Standardized)
std_beta(model_NoFE)
# Or:
# model_NoFE_std <- lm(scale(z) ~ scale(ln_a) + scale(tau) + scale(I) + factor(t) + scale(PR) + scale(lamda) + factor(dept), data=dat)
# summary(model_NoFE_std)

# Model: FE
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

# Model: FE
# Oneway (individual) effect Within Model
# plm(formula = z ~ ln_a + tau + I + factor(t), data = dat, effect = "individual", 
#     model = "within", index = c("i", "X"))

# Unbalanced Panel: n = 3900, T = 2-1388, N = 413565

# Residuals:
#      Min.   1st Qu.    Median   3rd Qu.      Max. 
# -3.607925 -0.625324  0.029558  0.621208  5.403245 

# Coefficients: (1 dropped because of singularities)
#                 Estimate Std. Error  t-value  Pr(>|t|)    
# ln_a           0.3121731  0.0026207 119.1183 < 2.2e-16 ***
# tau           -0.0094982  0.0015654  -6.0674 1.301e-09 ***
# I              0.1452960  0.0157895   9.2021 < 2.2e-16 ***
# (Rows of coefficients of D(t) were removed for shortenning the results)
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Total Sum of Squares:    365400
# Residual Sum of Squares: 352230
# R-Squared:      0.036052
# Adj. R-Squared: 0.026766
# F-statistic: 325.952 on 47 and 409618 DF, p-value: < 2.22e-16
# Interception: -0.290435239489826

