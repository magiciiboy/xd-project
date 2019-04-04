# install.packages('plm')
# install.packages('sjstats')
library(sjstats)
library(plm)
library(dplyr)
ZERO_REPLACEMENT_CONSTANT <- 1
ZERO_REPLACEMENT_CONSTANT_NUM <- 1
ZERO_REPLACEMENT_CONSTANT_AMT <- 1

dat <- read.csv('./data/Faculty_GoogleScholar_Funding_Data_N4190.csv')

print(colnames(dat))
print(nrow(dat))

# Summarize the data characteristics
# head(dat)
# summary(dat)
# str(dat)

# Treat NA, zeros values
cols_NA <- unlist(lapply(dat, function(x) any(is.na(x))))
cols_0s <- unlist(lapply(dat, function(x) any(x==0)))

cols_NA <- cols_NA[cols_NA==T]
cols_0s <- cols_0s[cols_0s==T]
print(names(cols_NA))
print(names(cols_0s))

# Note: No NA values

# List of columns having 0s values
# [1] "i10index"       "mean_of_IF"     "num_nsf"        "t_deflated_nsf" "num_nih"        "t_deflated_nih"
# [7] "KTotal"         "KDirect"        "KMediated"      "Chi"            "BetCentrality"  "PRCentrality" 

# Note: This way seems does not work really well
# Parameters of # of grants and total amount of grants are not close to the ones in the paper
# for (cname in names(cols_0s)) {
#  if (cname != 'PRCentrality') {
#    dat[cname] -> col
#    col[col==0] <- ZERO_REPLACEMENT_CONSTANT
#    dat[cname] <- col
#  }
# }
# 
# Try this manual way
col_num_nsf <- dat$num_nsf
col_num_nsf[col_num_nsf==0] <- ZERO_REPLACEMENT_CONSTANT_NUM
dat$num_nsf <- col_num_nsf

col_num_nih <- dat$num_nih
col_num_nih[col_num_nih==0] <- ZERO_REPLACEMENT_CONSTANT_NUM
dat$num_nih <- col_num_nih

col_t_deflated_nsf <- dat$t_deflated_nsf
col_t_deflated_nsf[col_t_deflated_nsf==0] <- ZERO_REPLACEMENT_CONSTANT_AMT
dat$t_deflated_nsf <- col_t_deflated_nsf

col_t_deflated_nih <- dat$t_deflated_nih
col_t_deflated_nih[col_t_deflated_nih==0] <- ZERO_REPLACEMENT_CONSTANT_AMT
dat$t_deflated_nih <- col_t_deflated_nih


# Rename the column name
dat$C_i <- dat$t_pubs_citations
dat$r_i <- dat$SchoolRank
dat$dollar_NSF <- dat$t_deflated_nsf
dat$dollar_NIH <- dat$t_deflated_nih
dat$C_PR <- dat$PRCentrality
dat$C_B <- dat$BetCentrality
dat$O <- dat$XDIndicator
# dat %>% mutate(C_D=Degree)

# Remove old columns
# dat$t_pubs_citations <- NULL
# dat$SchoolRank <- NULL
# dat$t_deflated_nsf <- NULL
# dat$t_deflated_nih <- NULL
# dat$PRCentrality <- NULL
# dat$BetCentrality <- NULL

# Build models
# str(dat)

# Model 1: CV
print(nrow(dat))
model_CV <- lm(log(C_i) ~ log(r_i) + log(h_index) + log(dollar_NSF) 
                          + log(num_nsf) + log(dollar_NIH) + log(num_nih)
                          + factor(O) + factor(Y05yr), data=dat)
summary(model_CV)

# Model 2: CV
# Remove row having NA (Ex: pagerank). Only keep 3900 nodes in the network.
datNet <- dat %>% filter(C_PR > 0)

print(nrow(datNet))
model_CV_Net <- lm(log(C_i) ~ log(r_i) + log(h_index) + log(dollar_NSF) 
               + log(num_nsf) + log(dollar_NIH) + log(num_nih)
               + log(C_PR) + Chi
               + factor(O) + factor(Y05yr), data=datNet)
summary(model_CV_Net)

# Model 3: CV (Standardized)
model_CV_Net_std <- lm(log(C_i) ~ scale(log(r_i)) + scale(log(h_index)) + scale(log(dollar_NSF)) 
                   + scale(log(num_nsf)) + scale(log(dollar_NIH)) + scale(log(num_nih))
                   + scale(log(C_PR)) + scale(Chi)
                   + factor(O) + factor(Y05yr), data=datNet)
summary(model_CV_Net_std)

# Summary
# Model CV:
c(model_CV$coef["log(r_i)"][[1]],
  model_CV$coef["log(h_index)"][[1]],
  model_CV$coef["log(dollar_NSF)"][[1]],
  model_CV$coef["log(num_nsf)"][[1]],
  model_CV$coef["log(dollar_NIH)"][[1]],
  model_CV$coef["log(num_nih)"][[1]],
  model_CV$coef["(Intercept)"][[1]])

# Model CV + Network:
c(model_CV_Net$coef["log(r_i)"][[1]],
  model_CV_Net$coef["log(h_index)"][[1]],
  model_CV_Net$coef["log(dollar_NSF)"][[1]],
  model_CV_Net$coef["log(num_nsf)"][[1]],
  model_CV_Net$coef["log(dollar_NIH)"][[1]],
  model_CV_Net$coef["log(num_nih)"][[1]],
  model_CV_Net$coef["log(C_PR)"][[1]],
  model_CV_Net$coef["Chi"][[1]],
  model_CV_Net$coef["(Intercept)"][[1]])

# Model CV + Network (Standardized):
c(model_CV_Net_std$coef["scale(log(r_i))"][[1]],
  model_CV_Net_std$coef["scale(log(h_index))"][[1]],
  model_CV_Net_std$coef["scale(log(dollar_NSF))"][[1]],
  model_CV_Net_std$coef["scale(log(num_nsf))"][[1]],
  model_CV_Net_std$coef["scale(log(dollar_NIH))"][[1]],
  model_CV_Net_std$coef["scale(log(num_nih))"][[1]],
  model_CV_Net_std$coef["scale(log(C_PR))"][[1]],
  model_CV_Net_std$coef["scale(Chi)"][[1]],
  model_CV_Net_std$coef["(Intercept)"][[1]])
