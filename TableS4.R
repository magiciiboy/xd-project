# install.packages('plm')
library(plm)
library(dplyr)

dat <- read.csv('./preprocessed/S4S5/panel_model_paper_citations_data.csv')
dat %>% mutate(ln_a = log(a)) 

model <- plm(z ~)