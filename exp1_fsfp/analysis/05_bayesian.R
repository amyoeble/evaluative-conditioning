# 05_bayesian.R
# Bayesian estimateion of MPT parameters for EC Exp 1
#
# Input: 01_preprocessing.R
# Output: extended-mpt.stan
# Packages: rstan
#
# Last mod: 10/09/2025, AO

library(rstan)
options(mc.cores=parallel::detectCores())
rstan_options(auto_write=TRUE)


## PREPROCESSING
source("01_preprocessing.R")
tab <- xtabs( ~ attitude + USvalence + relation + context, dat)

# MODEL
m <- stan(file="extended-mpt.stan", data=list(ntrials=c(apply(tab, 2:4, sum)), 
          y=matrix(tab, nrow=8, ncol=2, byrow=TRUE)))
print(m, pars=c("p_c", "p_u", "a", "g"), probs=c(.025, .975))

