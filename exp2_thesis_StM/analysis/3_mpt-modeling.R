# 3_mpt-modeling.R
# MPT analysis for shooting the messenger in EC data
#
# Input: 1_data-preprocessing.R
# Output: -
# Packages: mpt
#
# Last mod: 04/26/2026, AO

library(mpt)
source("1_data-preprocessing.R")

spec <- mptspec(
    # Condition.Response
    OwnWin.p = t + (1-t)*s + (1-t)*(1-s)*g,
    OwnWin.n = (1-t)*(1-s)*(1-g),
    OwnLoss.p = (1-t)*(1-s)*g,
    OwnLoss.n = t + (1-t)*s + (1-t)*(1-s)*(1-g),
    OppWin.p = (1-t)*s + (1-t)*(1-s)*g,
    OppWin.n = t + (1-t)*(1-s)*(1-g),
    OppLoss.p = t + (1-t)*(1-s)*g,
    OppLoss.n = (1-t)*s + (1-t)*(1-s)*(1-g)
)

y <- c(xtabs( ~ attitude + framing + perspective, dat))

m0 <- mpt(update(spec, .restr=list(t=0, s=0)), data=y, method="EM", 
          optimargs=list(maxit=2000))
m1 <- mpt(update(spec, .restr=list(s=0)), data=y, method="EM", 
          optimargs=list(maxit=2000))
m2 <- mpt(spec, data=y, method="EM", optimargs=list(maxit=2000))
anova(m0, m1, m2)

# 95% CIs for selected model
confint(m1) |> plogis()

