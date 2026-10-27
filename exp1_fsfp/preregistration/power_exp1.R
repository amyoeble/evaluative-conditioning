# power_exp1.R
# Power simulation for Exp. 1 of master's project
#
# Input: - 
# Output: -
# Packages: mpt
#
# Last mod: 06/16/2025, AO

# setwd("Uni/evaluative-conditioning/exp1/preregistration/")
library(mpt)

spec <- mptspec(
    # Condition|Reversal|CS valence.Response
    # a=apply, i=ignore relation; s=standard, r=reversed; p=positive, n=negative
    asp.p = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g,
    asp.n = (1-p_c)*(1-p_u)*(1-a)*(1-g),
    asn.p = (1-p_c)*(1-p_u)*(1-a)*g,
    asn.n = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g),
    arp.p = (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g,
    arp.n = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*(1-g),
    arn.p = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*g,
    arn.n = (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g),
    isp.p = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g,
    isp.n = (1-p_c)*(1-p_u)*(1-a)*(1-g),
    isn.p = (1-p_c)*(1-p_u)*(1-a)*g,
    isn.n = p_c + (1-p_c)*p_u + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g),
    irp.p = p_c + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*g,
    irp.n = (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*(1-g),
    irn.p = (1-p_c)*p_u + (1-p_c)*(1-p_u)*(1-a)*g,
    irn.n = p_c + (1-p_c)*(1-p_u)*a + (1-p_c)*(1-p_u)*(1-a)*(1-g)
)

nsubj <- 150
n <- nsubj*24/4 / 2  # nsubj*24CS/4cond / 2 groups (apply/ign. is between-subj)
m <- structure(list(
        treeid = spec$treeid,
        n = setNames(rep(n, 16), spec$treeid),
        pcat = spec$par2prob(c(p_c=0.45, p_u=0.1, a=0.1, g=0.5))
                             # has to be in THAT order!
), class = "mpt")

pval <- replicate(1000, {
    y <- simulate(m)
    m1 <- mpt(spec, data=y)
    m2 <- mpt(update(spec, .restr=list(a=0)), data=y)
    c(p_anova = anova(m2, m1)[["Pr(>Chi)"]][2], 
                p_m1    = m1$goodness.of.fit[["pval"]], 
                p_restr = m2$goodness.of.fit[["pval"]])
})
# p_anova: Power of test; p_m1: alpha; p_restr: restricted model fit
apply(pval, 1, function(x) mean(x < .05))

# test p_c = p_u = a = 0        -> Power is 1-alpha for more or less any n 
#      p_c           = 0        -> a joke, really, no need to power that one
#            p_u     = 0        -> p_u = 0.15: n = 54 , 1-beta = 0.83
#                                  p_u = 0.1:  n = 120, 1-beta = 0.80
#                  a = 0        -> a = 0.15: n = 74,  1-beta = 0.83
#                                  a = 0.1:  n = 150, 1-beta = 0.81
# NOTE: For all simulations, p_u = a
