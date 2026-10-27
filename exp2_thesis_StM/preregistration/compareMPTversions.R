# compareMPTversions.R
# Compare parameter estimates of MPT model with and without RT extension. Are
# parameter estimates from the simpler model similar to estimates for c, u, and
# g when fitting the RT extension?
#
# Input: -
# Output: -
# Packages: mpt
#
# Last mod: 05/03/2026, AO

# setwd("Uni/evaluative-conditioning/exp2/preregistration")
library(mpt)

spec <- mptspec(
    # Condition|CS valence.Response
    # s=standard, r=reversed, p=positive, n=negative
    sp.p = c + (1-c)*u + (1-c)*(1-u)*g,
    sp.n = (1-c)*(1-u)*(1-g),
    sn.p = (1-c)*(1-u)*g,
    sn.n = c + (1-c)*u + (1-c)*(1-u)*(1-g),
    rp.p = (1-c)*u + (1-c)*(1-u)*g,
    rp.n = c + (1-c)*(1-u)*(1-g),
    rn.p = c + (1-c)*(1-u)*g,
    rn.n = (1-c)*u + (1-c)*(1-u)*(1-g)
)

specRT <- mptspec(
    # Condition|CS valence.Response
    # s=standard, r=reversed, p=positive, n=negative, f=fast, s=slow
    sp.pf = c*f_c + (1-c)*u*f_u + (1-c)*(1-u)*g*f_g,
    sp.ps = c*(1-f_c) + (1-c)*u*(1-f_u) + (1-c)*(1-u)*g*(1-f_g),
    sp.nf = (1-c)*(1-u)*(1-g)*f_g,
    sp.ns = (1-c)*(1-u)*(1-g)*(1-f_g),
    sn.pf = (1-c)*(1-u)*g*f_g,
    sn.ps = (1-c)*(1-u)*g*(1-f_g),
    sn.nf = c*f_c + (1-c)*u*f_u + (1-c)*(1-u)*(1-g)*f_g,
    sn.ns = c*(1-f_c) + (1-c)*u*(1-f_u) + (1-c)*(1-u)*(1-g)*(1-f_g),
    rp.pf = (1-c)*u*f_u + (1-c)*(1-u)*g*f_g,
    rp.ps = (1-c)*u*(1-f_u) + (1-c)*(1-u)*g*(1-f_g),
    rp.nf = c*f_c + (1-c)*(1-u)*(1-g)*f_g,
    rp.ns = c*(1-f_c) + (1-c)*(1-u)*(1-g)*(1-f_g),
    rn.pf = c*f_c + (1-c)*(1-u)*g*f_g,
    rn.ps = c*(1-f_c) + (1-c)*(1-u)*g*(1-f_g),
    rn.nf = (1-c)*u*f_u + (1-c)*(1-u)*(1-g)*f_g,
    rn.ns = (1-c)*u*(1-f_u) + (1-c)*(1-u)*(1-g)*(1-f_g)
)

simulate_mptRT <- function(n, cval, fcval, uval, fuval, gval=0.5, fgval=0.5) {
    m <- structure(list(
        treeid = specRT$treeid,
        n = setNames(rep(n, 16), specRT$treeid),
        pcat = specRT$par2prob(
                    c(c=cval, f_c=fcval, u=uval, f_u=fuval, g=gval, f_g=fgval)
               ) # has to be in THAT order!
    ), class = "mpt")
    simulate(m)
}

sim <- replicate(100, {
    y <- simulate_mptRT(n=140, cval=.15, fcval=.3, uval=.15, fuval=.7)
    m1 <- mpt(specRT, y, method="EM")
    m2 <- mpt(spec, apply(matrix(y, ncol=2, byrow=TRUE), 1, sum), method="EM")
    list(m1=coef(m1), m2=coef(m2))
}, simplify=FALSE)

pars <- list(m1_RT=do.call(rbind, lapply(sim, function(x) x[["m1"]])),
         m2_simple=do.call(rbind, lapply(sim, function(x) x[["m2"]])))

par(mar=c(0, 0, 0, 0), oma=c(3, 3, 1, 1), mgp=c(2, .7, 0), mfrow=1:2)
boxplot(pars$m1_RT, ylab="Estimated value", ylim=0:1)
points(1:6, c(.15, .3, .15, .7, .5, .5), pch=16, col="blue", cex=1.3)
legend("topleft", "True value", pch=16, col="blue", cex=1, bty="n")
boxplot(pars$m2_simple, ylab="Estimated value", ylim=0:1)
points(1:3, c(.15, .15, .5), pch=16, col="blue", cex=1.3)

# Recovery is decent, irrespective of model version!
