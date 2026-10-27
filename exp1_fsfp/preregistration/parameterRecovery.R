# parameterRecovery.R
# MPT model for Experiment 1: parameter recovery
#
# Input: -
# Output: - 
# Packages: mpt
#
# Last mod: 06/16/2025, AO

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


# Stub MPT object to simulate
# n = nsubj * 24 CSs / 2 conditions (apply/ignore)
stubMPT <- function(n=40, pars=c(p_c=.4, p_u=.15, a=.15, g=.5)) {
    structure(list(
        treeid = spec$treeid,
        n = setNames(rep(n*24/2, 16), spec$treeid),
        pcat = spec$par2prob(pars)
    ), class = "mpt")
}

# Parameter recovery
recov <- function(n, pars) {
    estim <- t(replicate(500, {
        m <- mpt(spec, simulate(stubMPT(n, pars)))
        c(coef(m), m$goodness.of.fit["pval"])
    }))
    boxplot(estim, ylim=0:1, ylab="Parameter value")
    abline(h=0, lty=2)
    points(pars ~ I(1:4), pch=16, col="darkred")
    legend("topleft", "True value", pch=16, col="darkred", bty="n")
    invisible(list(meanPars=colMeans(estim), parVals=as.data.frame(estim)))
}


# Recovery for 40 participants (20 per group)
par(mar=c(3, 3, 1, 1), mgp=c(2, .7, 0))
rec40 <- recov(40, c(p_c=.4, p_u=.15, a=.15, g=.5))
mean(rec40$parVal$pval < .05)  # Beauty!

