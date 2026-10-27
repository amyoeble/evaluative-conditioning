# 03_mpt.R
# Analysis code for the extended MPT model in EC
#
# Input: 01_preprocessing.R
# Output: plots/MPTparameters.pdf
# Packages: mpt
#
# Last mod: 10/09/2025, AO

library(mpt)

## PREPROCESSING
source("01_preprocessing.R")

## RESPONSE FREQUENCIES
y <- c(xtabs( ~ attitude + USvalence + relation + context, dat))
cs_corr <- factor(dat$changeSign[dat$changeSign != "noChange"])
y_change <- c(xtabs( ~ cs_corr + USvalence + relation + context, 
              dat[dat$changeSign != "noChange", ]))

## MPT MODELLING
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

(m1 <- mpt(spec, data=y))
m2 <- mpt(update(spec, .restr=list(a=0)), data=y)
anova(m2, m1)

# Differences between predictions and data
predict(m1) - y     # everywhere, basically


## BASIC MODEL CHECK (fit simpler MPT model for apply condition only)
spec_simple <- mptspec(
    # Condition|CS valence.Response
    # s=standard, r=reversed, p=positive, n=negative
    sp.p = c + (1-c)*u + (1-c)*(1-u)*r,
    sp.n = (1-c)*(1-u)*(1-r),
    sn.p = (1-c)*(1-u)*r,
    sn.n = c + (1-c)*u + (1-c)*(1-u)*(1-r),
    rp.p = (1-c)*u + (1-c)*(1-u)*r,
    rp.n = c + (1-c)*(1-u)*(1-r),
    rn.p = c + (1-c)*(1-u)*r,
    rn.n = (1-c)*u + (1-c)*(1-u)*(1-r)
)

(m_simple <- mpt(spec_simple, data=y[1:8]))
(m_simple_c <- mpt(spec_simple, data=y_change[1:8]))
(m_simple_ignore <- mpt(spec_simple, data=y[9:16]))
(m_simple_c_ignore <- mpt(spec_simple, data=y_change[9:16]))


## JOINT SIMPLE MODEL
# p: propositional process; a: associative process; v: based on US valence;
# p_u: uncontrolled-propositional; g: guessing "positive"
spec_joinSimple0 <- mptspec(
    # Condition|Reversal|CS valence.Response
    # a=apply, i=ignore relation; s=standard, r=reversed; p=positive, n=negative
    asp.p = p_p + (1-p_p)*a + (1-p_p)*(1-a)*g_a,
    asp.n = (1-p_p)*(1-a)*(1-g_a),
    asn.p = (1-p_n)*(1-a)*g_a,
    asn.n = p_n + (1-p_n)*a + (1-p_n)*(1-a)*(1-g_a),
    arp.p = (1-p_p)*a + (1-p_p)*(1-a)*g_a,
    arp.n = p_p + (1-p_p)*(1-a)*(1-g_a),
    arn.p = p_n + (1-p_n)*(1-a)*g_a,
    arn.n = (1-p_n)*a + (1-p_n)*(1-a)*(1-g_a),

    isp.p = v + (1-v)*p_u_p + (1-v)*(1-p_u_p)*g_i,
    isp.n = (1-v)*(1-p_u_p)*(1-g_i),
    isn.p = (1-v)*(1-p_u_n)*g_i,
    isn.n = v + (1-v)*p_u_n + (1-v)*(1-p_u_n)*(1-g_i),
    irp.p = v + (1-v)*(1-p_u_p)*g_i,
    irp.n = (1-v)*p_u_p + (1-v)*(1-p_u_p)*(1-g_i),
    irn.p = (1-v)*p_u_n + (1-v)*(1-p_u_n)*g_i,
    irn.n = v + (1-v)*(1-p_u_n)*(1-g_i)
)
spec_joinSimple <- update(spec_joinSimple0, .restr=list(g_a=g, g_i=g))
(m_joinSimple <- mpt(spec_joinSimple, data=y))
round(predict(m_joinSimple) - y)
pars <- cbind(coef(m_joinSimple), plogis(confint(m_joinSimple))) |> round(3)
pars <- pars[c("p_p", "p_n", "v", "p_u_p", "p_u_n", "a", "g"), ]
pars <- as.data.frame(pars)
colnames(pars) <- c("estimate", "lower", "upper")

(m1_joinSimple <- mpt(update(spec_joinSimple, 
                      .restr=list(p_u_p=p_u, p_u_n=p_u)), data=y))
anova(m1_joinSimple, m_joinSimple)  # p_u_p != p_u_n

(m2_joinSimple <- mpt(update(spec_joinSimple, 
                      .restr=list(p_p=p, p_n=p)), data=y))
anova(m2_joinSimple, m_joinSimple)  # p_p != p_n

# Test a=0, p_u=0
m_pu1 <- mpt(update(spec_joinSimple, .restr=list(p_u_p=0)), data=y)
anova(m_pu1, m_joinSimple)
m_pu2 <- mpt(update(spec_joinSimple, .restr=list(p_u_n=0)), data=y)
anova(m_pu2, m_joinSimple)
m_pu3 <- mpt(update(spec_joinSimple, .restr=list(p_u_p=0, p_u_n=0)), data=y)
# p_u != 0
m_a <- mpt(update(spec_joinSimple, .restr=list(a=0)), data=y)
anova(m_a, m_joinSimple)
# a != 0
# Test g_a = g_i
anova(m_joinSimple, mpt(spec_joinSimple0, data=y))


# Estimated percentage of associative / uncontrolled propositional responses
(1-coef(m_joinSimple)["p_p"])*coef(m_joinSimple)["a"]   # 2.3 %
(1-coef(m_joinSimple)["p_n"])*coef(m_joinSimple)["a"]   # 4.4 %

(1-coef(m_joinSimple)["v"])*coef(m_joinSimple)["p_u_p"]     # 34.0 %
(1-coef(m_joinSimple)["v"])*coef(m_joinSimple)["p_u_n"]     # 21.0 %

# Plot parameter estimates
pdf("plots/MPTparameters.pdf", pointsize=10, height=3, width=6)
par(mar=c(2, 3, 1, 1), mgp=c(2, .7, 0))
plot(NULL, xlim=c(0.5, 7.5), ylim=0:1, ylab="Estimate", type="n", axes=FALSE,
    xlab="")
arrows(x0=1:7, y0=pars$lower, y1=pars$upper, col="dimgray", code=3, angle=90,
    length=0.1)
points(1:7, pars$estimate, pch=16, col="darkblue")
axis(1, at=1:7, labels=c(expression(p^"+"), expression(p^"-"), expression(v), 
    expression(p[u]^~"+"), expression(p[u]^~"-"), expression(a), expression(g)))
axis(2, at=seq(0, 1, by=.2))
axis(2, at=seq(0, 1, by=.1), labels=FALSE, tcl=-.2); box()
abline(h=seq(0, 1, by=.1), lty=3, col="darkgray")
abline(h=0:1, lty=2)
text(1:7+.34, pars$estimate, labels=pars$estimate, cex=.8)
dev.off()

