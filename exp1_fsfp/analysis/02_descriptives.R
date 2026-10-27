# 02_descriptives.R
# Descriptives for the extended EC model task
#
# Input: 01_preprocessing.R
# Output: plots/EC-effect.pdf, plots/individualRatings.pdf, plots/rawRatings.pdf
# Packages: -
#
# Last mod: 10/09/2025, AO

## PREPROCESSING
source("01_preprocessing.R")
dat$subject <- factor(as.numeric(dat$subject))

## DEMOGRAPHIC INFORMATION
(nsubj <- length(unique(dat$subj)))
dem <- pre[pre$index == 1, c("subject", "age", "gender", "study")]
summary(dem)
dem$gender |> table() |> proportions() |> round(4)
dem$age |> mean(na.rm=TRUE) |> round(4)
dem$age |> sd(na.rm=TRUE) |> round(4)

## RATING DESCRIPTIVES
## EC effects
se <- function(x) sd(x) / sqrt(length(x))
eps <- .05
ec_individual <- aggregate(cbind(prerating, postrating) ~ USvalence + 
                           relation + context + subject, dat, mean)
ec0 <- aggregate(cbind(prerating, postrating) ~ USvalence + relation + context,
                 ec_individual, mean)
ec <- reshape(ec0, direction="long", varying=list(4:5), timevar="ratingTime",
              idvar=c("USvalence", "relation", "context"), v.names="rating",
              new.row.names=1:16)
ec$se <- unlist(aggregate(cbind(prerating, postrating) ~ USvalence + relation + 
                         context, ec_individual, se)[4:5])
# Plot
pdf("plots/EC-effect.pdf", height=5.5, width=7, pointsize=10)
par(mfrow=c(2, 2), oma=c(1, 4, 0, 1), mar=c(2, 0, 3, 0), mgp=c(2, .7, 0))  
out <- lapply(split(ec, ec$context), function(x) {
  lapply(split(x, x$relation), function(y) {
    plot(rating ~ ratingTime, y[y$USvalence == "positive", ], pch=16, lty=1, 
         type="n", xlim=c(.7, 2.3), ylim=c(-100, 100), axes=FALSE, xlab="")
    abline(h=seq(-100, 100, by=20), lty=3, col="darkgray")
    cond <- dat[dat$context == y$context[1] & dat$relation == y$relation[1], ] 
    with(cond[cond$USvalence == "positive", ],
      boxplot(cbind(prerating, postrating), names=c("", ""), boxwex=.25, 
        ann=FALSE, col=adjustcolor("maroon", alpha.f=0.3), add=TRUE, 
        width=c(.3, .3), at=1:2-eps, axes=FALSE, staplewex=.3, medlwd=2))
    with(cond[cond$USvalence == "negative", ],
      boxplot(cbind(prerating, postrating), names=c("", ""), boxwex=.25, 
        ann=FALSE, col=adjustcolor("slateblue3", alpha.f=0.3), add=TRUE, 
        width=c(.3, .3), at=1:2+eps, axes=FALSE, staplewex=.3, medlwd=2))
    with(y[y$USvalence == "positive", ],
      arrows(I(ratingTime-eps), y0=rating - se, y1=rating+se, angle=90, code=3, 
             length=.06, col="maroon")
    )
    with(y[y$USvalence == "negative", ],
      arrows(I(ratingTime+eps), y0=rating - se, y1=rating+se, angle=90, code=3, 
             length=.06, col="slateblue3")
    )
    points(rating ~ I(ratingTime-eps), y[y$USvalence == "positive", ], pch=16, 
           type="b", cex=1.2, col="maroon", lwd=2)
    points(rating ~ I(ratingTime+eps), y[y$USvalence == "negative", ], pch=16, 
           type="b", cex=1.2, col="slateblue3", lwd=2)
    axis(1, at=1:2, labels=c("Prerating", "Postrating"))
    axis(2, at=seq(-80, 80, by=40)); axis(2, at=seq(-100, 100, by=10), FALSE, 
         tcl=-.2) 
    title(main=ifelse(unique(y$relation) == "foerdert", "'promotes'",
          "'prevents'"), line=0.7)
    if(unique(y$relation) == "foerdert") {
        axis(2, at=0, "Mean Rating", tcl=0, line=2)
    }
    if(all(y$context == "apply" & y$relation == "foerdert")) {
        legend(x=.75, y=-65, c("US positive", "US negative"), pch=16,
            lty=1, box.col="white", , col=c("maroon", "slateblue3"), bg="white") 
    }
    box()
  })
  mtext(c("Apply", "Ignore"), side=3, line=c(-1.5, -21.8), outer=TRUE, font=2)
})
dev.off()


# Pre- and postrating distributions (individual trajectories)
# Raw data
pdf("plots/individualRatings.pdf", height=8, width=6, pointsize=10)
par(mfrow=c(4, 2), mar=c(0, 0, 0, 0), oma=c(3, 3, 2, 1), mgp=c(2, .7, 0))
lapply(split(dat, dat$context), function(x0) {
  lapply(split(x0, x0$relation), function(x) {
    plot(as.numeric(subject) ~ prerating, x[x$USvalence == "positive", ], 
         col="blue", xlim=c(-100, 100), ylim=c(1, 116), xlab="Prerating", 
         main="", ylab="", axes=FALSE, type="n")
    abline(h=1:116, col="gray")
    points(as.numeric(subject) ~ prerating, x[x$USvalence == "positive", ], 
         col="blue")
    points(as.numeric(subject) ~ prerating, x[x$USvalence == "negative", ], 
         col="orange")
    box(); axis(1); axis(2, at=c(1, 116), labels=c(1, 116), tcl=-.2)
    mtext("Subject ID", side=2, line=1)
    plot(as.numeric(subject) ~ postrating, x[x$USvalence == "positive", ], 
         col="blue", xlim=c(-100, 100), ylim=c(1, 116), xlab="Postrating", 
         main="", ylab="", axes=FALSE, type="n")
    abline(h=1:116, col="gray")
    points(as.numeric(subject) ~ postrating, x[x$USvalence == "positive", ], 
         col="blue")
    points(as.numeric(subject) ~ postrating, x[x$USvalence == "negative", ], 
         col="orange")
    box(); axis(1)
  })
})
dev.off()

# Preratings
boxplot(prerating ~ subject, pre, drop=TRUE, xlab="Participant ID", 
    ylab="Prerating")
abline(h=0, lty=2, lwd=2)
# Average pre- and postrating
aggregate(cbind(prerating, postrating) ~ subject, dat, mean)
pdf("plots/rawRatings.pdf", pointsize=12, height=6, width=10)
par(mfrow=c(1, 6), mar=rep(0, 4), oma=c(3, 3, 1, 1), mgp=c(2, .7, 0))
for(i in sort(unique(dat$subject))) {
    plot(rep(1, 32), dat[dat$subject == i, ]$prerating, xlab="Rating", 
        xlim=c(0.5, 2.5), ylim=c(-100, 100), pch=21, col="lightblue", 
        bg="lightblue", axes=FALSE)
    title(main=i, line=-1.2)
    points(rep(2, 32), dat[dat$subject == i, ]$postrating, pch=21, 
        col="lightblue", bg="lightblue")
    axis(1, at=1:2, labels=c("Prerating", "Postrating")) 
    axis(2, at=seq(-100, 100, by=20)); box()
}
dev.off()

# Categoriacal postrating vs. change score
par(mgp=c(2, .7, 0), mar=c(3, 3, 1, 1))
boxplot(changeScore ~ attitude, dat, horizontal=TRUE, xlab="Change score",
        ylab="Postrating", axes=FALSE, xlim=c(.5, 2.5))
axis(1); axis(2, at=1:2, labels=c("pleasant", "unpleasant")); box()
abline(v=0, lty=4, lw=2)
# Aggreement between binary posterior attitude and sign of change score
mean(as.numeric(dat$attitude) == as.numeric(dat$changeSign))

