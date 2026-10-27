# 2_descriptive-analysis.R
# Descriptives (demographics, EC effects, ...) for shooting the messenger in 
#   EC data
#
# Input: 1_data-preprocessing.R
# Output: -
# Packages: - 
#
# Last mod: 04/26/2026, AO

source("1_data-preprocessing.R")

## DEMOGRAPHICS
table(demographics$gender) |> proportions() |> round(2)
mean(demographics$age) |> round(2)
sd(demographics$age) |> round(2)

## EC effects
dat$attitudeNumeric <- ifelse(dat$attitude == "positive", 100, -100)
# Continuous Postratings / Ascribed Competence
rating_means_ind <- aggregate(cbind(contPR, competence, attitudeNumeric) ~ 
                              subject + framing + perspective, dat, mean)
agg_ratings <- list(
    contPR = aggregate(contPR ~ framing + perspective, rating_means_ind, mean),
    binaryPR = aggregate(attitudeNumeric ~ framing + perspective, 
                         rating_means_ind, mean),
    competence = aggregate(competence ~ framing + perspective, 
                           rating_means_ind, mean)
)
agg_ratings[["contPR"]]$se <- aggregate(contPR ~ framing + perspective, 
               rating_means_ind, \(x) sd(x)/sqrt(length(x)))$contPR
agg_ratings[["competence"]]$se <- aggregate(competence ~ framing + perspective, 
                   rating_means_ind, \(x) sd(x)/sqrt(length(x)))$competence
agg_ratings[["binaryPR"]]$se <- aggregate(attitudeNumeric ~ framing + 
    perspective, rating_means_ind, \(x) sd(x)/sqrt(length(x)))$attitudeNumeric
colnames(agg_ratings$contPR)[3] <- "Postrating (Continuous)"
colnames(agg_ratings$competence)[3] <- "Ascribed Competence"
colnames(agg_ratings$binaryPR)[3] <- "Binary Postrating (% 'likeable')"

# Agreement of continuous (sign) and categorical postratings
dat$evalComp <- factor(ifelse(dat$contPR >= 0, 
        ifelse(dat$attitude == "positive", "bothPos", "contPcatN"), 
        ifelse(dat$attitude == "positive", "contNcatP", "bothNeg")),
    levels=c("bothPos", "bothNeg", "contPcatN", "contNcatP"))
# Overall agreement
mean(dat$evalComp %in% c("bothPos", "bothNeg"))


# Quantification/classification of individual EC effects
ec_eff <- function(ownw, ownl, oppw, oppl) {
    c(stan=(ownw - ownl), rev=(oppw - oppl))
}
rating_means_ind$condition <- factor(paste0(rating_means_ind$perspective,
                                            rating_means_ind$framing),
        levels=c("ownwin", "ownloss", "opponentwin", "opponentloss"))
effdat <- reshape(rating_means_ind, direction="wide", idvar="subject", 
    drop=c("framing", "perspective", "attitudeNumeric", "competence"),
    timevar="condition")
ind_ec <- as.data.frame(t(apply(effdat[, 2:5], 1, 
                                \(x) ec_eff(x[1], x[2], x[3], x[4]))))
colnames(ind_ec) <- c("standard", "reversal")
ind_ec$type <- factor(ifelse(ind_ec$standard > 0, 
                    ifelse(ind_ec$reversal > 0, "fram", "rev"),
                    ifelse(ind_ec$reversal > 0, "revrev", "revfram")))
round(proportions(table(ind_ec$type))*100)
