# 1_data-preprocessing.R
# Create clean data file from SoSciSurvey output that can actually be analyzed;
#   exclude participants who should be excluded according to preregistration
#
# Input: ../data/data_exp2_stm_ec_raw.csv
# Output: ../data/data_exp2_stm_ec_preprocessed.txt, 
#         ../data/clubRankings.txt
# Packages: -
#
# Last mod: 04/25/2026, AO

dat0 <- read.table("../data/data_exp2_stm_ec_raw.csv", sep=";", header=TRUE)
# Exclude screenouts
dat0 <- dat0[dat0$STATUS == "complete", ]
# Select relevant columns only
dat1 <- cbind(subject = factor(dat0$CASE), prolificID = dat0$PL01_01, 
          isAttentive = dat0$AC01_01,
          dat0[ , c("consent", "isFan", "gender", "age", "language", "FINISHED",
                    "MODE", "favClub", "STATUS",
                    grep("clubRank", colnames(dat0), value=TRUE),
                    grep("CS", colnames(dat0), value=TRUE))])
dat1$gender <- factor(ifelse(dat1$gender == 1, "male", 
                      ifelse(dat1$gender == 2, "female", "diverse")))

# Label favorite club
clubs <- c("hdh", "koe", "fcu", "fsv", "b04", "fcb", "bmg", "bvb", "sge", 
           "fca", "stp", "hsv", "rbl", "scf", "tsg", "vfb", "vfl", "svw")
dat1$favClub <- clubs[dat1$favClub]

## Exclude participants
excludeSubj <- list(notFinished=dat1[dat1$FINISHED == 0, ]$subject)
excludeSubj$inattentive <- dat1[dat1$isAttentive == 0, ]$subject
excludeSubj$language <- dat1[dat1$language == 0, ]$subject
excludeSubj$tooYoung <- dat1[dat1$age < 18, ]$subject
excludeSubj$noFan <- dat1[dat1$isFan == 0, ]$subject
excludeSubj$RT <- dat1$subject[apply(dat1[, grep("CS0\\d_04", colnames(dat1), 
                                            value=TRUE)], 1, \(x) any(x < 150))]
dat2 <- dat1[!dat1$subject %in% unlist(excludeSubj), ]
dat2$subject <- droplevels(dat2$subject)
demographics <- dat2[ , c("subject", "gender", "age", "favClub")]

# Data for main analyses (all pervious selections but club rankings)
dat3 <- dat2[ , -grep("clubRank", colnames(dat2))]
dat <- reshape(dat3, direction="long", idvar="subject", 
    varying=list(grep("CS0\\d_01", colnames(dat3)),
                 grep("CS0\\d_02", colnames(dat3)),
                 grep("CS0\\d_03", colnames(dat3)),
                 grep("CS0\\d_04", colnames(dat3)),
                 grep("CS0\\d_05", colnames(dat3)),
                 grep("CS0\\d_06", colnames(dat3)),
                 grep("CS0\\d_07", colnames(dat3)),
                 grep("CS0\\d_08", colnames(dat3))
    ), v.names=c("face", "cond", "attitude", "attitudeRT", "contPR", 
                 "competence", "opp1", "opp2"), timevar="face")
attributes(dat)$reshapeLong <- NULL
dat <- dat[order(dat$subject), ]; rownames(dat) <- NULL
# Label factors
dat$framing <- factor(ifelse(dat$cond %in% 0:1, "win", "loss"),
                      levels=c("win", "loss"))
dat$perspective <- factor(ifelse(dat$cond %in% c(0, 2), "own", "opponent"),
                          levels=c("own", "opponent"))
dat$cond <- paste0(substr(dat$perspective, 1, 3), "_", dat$framing)
dat$attitude <- factor(dat$attitude, levels=2:1, 
                       labels=c("positive", "negative"))
# Recode continuous ratings (-100 to 100 instead of 1 to 201)
dat$contPR <- dat$contPR - 101
dat$competence <- dat$competence - 101

# Club rankings
cR0 <- dat2[ , c("subject", "favClub", 
                grep("clubRank", colnames(dat2), value=TRUE))]
# Caution: NAs are coded as "-9"!
cR0 <- cR0[apply(cR0[, 3:20] != -9, 1, all), ]

cR <- apply(cR0[, 3:20], 1, function(x) {
  sapply(1:18, function(i) substr(names(x)[which(x == i)], 10, 13))
})
dimnames(cR) <- list(paste0("rank", 1:18), cR0$subject)

## Export data files
write.table(dat, file="../data/data_exp2_stm_ec_preprocessed.txt", quote=FALSE, 
            sep="\t", row.names=FALSE)
write.table(cR, file="../data/clubRankings.txt", quote=FALSE, sep="\t", 
            row.names=FALSE)
write.table(demographics, file="../data/demographics.txt", quote=FALSE, 
            sep="\t", row.names=FALSE)

# Clean workspace (for script execution via source())
rm(clubs, cR0, dat1, dat2, dat3)
