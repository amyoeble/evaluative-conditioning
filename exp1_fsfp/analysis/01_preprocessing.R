# 01_preprocessing.R
# Data preprocessing for the extended MPT model in EC
#
# Input: ../data/datafiles/exp1_*_EC.txt
# Output: -
# Packages: -
#
# Last mod: 10/09/2025, AO

## READ DEMOGRAPHIC INFORMATION AND PRERATINGS
pre <- do.call(rbind,
    lapply(dir("../data/datafiles/", "exp1_\\d{1, }_pre.txt", 
    full.names=TRUE), read.table, header=TRUE, as.is=FALSE, sep="\t"))
pre$subject <- factor(pre$subject)

## DATA PREPARATION ##
dat <- do.call(rbind, 
    lapply(dir("../data/datafiles/", "exp1_\\d{1, }_EC.txt", 
    full.names=TRUE), read.table, header=TRUE, as.is=FALSE, sep="\t"))
dat$subject <- factor(dat$subject)
dat$USvalence <- factor(dat$USvalence, levels=c("positive", "negative"))
dat$context <- factor(dat$context, levels=c("apply", "ignore"))
dat$memoryCorrect <- dat$relation == dat$memory
# Calculate change scores and change score signs
dat$changeScore <- dat$postrating - dat$prerating
any(dat$postrating == dat$prerating)
dat$changeSign <- ifelse(dat$postrating - dat$prerating < 0, "negative", 
        ifelse(dat$postrating - dat$prerating == 0, "noChange", "positive"))
dat$changeSign <- factor(dat$changeSign, 
                         levels=c("positive", "negative", "noChange"))

## Exclude participants
# Exclude subject that participated twice (ID: 107)
excludeSubj <- list(sys.error=107)
# Check for incomplete data sets
if(all(aggregate(memory ~ subject, dat, length)$memory == 32)) {
    cat("All data sets complete.\n")
} else cat("Caution! Incomplete data sets\n.")
# Language (German skill level < C1)
excludeSubj$language <- unique(pre[pre$german != "yes", ]$subject)
# Extreme preratings (Tukey, 1977)
avgPre <- aggregate(prerating ~ subject, dat, mean)
excludeSubj$preratingOutlier <- avgPre$subject[avgPre$prerating %in% 
                    boxplot(avgPre$prerating, range=1.5, plot=FALSE)$out]
# Practice fails
excludeSubj$practiceFail <- unique(dat[dat$practiceFail != 0, ]$subject)
# Memory rates for relations below cutoff (0.5) - NOTE: Dropped on 11/20/2025
memRates <- aggregate(memoryCorrect ~ subject, dat, mean)
# excludeSubj$memory <- memRates[memRates$memoryCorrect < .5, ]$subject

# Number of exclusions per criterion (caution, might have duplicates!)
cat(paste("\nSubjects excluded:", length(unique(unlist(excludeSubj))), "/",
      length(unique(dat$subject)), "\n\n")) 
print(lapply(excludeSubj, length))

# Exclude
dat <- dat[!dat$subject %in% unlist(excludeSubj), ]
pre <- pre[!pre$subject %in% unlist(excludeSubj), ]
if(all(unique(pre$subj) == unique(dat$subj))) {
    cat(paste("Sample seems good.\nn =", length(unique(dat$subject)), "\n"))
} else cat("Better double check exclusions...!\n")

