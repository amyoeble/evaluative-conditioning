# 04_memory.R
# Investigate memory variable
#
# Input: 01_preprocessing.R
# Output: -
# Packages: -
#
# Last mod: 02/03/2026, AO

## PREPROCESSING
source("01_preprocessing.R")

# Memory rates for each subject (with exclusions)
memRates <- aggregate(memoryCorrect ~ subject, dat, mean)
memRates$memoryCorrect |> summary()
# Memory rates per condition
mem_cond <- aggregate(memoryCorrect ~ USvalence + relation + context, dat, mean)
# Correct postrating according to experimental condition?
dat$correct <- ifelse(dat$context == "ignore" | 
                        (dat$context == "apply" & dat$relation == "foerdert"), 
                  ifelse(dat$USvalence == "positive", "angenehm", "unangenehm"),
                ifelse(dat$context == "apply" & dat$relation == "verhindert" &
                          dat$USvalence == "positive",
                    "unangenehm", "angenehm")
  ) == dat$attitude
aggregate(correct ~ USvalence + relation + context, dat, mean)
# Test whether memory predicts correct attitude formation
# m0 <- glm(correct ~ context*USvalence*relation, family=binomial(logit), dat)
# m1 <- glm(correct ~ memoryCorrect + context*USvalence*relation, 
#           family=binomial(logit), dat)
# anova(m0, m1)
# exp(coef(m1)["memoryCorrectTRUE"])  
# Chance of correct response with correct memory is 0.97-times chance of correct
#   response without correct memory.

library(lme4)
m0 <- glmer(correct ~ context*USvalence*relation + (memoryCorrect | subject) +
            (USvalence*relation | subject), dat, family=binomial())
m1 <- glmer(correct ~ memoryCorrect + context*USvalence*relation + 
            (memoryCorrect | subject) + (USvalence*relation | subject), dat,
            family=binomial())
anova(m0, m1)

