## Allison Galezo
## Permuted t-test for activity budget analysis
## Created: 2 Feb 2017

library(dplyr)
library(perm)

## Load data
data <- read.csv("DRYAD Activity Budget Lone Adults Data.csv")

## Summary stats
data %>%
  group_by(Sex) %>%
  summarize(mean_forage = mean(Forage),
            mean_rest = mean(Rest))

## Test for normality
shapiro.test(data$Forage)
shapiro.test(data$Rest)
## Neither time spent foraging or time spent resting are normally distributed, so we'll test for sex differences with a permutation test:

## Permutation t-tests:
permTS(data$Forage ~ data$Sex, alternative = "greater", method = "exact.mc") # Lone females forage significantly more than lone males.
permTS(data$Rest ~ data$Sex, alternative = "less", method = "exact.mc") # Lone males rest significantly more than lone females.
