## Allison Galezo
## Test for sex segregation in bottlenose dolphins
## Created: 23 Feb 2016

## Load dependencies
library(vegan)
library(tidyverse)

## Load dataset
data <- read.csv("DRYAD SSAS Data.csv")
colnames(data) <- c("adult_males","adult_females","month")

##########################################
## Test for significant sexual segregation
##########################################

## Function for SSAS (Sexual Segregation and Aggregation Statistic):
## See Bonenfant et al. 2007 ("Testing sexual segregation and aggregation: old ways are best") for derivation
SSAS <- function(ct)
  # where 'ct' is a contingency table: 1st column is # females, 2nd column is # males
{
  ss.X <- as.numeric(sum(ct[,2]))
  ss.Y <- as.numeric(sum(ct[,1]))
  ss.N <- as.numeric(ss.X + ss.Y)
  ss.xi <- as.vector(ct[,2])
  ss.yi <- as.vector(ct[,1])
  ss.ni <- ss.xi + ss.yi
  (1-(ss.N/(ss.X*ss.Y))*sum((ss.yi*ss.xi)/ss.ni))
}

## Generate contingency table by removing "month" column from dataset
ct <- data[c("adult_females","adult_males")]

## Calculate observed SSAS (pooled across all months)
obs <- SSAS(ct)

## Generate permuted distribution of SSAS values by permuting contingency table
## Set number of permutations
nperm <- 999
## Generate permuted datasets
perms <- permatfull(ct,
                    fixedmar = "both",  # Preserve row totals (group size) and column totals (sex ratio)
                    mtype = "count",
                    times = nperm)
## Calculate SSAS for all permuted datasets
permutedSSAS <- sapply(perms[[3]], SSAS)

## Test for significant sex segregation
p <- (sum(permutedSSAS > obs) + 1) / (nperm + 1)
print(p)

##########################################
## Calculate sexual segregation and aggregation statistic by month
##########################################

## Default list of months in R
months <- month.abb

## Calculate observed SSAS for each month
obs_month <- sapply(months, function(mon){
  data_by_month <- data[data$month == mon, ]
  SSAS(data_by_month)
  })

## Generate permuted datasets for each month
perm_month <- lapply(months, function(mon){
  data_by_month <- data[data$month == mon, c("adult_females","adult_males")]
  permatfull(data_by_month, fixedmar = "both", mtype = "count", times = nperm)
  })
names(perm_month) <- months

## Calculate SSAS for each permuted dataset for each month
SSAS_month <- sapply(months, function(mon){
  sapply(perm_month[[mon]][[3]], SSAS)
})
SSAS_month <- as.data.frame(SSAS_month)
colnames(SSAS_month) <- months

## Transform from wide-form to long-form
SSAS_month <- tidyr::gather(SSAS_month, month, SSAS)

## Combine permuted and observed SSAS values by month into one dataset
SSAS_month$type <- "Expected"
SSAS_month <- rbind(SSAS_month,
                    data.frame(month = names(obs_month),
                               SSAS = obs_month,
                               type = "Observed"))

## Plot 95% confidence intervals vs. observed value by month
SSAS_month %>%
  group_by(month, type) %>%
  summarize(upper = quantile(SSAS, 0.95),
            lower = quantile(SSAS, 0.05),
            mid = quantile(SSAS, 0.5)) %>%
  ungroup() %>%
  mutate(month = factor(month, levels = substr(month.name, 1, 3))) %>%
  arrange(month) %>%
  mutate(month_numeric = rep(1:12, each = 2)) %>%
  ggplot(aes(x = month_numeric, y = mid, ymax = upper, ymin = lower, group = type)) +
  geom_line(aes(size = type, color = type)) +
  scale_size_manual(values = c(2.5, 0.8)) +
  scale_color_manual(values = c("gray","black")) +
  geom_ribbon(fill = "gray") +
  scale_x_continuous(breaks = 1:12, labels = substr(month.name, 1, 3)) +
  scale_y_continuous(limits = c(0, 0.8), expand = c(0,0), breaks = seq(0, 0.8, 0.1)) +
  theme_classic() +
  xlab("Month") +
  ylab("Sexual Segregation and Aggregation Statistic") +
  theme(legend.position = c(0.88, 0.13),
        legend.title = element_blank())

## Save plot
ggsave("SSAS.pdf", plot = last_plot(), device = "pdf", width = 4, height = 4, units = c("in"))

