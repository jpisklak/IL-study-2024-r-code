library(tidyverse)
library(RColorBrewer)
source("theme-custom.R")
source("SiGN-function.R")
options(
  pillar.print_max = 500,
  pillar.width = 50
)

data <- read_csv('../data/predictions.csv')

reg <- lm(cp ~ SiGN, data = data)

sum_info <- summary(reg)
sum_info


# Test b0 = 0
# ------------------------------------------------------------------------------
b0 <- sum_info$coefficients[1, 1]
b0_err <- sum_info$coefficients[1, 2]

t_stat_b0 <- (b0 - 0) / b0_err
df_b0 <- sum_info$fstatistic[3]

p_b0 <- pt(abs(t_stat_b0), df = 3, lower.tail = FALSE) * 2
t_stat_b0
df_b0
p_b0

# Test b1 = 1
# ------------------------------------------------------------------------------
b1 <- sum_info$coefficients[2, 1]
b1_err <- sum_info$coefficients[2, 2]

t_stat_b1 <- (b1 - 1) / b1_err
df_b1 <- sum_info$fstatistic[3]

p_b1 <- pt(abs(t_stat_b1), df = 3, lower.tail = FALSE) * 2
t_stat_b1
df_b1
p_b1

# Correlation
# ------------------------------------------------------------------------------
cor.test(data$SiGN, data$cp)










