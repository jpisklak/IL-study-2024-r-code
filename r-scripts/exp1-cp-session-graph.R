library(tidyverse)
library(RColorBrewer)
source('theme-custom.R')
options(
  pillar.print_max = 500,
  pillar.width = 50
)

# Import Data
full_data <- read_csv("../data/exp-data.csv")
exp1 <- full_data %>%
  filter(exp == 1) %>%
  select(exp, subject, condition, session, pks_sub, pks_opt)


#Factor subject column
exp1$subject <- factor(exp1$subject)

# Create order factor
exp1$order <- ifelse(((exp1$session <= 16 & exp1$condition == "FR 1") |
  (exp1$session > 16 & exp1$condition == "VI 30")),
  "FR 1 / VI 30", "VI 30 / FR 1"
)

# Create phase factor
exp1$phase <- ifelse(exp1$session <= 16, 'Phase 1', 'Phase 2')

#Get choice proportions
exp1$cp <- exp1$pks_sub / (exp1$pks_sub + exp1$pks_opt)

# Session CPs
plt_means <- exp1 %>%
  group_by(order, phase, session) %>%
  summarise(
    mean_cp = mean(cp)
  )

# Plot
ggplot(plt_means, aes(x = session, y = mean_cp)) +
  geom_hline(
    yintercept = 0.5,
    linetype = 3,
    linewidth = 0.5
  ) +
  
  geom_line(
    data = exp1,
    aes(
      x = session,
      y = cp,
      group = subject,
      colour = order
    ),
    alpha = 0.30
  ) +
  
  geom_line(aes(colour = order), linewidth = 1.5) +
  geom_point(
    aes(
      colour = order,
      shape = order
    ),
    size = 4, stroke = 2,
    fill = "white"
  )+
  
  scale_colour_manual(values = brewer.pal(n = 11, name = "RdYlBu")[c(2, 10)]) +
  scale_shape_manual(values = c(21, 24)) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  xlab("Session") +
  ylab("Suboptimal Choice Proportion") +
  labs(
    colour = "Order:",
    linewidth = "Order:",
    shape = "Order:"
  ) +
  
  facet_grid(~phase, scales = "free_x", space = "free") +
  
  theme_custom() +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(colour = "white")
  )

# Save
ggsave('../plots/exp1-session-means.png', 
       units = 'cm', width = 40, height = 22, 
       dpi = 500)

ggsave('../plots/exp1-session-means.svg', 
       units = 'cm', width = 40, height = 22)

ggsave('../plots/exp1-session-means.pdf', 
       units = 'cm', width = 40, height = 22)
