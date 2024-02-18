theme_custom <- function() {
  theme_classic() %+replace%
    theme(
      axis.text = element_text(colour = "black", size = 32),
      axis.title = element_text(colour = "black", size = 36),
      panel.border = element_blank(),
      strip.text = element_text(
        size = 36, colour = "black",
        margin = margin(1, 0, 1, 0, "cm")
      ),
      strip.background = element_rect(
        colour = "white",
        fill = "white"
      ),
      legend.key.width = unit(1, "cm"),
      legend.key.height = unit(1.5, "cm"),
      legend.title = element_text(size = 28, hjust = 0.5),
      legend.text = element_text(size = 26),
      legend.position = "none",
      legend.background = element_blank(),
      legend.box.background = element_rect(colour = "black")
    )
}