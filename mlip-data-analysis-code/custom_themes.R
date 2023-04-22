# library
library(tidyverse)

theme_1 <- 
  theme_minimal() +
  theme(plot.margin = margin(rep(20, 4)),
        panel.border = element_rect(fill = NA, color = "gray80"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14, margin = margin(t = 20)),
        axis.title.y = element_text(size = 13, margin = margin(r = 20)),
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing.x = unit(10, "mm")
  )


theme_2 <- 
  theme_minimal() +
  theme(plot.margin = margin(rep(5, 4)),
        panel.border = element_rect(fill = NA, color = "gray80"),
        panel.grid = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 10, face = "bold"),
        strip.text = element_text(size = 10, face = "bold"),
        panel.spacing.x = unit(10, "mm")
  )
