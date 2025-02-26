library(ggplot2)
library(gridExtra)
library(grid)

theme_microcol = function(base_size = 12, base_family = "") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),
      axis.text.x = element_text(
        size = base_size * 0.8,
        color = "black",
        lineheight = 0.9
      ),
      axis.text.y = element_text(
        size = base_size * 0.8,
        color = "black",
        lineheight = 0.9
      ),
      axis.ticks = element_line(color = "black", 
                                size  =  0.8),
      axis.title.x = element_text(
        size = base_size,
        face = "bold", 
        color = "black",
        margin = margin(0, 10, 0, 0)
      ),
      axis.title.y = element_text(
        size = base_size,
        face = "bold",
        color = "black",
        angle = 90,
        margin = margin(0, 10, 0, 0)
      ),
      axis.ticks.length = unit(0.5, "lines"),
      # Specify legend options
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(1, "lines"),
      legend.key.height = NULL,
      legend.key.width = NULL,
      legend.text = element_text(size = base_size * 0.8, 
                                 color = "black"),
      legend.title = element_text(
        size = base_size * 0.8,
        face = "bold",
        hjust = 0,
        color = "black"
      ),
      legend.position = "right",
      legend.text.align = NULL,
      legend.title.align = NULL,
      legend.direction = "vertical",
      legend.box = NULL,
      # Specify panel options
      panel.background = element_rect(fill = NA, 
                                      color  =  NA),
      panel.border = element_rect(fill = NA, 
                                  color = "black",
                                  size = 1.0),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "lines"),
      # Specify facetting options
      strip.background = element_rect(fill = "transparent", 
                                      color = "grey10",
                                      size = 1),
      strip.text.x = element_text(size = base_size * 0.8, 
                                  color = "black"),
      strip.text.y = element_text(
        size = base_size * 0.8,
        color = "black",
        angle = -90
      ),
      # Specify plot options
      plot.background = element_rect(color = "black", 
                                     fill = "white"),
      plot.title = element_text(size = base_size * 1.2, 
                                color = "black"),
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}


