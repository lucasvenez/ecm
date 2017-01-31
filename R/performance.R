setwd("/home/lucas/Dropbox/Papers/Writing/FGCS/analysis/")

require("ggplot2")
require("scales")

time <- read.table("data/models.performance.csv", sep = ",", header = T)

time$method <- toupper(time$method)

time$arch <- factor(time$arch, levels = c("AMD", "Intel", "Both"))
time$method <- factor(time$method, levels = c("GLR", "MLP", "MRT"))

fancy_scientific <- function(l) {
   # turn in to character string in scientific notation
   l <- format(l, scientific = TRUE)
   # quote the part before the exponent to keep all the digits
   l <- gsub("^(.*)e", "'\\1'e", l)
#   # turn the 'e+' into plotmath format
   l <- gsub("e", "%*%10^", l)
#   # return this as an expression
   parse(text=l)
}

graph <- ggplot(time, aes(x = 1, y = time, fill = type)) + 
      geom_bar(stat = "identity", alpha = .6) +
      facet_grid(method ~ arch, scale = "free") +
      scale_y_sqrt(labels = fancy_scientific) +
      xlab("Architecture") + ylab(expression(sqrt("Time"))) +
      scale_fill_hue("Type of time") +
      theme(
         text            = element_text(size = 10),
         legend.position = "top",#c(-.18,-.05),
         axis.title.x = element_blank(),
         axis.text.x = element_blank(),
         axis.ticks.x = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank()
      )

ggsave("../images/PERFORMANCE.pdf", graph, units = "cm", width = 8.6, height = 9)
