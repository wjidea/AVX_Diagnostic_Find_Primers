require(ggplot2)
require(tidyr)
require(ggExtra)
library(gridExtra)
library(ggpmisc)


theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
  
}

lm_eqn <- function(df){
  m <- lm(ct ~ log_qty, df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2*"\n PCR efficiency: 100%", 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))                 
}


file_path <- "/Users/wangj/Documents/OneDrive/Writing/AVX_Diagnostics/data_realtimePCR/data_realtimePCR.txt"
in_file <- read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

taq17 <- in_file[in_file$experiment == "taqman_17",]
taq19 <- in_file[in_file$experiment == "taqman_19",]
zen17 <- in_file[in_file$experiment == "zen_17",]
zen19 <- in_file[in_file$experiment == "zen_19",]


draw_plot <- function(data){
  
  p <- ggplot(data, aes(x=log_qty, y=ct)) +
  geom_point(shape=16, size=2) +
  geom_smooth(method=lm,  se=FALSE, color = "black", size = 0.6, formula = y ~ x) +   
  theme_Publication() + xlab("Log DNA amount (fg)") + ylab("Cycle of threshold") +
  geom_text(x = 7, y = 33, label = bquote(atop(lm_eqn(data), "test")), parse = TRUE)
  return(p)
}

draw_plot(taq17) 

p2 <- draw_plot(taq17) 
p4 <- draw_plot(taq19)
p1 <- draw_plot(zen17)
p3 <- draw_plot(zen19)

grid.arrange(p1,p2,p3,p4,nrow=2)
