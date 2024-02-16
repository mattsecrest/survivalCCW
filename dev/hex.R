# Color palette reference: https://coolors.co/04e762-f5b700-00a1e4-dc0073-89fc00

# Make a ggplot2 image of 3 distributions
make_survplot <- function() {

   # Dependancies
   require(ggplot2)
   require(showtext)
   require(dplyr)
   require(ggthemes)
   require(RColorBrewer)
   require(ggsurvfit)

   # KM curve
   set.seed(123)
   n <- 100
   time <- rexp(n, rate = 1/2)
   status <- sample(0:1, n, replace = TRUE)
   group <- sample(1:2, n, replace = TRUE)
   time  <-  ifelse(group == 1, time*.5, time*1.2 + 0.5)
   df <- data.frame(time, status)
   fit <- survfit2(Surv(time, status) ~ group, data = df)
   p <- ggsurvfit(fit) +
      add_confidence_interval() +
      theme_few() +
      labs(title = "", x = "", y = "") +
      theme(
         axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         panel.margin = unit(c(0, 0, 0, 0), "cm"),
         panel.background = element_blank(),
         plot.margin = unit(c(0, 0, 0, 0), "cm"),   
         legend.position = "none",
         plot.background = element_rect(fill = "transparent",colour = NA),
      )+
      scale_color_manual(values = c("#6457A6", "#9DACFF")) +
      scale_fill_manual(values = c("#6457A6", "#9DACFF")) +
      geom_vline(xintercept = 0.5, linetype = "dashed", color = "#5C2751", size = 0.5)+
      geom_vline(xintercept = 1, linetype = "dotted", color = "#5C2751", size = 0.5) +
      xlim(0, 2.5)
   p

}

make_hexplot <- function(out_path = "./man/figures/hex.png") {
  # Dependancies
  require(hexSticker)
  require(showtext)

  # HexSticker
  hexSticker::sticker(
    subplot = make_survplot(),
    package = "survivalCCW",
    p_size = 180,
    p_color = "#5C2751",
    p_y = 1.35,
    s_y = .9,
    s_x = 1.1,
    s_width = 2,
    s_height = .75,
    h_fill = "#76E5FC",
    h_color = "#9DACFF",
    h_size = 2,
    url = "github.com/mattsecrest/survivalCCW",
    u_x = 1, 
    u_y = .09,
    u_size = 42,
    u_color = "#6457A6",
    filename = out_path,
    p_family = "mono",
    #u_angle = 10,
    dpi = 3000
  )
}

make_hexplot()