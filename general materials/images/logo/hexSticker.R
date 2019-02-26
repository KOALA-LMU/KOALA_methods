
library(hexSticker)
library(ggplot2)
library(png)
library(grid)
library(gridExtra)

gg <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()
img <- readPNG("hexSticker_icon.png", info = FALSE)
grob_img <- grobTree(rasterGrob(img, hjust = 0.5))
gg <- grid.arrange(grob_img)

gg_s <- sticker(gg,
                package = "coalitions", 
                filename = "hexSticker_coalitions.png",
                p_y = 0.65,
                s_x = 1, s_y = 1.25, s_width = 0.7, s_height = 0.7,
                h_size = 2, 
                # h_fill = "#3C8DBC", h_color = "#FF0000")
                # h_fill = "#3C8DBC", h_color = "#234D65") #"#4C5659")
                h_fill = "#78909C", h_color = "#455A64")