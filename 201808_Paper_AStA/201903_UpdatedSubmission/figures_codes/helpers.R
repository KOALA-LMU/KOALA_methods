

# Helper function for ridgeline plots over time ---------------------------
## define function to reverse date time scale
# source: https://gist.github.com/adibender/28041453a2a8c3e42c09484a55668d55
library(scales)
c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inv, breaks, format)
}



# Prepare KOALA logo for branding all plots -------------------------------
# Helper function to brand ggplot2 plots with the KOALA logo.
# Currently not in use...

library(png)
library(grid)

img <- readPNG("../figures_codes/Koala_Logo_Schrift.png", info = FALSE)
grob_img <- grobTree(rasterGrob(img, vjust = 0))

# Helper function to brand a ggplot-based plot
# NOTE: This worked fine as one didn't apply the rev_date y axis in the
#       ridgeline plot!
brand_plot <- function(gg, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf) {
  gg + annotation_custom(grob_img, xmax = xmax, ymax = ymax, xmin = xmin, ymin = ymin)
}
