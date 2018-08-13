# |------------------------------------------------------------------------|
# | Project: Batlight Plot                                                 |
# | Script:   main script, based on Andrew's work                          |
# | Authors:  Davit Sargsyan                                               |   
# | Created:  08/04/2018                                                   |
# | Modified:                                                              |
# |------------------------------------------------------------------------|
# Sources:
# https://github.com/tidyverse/ggplot2/wiki/Creating-a-new-geom
# https://deepsense.ai/how-to-create-a-new-geom-for-ggplot2/
# https://github.com/tidyverse/ggplot2/blob/master/vignettes/extending-ggplot2.Rmd
# Best reference
# https://stackoverflow.com/questions/36156387/how-to-make-a-custom-ggplot2-geom-with-multiple-geometries
# NOTE: use 'roxigen2' pacakge to create folder structure for the package;
# use 'packrat' to version control your dependencies.

# Load dependencies----
require(ggplot2)
require(data.table)
require(ggforce)

require(proto)
require(grid)

GeomBatlight <- ggproto("GeomBatlight", 
                        Geom,
                        required_aes = c("x",
                                         "y"),
                        setup_data = function(self, 
                                              data, 
                                              params) {
                          data <- ggproto_parent(Geom, self)$setup_data(data, params)
                          print(data)
                          data
                        },
                        default_aes = aes(shape = 21,
                                          colour = "black",
                                          fill = "red"),
                        
                        draw_group = function(data,
                                              panel_params,
                                              coord) {
                          print(panel_params)
                          print(coord)
                          coords <- coord$transform(data, 
                                                    panel_params)
                          obj1 <- grid::circleGrob(x = coords$x,
                                                   y = coords$y,
                                                   r = 1)

                          obj2 <- grid::segmentsGrob(x0 = coords$x,
                                                     y0 = coords$y,
                                                     x1 = coords$x + 1,
                                                     y1 = coords$y + 1)

                          obj3 <- grid::pointsGrob(x = coords$x,
                                                   y = coords$y,
                                                   pch = coords$shape,
                                                   gp = grid::gpar(col = coords$colour,
                                                                   fill = coords$fill))
                          
                          grobTree(obj1,
                                   obj2,
                                   obj3)
                          
                          # OPTION 2 (doesn't work!):
                          # obj1 <- grob(geom_segment(aes(x = coords$x,
                          #                               y = coords$y,
                          #                               xend = coords$x + 1,
                          #                               yend = coords$y + 1)))
                          # obj2 <- grob(geom_point(aes(x = c(coords$x,
                          #                                   coords$x + 1),
                          #                             y = c(coords$y,
                          #                                   coords$y + 1))))
                          # return(gList(obj1,
                          #              obj2))
                          
                        })

geom_batlight <- function(mapping = NULL, 
                          data = NULL, 
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE, 
                          show.legend = NA, 
                          inherit.aes = TRUE, ...) {
  layer(geom = GeomBatlight, 
        mapping = mapping, 
        data = data, 
        stat = stat, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

ggplot() + 
  geom_batlight(aes(x = 0,
                    y = 0)) +
  coord_cartesian(xlim = c(-2, 10),
                  ylim = c(-2, 10))