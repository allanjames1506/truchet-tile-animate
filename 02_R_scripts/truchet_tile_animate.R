
# 1. LIBRARIES----

#https://paezha.github.io/truchet/articles/a03-figurative-mosaics.html

options(timeout = max(300, getOption("timeout")))
install.packages("devtools")
devtools::install_github("paezha/truchet")

library(dplyr)
library(gganimate)
library(ggplot2)
library(sf)
library(truchet)
library(purrr)
library(terra)
library(tmap)
library(magick)
library(here)

# tile_types <- data.frame(type = c("dl", "dr")) %>%
#   mutate(tile = 1:n(),
#          x = 2 * tile %% 2,
#          y = 2 * tile %/% 2)

# Tiles types
tile_types <- data.frame(type = c("Al", "Bl", "Cl", "Dl")) %>%
  mutate(x = c(1, 2.5, 1, 2.5),
         y = c(2.5, 2.5, 1, 1),
         b = 1/2)

# Elements for assembling the mosaic
x_c <- tile_types$x
y_c <- tile_types$y
type <- as.character(tile_types$type)
b <- tile_types$b

pmap_dfr(list(x_c, y_c, type, b), st_truchet_flex) %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = "black",
          size = 2) +
  geom_text(data = tile_types,
            aes(x = x,
                y = y,
                label = c("A", "B", "C", "D")),
            nudge_y = 0.6) + 
  scale_fill_distiller(direction = 1) +
  theme_void() +
  theme(legend.position = "none")

# magick
# https://r-charts.com/miscellaneous/image-processing-magick/?utm_content=cmp-true

truchet4_magick <- image_read(here("00_raw_data", "truchet4_348.png")) %>%
  image_crop(geometry = "340x340+0+4") %>%
  image_write(here("00_raw_data", "truchet4_340.png"), format = "png", quality = 75)

# https://stackoverflow.com/questions/68582979/is-there-a-way-to-transform-a-png-icon-to-a-sf-multipolygon-in-r

rast('./00_raw_data/truchet4_340.png') %>% 
  .[[1:3]] %>% 
  {. ->> truchet}

truchet

truchet %>% 
  hist

truchet %>% 
  app(fun = function(x) plyr::round_any(x, 10)) %>% 
  {. ->> truchet_rounded}

truchet_rounded

truchet_rounded %>% 
  as.polygons %>% 
  st_as_sf %>%
  rename(
    value = 1
  ) %>% 
  {. ->> truchet_multipoly}

truchet_multipoly

tm_shape(truchet_multipoly)+
  tm_polygons(col = 'value', style = 'cat')

truchet %>% 
  as.contour(level = 2) %>%
  st_as_sf %>% 
  st_cast('MULTIPOLYGON') %>% 
  {. ->> truchet_contour_250}

truchet_contour_250

truchet_contour_250 %>% 
  tm_shape()+
  tm_polygons()

# https://scikit-image.org/docs/stable/user_guide/install.html
# https://towardsdatascience.com/virtual-environments-104c62d48c54?gi=2a48dfdcdba9&skipOnboarding=1&source=-----104c62d48c54---------------------post_regwall-----------
# https://shop.paulrickards.com/truchet-tiles/
# https://happycoding.io/tutorials/p5js/images/truchet-tiles
# https://pixlr.com/editor/
# https://www.youtube.com/watch?v=8pYivqKFpFc
# https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/
# https://paezha.github.io/truchet/
# https://art-from-code.netlify.app/day-2/session-2/
# https://www.deconbatch.com/2022/03/truchet-tiling.html
# https://fronkonstin.com/page/2/
# https://rstudio.github.io/cheatsheets/gganimate.pdf





