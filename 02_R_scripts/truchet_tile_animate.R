
# 1. LIBRARIES----

#https://paezha.github.io/truchet/articles/a03-figurative-mosaics.html

options(timeout = max(300, getOption("timeout")))
install.packages("devtools")
devtools::install_github("paezha/truchet")

library(dplyr)
library(gganimate)
library(ggplot2)
library(sf)
library(sfheaders)
library(truchet)
library(purrr)
library(terra)
library(tmap)
library(magick)
library(here)
library(patchwork)

xlim <- c(0, 7)
ylim <- c(0, 7)

# Create a data frame with the spots for tiles
container <- expand.grid(x = seq(xlim[1], xlim[2], 1),
                         y = seq(ylim[1], ylim[2], 1)) %>%
  mutate(tiles = case_when(x <= 3 ~ "dl", 
                           x > 3 ~ "dr"),
         scale_p = 1)

mosaic <- st_truchet_ms(df = container)

ggplot() +
  geom_sf(data = mosaic,
          aes(fill = color),
          color = NA)

#https://stackoverflow.com/questions/50303438/points-in-multiple-polygons-using-r

web_digitiser_truchet340_multi_polygon <- read.csv('./00_raw_data/web_digitiser_truchet340_multi_polygon_closed.csv')

max(web_digitiser_truchet340_multi_polygon$y)

web_digitiser_truchet340_multi_polygon[,c(1,2)] <-lapply(web_digitiser_truchet340_multi_polygon[,c(1,2)], scales::rescale, to=c(0,1))

# https://gis.stackexchange.com/questions/332427/converting-points-to-polygons-by-group

xys_AJ <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y"))

# st_truchet_fm(
#   df = web_digitiser_truchet340_multi_polygon
# ) %>%
#   ggplot() +
#   geom_sf(aes(fill = factor(group)))

xymp_AJ <- st_sf(
  aggregate(
    xys_AJ,
    by=list(ID=xys_AJ$group),
    do_union=TRUE,
    FUN=function(vals){vals[1]}))

xychull_AJ <- xymp_AJ

st_geometry(xychull_AJ) <- st_convex_hull(xymp_AJ$geometry)

plot(xychull_AJ['group'])

class(xychull_AJ)

glimpse(xys_AJ)

xychull_AJ_mclaren_colours <-  xychull_AJ %>% 
  mutate(group = case_when(group %in% 'A' ~ '1',
                           group %in% 'B' ~ '2',
                           group %in% 'C' ~ '3',
                           group %in% 'D' ~ '4',
                           group %in% 'E' ~ '5',
                           group %in% 'F' ~ '6',
                           group %in% 'G' ~ '7',
                           group %in% 'H' ~ '8',
                           group %in% 'I' ~ '9',
                           group %in% 'J' ~ '10',
                           group %in% 'K' ~ '11',
                           TRUE ~ group)) %>%
  mutate(group = as.numeric(group)) %>% 
  ggplot(aes(fill = group)) +
  geom_sf(color = NA, show.legend = FALSE) + 
  scale_fill_gradientn(colours = c("#FF8000", "#ffffff")) + 
  theme_void()

# 2. AJ version of ST_TRUCHET_P

# https://github.com/paezha/truchet/blob/5f8c93c1c316288612aa6280244cf176e7eed18e/R/st_truchet_p.R

#2. CREATE BASE TILE----
#  Define square polygon
tile <- matrix(c(0, 0,
                 0, 1,
                 1, 1,
                 1, 0,
                 0, 0),
               ncol = 2,
               byrow = TRUE)

# Convert coordinates to polygons and then to simple features
tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
                     sf::st_sfc()) %>%
  sf::st_as_sf()

# Points for base tile
pts <- data.frame(x = c(0, 0, 1, 1),
                  y = c(0, 1, 1, 0))

# Convert coordinates to points and then to simple features
pts <- pts %>%
  sf::st_as_sf(coords = c("x", "y"))

# Assign constant geometry
sf::st_agr(pts) <- "constant"

# Obtain points geometry
pts_gmtry <- pts %>%
    dplyr::mutate(geometry = pts %>%
                    dplyr::pull(.data$geometry))

# Assemble base tile
tile <- data.frame(colour = 1,
                   sf::st_geometry(rbind(tile,
                                         pts_gmtry) %>%
                                     sf::st_union())) %>%
  sf::st_as_sf()

plot(tile)

## BASE TILE DONE

# 3. POLYGON MATRICES----

polygon_A_as_matrix <- matrix(c(0.22435562, 0.43253092,
                                0.224319488,0.477261165,
                                0.224277381, 0.529389717,
                                0.225345924, 0.588887055,
                                0.227185158, 0.635444565,
                                0.232511174, 0.712425068,
                                0.242936336, 0.776662561,
                                0.253961728, 0.838992083,
                                0.269635125, 0.891209373,
                                0.291296055, 0.942624735,
                                0.308144899, 0.980814811,
                                0.334656975, 0.997100124,
                                0.36758407, 0.998062108,
                                0.400511835, 0.998195496,
                                0.433439599, 0.998328884,
                                0.466367206, 0.998657235,
                                0.499294813, 0.998985587,
                                0.532626951, 0.998504773,
                                0.567170662, 0.99809695,
                                0.604540436, 0.999022001,
                                0.639083232, 0.999746824,
                                0.676015334, 0.998392422,
                                0.681950601, 0.959355513,
                                0.683728615, 0.920804283,
                                0.684437555, 0.88050715,
                                0.687428857, 0.836098652,
                                0.689547322, 0.789909658,
                                0.689585144, 0.743085873,
                                0.689623255, 0.695904655,
                                0.690829699, 0.643502816,
                                0.69011789, 0.588893822,
                                0.685368703, 0.525540028,
                                0.678140013, 0.463627745,
                                0.671442511, 0.427195045,
                                0.63148084, 0.411508585,
                                0.598565359, 0.396168028,
                                0.565650508, 0.380047615,
                                0.532736287, 0.363147348,
                                0.499821908, 0.346442045,
                                0.466908475, 0.328566959,
                                0.433995198, 0.31049691,
                                0.401083812, 0.290087296,
                                0.368171323, 0.271042428,
                                0.33526051, 0.249923363,
                                0.291473123, 0.223414191,
                                0.261659971, 0.249743584,
                                0.22802993, 0.284469994,
                                0.228382472, 0.329640097,
                                0.226394577, 0.378889942,
                                0.22435562, 0.43253092),
                              ncol = 2,
                              byrow = TRUE)

polygon_B_as_matrix <- matrix(c(-0.000291386, 0.604346926,
                                0.009300166, 0.568241763,
                                0.018606338, 0.532494013,
                                0.037189431, 0.526625664,
                                0.025469371, 0.498626025, 
                                0.057406472, 0.497928967,
                                0.077648936, 0.467169465,
                                0.037781441, 0.4466587,
                                0.052102218, 0.388123984,
                                0.067109724, 0.338243216,
                                0.079456073, 0.288757327,
                                0.09429943, 0.236211588,
                                0.109140423, 0.186591697,
                                0.122816079, 0.138482305,
                                0.133556517, 0.106526241,
                                0.23826048, 0.18735358,
                                0.22802993, 0.284469994,
                                0.194263916, 0.318412901,
                                0.163192808, 0.349952494,
                                0.132514952, 0.389196022,
                                0.103957349, 0.426766779,
                                0.170713242, 0.135912103,
                                0.203620061, 0.161975665,
                                0.233933066, 0.234476035,
                                -0.000291386, 0.604346926),
                              ncol = 2,
                              byrow = TRUE)

polygon_C_as_matrix <- matrix(c(0.23826048, 0.18735358,
                                0.233933066, 0.234476035,
                                0.22802993, 0.284469994,
                                0.291473123, 0.223414191,
                                0.264095009, 0.205754946,
                                0.261659971, 0.249743584,
                                0.23826048, 0.18735358),
                              ncol = 2,
                              byrow = TRUE)

polygon_D_as_matrix <- matrix(c(0.291473123, 0.223414191,
                                0.671442511, 0.427195045,
                                0.63148084, 0.411508585,
                                0.598565359, 0.396168028,
                                0.565650508, 0.380047615,
                                0.532736287, 0.363147348,
                                0.499821908, 0.346442045,
                                0.466908475, 0.328566959,
                                0.433995198, 0.31049691,
                                0.401083812, 0.290087296,
                                0.368171323, 0.271042428,
                                0.33526051, 0.249923363,
                                0.662510144, 0.371561372,
                                0.646527114, 0.313632445,
                                0.631328134, 0.263660002,
                                0.609523254, 0.212646261,
                                0.585659456, 0.177081265,
                                0.368255591, 0.166719512,
                                0.33854519, 0.183489879,
                                0.409644822, 0.143763283,
                                0.446114435, 0.130411324,
                                0.479041422, 0.131507152,
                                0.511966352, 0.135149886,
                                0.550415539, 0.152703888,
                                0.312774426, 0.205342234,
                                0.291473123, 0.223414191),
                              ncol = 2,
                              byrow = TRUE)

polygon_E_as_matrix <- matrix(c(0.802435448, -0.083237344,
                                0.836081646, -0.060760607,
                                0.874240462, -0.036668739,
                                0.913206867, -0.012372445,
                                0.946117781, 0.00862206,
                                0.975029427, 0.02482838,
                                0.999971073, 0.044128763,
                                0.998436246, 0.091303918,
                                0.998398135, 0.138485136,
                                0.998360024, 0.185666354,
                                0.998321913, 0.232847572,
                                0.998283802, 0.28002879,
                                0.998245692, 0.327210009,
                                0.998207581, 0.374391227,
                                0.99816947, 0.421572445,
                                0.998123412, 0.478591594,
                                0.99796371, 0.529244542,
                                0.974625798, 0.524520372,
                                0.945704704, 0.520011875,
                                0.912783396, 0.511884975,
                                0.879861773, 0.504148002,
                                0.846941883, 0.494266429,
                                0.814023095, 0.483020109,
                                0.781103992, 0.472163717,
                                0.748185834, 0.460137543,
                                0.715268463, 0.44713655,
                                0.671442511, 0.427195045,
                                0.511966352, 0.135149886,
                                0.522968806, 0.13175937, 
                                0.56095881, 0.129546253,
                                0.598535896, 0.123818959,
                                0.631719741, 0.115748635,
                                0.664655222, 0.106328801,
                                0.697593066, 0.093984511,
                                0.730537733, 0.073191794,
                                0.763491272, 0.041416121,
                                0.78371382, 0.005902503,
                                0.797169696, -0.034824579,
                                0.550415539, 0.152703888,
                                0.662510144, 0.371561372,
                                0.646527114, 0.313632445,
                                0.631328134, 0.263660002,
                                0.609523254, 0.212646261,
                                0.585659456, 0.177081265,
                                0.802435448, -0.083237344),
                              ncol = 2,
                              byrow = TRUE)

polygon_F_as_matrix <- matrix(c(0.133556517, 0.106526241,
                                0.141343944, 0.083338436,
                                0.149631303, 0.058881413,
                                0.12835074, 0.05127736,
                                0.098164281, 0.039760741,
                                0.075598763, 0.064107956,
                                0.101666759, 0.086046159,
                                0.063133312, 0.025748125,
                                0.042835153, 0.037221792,
                                0.031545169, 0.007434353,
                                0.015794165, 0.013964103,
                                -0.002790918, -0.007114724,
                                0.133556517, 0.106526241),
                              ncol = 2,
                              byrow = TRUE)

polygon_G_as_matrix <- matrix(c(0.133556517, 0.106526241,
                                0.141343944, 0.083338436,
                                0.149631303, 0.058881413,
                                0.23826048, 0.18735358,
                                0.241853416, 0.152611333,
                                0.245111536, 0.119059717,
                                0.247962039, 0.090136079,
                                0.216384388, 0.083267964,
                                0.186325052, 0.073196825,
                                0.170713242, 0.135912103,
                                0.203620061, 0.161975665,
                                0.133556517,0.106526241),
                              ncol = 2,
                              byrow = TRUE)

polygon_H_as_matrix <- matrix(c(0.291473123, 0.223414191,
                                0.264095009, 0.205754946,
                                0.23826048, 0.18735358,
                                0.241853416, 0.152611333,
                                0.245111536, 0.119059717,
                                0.247962039, 0.090136079,
                                0.409644822,	0.143763283,
                                0.446114435,	0.130411324,
                                0.368255591,	0.166719512,
                                0.33854519,	0.183489879,
                                0.312774426,	0.205342234,
                                0.26953304,	0.091119679,
                                0.302453119,	0.100767296,
                                0.335372735,	0.110989282,
                                0.368294525,	0.118518912,
                                0.407417767,	0.125121279,
                                0.291473123,	0.223414191),
                              ncol = 2,
                              byrow = TRUE)

polygon_I_as_matrix <- matrix(c(0.16118388,	0.02149751,
                                0.172034672,	-0.01766571,
                                0.185841346,	-0.063268481,
                                0.200843677,	-0.106743247,
                                0.217333425,	-0.15474036,
                                0.236687097,	-0.205495664,
                                0.289737832,	-0.216530202,
                                0.254850534,	-0.256825186,
                                0.274376645,	-0.305263453,
                                0.303046377,	-0.28074493,
                                0.313784044,	-0.338682248,
                                0.296122491,	-0.35603226,
                                0.326047847,	-0.388916814,
                                0.319851015,	-0.408415297,
                                0.335804371,	-0.435141248,
                                0.149631303,	0.058881413,
                                0.247962039,	0.090136079,
                                0.279314388,	-0.159365013,
                                0.271937549,	-0.106250322,
                                0.264472506,	-0.045008683,
                                0.257578097,	0.008986308,
                                0.252436461,	0.050802353,
                                0.216384388,	0.083267964,
                                0.186325052,	0.073196825,
                                0.16118388,	0.02149751),
                              ncol = 2,
                              byrow = TRUE)

polygon_J_as_matrix <- matrix(c(0.289737832,	-0.216530202,
                                0.303046377,	-0.28074493,
                                0.313784044,	-0.338682248,
                                0.326047847,	-0.388916814,
                                0.335804371,	-0.435141248,
                                0.353120031,	-0.418986545,
                                0.802435448,	-0.083237344,
                                0.247962039,	0.090136079,
                                0.26953304,	0.091119679,
                                0.302453119,	0.100767296,
                                0.335372735,	0.110989282,
                                0.368294525,	0.118518912,
                                0.407417767,	0.125121279,
                                0.446114435,	0.130411324,
                                0.479041422,	0.131507152,
                                0.522968806,	0.13175937,
                                0.56095881,	0.129546253,
                                0.598535896,	0.123818959,
                                0.631719741,	0.115748635,
                                0.664655222,	0.106328801,
                                0.697593066,	0.093984511,
                                0.730537733,	0.073191794,
                                0.763491272,	0.041416121,
                                0.78371382,	0.005902503,
                                0.797169696,	-0.034824579,
                                0.745661384,	-0.120476516,
                                0.71275362,	-0.145370295,
                                0.679843966,	-0.16792451,
                                0.646934785,	-0.191063616,
                                0.614026233,	-0.214982577,
                                0.581118154,	-0.239486429,
                                0.548210862,	-0.264965099,
                                0.515304043,	-0.291028661,
                                0.482395963,	-0.315532512,
                                0.449488987,	-0.34140111,
                                0.41658264,	-0.368049563,
                                0.386501429,	-0.392217023,
                                0.279314388,	-0.159365013,
                                0.271937549,	-0.106250322,
                                0.264472506,	-0.045008683,
                                0.257578097,	0.008986308,
                                0.252436461,	0.050802353,
                                0.778573361,	-0.100798016,
                                0.289737832,	-0.216530202),
                              ncol = 2,
                              byrow = TRUE)

polygon_K_as_matrix <- matrix(c(0.335804371,	-0.435141248,
                                0.353120031,	-0.418986545,
                                0.373019319,	-0.436658521,
                                0.411554216,	-0.436978999,
                                0.449565681,	-0.436348438,
                                0.482493445,	-0.43621505,
                                0.51542121,	-0.436081662,
                                0.548348974,	-0.435948274,
                                0.581276739,	-0.435814886,
                                0.614204503,	-0.435681498,
                                0.64713198,	-0.435191605,
                                0.678181405,	-0.433202833,
                                0.702721434,	-0.393170996,
                                0.72116833,	-0.356898522,
                                0.739251851,	-0.320768622,
                                0.755511671,	-0.281194313,
                                0.773028921,	-0.236781032,
                                0.788518109,	-0.182983945,
                                0.798057829,	-0.134334189,
                                0.778573361,	-0.100798016,
                                0.802435448,	-0.083237344,
                                0.71275362,	-0.145370295,
                                0.679843966,	-0.16792451,
                                0.646934785,	-0.191063616,
                                0.614026233,	-0.214982577,
                                0.581118154,	-0.239486429,
                                0.548210862,	-0.264965099,
                                0.515304043,	-0.291028661,
                                0.482395963,	-0.315532512,
                                0.449488987,	-0.34140111,
                                0.41658264,	-0.368049563,
                                0.386501429,	-0.392217023,
                                0.745661384,	-0.120476516,
                                0.335804371,	-0.435141248),
                              ncol = 2,
                              byrow = TRUE)

# 4. SMOOTHED POLYGONS----
# *4.1 Polygon A----
polygon_A <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_A_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_A) <- "constant"

# Smooth the polygon
polygon_A <- polygon_A %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

# *4.2 Polygon B----
polygon_B <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_B_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_B) <- "constant"

# Smooth the polygon
polygon_B <- polygon_B %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

# *4.3 Polygon C----
polygon_C <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_C_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_C) <- "constant"

# Smooth the polygon
polygon_C <- polygon_C %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

# *4.4 Polygon D----
polygon_D <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_D_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_D) <- "constant"

# Smooth the polygon
polygon_D <- polygon_D %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

# *4.5 Polygon E----
polygon_E <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_E_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_E) <- "constant"

# Smooth the polygon
polygon_E <- polygon_E %>% 
  st_concave_hull(0.1) %>%
  smooth(method = "chaikin")

polygon_F <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_F_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

polygon_G <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_G_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

polygon_H <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_H_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

polygon_I <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_I_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

polygon_J <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_J_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

polygon_K <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(polygon_K_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() %>% 
  st_convex_hull() %>%
  smooth(method = "chaikin")

# 5. UNIONS between POLYGONS and BASE TILE----

# https://stackoverflow.com/questions/54710574/how-to-do-a-full-union-with-the-r-package-sf
# https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html
# https://gist.github.com/mstrimas/ac50a38a7e656a2b3a173f3a6b31a760

output1 <- st_difference(tile, st_union(polygon_E)) #notice the use of st_union()

#plot(st_geometry(op1), border="red", add=TRUE)

#op2 <- st_difference(polygon_E, st_union(tile)) #notice the order of b and a and st_union()
#plot(st_geometry(op2), border="green", add=TRUE)

output2 <- st_intersection(polygon_E, tile) #notice the order 

#plot(st_geometry(op3), border="blue", add=TRUE)

union <- dplyr::bind_rows(output1, output2) %>% 
  select(colour, geometry)

plot(union)

# 6. TILE TYPES----

aj_truchet_p <- function(x = 0, y = 0, type = "dl", scale_p = 1){
  
  #' Truchet tiles made with polygons
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "A", "B", "C", "D", "E"
  #' @param scale_p A number to designate the scale of the tile; currently supported options are 1, 1/2, and 1/4
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_p(type = "A")
  #' st_truchet_p(type = "B", scale_p = 1/2)
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}
  
  # Validate inputs
  checkmate::assertChoice(scale_p, c(1, 1/2, 1/4))
  checkmate::assertChoice(type, c("A", "B", "C", "D", "E"))

  # CREATE BASE TILE
  # Define square polygon
  tile <- matrix(c(0, 0,
                   0, 1,
                   1, 1,
                   1, 0,
                   0, 0),
                 ncol = 2,
                 byrow = TRUE)
  
  # Convert coordinates to polygons and then to simple features
  tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
                       sf::st_sfc()) %>%
    sf::st_as_sf()
  
  # Points for base tile
  pts <- data.frame(x = c(0, 0, 1, 1),
                    y = c(0, 1, 1, 0))
  
  # Convert coordinates to points and then to simple features
  pts <- pts %>%
    sf::st_as_sf(coords = c("x", "y"))
  
  # Assign constant geometry
  sf::st_agr(pts) <- "constant"
  
  # Obtain points geometry
  pts_gmtry <- pts %>%
    dplyr::mutate(geometry = pts %>%
                    dplyr::pull(.data$geometry))
  
  # Assemble base tile
  tile <- data.frame(colour = 1,
                     sf::st_geometry(rbind(tile,
                                           pts_gmtry) %>%
                                       sf::st_union())) %>%
    sf::st_as_sf()
  
  # Tile types
  
  switch(type,
         
         "A" ={
           ## POLYGON A
           A_as_matrix <- matrix(c(0.22435562, 0.43253092,
                                   0.224319488,0.477261165,
                                   0.224277381, 0.529389717,
                                   0.225345924, 0.588887055,
                                   0.227185158, 0.635444565,
                                   0.232511174, 0.712425068,
                                   0.242936336, 0.776662561,
                                   0.253961728, 0.838992083,
                                   0.269635125, 0.891209373,
                                   0.291296055, 0.942624735,
                                   0.308144899, 0.980814811,
                                   0.334656975, 0.997100124,
                                   0.36758407, 0.998062108,
                                   0.400511835, 0.998195496,
                                   0.433439599, 0.998328884,
                                   0.466367206, 0.998657235,
                                   0.499294813, 0.998985587,
                                   0.532626951, 0.998504773,
                                   0.567170662, 0.99809695,
                                   0.604540436, 0.999022001,
                                   0.639083232, 0.999746824,
                                   0.676015334, 0.998392422,
                                   0.681950601, 0.959355513,
                                   0.683728615, 0.920804283,
                                   0.684437555, 0.88050715,
                                   0.687428857, 0.836098652,
                                   0.689547322, 0.789909658,
                                   0.689585144, 0.743085873,
                                   0.689623255, 0.695904655,
                                   0.690829699, 0.643502816,
                                   0.69011789, 0.588893822,
                                   0.685368703, 0.525540028,
                                   0.678140013, 0.463627745,
                                   0.671442511, 0.427195045,
                                   0.63148084, 0.411508585,
                                   0.598565359, 0.396168028,
                                   0.565650508, 0.380047615,
                                   0.532736287, 0.363147348,
                                   0.499821908, 0.346442045,
                                   0.466908475, 0.328566959,
                                   0.433995198, 0.31049691,
                                   0.401083812, 0.290087296,
                                   0.368171323, 0.271042428,
                                   0.33526051, 0.249923363,
                                   0.291473123, 0.223414191,
                                   0.261659971, 0.249743584,
                                   0.22802993, 0.284469994,
                                   0.228382472, 0.329640097,
                                   0.226394577, 0.378889942,
                                   0.22435562, 0.43253092),
                                 ncol = 2,
                                 byrow = TRUE)
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_A <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(A_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_A) <- "constant"
           
           # Smooth the polygon
           polygon_A <- polygon_A %>% 
             st_convex_hull() %>%
             smooth(method = "chaikin")
           
           # Bind BASE TILE with polygon
           # polygon A
           
           output1 <- st_difference(tile, st_union(polygon_A))
           
           output2 <- st_intersection(polygon_A, tile)
           
           tile <- dplyr::bind_rows(output1, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON A DONE
           
           },
       
       "B" ={
         ## POLYGON B
         B_as_matrix <- matrix(c(-0.000291386, 0.604346926,
                                 0.009300166, 0.568241763,
                                 0.018606338, 0.532494013,
                                 0.037189431, 0.526625664,
                                 0.025469371, 0.498626025, 
                                 0.057406472, 0.497928967,
                                 0.077648936, 0.467169465,
                                 0.037781441, 0.4466587,
                                 0.052102218, 0.388123984,
                                 0.067109724, 0.338243216,
                                 0.079456073, 0.288757327,
                                 0.09429943, 0.236211588,
                                 0.109140423, 0.186591697,
                                 0.122816079, 0.138482305,
                                 0.133556517, 0.106526241,
                                 0.23826048, 0.18735358,
                                 0.22802993, 0.284469994,
                                 0.194263916, 0.318412901,
                                 0.163192808, 0.349952494,
                                 0.132514952, 0.389196022,
                                 0.103957349, 0.426766779,
                                 0.170713242, 0.135912103,
                                 0.203620061, 0.161975665,
                                 0.233933066, 0.234476035,
                                 -0.000291386, 0.604346926),
                               ncol = 2,
                               byrow = TRUE)
         
         # Convert coordinates to polygon and then to simple features and then smooth
         polygon_B <- data.frame(colour = 2,
                                 geometry = sf::st_polygon(list(B_as_matrix)) %>% 
                                   sf::st_sfc()) %>%
           sf::st_as_sf() 
         
         # Assign constant geometry
         sf::st_agr(polygon_B) <- "constant"
         
         # Smooth the polygon
         polygon_B <- polygon_B %>% 
           st_convex_hull() %>%
           smooth(method = "chaikin")
         
         # Bind BASE TILE with polygon
         # polygon B
         
         output1 <- st_difference(tile, st_union(polygon_B))
         
         output2 <- st_intersection(polygon_B, tile)
         
         tile <- dplyr::bind_rows(output1, output2) %>% 
           select(colour, geometry)
         
         ## POLYGON B DONE
       },
       
       "C" ={
         ## POLYGON C
         C_as_matrix <- matrix(c(0.23826048, 0.18735358,
                                 0.233933066, 0.234476035,
                                 0.22802993, 0.284469994,
                                 0.291473123, 0.223414191,
                                 0.264095009, 0.205754946,
                                 0.261659971, 0.249743584,
                                 0.23826048, 0.18735358),
                               ncol = 2,
                               byrow = TRUE)
         
         # Convert coordinates to polygon and then to simple features and then smooth
         polygon_C <- data.frame(colour = 2,
                                 geometry = sf::st_polygon(list(C_as_matrix)) %>% 
                                   sf::st_sfc()) %>%
           sf::st_as_sf() 
         
         # Assign constant geometry
         sf::st_agr(polygon_C) <- "constant"
         
         # Smooth the polygon
         polygon_C <- polygon_C %>% 
           st_convex_hull() %>%
           smooth(method = "chaikin")
         
         # Bind BASE TILE with polygon
         # polygon C
         
         output1 <- st_difference(tile, st_union(polygon_C))
         
         output2 <- st_intersection(polygon_C, tile)
         
         tile <- dplyr::bind_rows(output1, output2) %>% 
           select(colour, geometry)
         
         ## POLYGON C DONE
       },
       
       "D" ={
         ## POLYGON D
         D_as_matrix <- matrix(c(0.291473123, 0.223414191,
                                 0.671442511, 0.427195045,
                                 0.63148084, 0.411508585,
                                 0.598565359, 0.396168028,
                                 0.565650508, 0.380047615,
                                 0.532736287, 0.363147348,
                                 0.499821908, 0.346442045,
                                 0.466908475, 0.328566959,
                                 0.433995198, 0.31049691,
                                 0.401083812, 0.290087296,
                                 0.368171323, 0.271042428,
                                 0.33526051, 0.249923363,
                                 0.662510144, 0.371561372,
                                 0.646527114, 0.313632445,
                                 0.631328134, 0.263660002,
                                 0.609523254, 0.212646261,
                                 0.585659456, 0.177081265,
                                 0.368255591, 0.166719512,
                                 0.33854519, 0.183489879,
                                 0.409644822, 0.143763283,
                                 0.446114435, 0.130411324,
                                 0.479041422, 0.131507152,
                                 0.511966352, 0.135149886,
                                 0.550415539, 0.152703888,
                                 0.312774426, 0.205342234,
                                 0.291473123, 0.223414191),
                               ncol = 2,
                               byrow = TRUE)
         
         # Convert coordinates to polygon and then to simple features and then smooth
         polygon_D <- data.frame(colour = 2,
                                 geometry = sf::st_polygon(list(D_as_matrix)) %>% 
                                   sf::st_sfc()) %>%
           sf::st_as_sf() 
         
         # Assign constant geometry
         sf::st_agr(polygon_D) <- "constant"
         
         # Smooth the polygon
         polygon_D <- polygon_D %>% 
           st_convex_hull() %>%
           smooth(method = "chaikin")
         
         # Bind BASE TILE with polygon
         # polygon D
         
         output1 <- st_difference(tile, st_union(polygon_D))
         
         output2 <- st_intersection(polygon_D, tile)
         
         tile <- dplyr::bind_rows(output1, output2) %>% 
           select(colour, geometry)
         
         ## POLYGON D DONE
       },
       
       "E" ={
         ## POLYGON E
         E_as_matrix <- matrix(c(0.802435448, -0.083237344,
                                 0.836081646, -0.060760607,
                                 0.874240462, -0.036668739,
                                 0.913206867, -0.012372445,
                                 0.946117781, 0.00862206,
                                 0.975029427, 0.02482838,
                                 0.999971073, 0.044128763,
                                 0.998436246, 0.091303918,
                                 0.998398135, 0.138485136,
                                 0.998360024, 0.185666354,
                                 0.998321913, 0.232847572,
                                 0.998283802, 0.28002879,
                                 0.998245692, 0.327210009,
                                 0.998207581, 0.374391227,
                                 0.99816947, 0.421572445,
                                 0.998123412, 0.478591594,
                                 0.99796371, 0.529244542,
                                 0.974625798, 0.524520372,
                                 0.945704704, 0.520011875,
                                 0.912783396, 0.511884975,
                                 0.879861773, 0.504148002,
                                 0.846941883, 0.494266429,
                                 0.814023095, 0.483020109,
                                 0.781103992, 0.472163717,
                                 0.748185834, 0.460137543,
                                 0.715268463, 0.44713655,
                                 0.671442511, 0.427195045,
                                 0.511966352, 0.135149886,
                                 0.522968806, 0.13175937, 
                                 0.56095881, 0.129546253,
                                 0.598535896, 0.123818959,
                                 0.631719741, 0.115748635,
                                 0.664655222, 0.106328801,
                                 0.697593066, 0.093984511,
                                 0.730537733, 0.073191794,
                                 0.763491272, 0.041416121,
                                 0.78371382, 0.005902503,
                                 0.797169696, -0.034824579,
                                 0.550415539, 0.152703888,
                                 0.662510144, 0.371561372,
                                 0.646527114, 0.313632445,
                                 0.631328134, 0.263660002,
                                 0.609523254, 0.212646261,
                                 0.585659456, 0.177081265,
                                 0.802435448, -0.083237344),
                               ncol = 2,
                               byrow = TRUE)
         
         # Convert coordinates to polygon and then to simple features and then smooth
         polygon_E <- data.frame(colour = 2,
                                 geometry = sf::st_polygon(list(E_as_matrix)) %>% 
                                   sf::st_sfc()) %>%
           sf::st_as_sf() 
         
         # Assign constant geometry
         sf::st_agr(polygon_E) <- "constant"
         
         # Smooth the polygon
         polygon_E <- polygon_E %>% 
           st_concave_hull(0.1) %>%
           smooth(method = "chaikin")
         
         # Bind BASE TILE with polygon
         # polygon E
         
         output1 <- st_difference(tile, st_union(polygon_E))
         
         output2 <- st_intersection(polygon_E, tile)
         
         tile <- dplyr::bind_rows(output1, output2) %>% 
           select(colour, geometry)
         
         ## POLYGON E DONE
       }
)

# Translate so that the tiles are centered on the point (0, 0)
tile <- tile %>%
  dplyr::mutate(geometry = sf::st_geometry(tile) + c(-0.5, - 0.5))

## SCALE AS REQUESTED
if(methods::hasArg(scale_p)){
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) * scale_p)
  if(scale_p == 1/2){
    # If scale is 1/2 reverse colors
    tile <- tile %>%
      dplyr::mutate(color = dplyr::case_when(color == 1 ~ 2,
                                             color == 2 ~ 1))
  }
}
## FINISH SCALING

## FINISH TILES
# position at point (x, y)
tile <- tile %>%
  dplyr::mutate(geometry = sf::st_geometry(tile) + c(x, y)) %>%
  sf::st_sf()

## TILES DONE

return(tile)
}

# Bind BASE TILE with adorn1
# dl
tile <- rbind(tile,
              adorn1 %>%
                dplyr::transmute(color = 2))

plot(tile)

ggplot() +
  geom_sf(data = tile)

glimpse(tile)

# actual
tile <- matrix(c(0, 0,
                 0, 1,
                 1, 1,
                 1, 0,
                 0, 0),
               ncol = 2,
               byrow = TRUE)

# Convert coordinates to polygons and then to simple features
tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
                     sf::st_sfc()) %>%
  sf::st_as_sf()

# Points
pts <- data.frame(x = c(0, 0, 1, 1),
                  y = c(0, 1, 1, 0))

# Convert coordinates to points and then to simple features
pts <- pts %>%
  sf::st_as_sf(coords = c("x", "y"))

# Assign constant geometry
sf::st_agr(pts) <- "constant"

# Circle segments
cs <- c(1/3)

# Create first set of buffers and cast to polygons
bfs_1 <- pts %>%
  dplyr::mutate(r = cs[1],
                geometry = pts %>%
                  sf::st_buffer(dist = .data$r) %>%
                  dplyr::pull(.data$geometry)) %>%
  dplyr::select(-.data$r)

# Assemble base tile
tile <- data.frame(color = 1,
                   sf::st_geometry(rbind(tile,
                                         bfs_1) %>%
                                     sf::st_union())) %>%
  sf::st_as_sf()

plot(tile)

pts_act <- data.frame(x = c(0, 1),
                      y = c(0, 1))

# Convert coordinates to points and then to simple features
pts_act <- pts_act %>%
  sf::st_as_sf(coords = c("x", "y"))

# Assign constant geometry
sf::st_agr(pts_act) <- "constant"

# Circle segments
cs <- c(1/2)

# Make lines for second set of buffers
# Create buffers and cast to lines
bfs_2 <- pts_act %>%
  dplyr::mutate(r = cs[1],
                geometry = pts_act %>%
                  sf::st_buffer(dist = .data$r) %>%
                  dplyr::pull(.data$geometry)) %>%
  sf::st_set_agr("constant") %>%
  sf::st_cast(to = "LINESTRING") %>%
  dplyr::select(-.data$r)

# Intersect lines with tile
line_1 <- bfs_2 %>%
  sf::st_intersection(sf::st_geometry(tile))

line_2 <- bfs_2 %>%
  sf::st_intersection(sf::st_geometry(tile))

# Buffer the lines
bfs_2 <- rbind(line_1 %>%
                 sf::st_buffer(dist = 1/6))

# Set geometry to constant
sf::st_agr(tile) <- "constant"
sf::st_agr(bfs_2) <- "constant"

# Bind BASE TILE with second set of buffers
# dl
tile_act <- rbind(tile %>%
                sf::st_difference(bfs_2 %>%
                                    sf::st_union()),
              bfs_2 %>%
                dplyr::transmute(color = 2))

plot(tile_act)

polygon_A <- web_digitiser_truchet340_multi_polygon %>%
  filter(group %in% 'A') %>%
  select(x,y)

polygon_A <- data.frame(geometry = sf::st_polygon(list(polygon_A_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf()

plot(polygon_A)

## TEST ADORNMENT AJ----
### group A----

shapeA_xy_coords <- web_digitiser_truchet340_multi_polygon %>% 
  filter(group %in% 'A') %>% 
  select(x,y) %>% 
  as.data.frame()

# Convert coordinates to points and then to simple features

points_AJ <- shapeA_xy_coords %>%
  sf::st_as_sf(coords = c("x", "y"))

plot(points_AJ)

# Assign constant geometry
sf::st_agr(points_AJ) <- "constant"

points_AJ <- points_AJ %>% 
  mutate(geometry = points_AJ) %>% 
           dplyr::pull(.data$geometry) 
# %>%sf::st_set_agr("constant") %>%
#   sf::st_cast(to = "LINESTRING")

tile_points_AJ <- data.frame(color = 1,
                   sf::st_geometry(rbind(tile,
                                         points_AJ) %>%
                                     sf::st_union())) %>%
  sf::st_as_sf()

class(tile_points_AJ)

ggplot() +
  geom_sf(data = tile_points_AJ, colour = 'black')

# Circle segments
cs <- c(1/2)

# Make lines for second set of buffers
# Create buffers and cast to lines
bfs_2_AJ <- shapeA_xy_coords_points_sf %>%
  dplyr::mutate(#r = cs[1],
                geometry = shapeA_xy_coords_points_sf %>%
                  #sf::st_buffer(dist = .data$r) %>%
                  dplyr::pull(.data$geometry)) %>%
  sf::st_set_agr("constant") %>%
  sf::st_cast(to = "LINESTRING") 
#%>%dplyr::select(-.data$r)

plot(bfs_2_AJ)

# Intersect lines with tile
line_1_AJ <- bfs_2_AJ %>%
  sf::st_intersection(sf::st_geometry(tile))

line_2_AJ <- bfs_2_AJ %>%
  sf::st_intersection(sf::st_geometry(tile))

# Buffer the lines
bfs_2_AJ <- rbind(line_1_AJ %>%
                    sf::st_buffer(dist = 1/6))

# Set geometry to constant
sf::st_agr(tile) <- "constant"
sf::st_agr(bfs_2_AJ) <- "constant"

# Bind BASE TILE with second set of buffers
# dl
tile <- rbind(tile %>%
                sf::st_difference(bfs_2_AJ %>%
                                    sf::st_union()),
              bfs_2_AJ %>%
                dplyr::transmute(color = 2))

plot(tile)



pts_A_test <- data.frame(x = c(0, 1),
                         y = c(0, 1))

# Tile types

switch(type,
       
       "dl" ={
         ## ADORNMENTS
         pts <- data.frame(x = c(0, 1),
                           y = c(0, 1))
         
         # Convert coordinates to points and then to simple features
         pts <- pts %>%
           sf::st_as_sf(coords = c("x", "y"))
         
         # Assign constant geometry
         sf::st_agr(pts) <- "constant"
         
         # Circle segments
         cs <- c(1/2)
         
         # Make lines for second set of buffers
         # Create buffers and cast to lines
         bfs_2 <- pts %>%
           dplyr::mutate(r = cs[1],
                         geometry = pts %>%
                           sf::st_buffer(dist = .data$r) %>%
                           dplyr::pull(.data$geometry)) %>%
           sf::st_set_agr("constant") %>%
           sf::st_cast(to = "LINESTRING") %>%
           dplyr::select(-.data$r)
         
         # Intersect lines with tile
         line_1 <- bfs_2 %>%
           sf::st_intersection(sf::st_geometry(tile))
         
         line_2 <- bfs_2 %>%
           sf::st_intersection(sf::st_geometry(tile))
         
         # Buffer the lines
         bfs_2 <- rbind(line_1 %>%
                          sf::st_buffer(dist = 1/6))
         
         # Set geometry to constant
         sf::st_agr(tile) <- "constant"
         sf::st_agr(bfs_2) <- "constant"
         
         # Bind BASE TILE with second set of buffers
         # dl
         tile <- rbind(tile %>%
                         sf::st_difference(bfs_2 %>%
                                             sf::st_union()),
                       bfs_2 %>%
                         dplyr::transmute(color = 2))
         ## ADORNMENTS DONE
       },
       
       "dr" ={
         ## ADORNMENTS
         pts <- data.frame(x = c(0, 1),
                           y = c(1, 0))
         
         # Convert coordinates to points and then to simple features
         pts <- pts %>%
           sf::st_as_sf(coords = c("x", "y"))
         
         # Assign constant geometry
         sf::st_agr(pts) <- "constant"
         
         # Circle segments
         cs <- c(1/2)
         
         # Make lines for second set of buffers
         # Create buffers and cast to lines
         bfs_2 <- pts %>%
           dplyr::mutate(r = cs[1],
                         geometry = pts %>%
                           sf::st_buffer(dist = .data$r) %>%
                           dplyr::pull(.data$geometry)) %>%
           sf::st_set_agr("constant") %>%
           sf::st_cast(to = "LINESTRING") %>%
           dplyr::select(-.data$r)
         
         # Intersect lines with tile
         line_2 <- bfs_2 %>%
           sf::st_intersection(sf::st_geometry(tile))
         
         # Buffer the lines
         bfs_2 <- rbind(line_2 %>%
                          sf::st_buffer(dist = 1/6))
         
         # Set geometry to constant
         sf::st_agr(tile) <- "constant"
         sf::st_agr(bfs_2) <- "constant"
         
         # Bind BASE TILE with second set of buffers
         tile <- rbind(tile %>%
                         sf::st_difference(bfs_2 %>%
                                             sf::st_union()),
                       bfs_2 %>%
                         dplyr::transmute(color = 2))
         ## ADORNMENTS DONE
       # },
       # 
       # "|" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(0, 1),
       #                     y = c(1/2, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create first set of buffers and cast to polygons
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   
       #   # Make lines for second set of buffers
       #   # Make lines
       #   line_1 <- matrix(c(1/2, 1/2,
       #                      0, 1),
       #                    nrow = 2,
       #                    byrow = FALSE)
       #   
       #   
       #   # Convert coordinates to lines and then to simple features
       #   line_1 <- data.frame(id = "line_1",
       #                        r = 1/6,
       #                        geometry = sf::st_linestring(line_1) %>%
       #                          sf::st_sfc()) %>%
       #     sf::st_as_sf()
       #   
       #   # Buffer the lines and join to dots
       #   bfs_2 <- rbind(line_1 %>%
       #                    sf::st_buffer(dist = 1/6)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "-" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1/2),
       #                     y = c(1, 0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create first set of buffers and cast to polygons
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   
       #   # Make lines for second set of buffers
       #   # Make lines
       #   line_2 <- matrix(c(0, 1,
       #                      1/2, 1/2),
       #                    nrow = 2,
       #                    byrow = FALSE)
       #   
       #   # Convert coordinates to lines and then to simple features
       #   line_2 <- data.frame(id = "line_2",
       #                        r = 1/6,
       #                        geometry = sf::st_linestring(line_2) %>%
       #                          sf::st_sfc()) %>%
       #     sf::st_as_sf()
       #   
       #   # Buffer the lines and join to dots
       #   bfs_2 <- rbind(line_2 %>%
       #                    sf::st_buffer(dist = 1/6)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "fne" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(0, 1/2),
       #                     y = c(1/2, 0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create buffers
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Points for lines
       #   pts <- data.frame(x = c(1),
       #                     y = c(1))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_1 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_1 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "fsw" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1),
       #                     y = c(1, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create buffers
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Points for lines
       #   pts <- data.frame(x = c(0),
       #                     y = c(0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_2 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_2 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "fse" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(0, 1/2),
       #                     y = c(1/2, 0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create buffers
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Points for lines
       #   pts <- data.frame(x = c(1),
       #                     y = c(1))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_1 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_1 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   # fne
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   
       #   # Rotate and translate to create alternate tiles
       #   tile <- tile %>%
       #     dplyr::mutate(geometry = sf::st_geometry(tile) * matrix(c(cos(pi/2),
       #                                                               sin(pi/2),
       #                                                               -sin(pi/2),
       #                                                               cos(pi/2)),
       #                                                             nrow = 2,
       #                                                             ncol = 2) + c(0, 1))
       #   
       #   ## ADORNMENTS DONE
       # },
       # 
       # "fnw" = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1),
       #                     y = c(1, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create buffers
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Points for lines
       #   pts <- data.frame(x = c(0),
       #                     y = c(0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_2 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_2 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   
       #   # Rotate and translate to create alternate tiles
       #   # fnw
       #   tile <- tile %>%
       #     dplyr::mutate(geometry = sf::st_geometry(tile) * matrix(c(cos(pi/2),
       #                                                               sin(pi/2),
       #                                                               -sin(pi/2),
       #                                                               cos(pi/2)),
       #                                                             nrow = 2,
       #                                                             ncol = 2) + c(0, 1))
       #   
       #   ## ADORNMENTS DONE
       # },
       # 
       # "+" = {
       #   ## ADORNMENTS
       #   # Make lines
       #   line_1 <- matrix(c(1/2, 1/2,
       #                      0, 1),
       #                    nrow = 2,
       #                    byrow = FALSE)
       #   
       #   line_2 <- matrix(c(0, 1,
       #                      1/2, 1/2),
       #                    nrow = 2,
       #                    byrow = FALSE)
       #   
       #   # Convert coordinates to lines and then to simple features
       #   line_1 <- data.frame(id = "line_1",
       #                        r = 1/6,
       #                        geometry = sf::st_linestring(line_1) %>%
       #                          sf::st_sfc()) %>%
       #     sf::st_as_sf() %>%
       #     sf::st_set_agr("constant")
       #   
       #   # Convert coordinates to lines and then to simple features
       #   line_2 <- data.frame(id = "line_2",
       #                        r = 1/6,
       #                        geometry = sf::st_linestring(line_2) %>%
       #                          sf::st_sfc()) %>%
       #     sf::st_as_sf() %>%
       #     sf::st_set_agr("constant")
       #   
       #   # Buffer the lines and join to dots
       #   bfs_1 <- line_1 %>%
       #     sf::st_union(line_2 %>%
       #                    sf::st_geometry()) %>%
       #     sf::st_buffer(dist = 1/6)
       #   
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant"),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "+." = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1/2, 0, 1),
       #                     y = c(1, 0, 1/2, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create first set of buffers and cast to polygons
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant"),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "x." = {
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1/2, 0, 1),
       #                     y = c(1, 0, 1/2, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create first set of buffers and cast to polygons
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Assemble tile
       #   tile <- data.frame(color = 2,
       #                      sf::st_geometry(rbind(tile %>%
       #                                              dplyr::select(-.data$color),
       #                                            bfs_1) %>%
       #                                        sf::st_union())) %>%
       #     sf::st_as_sf()
       #   
       #   # Second set of points
       #   pts_2 <- data.frame(x = c(0, 0, 1, 1),
       #                       y = c(0, 1, 1, 0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts_2 <- pts_2 %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts_2) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/3)
       #   
       #   # Create second set of buffers and cast to polygons
       #   bfs_2 <- pts_2 %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts_2 %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Assign constant geometry
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 1))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "tn" = {
       #   ## ADORNMENTS
       #   
       #   #  Define square polygon
       #   pol_1 <- matrix(c(0, 1/3,
       #                     0, 1,
       #                     1, 1,
       #                     1, 1/3,
       #                     0, 1/3),
       #                   ncol = 2,
       #                   byrow = TRUE)
       #   
       #   # Convert coordinates to polygons and then to simple features
       #   pol_1 <- data.frame(geometry = sf::st_polygon(list(pol_1)) %>%
       #                         sf::st_sfc()) %>%
       #     sf::st_as_sf()
       #   
       #   # Points
       #   pts <- data.frame(x = c(1/2, 1/2, 0, 1),
       #                     y = c(1, 0, 1/2, 1/2))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/6)
       #   
       #   # Create first set of buffers and cast to polygons
       #   bfs_1 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Assemble tile
       #   bfs_1 <- data.frame(sf::st_geometry(rbind(pol_1,
       #                                             bfs_1) %>%
       #                                         sf::st_union())) %>%
       #     sf::st_as_sf() %>%
       #     sf::st_set_agr("constant")
       #   
       #   # Second set of points
       #   pts_2 <- data.frame(x = c(0, 1),
       #                       y = c(1, 1))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts_2 <- pts_2 %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Assign constant geometry
       #   sf::st_agr(pts_2) <- "constant"
       #   
       #   # Circle segments
       #   cs <- c(1/3)
       #   
       #   # Create second set of buffers and cast to polygons
       #   bfs_2 <- pts_2 %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts_2 %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Assign constant geometry
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Difference between first set of buffers and second set of buffers
       #   bfs_1 <- bfs_1 %>%
       #     sf::st_difference(bfs_2 %>%
       #                         sf::st_union())
       #   
       #   # Bind TILE with first set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_1),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2))
       #   
       #   ## ADORNMENTS DONE
       # },
       # 
       # "ane" = {
       #   # THESE TILES ARE AN ABERRRATION, THEY DO NOT FOLLOW THE PROPER RULES, BUT PRODUCE SOMETHING INTERESTING NONETHELESS
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(1),
       #                     y = c(1))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_1 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   line_2 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_1 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       # },
       # 
       # "asw" = {
       #   # THESE TILES ARE AN ABERRRATION, THEY DO NOT FOLLOW THE PROPER RULES, BUT PRODUCE SOMETHING INTERESTING NONETHELESS
       #   ## ADORNMENTS
       #   # Points
       #   pts <- data.frame(x = c(0),
       #                     y = c(0))
       #   
       #   # Convert coordinates to points and then to simple features
       #   pts <- pts %>%
       #     sf::st_as_sf(coords = c("x", "y"))
       #   
       #   # Circle segments
       #   cs <- c(1/2)
       #   
       #   # Create buffers and cast to lines
       #   bfs_2 <- pts %>%
       #     dplyr::mutate(r = cs[1],
       #                   geometry = pts %>%
       #                     sf::st_buffer(dist = .data$r) %>%
       #                     dplyr::pull(.data$geometry)) %>%
       #     sf::st_set_agr("constant") %>%
       #     sf::st_cast(to = "LINESTRING") %>%
       #     dplyr::select(-.data$r)
       #   
       #   # Intersect lines with tile
       #   line_2 <- bfs_2 %>%
       #     sf::st_intersection(sf::st_geometry(tile))
       #   
       #   # Buffer the lines
       #   bfs_2 <- rbind(line_2 %>%
       #                    sf::st_buffer(dist = 1/6))
       #   
       #   # Set geometry to constant
       #   sf::st_agr(tile) <- "constant"
       #   sf::st_agr(bfs_1) <- "constant"
       #   sf::st_agr(bfs_2) <- "constant"
       #   
       #   # Bind BASE TILE with second set of buffers
       #   tile <- rbind(tile %>%
       #                   sf::st_difference(bfs_1 %>%
       #                                       sf::st_union()) %>%
       #                   sf::st_set_agr("constant") %>%
       #                   sf::st_difference(bfs_2 %>%
       #                                       sf::st_union()),
       #                 bfs_1 %>%
       #                   dplyr::transmute(color = 2),
       #                 bfs_2 %>%
       #                   dplyr::transmute(color = 2))
       #   ## ADORNMENTS DONE
       }
)


# Create a data frame with the spots for tiles
container <- expand.grid(x = seq(xlim[1], xlim[2], 1),
                         y = seq(ylim[1], ylim[2], 1)) %>%
  mutate(tiles = case_when(x <= 3 ~ "dl", 
                           x > 3 ~ "dr"),
         scale_p = 1)
min(web_digitiser_truchet340_multi_polygon$x)

container_AJ <- web_digitiser_truchet340_multi_polygon %>%
  mutate(tiles = case_when(x <= 0.5 ~ "dl", 
                           x > 0.5 ~ "dr"),
         scale_p = 1)

mosaic_AJ <- st_truchet_ms(df = container_AJ)

ggplot() +
  geom_sf(data = xys_AJ_polys,
          aes(fill = group),
          color = NA)

# reindernijhoff.net

web_digitiser_truchet340 <- read.csv('./00_raw_data/web_digitiser_truchet340.csv') %>%
  #select(2:3) %>% 
  rename(lon = x, lat = y)

p_truchet340 <- web_digitiser_truchet340 %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'black')

p_truchet340_polygon <- web_digitiser_truchet340 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise() %>% 
  st_convex_hull()
  # summarise(geometry = st_combine(geometry)) %>%
  # st_cast("POLYGON")

#https://stackoverflow.com/questions/50303438/points-in-multiple-polygons-using-r
web_digitiser_truchet340_multi_polygon <- read.csv('./00_raw_data/web_digitiser_truchet340_multi_polygon_closed.csv') 

#https://stackoverflow.com/questions/67001602/can-you-create-multiple-polygons-in-r-from-a-dataframe-containing-the-vertices
x <- sfheaders::sf_polygon(web_digitiser_truchet340_multi_polygon, x = "x", y = "y", polygon_id = "group", close = TRUE) 

demo(nc, ask = FALSE, echo = FALSE)
plot(st_geometry(x))


set.seed(999)
xy = data.frame(x=runif(24), y=runif(24))
xy$ID = rep(1:6, each = 4)
head(xy)

xys = st_as_sf(xy, coords=c("x","y"))

polys = xys %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")

plot(polys)

xys_AJ <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y"))

class(xypoly_AJ)

xymp_AJ <- st_sf(
  aggregate(
    xys_AJ,
    by=list(ID=xys_AJ$group),
    do_union=FALSE,
    FUN=function(vals){vals[1]}))

xypoly_AJ = st_cast(xymp_AJ, 'POLYGON')

plot(xypoly_AJ['group'])

xys_AJ_polys <- xys_AJ %>% 
  dplyr::group_by(group) %>% 
  dplyr::summarise() %>%
  st_cast("POLYGON")

plot(xychull_AJ)

xychull_AJ <- xymp_AJ
st_geometry(xychull_AJ) <- st_convex_hull(xymp_AJ$geometry)
plot(xychull_AJ['group'])

multi_poly <- web_digitiser_truchet340_multi_polygon %>% split(web_digitiser_truchet340_multi_polygon$group) %>% 
  lapply(function(x) rbind(x,x[1,])) %>% 
  lapply(function(x) x[,1:2]) %>% 
  lapply(function(x) list(as.matrix(x))) %>%
  lapply(function(x) st_polygon(x)) 

points <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c('x','y'),remove = F)

polys <- multi_poly %>% st_sfc() %>% st_sf(geom=.) %>% 
  mutate(id=factor(1:11))

joins <- polys  %>% st_intersection(points)

ggplot() + geom_sf(data=polys) +
  geom_sf(data=joins,aes(col=id)) 


grid_mclaren_simple <- read.csv('./00_raw_data/mclaren_logo_dots.csv') %>% 
  select(2:3) %>% 
  rename(lon = x, lat = y)

p_mclaren <- grid_mclaren_simple %>% 
  ggplot(aes(lon, lat)) + 
  geom_point(color = 'black')

p_mclaren_polygon <- grid_mclaren_simple %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")



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
# https://happygitwithr.com/rstudio-git-github

# misc----
outline_AJ <- xychull_AJ %>% 
  summarise() %>% 
  concaveman::concaveman(concavity = 1) %>% 
  st_coordinates() %>% # retrieves coordinates in a matrix
  as.data.frame %>%    # converts into dataframe
  split(.,.$L1) %>%    # creates a list with one coordinates table for each feature
  lapply(., `select`, c("X", "Y")) # keeps only X and Y columns for each table of the list

write.csv(outline_AJ, './00_raw_data/outline_truchet340_multi_polygon_closed.csv')

plot(outline_AJ)

#x_and_y <- outline_AJ %>% fortify() %>% select(all_of(lat), all_of(lon))

glimpse(outline_AJ)

pts_AJ <- data.frame(x = c(0.3358,0.3199,0.2961,0.2744,0.2549,0.2173,0.1858,0.172,-0.0028,0.0158,
                           0.0315,0.0428,0.1017,0.1336,0.1228,0.0943,0.0795,0.0521,0.0378,-3.00E-04,
                           0.2253,0.2272,0.2325,0.2429,0.254,0.2696,0.2913,0.3081,0.3347,0.3676,
                           0.4993,0.6391,0.676,0.682,0.6837,0.6895,0.6908,0.6901,0.6854,0.6781,
                           0.6714,0.7153,0.7482,0.7811,0.8469,0.8799,0.9457,0.998,1,0.975,
                           0.8024,0.7981,0.7885,0.773,0.7555,0.7393,0.7212,0.7027,0.6782,0.6471,
                           0.6142,0.4116,0.373,0.3358),
                     y = c(-0.4351,-0.4084,-0.356,-0.3053,-0.2568,-0.1547,-0.0633,-0.0177,-0.0071,0.014,
                           0.0074,0.0372,0.086,0.1065,0.1385,0.2362,0.2888,0.3881,0.4467,0.6043,
                           0.5889,0.6354,0.7124,0.7767,0.839,0.8912,0.9426,0.9808,0.9971,0.9981,
                           0.999,0.9997,0.9984,0.9594,0.9208,0.7899,0.6435,0.5889,0.5255,0.4636,
                           0.4272,0.4471,0.4601,0.4722,0.4943,0.5041,0.52,0.5292,0.0441,0.0248,
                           -0.0832,-0.1343,-0.183,-0.2368,-0.2812,-0.3208,-0.3569,-0.3932,-0.4332,-0.4352,
                           -0.4357,-0.437,-0.4367,-0.4351))

# Convert coordinates to points and then to simple features
pts_AJ <- pts_AJ %>%
  sf::st_as_sf(coords = c("x", "y"))

plot(pts_AJ)

# Assign constant geometry
sf::st_agr(pts_AJ) <- "constant"

# Create first set of buffers and cast to polygons
bfs_1_AJ <- pts_AJ %>%
  dplyr::mutate(r = cs[1],
                geometry = pts_AJ %>%
                  sf::st_buffer(dist = .data$r) %>%
                  dplyr::pull(.data$geometry)) %>%
  dplyr::select(-.data$r)

# Assemble base tile
tile_AJ <- data.frame(color = 1,
                      sf::st_geometry(rbind(tile,
                                            bfs_1_AJ) %>%
                                        sf::st_union())) %>%
  sf::st_as_sf()

plot(tile)
plot(xys_AJ)
plot(xymp_AJ)
plot(xypoly_AJ)
plot(xychull_AJ['ID'])

glimpse(tile)
glimpse(xychull_AJ)

class(tile)
class(xys_AJ)





