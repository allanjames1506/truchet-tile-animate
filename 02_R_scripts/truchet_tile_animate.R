
# https://www.swyx.io/solve-git-bad-object-head

# 1. LIBRARIES----

#https://paezha.github.io/truchet/articles/a03-figurative-mosaics.html

options(timeout = max(300, getOption("timeout")))
install.packages("devtools")
devtools::install_github("paezha/truchet")

library(dplyr)
library(gganimate)
library(ggplot2)
library(imager)
library(lwgeom)
library(purrr)
library(sf)
library(magick)
library(showtext)
#library(sfheaders)
library(truchet)
# library(terra)
# library(tmap)
# library(magick)
library(here)
library(ggimage)
library(data.table)
# library(patchwork)
# library(spatialEco)
library(darklyplot)
library(ggfx)
library(tidyr)

# 2. Fonts----

font_families_google()
font_add_google("Zen Dots", "zen")

showtext_auto()
showtext_opts(dpi = 300)

# xlim <- c(0, 7)
# ylim <- c(0, 7)
# 
# # Create a data frame with the spots for tiles
# container <- expand.grid(x = seq(xlim[1], xlim[2], 1),
#                          y = seq(ylim[1], ylim[2], 1)) %>%
#   mutate(tiles = case_when(x <= 3 ~ "dl", 
#                            x > 3 ~ "dr"),
#          scale_p = 1)
# 
# mosaic <- st_truchet_ms(df = container)
# 
# ggplot() +
#   geom_sf(data = mosaic,
#           aes(fill = color),
#           color = NA)
# 
# #https://stackoverflow.com/questions/50303438/points-in-multiple-polygons-using-r
# 
# web_digitiser_truchet340_multi_polygon <- read.csv('./00_raw_data/web_digitiser_truchet340_multi_polygon_closed.csv')
# 
# max(web_digitiser_truchet340_multi_polygon$y)
# 
# web_digitiser_truchet340_multi_polygon[,c(1,2)] <-lapply(web_digitiser_truchet340_multi_polygon[,c(1,2)], scales::rescale, to=c(0,1))
# 
# # https://gis.stackexchange.com/questions/332427/converting-points-to-polygons-by-group
# 
# xys_AJ <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y"))
# 
# # st_truchet_fm(
# #   df = web_digitiser_truchet340_multi_polygon
# # ) %>%
# #   ggplot() +
# #   geom_sf(aes(fill = factor(group)))
# 
# xymp_AJ <- st_sf(
#   aggregate(
#     xys_AJ,
#     by=list(ID=xys_AJ$group),
#     do_union=TRUE,
#     FUN=function(vals){vals[1]}))
# 
# xychull_AJ <- xymp_AJ
# 
# st_geometry(xychull_AJ) <- st_convex_hull(xymp_AJ$geometry)
# 
# plot(xychull_AJ['group'])
# 
# class(xychull_AJ)
# 
# glimpse(xys_AJ)
# 
# xychull_AJ_mclaren_colours <-  xychull_AJ %>% 
#   mutate(group = case_when(group %in% 'A' ~ '1',
#                            group %in% 'B' ~ '2',
#                            group %in% 'C' ~ '3',
#                            group %in% 'D' ~ '4',
#                            group %in% 'E' ~ '5',
#                            group %in% 'F' ~ '6',
#                            group %in% 'G' ~ '7',
#                            group %in% 'H' ~ '8',
#                            group %in% 'I' ~ '9',
#                            group %in% 'J' ~ '10',
#                            group %in% 'K' ~ '11',
#                            TRUE ~ group)) %>%
#   mutate(group = as.numeric(group)) %>% 
#   ggplot(aes(fill = group)) +
#   geom_sf(color = NA, show.legend = FALSE) + 
#   scale_fill_gradientn(colours = c("#FF8000", "#ffffff")) + 
#   theme_void()
# 
# xychull_AJ_mclaren_colours
# 
# ggsave('./03_plots/mcL_polygon1.png', dpi = 350, height = 4, width = 4, units = 'in')


# https://github.com/paezha/truchet/blob/5f8c93c1c316288612aa6280244cf176e7eed18e/R/st_truchet_p.R

#2. CREATE BASE TILE----
#  Define square polygon
# tile <- matrix(c(0, 0,
#                  0, 1,
#                  1, 1,
#                  1, 0,
#                  0, 0),
#                ncol = 2,
#                byrow = TRUE)
# 
# # Convert coordinates to polygons and then to simple features
# tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
#                      sf::st_sfc()) %>%
#   sf::st_as_sf()
# 
# # Points for base tile
# pts <- data.frame(x = c(0, 0, 1, 1),
#                   y = c(0, 1, 1, 0))
# 
# # Convert coordinates to points and then to simple features
# pts <- pts %>%
#   sf::st_as_sf(coords = c("x", "y"))
# 
# # Assign constant geometry
# sf::st_agr(pts) <- "constant"
# 
# # Obtain points geometry
# pts_gmtry <- pts %>%
#     dplyr::mutate(geometry = pts %>%
#                     dplyr::pull(.data$geometry))
# 
# # Assemble base tile
# tile <- data.frame(colour = 1,
#                    sf::st_geometry(rbind(tile,
#                                          pts_gmtry) %>%
#                                      sf::st_union())) %>%
#   sf::st_as_sf()
# 
# plot(tile)


## BASE TILE DONE

# 3. POLYGON MATRICES----
#* 3.1 Polygon A----
polygon_A_as_matrix <- matrix(c(0.336886377,	1,
                                0.369760292,	1,
                                0.402635141,	1,
                                0.435509989,	1,
                                0.468384618,	1,
                                0.501259467,	1,
                                0.534134789,	1,
                                0.567010254,	1,
                                0.596658417,	1,
                                0.627516473,	1,
                                0.656727742,	1,
                                0.681170078,	1,
                                0.684035182,	0.968485348,
                                0.684857423,	0.943990054,
                                0.686669314,	0.916638769,
                                0.689944431,	0.884162868,
                                0.691272995,	0.848688423,
                                0.691325702,	0.816009919,
                                0.691378812,	0.783081961,
                                0.691779261,	0.755391967,
                                0.691880102,	0.724955875,
                                0.689561187,	0.691259253,
                                0.686834089,	0.659633539,
                                0.680380304,	0.627892088,
                                0.672105957,	0.594016593,
                                0.633420536,	0.584859998,
                                0.600562806,	0.574299964,
                                0.567705953,	0.563195667,
                                0.534849978,	0.551547105,
                                0.501993784,	0.54003461,
                                0.469139126,	0.527569653,
                                0.436284249,	0.515240762,
                                0.403432224,	0.501143013,
                                0.370578663,	0.487997726,
                                0.337727437,	0.473404849,
                                0.295631731,	0.455095902,
                                0.269896178,	0.469962147,
                                0.252440206,	0.483840961,
                                0.232272074,	0.497274248,
                                0.230242612,	0.540467147,
                                0.228173521,	0.588009214,
                                0.226891993,	0.632556566,
                                0.22681247,	0.681861392,
                                0.22970257,	0.742940094,
                                0.234995462,	0.796641166,
                                0.24538338,	0.841426247,
                                0.256371173,	0.884877088,
                                0.272002668,	0.921291539,
                                0.293612472,	0.957036586,
                                0.312437553,	0.984015266,
                                0.336886377,	1),
                              ncol = 2,
                              byrow = TRUE)

#* 3.2 Polygon B----
polygon_B_as_matrix <- matrix(c(0,	0.709136032,
                                0.012189958,	0.688994838,
                                0.023450981,	0.663042977,
                                0.031588642,	0.643094386,
                                0.041469699,	0.612026031,
                                0.054979712,	0.57111235,
                                0.069979041,	0.53623391,
                                0.082321361,	0.501642873,
                                0.097157655,	0.464905277,
                                0.111990655,	0.430209643,
                                0.125659708,	0.396573361,
                                0.134869287,	0.377811188,
                                0.163405541,	0.391216143,
                                0.193835529,	0.406976508,
                                0.218672726,	0.419679034,
                                0.240905742,	0.428552548,
                                0.236987142,	0.452976445,
                                0.23470995,	0.476599814,
                                0.232272074,	0.497274248,
                                0.205012099,	0.514267766,
                                0.18099414,	0.530402263,
                                0.159848534,	0.548431638,
                                0.135262981,	0.571503489,
                                0.106739242,	0.597850931,
                                0.080058254,	0.625357685,
                                0.057012806,	0.648829297,
                                0.037362112,	0.670494608,
                                0.024693899,	0.686551375,
                                0,	0.709136032),
                              ncol = 2,
                              byrow = TRUE)

#* 3.3 Polygon C----
polygon_C_as_matrix <- matrix(c(0.232272074,	0.497274248,
                                0.23470995,	0.476599814,
                                0.236987142,	0.452976445,
                                0.240905742,	0.428552548,
                                0.267092883,	0.443298901,
                                0.295631731,	0.455095902,
                                0.269896178,	0.469962147,
                                0.252440206,	0.483840961,
                                0.232272074,	0.497274248),
                              ncol = 2,
                              byrow = TRUE)

#* 3.4 Polygon D----
polygon_D_as_matrix <- matrix(c(0.295631731,	0.455095902,
                                0.337727437,	0.473404849,
                                0.370578663,	0.487997726,
                                0.403432224,	0.501143013,
                                0.436284249,	0.515240762,
                                0.469139126,	0.527569653,
                                0.501993784,	0.54003461,
                                0.534849978,	0.551547105,
                                0.567705953,	0.563195667,
                                0.600562806,	0.574299964,
                                0.633420536,	0.584859998,
                                0.672105957,	0.594016593,
                                0.662004479,	0.54998103,
                                0.648473892,	0.516485399,
                                0.633315268,	0.481677098,
                                0.611557125,	0.448998553,
                                0.581566474,	0.425554811,
                                0.550271562,	0.406573624,
                                0.514995881,	0.390499121,
                                0.481318961,	0.387836535,
                                0.451265398,	0.388692841,
                                0.426778823,	0.393898572,
                                0.404764201,	0.401420417,
                                0.370696095,	0.415190352,
                                0.337798325,	0.429454794,
                                0.313676589,	0.443754624,
                                0.295631731,	0.455095902),
                              ncol = 2,
                              byrow = TRUE)

#* 3.5 Polygon E----
polygon_E_as_matrix <- matrix(c(0.672105957,	0.594016593,
                                0.717062246,	0.609352746,
                                0.74992261,	0.618279988,
                                0.782784072,	0.626526901,
                                0.81564685,	0.633957419,
                                0.848509189,	0.641660068,
                                0.881373064,	0.648410255,
                                0.914239354,	0.653663717,
                                0.947105204,	0.659189311,
                                0.977184192,	0.664628697,
                                1,	0.665398154,
                                1,	0.628837706,
                                1,	0.590255452,
                                1,	0.557327494,
                                1,	0.524399536,
                                1,	0.491471578,
                                1,	0.45854362,
                                1,	0.425615661,
                                1,	0.392687703,
                                1,	0.359759745,
                                1,	0.326831979,
                                0.976938915,	0.316700746,
                                0.947680851,	0.302288344,
                                0.914829485,	0.287782398,
                                0.881977899,	0.273412518,
                                0.85396025,	0.262002354,
                                0.826420399,	0.248474842,
                                0.803641549,	0.239008648,
                                0.801412984,	0.26777781,
                                0.790790909,	0.294640926,
                                0.765337126,	0.325986203,
                                0.732426359,	0.348308772,
                                0.699527955,	0.362966293,
                                0.666639061,	0.371727623,
                                0.633753459,	0.378447964,
                                0.600620323,	0.384227584,
                                0.573561868,	0.388411039,
                                0.547775218,	0.390839532,
                                0.514995881,	0.390499121,
                                0.550271562,	0.406573624,
                                0.581566474,	0.425554811,
                                0.611557125,	0.448998553,
                                0.633315268,	0.481677098,
                                0.648473892,	0.516485399,
                                0.662004479,	0.54998103,
                                0.672105957,	0.594016593),
                              ncol = 2,
                              byrow = TRUE)

#* 3.6 Polygon F----
polygon_F_as_matrix <- matrix(c(0,	0.295324516,
                                0.03173759,	0.304757234,
                                0.061984235,	0.313149587,
                                0.089085526,	0.322566662,
                                0.118720928,	0.330970024,
                                0.151866048,	0.339819271,
                                0.144484345,	0.357651487,
                                0.134869287,	0.377811188,
                                0.109895278,	0.361696445,
                                0.083305105,	0.347603699,
                                0.060343974,	0.33350509,
                                0.037784767,	0.320213586,
                                0,	0.295324516),
                              ncol = 2,
                              byrow = TRUE)

#* 3.7 Polygon G----
polygon_G_as_matrix <- matrix(c(0.134869287,	0.377811188,
                                0.163405541,	0.391216143,
                                0.193835529,	0.406976508,
                                0.218672726,	0.419679034,
                                0.240905742,	0.428552548,
                                0.24450009,	0.406713239,
                                0.247762384,	0.384090844,
                                0.249738031,	0.359189809,
                                0.21287532,	0.352305697,
                                0.17986824,	0.34610693,
                                0.151866048,	0.339819271,
                                0.144484345,	0.357651487,
                                0.134869287,	0.377811188),
                              ncol = 2,
                              byrow = TRUE)

#* 3.8 Polygon H----
polygon_H_as_matrix <- matrix(c(0.240905742,	0.428552548,
                                0.267092883,	0.443298901,
                                0.295631731,	0.455095902,
                                0.313676589,	0.443754624,
                                0.337798325,	0.429454794,
                                0.370696095,	0.415190352,
                                0.404764201,	0.401420417,
                                0.426778823,	0.393898572,
                                0.451265398,	0.388692841,
                                0.408600938,	0.384775368,
                                0.370752306,	0.380339329,
                                0.337885783,	0.375230572,
                                0.305017079,	0.371473835,
                                0.274975529,	0.364882367,
                                0.249738031,	0.359189809,
                                0.247762384,	0.384090844,
                                0.24450009,	0.406713239,
                                0.240905742,	0.428552548),
                              ncol = 2,
                              byrow = TRUE)

#* 3.9 Polygon I----
polygon_I_as_matrix <- matrix(c(0.151866048,	0.339819271,
                                0.17986824,	0.34610693,
                                0.21287532,	0.352305697,
                                0.249738031,	0.359189809,
                                0.258627898,	0.309237217,
                                0.267147241,	0.267885973,
                                0.274619845,	0.225112266,
                                0.282001792,	0.188010751,
                                0.292426746,	0.148068864,
                                0.304110437,	0.110062829,
                                0.315652552,	0.071598766,
                                0.328733481,	0.027598968,
                                0.340465457,	0,
                                0.322330706,	0.013496179,
                                0.300873541,	0.043408914,
                                0.282774799,	0.079335051,
                                0.255995423,	0.120104488,
                                0.23744154,	0.156008678,
                                0.218909198,	0.191515451,
                                0.203640289,	0.225083914,
                                0.18864817,	0.25549158,
                                0.174849111,	0.287379072,
                                0.164003241,	0.314759276,
                                0.151866048,	0.339819271),
                              ncol = 2,
                              byrow = TRUE)

#* 3.10 Polygon J----
polygon_J_as_matrix <- matrix(c(0.249738031,	0.359189809,
                                0.274975529,	0.364882367,
                                0.305017079,	0.371473835,
                                0.337885783,	0.375230572,
                                0.370752306,	0.380339329,
                                0.408600938,	0.384775368,
                                0.451265398,	0.388692841,
                                0.481318961,	0.387836535,
                                0.514995881,	0.390499121,
                                0.547775218,	0.390839532,
                                0.573561868, 0.388411039,
                                0.600620323,	0.384227584,
                                0.633753459,	0.378447964,
                                0.666639061,	0.371727623,
                                0.699527955,	0.362966293,
                                0.732426359,	0.348308772,
                                0.765337126,	0.325986203,
                                0.790790909,	0.294640926,
                                0.801412984,	0.26777781,
                                0.803641549,	0.239008648,
                                0.780440391,	0.226667941,
                                0.747587543,	0.21308044,
                                0.714740566,	0.195853175,
                                0.681890956,	0.180258701,
                                0.649042004,	0.164256029,
                                0.61619393,	0.147709094,
                                0.583346514,	0.130753961,
                                0.550500195,	0.113118498,
                                0.517654535,	0.095074837,
                                0.484807119,	0.078119704,
                                0.45196124,	0.060212109,
                                0.419116238,	0.041760251,
                                0.389092347,	0.024219578,
                                0.362878115,	0.01233783,
                                0.340465457,	0,
                                0.328733481,	0.027598968,
                                0.315652552,	0.071598766,
                                0.304110437,	0.110062829,
                                0.292426746,	0.148068864,
                                0.282001792,	0.188010751,
                                0.274619845,	0.225112266,
                                0.267147241,	0.267885973,
                                0.258627898,	0.309237217,
                                0.249738031,	0.359189809),
                              ncol = 2,
                              byrow = TRUE)

#* 3.11 Polygon K----
polygon_K_as_matrix <- matrix(c(0.340465457,	0,
                                0.362878115,	0.01233783,
                                0.389092347,	0.024219578,
                                0.419116238,	0.041760251,
                                0.45196124,	0.060212109,
                                0.484807119,	0.078119704,
                                0.517654535,	0.095074837,
                                0.550500195,	0.113118498,
                                0.583346514,	0.130753961,
                                0.61619393,	0.147709094,
                                0.649042004,	0.164256029,
                                0.681890956,	0.180258701,
                                0.714740566,	0.195853175,
                                0.747587543,	0.21308044,
                                0.780440391,	0.226667941,
                                0.803641549,	0.239008648,
                                0.799904276,	0.203176522,
                                0.790395403,	0.169266197,
                                0.774948258,	0.131789964,
                                0.755468252,	0.094009023,
                                0.735685864,	0.062709612,
                                0.717520477,	0.037014356,
                                0.69997724,	0.015291651,
                                0.679509286,	0,
                                0.649314853,	0,
                                0.61644236,	0,
                                0.583567511,	0,
                                0.550692663,	0,
                                0.517817814,	0,
                                0.484942966,	0,
                                0.452068117,	0,
                                0.419193269,	0,
                                0.392341035,	0,
                                0.364517079,	0,
                                0.340465457,	0),
                              ncol = 2,
                              byrow = TRUE)

# 4. MULTI POLYGON DATAFRAMES----

#https://stackoverflow.com/questions/50303438/points-in-multiple-polygons-using-r
#https://stackoverflow.com/questions/67001602/can-you-create-multiple-polygons-in-r-from-a-dataframe-containing-the-vertices

#*4.1 multi-polygon data input----
web_digitiser_truchet340_multi_polygon <- read.csv('./00_raw_data/web_digitiser_truchet340_multi_polygon_closed2_neat.csv') 

#*4.2 Multi-polygon AK----

polygon_AK_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y")) %>% 
  mutate(colour = case_when(group %in% 'A' ~ 2,
                            group %in% 'B' ~ 3,
                            group %in% 'C' ~ 4,
                            group %in% 'D' ~ 5,
                            group %in% 'E' ~ 6,
                            group %in% 'F' ~ 7,
                            group %in% 'G' ~ 8,
                            group %in% 'H' ~ 9,
                            group %in% 'I' ~ 10,
                            group %in% 'J' ~ 11,
                            group %in% 'K' ~ 12,
                            TRUE ~ NA)) %>% 
  select(-group)

#*4.3 Multi-polygon AK+----
# rotate original polygon AK 90 degrees

# Rotating by 90 degrees:
# https://www.khanacademy.org/math/geometry/hs-geo-transformations/hs-geo-rotations/a/rotating-shapes#:~:text=Rotating%20by%2090%20degrees%3A&text=So%20from%200%20degrees%20you,Same%20thing.&text=X%20and%20Y%20swaps%2C%20and%20Y%20becomes%20negative.
# When you rotate by 90 degrees, you take your original X and Y, swap them, and make Y negative
# So from 0 degrees you take (x, y), swap them, and make y negative (-y, x) and then you have made a 90 degree rotation

web_digitiser_truchet340_multi_polygon_plus <- web_digitiser_truchet340_multi_polygon 

web_digitiser_truchet340_multi_polygon_plus$x_new = web_digitiser_truchet340_multi_polygon_plus$y

web_digitiser_truchet340_multi_polygon_plus$y_new = web_digitiser_truchet340_multi_polygon_plus$x

web_digitiser_truchet340_multi_polygon_plus <- web_digitiser_truchet340_multi_polygon_plus %>% 
  select(x_new, y_new, group)

# rename columns
colnames(web_digitiser_truchet340_multi_polygon_plus) <- c('x', 'y', 'group')

# make y negative
web_digitiser_truchet340_multi_polygon_plus$y_neg = web_digitiser_truchet340_multi_polygon_plus$y*(-1)

web_digitiser_truchet340_multi_polygon_plus <- web_digitiser_truchet340_multi_polygon_plus %>% 
  select(x, y_neg, group)

colnames(web_digitiser_truchet340_multi_polygon_plus) <- c('x', 'y', 'group')

min(web_digitiser_truchet340_multi_polygon_plus$x)

web_digitiser_truchet340_multi_polygon_plus[,c(1,2)] <-lapply(web_digitiser_truchet340_multi_polygon_plus[,c(1,2)], scales::rescale, to=c(0,1))

polygon_AK_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus, coords=c("x","y")) %>% 
  mutate(colour = case_when(group %in% 'A' ~ 2,
                            group %in% 'B' ~ 3,
                            group %in% 'C' ~ 4,
                            group %in% 'D' ~ 5,
                            group %in% 'E' ~ 6,
                            group %in% 'F' ~ 7,
                            group %in% 'G' ~ 8,
                            group %in% 'H' ~ 9,
                            group %in% 'I' ~ 10,
                            group %in% 'J' ~ 11,
                            group %in% 'K' ~ 12,
                            TRUE ~ NA)) %>% 
  select(-group)

#*4.4 Multi-polygon AK++----
# rotate polygon AK+ 90 degrees to give AK++

web_digitiser_truchet340_multi_polygon_plus_plus <- web_digitiser_truchet340_multi_polygon_plus 

web_digitiser_truchet340_multi_polygon_plus_plus$x_new = web_digitiser_truchet340_multi_polygon_plus_plus$y

web_digitiser_truchet340_multi_polygon_plus_plus$y_new = web_digitiser_truchet340_multi_polygon_plus_plus$x

web_digitiser_truchet340_multi_polygon_plus_plus <- web_digitiser_truchet340_multi_polygon_plus_plus %>% 
  select(x_new, y_new, group)

# rename columns
colnames(web_digitiser_truchet340_multi_polygon_plus_plus) <- c('x', 'y', 'group')

# make y negative
web_digitiser_truchet340_multi_polygon_plus_plus$y_neg = web_digitiser_truchet340_multi_polygon_plus_plus$y*(-1)

web_digitiser_truchet340_multi_polygon_plus_plus <- web_digitiser_truchet340_multi_polygon_plus_plus %>% 
  select(x, y_neg, group)

colnames(web_digitiser_truchet340_multi_polygon_plus_plus) <- c('x', 'y', 'group')

#min(web_digitiser_truchet340_multi_polygon_plus$x)
# rescale x and y to between 0 and 1
web_digitiser_truchet340_multi_polygon_plus_plus[,c(1,2)] <-lapply(web_digitiser_truchet340_multi_polygon_plus_plus[,c(1,2)], scales::rescale, to=c(0,1))

polygon_AK_plus_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus_plus, coords=c("x","y")) %>% 
  mutate(colour = case_when(group %in% 'A' ~ 2,
                            group %in% 'B' ~ 3,
                            group %in% 'C' ~ 4,
                            group %in% 'D' ~ 5,
                            group %in% 'E' ~ 6,
                            group %in% 'F' ~ 7,
                            group %in% 'G' ~ 8,
                            group %in% 'H' ~ 9,
                            group %in% 'I' ~ 10,
                            group %in% 'J' ~ 11,
                            group %in% 'K' ~ 12,
                            TRUE ~ NA)) %>% 
  select(-group)

#*4.5 Multi-polygon AK-----
# rotate polygon AK++ 90 degrees to give AK-

web_digitiser_truchet340_multi_polygon_neg <- web_digitiser_truchet340_multi_polygon_plus_plus 

web_digitiser_truchet340_multi_polygon_neg$x_new = web_digitiser_truchet340_multi_polygon_neg$y

web_digitiser_truchet340_multi_polygon_neg$y_new = web_digitiser_truchet340_multi_polygon_neg$x

web_digitiser_truchet340_multi_polygon_neg <- web_digitiser_truchet340_multi_polygon_neg %>% 
  select(x_new, y_new, group)

# rename columns
colnames(web_digitiser_truchet340_multi_polygon_neg) <- c('x', 'y', 'group')

# make y negative
web_digitiser_truchet340_multi_polygon_neg$y_neg = web_digitiser_truchet340_multi_polygon_neg$y*(-1)

web_digitiser_truchet340_multi_polygon_neg <- web_digitiser_truchet340_multi_polygon_neg %>% 
  select(x, y_neg, group)

colnames(web_digitiser_truchet340_multi_polygon_neg) <- c('x', 'y', 'group')

#min(web_digitiser_truchet340_multi_polygon_plus$x)
# rescale x and y to between 0 and 1
web_digitiser_truchet340_multi_polygon_neg[,c(1,2)] <-lapply(web_digitiser_truchet340_multi_polygon_neg[,c(1,2)], scales::rescale, to=c(0,1))

polygon_AK_neg_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_neg, coords=c("x","y")) %>% 
  mutate(colour = case_when(group %in% 'A' ~ 2,
                            group %in% 'B' ~ 3,
                            group %in% 'C' ~ 4,
                            group %in% 'D' ~ 5,
                            group %in% 'E' ~ 6,
                            group %in% 'F' ~ 7,
                            group %in% 'G' ~ 8,
                            group %in% 'H' ~ 9,
                            group %in% 'I' ~ 10,
                            group %in% 'J' ~ 11,
                            group %in% 'K' ~ 12,
                            TRUE ~ NA)) %>% 
  select(-group)


# 5. SMOOTHED POLYGONS----
# # *5.1 Polygon A----
# polygon_A <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_A_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_A) <- "constant"
# 
# # Smooth the polygon
# polygon_A <- polygon_A %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# # *5.2 Polygon B----
# polygon_B <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_B_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_B) <- "constant"
# 
# # Smooth the polygon
# polygon_B <- polygon_B %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# # *5.3 Polygon C----
# polygon_C <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_C_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_C) <- "constant"
# 
# # Smooth the polygon
# polygon_C <- polygon_C %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# # *5.4 Polygon D----
# polygon_D <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_D_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_D) <- "constant"
# 
# # Smooth the polygon
# polygon_D <- polygon_D %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# # *5.5 Polygon E----
# polygon_E <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_E_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_E) <- "constant"
# 
# # Smooth the polygon
# polygon_E <- polygon_E %>% 
#   st_concave_hull(0.1) %>%
#   smooth(method = "chaikin")
# 
# polygon_F <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_F_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# polygon_G <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_G_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# polygon_H <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_H_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# polygon_I <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_I_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# polygon_J <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_J_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")
# 
# polygon_K <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_K_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() %>% 
#   st_convex_hull() %>%
#   smooth(method = "chaikin")

# *5.12 Multi-Polygon AK----
# 
polygon_AK <- st_sf(
  aggregate(
    polygon_AK_as_matrix,
    by=list(colour = polygon_AK_as_matrix$colour),
    do_union=FALSE,
    FUN=function(vals){vals[1]})) %>%
  select(colour, geometry) %>%
  st_convex_hull()

polygon_AK = st_cast(polygon_AK, 'POLYGON')

plot(polygon_AK)
# 
# ggsave('./03_plots/mcL_polygon2.png', dpi = 350, height = 4, width = 4, units = 'in')
# 
# 
# # *5.13 Multi-Polygon AK+----
# 
# polygon_AK_plus <- st_sf(
#   aggregate(
#     polygon_AK_plus_as_matrix,
#     by=list(colour = polygon_AK_plus_as_matrix$colour),
#     do_union=FALSE,
#     FUN=function(vals){vals[1]})) %>% 
#   select(colour, geometry) %>% 
#   st_convex_hull()
# 
# polygon_AK_plus = st_cast(polygon_AK_plus, 'POLYGON')
# 
# plot(polygon_AK_plus)
# 
# # *5.14 Multi-Polygon AK++----
# 
# polygon_AK_plus_plus <- st_sf(
#   aggregate(
#     polygon_AK_plus_plus_as_matrix,
#     by=list(colour = polygon_AK_plus_plus_as_matrix$colour),
#     do_union=FALSE,
#     FUN=function(vals){vals[1]})) %>% 
#   select(colour, geometry) %>% 
#   st_convex_hull()
# 
# polygon_AK_plus_plus = st_cast(polygon_AK_plus_plus, 'POLYGON')
# 
# plot(polygon_AK_plus_plus)
# 
# # *5.15 Multi-Polygon AKneg----
# 
# polygon_AK_neg <- st_sf(
#   aggregate(
#     polygon_AK_neg_as_matrix,
#     by=list(colour = polygon_AK_neg_as_matrix$colour),
#     do_union=FALSE,
#     FUN=function(vals){vals[1]})) %>% 
#   select(colour, geometry) %>% 
#   st_convex_hull()
# 
# polygon_AK_neg = st_cast(polygon_AK_neg, 'POLYGON')
# 
# plot(polygon_AK_neg)
# 
# # try binding polygons together
# 
# # *5.1x Polygon A----
# polygon_A_x <- data.frame(colour = 2,
#                         geometry = sf::st_polygon(list(polygon_A_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_A_x) <- "constant"
# 
# # Smooth the polygon
# polygon_A_x <- polygon_A_x %>% 
#   st_convex_hull() 
# 
# # *5.2x Polygon B----
# polygon_B_x <- data.frame(colour = 3,
#                         geometry = sf::st_polygon(list(polygon_B_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_B_x) <- "constant"
# 
# # Smooth the polygon
# polygon_B_x <- polygon_B_x %>% 
#   st_convex_hull() 
# 
# # *5.3x Polygon C----
# polygon_C_x <- data.frame(colour = 4,
#                         geometry = sf::st_polygon(list(polygon_C_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_C_x) <- "constant"
# 
# # Smooth the polygon
# polygon_C_x <- polygon_C_x %>% 
#   st_convex_hull() 
# 
# # *5.4x Polygon D----
# polygon_D_x <- data.frame(colour = 5,
#                         geometry = sf::st_polygon(list(polygon_D_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_D_x) <- "constant"
# 
# # Smooth the polygon
# polygon_D_x <- polygon_D_x %>% 
#   st_convex_hull() 
# 
# # *5.5x Polygon E----
# polygon_E_x <- data.frame(colour = 6,
#                         geometry = sf::st_polygon(list(polygon_E_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_E_x) <- "constant"
# 
# # Smooth the polygon
# polygon_E_x <- polygon_E_x %>% 
#   st_concave_hull(0.1) 
# 
# # *5.6x Polygon F----
# polygon_F_x <- data.frame(colour = 7,
#                         geometry = sf::st_polygon(list(polygon_F_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_F_x) <- "constant"
# 
# # Smooth the polygon
# polygon_F_x <- polygon_F_x %>% 
#   st_convex_hull() 
# 
# # *5.7x Polygon G----
# polygon_G_x <- data.frame(colour = 8,
#                         geometry = sf::st_polygon(list(polygon_G_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf()
# 
# # Assign constant geometry
# sf::st_agr(polygon_G_x) <- "constant"
# 
# # Smooth the polygon
# polygon_G_x <- polygon_G_x %>% 
#   st_convex_hull() 
# 
# # *5.8x Polygon H----
# polygon_H_x <- data.frame(colour = 9,
#                         geometry = sf::st_polygon(list(polygon_H_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_H_x) <- "constant"
# 
# # Smooth the polygon
# polygon_H_x <- polygon_H_x %>% 
#   st_convex_hull() 
# 
# # *5.9x Polygon I----
# polygon_I_x <- data.frame(colour = 10,
#                         geometry = sf::st_polygon(list(polygon_I_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_I_x) <- "constant"
# 
# # Smooth the polygon
# polygon_I_x <- polygon_I_x %>% 
#   st_convex_hull() 
# 
# # *5.10x Polygon J----
# polygon_J_x <- data.frame(colour = 11,
#                         geometry = sf::st_polygon(list(polygon_J_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_J_x) <- "constant"
# 
# # Smooth the polygon
# polygon_J_x <- polygon_J_x %>% 
#   st_concave_hull(0.1) 
# 
# # *5.11x Polygon K----
# polygon_K_x <- data.frame(colour = 12,
#                         geometry = sf::st_polygon(list(polygon_K_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf() 
# 
# # Assign constant geometry
# sf::st_agr(polygon_K_x) <- "constant"
# 
# # Smooth the polygon
# polygon_K_x <- polygon_K_x %>% 
#   st_convex_hull() 
# 
# polygon_AK_x <- bind_rows(polygon_A_x,
#                           polygon_B_x,
#                           polygon_C_x,
#                           polygon_D_x,
#                           polygon_E_x,
#                           polygon_F_x,
#                           polygon_G_x,
#                           polygon_H_x,
#                           polygon_I_x,
#                           polygon_J_x,
#                           polygon_K_x)
# 
# polygon_AB <- bind_rows(polygon_A,
#                         polygon_B)
# 
# # 5. UNIONS between POLYGONS and BASE TILE----
# 
# # https://stackoverflow.com/questions/54710574/how-to-do-a-full-union-with-the-r-package-sf
# # https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html
# # https://gist.github.com/mstrimas/ac50a38a7e656a2b3a173f3a6b31a760
# 
output1 <- st_difference(tile, st_union(polygon_AK)) #notice the use of st_union()

#plot(st_geometry(output1), border="red", add=TRUE)

output2 <- st_difference(polygon_AK, st_union(tile)) #notice the order of b and a and st_union()
#plot(st_geometry(op2), border="green", add=TRUE)

output3 <- st_intersection(polygon_AK, tile) #notice the order

#plot(st_geometry(op3), border="blue", add=TRUE)

union <- dplyr::bind_rows(output2, output3) %>%
  select(colour, geometry)

plot(union)

A_as_matrix <- polygon_A_as_matrix

# Convert coordinates to polygon and then to simple features and then smooth
polygon_A <- data.frame(colour = 2,
                        geometry = sf::st_polygon(list(A_as_matrix)) %>% 
                          sf::st_sfc()) %>%
  sf::st_as_sf() 

# Assign constant geometry
sf::st_agr(polygon_A) <- "constant"

# Smooth the polygon
polygon_A <- polygon_A %>% 
  st_convex_hull() 

output1A <- st_difference(tile, st_union(polygon_A))

output2A <- st_intersection(polygon_A, tile)

output3A <- st_difference(polygon_A, tile)

unionA <- dplyr::bind_rows(output3A, output2A) %>% 
  select(colour, geometry)

plot(unionA)

# 6. TRUCHET P----

aj_truchet_p <- function(x = 0, y = 0, type = "A", scale_p = 1){
  
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
  checkmate::assertChoice(type, c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "AK", "AK+", "AK++", "AK-"))

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
           A_as_matrix <- polygon_A_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_A <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(A_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_A) <- "constant"
           
           # Smooth the polygon
           polygon_A <- polygon_A %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon A
           
           output1 <- st_difference(tile, st_union(polygon_A))
           
           output2 <- st_intersection(polygon_A, tile)
           
           output3 <- st_difference(polygon_A, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON A DONE
           
           },
         
         "B" ={
           ## POLYGON B
           B_as_matrix <- polygon_B_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_B <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(B_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_B) <- "constant"
           
           # Smooth the polygon
           polygon_B <- polygon_B %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon B
           
           output1 <- st_difference(tile, st_union(polygon_B))
           
           output2 <- st_intersection(polygon_B, tile)
           
           output3 <- st_difference(polygon_B, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON B DONE
           
           },
         
         "C" ={
           ## POLYGON C
           C_as_matrix <- polygon_C_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_C <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(C_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_C) <- "constant"
           
           # Smooth the polygon
           polygon_C <- polygon_C %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon C
           
           output1 <- st_difference(tile, st_union(polygon_C))
           
           output2 <- st_intersection(polygon_C, tile)
           
           output3 <- st_difference(polygon_C, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON C DONE
           
           },
         
         "D" ={
           ## POLYGON D
           D_as_matrix <- polygon_D_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_D <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(D_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_D) <- "constant"
           
           # Smooth the polygon
           polygon_D <- polygon_D %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon D
           
           output1 <- st_difference(tile, st_union(polygon_D))
           
           output2 <- st_intersection(polygon_D, tile)
           
           output3 <- st_difference(polygon_D, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON D DONE
           
           },
         
         "E" ={
           ## POLYGON E
           E_as_matrix <- polygon_E_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_E <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(E_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_E) <- "constant"
           
           # Smooth the polygon
           polygon_E <- polygon_E %>% 
             st_concave_hull(0.1) 
           
           # Bind BASE TILE with polygon
           # polygon E
           
           output1 <- st_difference(tile, st_union(polygon_E))
           
           output2 <- st_intersection(polygon_E, tile)
           
           output3 <- st_difference(polygon_E, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON E DONE
           
           },
         
         "F" ={
           ## POLYGON F
           F_as_matrix <- polygon_F_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_F <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(F_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_F) <- "constant"
           
           # Smooth the polygon
           polygon_F <- polygon_F %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon F
           
           output1 <- st_difference(tile, st_union(polygon_F))
           
           output2 <- st_intersection(polygon_F, tile)
           
           output3 <- st_difference(polygon_F, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON F DONE
           
         },
         
         "G" ={
           ## POLYGON G
           G_as_matrix <- polygon_G_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_G <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(G_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_G) <- "constant"
           
           # Smooth the polygon
           polygon_G <- polygon_G %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon G
           
           output1 <- st_difference(tile, st_union(polygon_G))
           
           output2 <- st_intersection(polygon_G, tile)
           
           output3 <- st_difference(polygon_G, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON G DONE
           
         },
         
         "H" ={
           ## POLYGON H
           H_as_matrix <- polygon_H_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_H <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(H_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_H) <- "constant"
           
           # Smooth the polygon
           polygon_H <- polygon_H %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon H
           
           output1 <- st_difference(tile, st_union(polygon_H))
           
           output2 <- st_intersection(polygon_H, tile)
           
           output3 <- st_difference(polygon_H, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON H DONE
           
         },
         
         "I" ={
           ## POLYGON F
           I_as_matrix <- polygon_I_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_I <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(I_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_I) <- "constant"
           
           # Smooth the polygon
           polygon_I <- polygon_I %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon I
           
           output1 <- st_difference(tile, st_union(polygon_I))
           
           output2 <- st_intersection(polygon_I, tile)
           
           output3 <- st_difference(polygon_I, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON I DONE
           
         },
         
         "J" ={
           ## POLYGON J
           J_as_matrix <- polygon_J_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_J <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(J_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_J) <- "constant"
           
           # Smooth the polygon
           polygon_J <- polygon_J %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon J
           
           output1 <- st_difference(tile, st_union(polygon_J))
           
           output2 <- st_intersection(polygon_J, tile)
           
           output3 <- st_difference(polygon_J, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON J DONE
           
         },
         
         "K" ={
           ## POLYGON K
           K_as_matrix <- polygon_K_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_K <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(K_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_K) <- "constant"
           
           # Smooth the polygon
           polygon_K <- polygon_K %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon F
           
           output1 <- st_difference(tile, st_union(polygon_K))
           
           output2 <- st_intersection(polygon_K, tile)
           
           output3 <- st_difference(polygon_K, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON K DONE
           
         },
         
         "AK" ={
           ## POLYGON AK
           AK_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK <- st_sf(
             aggregate(
               AK_as_matrix,
               by=list(colour = AK_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK = st_cast(polygon_AK, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK))
           
           output2 <- st_intersection(polygon_AK, tile)
           
           output3 <- st_difference(polygon_AK, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK DONE
           
         },
         
         "AK+" ={
           ## POLYGON AK+
           AK_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_plus <- st_sf(
             aggregate(
               AK_plus_as_matrix,
               by=list(colour = AK_plus_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_plus = st_cast(polygon_AK_plus, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_plus))
           
           output2 <- st_intersection(polygon_AK_plus, tile)
           
           output3 <- st_difference(polygon_AK_plus, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK+ DONE
           
         },
         
         "AK++" ={
           ## POLYGON AK++
           AK_plus_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus_plus, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_plus_plus <- st_sf(
             aggregate(
               AK_plus_plus_as_matrix,
               by=list(colour = AK_plus_plus_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_plus_plus = st_cast(polygon_AK_plus_plus, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_plus_plus))
           
           output2 <- st_intersection(polygon_AK_plus_plus, tile)
           
           output3 <- st_difference(polygon_AK_plus_plus, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK++ DONE
         
           },
         
         "AK-" ={
           ## POLYGON AK-
           AK_neg_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_neg, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_neg <- st_sf(
             aggregate(
               AK_neg_as_matrix,
               by=list(colour = AK_neg_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_neg = st_cast(polygon_AK_neg, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_neg))
           
           output2 <- st_intersection(polygon_AK_neg, tile)
           
           output3 <- st_difference(polygon_AK_neg, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK- DONE
           
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
      # If scale is 1/2 reverse colours
      # tile <- tile %>%
      #   dplyr::mutate(colour = dplyr::case_when(colour == 1 ~ 2,
      #                                        colour == 2 ~ 1))
        tile <- tile %>%
          dplyr::mutate(colour = dplyr::case_when(colour == 1 ~ 2,
                                                  colour == 2 ~ 3,
                                                  colour == 3 ~ 4,
                                                  colour == 4 ~ 5,
                                                  colour == 5 ~ 6,
                                                  colour == 6 ~ 7,
                                                  colour == 7 ~ 8,
                                                  colour == 8 ~ 9,
                                                  colour == 9 ~ 10,
                                                  colour == 10 ~ 11,
                                                  colour == 11 ~ 12,
                                                  colour == 12 ~ 1))
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

#test functions----
aj_truchet_p(x = 1, y = 4, type = "AK") %>%
  ggplot() +
  geom_sf(aes(fill = factor(colour)))

ggplot() +
  geom_sf(data = aj_truchet_p(x = 0, y = 1, type = "AK", scale_p = 1),
          aes(fill = factor(colour))) +
  geom_sf(data = aj_truchet_p(x = 1, y = 1, type = "AK+", scale_p = 1/2),
          aes(fill = factor(colour))) +
  geom_sf(data = aj_truchet_p(x = 2, y = 1, type = "AK-", scale_p = 1/4),
          aes(fill = factor(colour))) +
  geom_sf(data = aj_truchet_p(x = 0, y = 0, type = "AK+", scale_p = 1),
          aes(fill = factor(colour))) +
  geom_sf(data = aj_truchet_p(x = 1, y = 0, type = "AK-", scale_p = 1/2),
          aes(fill = factor(colour))) +
  geom_sf(data = aj_truchet_p(x = 2, y = 0, type = "AK", scale_p = 1/4),
          aes(fill = factor(colour)))

# 7. TRUCHET MS----

aj_truchet_ms <- function(df = NULL, p1 = 1, p2 = 0, p3 = 0, tiles = c("A", "B"), xlim = c(1, 3), ylim = c(1, 6)){
  
  #' Truchet mosaics
  #'
  #' @param df an (optional) data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid), tiles (characters with types of tiles to use for mosaic), scale_p (the scale of the tile to be placed at each coordinate)
  #' @param p1 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1 (the sum of p1, p2, p3 must be equal to one, or less to avoid empty spots in the mosaic)
  #' @param p2 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1/2
  #' @param p3 a number between 0 and 1 with the proportion of spots in the mosaic to cover with tiles of scale 1/4
  #' @param tiles a character vector with types of tiles to use for mosaic (default: \code{c("dr", "dl")})
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @return An object of type \code{sf} with the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_ms()
  #' plot(mosaic)
  #' mosaic <- st_truchet_ms(p1 = 0.8, p2 = 0.16, p3 = 0.04)
  #' plot(mosaic)
  #' mosaic <- st_truchet_ms(p1 = 0.6, p2 = 0.3, p3 = 0.1, tiles = c("|", "-"))
  #' plot(mosaic)
  #' @note For a discussion of multi-scale Truchet patterns see \url{https://christophercarlson.com/portfolio/multi-scale-truchet-patterns/}
  
  # Validate inputs
  # Assert proportions
  checkmate::assertNumber(p1,
                          lower = 0,
                          upper = 1)
  checkmate::assertNumber(p2,
                          lower = 0,
                          upper = 1)
  checkmate::assertNumber(p3,
                          lower = 0,
                          upper = 1)
  # Assert sum of proportions
  checkmate::assertTRUE(p1 + p2 + p3 <= 1)
  # Assert xlim
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  # Assert ylim
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)
  
  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){
    
    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1)))
    
    # Adjust container to accommodate tiles at multiple scales:
    df_1 <- df %>%
      dplyr::slice_sample(prop = p1)
    
    df_2 <- df %>%
      dplyr::anti_join(df_1,
                       by = c("x", "y"))
    
    if(p2 != 0 | p3 != 0){
      df_2 <- df_2 %>%
        dplyr::slice_sample(prop = p2/(p2 + p3))
    }
    
    df_3 <- df %>%
      dplyr::anti_join(df_1 %>%
                         rbind(df_2),
                       by = c("x", "y"))
    
    df <- rbind(df_1,
                df_2,
                df_3) %>%
      dplyr::mutate(tiles = sample(tiles,
                                   dplyr::n(),
                                   replace = TRUE),
                    scale_p = c(rep(1,
                                    nrow(df_1)),
                                rep(1/2,
                                    nrow(df_2)),
                                rep(1/4,
                                    nrow(df_3))))
  }
  
  # Adjust points for multiscale mosaics
  # Scale 1
  df_1 <- df %>%
    dplyr::filter(scale_p == 1)
  
  # Scale 2
  df_2 <- df %>%
    dplyr::filter(scale_p == 1/2) %>%
    dplyr::mutate(x_1 = -0.25, y_1 = 0.25,
                  x_2 = 0.25, y_2 = 0.25,
                  x_3 = 0.25, y_3 = -0.25,
                  x_4 = 0.25, y_4 = -0.25) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("x_"),
                        names_to = "xpos",
                        values_to = "x_shift") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("y_"),
                        names_to = "ypos",
                        values_to = "y_shift") %>%
    dplyr::transmute(x = .data$x + .data$x_shift,
                     y = .data$y + .data$y_shift,
                     tiles,
                     scale_p) %>%
    dplyr::distinct()
  
  # Scale 3
  df_3 <- df %>%
    dplyr::filter(scale_p == 1/4) %>%
    dplyr::mutate(x_1 = -1/8 * 3, y_1 = 1/8 * 3,
                  x_2 = -1/8, y_2 = 1/8 * 3,
                  x_3 = 1/8, y_3 = 1/8 * 3,
                  x_4 = 1/8 * 3, y_4 = 1/8 * 3,
                  x_5 = -1/8 * 3, y_5 = 1/8,
                  x_6 = -1/8, y_6 = 1/8,
                  x_7 = 1/8, y_7 = 1/8,
                  x_8 = 1/8 * 3, y_8 = 1/8,
                  x_9 = -1/8 * 3, y_9 = -1/8 ,
                  x_10 = -1/8, y_10 = -1/8,
                  x_11 = 1/8, y_11 = -1/8,
                  x_12 = 1/8 * 3, y_12 = -1/8,
                  x_13 = -1/8 * 3, y_13 = -1/8 * 3,
                  x_14 = -1/8, y_14 = -1/8 * 3,
                  x_15 = 1/8, y_15 = -1/8 * 3,
                  x_16 = 1/8 * 3, y_16 = -1/8 * 3) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("x_"),
                        names_to = "xpos",
                        values_to = "x_shift") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("y_"),
                        names_to = "ypos",
                        values_to = "y_shift") %>%
    dplyr::transmute(x = .data$x + .data$x_shift,
                     y = .data$y + .data$y_shift,
                     tiles,
                     scale_p) %>%
    dplyr::distinct()
  
  # Bind all scales
  df <- rbind(df_1,
              df_2,
              df_3)
  
  # Collect elements for assembling the mosaic
  x_c <- df$x
  y_c <- df$y
  type <- df$tiles
  scale_p <- df$scale_p
  
  ## NOTE: purrr does not like it when .id is used, complains that geometry column not present: why?
  
  mosaic <- purrr::pmap_dfr(list(x_c, y_c, type, scale_p), aj_truchet_p)
  
  return(mosaic)
}

mosaic <- aj_truchet_ms(tiles = c("AK", "AK+", "AK++", "AK-"), 
                        p1 = 0.2, 
                        p2 = 0.6,
                        p3 = 0.2,
                        xlim = c(0, 6),
                        ylim = c(0, 6))

# aj_truchet_ms(tiles = c("A", "C", "E", "J"), 
#               p1 = 0.2, 
#               p2 = 0.6,
#               p3 = 0.2,
#               xlim = c(0, 125),
#               ylim = c(50, 80))

ggplot() +
  geom_sf(data = mosaic,
          aes(fill = colour),
          color = NA)

mosaic |> 
  ggplot(aes(fill = colour)) +
  geom_sf(color = NA, show.legend = FALSE) + 
  scale_fill_gradientn(colours = c("#FF8000", "#ffffff")) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "#47c7fc"))

ggsave('./03_plots/mcL_truchet_ms2.png', dpi = 350, height = 4, width = 4, units = 'in')


# 8. TRUCHET DISSOLVE----
aj_truchet_dissolve <- function(mosaic){
  
  #' Dissolving the boundaries of individual tiles in Truchet mosaics
  #'
  #' @param mosaic a mosaic produced by function \code{st_truchet_ms}
  #' @return An object of type \code{sf} with the mosaic after dissolving the boundaries of individual tiles
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_ms()
  #' mosaic <- st_truchet_dissolve(mosaic)
  
  # Validate inputs
  
  # Summarize by color to produce individual pieces made of compact segments of mosaic by color
  mosaic_2 <- mosaic %>%
    dplyr::group_by(.data$colour) %>%
    dplyr::summarize(colour = max(.data$colour))
  
  sf::st_agr(mosaic_2) <- "constant"
  
  # Obtain the difference of mosaics of color 1 with respect to 2
  mosaic_3 <- mosaic_2[1,] %>%
    sf::st_difference(mosaic_2[2,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_4 <- mosaic_2[2,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 2 with respect to 3
  mosaic_5 <- mosaic_2[2,] %>%
    sf::st_difference(mosaic_2[3,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_6 <- mosaic_2[3,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 3 with respect to 4
  mosaic_7 <- mosaic_2[3,] %>%
    sf::st_difference(mosaic_2[4,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_8 <- mosaic_2[4,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 4 with respect to 5
  mosaic_9 <- mosaic_2[4,] %>%
    sf::st_difference(mosaic_2[5,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_10 <- mosaic_2[5,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 5 with respect to 6
  mosaic_11 <- mosaic_2[5,] %>%
    sf::st_difference(mosaic_2[6,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_12 <- mosaic_2[6,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 6 with respect to 7
  mosaic_13 <- mosaic_2[6,] %>%
    sf::st_difference(mosaic_2[7,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_14 <- mosaic_2[7,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 7 with respect to 8
  mosaic_15 <- mosaic_2[7,] %>%
    sf::st_difference(mosaic_2[8,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_16 <- mosaic_2[8,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 8 with respect to 9
  mosaic_17 <- mosaic_2[8,] %>%
    sf::st_difference(mosaic_2[9,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_18 <- mosaic_2[9,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 9 with respect to 10
  mosaic_19 <- mosaic_2[9,] %>%
    sf::st_difference(mosaic_2[10,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_20 <- mosaic_2[10,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 10 with respect to 11
  mosaic_21 <- mosaic_2[10,] %>%
    sf::st_difference(mosaic_2[11,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_22 <- mosaic_2[11,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Obtain the difference of mosaics of color 11 with respect to 12
  mosaic_23 <- mosaic_2[11,] %>%
    sf::st_difference(mosaic_2[12,]$geometry) %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # Cast the multipolygon of the opposite color to individual polygons
  mosaic_24 <- mosaic_2[12,] %>%
    sf::st_set_agr("constant") %>%
    sf::st_cast(to = "POLYGON") %>%
    dplyr::mutate(area = sf::st_area(.data$geometry))
  
  # # Bind both colors
  mosaic <- rbind(mosaic_3,
                  mosaic_4,
                  mosaic_5,
                  mosaic_6,
                  mosaic_7,
                  mosaic_8,
                  mosaic_9,
                  mosaic_10,
                  mosaic_11,
                  mosaic_12,
                  mosaic_13,
                  mosaic_14,
                  mosaic_15,
                  mosaic_16,
                  mosaic_17,
                  mosaic_18,
                  mosaic_19,
                  mosaic_20,
                  mosaic_21,
                  mosaic_22,
                  mosaic_23,
                  mosaic_24)
  
  return(mosaic)
}

mosaic_dissolved <- aj_truchet_dissolve(mosaic = mosaic)

ggplot() +
  geom_sf(data = mosaic_dissolved,
          color = "black",
          fill = NA) + 
  theme_void() +
  theme(plot.background = element_rect(fill = "#FF8000", colour = '#FF8000'))

ggsave('./03_plots/mcL_polygon4.png', dpi = 350, height = 4, width = 4, units = 'in')


ggplot() +
  geom_sf(data = mosaic_dissolved,
          aes(fill = colour),
          color = "white")

# buffered_tiles <- mosaic_dissolved %>%
#   filter(color == 2)  %>%
#   st_buffer(dist = -0.1)
# 
# ggplot() +
#   geom_sf(data = mosaic_dissolved,
#           aes(fill = color),
#           color = "white") +
#   geom_sf(data = buffered_tiles,
#           fill = "red",
#           color = "white")
# 
# 
# polygon_A <- web_digitiser_truchet340_multi_polygon %>%
#   #filter(group %in% 'A') %>%
#   select(x,y)
# 
# polygon_A <- data.frame(geometry = sf::st_polygon(list(polygon_A_as_matrix)) %>% 
#                           sf::st_sfc()) %>%
#   sf::st_as_sf()
# 
# plot(polygon_A)
# 
# polygon_AK <- web_digitiser_truchet340_multi_polygon %>%
#   mutate(colour = case_when(group %in% 'A' ~ 2,
#                             group %in% 'B' ~ 3,
#                             group %in% 'C' ~ 4,
#                             group %in% 'D' ~ 5,
#                             group %in% 'E' ~ 6,
#                             group %in% 'F' ~ 7,
#                             group %in% 'G' ~ 8,
#                             group %in% 'H' ~ 9,
#                             group %in% 'I' ~ 10,
#                             group %in% 'J' ~ 11,
#                             group %in% 'K' ~ 12,
#                             TRUE ~ NA)) %>% 
#   select(-group)
# 
# class(polygon_AK)
# 
# polygon_AK_as_matrix <- data.matrix(polygon_AK)
# 
# mat <- data.matrix(df)
# 
# # reindernijhoff.net
# 
# web_digitiser_truchet340 <- read.csv('./00_raw_data/web_digitiser_truchet340.csv') %>%
#   #select(2:3) %>% 
#   rename(lon = x, lat = y)
# 
# p_truchet340 <- web_digitiser_truchet340 %>% 
#   ggplot(aes(lon, lat)) + 
#   geom_point(color = 'black')
# 
# p_truchet340_polygon <- web_digitiser_truchet340 %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   summarise() %>% 
#   st_convex_hull()
#   # summarise(geometry = st_combine(geometry)) %>%
#   # st_cast("POLYGON")
# 
# #https://stackoverflow.com/questions/50303438/points-in-multiple-polygons-using-r
# web_digitiser_truchet340_multi_polygon <- read.csv('./00_raw_data/web_digitiser_truchet340_multi_polygon_closed.csv') 
# 
# #https://stackoverflow.com/questions/67001602/can-you-create-multiple-polygons-in-r-from-a-dataframe-containing-the-vertices
# 
# xys_AJ <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y"))
# 
# xymp_AJ <- st_sf(
#   aggregate(
#     xys_AJ,
#     by=list(ID=xys_AJ$group),
#     do_union=FALSE,
#     FUN=function(vals){vals[1]})) %>% 
#   select(ID, geometry) %>% 
#   st_convex_hull()
# 
# plot(xymp_AJ)

# 9. Photo mosaics with variable-width lines----
# Point to the place where your image is stored
lando <- './00_raw_data/lando.jpeg'

mcl38 <- './00_raw_data/mcl38.jpg'

# Load and convert to grayscale
load.image(lando) %>%
  grayscale() -> img

load.image(mcl38) %>%
  grayscale() -> img

plot(img)

lando_rs <- imager::imresize(img, 
                             scale = 1/4, 
                             interpolation = 6)

mcl38_rs <- imager::imresize(img, 
                             scale = 1/50, 
                             interpolation = 6)

plot(mcl38_rs)

lando_df <- lando_rs %>%
  as.data.frame() %>%
  mutate(y = -(y - max(y)))

mcl38_df <- mcl38_rs %>%
  as.data.frame() %>%
  mutate(y = -(y - max(y)))

ggplot() +
  geom_point(data = mcl38_df,
             aes(x = x,
                 y = y,
                 colour = value)) +
  coord_equal()

# This will use a smaller subset of points to create the mosaic, which will then be rescaled
s <- 15

xlim <- c(min(mcl38_df$x)/s - 4, max(mcl38_df$x)/s + 4)
ylim <- c(min(mcl38_df$y)/s - 4, max(mcl38_df$y)/s + 4)

# Create a data frame with the coordinates for the tiles and define a scale parameter
m_1 <- expand.grid(x = seq(xlim[1], xlim[2], 1),
                   y = seq(ylim[1], ylim[2], 1)) %>%
  dplyr::mutate(tiles = sample(c("AK"), n(), replace = TRUE),
         scale_p = 1)

m_1 <- aj_truchet_ms(df = m_1) 

ggplot() +
  geom_sf(data = m_1 %>% aj_truchet_dissolve(),
          aes(fill = colour),
          colour = "white")

m_2 <- m_1 %>% 
  # Dissolve boundaries
  aj_truchet_dissolve() %>% 
  # Buffer the polygons
  st_buffer(dist = -0.05) %>%
  # Adjust the color field to distinguish it from the original polygons
  mutate(colour = colour + 2)

# Remove empty geometries
m_2 <- m_2[!st_is_empty(m_2), , drop = FALSE]

m_1_lines <- m_1 %>% 
  aj_truchet_dissolve() %>% 
  st_cast(to = "MULTILINESTRING") %>% 
  tidyr::drop_na()

m_2_lines <- m_2 %>% 
  st_cast(to = "MULTILINESTRING")

ggplot() +
  geom_sf(data = m_1_lines,
          color = "#FF8000") +
  geom_sf(data = m_2_lines,
          color = "#47c7fc") +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = 'white'))

m_1_union <- st_union(m_1)
m_2_union <- st_union(m_2)

m_1_union <- (m_1_lines * s) %>%
  st_sf()

m_2_union <- (m_2_lines * s) %>% 
  st_sf()

ggplot() +
  geom_sf(data = m_1_union,
          color = "#FF8000") +
  geom_sf(data = m_2_union,
          color = "#47c7fc")

mosaic <- rbind(m_1_union,
                m_2_union)


plot(mosaic)

# Use the bounding box of the mosaic to define the extents of the grid that becomes the blade
bbox <- st_bbox(mosaic) %>% 
  round()

# Create a data frame with the start and end points of the lines that become the blade to split the mosaic lines
blade <- data.frame(x_start = c(bbox$xmin:bbox$xmax, 
                                rep(bbox$ymin, 
                                    length(bbox$ymin:bbox$ymax))),
                    x_end = c(bbox$xmin:bbox$xmax, 
                              rep(bbox$xmax, 
                                  length(bbox$ymin:bbox$ymax))),
                    y_start = c(rep(bbox$ymin, 
                                    length(bbox$xmin:bbox$xmax)),
                                bbox$ymin:bbox$ymax),
                    y_end = c(rep(bbox$ymax,
                                  length(bbox$xmin:bbox$xmax)),
                              bbox$ymin:bbox$ymax))

# Shift the blade a small amount to avoid perfect overlap with lines in the mosaic
blade <- blade %>%
  mutate(across(everything(), 
                ~ .x + 0.18))

# Create the blade and convert to simple features
blade <- purrr::pmap(blade, 
                     function(x_start, x_end, y_start, y_end){
                       st_linestring(
                         matrix(c(x_start,
                                  y_start,
                                  x_end,
                                  y_end),
                                ncol = 2,
                                byrow = TRUE))}) %>%
  st_as_sfc()


mosaic_lines <- mosaic %>%
  st_split(blade)

plot(mosaic_lines)

mosaic_lines <- mosaic_lines %>%
  st_collection_extract(type = "LINESTRING") %>%
  st_cast(to = "LINESTRING") %>%
  mutate(id = 1:n())

lando_sf <- lando_df %>%
  st_as_sf(coords = c("x", "y"))

mcl38_sf <- mcl38_df %>%
  st_as_sf(coords = c("x", "y"))

value <- lando_sf[mosaic_lines %>% 
                      st_nearest_feature(lando_sf),] %>%
  pull(value)

value <- mcl38_sf[mosaic_lines %>% 
                    st_nearest_feature(mcl38_sf),] %>%
  pull(value)

mosaic_lines$value <- value

# ggplot() +
#   geom_sf(data = mosaic_lines %>%
#             st_set_agr("constant") %>%
#             st_crop(lando_sf),
#           # Reverse the valence of values
#           aes(size = -value)) +
#   scale_size(range = c(0.01, 1)) + 
#   coord_sf(expand = FALSE) + 
#   theme_void() + 
#   theme(legend.position = "none",
#         plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"))

ggplot() +
  geom_sf(data = mosaic_lines %>%
            st_set_agr("constant") %>%
            st_crop(lando_sf),
          aes(size = exp(-2 * value))) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"))

ggplot() +
  geom_sf(data = mosaic_lines %>%
            st_set_agr("constant") %>%
            st_crop(mcl38_sf),
          aes(color = value,
              size = exp(-2 * value))) +
  scale_color_distiller(direction = -1) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        plot.background = element_rect(fill = "azure"))

# marilyn----
marilyn <- load.image(system.file("extdata", 
                                  "marilyn.jpg", 
                                  package = "truchet"))

marilyn

marilyn_rs <- imager::imresize(marilyn, 
                               scale = 1/4, 
                               interpolation = 6)

marilyn_df <- marilyn_rs %>%
  as.data.frame() %>%
  mutate(y = -(y - max(y)))

dim(marilyn_df)
dim(lando_df)

ggplot() +
  geom_point(data = lando_df,
             aes(x = x,
                 y = y,
                 color = value)) +
  coord_equal()

# This will use a smaller subset of points to create the mosaic, which will then be rescaled
s <- 15

xlim_m <- c(min(marilyn_df$x)/s - 4, max(marilyn_df$x)/s + 4)
ylim_m <- c(min(marilyn_df$y)/s - 4, max(marilyn_df$y)/s + 4)

# Create a data frame with the coordinates for the tiles and define a scale parameter
m_1_m <- expand.grid(x = seq(xlim_m[1], xlim_m[2], 1),
                   y = seq(ylim_m[1], ylim_m[2], 1)) %>%
  mutate(tiles = sample(c("dl", "dr"), n(), replace = TRUE),
         scale_p = 1)

m_1_m <- st_truchet_ms(df = m_1_m)

ggplot() +
  geom_sf(data = m_1_m %>% st_truchet_dissolve(),
          aes(fill = color),
          color = "white")

m_2_m <- m_1_m %>% 
  # Dissolve boundaries
  st_truchet_dissolve() %>% 
  # Buffer the polygons
  st_buffer(dist = -0.15) %>%
  # Adjust the color field to distinguish it from the original polygons
  mutate(color = color + 2)

# Remove empty geometries
m_2_m <- m_2_m[!st_is_empty(m_2_m), , drop = FALSE]

m_1_m_lines <- m_1_m %>% 
  st_truchet_dissolve() %>% 
  st_cast(to = "MULTILINESTRING")

m_2_m_lines <- m_2_m %>% 
  st_cast(to = "MULTILINESTRING")

ggplot() +
  geom_sf(data = m_1_m_lines,
          color = "red") +
  geom_sf(data = m_2_m_lines,
          color = "blue")

m_1_m_union <- st_union(m_1_m)
m_2_m_union <- st_union(m_2_m)

m_1_m_union <- (m_1_m_lines * s) %>%
  st_sf()

m_2_m_union <- (m_2_m_lines * s) %>% 
  st_sf()

ggplot() +
  geom_sf(data = m_1_m_union,
          color = "red") +
  geom_sf(data = m_2_m_union,
          color = "blue")

mosaic_m <- rbind(m_1_m_union,
                  m_2_m_union)

# Use the bounding box of the mosaic to define the extents of the grid that becomes the blade
bbox_m <- st_bbox(mosaic_m) %>% 
  round()

# Create a data frame with the start and end points of the lines that become the blade to split the mosaic lines
blade_m <- data.frame(x_start = c(bbox_m$xmin:bbox_m$xmax, 
                                rep(bbox_m$ymin, 
                                    length(bbox_m$ymin:bbox_m$ymax))),
                    x_end = c(bbox_m$xmin:bbox_m$xmax, 
                              rep(bbox_m$xmax, 
                                  length(bbox_m$ymin:bbox_m$ymax))),
                    y_start = c(rep(bbox_m$ymin, 
                                    length(bbox_m$xmin:bbox_m$xmax)),
                                bbox_m$ymin:bbox_m$ymax),
                    y_end = c(rep(bbox_m$ymax,
                                  length(bbox_m$xmin:bbox_m$xmax)),
                              bbox_m$ymin:bbox_m$ymax))

# Shift the blade a small amount to avoid perfect overlap with lines in the mosaic
blade_m <- blade_m %>%
  mutate(across(everything(), 
                ~ .x + 0.18))

# Create the blade and convert to simple features
blade_m <- purrr::pmap(blade_m, 
                     function(x_start, x_end, y_start, y_end){
                       st_linestring(
                         matrix(c(x_start,
                                  y_start,
                                  x_end,
                                  y_end),
                                ncol = 2,
                                byrow = TRUE))}) %>%
  st_as_sfc()

mosaic_m_lines <- mosaic_m %>%
  st_split(blade_m)

mosaic_m_lines <- mosaic_m_lines %>%
  st_collection_extract(type = "LINESTRING") %>%
  st_cast(to = "LINESTRING") %>%
  mutate(id = 1:n())

#table(mosaic_lines$colour)

marilyn_sf <- marilyn_df %>%
  st_as_sf(coords = c("x", "y"))

value <- marilyn_sf[mosaic_m_lines %>% 
                      st_nearest_feature(marilyn_sf),] %>%
  pull(value)

mosaic_m_lines$value <- value

ggplot() +
  geom_sf(data = mosaic_m_lines %>%
            st_set_agr("constant") %>%
            st_crop(marilyn_sf),
          # Reverse the valence of values
          aes(size = -value)) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  #theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"))

plot(mosaic_lines)

ggplot() +
  geom_sf(data = mosaic_lines %>%
            st_set_agr("constant") %>%
            st_crop(marilyn_sf),
          aes(color = value,
              size = exp(-2 * value))) +
  scale_color_distiller(direction = -1) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"))

mosaic_test_ms <- aj_truchet_ms(tiles = c("A", "B"), p1 = 0.2, p2 = 0.8)
warnings()
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

# 10. TRUCHET FLEX----

aj_truchet_flex <- function(x = 0, y = 0, type = "A", b = 1/2){
  
  #' Flexible Truchet tiles
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "Ac", "Bc", "Cc", "Dc", "As", "Bs", "Cs", "Ds"
  #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_flex(type = "Al")
  #' st_truchet_flex(type = "Cl")
  #' @note For a discussion of Truchet patterns see: Robert Bosch & Urchin Colley (2013) Figurative mosaics from flexible Truchet tiles, Journal of Mathematics and the Arts, 7:3-4, 122-135, \url{10.1080/17513472.2013.838830}
  
  # Validate inputs
  checkmate::assertChoice(type, c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "AK", "AK+", "AK++", "AK-"))
  # b must be a value beween zero and 1
  checkmate::assert_number(b, lower = 0, upper = 1)
  
  # Adjust values of b in case that there is an exact zero or one, which messes up the selection of colors later on
  if(b == 0) b <- 1/100
  if(b == 1) b <- 99/100
  
  ## CREATE BASE TILE
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
  
  ## BASE TILE DONE
  
  # Tile types
  
  switch(type,
         
         "A" ={
           ## POLYGON A
           A_as_matrix <- polygon_A_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_A <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(A_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_A) <- "constant"
           
           # Smooth the polygon
           polygon_A <- polygon_A %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon A
           
           output1 <- st_difference(tile, st_union(polygon_A))
           
           output2 <- st_intersection(polygon_A, tile)
           
           output3 <- st_difference(polygon_A, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON A DONE
           
         },
         
         "B" ={
           ## POLYGON B
           B_as_matrix <- polygon_B_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_B <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(B_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_B) <- "constant"
           
           # Smooth the polygon
           polygon_B <- polygon_B %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon B
           
           output1 <- st_difference(tile, st_union(polygon_B))
           
           output2 <- st_intersection(polygon_B, tile)
           
           output3 <- st_difference(polygon_B, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON B DONE
           
         },
         
         "C" ={
           ## POLYGON C
           C_as_matrix <- polygon_C_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_C <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(C_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_C) <- "constant"
           
           # Smooth the polygon
           polygon_C <- polygon_C %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon C
           
           output1 <- st_difference(tile, st_union(polygon_C))
           
           output2 <- st_intersection(polygon_C, tile)
           
           output3 <- st_difference(polygon_C, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON C DONE
           
         },
         
         "D" ={
           ## POLYGON D
           D_as_matrix <- polygon_D_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_D <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(D_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_D) <- "constant"
           
           # Smooth the polygon
           polygon_D <- polygon_D %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon D
           
           output1 <- st_difference(tile, st_union(polygon_D))
           
           output2 <- st_intersection(polygon_D, tile)
           
           output3 <- st_difference(polygon_D, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON D DONE
           
         },
         
         "E" ={
           ## POLYGON E
           E_as_matrix <- polygon_E_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_E <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(E_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_E) <- "constant"
           
           # Smooth the polygon
           polygon_E <- polygon_E %>% 
             st_concave_hull(0.1) 
           
           # Bind BASE TILE with polygon
           # polygon E
           
           output1 <- st_difference(tile, st_union(polygon_E))
           
           output2 <- st_intersection(polygon_E, tile)
           
           output3 <- st_difference(polygon_E, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON E DONE
           
         },
         
         "F" ={
           ## POLYGON F
           F_as_matrix <- polygon_F_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_F <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(F_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_F) <- "constant"
           
           # Smooth the polygon
           polygon_F <- polygon_F %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon F
           
           output1 <- st_difference(tile, st_union(polygon_F))
           
           output2 <- st_intersection(polygon_F, tile)
           
           output3 <- st_difference(polygon_F, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON F DONE
           
         },
         
         "G" ={
           ## POLYGON G
           G_as_matrix <- polygon_G_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_G <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(G_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_G) <- "constant"
           
           # Smooth the polygon
           polygon_G <- polygon_G %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon G
           
           output1 <- st_difference(tile, st_union(polygon_G))
           
           output2 <- st_intersection(polygon_G, tile)
           
           output3 <- st_difference(polygon_G, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON G DONE
           
         },
         
         "H" ={
           ## POLYGON H
           H_as_matrix <- polygon_H_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_H <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(H_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_H) <- "constant"
           
           # Smooth the polygon
           polygon_H <- polygon_H %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon H
           
           output1 <- st_difference(tile, st_union(polygon_H))
           
           output2 <- st_intersection(polygon_H, tile)
           
           output3 <- st_difference(polygon_H, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON H DONE
           
         },
         
         "I" ={
           ## POLYGON F
           I_as_matrix <- polygon_I_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_I <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(I_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_I) <- "constant"
           
           # Smooth the polygon
           polygon_I <- polygon_I %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon I
           
           output1 <- st_difference(tile, st_union(polygon_I))
           
           output2 <- st_intersection(polygon_I, tile)
           
           output3 <- st_difference(polygon_I, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON I DONE
           
         },
         
         "J" ={
           ## POLYGON J
           J_as_matrix <- polygon_J_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_J <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(J_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_J) <- "constant"
           
           # Smooth the polygon
           polygon_J <- polygon_J %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon J
           
           output1 <- st_difference(tile, st_union(polygon_J))
           
           output2 <- st_intersection(polygon_J, tile)
           
           output3 <- st_difference(polygon_J, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON J DONE
           
         },
         
         "K" ={
           ## POLYGON K
           K_as_matrix <- polygon_K_as_matrix
           
           # Convert coordinates to polygon and then to simple features and then smooth
           polygon_K <- data.frame(colour = 2,
                                   geometry = sf::st_polygon(list(K_as_matrix)) %>% 
                                     sf::st_sfc()) %>%
             sf::st_as_sf() 
           
           # Assign constant geometry
           sf::st_agr(polygon_K) <- "constant"
           
           # Smooth the polygon
           polygon_K <- polygon_K %>% 
             st_convex_hull() 
           
           # Bind BASE TILE with polygon
           # polygon F
           
           output1 <- st_difference(tile, st_union(polygon_K))
           
           output2 <- st_intersection(polygon_K, tile)
           
           output3 <- st_difference(polygon_K, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON K DONE
           
         },
         
         "AK" ={
           ## POLYGON AK
           AK_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK <- st_sf(
             aggregate(
               AK_as_matrix,
               by=list(colour = AK_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK = st_cast(polygon_AK, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK))
           
           output2 <- st_intersection(polygon_AK, tile)
           
           output3 <- st_difference(polygon_AK, tile)
           
           tile <- dplyr::bind_rows(output1, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK DONE
           
         },
         
         "AK+" ={
           ## POLYGON AK+
           AK_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_plus <- st_sf(
             aggregate(
               AK_plus_as_matrix,
               by=list(colour = AK_plus_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_plus = st_cast(polygon_AK_plus, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_plus))
           
           output2 <- st_intersection(polygon_AK_plus, tile)
           
           output3 <- st_difference(polygon_AK_plus, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK+ DONE
           
         },
         
         "AK++" ={
           ## POLYGON AK++
           AK_plus_plus_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_plus_plus, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_plus_plus <- st_sf(
             aggregate(
               AK_plus_plus_as_matrix,
               by=list(colour = AK_plus_plus_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_plus_plus = st_cast(polygon_AK_plus_plus, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_plus_plus))
           
           output2 <- st_intersection(polygon_AK_plus_plus, tile)
           
           output3 <- st_difference(polygon_AK_plus_plus, tile)
           
           tile <- dplyr::bind_rows(output1, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK++ DONE
           
         },
         
         "AK-" ={
           ## POLYGON AK-
           AK_neg_as_matrix <- st_as_sf(web_digitiser_truchet340_multi_polygon_neg, coords=c("x","y")) %>% 
             mutate(colour = case_when(group %in% 'A' ~ 2,
                                       group %in% 'B' ~ 3,
                                       group %in% 'C' ~ 4,
                                       group %in% 'D' ~ 5,
                                       group %in% 'E' ~ 6,
                                       group %in% 'F' ~ 7,
                                       group %in% 'G' ~ 8,
                                       group %in% 'H' ~ 9,
                                       group %in% 'I' ~ 10,
                                       group %in% 'J' ~ 11,
                                       group %in% 'K' ~ 12,
                                       TRUE ~ NA)) %>% 
             select(-group)
           
           # Convert coordinates to polygon and then to simple features 
           polygon_AK_neg <- st_sf(
             aggregate(
               AK_neg_as_matrix,
               by=list(colour = AK_neg_as_matrix$colour),
               do_union=FALSE,
               FUN=function(vals){vals[1]})) %>% 
             select(colour, geometry) %>% 
             st_convex_hull()
           
           polygon_AK_neg = st_cast(polygon_AK_neg, 'POLYGON')
           
           output1 <- st_difference(tile, st_union(polygon_AK_neg))
           
           output2 <- st_intersection(polygon_AK_neg, tile)
           
           output3 <- st_difference(polygon_AK_neg, tile)
           
           tile <- dplyr::bind_rows(output3, output2) %>% 
             select(colour, geometry)
           
           ## POLYGON AK- DONE
           
         }
  )
  
  # Translate so that the tiles are centered on the point (0, 0)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(-0.5, - 0.5))
  
  ## FINISH TILES
  # position at point (x, y)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(x, y))
  
  ## TILES DONE
  
  return(tile)
}


# Tiles types
tile_types <- data.frame(type = c("A", "B", "C", "D")) %>%
  mutate(x = c(1, 2.5, 1, 2.5),
         y = c(2.5, 2.5, 1, 1),
         b = 1/2)

# Elements for assembling the mosaic
x_c <- tile_types$x
y_c <- tile_types$y
type <- as.character(tile_types$type)
b <- tile_types$b

flex_test <- pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex) %>% 
  mutate(colour = tidyr::replace_na(colour, 1))

pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex) %>%
  mutate(colour = tidyr::replace_na(colour, 1)) %>% 
  ggplot() + 
  geom_sf(aes(fill = colour),
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


# Tiles types
tile_types <- data.frame(type = c("A", "B", "C", "D")) %>%
  mutate(x = c(1, 2, 1, 2),
         y = c(2, 2, 1, 1),
         b = 1/2)

# tile_types <- data.frame(type = c("A", "B", "C", "D")) %>%
#   mutate(x = c(1, 1, 1, 1),
#          y = c(1, 1, 1, 1),
#          b = 1/2)

# Elements for assembling the mosaic
x_c <- tile_types$x
y_c <- tile_types$y
type <- as.character(tile_types$type)
b <- tile_types$b

test_pmap_aj <- pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex)

test_pmap_st <- pmap_dfr(list(x_c, y_c, type, b), st_truchet_flex)

pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex) %>%
  mutate(colour = tidyr::replace_na(colour, 1)) %>%
  ggplot() + 
  geom_sf(aes(fill = colour),
          color = "black",
          size = 2) +
  geom_text(data = tile_types,
            aes(x = x,
                y = y,
                label = c("Design AK", "", "", "")),
            nudge_y = 0.6) + 
  scale_fill_distiller(direction = 1) +
  theme_void() +
  theme(legend.position = "none")


# # Tiles types
# tile_types <- data.frame(type = c("AK", "AK++", "AK", "AK++")) %>%
#   mutate(x = c(1, 2, 1, 2),
#          y = c(2, 2, 1, 1),
#          b = c(1/3, 1 - 1/3, 1 - 1/3, 1/3))
# 
# # Elements for assembling the mosaic
# x_c <- tile_types$x
# y_c <- tile_types$y
# type <- as.character(tile_types$type)
# b <- tile_types$b
# 
# pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex) %>%
#   ggplot() + 
#   geom_sf(aes(fill = colour),
#           colour = "black",
#           size = 2) +
#   geom_text(data = tile_types,
#             aes(x = x,
#                 y = y,
#                 label = c("Design C with b = 1/3", "", "", "")),
#             nudge_y = 0.6) + 
#   scale_fill_distiller(direction = 1) +
#   theme_void() +
#   theme(legend.position = "none")

elvis <- load.image(system.file("extdata", 
                                "elvis.jpg", 
                                package = "truchet"))

plot(elvis)

elvis_rs <- imresize(elvis, scale = 1/15, interpolation = 6)

plot(elvis_rs)

elvis_df <- elvis_rs %>%
  grayscale() %>% 
  as.data.frame()

summary(elvis_df)

# 11. TRUCHET FM----

aj_truchet_fm <- function(df = NULL, tiles = c("AK", "AK++"), b = 1/2, xlim = c(1, 3), ylim = c(1, 6)){
  
  #' Mosaics with flexible Truchet tiles
  #'
  #' @param df an (optional) data frame with the following columns: x and y (the coordinates of the tiles in a 1 by 1 grid), tiles (characters with types of tiles to use for mosaic), b (control of the boundary; defaults to 1/2)
  #' @param tiles a character vector with types of tiles to use for mosaic (default: \code{c("dr", "dl")})
  #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
  #' @param xlim a numeric vector of length 2 giving the range of the x coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @param ylim a numeric vector of length 2 giving the range of the y coordinates of the mosaic (ignored if argument \code{df} is an input)
  #' @return An object of type \code{sf} with the tiles arranged as a mosaic
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' mosaic <- st_truchet_fm()
  #' plot(mosaic)
  #' mosaic <- st_truchet_fm(b = 1/3)
  #' plot(mosaic)
  #' @note For a discussion of Truchet patterns see \url{http://arearugscarpet.blogspot.com/2014/04/the-curse-of-truchets-tiles.html}
  
  
  # Validate inputs
  # Assert xlim
  checkmate::assertAtomicVector(xlim,
                                min.len = 2,
                                max.len = 2)
  # Assert ylim
  checkmate::assertAtomicVector(ylim,
                                min.len = 2,
                                max.len = 2)
  # b must be a value beween zero and 1
  checkmate::assert_number(b, lower = 0, upper = 1)
  
  # Initialize data frame with coordinates for placing tiles if argument df was not provided
  if(is.null(df)){
    
    # Create grid for placing tiles using the limits provided xlim and ylim
    df <- data.frame(expand.grid(x = seq(xlim[1], xlim[2], 1),
                                 y = seq(ylim[1], ylim[2], 1))) %>%
      dplyr::mutate(tiles = sample(tiles,
                                   dplyr::n(),
                                   replace = TRUE),
                    b = b)
  }
  
  # Collect elements for assembling the mosaic
  x_c <- df$x
  y_c <- df$y
  type <- df$tiles
  b <- df$b
  
  mosaic <- purrr::pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex, .id = "id") %>% 
    mutate(colour = tidyr::replace_na(colour, 1))
  
  return(mosaic)
}

df <- elvis_df %>% 
  # Reverse the y axis
  mutate(y = -(y - max(y)),
         # The modulus of x + y can be used to create a checkerboard pattern
         tiles = case_when((x + y) %% 2 == 0 ~ "A",
                           (x + y) %% 2 == 1 ~ "D"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

# Start a timer
start_time <- Sys.time()

# Assemble mosaic
mosaic <- aj_truchet_fm(df) %>% 
  mutate(b = b * 0.99 + 0.001)

plot(mosaic)

# End timer
end_time <- Sys.time()

# Calculate time
end_time - start_time

mosaic <- mosaic %>% 
  mutate(colour = tidyr::replace_na(colour, 1))

mosaic %>%
  ggplot() + 
  geom_sf(aes(fill = colour),
          colour = NA) +
  scale_fill_distiller(direction = 1) + 
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

ggplot() +
  geom_sf(data = mosaic_lines %>%
            st_set_agr("constant") %>%
            st_crop(mcl38_sf),
          aes(color = value,
              size = exp(-2 * value))) +
  scale_color_distiller(direction = -1) +
  scale_size(range = c(0.01, 1)) + 
  coord_sf(expand = FALSE) + 
  theme_void() + 
  theme(legend.position = "none",
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in"),
        plot.background = element_rect(fill = "azure"))

# elvis correct----

df_elv <- elvis_df %>% 
  # Reverse the y axis
  mutate(y = -(y - max(y)),
         # The modulus of x + y can be used to create a checkerboard pattern
         tiles = case_when((x + y) %% 2 == 0 ~ "Ac",
                           (x + y) %% 2 == 1 ~ "Cc"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

# Start a timer
start_time <- Sys.time()

# Assemble mosaic
mosaic_elv <- st_truchet_fm(df = df_elv %>% 
                          mutate(b = b * 0.99 + 0.001))

# End timer
end_time <- Sys.time()

# Calculate time
end_time - start_time

mosaic_elv %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = NA) +
  scale_fill_distiller(direction = 1) + 
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

# mcl38 correct----

df_mcl38 <- mcl38_df %>% 
  # Reverse the y axis
  mutate(y = -(y - max(y)),
         # The modulus of x + y can be used to create a checkerboard pattern
         tiles = case_when((x + y) %% 2 == 0 ~ "Ac",
                           (x + y) %% 2 == 1 ~ "Cc"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

# Start a timer
start_time <- Sys.time()

# Assemble mosaic
mosaic_mcl38 <- st_truchet_fm(df = df_mcl38 %>% 
                              mutate(b = b * 0.99 + 0.001))

# End timer
end_time <- Sys.time()

# Calculate time
end_time - start_time

mosaic_mcl38 %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = NA) +
  scale_fill_distiller(direction = 1) + 
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none") 


pts <- matrix( c(0, 1,
                 0.33, 0.33,
                 1, 0),
               nrow=3,
               ncol=2,
               byrow=TRUE)

plot(pts)

t <- seq(0,
          1,
          length=50)

pts <- matrix( c(0, 1,
                  0.95, 0.95,
                  1, 0),
                nrow=3,
                ncol=2,
                byrow=TRUE)

# Compute bezier curve

line_1 <- bezier::bezier(t = t,
                         p = pts) 


plot(line_1)


# Tiles types
tile_types <- data.frame(type = c("Bc", "Bc", "Bc", "Bc")) %>%
  mutate(x = c(1, 2, 1, 2),
         y = c(2, 2, 1, 1),
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
                label = c("Design A", "", "", "")),
            nudge_y = 0.6) + 
  scale_fill_distiller(direction = 1) +
  theme_void() +
  theme(legend.position = "none")

# 12. Bruce McLaren----

# https://automobilist.com/en-gb/blogs/stories/the-true-visionary-bruce-mclaren?srsltid=AfmBOoo5uQWDk9GYn4IIQoyxnzSKiDm-GoBo_9aITBYIe7DaCFeVRCdU

bruce <- image_read('./00_raw_data/BruceMcLaren.jpeg.webp')

info <- image_info(bruce) 

print(info)

# whole image
bruce_crop <- image_crop(bruce, geometry = "1000x400+50+125")

image_write(bruce_crop, path = './00_raw_data/BruceMcLaren_cropped.jpeg', format = "jpeg")

# chequered flag man
bruce_crop1 <- image_crop(bruce, geometry = "450x350+75+130")

image_write(bruce_crop1, path = './00_raw_data/BruceMcLaren_cropped1.jpeg', format = "jpeg")

# Bruce in his car
bruce_crop2 <- image_crop(bruce, geometry = "600x400+530+130")

image_write(bruce_crop2, path = './00_raw_data/BruceMcLaren_cropped2.jpeg', format = "jpeg")

# flag
bruce_crop3 <- image_crop(bruce, geometry = "150x150+360+180")

image_write(bruce_crop3, path = './00_raw_data/BruceMcLaren_cropped3.jpeg', format = "jpeg")

bruce_cropped <- './00_raw_data/BruceMcLaren_cropped.jpeg'

bruce_cropped1 <- './00_raw_data/BruceMcLaren_cropped1.jpeg'

bruce_cropped2 <- './00_raw_data/BruceMcLaren_cropped2.jpeg'

bruce_cropped3 <- './00_raw_data/BruceMcLaren_cropped3.jpeg'

# Load and convert to grayscale
load.image(bruce_cropped) %>%
  grayscale() -> img

plot(img)

# bruce_cropped 1/8
bruce_rs <- imager::imresize(img, 
                             scale = 1/8, 
                             interpolation = 6)

plot(bruce_rs)

bruce_df <- bruce_rs %>%
  as.data.frame() %>%
  mutate(y = y - max(y))

df_bruce <- bruce_df %>% 
  # Reverse the y axis
  mutate(y = -(y - max(y)),
         # The modulus of x + y can be used to create a checkerboard pattern
         tiles = case_when((x + y) %% 2 == 0 ~ "Ac",
                           (x + y) %% 2 == 1 ~ "Dc"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

# df_bruce_small <- df_bruce %>% 
#   filter(x>19 & x<31, 
#          y>19 & y<31)

# Start a timer
start_time <- Sys.time()

# Assemble mosaic
mosaic_bruce <- st_truchet_fm(df = df_bruce %>% 
                                mutate(b = b * 0.99 + 0.001))

# End timer
end_time <- Sys.time()

# Calculate time
end_time - start_time

mosaic_bruce <- mosaic_bruce %>% mutate(id = as.numeric(id))

#glimpse(mosaic_bruce)

#https://stackoverflow.com/questions/64037373/gganimate-data-present-only-in-some-frames

McL_logo <- image_read('./00_raw_data/McLaren_logo1.png')

info <- image_info(McL_logo) 

print(info)

# McL_logo_resized <- image_resize(McL_logo, '3650x2000')
# 
# image_write(McL_logo_resized, here("00_raw_data", "McLaren_logo3_resized.png"), format = "png", quality = 75)
# 
# print(info)

image_path <- "./00_raw_data/"

mclaren_logo <- paste0(image_path, 'McLaren_logo1.png')

# logo_anim = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110),
#                        y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
#                        t = 1:20,
#                        McL=rep("dtP1", 20),
#                        Image = rep(mclaren_logo, 20))

dTP1_alt = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115),
                      y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
                      t = 1:21,
                      McL=rep("dtP1", 21),
                      Image = rep(mclaren_logo, 21))

# dTP1_alt <- dTP1_alt %>% 
#   mutate(logo_size = seq(from = 0.05, to = 0.4, len =20))
# 
# glimpse(dTP1_alt)
# 
# dTP1_alt <- as_tibble(dTP1_alt)

mosaic_bruce %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = 'black') +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 2) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "floralwhite"),
        # plot.caption = element_text(size = 16, family = "zen"),
        panel.border = element_rect(colour = "#FF8000", fill=NA, linewidth=2)) +
  expand_limits(y = 80) +
  annotate(
    "text",
    x = 60,
    y = 53,
    label = 'racer | innovator | visionary',
    hjust = 0.5,
    vjust = 0.5,
    size = 6,
    colour = 'grey20',
    fontface = "bold",
    family = "zen") +
  annotate(
    "text",
    x = 60,
    y = 65,
    label = 'Bruce',
    hjust = 0.5,
    vjust = 0.5,
    size = 48,
    colour = '#FF8000',
    fontface = "bold",
    family = "zen") +
  geom_image(data = dTP1_alt, aes(x = x,
                                  y = y, 
                                  image = Image,
                                  group = Image), size = case_when(dTP1_alt$t <= 4 ~ 0.05,
                                                                   dTP1_alt$t >4 & dTP1_alt$t <= 6 ~ 0.1,
                                                                   dTP1_alt$t >6 & dTP1_alt$t <= 9 ~ 0.15,
                                                                   dTP1_alt$t >9 & dTP1_alt$t <= 12 ~ 0.2,
                                                                   dTP1_alt$t >12 & dTP1_alt$t <= 15 ~ 0.225,
                                                                   dTP1_alt$t >15 & dTP1_alt$t <= 18 ~ 0.25,
                                                                   dTP1_alt$t >18 & dTP1_alt$t <= 20 ~ 0.275,
                                                                   TRUE ~ 0.3)) -> plot

plot

mosaic_bruce %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = '#73AEA4',
          show.legend = FALSE) +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=3)) -> plot

plot
ggsave('./03_plots/bruce_car2.png', dpi = 350, height = 2.8, width = 4.2, units = 'in')

# animate with the t variable
plot_bruce <- plot + gganimate::transition_time(t) + gganimate::ease_aes('linear')

# animate with the color variable
plot_bruce <- plot + gganimate::transition_states(color, transition_length = 3, state_length = 1) + shadow_wake(wake_length = 0.05) 

animate(plot_bruce, fps = 30, duration = 20, end_pause = 20, height = 600, width = 600)
anim_save("./04_gifs/animation_bruce3.gif")

animate(plot_bruce, nframes = 200, end_pause = 10, height = 600, width = 600)
anim_save("./04_gifs/animation_bruce3.gif")

# animate(plot_bruce, nframes = 65, height = 150, width = 150)
# for the bruce, racer, innovator, visionary wider than taller gif
animate(plot_bruce, fps = 30, duration = 6.5, end_pause = 10, height = 400, width = 600)
anim_save("./04_gifs/animation_bruce10.gif")

plot + transition_states(color)

bruce_animate <- plot +
  transition_states(color, transition_length = 3, state_length = 3, wrap = FALSE) + 
  shadow_mark() +
  enter_fade() +
  exit_fade()


animate(bruce_animate, nframes = 250, end_pause = 100, height = 600, width = 600)
#animate(bruce_animate, fps = 30, duration = 20, end_pause = 100)

#https://stackoverflow.com/questions/64037373/gganimate-data-present-only-in-some-frames

# McL_logo <- image_read('./00_raw_data/McLaren_logo1.png')
# 
# info <- image_info(McL_logo) 
# 
# print(info)
# 
# McL_logo_resized <- image_resize(McL_logo, '3650x2000')
# 
# image_write(McL_logo_resized, here("00_raw_data", "McLaren_logo3_resized.png"), format = "png", quality = 75)
# 
# print(info)
# 
# image_path <- "./00_raw_data/"
# 
# mclaren_logo <- paste0(image_path, 'McLaren_logo1.png')
# 
# logo_anim = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110),
#                   y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
#                   t = 1:20,
#                   McL=rep("dtP1", 20),
#                   Image = rep(mclaren_logo, 20))
# 
# dTP1_alt = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115),
#                        y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
#                        t = 1:21,
#                        McL=rep("dtP1", 21),
#                        Image = rep(mclaren_logo, 21))
# 
# dTP1_alt <- dTP1_alt %>% 
#   mutate(logo_size = seq(from = 0.05, to = 0.4, len =20))
# 
# glimpse(dTP1_alt)
# 
# dTP1_alt <- as_tibble(dTP1_alt)

# 13. Animate patterns----

bruce <- image_read('./00_raw_data/BruceMcLaren.jpeg.webp')

info <- image_info(bruce) 

print(info)

# flag - use for random pattern
chequered_pattern <- image_crop(bruce, geometry = "150x150+360+180")

image_write(chequered_pattern, path = './00_raw_data/chequered_pattern1.jpeg', format = "jpeg")

chequered_pattern1 <- './00_raw_data/chequered_pattern1.jpeg'

# Load and convert to grayscale
load.image(chequered_pattern1) %>%
  grayscale() -> img

plot(img)

chequered_pattern1_rs <- imager::imresize(img, 
                                          scale = 1/2, 
                                          interpolation = 6)

plot(chequered_pattern1_rs)

chequered_pattern_df <- chequered_pattern1_rs %>%
  as.data.frame() %>%
  mutate(y = y - max(y))

chequered_pattern_df <- chequered_pattern_df %>% 
  # Reverse the y axis
  mutate(y = -(y - max(y)),
         # The modulus of x + y can be used to create a checkerboard pattern
         tiles = case_when((x + y) %% 2 == 0 ~ "Ac",
                           (x + y) %% 2 == 1 ~ "Dc"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

df_chequered_pattern_small <- chequered_pattern_df %>% 
  filter(x>19 & x<31, 
         y>19 & y<31)

df_chequered_pattern_very_small <- chequered_pattern_df %>% 
  filter(x>19 & x<22, 
         y>19 & y<22)

# Start a timer
start_time <- Sys.time()

# Assemble mosaic
mosaic_chequered_pattern <- st_truchet_fm(df = df_chequered_pattern_very_small %>% 
                                mutate(b = b * 0.99 + 0.001))

# End timer
end_time <- Sys.time()

# Calculate time
end_time - start_time

mosaic_chequered_pattern <- mosaic_chequered_pattern %>% mutate(id = as.numeric(id))

glimpse(mosaic_chequered_pattern)

mosaic_chequered_pattern %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = '#73AEA4',
          show.legend = FALSE) +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=3)) -> plot

plot

rotation = function(a, x, y, type, b){
  tile <- st_truchet_flex(type = type, b = b)
  rm <- matrix(c(cos(a), sin(a), 
                 -sin(a), cos(a)),
               nrow = 2, 
               ncol = 2)
  tile %>%
    mutate(geometry = st_geometry(tile) * rm + c(x, y)) %>%
    st_sf()
} 

pause <- 60
steps_anim <- 180

a <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
       rep(pi/2, pause), # Pause at pi/2 for `pause` 
       seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(pi, pause), # Pause at pi for `pause` 
       seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
       rep(3 * pi/2, pause), # Pause at 3 * pi/2 for `pause` 
       seq(3 * pi/2, 2 * pi, pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(2 * pi, pause)) # Pause at 2 * pi for `pause` 

a_alt <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
           seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
           seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
           seq(3 * pi/2, 2 * pi, pi/steps_anim)) # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
        

# Set seed
set.seed(7336)

tile_type <- sample(c("Ac", "Bc", "Cc", "Dc"),
                    4, 
                    replace = TRUE)

tile_type_alt <- c("Ac", "Bc", "Ac", "Bc")

# Create a data frame with the spots for the background tiles
background <- data.frame(x = c(1, 1, 2, 2),
                         y = c(1, 2, 2, 1),
                         tiles = tile_type_alt,
                         b = 0.05) %>%
  st_truchet_fm()

#mosaic_chequered_pattern_try <- df_chequered_pattern_very_small  %>% st_truchet_fm() 

ggplot() +
  geom_sf(data = background,
          aes(fill = factor(color)),
          color = NA)+
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c("1" = "#0057b7", "2" = "#ffd700"))

ggplot() +
  geom_sf(data = mosaic_chequered_pattern,
          aes(fill = color),
          color = '#73AEA4',
          show.legend = FALSE) +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=3)) 

# Tile 1
# Initialize an empty data frame
tile_1 <- data.frame()

# Initialize the counter
count <- 0

for(i in a_alt){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1 <- rbind(tile_1,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 1, 
                                      y = 1,
                                      type = tile_type_alt[1],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1 <- tile_1 %>%
  mutate(tile = "1") %>%
  st_sf()

# Tile 2
tile_2 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_2 <- rbind(tile_2,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 2, 
                                      y = 2, 
                                      type = tile_type_alt[2],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2 <- tile_2 %>%
  mutate(tile = "2") %>%
  st_sf()

plot(tile_2)

# Tile 3
tile_3 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_3 <- rbind(tile_3,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 1, 
                                      y = 2, 
                                      type = tile_type_alt[3],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3 <- tile_3 %>%
  mutate(tile = "3") %>%
  st_sf()

# Tile 4
tile_4 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_4 <- rbind(tile_4,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 2, 
                                      y = 1, 
                                      type = tile_type_alt[4],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4 <- tile_4 %>%
  mutate(tile = "4") %>%
  st_sf()

mosaic_tile <- rbind(tile_1,
                     tile_2,
                     tile_3,
                     tile_4)

p <- ggplot() +
  # Render the static background mosaic
  # geom_sf(data = background,
  #         aes(fill = factor(color)),
  #         color = NA) +
  # Render the animated tiles: first the tiles of color 1
  geom_sf(data = mosaic_tile %>%
            filter(color == 1),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  # Render the animated tiles: then the tiles of color 2
  geom_sf(data = mosaic_tile %>%
            filter(color == 2),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c("1" = "#FF8000", "2" = "#B6BABD")) + 
  # "Crop" the mosaic by limiting the extent of the coordinates
  coord_sf(xlim = c(0.5, 2.5),
           ylim = c(0.5, 2.5),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FF8000"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=1)) 

p
   
plot_tiles <- p + gganimate::transition_time(state) 

animate(plot_tiles, 
        fps = 30, 
        duration = 6.5, 
        end_pause = 50, 
        height = 600, width = 600)


anim_save("./04_gifs/animation_four_tiles1.gif")

gganimate::animate(plot_tiles, 
                   rewind = FALSE,
                   #end_pause = 10,
                   fps = 60,
                   duration = 10,
                   res = 300,
                   height = 1.0, 
                   width = 1.0,
                   units = "in")

plot_bruce <- plot + gganimate::transition_states(color, transition_length = 3, state_length = 1) + shadow_wake(wake_length = 0.05) 


# elements of mclaren speedmark----

rotation = function(a, x, y, type, b){
  tile <- st_truchet_flex(type = type, b = b)
  rm <- matrix(c(cos(a), sin(a), 
                 -sin(a), cos(a)),
               nrow = 2, 
               ncol = 2)
  tile %>%
    mutate(geometry = st_geometry(tile) * rm + c(x, y)) %>%
    st_sf()
}

rotation_aj = function(a, x_c, y_c, type, b){
  tile <- pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex) 
  #tile <- aj_truchet_flex(type = type, b = 1/2)
  rm <- matrix(c(cos(a), sin(a), 
                 -sin(a), cos(a)),
               nrow = 2, 
               ncol = 2)
  tile %>%
    mutate(geometry = st_geometry(tile) * rm + c(x_c, y_c)) %>%
    st_sf()
}

pause <- 60
steps_anim <- 180

a1 <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
       rep(pi/2, pause), # Pause at pi/2 for `pause` 
       seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(pi, pause), # Pause at pi for `pause` 
       seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
       rep(3 * pi/2, pause), # Pause at 3 * pi/2 for `pause` 
       seq(3 * pi/2, 2 * pi, pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(2 * pi, pause)) # Pause at 2 * pi for `pause`

a2 <- c(seq(0, -pi/2, -pi/steps_anim), # From zero to -pi/2 in `-pi/steps_anim` increments, basically a 90 degrees rotation
       rep(-pi/2, pause), # Pause at -pi/2 for `pause` 
       seq(-pi/2, -pi, -pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(-pi, pause), # Pause at pi for `pause` 
       seq(-pi, 3 * -pi/2, -pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
       rep(3 * -pi/2, pause), # Pause at 3 * pi/2 for `pause` 
       seq(3 * -pi/2, 2 * -pi, -pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(2 * pi, pause)) # Pause at 2 * pi for `pause`

a3 <- c(seq(0, pi/4, pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(pi/4, pause*0.57142857), # Pause at -pi/4 for `pause`
        seq(pi/4, pi/2, pi/steps_anim), # From -pi/4 to -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/2, pause*0.57142857), # Pause at -pi/2 for `pause`
        seq(pi/2, pi/1.333, pi/steps_anim), # From -pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/1.333, pause*0.57142857), # Pause at -pi for `pause`
        seq(pi/1.333, pi, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi, pause*0.57142857), # Pause at -pi for `pause`
        seq(pi, 1.667 * pi/1.333, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(1.667 * pi/1.333, pause*0.57142857), # Pause at 3 * -pi/2 for `pause`
        seq(1.667 * pi/1.333, 3 * pi/2, pi/steps_anim), # From 3 * -pi/4 to 2 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(3 * pi/2, pause*0.57142857), # Pause at 2 * -pi/2 for `pause`
        seq(3 * pi/2, 2 * pi, pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(2 * pi, pause*0.57142857)) # Pause at 2 * pi for `pause`

a3_rev <- c(seq(2 * -pi, 3 * -pi/2, pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
            rep(3 * -pi/2, pause*0.57142857), # Pause at -pi/4 for `pause`
            seq(3 * -pi/2, 1.667 * -pi/1.333, pi/steps_anim), # From -pi/4 to -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
            rep(1.667 * -pi/1.333, pause*0.57142857), # Pause at -pi/2 for `pause`
            seq(1.667 * -pi/1.333, -pi, pi/steps_anim), # From -pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
            rep(-pi, pause*0.57142857), # Pause at -pi for `pause`
            seq(-pi, -pi/1.333, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
            rep(-pi/1.333, pause*0.57142857), # Pause at -pi for `pause`
            seq(-pi/1.333, -pi/2, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
            rep(-pi/2, pause*0.57142857), # Pause at 3 * -pi/2 for `pause`
            seq(-pi/2, -pi/4, pi/steps_anim), # From 3 * -pi/4 to 2 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
            rep(-pi/4, pause*0.57142857), # Pause at 2 * -pi/2 for `pause`
            seq(-pi/4, 0, pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
            rep(0, pause*0.57142857)) # Pause at 2 * pi for `pause'


a4 <- c(seq(0, -pi/4, -pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(-pi/4, pause*0.57142857), # Pause at -pi/4 for `pause`
        seq(-pi/4, -pi/2, -pi/steps_anim), # From -pi/4 to -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/2, pause*0.57142857), # Pause at -pi/2 for `pause`
        seq(-pi/2, -pi/1.333, -pi/steps_anim), # From -pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/1.333, pause*0.57142857), # Pause at -pi for `pause`
        seq(-pi/1.333, -pi, -pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi, pause*0.57142857), # Pause at -pi for `pause`
        seq(-pi, 1.667 * -pi/1.333, -pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(1.667 * -pi/1.333, pause*0.57142857), # Pause at 3 * -pi/2 for `pause`
        seq(1.667 * -pi/1.333, 3 * -pi/2, -pi/steps_anim), # From 3 * -pi/4 to 2 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(3 * -pi/2, pause*0.57142857), # Pause at 2 * -pi/2 for `pause`
        seq(3 * -pi/2, 2 * -pi, -pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(2 * pi, pause*0.57142857)) # Pause at 2 * pi for `pause`

a5 <- c(seq(0, pi/8, pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(pi/8, pause*0.25), # Pause at -pi/4 for `pause`
        seq(pi/8, pi/4, pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(pi/4, pause*0.25), # Pause at -pi/4 for `pause`
        seq(pi/4, pi/2.667, pi/steps_anim), # From -pi/4 to -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/2.667, pause*0.25), # Pause at -pi/2 for `pause`
        seq(pi/2.667, pi/2, pi/steps_anim), # From -pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/2, pause*0.25), # Pause at -pi for `pause`
        seq(pi/2, pi/1.6, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/1.6, pause*0.25), # Pause at -pi for `pause`
        seq(pi/1.6, pi/1.333, pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/1.333, pause*0.25), # Pause at 3 * -pi/2 for `pause`
        seq(pi/1.333, pi/1.143, pi/steps_anim), # From 3 * -pi/4 to 2 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(pi/1.143, pause*0.25),
        seq(pi/1.143, pi, pi/steps_anim),
        rep(pi, pause*0.25),
        seq(pi, 1.125*pi, pi/steps_anim),
        rep(1.125*pi, pause*0.25),
        seq(1.125*pi, 1.25*pi, pi/steps_anim),
        rep(1.25*pi, pause*0.25),
        seq(1.25*pi, 1.375*pi, pi/steps_anim),
        rep(1.375*pi, pause*0.25),
        seq(1.375*pi, 1.5*pi, pi/steps_anim),
        rep(1.5*pi, pause*0.25),
        seq(1.5*pi, 1.625*pi, pi/steps_anim),
        rep(1.625*pi, pause*0.25),
        seq(1.625*pi, 1.75*pi, pi/steps_anim),
        rep(1.75*pi, pause*0.25),
        seq(1.75*pi, 1.875*pi, pi/steps_anim),
        rep(1.875*pi, pause*0.25),
        seq(1.875*pi, 2*pi, pi/steps_anim),
        rep(2*pi, pause*0.25)) 

a6 <- c(seq(0, -pi/8, -pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(-pi/8, pause*0.25), # Pause at -pi/4 for `pause`
        seq(-pi/8, -pi/4, -pi/steps_anim), # From zero to -pi/4 in `-pi/steps_anim` increments, basically a 90 degrees rotation
        rep(-pi/4, pause*0.25), # Pause at -pi/4 for `pause`
        seq(-pi/4, -pi/2.667, -pi/steps_anim), # From -pi/4 to -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/2.667, pause*0.25), # Pause at -pi/2 for `pause`
        seq(-pi/2.667, -pi/2, -pi/steps_anim), # From -pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/2, pause*0.25), # Pause at -pi for `pause`
        seq(-pi/2, -pi/1.6, -pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/1.6, pause*0.25), # Pause at -pi for `pause`
        seq(-pi/1.6, -pi/1.333, -pi/steps_anim), # From -pi to 3 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/1.333, pause*0.25), # Pause at 3 * -pi/2 for `pause`
        seq(-pi/1.333, -pi/1.143, -pi/steps_anim), # From 3 * -pi/4 to 2 * -pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
        rep(-pi/1.143, pause*0.25),
        seq(-pi/1.143, -pi, -pi/steps_anim),
        rep(-pi, pause*0.25),
        seq(-pi, -1.125*pi, -pi/steps_anim),
        rep(-1.125*pi, pause*0.25),
        seq(-1.125*pi, -1.25*pi, -pi/steps_anim),
        rep(-1.25*pi, pause*0.25),
        seq(-1.25*pi, -1.375*pi, -pi/steps_anim),
        rep(-1.375*pi, pause*0.25),
        seq(-1.375*pi, -1.5*pi, -pi/steps_anim),
        rep(-1.5*pi, pause*0.25),
        seq(-1.5*pi, -1.625*pi, -pi/steps_anim),
        rep(-1.625*pi, pause*0.25),
        seq(-1.625*pi, -1.75*pi, -pi/steps_anim),
        rep(-1.75*pi, pause*0.25),
        seq(-1.75*pi, -1.875*pi, -pi/steps_anim),
        rep(-1.875*pi, pause*0.25),
        seq(-1.875*pi, -2*pi, -pi/steps_anim),
        rep(2*pi, pause*0.25))
        

# a_alt <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
#            seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
#            seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
#            seq(3 * pi/2, 2 * pi, pi/steps_anim)) # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
# 
# a_alt2 <- c(seq(0, -pi/2, -pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
#             seq(-pi/2, -pi, -pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
#             seq(-pi, 3 * -pi/2, -pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
#             seq(3 * -pi/2, 2 * -pi, -pi/steps_anim)) # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation


# Set seed
set.seed(7336)

tile_type <- sample(c("Ac", "Bc", "Cc", "Dc"),
                    4, 
                    replace = TRUE)

tile_type_alt_aj <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")

# Create a data frame with the spots for the background tiles
background_aj <- data.frame(x = c(1, 1, 1, 1),
                            y = c(1, 1, 1, 1),
                            tiles = tile_type_alt_aj,
                            b = 1/2) %>%
  aj_truchet_flex()

tile_types <- data.frame(type = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")) %>%
  mutate(x = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
         y = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
         b = 1/2)

# Elements for assembling the mosaic
x_c <- tile_types$x
y_c <- tile_types$y
type <- as.character(tile_types$type)
b <- tile_types$b

tile_types_alt <- data.frame(type = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")) %>%
  mutate(x = runif(11, 0.5, 1.5),
         y = runif(11, 0.5, 1.5),
         b = 1/2)

# Elements for assembling the mosaic
x_c_alt <- tile_types_alt$x
y_c_alt <- tile_types_alt$y
type_alt <- as.character(tile_types_alt$type)
b_alt <- tile_types_alt$b


# test <- pmap_dfr(list(x_c, y_c, type, b), aj_truchet_flex)
# mosaic_chequered_pattern_try <- df_chequered_pattern_very_small  %>% st_truchet_fm() 

pmap_dfr(list(x_c_alt, y_c_alt, type_alt, b_alt), aj_truchet_flex) %>%
  #mutate(colour = tidyr::replace_na(colour, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = colour),
          color = "black",
          size = 2)+
  scale_fill_distiller(direction = 1) +
  theme_void() +
  theme(legend.position = "none")

pmap_dfr(list(x_c_alt, y_c_alt, type_alt, b_alt), aj_truchet_flex) %>%
  #mutate(colour = tidyr::replace_na(colour, 1)) %>% 
  ggplot() +
  geom_sf(aes(fill = colour),
          color = '#73AEA4',
          show.legend = FALSE) +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=3)) 

# Tile 1
# Initialize an empty data frame
tile_1 <- data.frame()

# Initialize the counter
count <- 0

for(i in a1){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1 <- rbind(tile_1,
                  data.frame(rotation_aj(a = i, 
                                      # Coordinates of tile
                                      x_c = 1, 
                                      y_c = 1,
                                      type = tile_type_alt_aj[1],
                                      b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1 <- tile_1 %>%
  mutate(tile = "1") %>%
  st_sf() %>% 
  slice(1:596) 

# Tile 2
tile_2 <- data.frame()
count <- 0

for(i in a2){
  count <- count + 1
  tile_2 <- rbind(tile_2,
                  data.frame(rotation_aj(a = i, 
                                      # Coordinates of tile
                                      x_c = 1, 
                                      y_c = 1, 
                                      type = tile_type_alt_aj[2],
                                      b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2 <- tile_2 %>%
  mutate(tile = "2") %>%
  st_sf() %>% 
  slice(1:596)


# Tile 3
tile_3 <- data.frame()
count <- 0

for(i in a3_rev){
  count <- count + 1
  tile_3 <- rbind(tile_3,
                  data.frame(rotation_aj(a = i, 
                                      # Coordinates of tile
                                      x_c = 1, 
                                      y_c = 1, 
                                      type = tile_type_alt_aj[3],
                                      b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3 <- tile_3 %>%
  mutate(tile = "3") %>%
  st_sf() %>% 
  slice(1:596)


# Tile 4
tile_4 <- data.frame()
count <- 0

for(i in a4){
  count <- count + 1
  tile_4 <- rbind(tile_4,
                  data.frame(rotation_aj(a = i, 
                                      # Coordinates of tile
                                      x_c = 1, 
                                      y_c = 1, 
                                      type = tile_type_alt_aj[4],
                                      b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4 <- tile_4 %>%
  mutate(tile = "4") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 5
# Initialize an empty data frame
tile_5 <- data.frame()

# Initialize the counter
count <- 0

for(i in a5){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_5 <- rbind(tile_5,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[5],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_5 <- tile_5 %>%
  mutate(tile = "5") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 6
# Initialize an empty data frame
tile_6 <- data.frame()

# Initialize the counter
count <- 0

for(i in a6){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_6 <- rbind(tile_6,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[6],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_6 <- tile_6 %>%
  mutate(tile = "6") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 7
# Initialize an empty data frame
tile_7 <- data.frame()

# Initialize the counter
count <- 0

for(i in a1){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_7 <- rbind(tile_7,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[7],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_7 <- tile_7 %>%
  mutate(tile = "7") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 8
# Initialize an empty data frame
tile_8 <- data.frame()

# Initialize the counter
count <- 0

for(i in a2){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_8 <- rbind(tile_8,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[8],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_8 <- tile_8 %>%
  mutate(tile = "8") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 9
# Initialize an empty data frame
tile_9 <- data.frame()

# Initialize the counter
count <- 0

for(i in a3_rev){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_9 <- rbind(tile_9,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[9],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_9 <- tile_9 %>%
  mutate(tile = "9") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 10
# Initialize an empty data frame
tile_10 <- data.frame()

# Initialize the counter
count <- 0

for(i in a4){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_10 <- rbind(tile_10,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[10],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_10 <- tile_10 %>%
  mutate(tile = "10") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 11
# Initialize an empty data frame
tile_11 <- data.frame()

# Initialize the counter
count <- 0

for(i in a5){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_11 <- rbind(tile_11,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 1, 
                                         y_c = 1,
                                         type = tile_type_alt_aj[11],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_11 <- tile_11 %>%
  mutate(tile = "11") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 1a
# Initialize an empty data frame
tile_1a <- data.frame()

# Initialize the counter
count <- 0

for(i in a2){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1a <- rbind(tile_1a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[1],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1a <- tile_1a %>%
  mutate(tile = "1a") %>%
  st_sf() %>% 
  slice(1:596) 

# Tile 2a
tile_2a <- data.frame()
count <- 0

for(i in a3_rev){
  count <- count + 1
  tile_2a <- rbind(tile_2a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5, 
                                         type = tile_type_alt_aj[2],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2a <- tile_2a %>%
  mutate(tile = "2a") %>%
  st_sf() %>% 
  slice(1:596)


# Tile 3a
tile_3a <- data.frame()
count <- 0

for(i in a4){
  count <- count + 1
  tile_3a <- rbind(tile_3a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5, 
                                         type = tile_type_alt_aj[3],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3a <- tile_3a %>%
  mutate(tile = "3a") %>%
  st_sf() %>% 
  slice(1:596)


# Tile 4a
tile_4a <- data.frame()
count <- 0

for(i in a5){
  count <- count + 1
  tile_4a <- rbind(tile_4a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5, 
                                         type = tile_type_alt_aj[4],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4a <- tile_4a %>%
  mutate(tile = "4a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 5a
# Initialize an empty data frame
tile_5a <- data.frame()

# Initialize the counter
count <- 0

for(i in a6){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_5a <- rbind(tile_5a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[5],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_5a <- tile_5a %>%
  mutate(tile = "5a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 6a
# Initialize an empty data frame
tile_6a <- data.frame()

# Initialize the counter
count <- 0

for(i in a1){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_6a <- rbind(tile_6a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[6],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_6a <- tile_6a %>%
  mutate(tile = "6a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 7a
# Initialize an empty data frame
tile_7a <- data.frame()

# Initialize the counter
count <- 0

for(i in a2){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_7a <- rbind(tile_7a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[7],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_7a <- tile_7a %>%
  mutate(tile = "7a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 8a
# Initialize an empty data frame
tile_8a <- data.frame()

# Initialize the counter
count <- 0

for(i in a3_rev){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_8a <- rbind(tile_8a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[8],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_8a <- tile_8a %>%
  mutate(tile = "8a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 9a
# Initialize an empty data frame
tile_9a <- data.frame()

# Initialize the counter
count <- 0

for(i in a4){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_9a <- rbind(tile_9a,
                  data.frame(rotation_aj(a = i, 
                                         # Coordinates of tile
                                         x_c = 0.5, 
                                         y_c = 0.5,
                                         type = tile_type_alt_aj[9],
                                         b = 1/2), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_9a <- tile_9a %>%
  mutate(tile = "9a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 10a
# Initialize an empty data frame
tile_10a <- data.frame()

# Initialize the counter
count <- 0

for(i in a5){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_10a <- rbind(tile_10a,
                   data.frame(rotation_aj(a = i, 
                                          # Coordinates of tile
                                          x_c = 0.5, 
                                          y_c = 0.5,
                                          type = tile_type_alt_aj[10],
                                          b = 1/2), 
                              state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_10a <- tile_10a %>%
  mutate(tile = "10a") %>%
  st_sf() %>% 
  slice(1:596)

# Tile 11a
# Initialize an empty data frame
tile_11a <- data.frame()

# Initialize the counter
count <- 0

for(i in a6){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_11a <- rbind(tile_11a,
                   data.frame(rotation_aj(a = i, 
                                          # Coordinates of tile
                                          x_c = 0.5, 
                                          y_c = 0.5,
                                          type = tile_type_alt_aj[11],
                                          b = 1/2), 
                              state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_11a <- tile_11a %>%
  mutate(tile = "11a") %>%
  st_sf() %>% 
  slice(1:596)

# normal truchet 4 tiles rotating----

a <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
       rep(pi/2, pause), # Pause at pi/2 for `pause` 
       seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(pi, pause), # Pause at pi for `pause` 
       seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
       rep(3 * pi/2, pause), # Pause at 3 * pi/2 for `pause` 
       seq(3 * pi/2, 2 * pi, pi/steps_anim), # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation
       rep(2 * pi, pause)) # Pause at 2 * pi for `pause` 

a_alt <- c(seq(0, pi/2, pi/steps_anim), # From zero to pi/2 in `pi/steps_anim` increments, basically a 90 degrees rotation
           seq(pi/2, pi, pi/steps_anim), # From pi/2 to pi in `pi/steps_anim` increments, another 90 degrees rotation
           seq(pi, 3 * pi/2, pi/steps_anim), # From pi to 3 * pi/2 in `pi/steps_anim` increments, another 90 degrees rotation
           seq(3 * pi/2, 2 * pi, pi/steps_anim)) # From 3 * pi/2 to 2 * pi in `pi/steps_anim` increments, another 90 degrees rotation


# Set seed
set.seed(7336)

tile_type <- sample(c("Ac", "Bc", "Cc", "Dc"),
                    4, 
                    replace = TRUE)

tile_type_alt <- c("Ac", "Bc", "Ac", "Bc")

# Create a data frame with the spots for the background tiles
background <- data.frame(x = c(1, 1, 2, 2),
                         y = c(1, 2, 2, 1),
                         tiles = tile_type_alt,
                         b = 0.05) %>%
  st_truchet_fm()

#mosaic_chequered_pattern_try <- df_chequered_pattern_very_small  %>% st_truchet_fm() 

ggplot() +
  geom_sf(data = background,
          aes(fill = factor(color)),
          color = NA)+
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c("1" = "#0057b7", "2" = "#ffd700"))

ggplot() +
  geom_sf(data = mosaic_chequered_pattern,
          aes(fill = color),
          color = '#73AEA4',
          show.legend = FALSE) +
  scale_fill_gradient2(low = "white", 
                       mid = "grey", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=3)) 

# Tile 1
# Initialize an empty data frame
tile_1_s <- data.frame()

# Initialize the counter
count <- 0

for(i in a_alt){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1_s <- rbind(tile_1_s,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 0.5, 
                                      y = 0.5,
                                      type = tile_type_alt[1],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1_s <- tile_1_s %>%
  mutate(tile = "1") %>%
  st_sf()

# Tile 2
tile_2_s <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_2_s <- rbind(tile_2_s,
                    data.frame(rotation(a = i, 
                                        # Coordinates of tile
                                        x = 1.5, 
                                        y = 1.5, 
                                        type = tile_type_alt[2],
                                        b = count*0.0013), 
                               state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2_s <- tile_2_s %>%
  mutate(tile = "2") %>%
  st_sf()

#plot(tile_2)

# Tile 3
tile_3_s <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_3_s <- rbind(tile_3_s,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 0.5, 
                                      y = 1.5, 
                                      type = tile_type_alt[3],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3_s <- tile_3_s %>%
  mutate(tile = "3") %>%
  st_sf()

# Tile 4
tile_4_s <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_4_s <- rbind(tile_4_s,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 1.5, 
                                      y = 0.5, 
                                      type = tile_type_alt[4],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4_s <- tile_4_s %>%
  mutate(tile = "4") %>%
  st_sf()

mosaic_tile_s <- rbind(tile_1_s,
                       tile_2_s,
                       tile_3_s,
                       tile_4_s)


mosaic <- rbind(tile_1,
                tile_2,
                #tile_3,
                tile_4,
                tile_5,
                #tile_6,
                tile_7,
                tile_8,
                tile_9,
                #tile_10,
                tile_11,
                tile_3a,
                tile_6a,
                tile_10a)

mosaic_a <- rbind(tile_1a,
                  tile_2a,
                  tile_3a,
                  tile_4a,
                  tile_5a,
                  tile_6a,
                  tile_7a,
                  tile_8a,
                  tile_9a,
                  tile_10a,
                  tile_11a)

mosaic_with_a <- rbind(mosaic,
                       mosaic_a)

mosaic_tile_s_colour <- mosaic_tile_s %>% 
  rename(colour = color)

mosaic <- mosaic %>% 
  bind_rows(mosaic_tile_s_colour)

mosaic <- rbind(tile_1,
                tile_2,
                tile_3,
                tile_4,
                tile_8,
                tile_9,
                tile_11)

mosaic <- rbind(tile_1,
                tile_2,
                tile_3,
                tile_4,
                tile_5,
                tile_6)

image_path <- "./00_raw_data/"

mclaren_logo <- paste0(image_path, 'McLaren_logo1.png')

mclaren_logo_outline <- paste0(image_path, 'McL_speedmark_outline.png')

dTP1_alt = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115),
                      y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
                      t = 1:21,
                      McL=rep("dtP1", 21),
                      Image = rep(mclaren_logo, 21))

image_tibble <- tibble(x = runif(596, 0.5, 1.5),
                       y = runif(596, 0.5, 1.5),
                       state = as.numeric(1:596),
                       Image = rep(mclaren_logo, 596),
                       Size = runif(596, 0.01, 0.05),
                       angle = rep(c(pi/3, 2*pi/3, pi, pi/6), 149))

image_tibble_alt <- tibble(x = 1,
                           y = 0.8,
                           state = as.numeric(1:596),
                           Image = rep(mclaren_logo, 596),
                           #Size = runif(596, 0.01, 0.05),
                           Size = seq(0.01, 0.05, length.out = 596),
                           angle = rep(c(pi/3, 2*pi/3, pi, pi/6), 149))

image_tibble_less_logo <- image_tibble_alt %>% 
  mutate(less_image = if_else(row_number() %% 20 == 0, Image, NA))

glimpse(mosaic)

p <- ggplot() +
  # Render the static background mosaic
  # geom_sf(data = background,
  #         aes(fill = factor(color)),
  #         color = NA) +
  # Render the animated tiles: first the tiles of color 1
  # geom_sf(data = mosaic %>%
  #           filter(colour == 1),
  #         aes(fill = factor(colour),
  #             # It is important to group by `state` and `tile` otherwise the animation gets wacky
  #             group = interaction(state, tile)),
  #         color = NA) +
  # Render the animated tiles: then the tiles of color 2
with_outer_glow(geom_sf(data = mosaic %>%
            filter(colour == 2),
          aes(fill = factor(tile),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA,
          alpha = 0.5), colour = "#FF8000", sigma = 5, expand = 5) +
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c('1' = '#FF0000', '2' = '#FF2000', 
                               '3' = 'grey70', '4' = '#FF6000',
                               '5' = '#FF8000', '6' = '#FF1000',
                               '7' = '#FF3000', '8' = '#FF5000',
                               '9' = '#FF7000', '10' = '#FF0000',
                               '11' = '#FF2000',
                               '1a' = '#FF0000', '2a' = '#FF2000', 
                               '3a' = 'grey70', '4a' = '#FF6000',
                               '5a' = '#FF8000', '6a' = '#FF1000',
                               '7a' = '#FF3000', '8a' = '#FF5000',
                               '9a' = '#FF7000', '10a' = '#FF0000',
                               '11a' = '#FF2000')) +
  # with_outer_glow(geom_point(data = stars_tibble %>% group_by(t),
  #                            aes(x = x,
  #                                y = y), colour = 'black'), colour = "#FF8000", sigma = 5, expand = 5) +
  # with_outer_glow(geom_image(data = image_tibble_alt, 
  #                            aes(x = x,
  #                                y = y, 
  #                                image = Image,
  #                                group = Image), size = seq(0.01, 0.65, length.out = 596)), colour = "#FF8000", sigma = 5, expand = 5) +
  # "Crop" the mosaic by limiting the extent of the coordinates
  coord_sf(xlim = c(-1, 3),
           ylim = c(-1, 3),
           expand = FALSE) +
  #darklyplot::theme_dark2() 
  theme_void() +
  # theme(legend.position = "none",
  #       panel.background = element_rect(fill='transparent'),
  #       plot.background = element_rect(fill='transparent', color=NA),
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       legend.background = element_rect(fill='transparent'),
  #       legend.box.background = element_rect(fill='transparent'))
theme(legend.position = "none",
        plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#FF8000", fill=NA, linewidth=2)) 

# case_when(mosaic$state <= 100 ~ 0.05,
#           mosaic$state >100 & mosaic$state <= 200 ~ 0.1,
#           mosaic$state >200 & mosaic$state <= 300 ~ 0.15,
#           mosaic$state >300 & mosaic$state <= 400 ~ 0.2,
#           mosaic$state >400 & mosaic$state <= 500 ~ 0.225,
#           TRUE ~ 0.25)

p

plot_tiles <- p + gganimate::transition_time(state) 

animate(plot_tiles, 
        fps = 30, 
        duration = 13, 
        end_pause = 20, 
        height = 600, width = 600)

anim_save("./04_gifs/animation_four_McL_shapes4.gif")

gganimate::animate(plot_tiles, 
                   rewind = FALSE,
                   #end_pause = 10,
                   fps = 60,
                   duration = 10,
                   res = 300,
                   height = 1.0, 
                   width = 1.0,
                   units = "in")

plot_bruce <- plot + gganimate::transition_states(color, transition_length = 3, state_length = 1) + shadow_wake(wake_length = 0.05) 

p <- map_dfr(1:25, ~tibble(y = seq(1, .x, length.out = 25), t = 1:25)) %>% 
  mutate(x = runif(n())) %>% 
  slice(1:596) %>%
  ggplot(aes(x, y)) +
  geom_point(color = 'white') +
  coord_polar() +
  transition_states(t) + shadow_wake(0.5)


stars_tibble <- map_dfr(1:25, ~tibble(y = seq(1, .x, length.out = 25), t = 1:25)) %>% 
  mutate(x = runif(n())) %>% 
  slice(1:596) %>% 
  mutate(state = as.numeric(1:596),
         y = y/10)

glimpse(stars_tibble) 

# wave patterns----
# https://github.com/aschinchon/spinning-pins/blob/master/spinning_pins.R

# 1st pattern
n_points  <- 2
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 2*pi, length.out = n_points)

# 2nd pattern
n_points  <- 20
closeness <- 0
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/2, length.out = n_points)

# 3rd pattern
n_points  <- 20
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 0, length.out = n_points)

# 4th pattern
n_points  <- 20
closeness <- pi/4
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/4, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

mosaic_tile_numeric <- mosaic_tile %>% 
  rename(frame = state) %>% 
  mutate(frame = as.integer(frame))

# Plot pins using frame as transition time
ggplot(df) +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 1) +
  geom_point(aes(x+cos(angle), y+sin(angle)), size=4) +
  # geom_sf(data = mosaic_tile %>%
  #           filter(color == 1),
  #         aes(fill = factor(color),
  #             # It is important to group by `state` and `tile` otherwise the animation gets wacky
  #             group = interaction(state, tile)),
  #         color = NA) +
  theme_void() + 
  coord_fixed() +
  transition_time(time=frame)

glimpse(mosaic_tile_numeric)

# 4 tiles----
# Tile 1
# Initialize an empty data frame
tile_1 <- data.frame()

# Initialize the counter
count <- 0

for(i in a_alt){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1 <- rbind(tile_1,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 1, 
                                      y = 1,
                                      type = tile_type_alt[1],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1 <- tile_1 %>%
  mutate(tile = "1") %>%
  st_sf()

# Tile 2
tile_2 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_2 <- rbind(tile_2,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 2, 
                                      y = 2, 
                                      type = tile_type_alt[2],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2 <- tile_2 %>%
  mutate(tile = "2") %>%
  st_sf()

plot(tile_2)

# Tile 3
tile_3 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_3 <- rbind(tile_3,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 1, 
                                      y = 2, 
                                      type = tile_type_alt[3],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3 <- tile_3 %>%
  mutate(tile = "3") %>%
  st_sf()

# Tile 4
tile_4 <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_4 <- rbind(tile_4,
                  data.frame(rotation(a = i, 
                                      # Coordinates of tile
                                      x = 2, 
                                      y = 1, 
                                      type = tile_type_alt[4],
                                      b = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4 <- tile_4 %>%
  mutate(tile = "4") %>%
  st_sf()

mosaic_tile <- rbind(tile_1,
                     tile_2,
                     tile_3,
                     tile_4)

p <- ggplot() +
  # Render the static background mosaic
  # geom_sf(data = background,
  #         aes(fill = factor(color)),
  #         color = NA) +
  # Render the animated tiles: first the tiles of color 1
  geom_sf(data = mosaic_tile %>%
            filter(color == 1),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  # Render the animated tiles: then the tiles of color 2
  geom_sf(data = mosaic_tile %>%
            filter(color == 2),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c("1" = "#FF8000", "2" = "#B6BABD")) + 
  # "Crop" the mosaic by limiting the extent of the coordinates
  coord_sf(xlim = c(0.5, 2.5),
           ylim = c(0.5, 2.5),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FF8000"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=1)) 

p

plot_tiles <- p + gganimate::transition_time(state) 

animate(plot_tiles, 
        fps = 30, 
        duration = 6.5, 
        end_pause = 50, 
        height = 600, width = 600)

df_test <- expand.grid(x = 1:10, y=1:10)

df_test$angle <- runif(100, 0, 2*pi)
df_test$speed <- runif(100, 0, sqrt(0.1 * df_test$x))

ggplot(df_test, aes(x, y)) +
  geom_point() +
  geom_spoke(aes(angle = angle), radius = 0.5) 

ggplot(df_test, aes(x, y)) +
  geom_point() +
  geom_spoke(aes(angle = angle), radius = speed)

t <- seq(0,
         1,
         length=50)

pts <- matrix( c(0, 1,
                 0.1, 0.1,
                 1, 0),
               nrow=3,
               ncol=2,
               byrow=TRUE)

line_1 <- bezier::bezier(t = t,
                         p = pts)

line_1 <- data.frame(id = 1,
                     geometry = sf::st_linestring(line_1) %>%
                       sf::st_geometry()) %>%
  sf::st_sf()

plot(line_1)

# https://blog.djnavarro.net/posts/2024-11-24_bezier-curves/

t_bez <- seq(0, 1, length=100)

p_bez <- matrix(c(0,0,0, 
                  1,4,3, 
                  2,2,0, 
                  3,0,2, 
                  5,5,0), nrow=5, ncol=3, byrow=TRUE)

bezier_points <- bezier::bezier(t=t_bez, p=p_bez)

bezier_points <- data.frame(id = 1,
                            geometry = sf::st_linestring(bezier_points) %>%
                              sf::st_geometry()) %>%
  sf::st_sf()

plot(bezier_points)

bernstein <- function(beta, t = seq(0, 1, .01)) {
  n <- length(beta) - 1
  w <- choose(n, 0:n)
  b <- rep(0, length(t))
  for(v in 0:n) {
    b = b + beta[v + 1] * w[v + 1] * t^v * (1 - t)^(n-v)
  }
  b
}

control <- tibble(
  x = c(1, 5, 8),
  y = c(1, 1, 6)
)

plot(control)

bezier <- tibble(
  t = seq(0, 1, .01),
  x = bernstein(control$x, t),
  y = bernstein(control$y, t)
)

ggplot() + 
  aes(x, y) +
  geom_path(data = bezier) + 
  geom_point(data = control, color = "red") + 
  coord_equal(xlim = c(0, 10), ylim = c(0, 10)) + 
  theme_bw()

control <- tibble(
  x = c(0, 0.25, 0.5, 0.75, 1),
  y = c(1, 0.1, 0.5, 0.9, 0)
)

plot(control)

bezier <- tibble(
  t = seq(0, 1, .01),
  x = bernstein(control$x, t),
  y = bernstein(control$y, t)
)
ggplot() + 
  aes(x, y) +
  geom_path(data = bezier) + 
  geom_point(data = control, color = "red") + 
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) + 
  theme_bw()

st_truchet_flex_bezier <- function(x = 0, y = 0, type = "Abez", b1 = 1/2, b2 = 1/2, b3 = 1/2, b4 = 1/2){
  
  #' Flexible Truchet tiles
  #'
  #' @param x A number with the x coordinate of the center of the tile
  #' @param y A number with the y coordinate of the center of the tile
  #' @param type A single character to designate a type of tile; currently supported options are "Ac", "Bc", "Cc", "Dc", "As", "Bs", "Cs", "Ds"
  #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
  #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
  #' @importFrom rlang .data
  #' @export
  #' @examples
  #' st_truchet_flex_bezier(type = "Abez")
  #' @note For a discussion of Truchet patterns see: Robert Bosch & Urchin Colley (2013) Figurative mosaics from flexible Truchet tiles, Journal of Mathematics and the Arts, 7:3-4, 122-135, \url{10.1080/17513472.2013.838830}
  
  # Validate inputs
  checkmate::assertChoice(type, c("Abez", "Bbez", "Cbez", "Dbez"))
  # b1 must be a value beween zero and 1
  checkmate::assert_number(b1, lower = 0, upper = 1)
  # b2 must be a value beween zero and 1
  checkmate::assert_number(b2, lower = 0, upper = 1)
  # b3 must be a value beween zero and 1
  checkmate::assert_number(b3, lower = 0, upper = 1)
  # b4 must be a value beween zero and 1
  checkmate::assert_number(b4, lower = 0, upper = 1)
  
  # Adjust values of b1 in case that there is an exact zero or one, which messes up the selection of colors later on
  if(b1 == 0) b1 <- 1/100
  if(b1 == 1) b1 <- 99/100
  
  # Adjust values of b2 in case that there is an exact zero or one, which messes up the selection of colors later on
  if(b2 == 0) b2 <- 1/100
  if(b2 == 1) b2 <- 99/100
  
  # Adjust values of b3 in case that there is an exact zero or one, which messes up the selection of colors later on
  if(b3 == 0) b3 <- 1/100
  if(b3 == 1) b3 <- 99/100
  
  # Adjust values of b4 in case that there is an exact zero or one, which messes up the selection of colors later on
  if(b4 == 0) b4 <- 1/100
  if(b4 == 1) b4 <- 99/100
  
  ## CREATE BASE TILE
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
  
  ## BASE TILE DONE
  
  # Tile types
  
  switch(type,
         
         "Abez" ={
           ## ADORNMENTS
           # Define bezier control points for line
           
           control_Abez <- tibble(
             x = c(0, b1, 0.5, b3, 1),
             y = c(1, b2, 0.5, b4, 0)
           )
           
           line_1 <- bezier::bezier(t = seq(0, 1, .01),
                                    p = control_Abez)
           
           # Convert points to line
           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()
           
           # Split the base tile to give the final tile
           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 2:1)
           ## ADORNMENTS DONE
         },
         
         "Bbez" ={
           ## ADORNMENTS
           # Define bezier control points for line
           
           control_Bbez <- tibble(
             x = c(0, b1, 0.5, 1 - b3, 1),
             y = c(1, b2, 0.5, 1 - b4, 0)
           )
           
           # Compute bezier curve
           line_1 <- bezier::bezier(t = seq(0, 1, .01),
                                    p = control_Bbez)
           
           # Convert points to line
           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()
           
           # Split the base tile to give the final tile
           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 2:1)
           ## ADORNMENTS DONE
         },
         
         "Cbez" ={
           ## ADORNMENTS
           # Define bezier control points for line
           
           control_Cbez <- tibble(
             x = c(0, b1, 0.5, b3, 1),
             y = c(1, b2, 0.5, b4, 0)
           )
           
           # Compute bezier curve
           line_1 <- bezier::bezier(t = seq(0, 1, .01),
                                    p = control_Cbez)
           
           # Convert points to line
           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()
           
           # Split the base tile to give the final tile
           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 1:2)
           ## ADORNMENTS DONE
         },
         
         "Dbez" ={
           ## ADORNMENTS
           # Define bezier control points for line
           
           control_Dbez <- tibble(
             x = c(0, b1, 0.5, 1 - b3, 1),
             y = c(1, b2, 0.5, 1 - b4, 0)
           )
           
           # Compute bezier curve
           line_1 <- bezier::bezier(t = seq(0, 1, .01),
                                    p = control_Dbez)
           
           # Convert points to line
           line_1 <- data.frame(id = 1,
                                geometry = sf::st_linestring(line_1) %>%
                                  sf::st_geometry()) %>%
             sf::st_sf()
           
           # Split the base tile to give the final tile
           tile <- tile %>%
             lwgeom::st_split(line_1) %>%
             sf::st_collection_extract() %>%
             dplyr::mutate(color = 1:2)
           ## ADORNMENTS DONE
         }
  )
  
  # Translate so that the tiles are centered on the point (0, 0)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(-0.5, - 0.5))
  
  ## FINISH TILES
  # position at point (x, y)
  tile <- tile %>%
    dplyr::mutate(geometry = sf::st_geometry(tile) + c(x, y))
  
  ## TILES DONE
  
  return(tile)
}

# 5 Bezier tiles----

# bezier rotation----

rotation_bezier = function(a, x, y, type, b1, b2, b3, b4){
  tile <- st_truchet_flex_bezier(type = type, b1 = b1, b2 = b2, b3 = b3, b4 = b4)
  rm <- matrix(c(cos(a), sin(a), 
                 -sin(a), cos(a)),
               nrow = 2, 
               ncol = 2)
  tile %>%
    mutate(geometry = st_geometry(tile) * rm + c(x, y)) %>%
    st_sf()
}

rotation_bezier_thin = function(a, x, y, type, b1, b2, b3, b4){
  tile <- st_truchet_flex_bezier_thin(type = type, b1 = b1, b2 = b2, b3 = b3, b4 = b4)
  rm <- matrix(c(cos(a), sin(a), 
                 -sin(a), cos(a)),
               nrow = 2, 
               ncol = 2)
  tile %>%
    mutate(geometry = st_geometry(tile) * rm + c(x, y)) %>%
    st_sf()
}

tile_type_bezier <- c("Dbez", "Bbez", "Dbez", "Bbez")

# Tile 1
# Initialize an empty data frame
tile_1_bez <- data.frame()

# Initialize the counter
count <- 0

for(i in a_alt){
  # Increase the counter by one
  count <- count + 1
  # Bind a rotated tile to the existing data frame
  tile_1_bez <- rbind(tile_1_bez,
                  data.frame(rotation_bezier(a = i, 
                                             # Coordinates of tile
                                             x = 1, 
                                             y = 1,
                                             type = tile_type_bezier[1],
                                             b1 = count*0.0013,
                                             b2 = count*0.0013,
                                             b3 = count*0.0013,
                                             b4 = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 1
tile_1_bez <- tile_1_bez %>%
  mutate(tile = "1") %>%
  st_sf()

# Tile 2
tile_2_bez <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_2_bez <- rbind(tile_2_bez,
                  data.frame(rotation_bezier(a = i, 
                                             # Coordinates of tile
                                             x = 2, 
                                             y = 2, 
                                             type = tile_type_bezier[2],
                                             b1 = count*0.0013,
                                             b2 = count*0.0013,
                                             b3 = count*0.0013,
                                             b4 = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 2
tile_2_bez <- tile_2_bez %>%
  mutate(tile = "2") %>%
  st_sf()

plot(tile_2_bez)

# Tile 3
tile_3_bez <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_3_bez <- rbind(tile_3_bez,
                  data.frame(rotation_bezier(a = i, 
                                             # Coordinates of tile
                                             x = 1, 
                                             y = 2, 
                                             type = tile_type_bezier[3],
                                             b1 = count*0.0013,
                                             b2 = count*0.0013,
                                             b3 = count*0.0013,
                                             b4 = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 3
tile_3_bez <- tile_3_bez %>%
  mutate(tile = "3") %>%
  st_sf()

# Tile 4
tile_4_bez <- data.frame()
count <- 0

for(i in a_alt){
  count <- count + 1
  tile_4_bez <- rbind(tile_4_bez,
                  data.frame(rotation_bezier(a = i, 
                                             # Coordinates of tile
                                             x = 2, 
                                             y = 1, 
                                             type = tile_type_bezier[4],
                                             b1 = count*0.0013,
                                             b2 = count*0.0013,
                                             b3 = count*0.0013,
                                             b4 = count*0.0013), 
                             state = count))
}

# Convert the data frame to simple features and label it as tile 4
tile_4_bez <- tile_4_bez %>%
  mutate(tile = "4") %>%
  st_sf()

mosaic_tile_bezier <- rbind(tile_1_bez,
                            tile_2_bez,
                            tile_3_bez,
                            tile_4_bez)

p <- ggplot() +
  # Render the static background mosaic
  # geom_sf(data = background,
  #         aes(fill = factor(color)),
  #         color = NA) +
  # Render the animated tiles: first the tiles of color 1
  geom_sf(data = mosaic_tile_bezier_coords %>%
            filter(color == 1),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  # Render the animated tiles: then the tiles of color 2
  geom_sf(data = mosaic_tile_bezier_coords %>%
            filter(color == 2),
          aes(fill = factor(color),
              # It is important to group by `state` and `tile` otherwise the animation gets wacky
              group = interaction(state, tile)),
          color = NA) +
  geom_spoke(data = mosaic_tile_bezier_coords %>% filter(color == 2), aes(x=X, y=Y, angle = color), radius = 0.5) +
  geom_spoke(data = mosaic_tile_bezier_coords %>% filter(color == 1), aes(x=X, y=Y, angle = 1/color), radius = 0.5) +
  #geom_point(data = mosaic_tile_bezier_coords, aes(geometry+cos(state), geometry+sin(state)), size=10, colour = 'green') +
  # Select colors; this is a duotone mosaic
  scale_fill_manual(values = c("1" = "#FF8000", "2" = "#B6BABD")) + 
  # "Crop" the mosaic by limiting the extent of the coordinates
  coord_sf(xlim = c(0.5, 2.5),
           ylim = c(0.5, 2.5),
           expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#FF8000"),
        panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=1)) 

sf_cent <- st_coordinates(st_centroid(mosaic_tile_bezier))

sf_coords <- st_coordinates(mosaic_tile_bezier$geometry)

plot(sf_coords)

mosaic_tile_bezier_coords <- mosaic_tile_bezier %>% 
  bind_cols(sf_cent) 

mosaic_tile_bezier_coords_angle <- mosaic_tile_bezier_coords %>% 
  mutate(angle = case_when(tile == 1 & color == 1 ~ 1,
                           tile == 1 & color == 2 ~ 2,
                           tile == 2 & color == 1 ~ 3,
                           tile == 2 & color == 2 ~ 4,
                           tile == 3 & color == 1 ~ 5,
                           tile == 3 & color == 2 ~ 6,
                           tile == 4 & color == 1 ~ 7,
                           tile == 4 & color == 2 ~ 8,
                           TRUE ~ NA))

seq <- seq(0, 360, 182)

seq <- seq(from = 0, to = 360, length.out = 364)

plot_tiles <- p + gganimate::transition_time(state) 

animate(plot_tiles, 
        fps = 30, 
        duration = 6.5, 
        end_pause = 50, 
        height = 600, width = 600)

anim_save("./04_gifs/animation_four_tiles2.gif")

# tile_test <- matrix(c(0, 0,
#                       0, 5,
#                       1, 5,
#                       1, 0,
#                       0, 0),
#                     ncol = 2,
#                     byrow = TRUE)
# 
# plot(tile_test2)
# 
# tile_test2 <- matrix(c(0, 0,
#                        0, 1,
#                        1, 1,
#                        1, 0,
#                        0, 0),
#                      ncol = 2,
#                      byrow = TRUE)

#' st_truchet_flex_bezier_thin <- function(x = 0, y = 0, type = "Abez", b1 = 2.5, b2 = 2.5, b3 = 2.5, b4 = 2.5){
#'   
#'   #' Flexible Truchet tiles
#'   #'
#'   #' @param x A number with the x coordinate of the center of the tile
#'   #' @param y A number with the y coordinate of the center of the tile
#'   #' @param type A single character to designate a type of tile; currently supported options are "Ac", "Bc", "Cc", "Dc", "As", "Bs", "Cs", "Ds"
#'   #' @param b A number between zero and one that controls the shape of the boundary between the two parts of the tile
#'   #' @return A list with one or more objects of type \code{sf} representing one or more tiles depending on type
#'   #' @importFrom rlang .data
#'   #' @export
#'   #' @examples
#'   #' st_truchet_flex_bezier(type = "Abez")
#'   #' @note For a discussion of Truchet patterns see: Robert Bosch & Urchin Colley (2013) Figurative mosaics from flexible Truchet tiles, Journal of Mathematics and the Arts, 7:3-4, 122-135, \url{10.1080/17513472.2013.838830}
#'   
#'   # Validate inputs
#'   checkmate::assertChoice(type, c("Abez", "Bbez", "Cbez", "Dbez"))
#'   # b1 must be a value beween zero and 1
#'   checkmate::assert_number(b1, lower = 0, upper = 5)
#'   # b2 must be a value beween zero and 1
#'   checkmate::assert_number(b2, lower = 0, upper = 5)
#'   # b3 must be a value beween zero and 1
#'   checkmate::assert_number(b3, lower = 0, upper = 5)
#'   # b4 must be a value beween zero and 1
#'   checkmate::assert_number(b4, lower = 0, upper = 5)
#'   
#'   # Adjust values of b1 in case that there is an exact zero or one, which messes up the selection of colors later on
#'   if(b1 == 0) b1 <- 1/100
#'   if(b1 == 1) b1 <- 99/100
#'   
#'   # Adjust values of b2 in case that there is an exact zero or one, which messes up the selection of colors later on
#'   if(b2 == 0) b2 <- 1/100
#'   if(b2 == 1) b2 <- 99/100
#'   
#'   # Adjust values of b3 in case that there is an exact zero or one, which messes up the selection of colors later on
#'   if(b3 == 0) b3 <- 1/100
#'   if(b3 == 1) b3 <- 99/100
#'   
#'   # Adjust values of b4 in case that there is an exact zero or one, which messes up the selection of colors later on
#'   if(b4 == 0) b4 <- 1/100
#'   if(b4 == 1) b4 <- 99/100
#'   
#'   ## CREATE BASE TILE
#'   #  Define square polygon
#'   tile <- matrix(c(0, 0,
#'                    0, 5,
#'                    1, 5,
#'                    1, 0,
#'                    0, 0),
#'                  ncol = 2,
#'                  byrow = TRUE)
#'   
#'   # Convert coordinates to polygons and then to simple features
#'   tile <- data.frame(geometry = sf::st_polygon(list(tile)) %>%
#'                        sf::st_sfc()) %>%
#'     sf::st_as_sf()
#'   
#'   ## BASE TILE DONE
#'   
#'   # Tile types
#'   
#'   switch(type,
#'          
#'          "Abez" ={
#'            ## ADORNMENTS
#'            # Define bezier control points for line
#'            
#'            control_Abez <- tibble(
#'              x = c(0, b1, 2.5, b3, 5),
#'              y = c(5, b2, 2.5, b4, 0)
#'            )
#'            
#'            line_1 <- bezier::bezier(t = seq(0, 1, .01),
#'                                     p = control_Abez)
#'            
#'            # Convert points to line
#'            line_1 <- data.frame(id = 1,
#'                                 geometry = sf::st_linestring(line_1) %>%
#'                                   sf::st_geometry()) %>%
#'              sf::st_sf()
#'            
#'            # Split the base tile to give the final tile
#'            tile <- tile %>%
#'              lwgeom::st_split(line_1) %>%
#'              sf::st_collection_extract() %>%
#'              dplyr::mutate(color = 2:1)
#'            ## ADORNMENTS DONE
#'          },
#'          
#'          "Bbez" ={
#'            ## ADORNMENTS
#'            # Define bezier control points for line
#'            
#'            control_Bbez <- tibble(
#'              x = c(0, b1, 2.5, 5 - b3, 5),
#'              y = c(5, b2, 2.5, 5 - b4, 0)
#'            )
#'            
#'            # Compute bezier curve
#'            line_1 <- bezier::bezier(t = seq(0, 1, .01),
#'                                     p = control_Bbez)
#'            
#'            # Convert points to line
#'            line_1 <- data.frame(id = 1,
#'                                 geometry = sf::st_linestring(line_1) %>%
#'                                   sf::st_geometry()) %>%
#'              sf::st_sf()
#'            
#'            # Split the base tile to give the final tile
#'            tile <- tile %>%
#'              lwgeom::st_split(line_1) %>%
#'              sf::st_collection_extract() %>%
#'              dplyr::mutate(color = 2:1)
#'            ## ADORNMENTS DONE
#'          },
#'          
#'          "Cbez" ={
#'            ## ADORNMENTS
#'            # Define bezier control points for line
#'            
#'            control_Cbez <- tibble(
#'              x = c(0, b1, 2.5, b3, 5),
#'              y = c(5, b2, 2.5, b4, 0)
#'            )
#'            
#'            # Compute bezier curve
#'            line_1 <- bezier::bezier(t = seq(0, 1, .01),
#'                                     p = control_Cbez)
#'            
#'            # Convert points to line
#'            line_1 <- data.frame(id = 1,
#'                                 geometry = sf::st_linestring(line_1) %>%
#'                                   sf::st_geometry()) %>%
#'              sf::st_sf()
#'            
#'            # Split the base tile to give the final tile
#'            tile <- tile %>%
#'              lwgeom::st_split(line_1) %>%
#'              sf::st_collection_extract() %>%
#'              dplyr::mutate(color = 1:2)
#'            ## ADORNMENTS DONE
#'          },
#'          
#'          "Dbez" ={
#'            ## ADORNMENTS
#'            # Define bezier control points for line
#'            
#'            control_Dbez <- tibble(
#'              x = c(0, b1, 2.5, 5 - b3, 5),
#'              y = c(5, b2, 2.5, 5 - b4, 0)
#'            )
#'            
#'            # Compute bezier curve
#'            line_1 <- bezier::bezier(t = seq(0, 1, .01),
#'                                     p = control_Dbez)
#'            
#'            # Convert points to line
#'            line_1 <- data.frame(id = 1,
#'                                 geometry = sf::st_linestring(line_1) %>%
#'                                   sf::st_geometry()) %>%
#'              sf::st_sf()
#'            
#'            # Split the base tile to give the final tile
#'            tile <- tile %>%
#'              lwgeom::st_split(line_1) %>%
#'              sf::st_collection_extract() %>%
#'              dplyr::mutate(color = 1:2)
#'            ## ADORNMENTS DONE
#'          }
#'   )
#'   
#'   # Translate so that the tiles are centered on the point (0, 0)
#'   tile <- tile %>%
#'     dplyr::mutate(geometry = sf::st_geometry(tile) + c(-2.5, - 2.5))
#'   
#'   ## FINISH TILES
#'   # position at point (x, y)
#'   tile <- tile %>%
#'     dplyr::mutate(geometry = sf::st_geometry(tile) + c(x, y))
#'   
#'   ## TILES DONE
#'   
#'   return(tile)
#' }
#' 
#' # Tile 1
#' # Initialize an empty data frame
#' tile_1_bez_thin <- data.frame()
#' 
#' # Initialize the counter
#' count <- 0
#' 
#' for(i in a_alt){
#'   # Increase the counter by one
#'   count <- count + 1
#'   # Bind a rotated tile to the existing data frame
#'   tile_1_bez_thin <- rbind(tile_1_bez_thin,
#'                            data.frame(rotation_bezier_thin(a = i, 
#'                                                  # Coordinates of tile
#'                                                  x = 0.5, 
#'                                                  y = 2.5,
#'                                                  type = tile_type_bezier[1],
#'                                                  b1 = count*0.0013,
#'                                                  b2 = count*0.0013,
#'                                                  b3 = count*0.0013,
#'                                                  b4 = count*0.0013), 
#'                                  state = count))
#' }
#' 
#' # Convert the data frame to simple features and label it as tile 1
#' tile_1_bez_thin <- tile_1_bez_thin %>%
#'   mutate(tile = "1") %>%
#'   st_sf()
#' 
#' # Tile 2
#' tile_2_bez_thin <- data.frame()
#' count <- 0
#' 
#' for(i in a_alt){
#'   count <- count + 1
#'   tile_2_bez_thin <- rbind(tile_2_bez_thin,
#'                            data.frame(rotation_bezier_thin(a = i, 
#'                                                  # Coordinates of tile
#'                                                  x = 0.5, 
#'                                                  y = 7.5, 
#'                                                  type = tile_type_bezier[2],
#'                                                  b1 = count*0.0013,
#'                                                  b2 = count*0.0013,
#'                                                  b3 = count*0.0013,
#'                                                  b4 = count*0.0013), 
#'                                  state = count))
#' }
#' 
#' # Convert the data frame to simple features and label it as tile 2
#' tile_2_bez_thin <- tile_2_bez_thin %>%
#'   mutate(tile = "2") %>%
#'   st_sf()
#' 
#' #plot(tile_2_bez)
#' 
#' # Tile 3
#' tile_3_bez_thin <- data.frame()
#' count <- 0
#' 
#' for(i in a_alt){
#'   count <- count + 1
#'   tile_3_bez_thin <- rbind(tile_3_bez_thin,
#'                       data.frame(rotation_bezier_thin(a = i, 
#'                                                  # Coordinates of tile
#'                                                  x = 1, 
#'                                                  y = 7.5, 
#'                                                  type = tile_type_bezier[3],
#'                                                  b1 = count*0.0013,
#'                                                  b2 = count*0.0013,
#'                                                  b3 = count*0.0013,
#'                                                  b4 = count*0.0013), 
#'                                  state = count))
#' }
#' 
#' # Convert the data frame to simple features and label it as tile 3
#' tile_3_bez_thin <- tile_3_bez_thin %>%
#'   mutate(tile = "3") %>%
#'   st_sf()
#' 
#' # Tile 4
#' tile_4_bez_thin <- data.frame()
#' count <- 0
#' 
#' for(i in a_alt){
#'   count <- count + 1
#'   tile_4_bez_thin <- rbind(tile_4_bez_thin,
#'                       data.frame(rotation_bezier_thin(a = i, 
#'                                                  # Coordinates of tile
#'                                                  x = 1, 
#'                                                  y = 2.5, 
#'                                                  type = tile_type_bezier[4],
#'                                                  b1 = count*0.0013,
#'                                                  b2 = count*0.0013,
#'                                                  b3 = count*0.0013,
#'                                                  b4 = count*0.0013), 
#'                                  state = count))
#' }
#' 
#' # Convert the data frame to simple features and label it as tile 4
#' tile_4_bez_thin <- tile_4_bez_thin %>%
#'   mutate(tile = "4") %>%
#'   st_sf()
#' 
#' mosaic_tile_bezier_thin <- rbind(tile_1_bez_thin,
#'                                  tile_2_bez_thin,
#'                                  tile_3_bez_thin,
#'                                  tile_4_bez_thin)
#' 
#' p <- ggplot() +
#'   # Render the static background mosaic
#'   # geom_sf(data = background,
#'   #         aes(fill = factor(color)),
#'   #         color = NA) +
#'   # Render the animated tiles: first the tiles of color 1
#'   geom_sf(data = mosaic_tile_bezier_thin %>%
#'             filter(color == 1),
#'           aes(fill = factor(color),
#'               # It is important to group by `state` and `tile` otherwise the animation gets wacky
#'               group = interaction(state, tile)),
#'           color = NA) +
#'   # Render the animated tiles: then the tiles of color 2
#'   geom_sf(data = mosaic_tile_bezier_thin %>%
#'             filter(color == 2),
#'           aes(fill = factor(color),
#'               # It is important to group by `state` and `tile` otherwise the animation gets wacky
#'               group = interaction(state, tile)),
#'           color = NA) +
#'   # Select colors; this is a duotone mosaic
#'   scale_fill_manual(values = c("1" = "#FF8000", "2" = "#B6BABD")) + 
#'   # "Crop" the mosaic by limiting the extent of the coordinates
#'   coord_sf(xlim = c(0.5, 2.5),
#'            ylim = c(0.5, 12.5),
#'            expand = FALSE) +
#'   theme_void() +
#'   theme(legend.position = "none",
#'         plot.background = element_rect(fill = "#FF8000"),
#'         panel.border = element_rect(colour = "#73AEA4", fill=NA, linewidth=1)) 
#' 
#' plot_tiles_thin <- p + gganimate::transition_time(state) 
#' 
#' animate(plot_tiles_thin, 
#'         fps = 30, 
#'         duration = 6.5, 
#'         end_pause = 50, 
#'         height = 600, width = 600)
#' 
#' anim_save("./04_gifs/animation_four_tiles3.gif")

# 6 Wave Patterns----
# *6.1 3rd pattern----
# 3rd pattern - use this - wave pattern
# n_points is the number of points for the mclaren logo

wave_points_tile1_colour1 <- mosaic_tile_bezier_coords_angle %>% 
  filter(color %in% 1 & tile %in% 1) %>% 
  slice(which(row_number() %% 10 == 1))

n_points_w1_t1 <- nrow(wave_points_tile1_colour1)
closeness_w1_t1 <- 2*pi/n_points_w1_t1
speed_w1_t1 <- 2*pi/n_points_w1_t1
v_angles_w1_t1 <- seq(0, 0, length.out = n_points_w1_t1)

n_points  <- 32
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 0, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins - inspect the structure of the output -> df 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

lapply(1:(n_points_w1_t1+1), function(x) {
  create_grid(n_points_w1_t1, 
              v_angles_w1_t1+(x-1)*speed_w1_t1,
              closeness_w1_t1)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df_w1_t1

df_w1_t1 <- df_w1_t1 %>%
  group_by(frame) %>% 
  mutate(size = runif(1369, 0.5, 6.5),
         # colour = case_when(size >= 0.5 & size <= 1 ~ '#C9EFFE', 
         #                    size > 1 & size <= 2 ~  '#0F3A57',
         #                    size > 2 & size <= 3 ~  '#C9EFFE',
         #                    size > 2 & size <= 3 ~  '#0F3A57',
         #                    size > 3 & size <= 4 ~  '#C9EFFE',
         #                    TRUE ~ '#0F3A57'), 
         fill = case_when(size >= 0.5 & size <= 1 ~ '#0F3A57', 
                          size > 1 & size <= 2 ~  '#2D4FA1',
                          size > 2 & size <= 3 ~  '#27C6B1',
                          size > 2 & size <= 3 ~  '#A8CDF1',
                          size > 3 & size <= 4 ~  '#59819F',
                          TRUE ~ '#C9EFFE'),
         alpha = runif(1369, 0.7, 0.95)) %>%
  #mutate(size = seq(0.5, 10, length.out = 1369)) %>% 
  ungroup()

df <- df %>%
  group_by(frame) %>% 
  mutate(size = runif(1024, 0.5, 6.5),
         # colour = case_when(size >= 0.5 & size <= 1 ~ '#C9EFFE', 
         #                    size > 1 & size <= 2 ~  '#0F3A57',
         #                    size > 2 & size <= 3 ~  '#C9EFFE',
         #                    size > 2 & size <= 3 ~  '#0F3A57',
         #                    size > 3 & size <= 4 ~  '#C9EFFE',
         #                    TRUE ~ '#0F3A57'), 
         fill = case_when(size >= 0.5 & size <= 1 ~ '#0F3A57', 
                          size > 1 & size <= 2 ~  '#2D4FA1',
                          size > 2 & size <= 3 ~  '#27C6B1',
                          size > 2 & size <= 3 ~  '#A8CDF1',
                          size > 3 & size <= 4 ~  '#59819F',
                          TRUE ~ '#C9EFFE'),
         alpha = runif(1024, 0.7, 0.95)) %>%
  #mutate(size = seq(0.5, 10, length.out = 1369)) %>% 
  ungroup()

# colours_scotland
blues <- c('#0F3A57','#2D4FA1','#27C6B1', '#A8CDF1', '#59819F', '#C9EFFE')

# df_w1_t1 <- df_w1_t1 %>% 
#   slice(1:8400)

# for checking original spinning pins animation
# Plot pins using frame as transition time
wave <- ggplot(df) +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 1, colour = '#C9EFFE') +
  geom_point(aes(x+cos(angle), y+sin(angle)), 
             shape = 23,
             # colour = df_w1_t1$colour,
             size = df$size, 
             fill = df$fill,
             alpha = df$alpha) +
  # geom_point(aes(x+cos(angle), y+sin(angle)), 
  #            shape = 21, 
  #            size = 2, 
  #            fill = 'white',
  #            alpha = df_w1_t1$alpha) +
  #scale_fill_manual(values="cyan4") +
  #geom_tile(aes(x=x, y=y)) +
  theme_void() +
  theme(panel.background = element_rect(fill = '#C9EFFE', colour = '#C9EFFE')) +
  coord_fixed() +
  transition_time(time=frame)

# animate
animate(wave, fps=10, height = 600, width = 600)

# animation save
#anim_save("./04_gifs/mclaren_pins_animate_lando_max.gif", height = 372, width = 538, units = "px")
anim_save("./04_gifs/wave2.gif")

anim_save("./04_gifs/animation_four_tiles2.gif")

# x variable from the square pattern
df_x <- df_w1_t1 %>% 
  select(x) %>% 
  rename(original_x = x)

# x variable from the rescaled McLaren logo coordinates
new_df_x <- wave_points_tile1_colour1 %>%
  #slice(1:10) %>%  
  select(X) %>% 
  rename(new_x = X) 

#slice(1:52022)

# https://stackoverflow.com/questions/66434941/make-a-column-based-a-repetitive-numbers-that-follows-another-column
# transpose new McLaren x-cordinates to same length as square pattern
df_x_rep <- df_x %>%
  mutate(new_col = rep(list(new_df_x), n())) %>% 
  unnest(new_col) %>% 
  slice(1:52022)

# y variable
# y variable from the square pattern
df_y <- df_w1_t1 %>% 
  select(y) %>% 
  rename(original_y = y)

# different structure for y - stepwise, single coordinates repeated n times
# https://stackoverflow.com/questions/2894775/repeat-each-row-of-data-frame-the-number-of-times-specified-in-a-column
new_df_y <- wave_points_tile1_colour1 %>%
  select(Y) %>% 
  #slice(1:83) %>% 
  mutate(freq = 37) %>% 
  slice(rep(seq_len(n()), freq)) %>% 
  select(-freq)

# then repeat previous step n times
df_y_rep <- purrr::map_dfr(seq_len(38), ~new_df_y)

# bind new x and y's
new_xy <- df_x_rep %>% 
  bind_cols(df_y_rep) %>% 
  select(new_x, Y) %>% 
  rename(x = new_x,
         y = Y)

# reclaim frame and angle variables from original square pattern
df_frame_angle <- df_w1_t1 %>% 
  select(frame, angle)

# bind frame, angle variables to new x,y variables
# add row numbers in case needed later
df_w1_t1_frame_angle <- new_xy %>% 
  bind_cols(df_frame_angle) %>% 
  select(frame, x, y, angle) %>% 
  mutate(row = row_number())

# *6.2 1st pattern----
# 1st pattern
n_points  <- 20
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 2*pi, length.out = n_points)

# This function creates a grid of vectors (coordinates and angle)
# using a initial vector of angles adding factor f each iteration
create_grid <- function(n, a, f) {
  lapply(seq_len(n), function (x) {a+f*(x-1)}) %>% 
    do.call("rbind", .) %>% 
    melt(varnames=c('x', 'y'), value.name="angle")
}

# This is what makes to spin the pins 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df1

df_bind <- df %>% 
  bind_rows(df1)

df <- df1 %>%
  group_by(frame) %>% 
  mutate(size = runif(400, 0.5, 15),
         # colour = case_when(size >= 0.5 & size <= 1 ~ '#C9EFFE', 
         #                    size > 1 & size <= 2 ~  '#0F3A57',
         #                    size > 2 & size <= 3 ~  '#C9EFFE',
         #                    size > 2 & size <= 3 ~  '#0F3A57',
         #                    size > 3 & size <= 4 ~  '#C9EFFE',
         #                    TRUE ~ '#0F3A57'), 
         fill = case_when(size >= 0.5 & size <= 1 ~ '#0F3A57', 
                          size > 1 & size <= 2 ~  '#2D4FA1',
                          size > 2 & size <= 3 ~  '#27C6B1',
                          size > 2 & size <= 3 ~  '#A8CDF1',
                          size > 3 & size <= 4 ~  '#59819F',
                          TRUE ~ '#C9EFFE'),
         alpha = runif(400, 0.8, 0.95)) %>%
  #mutate(size = seq(0.5, 10, length.out = 1369)) %>% 
  ungroup() %>%
  mutate(point_colour = case_when(frame %in% 1 ~ '#0F3A57', 
                                  frame > 1 & frame <= 4 ~  '#2D4FA1',
                                  angle > 4 & frame <= 8 ~  '#27C6B1',
                                  frame > 8 & frame <= 12 ~  '#A8CDF1',
                                  frame > 12 & frame <= 16 ~  '#59819F',
                                  TRUE ~ '#C9EFFE'))

# Plot pins using frame as transition time
# Plot pins using frame as transition time
wave2 <- ggplot(df) +
  geom_spoke(aes(x=x, y=y, angle = angle), radius = 2, colour = '#27C6B1', size = 0.75) +
  # geom_point(aes(x+cos(angle), y+sin(angle)),
  #            shape = 21,
  #            # colour = df_w1_t1$colour,
  #            size = 5,
  #            fill = df$point_colour,
  #            alpha = df$alpha) +
  # geom_point(aes(x+cos(angle), y+sin(angle)), 
  #            shape = 21, 
  #            size = 2, 
  #            fill = 'white',
  #            alpha = df_w1_t1$alpha) +
  #scale_fill_manual(values="cyan4") +
  #geom_tile(aes(x=x, y=y)) +
  theme_void() +
  theme(panel.background = element_rect(fill = '#59819F', colour = '#59819F')) +
  coord_fixed() +
  transition_time(time=frame)

# animate
animate(wave2, fps=10, height = 600, width = 600)

# animation save
#anim_save("./04_gifs/mclaren_pins_animate_lando_max.gif", height = 372, width = 538, units = "px")
anim_save("./04_gifs/wave7.gif")

# 7 OP81 logo----
# *7.1 as a wave----
# whole image
OP81 <- image_read('./00_raw_data/OSCPIA01.png')

info <- image_info(OP81_png)

OP81_png <- image_convert(OP81, 'png')

OP81_png_trim <- image_trim(OP81_png)

print(info)

image_write(OP81_png_trim, path = './00_raw_data/OP81_trim.png', format = "png")

# cropped image no.1
OP81_png_crop <- image_crop(OP81_png, geometry = "100x225+265")

OP81_png_crop_trim <- image_trim(OP81_png_crop) 

image_write(OP81_png_crop_trim, path = './00_raw_data/no1_of_OP81_trim.png', format = "png")

# cropped image no.8
OP81_png_crop2 <- image_crop(OP81_png, geometry = "262x225+0")

OP81_png_crop2_trim <- image_trim(OP81_png_crop2) 

image_write(OP81_png_crop2_trim, path = './00_raw_data/no8_of_OP81_trim.png', format = "png")

# cropped image no.8 to 'O'
info <- image_info(OP81_png_crop2)

print(info)

# OP81_png_crop3 <- image_crop(OP81_png_crop2, geometry = "262x43+0+90")
# 
# OP81_png_crop3 <- image_crop(OP81_png_crop2, geometry = "157x43+56+90")
# 
# OP81_png_crop3_orange <- image_fill(OP81_png_crop3, 'orange')
# 
# image_write(OP81_png_crop3_orange, path = './00_raw_data/centre_bar_of_no8_2_orange.png', format = "png")
# 
# OP81_png_crop3_trim <- image_trim(OP81_png_crop3)

# pixlr 'O'
O_pixlr <- image_read('./00_raw_data/OSCPIA01_O_pixlr.png')

O_pixlr_trim <- image_trim(O_pixlr)

image_write(O_pixlr_trim, path = './00_raw_data/OSCPIA01_O_pixlr_trim.png', format = "png")

O_final <- image_read('./00_raw_data/OSCPIA01_O_pixlr.png') %>% 
  image_trim() %>%
  image_write(path = './00_raw_data/OSCPIA01_O_pixlr_trim.png', format = "png")

# pixlr 'P'
P_pixlr <- image_read('./00_raw_data/OSCPIA01_P_pixlr.png')

P_pixlr_trim <- image_trim(P_pixlr)

image_write(P_pixlr_trim, path = './00_raw_data/OSCPIA01_P_pixlr_trim.png', format = "png")

P_final <- image_read('./00_raw_data/OSCPIA01_P_pixlr.png') %>% 
  image_trim() %>%
  image_write(path = './00_raw_data/OSCPIA01_P_pixlr_trim.png', format = "png")

# pixlr bar
bar_pixlr <- image_read('./00_raw_data/OSCPIA01_pixlr_bar.png')

bar_pixlr_trim <- image_trim(bar_pixlr)

image_write(bar_pixlr_trim, path = './00_raw_data/OSCPIA01_pixlr_bar_trim.png', format = "png")

bar_final <- image_read('./00_raw_data/OSCPIA01_pixlr_bar.png') %>% 
  image_trim() %>%
  image_write(path = './00_raw_data/OSCPIA01_pixlr_bar_trim.png', format = "png")

# cropped image no.1
# OP81_png_crop <- image_crop(OP81_png, geometry = "100x225+265")
# 
# OP81_png_crop_trim <- image_trim(OP81_png_crop) 
# 
# image_write(OP81_png_crop_trim, path = './00_raw_data/no1_of_OP81_trim.png', format = "png")
# 
# no1_final <- image_crop(OP81_png, geometry = "100x225+265") %>% 
#   image_trim() %>%
#   image_write(path = './00_raw_data/no1_of_OP81_trim.png', format = "png")

# pixlr no.1

no1_pixlr <- image_read('./00_raw_data/OSCPIA01_no1_trim.png')

info <- image_info(no1_pixlr)

print(info)

image_write(no1_pixlr, path = './00_raw_data/OSCPIA01_no1_trim_rescale.png', format = "png")

no1_final <- image_read('./00_raw_data/OSCPIA01_no1_trim.png') %>% 
  image_write(path = './00_raw_data/OSCPIA01_no1_trim.png', format = "png")

# # cropped image no.8
# OP81_png_crop2 <- image_crop(OP81_png, geometry = "262x225+0")
# 
# OP81_png_crop2_trim <- image_trim(OP81_png_crop2) 
# 
# image_write(OP81_png_crop2_trim, path = './00_raw_data/no8_of_OP81_trim.png', format = "png")
# 
# no8_final <- image_crop(OP81_png, geometry = "262x225+0") %>% 
#   image_trim() %>%
#   image_write(path = './00_raw_data/no8_of_OP81_trim.png', format = "png")

# pixlr no.8

no8_pixlr <- image_read('./00_raw_data/OSCPIA01_pixlr_no8.png')

no8_trim <- image_trim(no8_pixlr)

info <- image_info(no8_trim)

print(info)

image_write(no8_trim, path = './00_raw_data/OSCPIA01_no8_trim.png', format = "png")

no8_final <- image_read('./00_raw_data/OSCPIA01_pixlr_no8.png') %>% 
  image_trim() %>%
  image_write(path = './00_raw_data/OSCPIA01_no8_trim.png', format = "png")


# *7.2 rotation pattern - 2nd pattern----
# n_points is the number of points for the mclaren logo
n_points  <- 8
closeness <- 6
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi*2, length.out = n_points)

n_points  <- 6
closeness <- 0
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/2, length.out = n_points)

# n_points  <- 4
# closeness <- 1
# speed     <- 8/n_points
# v_angles <- seq(0, by=45, length.out = n_points)

# 4th pattern
n_points  <- 6
closeness <- pi/4
speed     <- 2*pi/n_points
v_angles <- seq(0, by=pi/4, length.out = n_points)

# 3rd pattern
n_points  <- 6
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 0, length.out = n_points)

# 1st pattern
n_points  <- 6
closeness <- 2*pi/n_points
speed     <- 2*pi/n_points
v_angles <- seq(0, 2*pi, length.out = n_points)

# This is what makes to spin the pins - inspect the structure of the output -> df 
lapply(1:(n_points+1), function(x) {
  create_grid(n_points, 
              v_angles+(x-1)*speed,
              closeness)}) %>% 
  as.list(.) %>% 
  rbindlist(idcol="frame") -> df

# df <- df %>% 
#   mutate(label = case_when(x %% 2 == 0 ~ P_final,
#                            x %% 3 == 0 ~ bar_final,
#                            TRUE ~ O_final))
# df <- df %>% 
#   mutate (label = rep(c(O_final,
#                         P_final,
#                         no8_final,
#                         no1_final,
#                         O_final,
#                         P_final,
#                         no8_final,
#                         no1_final), 
#                       42, 
#                       length.out = n()))

df <- df %>% 
  mutate (label = rep(c(bar_final,
                        O_final,
                        P_final,
                        no8_final,
                        no1_final,
                        bar_final), 
                      42, 
                      length.out = n()))


OP_animate <- df %>% 
  ggplot() +
  #geom_spoke(aes(x=x, y=y, angle = angle), radius = 1, colour = '#FF8000') +
  geom_image(aes(x+cos(angle), y+sin(angle), image = label), size = 0.07, by = 'height') +
  #scale_size_identity() +
  #geom_point(aes(x+cos(angle), y+sin(angle)), size=3, colour = '#FF8000') +
  #geom_text(aes(x+cos(angle), y+sin(angle), label = label), family = 'fontawesome-webfont') +
  theme_void() +
  #darklyplot::theme_dark2() +
  theme(legend.position = "none",
        plot.margin = unit(c(5, 5, 5, 5), "pt"),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.caption = element_text(size = 14, family = "alfa",hjust = 0.5, colour = '#FF87BC')) +
  coord_fixed() +
  theme(panel.background = element_rect(fill = '#59819F', colour = '#59819F')) +
  transition_time(time=frame) +
  #shadow_wake(0.15) +
  labs(caption = "design by hey-jay")

# animate
animate(OP_animate, fps=10, height = 600, width = 600)

# animation save
anim_save("./04_gifs/OP_wave_no2.gif")

# *7.3 as a race progression----

OP81_as_a_race <- read.csv('./00_raw_data/op81_animate_as_race.csv') %>% 
  pivot_longer(cols = 'oh':'one', names_to = 'icons', values_to = 'points') %>% 
  mutate(file_path = case_when(icons %in% 'oh' ~ O_final,
                               icons %in% 'pee' ~ P_final,
                               icons %in% 'eight' ~ no8_final,
                               TRUE ~ no1_final)) %>% 
  mutate(Race_id = case_when(icons %in% 'one' ~ 2, TRUE ~ 1))

OP81_as_a_race_alt <- read.csv('./00_raw_data/op81_animate_as_race_alt1.csv') %>% 
  pivot_longer(cols = 'oh':'one', names_to = 'icons', values_to = 'points') %>% 
  mutate(file_path = case_when(icons %in% 'oh' ~ O_final,
                               icons %in% 'pee' ~ P_final,
                               icons %in% 'eight' ~ no8_final,
                               TRUE ~ no1_final)) 

OP81_as_a_race_plot <- OP81_as_a_race_alt %>%
  ggplot(aes(x = factor(Race_id), y = points, colour = icons, group = icons)) + 
  geom_image(aes(image = file_path), size = 0.07, by = 'height') +
  #geom_point(shape=21, aes(group = seq_along(Race_id))) +
  #scale_colour_manual(values = drivers_2024$secondary_colour) +
  #coord_cartesian(xlim = c(-1, 1), ylim = c(0, 5), expand = F, clip = 'off') +
  transition_states(points, transition_length = 2, state_length = 1) +
  #transition_states(points) +
  enter_fade() +
  exit_shrink() +
  ease_aes('sine-in-out')
  #ease_aes('linear')

animate(OP81_as_a_race_plot, nframes = 300, end_pause = 100, height = 800, width = 555)

OP81_as_a_race_plot 

# animation save
anim_save("./04_gifs/OP_reveal.gif")
mtcars
OP81_as_a_race
