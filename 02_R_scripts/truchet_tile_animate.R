
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
# library(here)
# library(patchwork)
# library(spatialEco)

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

mosaic <- aj_truchet_ms(tiles = c("A", "C", "E", "J"), 
                        p1 = 0.2, 
                        p2 = 0.6,
                        p3 = 0.2,
                        xlim = c(0, 125),
                        ylim = c(50, 80))

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

bruce_cropped <- './00_raw_data/BruceMcLaren_cropped.jpeg'

bruce_cropped1 <- './00_raw_data/BruceMcLaren_cropped1.jpeg'

# Load and convert to grayscale
load.image(bruce_cropped1) %>%
  grayscale() -> img

plot(img)

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
                           (x + y) %% 2 == 1 ~ "Cc"),
         b = case_when((x + y) %% 2 == 0 ~ 1 - value,
                       (x + y) %% 2 == 1 ~ value))

#df_bruce %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df_bruce

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

glimpse(mosaic_bruce)

# mosaic_bruce <- mosaic_bruce %>% 
#   mutate(anim = case_when(id >50 & id <= 200 ~ 1,
#                           TRUE ~ 0))
# 
# mosaic_bruce_filtered <- mosaic_bruce %>% 
#   filter(anim == 1)

#mosaic_bruce$group <- cut_number(mosaic_bruce$color, n = 20)

#mosaic_bruce %>% mutate(z = cut(id, breaks = 20, labels = FALSE)) %>% ungroup() -> mosaic_bruce

mosaic_bruce %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = 'black') +
  # geom_sf(data = mosaic_dissolved,
  #         aes(fill = colour),
  #         color = "white", inherit.aes = FALSE) +
  scale_fill_distiller(direction = -1) + 
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "floralwhite"),
        # plot.caption = element_text(size = 16, family = "zen"),
        panel.border = element_rect(colour = "#FF8000", fill=NA, linewidth=2)) +
  expand_limits(y = 80) +
  # labs(x = NULL,
  #      y = NULL,
  #      title = NULL,
  #      caption = "racer | innovator | visionary") +
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
  # geom_point(data = dtP1,
  #            aes(x = x,
  #                y = y), size=1, alpha = 1) +
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

mosaic_bruce %>%
  ggplot() + 
  geom_sf(aes(fill = color),
          color = '#FF8000',
          show.legend = FALSE) +
  # geom_sf(data = mosaic_dissolved,
  #         aes(fill = colour),
  #         color = "white", inherit.aes = FALSE) +
  #scale_fill_distiller(palette = "Spectral", direction = -1) + 
  scale_fill_gradient2(low = "grey", 
                       mid = "white", 
                       high = "black",
                       midpoint = 1.5) +
  #scale_colour_brewer(palette = "Greens") +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(plot.background = element_rect(fill = "floralwhite"),
        panel.border = element_rect(colour = "#FF8000", fill=NA, linewidth=5)) -> plot

plot
plot_bruce <- plot + gganimate::transition_time(color) + gganimate::ease_aes('linear')

plot_bruce <- plot + gganimate::transition_states(color, transition_length = 2, state_length = 1) + shadow_wake(wake_length = 0.05) 


# plot_oscar_animate <- plot +
#   coord_fixed() +
#   scale_y_reverse() +
#   theme_void() + 
#   transition_states(z, transition_length = 3, state_length = 3, wrap = FALSE) + 
#   shadow_mark() +
#   enter_fade() +
#   exit_fade()

animate(plot_bruce, fps = 30, duration = 20, end_pause = 20, height = 600, width = 600)
anim_save("./04_gifs/animation_bruce3.gif")

animate(plot_bruce, nframes = 200, end_pause = 10, height = 600, width = 600)
anim_save("./04_gifs/animation_bruce3.gif")

animate(plot_bruce, nframes = 60, height = 450, width = 350)
anim_save("./04_gifs/animation_bruce5.gif")


#plot + transition_time(x) + shadow_mark()


plot + transition_states(color)

bruce_animate <- plot +
  transition_states(color, transition_length = 3, state_length = 3, wrap = FALSE) + 
  shadow_mark() +
  enter_fade() +
  exit_fade()


animate(bruce_animate, nframes = 250, end_pause = 100, height = 600, width = 600)
#animate(bruce_animate, fps = 30, duration = 20, end_pause = 100)

#https://stackoverflow.com/questions/64037373/gganimate-data-present-only-in-some-frames

McL_logo <- image_read('./00_raw_data/McLaren_logo1.png')

info <- image_info(McL_logo) 

print(info)

McL_logo_resized <- image_resize(McL_logo, '3650x2000')

image_write(McL_logo_resized, here("00_raw_data", "McLaren_logo3_resized.png"), format = "png", quality = 75)

print(info)

image_path <- "./00_raw_data/"

mclaren_logo <- paste0(image_path, 'McLaren_logo1.png')

logo_anim = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110),
                  y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
                  t = 1:20,
                  McL=rep("dtP1", 20),
                  Image = rep(mclaren_logo, 20))

dTP1_alt = data.table(x = c(15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115),
                       y = c(77.5, 77.5, 77.5, 77.5, 75, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5, 72.5),
                       t = 1:21,
                       McL=rep("dtP1", 21),
                       Image = rep(mclaren_logo, 21))

dTP1_alt <- dTP1_alt %>% 
  mutate(logo_size = seq(from = 0.05, to = 0.4, len =20))

glimpse(dTP1_alt)

dTP1_alt <- as_tibble(dTP1_alt)


p = ggplot() +
  geom_point(data = logo_anim, aes(x= x, y= y, group = McL), size=8) +
  geom_image(aes(image=Image)) +
  gganimate::transition_time(t) +
  gganimate::ease_aes('linear')

p

#aes(colour = factor(McL)

