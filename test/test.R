


library(rcdo)
library(tictoc)

 #
  setwd("~/Downloads/")
  ff <- "~/Downloads/thetao_Omon_HadGEM2-CC_historical_r1i1p1_199912-200511.nc"
 #
  tic()

   test <- nc_remap(ff, years = 2000, months = 1:6, vert_depths = c( 5.5), lon_range = c(-180, 30), lat_range = c(30, 80), coord_res = c(1,1))


 library(tidyverse)
 test %>%
 	ggplot2::ggplot(aes(Longitude, Latitude, colour = thetao))+
 	ggplot2::geom_point()+
 	facet_wrap(~Time)

 nc_read(ff, date_range = )



