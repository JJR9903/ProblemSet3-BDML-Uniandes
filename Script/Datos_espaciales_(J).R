
rm (list= ls())
install.packages(osmata)
library(pacman)
p_load(rstudioapi, tidyverse, sf, rio, osmdata,leaflet, skimr)
path_clse <- dirname(get)