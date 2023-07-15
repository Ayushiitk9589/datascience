data <- txhousing
complete.cases(data)
?txhousing

install.packages("geojsonio")
library(geojsonio)
install.packages("broom")
library(broom)

geo <- geojson_read("C:/Users/wizzi/Documents/On Programming/R/RStudio/IntroToR/Assignment 3/Texas_State_Boundary.geojson", what = "sp")
geo <- tidy(geo)

ggplot() +
  geom_polygon(data = geo, aes( x = long, y = lat, group = group), fill="white", color="grey") +
  theme_void()

vector <- levels(as.factor(data$city))
latitudes <- read_html("https://batchgeo.com/map/cities-latitude-longitude")
nodes <- html_elements(latitudes, ".scroller")
head(nodes)