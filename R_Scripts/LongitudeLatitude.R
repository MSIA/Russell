#We have 7000
#2500 today
#
coord = read.csv("Coordinates.csv")
coord = as.matrix(coord)
Aff = read.csv("Authors, Papers, and Affiliations.csv", stringsAsFactors = FALSE)
Aff = Aff[,3]
library(ggmap)
# Only allowed 2500 a day
lonlat = sapply(Aff[4501:7000], function(x) geocode(x))
coord = rbind(coord,t(as.matrix(lonlat)))
write.csv(coord,"Coordinates.csv", row.names = FALSE)
