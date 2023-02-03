

#install.packages('ggmap')
#install.packages('readr')
library(ggmap)
register_google(key = 'Your-Api-Key')
library(ggplot2)
library(readr)



# Update this path to your correct file

setwd("~/location/of/csv")
ots_r <- read_csv("source_dests.csv")



#ots_r <- read_csv("t_transit.csv")
View(ots_r)

# Update the following to match your csv structure, this matches ["...1"      "TripID"    "OriginLat" "OriginLon" "DestLat"   "DestLon"]  
ots_df <- ots_r[, c(1, 2, 3, 4, 5, 6)]

names(ots_df)

# Only process the first 3 
# Comment this out to process entire csv
# ots_df <- ots_df[1:3, ]



# Append comma seperated lat lng strings expected by google api
ots_df$start_coord <-
  paste(ots_df$OriginLat, ",", ots_df$OriginLon)

ots_df$end_coord <-
  paste(ots_df$DestLat, ",", ots_df$DestLon)



# Use output "all" returns unformatted results but does not strip invalid empty results 
ots_dist_time <-
  mapdist(ots_df$start_coord,
          ots_df$end_coord,
          mode = "transit",
          output = "all")


ots_df[2:4]

# Create an empty table to store api result values after parsing
results = data.frame(matrix(nrow = 0, ncol = 6))
colnames(results) = c("id", "start", "end", "status", "distance", "duration")

print(names(ots_dist_time))


for (i in seq_along(ots_dist_time)) {
  l = list()
  v <- ots_dist_time[[i]]
  
  
  # Distance matrix api results containing errors/no-results may come back out of order (async/network)
  # use the api result names to get the source and dest coordinate strings and find the record id from the 
  # original data
  sourceCoord <- names(ots_dist_time)[i]
  destCoord <- names(v)[1]
  
  sourceRecord<-ots_df[which(ots_df$start_coord == sourceCoord & ots_df$end_coord == destCoord),]
  
  

  for (j in seq_along(v[[1]])) {
    l <- append(l, v[[1]][j])
  }
  
  if (l$status == 'OK') {
    results[nrow(results) + 1, ] = c(
      toString(sourceRecord[1, "...1"]),
      toString(sourceRecord[1, "start_coord"]),
      toString(sourceRecord[1, "end_coord"]),
      l$status,
      l$distance$value,
      l$duration$value
    )
    
  } else{
    results[nrow(results) + 1, ] = c(
      toString(sourceRecord[1, "...1"]),
      toString(sourceRecord[1, "start_coord"]),
      toString(sourceRecord[1, "end_coord"]),
      l$status,
      "",
      ""
    )
  }
  
  
}

View(results)

write.csv(results, "results.csv")
