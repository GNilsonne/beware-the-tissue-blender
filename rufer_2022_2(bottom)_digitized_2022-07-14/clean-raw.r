data <- read.csv(file = "data-raw.csv")
data <- data[1:2]
#Name same as folder
write.csv(data, "cleaned.csv", row.names=F)


