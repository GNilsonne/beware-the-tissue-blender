data <- read.csv(file = "data.csv")
data <- data[1:2]
#Name same as folder
write.csv(data, tail(unlist(strsplit(getwd(), "/")), n=1), row.names=F)

