data2 <- read.csv("C:\\Dataset_2.csv")
data2$Type[data2$Type==""] <- NA
colSums(is.na(data2))

v <- 1:19
plot(v,data2$Price)
data2$Price[data2$Price<=300000] <- NA
data2

data2$Type = factor(data2$Type,
                    levels = c("h","m","l"), 
                    labels = c(1,2,3))
data2
data2
removed_row = 
  data2[complete.cases(data2$Rooms,data2$Price), ]
removed_row

data2$Rooms[is.na(data2$Rooms)] = mean(data2$Rooms, na.rm = TRUE)
data2$Price[is.na(data2$Price)] = mean(data2$Price, na.rm = TRUE)
data2

data2$Price[is.na(data2$Price)] = median(data2$Price, na.rm = TRUE)
data2$Rooms[is.na(data2$Rooms)] = median(data2$Rooms, na.rm = TRUE)
data2

install.packages("DescTools")
library("DescTools")
data2$Rooms[is.na(data2$Rooms)] = Mode(data2$Rooms, na.rm = TRUE)
data2$Price[is.na(data2$Price)] = Mode(data2$Price, na.rm = TRUE)
data2