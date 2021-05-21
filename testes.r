#Abiyyu Taj Mahasin Bagindo
#TP058652
library(ggplot2)

weatherdata <- read.csv("weather.csv")

num <- 1:366
mintemp <- weatherdata[,1]
maxtemp <- weatherdata[,2]
rainfall <- weatherdata[,3]
evaporation <- weatherdata[,4]
sunshine <- weatherdata[,5]
gustspd <- weatherdata[,7]
windspdm <- weatherdata[,10]
windspde <- weatherdata[,11]
humiditym <- weatherdata[,12]
humiditye <- weatherdata[,13]
pressurem <- weatherdata[,14]
pressuree <- weatherdata[,15]
cloudm <- weatherdata[,16]
cloude <- weatherdata[,17]

tempframe = data.frame(Min = mintemp, Max = maxtemp)
evaporationframe = data.frame(Days = num, Evaporation = evaporation)
rainfallframe <- data.frame(Days = num, Rainfall = rainfall)
sunframe <- data.frame(Days = num, Sunshine = sunshine)
gustspdframe <- data.frame(Days = num, WindGustSpeed = gustspd)
windspdframe <- data.frame(Morning = windspdm, Evening = windspde)
humidityframe <- data.frame(Morning = humiditym, Evening = humiditye)
pressureframe <- data.frame(Morning = pressurem, Evening = pressuree)
cloudframe <- data.frame(Morning = cloudm, Evening = cloude)

tempmedian <- rowMeans(tempframe)
windspdmedian <- rowMeans(windspdframe)
humiditymedian <- rowMeans(humidityframe)
pressuremedian <- rowMeans(pressureframe)
cloudmedian <- rowMeans(cloudframe)

tempmedframe <- data.frame(Days = num, Temperature_Median = tempmedian, Temperature_Min = mintemp, Temperature_Max = maxtemp)
windspdmedframe <- data.frame(Days = num, WindSpeedMedian = windspdmedian)
humiditymedframe <- data.frame(Days = num, HumidityMedian = humiditymedian)
pressuremedframe <- data.frame(Days = num, PressureMedian = pressuremedian)
cloudmedframe <- data.frame(Days = num, CloudMedian = cloudmedian)

ggplot(data = tempmedframe, mapping = aes(Days)) + geom_line(aes(y = Temperature_Min, colour = "Temperature Min")) + geom_line(aes(y = Temperature_Max, colour = "Temperature Max"))
ggplot(data = tempmedframe, mapping = aes(x = Days, y = Temperature_Median)) + geom_line(colour = "red")
#ggplot(test_data, aes(date)) + 
 # geom_line(aes(y = var0, colour = "var0")) + 
  #geom_line(aes(y = var1, colour = "var1"))