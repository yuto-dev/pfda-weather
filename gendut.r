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

sunshine[is.na(sunshine)] <- 0

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

tempmedframe <- data.frame(Days = num, Temperature_Median = tempmedian)
windspdmedframe <- data.frame(Days = num, WindSpeedMedian = windspdmedian)
humiditymedframe <- data.frame(Days = num, HumidityMedian = humiditymedian)
pressuremedframe <- data.frame(Days = num, PressureMedian = pressuremedian)
cloudmedframe <- data.frame(Days = num, CloudMedian = cloudmedian)

ggplot(data = tempmedframe, mapping = aes(x = Days, y = Temperature_Median)) + geom_line(colour = "red")
ggplot(data = evaporationframe, mapping = aes(x = Days, y = Evaporation)) + geom_line(colour = "red")
ggplot(data = rainfallframe, mapping = aes(x = Days, y = Rainfall)) + geom_line(colour = "blue")
ggplot(data = sunframe, mapping = aes(x = Days, y = Sunshine)) + geom_line(colour = "#000000")
ggplot(data = gustspdframe, mapping = aes(x = Days, y = WindGustSpeed)) + geom_line(colour = "green")
ggplot(data = windspdmedframe, mapping = aes(x = Days, y = WindSpeedMedian)) + geom_line(colour = "green")
ggplot(data = humiditymedframe, mapping = aes(x = Days, y = HumidityMedian)) + geom_line(colour = "blue")
ggplot(data = pressuremedframe, mapping = aes(x = Days, y = PressureMedian)) + geom_line(colour = "blue")
ggplot(data = cloudmedframe, mapping = aes(x = Days, y = CloudMedian)) + geom_line(colour = "blue")

#Question 1
#When is the best time to dry your clothes?
print("--------------------------------------------------")
print("When is the best time to dry your clothes?")
print("--------------------------------------------------")
#Analysis 1-1: Which days have zero rainfall?
#Goes through the rain database and gets a
#vector of days that with zero rainfall
rainyday <- vector()
rainydayindex <- 1
for(i in 1:366)
{
    if (rainfallframe[i,2] < 0.2) {
        rainyday[rainydayindex] <- i
        rainydayindex <- rainydayindex + 1
    }
}
sprintf("There are %d days that with zero rainfall", rainydayindex)

#Analysis 1-2: Which days have above average evaporation rate?
#The average evaporation rate throughout the year is 4.5, to simplify
#things, we have rounded it down to 4.
evamean <- mean(evaporation)
sprintf("The average evaporation rate is %f", evamean)
evameanr <- 4

#Goes through the evaporation database and gets a
#vector of days that have above average evaporation.
evaday <- vector()
evadayindex <- 1
for(i in 1:366)
{
    if (evaporationframe[i,2] > evameanr) {
        evaday[evadayindex] <- i
        evadayindex <- evadayindex + 1
    }
}
sprintf("There are %d days with above average evaporation rate (%d)", evadayindex, evameanr)

#Goes through both the rainy day and evaporation day vector
#to find days where they both rain and have above average evaporation.
rainevaday <- vector()
rainevaindex <- 1
for(rainvar in rainyday)
{
    for(evavar in evaday)
    {
        if(rainvar == evavar)
        {
            rainevaday[rainevaindex] <- evavar
            prevraineva <- rainevaindex
            rainevaindex <- rainevaindex + 1
            if(i > 1)
            {
                evavar == rainevaday[prevraineva]
                break()
            }
        }
    }
}
#Analysis 1-3: Which days have above average sunshine?
#The average sunshine throughout the year is 7.8, to simplify
#things, we have rounded it up to 8.
sunmean <- mean(sunshine)
sprintf("The average sunshine is %f", sunmean)
sunmeanr <- 8
#Goes through the sunshine database and gets a
#vector of days that have above average sunshine.
sunday <- vector()
sundayindex <- 1
for(i in 1:366)
{
    if (sunframe[i,2] > sunmeanr) {
        sunday[sundayindex] <- i
        sundayindex <- sundayindex + 1
    }
}
sprintf("There are %d days with above average sunshine (%d)", sundayindex, sunmeanr)

#Goes through both the rainy and evaporation day vector and the sunshine vector
#to find days where they rain and have above average evaporation and sunshine.
rainevasunday <- vector()
rainevasunindex <- 1
for(rainevavar in rainevaday)
{
    for(sunvar in sunday)
    {
        if(rainvar == sunvar)
        {
            rainevasunday[rainevasunindex] <- sunvar
            rainevasunindex <- rainevasunindex + 1
        }
    }
}
if(rainevasunday[1]==rainevasunday[2]){
    #There is only one day that satisfies the analysis conditions
    sprintf("The only day with zero rainfall and have both above average evaporation and sunshine is the %dth day", rainevasunday[1])
    
}else{
    rainevasuncount <- 1
    prevrainevasuncount <- 0
    print("Below are the days with zero rainfall and both the sunshine and evaporation rate are above average:")
    for(rainevasunvar in rainevasunday){
        if(rainevasuncount > 1){
            if(rainevasunday[rainevasuncount] == rainevasuncount[prevrainevasuncount]){
                break()
            }else {
                print(rainevasunday[rainevasuncount])
            }
        }else{
        print(rainevasunday[rainevasuncount])
        }
    prevrainevasuncount <- rainevasuncount
    rainevasuncount <- rainevasuncount + 1
    }
}

#Question 2: When is the best time to jog in the evening?
print("--------------------------------------------------")
print("When is the best time to jog?")
print("--------------------------------------------------")

#Analysis 2-1: Which days don not rain?
#Goes through the rain database and gets a
#vector of days that do not rain
rainyday <- vector()
rainydayindex <- 1
for(i in 1:366)
{
    if (rainfallframe[i,2] < 1.2) {
        rainyday[rainydayindex] <- i
        rainydayindex <- rainydayindex + 1
    }
}
sprintf("There are %d days that do not rain", rainydayindex)

#Analysis 2-2: Which days do not have high humidty in the evening?
humidityeframe <- data.frame(Days = num, HumidityEvening = humiditye)
lowhume <- humidityeframe[1,2]
for(i in 1:366)
{
    if(humidityeframe[i,2] < lowhume){
        lowhume <- humidityeframe[i,2]
    }
}
sprintf("The lowest humidity in the evening is %f", lowhume)
highhume <- humidityeframe[1,2]
for(i in 1:366)
{
    if(humidityeframe[i,2] > highhume){
        highhume <- humidityeframe[i,2]
    }
}
sprintf("The highest humidity in the evening is %f", highhume)
humemean <- mean(humiditye)
sprintf("The average humidity in the evening is %f", humemean)

humeday <- vector()
humedayindex <- 1
for(i in 1:366)
{
    if (humidityeframe[i,2] < 50 && humidityeframe[i,2] > 30) {
        humeday[humedayindex] <- i
    humedayindex <- humedayindex + 1
    }
}
sprintf("There are %d days with comfortable humidity levels", humedayindex)

#Goes through both the rainy day and evaporation day vector
#to find days where they both rain and have above average evaporation.
rainhumeday <- vector()
rainhumeindex <- 1
for(rainvar in rainyday)
{
    for(humevar in humeday)
    {
        if(rainvar == humevar)
        {
            rainhumeday[rainhumeindex] <- humevar
            prevrainhume <- rainhumeindex
            rainhumeindex <- rainhumeindex + 1
            if(i > 1)
            {
                humevar == rainhumeday[prevrainhume]
                break()
            }
        }
    }
}
sprintf("There are %d days with both zero rainfall and low humidity", rainhumeindex)

#Analysis 2-3: Which days have non-windy evenings?