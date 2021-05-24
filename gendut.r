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
winddirm <- weatherdata[,8]
windspdm <- weatherdata[,10]
windspde <- weatherdata[,11]
humiditym <- weatherdata[,12]
humiditye <- weatherdata[,13]
pressurem <- weatherdata[,14]
pressuree <- weatherdata[,15]
cloudm <- weatherdata[,16]
cloude <- weatherdata[,17]

sunshine[is.na(sunshine)] <- 0
windspdm[is.na(windspdm)] <- 0
gustspd[is.na(gustspd)] <- 0

tempframe = data.frame(Min = mintemp, Max = maxtemp)
evaporationframe = data.frame(Days = num, Evaporation = evaporation)
rainfallframe <- data.frame(Days = num, Rainfall = rainfall)
sunframe <- data.frame(Days = num, Sunshine = sunshine)
gustspdframe <- data.frame(Days = num, WindGustSpeed = gustspd)
winddirmframe <- data.frame(Days = num, WindDirMorning = winddirm)
windspdframe <- data.frame(Morning = windspdm, Evening = windspde)
windspdmframe <- data.frame(Days = num, WindSpeedMorning = windspdm)
windspdeframe <- data.frame(Days = num, WindSpeedEvening = windspde)
humidityframe <- data.frame(Morning = humiditym, Evening = humiditye)
humidityeframe <- data.frame(Days = num, HumidityEvening = humiditye)
pressureframe <- data.frame(Morning = pressurem, Evening = pressuree)
cloudframe <- data.frame(Morning = cloudm, Evening = cloude)
cloudmframe <- data.frame(Days = num, CloudMorning = cloudm)

tempmedian <- rowMeans(tempframe)
windspdmedian <- rowMeans(windspdframe)
humiditymedian <- rowMeans(humidityframe)
pressuremed <- rowMeans(pressureframe)
cloudmedian <- rowMeans(cloudframe)

tempmedframe <- data.frame(Days = num, Temperature_Median = tempmedian)
windspdmedframe <- data.frame(Days = num, WindSpeedMedian = windspdmedian)
humiditymedframe <- data.frame(Days = num, HumidityMedian = humiditymedian)
pressuremedframe <- data.frame(Days = num, PressureMedian = pressuremed)
cloudmedframe <- data.frame(Days = num, CloudMedian = cloudmedian)



#Question 1
#When is the best time to dry your clothes?
print("------------------------------------------")
print("When is the best time to hang clothes?")
print("------------------------------------------")

#Analysis 1-1: Which days have zero rainfall?

#Goes through the rainfall data frame and gets a
#vector of days with zero rainfall
rainyday <- vector()
rainydayindex <- 1
for(i in 1:366)
{
    if (rainfallframe[i,2] < 0.2) {
        rainyday[rainydayindex] <- i
        rainydayindex <- rainydayindex + 1
    }
}
sprintf("There are %d days with zero rainfall", rainydayindex)

#Analysis 1-2: Which days have above average evaporation rate?

#The average evaporation rate throughout the year is 4.5, to simplify
#things, we have rounded it up to 5.
evamean <- mean(evaporation)
evameanr <- as.integer(round(evamean)) #5

#Goes through the evaporation data frame and gets a
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

#Analysis 1-3: Which days have above average sunshine?

#The average sunshine throughout the year is 7.8, to simplify
#things, we have rounded it up to 8.
sunmean <- mean(sunshine, na.rm = TRUE)
sunmeanr <- as.integer(round(sunmean)) #8

#Goes through the sunshine data frame and gets a
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

#Results 1

#Goes through both the rainy day and evaporation day vector
#to find days that exist in both vectors.
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

#Goes through both the rainy and evaporation day vector and the sunshine vector
#to find days that exist in both vectors.
rainevasunday <- vector()
rainevasunindex <- 1
for(rainevavar in rainevaday)
{
    for(sunvar in sunday)
    {
        if(rainevavar == sunvar)
        {
            rainevasunday[rainevasunindex] <- sunvar
            rainevasunindex <- rainevasunindex + 1
        }
    }
}
sprintf("Below are the days suitable for hanging clothes:")
print(rainevasunday)

#Question 2: When is the best time to exercise in the evening?
print("-------------------------------------------------")
print("When is the best time to exercise in the evening?")
print("-------------------------------------------------")

#Analysis 2-1: Which days do not rain?

#Goes through the rainfall data frame and gets a
#vector of days that do not rain.
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

#The average humidity in the evening throughout the year is 44.5, to simplify
#things, we have rounded it up to 45.
humemean <- mean(humiditye, na.rm = TRUE)
humemeanr <- as.integer(round(humemean)) #45

#Goes through the evening humidity data frame and gets a
#vector of days with comfortable humidity levels.
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

#Analysis 2-3: Which days have non-windy evenings?

#The average wind speed in the evening throughout the year is 17.9, to simplify
#things, we have rounded it up to 18.
windsemean <- mean(windspde, na.rm = TRUE)
windsemeanr <- as.integer(round(windsemean)) #18

#Goes through the rainfall data frame and gets a
#vector of days with non-windy wind speed in the evenings.
windseday <- vector()
windseindex <- 1
for(i in 1:366)
{
    if (windspdeframe[i,2] < 20) {
        windseday[windseindex] <- i
    windseindex <- windseindex + 1
    }
}
sprintf("There are %d days with non-windy evenings", windseindex)

#Results 2

#Goes through both the rainy day and humidity evening day vector
#to find days that belong in both vectors.
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

#Goes through both the rainy day and humidity evening day vector and the wind speed evening vector
#to find days that belong in both vectors.
rainhumewindseday <- vector()
rainhumewindseindex <- 1
for(rainhumevar in rainhumeday)
{
    for(windsevar in sunday)
    {
        if(rainhumevar == windsevar)
        {
            rainhumewindseday[rainhumewindseindex] <- windsevar
            rainhumewindseindex <- rainhumewindseindex + 1
        }
    }
}
sprintf("Below are the days that are suitable for exercising in the evening:")
print(rainhumewindseday)

#Question 3: When is the best time to fly from the east to the west?
print("-------------------------------------------------------")
print("When is the best time to fly from the east to the west?")
print("-------------------------------------------------------")

#Analysis 3-1: Which days have winds travelling to the west?

#Goes through the morning wind direction data frame
#and convert the direction format from letter-based compass
#standard to numbers for easier processing.
winddirm[is.na(winddirm)] <- 0
for(i in 1:366)
{
    if (grepl(winddirm[i], "N", fixed=TRUE)) {
        winddirm[i] <- 1
    }else if (grepl(winddirm[i], "E", fixed=TRUE)) {
        winddirm[i] <- 5
    }else if (grepl(winddirm[i], "S", fixed=TRUE)) {
        winddirm[i] <- 9
    }else if (grepl(winddirm[i], "W", fixed=TRUE)) {
        winddirm[i] <- 13
    }else if (grepl(winddirm[i], "NE", fixed=TRUE)) {
        winddirm[i] <- 3
    }else if (grepl(winddirm[i], "SE", fixed=TRUE)) {
        winddirm[i] <- 7
    }else if (grepl(winddirm[i], "SW", fixed=TRUE)) {
        winddirm[i] <- 11
    }else if (grepl(winddirm[i], "NW", fixed=TRUE)) {
        winddirm[i] <- 15
    }else if (grepl(winddirm[i], "NNE", fixed=TRUE)) {
        winddirm[i] <- 2
    }else if (grepl(winddirm[i], "NNW", fixed=TRUE)) {
        winddirm[i] <- 16
    }else if (grepl(winddirm[i], "SSE", fixed=TRUE)) {
        winddirm[i] <- 8
    }else if (grepl(winddirm[i], "SSW", fixed=TRUE)) {
        winddirm[i] <- 10
    }else if (grepl(winddirm[i], "ENE", fixed=TRUE)) {
        winddirm[i] <- 4
    }else if (grepl(winddirm[i], "ESE", fixed=TRUE)) {
        winddirm[i] <- 6
    }else if (grepl(winddirm[i], "WNW", fixed=TRUE)) {
        winddirm[i] <- 14
    }else if (grepl(winddirm[i], "WSW", fixed=TRUE)) {
        winddirm[i] <- 12
    }
}
winddirm <- strtoi(winddirm)
winddirmframe <- data.frame(Days = num, WindDirMorning = winddirm)

#Goes through the converted morning wind direction data frame and gets a
#vector of days where the wind blows to the west.
winddirmday <- vector()
winddirmdayindex <- 1
for(i in 1:366)
{
    if (winddirmframe[i,2] > 9) {
        winddirmday[winddirmdayindex] <- i
        winddirmdayindex <- winddirmdayindex + 1
    }
}
sprintf("There are %d days where the winds travel to the west in the morning", winddirmdayindex)

#Analysis 3-2: Which days do not have cloudy mornings?

#The average cloudiness in the morning throughout the year is 3.8, to simplify
#things, we have rounded it up to 4.
cloudmmean <- mean(cloudm, na.rm = TRUE)
cloudmmeanr <- as.integer(round(cloudmmean)) #4

#Goes through the morning cloudiness data frame and gets a
#vector of days that have below average cloudiness in the morning.
cloudmday <- vector()
cloudmdayindex <- 1
for(i in 1:366)
{
    if (cloudmframe[i,2] < cloudmmeanr) {
        cloudmday[cloudmdayindex] <- i
        cloudmdayindex <- cloudmdayindex + 1
    }
}
sprintf("There are %d days with below average cloudiness (%d)", cloudmdayindex, cloudmmeanr)

#Analysis 3-3: Which days do not rain heavily?

#Goes through the rainfall data frame and gets a
#vector of days with low rainfall.
rainyday <- vector()
rainydayindex <- 1
for(i in 1:366)
{
    if (rainfallframe[i,2] < 10) {
        rainyday[rainydayindex] <- i
        rainydayindex <- rainydayindex + 1
    }
}
sprintf("There are %d days with less than 10 rainfall", rainydayindex)

#Results 3

#Goes through both the morning wind direction vector and morning cloudiness day vector
#to find days where that belong in both vectors.
winddirmcloudmday <- vector()
winddirmcloudmindex <- 1
for(winddirmvar in winddirmday)
{
    for(cloudmvar in cloudmday)
    {
        if(winddirmvar == cloudmvar)
        {
            winddirmcloudmday[winddirmcloudmindex] <- cloudmvar
            prevwinddirmcloudm <- winddirmcloudmindex
            winddirmcloudmindex <- winddirmcloudmindex + 1
            if(i > 1)
            {
                cloudmvar == winddirmcloudmday[prevwinddirmcloudm]
                break()
            }
        }
    }
}

#Goes through both the morning wind direction and morning cloudiness day vector
#and the rainy day vector to find days that belong in both vectors.
winddirmcloudmrainday <- vector()
winddirmcloudmrainindex <- 1
for(winddirmcloudmvar in winddirmcloudmday)
{
    for(raintwovar in rainyday)
    {
        if(winddirmcloudmvar == raintwovar)
        {
            winddirmcloudmrainday[winddirmcloudmrainindex] <- raintwovar
            winddirmcloudmrainindex <- winddirmcloudmrainindex + 1
        }
    }
}
sprintf("Below are the days suitable for flights to the west:")
print(winddirmcloudmrainday)

#Question 4: When is the best time to ski?
print("-----------------------------")
print("When is the best time to ski?")
print("-----------------------------")

#Analysis 4-1: When is winter?

#A season lasts for around 3 months, from an analysis of
#the temperature median graph, it has been approximated that
#winter occurs from around the 124th day and ends at around the 214th day.
ggplot(data = tempmedframe, mapping = aes(x = Days, y = Temperature_Median)) + geom_line(colour = "red")
winterday <- 124:214

#Analysis 4-2: When will the constant wind speed be low?

#The average morning wind speed in the morning throughout the year is 9.4, to simplify
#things, we have rounded it down to 9.
windspdmmean <- mean(windspdm, na.rm = TRUE)
windspdmmeanr <- as.integer(round(windspdmmean))

#Goes through the morning wind speed data frame and gets a
#vector of days with low wind speed.
windsmday <- vector()
windsmindex <- 1
for(i in 1:366)
{
    if (windspdmframe[i,2] < 9) {
        windsmday[windsmindex] <- i
    windsmindex <- windsmindex + 1
    }
}
sprintf("There are %d days with non-windy evenings", windsmindex)

#Analysis 4-3: When will there be a lack of gust?

#The average gust speed throughout the year is 39.6, to simplify
#things, we have rounded it up to 40.
gustspdmean <- mean(gustspd, na.rm = TRUE)
gustspdmeanr <- as.integer(round(gustspdmean))

#Goes through the gust speed data frame and gets a
#vector of days with low gust speed.
gustspdday <- vector()
gustspdindex <- 1
for(i in 1:366)
{
    if (gustspdframe[i,2] < 40) {
        gustspdday[gustspdindex] <- i
    gustspdindex <- gustspdindex + 1
    }
}
sprintf("There are %d days with below average gust speed", gustspdindex)

#Results 4

#Goes through both the morning winter vector and morning wind speed vector
#to find days that belong in both vectors.
winterwindsmday <- vector()
winterwindsmindex <- 1
for(wintervar in winterday)
{
    for(windsmvar in windsmday)
    {
        if(wintervar == windsmvar)
        {
            winterwindsmday[winterwindsmindex] <- windsmvar
            prevwinterwinds <- winterwindsmindex
            winterwindsmindex <- winterwindsmindex + 1
            if(i > 1)
            {
                windsmvar == winterwindsmday[prevwinterwinds]
                break()
            }
        }
    }
}

#Goes through both the morning winter and morning wind speed vector and the gust speed vector
#to find days that belong in both vectors.
winterwindsmgustspdday <- vector()
winterwindsmgustspdindex <- 1
for(winterwindsmvar in winterwindsmday)
{
    for(gustspdvar in gustspdday)
    {
        if(winterwindsmvar == gustspdvar)
        {
            winterwindsmgustspdday[winterwindsmgustspdindex] <- gustspdvar
            winterwindsmgustspdindex <- winterwindsmgustspdindex + 1
        }
    }
}
sprintf("Below are the days suitable for skiing:")
print(winterwindsmgustspdday)

#Question 5: When will a storm occur?
print("------------------------")
print("When will a storm occur?")
print("------------------------")

#Analysis 5-1: When will the air pressure drop?

#The average air pressure throughout the year is 1018.2, to simplify
#things, we have rounded it down to 1018.
pressuremedmean <- mean(pressuremed, na.rm = TRUE)
pressuremedmeanr <- as.integer(round(pressuremedmean))

#Goes through the median pressure data frame and gets a
#vector of days with low median pressure.
pressuremedday <- vector()
pressuremedindex <- 1
for(i in 1:366)
{
    if (pressuremedframe[i,2] < 1018) {
        pressuremedday[pressuremedindex] <- i
    pressuremedindex <- pressuremedindex + 1
    }
}
sprintf("There are %d days with below average air pressure", pressuremedindex)

#Analysis 5-2: When will there be heavy rain?

#Goes through the rainfall data frame and gets a
#vector of days with high rainfall.
rainyday <- vector()
rainydayindex <- 1
for(i in 1:366)
{
    if (rainfallframe[i,2] > 20) {
        rainyday[rainydayindex] <- i
        rainydayindex <- rainydayindex + 1
    }
}
sprintf("There are %d days with heavy rainfall", rainydayindex)

#Analysis 5-3: When will there be strong gusts of wind?

#The average gust speed throughout the year is 39.6, to simplify
#things, we have rounded it up to 40.
gustspdmean <- mean(gustspd, na.rm = TRUE)
gustspdmeanr <- as.integer(round(gustspdmean)) #40

#Goes through the gust speed data frame and gets a
#vector of days with high gust speed.
gustspdday <- vector()
gustspdindex <- 1
for(i in 1:366)
{
    if (gustspdframe[i,2] > gustspdmeanr) {
        gustspdday[gustspdindex] <- i
    gustspdindex <- gustspdindex + 1
    }
}
sprintf("There are %d days with above average gust speed", gustspdindex)

#Results 5

#Goes through both the pressure median vector and rain vector
#to find days that belong in both vectors.
pressuremedrainday <- vector()
pressuremedrainindex <- 1
for(pressuremedvar in pressuremedday)
{
    for(rainthreevar in rainyday)
    {
        if(pressuremedvar == rainthreevar)
        {
            pressuremedrainday[pressuremedrainindex] <- rainthreevar
            prevpressuremedrain <- pressuremedrainindex
            pressuremedrainindex <- pressuremedrainindex + 1
            if(i > 1)
            {
                rainthreevar == pressuremedrainday[prevpressuremedrain]
                break()
            }
        }
    }
}

#Goes through both the pressure median and rain vector and the gust speed vector
#to find days that belong in both vectors.
pressuremedgustspdday <- vector()
pressuremedgustspdindex <- 1
for(pressuremedrainvar in pressuremedrainday)
{
    for(gustspdvartwo in gustspdday)
    {
        if(pressuremedrainvar == gustspdvartwo)
        {
            pressuremedgustspdday[pressuremedgustspdindex] <- gustspdvartwo
            pressuremedgustspdindex <- pressuremedgustspdindex + 1
        }
    }
}
sprintf("Below are the days where storms occur:")
print(pressuremedgustspdday)





#Graphs
ggplot(data = evaporationframe, mapping = aes(x = Days, y = Evaporation)) + geom_line(colour = "red")
ggplot(data = rainfallframe, mapping = aes(x = Days, y = Rainfall)) + geom_line(colour = "blue")
ggplot(data = sunframe, mapping = aes(x = Days, y = Sunshine)) + geom_line(colour = "#000000")
ggplot(data = gustspdframe, mapping = aes(x = Days, y = WindGustSpeed)) + geom_line(colour = "green")
ggplot(data = windspdmedframe, mapping = aes(x = Days, y = WindSpeedMedian)) + geom_line(colour = "green")
ggplot(data = humiditymedframe, mapping = aes(x = Days, y = HumidityMedian)) + geom_line(colour = "blue")
ggplot(data = pressuremedframe, mapping = aes(x = Days, y = PressureMedian)) + geom_line(colour = "blue")
ggplot(data = cloudmedframe, mapping = aes(x = Days, y = CloudMedian)) + geom_line(colour = "blue")