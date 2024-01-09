#IBRAHIM AKHTAR PRIZADA
#TP059535

#Initial Coding, Data Exploration and Visualization:
#Loading Packages
library(data.table) #Loading Data Table Package
library(tidyverse)  # Contains multiple needed packages such as ggplot2, tibble, dplyr
library(corrplot)   # Loaded to allow plotting a correlation plot

#Importing provided Weather Data:

weather_dataorg= read.csv(file="C:\\Users\\emper\\Downloads\\weather.csv", header=TRUE, sep=",") #Importing Provided Dataset

#Exploring Data:
view(weather_dataorg) #viewing the weather data provided

head(weather_dataorg,20) #Viewing Top 20 rows

tail(weather_dataorg,20) #Viewing bottom 20 rows

str(weather_dataorg) #checking character Types

ncol(weather_dataorg) #Counting no of columns

nrow(weather_dataorg) #counting no of rows

summary(weather_dataorg) #Viewing a summary of Weather Data

sum(is.na(weather_dataorg)) #Checking how many NA values are there

#Initial Data Visualization:

#Declaring another variable with the same data as original data
weather_data2=weather_dataorg 

#Removing non-continuous variables and RISK_MM because it's un-needed and making another dataframe
corweather<- subset(weather_data2,select= -c(WindDir9am,WindGustDir,WindDir3pm,RainToday,RainTomorrow,RISK_MM)) 

weather_data2 <- corweather[complete.cases(corweather),] #Selecting only rows with no NA values

factor_weather<- names(which(sapply(weather_data2, class) == "factor")) 

numeric_weather <- setdiff(colnames(weather_data2), factor_weather)  


numeric_weather

numeric_weather_mat <- as.matrix(weather_data2[, numeric_weather, drop=FALSE]) #converting data to matrix

numeric_weather_cor <- cor(numeric_weather_mat) #computing the correlation function

col3 <- colorRampPalette(c("red", "white", "blue")) #deciding on a color palatte

corrplot(numeric_weather_cor, method= "square",col=col3(20))


#Initial Data Manipulation:

 #Removing Columns not needed for my analysis
  weather_data<-subset(weather_dataorg, select= -c(WindGustDir,WindDir9am,WindDir3pm,WindGustSpeed,WindSpeed9am,
                                                 WindSpeed3pm,RainToday,RISK_MM))
  
  sum(is.na(weather_data)) #Checking How many NA values remain


#Question 1: How does Sunlight affect the Weather?
#Analysis 1: Finding the relationship between Relative Humidity and sunshine

#Sunshine vs Relative Humidity 3 P.M

ggplot(weather_data, aes(x=Humidity3pm, y=Sunshine))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "lm", se=FALSE, fullrange=TRUE) +
  labs(x= "% Relative Humidity",y= "No of Sunlight hours", title= "Correlation of Sunshine and Relative Humidity at 3 P.M") 

#Sunshine VS Relative Humidity 9 A.M

ggplot(weather_data, aes(x=Humidity9am, y=Sunshine))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "lm", se=FALSE, fullrange=TRUE) +
  labs(x= "% Relative Humidity",y= "No of Sunlight hours", title= "Correlation of Sunshine and Relative Humidity at 9 A.M") 

#Analysis 2: Finding the relationship between sunshine and clouds

#Sunshine vs Cloud 9 A.M

ggplot(weather_data, aes(x=Cloud9am, y=Sunshine))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "lm",se=FALSE, fullrange=TRUE) +
  labs(x= "Amount of Clouds",y= "No of Sunlight hours", title= "Correlation of Sunshine and Amount of Clouds at 9 A.M") 

#Sunshine vs Cloud 3 P.M

ggplot(weather_data, aes(x=Cloud3pm, y=Sunshine))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "lm",se=FALSE, fullrange=TRUE) +
  labs(x= "Amount of Clouds",y= "No of Sunlight hours", title= "Correlation of Sunshine and Amount of Clouds at 3 P.M")

#Analysis 3: Finding the relationship between Sunshine and Temperature:

#Sunshine vs Minimum Temperature 

ggplot(weather_data, aes(x=Sunshine, y=MinTemp))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "lm",se=FALSE, fullrange=TRUE) +
  labs(x= "No of Sunlight hours",y= "Temperature (Celcius)", title= "Correlation of Sunshine and Minimum Temperature")

#Sunshine vs Maximum Temperature 
ggplot(weather_data, aes(x=Sunshine, y=MaxTemp))+geom_point(size=2,shape=1,color="red") +
  geom_smooth(color="black",method = "loess",se=FALSE, fullrange=TRUE) +
  labs(x= "No of Sunlight hours",y= "Temperature (Celcius)", title= "Correlation of Sunshine and Maximum Temperature")


#Question 2: What factors affects evaporation?
# Analysis 1: Finding the relationship between Evaporation and Temperature:

#Maximum Temperature vs Evaporation

ggplot(weather_data, aes(x=MaxTemp, y=Evaporation))+geom_point(size=2,shape=4,color="dark red") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Maximum Temperature (Celcius)",y= "Evaporation (mm)", title= "Correlation of Maximum Temperature and Evaporation") 

#Minimum Temperature vs Evaporation

ggplot(weather_data, aes(x=MinTemp, y=Evaporation))+geom_point(size=2,shape=4,color="dark red") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Minimum Temperature (Celcius)",y= "Evaporation (mm)", title= "Correlation of Minimum Temperature and Evaporation")

#9 A.M Temperature vs Evaporation

ggplot(weather_data, aes(x=Temp9am, y=Evaporation))+geom_point(size=2,shape=4,color="dark red") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Temperature at 9 A.M (Celcius)",y= "Evaporation (mm)", title= "Correlation of Temperature at 9 A.M and Evaporation")

#3 P.M Temperature vs Evaporation

ggplot(weather_data, aes(x=Temp9am, y=Evaporation))+geom_point(size=2,shape=4,color="dark red") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Temperature at 3 P.M (Celcius)",y= "Evaporation (mm)", title= "Correlation of Temperature at 3 P.M and Evaporation")


#Average Temperature vs Evaporation

Tempmean<-(weather_data$MaxTemp+weather_data$MinTemp)/2 #Calculating the average of min and max temp

ggplot(weather_data, aes(x=Tempmean, y=Evaporation))+geom_point(size=2,shape=4,color="dark red") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Average Temperature (Celcius)",y= "Evaporation (mm)", title= "Correlation of Average Temperature and Evaporation")

# Analysis 2: Finding the relationship between Evaporation and Humdity

#9 A.M Relative Humidity vs Evaporation

ggplot(weather_data, aes(x=Humidity9am, y=Evaporation))+geom_point(size=2,shape=4,color="dark blue") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Relative Humidity (%)",y= "Evaporation (mm)", title= "Correlation of Relative Humidity at 9 A.M and Evaporation")

#3 P.M Relative Humidity vs Evaporation

ggplot(weather_data, aes(x=Humidity3pm, y=Evaporation))+geom_point(size=2,shape=4,color="dark blue") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Relative Humidity (%)",y= "Evaporation (mm)", title= "Correlation of Relative Humidity at 3 P.M and Evaporation")

# Relative Humidity Average vs Evaporation:

meanHumid<-(weather_data$Humidity9am+weather_data$Humidity3pm)/2 #Finding the average of Humidity 9 A.M and 3 P.M

ggplot(weather_data, aes(x=meanHumid, y=Evaporation))+geom_point(size=2,shape=4,color="dark blue") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Relative Humidity (%)",y= "Evaporation (mm)", title= "Correlation of Relative Humidity Average and Evaporation")

#Analysis 3: Finding the relationship between Evaporation and Atmospheric Pressure:

#9 A.M Atmospheric Pressure vs Evaporation

ggplot(weather_data, aes(x=Pressure9am, y=Evaporation))+geom_point(size=2,shape=4,color="dark orange") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Pressure (hpa)",y= "Evaporation (mm)", title= "Correlation of Atmospheric Pressure at 9 A.M and Evaporation")

#3 P.M Atmospheric Pressure vs Evaporation

ggplot(weather_data, aes(x=Pressure3pm, y=Evaporation))+geom_point(size=2,shape=4,color="dark orange") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Pressure (hpa)",y= "Evaporation (mm)", title= "Correlation of Atmospheric Pressure at 3 P.M and Evaporation")

# Atmospheric Pressure Average vs Evaporation

meanPressure<-(weather_data$Pressure9am+weather_data$Pressure3pm)/2 #Calculating the average of Pressure 9 A.M and 3 P.M

ggplot(weather_data, aes(x=meanPressure, y=Evaporation))+geom_point(size=2,shape=4,color="dark orange") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(x= "Pressure (hpa)",y= "Evaporation (mm)", title= "Correlation of Atmospheric Pressure Average and Evaporation")

#Question 3: What factors affect chances of rain the next day?
# Analysis 1: Finding if Sunshine is a predictor for rain:

 #Density Plot
  ggplot(weather_data,aes(x=Sunshine, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
  labs(title="Sunlight Density Curve", y = "Density")+
  scale_x_continuous(limits=c(0, 15),name='No of Sunlight hours')+
  scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))

 #Box Plot
  ggplot(weather_data,aes(x=RainTomorrow, y=Sunshine, fill=RainTomorrow)) +geom_boxplot()+
  labs(title="Sunlight and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "No of Sunlight Hours")+
  theme(legend.position = "none")

# Analysis 2: Finding if Relative Humidity is a predictor for rain:
 
 #Finding if Relative Humidity 9 a.m is a predictor:
  #Density Plot
   ggplot(weather_data,aes(x=Humidity9am, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
   labs(title="Humidity 9 A.M Density Curve", y = "Density")+
   scale_x_continuous(name='Relative Humidity')+
   scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))

  #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Humidity9am, fill=RainTomorrow)) +geom_boxplot()+
   labs(title="Humidity at 9 A.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Relative Humidity")+
   theme(legend.position = "none")

 #Finding if Relative Humidity 3 p.m is a predictor
  #Density Plot
   ggplot(weather_data,aes(x=Humidity3pm, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
   labs(title="Humidity 3 P.M Density Curve", y = "Density")+
   scale_x_continuous(name='Relative Humidity')+
   scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))
  
  #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Humidity3pm, fill=RainTomorrow)) +geom_boxplot()+
   labs(title="Humidity at 3 P.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Relative Humidity")+
   theme(legend.position = "none")
   
# Analysis 3: Finding if Cloud is a predictor for rain:
   
 #Finding if Cloud at 9 a.m is a predictor:
  #Density Plot
   ggplot(weather_data,aes(x=Cloud9am, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
   labs(title="Cloud 9 A.M Density Curve", y = "Density")+
   scale_x_continuous(name='Cloud')+
   scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))
   
  #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Cloud9am, fill=RainTomorrow)) +geom_boxplot()+
   labs(title="Cloud at 9 A.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Cloud")+
   theme(legend.position = "none")
   
 #Finding if Cloud at 3 p.m is a predictor:
   #Density Plot
    ggplot(weather_data,aes(x=Cloud3pm, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
    labs(title="Cloud 3 P.M Density Curve", y = "Density")+
    scale_x_continuous(name='Cloud')+
    scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))
   
  #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Cloud3pm, fill=RainTomorrow)) +geom_boxplot()+
   labs(title="Clouds at 3 P.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Cloud")+
   theme(legend.position = "none")
  
# Analysis 4: Finding if Atmospheric Pressure is a predictor for rain:
   
 #Finding if Atmospheric Pressure 9 a.m is a predictor:
   #Density Plot
   ggplot(weather_data,aes(x=Pressure9am, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
    labs(title="Pressure 9 A.M Density Curve", y = "Density")+
     scale_x_continuous(name='Atmoshperic Pressure')+
     scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))
   
   #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Pressure9am, fill=RainTomorrow)) +geom_boxplot()+
     labs(title="Pressure at 9 A.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Atmoshperic Pressure")+
     theme(legend.position = "none")
   
   #Finding if Atmospheric Pressure 3 p.m is a predictor
   #Density Plot
   ggplot(weather_data,aes(x=Pressure3pm, fill=RainTomorrow)) +geom_density(alpha=0.3,size=1)+
     labs(title="Pressure 3 P.M Density Curve", y = "Density")+
     scale_x_continuous(name='Atmospheric Pressure')+
     scale_fill_discrete(name='Does it rain tomorrow?',labels=c("No", "Yes"))
   
   #Box Plot
   ggplot(weather_data,aes(x=RainTomorrow, y=Pressure3pm, fill=RainTomorrow)) +geom_boxplot()+
     labs(title="Pressure at 3 P.M and Rain Tomorrow Box Plot",x= "Does it Rain Tomorrow?", y = "Atmospheric Pressure")+
     theme(legend.position = "none")
   
#Question 4:What are the ideal days to grow garden vegetables?
 # Analysis 1:Finding the days with greater than 6 hours of sunlight.
   
   weather_dataday=tibble::rowid_to_column(weather_data, "Day") #Assigning Day to each row (Additional Feature)
   
   idealSunlight=filter(weather_dataday,Sunshine>=6)  #Finding Days with more than 6 hours of sunlight
   
   idealSunlight_clean<- subset(idealSunlight,select= c(Day,Sunshine)) #selecting only needed columns
   
   view(idealSunlight_clean) #Viewing result in table format
   
 #Analysis 2: Finding the days with relative humidity in 50-70 percent range:
   
   #Finding Days with Humidity in 50-70 range  
   idealHumidity= filter(weather_dataday, Humidity9am>= 50, Humidity9am<=70, Humidity3pm>= 50, Humidity3pm<= 70) 
   
   #selecting only needed columns
   idealHumidity_clean<- subset(idealHumidity,select= c(Day,Humidity9am,Humidity3pm))
   
   #viewing data
   view(idealHumidity_clean)
   
   #Analysis 3: Finding the days with Temperature in 18-24 celcius range:
   
   #Finding Days with Temperature in 18-24 celcius range:
   idealTemp= filter(weather_dataday, Temp9am>= 18, Temp9am<=24, Temp3pm>= 18, Temp3pm<= 24)
   
   #Selecting only needed columns
   idealtemp_clean<- subset(idealTemp,select= c(Day,MinTemp,MaxTemp,Temp9am,Temp3pm))
   
   #Viewing Data
   view(idealtemp_clean)
   
 #Analysis 4: Finding the days with humidity, temperature and Sunshine in ideal conditions:
   
   #Finding Days with all ideal conditions:
   idealDays= filter(weather_dataday,Sunshine>=6,Humidity9am>= 50, Humidity9am<=70, Humidity3pm>= 50, Humidity3pm<= 70,
                     Temp9am>= 18, Temp9am<=24, Temp3pm>= 18, Temp3pm<= 24)
   
   #Selecting only needed columns
   idealDays_clean<- subset(idealDays,select= c(Day,Sunshine,MinTemp,MaxTemp,Temp9am,Temp3pm,Humidity9am,Humidity3pm))
   
   #Viewing Data
   view(idealDays_clean)
   

#Question 5: How does Temperature effect the weather?

#Analysis 1: Finding the relationship between Relative Humidity and Temperature:
# Relative Humidity 9 A.M vs Temperature 9 A.M:
   
ggplot(weather_data, aes(x=Temp9am, y=Humidity9am))+geom_point(size=2,shape=8,color="dark green") +
geom_smooth(color="black",method = "loess",se=FALSE,fullrange=TRUE) +
labs(y= "Relative Humidity (in %)",x= "Temperature (Celcius)",
     title= "Correlation of Relative Humidity at 9 A.M and Temperature at 9 A.M")

# Relative Humidity 3 P.M vs Temperature 3 P.M:

ggplot(weather_data, aes(x=Temp3pm, y=Humidity3pm))+geom_point(size=2,shape=8,color="dark green") +
geom_smooth(color="black",method = "loess",se=FALSE,fullrange=TRUE) +
labs(y= "Relative Humidity (in %)",x= "Temperature (Celcius)", title=
       "Correlation of Relative Humidity at 3 P.M and Temperature at 3 P.M")

# Relative Humidity Average (9 A.M and 3 P.M) vs Temperature Average (9 A.M and 3 P.M) :

temp9n3mean<-(weather_data$Temp9am+weather_data$Temp3pm)/2 #Calculating Average of 9 A.M and 3 P.M Average

meanHumid<-(weather_data$Humidity9am+weather_data$Humidity3pm)/2 #Calculating Average of 9 A.M and 3 P.M Relative Humidity

ggplot(weather_data,aes(x=temp9n3mean, y=meanHumid))+geom_point(size=2,shape=8,color="dark green") +
geom_smooth(color="black",method = "loess",se=FALSE,fullrange=TRUE) +
labs(y= "Relative Humidity (in %)",x= "Temperature (Celcius)",
     title= "Correlation of Relative Humidity Average (9 A.M and 3 P.M) and Temperature Average (9 A.M and 3 P.M)")

#Analysis 2: Finding the relationship between Atmospheric Pressure and Temperature:
# Atmospheric Pressure 9 A.M vs Temperature 9 A.M:

ggplot(weather_data, aes(x=Temp9am, y=Pressure9am))+geom_point(size=2,shape=8,color="dark green") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(y= "Atmospheric Pressure (hpa)",x= "Temperature (Celcius)",
       title= "Correlation of Atmospheric Pressure at 9 A.M and Temperature at 9 A.M")

# Atmospheric Pressure 3 P.M vs Temperature 3 P.M:

ggplot(weather_data, aes(x=Temp3pm, y=Pressure3pm))+geom_point(size=2,shape=8,color="dark green") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(y= "Atmospheric Pressure (hpa)",x= "Temperature (Celcius)",
       title= "Correlation of Atmospheric Pressure at 3 P.M and Temperature at 3 P.M")

# Relative Humidity Average (9 A.M and 3 P.M) vs Temperature Average (9 A.M and 3 P.M) :

meanPressure<-(weather_data$Pressure9am+weather_data$Pressure3pm)/2 #Calculating Average of 9 A.M and 3 P.M Atmospheric Pressure

ggplot(weather_data, aes(x=temp9n3mean, y=meanPressure))+geom_point(size=2,shape=8,color="dark green") +
  geom_smooth(color="black",method = "lm",se=FALSE,fullrange=TRUE) +
  labs(y= "Atmospheric Pressure (hpa)",x= "Temperature (Celcius)",
       title= "Correlation of Atmospheric Pressure Average (9 A.M and 3 P.M) and Temperature Average (9 A.M and 3 P.M")

