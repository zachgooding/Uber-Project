# Uber-Project
## Introduction
This project is a breakdown of Uber rideshare data. The charts, heatmaps, and leaflet give an in depth breakdown of the csv files that are a half a year of data from 2014. A model was also created to predict rideshare using the data.
## Working Directory and CSV Files
```
#set working directory
setwd("~/Documents/Data332")

#loading in csv files and naming the month for each data frame
df_april <- read.csv('uber-raw-data-apr14.csv')
df_august <- read.csv('uber-raw-data-aug14.csv')
df_july <- read.csv('uber-raw-data-jul14.csv')
df_june <- read.csv('uber-raw-data-jun14.csv')
df_may <- read.csv('uber-raw-data-may14.csv')
df_september <- read.csv('uber-raw-data-sep14.csv')

#Combine all the month datasets into a master dataset using the rbind function
df_master <- rbind(df_april, df_august, df_july, df_june, df_may, df_september)

#Separate the Date.Time column into two separate columns to tidy the dataset so each column has 1 variable
df_uber <- separate(df_master, col = Date.Time, into = c("Date", "Time"), sep = " ")
```
## Date and Time Schema
```
#Mutate the Date and Time columns so that they read as a date schema and time schema instead of a character
df_uber1 <- df_uber %>%
  mutate(Date = as.POSIXct(Date, format = "%m/%d/%Y")) %>%
  mutate(Time = strptime(Time, format = "%H:%M:%S"))
```
## Pivot Table Trips by Hour
```
#Pivot table that displays trips by the hour
df_pivot <- df_uber1 %>%
  mutate(Time = format(as.POSIXct(Time), "%H")) %>%
  group_by(Time) %>%
  summarize(n = n())
```
## Bar Charts
```
#Chart that shows Trips by Hour and Month
chart1_data <- df_uber1 %>%
  mutate(Time = format(as.POSIXct(Time), "%H")) %>%
  mutate(Date = format(as.POSIXct(Date), "%m")) %>%
  group_by(Date, Time) %>%
  summarize(n = n())

ggplot(data = chart1_data, aes(x = Date, y = n, fill = Time)) +
  geom_bar(stat = "identity", position = "stack")

#Chart that displays Trips Every Hour
ggplot(data = df_pivot, aes(x = Time, y = n, fill = "blue")) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue"))

#Plot data by trips taken during every day of the month
chart2_data <- df_uber1 %>%
  mutate(Month = format(as.POSIXct(Date), "%m")) %>%
  mutate(Day = format(as.POSIXct(Date), "%d")) %>%
  group_by(Month, Day) %>%
  summarize(n = n())

ggplot(data = chart2_data, aes(x = Month, y = n, fill = Day)) +
  geom_bar(stat = "identity", position = "stack")

#Chart by Trips by Day and Month
chart3_data <- df_uber1 %>%
  mutate(Month = format(as.POSIXct(Date), "%m")) %>%
  mutate(DayOfWeek = weekdays(Date)) %>%
  group_by(Month, DayOfWeek) %>%
  summarize(n = n())

ggplot(data = chart3_data, aes(x = Month, y = n, fill = DayOfWeek )) +
  geom_bar(stat = "identity", position = "stack")

#Chart Trips by Bases and Month
chart4_data <- df_uber1 %>%
  mutate(Month = format(as.POSIXct(Date), "%m")) %>%
  group_by(Base, Month) %>%
  summarize(n = n())

ggplot(data = chart4_data, aes(x = Base, y = n, fill = Month)) +
  geom_bar(stat = "identity", position = "stack")
```
## Data Table
```
#Table that shows Trips Every Day
data_table <- DT::renderDataTable(chart2_data[,c(Month, Day, n)],options = list(pageLength = 4))
```
##Heat Maps
```
#Heat Map that displays by hour and day
heat1_data <- df_uber1 %>%
  mutate(Time = format(as.POSIXct(Time), "%H")) %>%
  mutate(Day = format(as.POSIXct(Date), "%d")) %>%
  group_by(Time, Day) %>%
  summarize(n = n())

ggplot(data = heat1_data, aes(x = Time, y = Day, fill = n)) +
  geom_tile()

#Heat Map by month and day
heat2_data <- df_uber1 %>%
  mutate(Month = format(as.POSIXct(Date), "%m")) %>%
  mutate(Day = format(as.POSIXct(Date), "%d")) %>%
  group_by(Month, Day) %>%
  summarize(n = n())

ggplot(data = heat2_data, aes(x = Month, y = Day, fill = n)) +
  geom_tile()

#Heat Map by month and week
heat3_data <- df_uber1 %>%
  mutate(Month = format(as.POSIXct(Date), "%m")) %>%
  mutate(Week = format(Date), "%W") %>%
  group_by(Month, Week) %>%
  summarize(n = n())

ggplot(data = heat3_data, aes(x = Month, y = Week, fill = n)) +
  geom_tile()  

#Heat Map Bases and Day of Week
heat4_data <- df_uber1 %>%
  mutate(DayOfWeek = weekdays(Date)) %>%
  group_by(Base, DayOfWeek) %>%
  summarize(n = n())

ggplot(data = heat4_data, aes(x = Base, y = DayOfWeek, fill = n)) +
  geom_tile()
```
## Leaflet
```
#Leaflet Shiny Geospatial Map
df_uber1$Lat <- round(df_uber1$Lat, 2)
df_uber1$Lon <- round(df_uber1$Lon, 2)

leaflet_data <- df_uber1 %>%
  group_by(Lat, Lon) %>%
  summarize(n = n())

leaflet(leaflet_data) %>%
  addTiles() %>%
  addMarkers(lng = ~Lon,
             lat = ~Lat,
             popup = ~n)
      
