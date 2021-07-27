#------ Preprocess Steps --------

dev.off() # Clear the graph window
cat('\014')   # Clear the console
rm(list=ls()) # Clear user objects from  the environment

#------ Set working directory and global variables --- 

working_dir <- "C:/Users/kastu/Desktop/Syracuse/Spring 21/IST687 DS/Project"
setwd(working_dir)

#------ Setting up libraries ----

if(!require("readxl")){install.packages("readxl")}
if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("xlsx")){install.packages("xlsx")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("wordcloud")){install.packages("wordcloud")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}
if(!require("countrycode")){install.packages("countrycode")}
if(!require("arules")){install.packages("arules")}
if(!require("arulesViz")){install.packages("arulesViz")}
if(!require("kernlab")){install.packages("kernlab")}
if(!require("caret")){install.packages("caret")}
if(!require("e1071")){install.packages("arulesViz")}


library(readxl)
library(xlsx)
library(countrycode)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(arules)
library(arulesViz)
library(kernlab)
library(caret)
library(e1071)

#------ Data Preparation --------

#Reading the datasets

resort_hotel <- read_excel('H1-Resort.xlsx')
str(resort_hotel) #40060 observations with 28 attributes \
summary(resort_hotel) #Summary of the whole dataset where descriptive stats of each attribute are shown

city_hotel <- read_excel('H2-City.xlsx')
str(city_hotel) #79330 observations with 28 attributes
summary(city_hotel) #Summary of the whole dataset where descriptive stats of each attribute are shown

#Checking for NAs
cbind(
  lapply(
    lapply(city_hotel, is.na)
    , sum)
) #39270 NA values in the Arrival Date column

cbind(
  lapply(
    lapply(resort_hotel, is.na)
    , sum)
) #0 NA values
#This shows us that all the NAs are in the Arrival Date column
#View(city_hotel)

# Verifying if any other column has na values:
x <- city_hotel[,-3] # generating a table without the arrivaldate column
na_x<- sum(is.na(x))
na_x #0, implying no other na values are present.

#------ Data Cleaning ------

#Converting attributes to factor type for proper analysis
city_hotel$Agent <- as.factor(city_hotel$Agent)
city_hotel$AssignedRoomType <- as.factor(city_hotel$AssignedRoomType)
city_hotel$Company <- as.factor(city_hotel$Company)
city_hotel$Country <- as.factor(city_hotel$Country)
city_hotel$CustomerType <- as.factor(city_hotel$CustomerType)
city_hotel$DepositType <- as.factor(city_hotel$DepositType)
city_hotel$DistributionChannel <- as.factor(city_hotel$DistributionChannel)
city_hotel$IsRepeatedGuest <- as.factor(city_hotel$IsRepeatedGuest)
city_hotel$MarketSegment <- as.factor(city_hotel$MarketSegment)
city_hotel$Meal <- as.factor(city_hotel$Meal)
city_hotel$ReservationStatus <- as.factor(city_hotel$ReservationStatus)
city_hotel$ReservedRoomType <- as.factor(city_hotel$ReservedRoomType)
city_hotel$Children <- as.numeric(city_hotel$Children)
city_hotel <- city_hotel %>% drop_na(Children) #Dropping 4 rows which has NA values in the attribute Children
#city_hotel <- city_hotel[,-3] #dropping arrival date column as new one with data is created.
str(city_hotel) #Verifying the conversions



#Converting attributes to factor type for proper analysis
resort_hotel$Agent <- as.factor(resort_hotel$Agent)
resort_hotel$AssignedRoomType <- as.factor(resort_hotel$AssignedRoomType)
resort_hotel$Company <- as.factor(resort_hotel$Company)
resort_hotel$Country <- as.factor(resort_hotel$Country)
resort_hotel$CustomerType <- as.factor(resort_hotel$CustomerType)
resort_hotel$DepositType <- as.factor(resort_hotel$DepositType)
resort_hotel$DistributionChannel <- as.factor(resort_hotel$DistributionChannel)
resort_hotel$IsRepeatedGuest <- as.factor(resort_hotel$IsRepeatedGuest)
resort_hotel$MarketSegment <- as.factor(resort_hotel$MarketSegment)
resort_hotel$Meal <- as.factor(resort_hotel$Meal)
resort_hotel$ReservationStatus <- as.factor(resort_hotel$ReservationStatus)
resort_hotel$ReservedRoomType <- as.factor(resort_hotel$ReservedRoomType)
str(resort_hotel) #Verifying the conversions

#Creating Derived Variables

#Creating a variable to calculate the average revenue per stay
resort_hotel$DaysStayed <- resort_hotel$StaysInWeekendNights + resort_hotel$StaysInWeekNights
city_hotel$DaysStayed <- city_hotel$StaysInWeekendNights + city_hotel$StaysInWeekNights

resort_hotel$Avg_Revenue_per_Stay <- resort_hotel$DaysStayed * resort_hotel$ADR
city_hotel$Avg_Revenue_per_Stay <- city_hotel$DaysStayed * city_hotel$ADR

#Creating a newArrivalDate column
#In terms of city_hotel which has a lot of NA values, newArrivalDate is calculated based on reservation status updation date and number of days stayed
city_hotel$newArrival <- as.Date(city_hotel$ReservationStatusDate - city_hotel$DaysStayed) 
resort_hotel$newArrival <- as.Date(resort_hotel$ReservationStatusDate - resort_hotel$DaysStayed) 

View(city_hotel)

#Create aggregate variables
city_hotel$TotalPeople <-  city_hotel$Adults + city_hotel$Children + city_hotel$Babies
city_hotel$TotalChildren <- city_hotel$Babies + city_hotel$Children
resort_hotel$TotalPeople <-  resort_hotel$Adults + resort_hotel$Children + resort_hotel$Babies
resort_hotel$TotalChildren <- resort_hotel$Babies + resort_hotel$Children

#Creating visitor type variable based on total number of people
city_hotel<- mutate(city_hotel, VisitorType = case_when( TotalPeople  == 1~ "Single",
                                                         TotalPeople == 2 & Adults == 2 ~ "Couple",
                                                         TotalPeople > 2 ~ "Family",
                                                         TotalPeople >= 2 & Adults >= 1 & TotalChildren >= 1 ~ "Family" ,
                                                         TotalPeople == 0 ~ "Family"
))

resort_hotel<- mutate(resort_hotel, VisitorType = case_when( TotalPeople  == 1~ "Single",
                                                             TotalPeople == 2 & Adults == 2 ~ "Couple",
                                                             TotalPeople > 2 ~ "Family",
                                                             TotalPeople >= 2 & Adults >= 1 & TotalChildren >= 1 ~ "Family" ,
                                                             TotalPeople == 0 ~ "Family"
))


#Creating seasonality variable based on when people visit.
resort_hotel <- mutate(resort_hotel, Season = case_when( format(resort_hotel$newArrival, "%m") == '03' ~ "Spring",
                                                         format(resort_hotel$newArrival, "%m") == '04' ~ "Spring",
                                                         format(resort_hotel$newArrival, "%m") == '05' ~ "Spring",
                                                         format(resort_hotel$newArrival, "%m") == '06' ~ "Summer",
                                                         format(resort_hotel$newArrival, "%m") == '07' ~ "Summer",
                                                         format(resort_hotel$newArrival, "%m") == '08' ~ "Summer",
                                                         format(resort_hotel$newArrival, "%m") == '09' ~ "Fall",
                                                         format(resort_hotel$newArrival, "%m") == '10' ~ "Fall",
                                                         format(resort_hotel$newArrival, "%m") == '11' ~ "Fall",
                                                         format(resort_hotel$newArrival, "%m") == '12' ~ "Winter",
                                                         format(resort_hotel$newArrival, "%m") == '01' ~ "Winter",
                                                         format(resort_hotel$newArrival, "%m") == '02' ~ "Winter"
))
city_hotel <- mutate(city_hotel, Season = case_when( format(city_hotel$newArrival, "%m") == '03' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '04' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '05' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '06' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '07' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '08' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '09' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '10' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '11' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '12' ~ "Winter",
                                                     format(city_hotel$newArrival, "%m") == '01' ~ "Winter",
                                                     format(city_hotel$newArrival, "%m") == '02' ~ "Winter"
))

#Creating a column for Deposit type numeric
city_hotel<- mutate(city_hotel, DepositFactor = case_when( DepositType  == 'No Deposit'~ 1,
                                                           DepositType == 'Non Refund' ~ 2,
                                                           DepositType == 'Refundable' ~ 3
))
resort_hotel<- mutate(resort_hotel, DepositFactor = case_when( DepositType  == 'No Deposit'~ 1,
                                                               DepositType == 'Non Refund' ~ 2,
                                                               DepositType == 'Refundable' ~ 3
))

#Creating a column for Reservation to numeric levels
city_hotel<- mutate(city_hotel, ReservationFactor = case_when( ReservationStatus == 'Canceled'~ 1,
                                                               ReservationStatus == 'Check-Out' ~ 2,
                                                               ReservationStatus == 'No-Show' ~ 3
))
resort_hotel<- mutate(resort_hotel, ReservationFactor = case_when( ReservationStatus == 'Canceled'~ 1,
                                                                   ReservationStatus == 'Check-Out' ~ 2,
                                                                   ReservationStatus == 'No-Show' ~ 3
))

#Checking for NAs again
cbind(
  lapply(
    lapply(city_hotel, is.na)
    , sum)
) #39270 NA values in the Arrival Date column as before and 205 NA values in the VisitorType column

cbind(
  lapply(
    lapply(resort_hotel, is.na)
    , sum)
) #205 NA values in the VistorType variable

#Dropping the rows with NA values as they are insignificant
city_hotel <- city_hotel %>% drop_na(VisitorType)
resort_hotel <- resort_hotel %>% drop_na(VisitorType)
resort_hotel <- resort_hotel %>% drop_na(ReservationFactor)


#Removing ArrivalDate column due to the NA Values and also because we created newArrival
city_hotel <- city_hotel[,-3]
resort_hotel <- resort_hotel[,-3]


#Final Check for NAs
cbind(
  lapply(
    lapply(city_hotel, is.na)
    , sum)
)

cbind(
  lapply(
    lapply(resort_hotel, is.na)
    , sum)
) 
#Both the datasets have no remaining NA Values

#-------- Data Exploration ------

#Changing country codes into country names
city_hotel$Country <- countrycode(city_hotel$Country, origin='iso3c',destination='country.name')
city_hotel$Country[city_hotel$Country == 'CN'] <- 'Pitcairn'
city_hotel$Country[city_hotel$Country == 'TMP'] <- 'East Timor'
city_hotel <- city_hotel %>% drop_na(Country)
#View(data.frame(city_hotel$Country))

resort_hotel$Country <- countrycode(resort_hotel$Country, origin='iso3c',destination='country.name')
resort_hotel$Country[resort_hotel$Country == 'CN'] <- 'Pitcairn'
resort_hotel <- resort_hotel %>% drop_na(Country)
#View(data.frame(resort_hotel$Country))

#Frequencies of hotels in countries
city_country_table <- table(city_hotel$Country)
#View(data.frame(city_country_table)) 
city_country_freq <- data.frame(city_country_table)
city_country_freq
names(city_country_freq)[names(city_country_freq)=="Var1"] <- "Country"
names(city_country_freq)[names(city_country_freq)=="Freq"] <- "Count"
#View(city_country_freq)
#View(city_hotel)

resort_country_table <- table(resort_hotel$Country)
#View(data.frame(resort_country_table))
resort_country_freq <- data.frame(resort_country_table)
resort_country_freq
names(resort_country_freq)[names(resort_country_freq)=="Var1"] <- "Country"
names(resort_country_freq)[names(resort_country_freq)=="Freq"] <- "Count"
#View(resort_country_freq)
#View(resort_hotel)

#--------Data Visualizations ---------

#Generating a wordcloud to show which countries people came frequently from
wordcloud(words = city_country_freq$Country, freq = city_country_freq$Count, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) #Portugal is the most occuring country of residence
wordcloud(words = resort_country_freq$Country, freq = resort_country_freq$Count, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2")) #Portugal is the most occuring country of residence followed by the United Kingdom
#View(city_hotel)
#View(resort_hotel)

#Generating a map to show which countries people came frequently from
world_map_city <- map_data("world")
world_map_city <- merge(world_map_city,city_country_freq, by.x ="region", by.y="Country")
#str(world_map_city)
world_map_city <- world_map_city[order(world_map_city$group,world_map_city$order), ]
ggplot(world_map_city, aes(x=long, y=lat, group = group)) + geom_polygon(aes(fill = Count),color ="black") + coord_map(projection = "mercator") +scale_fill_gradient(high = "red", low ="blue")+ scale_fill_continuous(low = "#9A7787", high = "RED",breaks =c(0,5000,10000,15000,20000,25000,30500)) + ggtitle("World Map for country frequencies for city_hotel")

world_map_resort <- map_data("world")
world_map_resort <- merge(world_map_resort,resort_country_freq, by.x ="region", by.y="Country")
#str(world_map_resort)
world_map_resort <- world_map_resort[order(world_map_resort$group,world_map_resort$order), ]
ggplot(world_map_resort, aes(x=long, y=lat, group = group)) + geom_polygon(aes(fill = Count),color ="black") + coord_map(projection = "mercator") +scale_fill_gradient(high = "red", low ="blue")+ scale_fill_continuous(low = "#9A7787", high = "RED",breaks =c(0,5000,10000,15000,20000,25000,30500)) + ggtitle("World Map for country frequencies for resort_hotel")

#Generating a table of months to get their total counts and converting it into a dataframe for further analysis
tab1 <- format(city_hotel$newArrival, "%m") # Getting month of the arrival date
tab1 <- data.frame(table(tab1))
names(tab1)[names(tab1)=="tab1"] <- "Month" #Renaming the Column name in the table
#View(tab1)

tab2 <- format(resort_hotel$newArrival, "%m") # Getting month of the arrival date
tab2 <- data.frame(table(tab2))
names(tab2)[names(tab2)=="tab2"] <- "Month1" #Renaming the Column name in the table
#View(tab2)

#Creating a plot for the month and number of customers
plot_month_city <- ggplot(data= tab1, aes(x = Month,y=Freq, group =1))+ ylab ('Number of Customers') + ggtitle(" Customers by Month for city_hotel") + geom_line()+ geom_point(shape = 21, color="black", fill="#69b3a2", size=2 ) + scale_x_discrete(labels=month.abb) + scale_y_continuous(breaks = seq(5000,8000, by = 500))
plot_month_city <- plot_month_city +  theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
plot_month_city

plot_month_resort <- ggplot(data= tab2, aes(x = Month1,y=Freq, group =1))+ ylab ('Number of Customers') + ggtitle(" Customers by Month for resort_hotel") + geom_line()+ geom_point(shape = 21, color="black", fill="#69b3a2", size=2 ) + scale_x_discrete(labels=month.abb) 
plot_month_resort <- plot_month_resort +  theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
plot_month_resort

#Plot for Seasons based on number of customers.
tab_season_city <- data.frame(table(city_hotel$Season)) #Generating a table of seasons with total number of customers
names(tab_season_city)[names(tab_season_city)=="Var1"] <- "Season"  # renaming the tabular column
season_plot_city <- ggplot(data= tab_season_city,aes(x=Season,y=Freq, color = Season, group =1)) +  geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
season_plot_city <- season_plot_city + ggtitle("Trend by Season for city_hotel") + ylab(" Number of Customers") 
season_plot_city <- season_plot_city + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
season_plot_city

tab_season_resort <- data.frame(table(resort_hotel$Season)) #Generating a table of seasons with total number of customers
names(tab_season_resort)[names(tab_season_resort)=="Var1"] <- "Season"  # renaming the tabular column
season_plot_resort <- ggplot(data= tab_season_resort,aes(x=Season,y=Freq, color = Season, group =1)) +  geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
season_plot_resort <- season_plot_resort + ggtitle("Trend by Season for resort_hotel") + ylab(" Number of Customers") 
season_plot_resort <- season_plot_resort + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
season_plot_resort

#Checking seasonality for only checkout dates
sample_hotel_city <- data.frame(city_hotel$newArrival,city_hotel$ReservationStatus,city_hotel$Season)
#View(sample_hotel_city)
sample_hotel_city<- sample_hotel_city[(sample_hotel_city$city_hotel.ReservationStatus != 'Canceled' 
                                       & sample_hotel_city$city_hotel.ReservationStatus != 'No-Show'), ]
#View(sample_hotel_city) # All check-outs
sample_hotel_resort <- data.frame(resort_hotel$newArrival,resort_hotel$ReservationStatus,resort_hotel$Season)
#View(sample_hotel_resort)
sample_hotel_resort<- sample_hotel_resort[(sample_hotel_resort$resort_hotel.ReservationStatus != 'Canceled' 
                                           & sample_hotel_resort$resort_hotel.ReservationStatus != 'No-Show'), ]
#View(sample_hotel_resort) # All check-outs

#Extracting arrival months only for checked-out customers
tab8_city <- format(sample_hotel_city$city_hotel.newArrival, "%m") # Getting month of the arrival date
tab8_city <- data.frame(table(tab8_city)) # generating a table of months to get their total counts and converting it into a dataframe for further analysis
names(tab8_city)[names(tab8_city)== "tab8_city"] <- "Month" #Renaming the Column name in the table
#View(tab8_city)
tab8_resort <- format(sample_hotel_resort$resort_hotel.newArrival, "%m") # Getting month of the arrival date
tab8_resort <- data.frame(table(tab8_resort)) # generating a table of months to get their total counts and converting it into a dataframe for further analysis
names(tab8_resort)[names(tab8_resort)== "tab8_resort"] <- "Month" #Renaming the Column name in the table
#View(tab8_resort)

#Creating a plot for the month and number of customers that checked out.
p_city <- ggplot(data = tab8_city, aes( x= Month, y= Freq, group = 1)) + ggtitle("Customers who checked out in city_hotel")+geom_line(data = tab8_city, aes( x= Month, y= Freq, group =1))+geom_point(shape = 21, size = 2,color = "black", fill = "#BCD979",data = tab8_city, aes( x= Month, y= Freq, group =1))+scale_x_discrete(labels=month.abb) + scale_y_continuous(breaks = seq(1000,6000, by = 500))
p_city
p_resort <- ggplot(data = tab8_resort, aes( x= Month, y= Freq, group = 1)) + ggtitle("Customers who checked out in resort_hotel")+geom_line(data = tab8_resort, aes( x= Month, y= Freq, group =1))+geom_point(shape = 21, size = 2,color = "black", fill = "#BCD979",data = tab8_resort, aes( x= Month, y= Freq, group =1))+  scale_x_discrete(labels=month.abb) + scale_y_continuous(breaks = seq(1000,6000, by = 500))
p_resort

#Plotting according to seasons for check-out only
checkout_season_city <- data.frame(table(sample_hotel_city$city_hotel.Season)) #Generating a table of seasons with total number of customers
names(checkout_season_city)[names(checkout_season_city)=="Var1"] <- "Season"  # renaming the tabular column
checkout_season_plot_city <- ggplot(data= checkout_season_city,aes(x=Season,y=Freq, color = Season, group =1))+geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
checkout_season_plot_city <- checkout_season_plot_city + ggtitle("Trend by Season for Check-out only in city_hotel") + ylab(" Number of Customers") 
checkout_season_plot_city <- checkout_season_plot_city + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
checkout_season_plot_city

checkout_season_resort <- data.frame(table(sample_hotel_resort$resort_hotel.Season)) #Generating a table of seasons with total number of customers
names(checkout_season_resort)[names(checkout_season_resort)=="Var1"] <- "Season"  # renaming the tabular column
checkout_season_plot_resort <- ggplot(data= checkout_season_resort,aes(x=Season,y=Freq, color = Season, group =1)) +  geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
checkout_season_plot_resort <- checkout_season_plot_resort + ggtitle("Trend by Season for Check-out only in resort_hotel") + ylab(" Number of Customers") 
checkout_season_plot_resort <- checkout_season_plot_resort + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
checkout_season_plot_resort

#6 - LeadTime plots
#For city_hotel
#LeadTime for Check-Out
Lead_CheckIn_city <- city_hotel[(city_hotel$ReservationStatus!= 'Canceled' & city_hotel$ReservationStatus!= 'No-Show'), ]
Lead_CheckIn_city_plot <- ggplot(data = Lead_CheckIn_city,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and Arrival for city_hotel")+geom_bar(fill = "#87a7b3") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,1000),breaks = seq(0,1000, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckIn_city_plot

#LeadTime for only Cancelled and No-Show data
Lead_CheckOut_city <- city_hotel[(city_hotel$ReservationStatus!= 'Check-Out'), ]
Lead_CheckOut_city_plot <- ggplot(data = Lead_CheckOut_city,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and cancellations for city_hotel")+geom_bar(fill = "#ffb037") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,1000),breaks = seq(0,1000, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckOut_city_plot

#LeadTime for Check-Out and No Deposit
Lead_CheckIn_Deposit_city <- city_hotel[(city_hotel$ReservationStatus!= 'Check-Out' & city_hotel$DepositType == 'No Deposit'), ]
Lead_CheckIn_Deposit_city_plot <- ggplot(data = Lead_CheckIn_Deposit_city,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and cancellations without deposits for city_hotel")+geom_bar(fill = "#ffb037") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckIn_Deposit_city_plot

#For resort_hotel
#LeadTime for Cancelled and No-Show data
Lead_CheckIn_resort <- resort_hotel[(resort_hotel$ReservationStatus!= 'Canceled' & resort_hotel$ReservationStatus!= 'No-Show'), ]
Lead_CheckIn_resort_plot <- ggplot(data = Lead_CheckIn_resort,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and Arrival for resort_hotel")+geom_bar(fill = "#87a7b3") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,1000),breaks = seq(0,1000, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckIn_resort_plot

#LeadTime for only Check-Out data
Lead_CheckOut_resort <- resort_hotel[(resort_hotel$ReservationStatus!= 'Check-Out'), ]
Lead_CheckOut_resort_plot <- ggplot(data = Lead_CheckOut_resort,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and cancellations for resort_hotel")+geom_bar(fill = "#ffb037") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,1000),breaks = seq(0,1000, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckOut_resort_plot

#LeadTime for Check-Out and No Deposit
Lead_CheckIn_Deposit_resort <- resort_hotel[(resort_hotel$ReservationStatus!= 'Check-Out' & resort_hotel$DepositType == 'No Deposit'), ]
Lead_CheckIn_Deposit_resort_plot <- ggplot(data = Lead_CheckIn_Deposit_resort,aes(x =LeadTime)) + ggtitle("Number of Days Between Booking Date and cancellations without deposits for resort_hotel")+geom_bar(fill = "#ffb037") + geom_vline(aes(xintercept = mean(LeadTime)),color = "red") + scale_x_continuous(limits = c(0,500),breaks = seq(0,500, by = 25)) +
  scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by=100)) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"))
Lead_CheckIn_Deposit_resort_plot

#7
# How many times did people get a different room assigned
room_plot_city <- ggplot(data = city_hotel,aes(x= ReservedRoomType, y = AssignedRoomType)) + geom_jitter(shape = 21,size = 1, color = "#FF6701") + ggtitle("Room Type : Reserved vs Assigned for city_hotel") + xlab("Reserved Room") + ylab("Assigned Room") + 
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
room_plot_city

room_plot_resort <- ggplot(data = resort_hotel,aes(x= ReservedRoomType, y = AssignedRoomType)) + geom_jitter(shape = 21,size = 1, color = "#FF6701") + ggtitle("Room Type : Reserved vs Assigned for resort_hotel") + xlab("Reserved Room") + ylab("Assigned Room") + 
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
room_plot_resort

#8 - Histograms
hist_hotel <- ggplot(data= city_hotel, aes(x=DaysInWaitingList)) + geom_histogram(fill = "#7393B3" ) +ggtitle("Days in Waiting List for city_hotel")
hist_hotel
hist_resort <- ggplot(data = resort_hotel,aes(x=DaysInWaitingList)) + geom_histogram(fill = "#B99095") +ggtitle("Days in Waiting List for resort_hotel")
hist_resort

#8 - BoxPlots
#Generating ADR box plots by customer type
boxplot_adr_customertype_city <- ggplot(data = city_hotel, aes(x = CustomerType, y= ADR, fill = CustomerType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="RdBu") + 
  ggtitle("Average Daily Rate by Customer Type for city_hotel") + scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                            axis.title.y = element_text( size=9))
boxplot_adr_customertype_city
boxplot_adr_customertype_resort <- ggplot(data = resort_hotel, aes(x = CustomerType, y= ADR, fill = CustomerType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="RdBu") + 
  ggtitle("Average Daily Rate by Customer Type for resort_hotel") + scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                              axis.title.y = element_text( size=9))
boxplot_adr_customertype_resort

#Generating ADR box plot by Room Type
boxplot_adr_roomtype_city <- ggplot(data = city_hotel, aes(x = ReservedRoomType, y= ADR, fill = ReservedRoomType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="Spectral") + 
  ggtitle("Average Daily Rate by Room Type for city_hotel")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                      axis.title.y = element_text( size=9))
boxplot_adr_roomtype_city
boxplot_adr_roomtype_resort <- ggplot(data = resort_hotel, aes(x = ReservedRoomType, y= ADR, fill = ReservedRoomType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="Spectral") + 
  ggtitle("Average Daily Rate by Room Type for resort_hotel")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                        axis.title.y = element_text( size=9))
boxplot_adr_roomtype_resort

#Generating ADR box plot by meal plan
boxplot_adr_mealtype_city <- ggplot(data = city_hotel, aes(x = Meal, y= ADR, fill = Meal)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="PuOr",labels=c("Bed & Breakfast", "Full Board", "Half Board", "Unidentified")) + 
  ggtitle("Average Daily Rate by Meal Type for city_hotel")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                      axis.title.y = element_text( size=9)) 
boxplot_adr_mealtype_city
boxplot_adr_mealtype_resort <- ggplot(data = resort_hotel, aes(x = Meal, y= ADR, fill = Meal)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="PuOr",labels=c("Bed & Breakfast", "Full Board", "Half Board", "Unidentified")) + 
  ggtitle("Average Daily Rate by Meal Type for resort_hotel")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                        axis.title.y = element_text( size=9)) 
boxplot_adr_mealtype_resort

#REVENUE ANALYSIS
#For city_hotel
#Dataframe for city_hotel to get revenue by season
sample_revenue_city <- data.frame(city_hotel$newArrival,city_hotel$ReservationStatus,city_hotel$Avg_Revenue_per_Stay,city_hotel$Season)
#View(sample_revenue_hotel)

#Filtering data only for people who checked in
sample_revenue_checkin <- sample_revenue_city[(sample_revenue_city$city_hotel.ReservationStatus != 'Canceled' & sample_revenue_city$city_hotel.ReservationStatus != 'No-Show'), ]
#View(sample_revenue_checkin) # All check-outs


#Creating a function to get sum of the column by condition.
sum_season <- function(j,m){
  j <- j[j$city_hotel.Season == m,]
  return(sum(j$city_hotel.Avg_Revenue_per_Stay))
}

#Creating a dataframe for average revenue by season. It has sum of all average revenues for those who checked in.
revenue_by_season <- data.frame(Season= c('Fall','Spring','Summer','Winter'))
revenue_by_season$Season <- as.factor(revenue_by_season$Season)
revenue_by_season <- mutate(revenue_by_season, Avg = case_when(revenue_by_season$Season == 'Fall' ~ sum_season(sample_revenue_checkin,'Fall'),
                                                               revenue_by_season$Season == 'Spring' ~ sum_season(sample_revenue_checkin,'Spring'),
                                                               revenue_by_season$Season == 'Summer' ~ sum_season(sample_revenue_checkin,'Summer'),
                                                               revenue_by_season$Season == 'Winter' ~ sum_season(sample_revenue_checkin,'Winter')
                                                               
))
#str(revenue_by_season)
#View(revenue_by_season)

#Creating a plot for average revenue of city_hotel by season for check-out data
plot_rev <- ggplot(data = revenue_by_season, aes( x= Season, y= Avg, group = 1)) + 
  ggtitle("Average Revenue for Check-Out in city_hotel by season")+
  geom_line(data = revenue_by_season, aes( x= Season, y= Avg, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#BCD979",data = revenue_by_season, aes( x= Season, y= Avg, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))

plot_rev

#Filtering data only for people who canceled
sample_revenue_cancel <- sample_revenue_city
#View(sample_revenue_cancel) # All check-outs

#Creating a function to get sum of the column by condition.
sum_season <- function(j,m){
  j <- j[j$city_hotel.Season == m,]
  return(sum(j$city_hotel.Avg_Revenue_per_Stay))
}

#Creating a dataframe for average revenue by season. It has  sum of all average revenues for those who checked in.
revenue_by_season_cancel <- data.frame(Season_cancel= c('Fall','Spring','Summer','Winter'))
revenue_by_season_cancel$Season_cancel <- as.factor(revenue_by_season_cancel$Season_cancel)
revenue_by_season_cancel <- mutate(revenue_by_season_cancel,Avg_cancel = case_when(revenue_by_season_cancel$Season == 'Fall' ~ sum_season(sample_revenue_cancel,'Fall'),
                                                                                   revenue_by_season_cancel$Season == 'Spring' ~ sum_season(sample_revenue_cancel,'Spring'),
                                                                                   revenue_by_season_cancel$Season == 'Summer' ~ sum_season(sample_revenue_cancel,'Summer'),
                                                                                   revenue_by_season_cancel$Season == 'Winter' ~ sum_season(sample_revenue_cancel,'Winter')
                                                                                   
))
str(revenue_by_season_cancel)
#View(revenue_by_season_cancel)

#Creating a plot for average revenue of hotels by season for checkin data
plot_rev_cancel <- ggplot(data = revenue_by_season_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1)) + 
  ggtitle("If there were no cancellations in city_hotel")+
  geom_line(data = revenue_by_season_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#ec4646",data = revenue_by_season_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))

plot_rev_cancel

combined_df_city <- data.frame(revenue_by_season,revenue_by_season_cancel)
#View(combined_df_city)
combined_plot_revenue_city <- ggplot(data = combined_df_city, aes ( x = Season, y = Avg)) + ggtitle("What could have been and What is for city_hotel")+
  geom_line(data = combined_df_city, aes( x= Season, y= Avg, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#BCD979",data = combined_df_city, aes( x= Season, y= Avg, group = 1)) +
  geom_line(data = revenue_by_season_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#ec4646",data = combined_df_city, aes( x= Season, y= Avg_cancel, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
combined_plot_revenue_city


#For resort_hotel
#Dataframe for hotels to get revenue by season
sample_revenue_resort <- data.frame(resort_hotel$newArrival,resort_hotel$ReservationStatus,resort_hotel$Avg_Revenue_per_Stay,resort_hotel$Season)
#View(sample_revenue_hotel)

#Filtering data only for people who checked in
sample_revenue_checkin_resort <- sample_revenue_resort[(sample_revenue_resort$resort_hotel.ReservationStatus != 'Canceled' & sample_revenue_resort$resort_hotel.ReservationStatus != 'No-Show'), ]
#View(sample_revenue_checkin) # All check-outs


#creating a function to get sum of the column by condition.
sum_season_resort <- function(j,m){
  j <- j[j$resort_hotel.Season == m,]
  return(sum(j$resort_hotel.Avg_Revenue_per_Stay))
}

#Creating a dataframe for average revenue by season. It has sum of all average revenues for those who checked in.
revenue_by_season_resort <- data.frame(Season= c('Fall','Spring','Summer','Winter'))
revenue_by_season_resort$Season <- as.factor(revenue_by_season_resort$Season)
revenue_by_season_resort <- mutate(revenue_by_season_resort,Avg = case_when(revenue_by_season_resort$Season == 'Fall' ~ sum_season_resort(sample_revenue_checkin_resort,'Fall'),
                                                                            revenue_by_season_resort$Season == 'Spring' ~ sum_season_resort(sample_revenue_checkin_resort,'Spring'),
                                                                            revenue_by_season_resort$Season == 'Summer' ~ sum_season_resort(sample_revenue_checkin_resort,'Summer'),
                                                                            revenue_by_season_resort$Season == 'Winter' ~ sum_season_resort(sample_revenue_checkin_resort,'Winter')
                                                                            
))
#str(revenue_by_season_resort)
#View(revenue_by_season_resort)

#creating a plot for average revenue of hotels by season for checkin data
plot_rev_resort <- ggplot(data = revenue_by_season_resort, aes( x= Season, y= Avg, group = 1)) + 
  ggtitle("Average Revenue for Check-Out in Resorts by season")+
  geom_line(data = revenue_by_season_resort, aes( x= Season, y= Avg, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "blue",data = revenue_by_season_resort, aes( x= Season, y= Avg, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))

plot_rev_resort

#Filtering data only for people who canceled
sample_revenue_cancel_resort <- sample_revenue_resort
#View(sample_revenue_cancel_resort) # All check-outs

#creating a function to get sum of the column by condition.
sum_season_resort <- function(j,m){
  j <- j[j$resort_hotel.Season == m,]
  return(sum(j$resort_hotel.Avg_Revenue_per_Stay))
}

#Creating a dataframe for average revenue by season. It has sum of all average revenues for those who checked in.
revenue_by_season_cancel_resort <- data.frame(Season_cancel_resort= c('Fall','Spring','Summer','Winter'))
revenue_by_season_cancel_resort$Season_cancel_resort <- as.factor(revenue_by_season_cancel_resort$Season_cancel)
revenue_by_season_cancel_resort <- mutate(revenue_by_season_cancel_resort,Avg_cancel = case_when(revenue_by_season_cancel_resort$Season_cancel_resort == 'Fall' ~ sum_season_resort(sample_revenue_cancel_resort,'Fall'),
                                                                                                 revenue_by_season_cancel_resort$Season_cancel_resort == 'Spring' ~ sum_season_resort(sample_revenue_cancel_resort,'Spring'),
                                                                                                 revenue_by_season_cancel_resort$Season_cancel_resort == 'Summer' ~ sum_season_resort(sample_revenue_cancel_resort,'Summer'),
                                                                                                 revenue_by_season_cancel_resort$Season_cancel_resort == 'Winter' ~ sum_season_resort(sample_revenue_cancel_resort,'Winter')
                                                                                                 
))
#str(revenue_by_season_cancel_resort)
#View(revenue_by_season_cancel_resort)

#creating a plot for average revenue of hotels by season for checkin data
plot_rev_cancel_resort <- ggplot(data = revenue_by_season_cancel_resort, aes( x= Season_cancel_resort, y= Avg_cancel, group = 1)) + 
  ggtitle("If no cancellations in resort_hotel")+
  geom_line(data = revenue_by_season_cancel_resort, aes( x= Season_cancel_resort, y= Avg_cancel, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "orange",data = revenue_by_season_cancel_resort, aes( x= Season_cancel_resort, y= Avg_cancel, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))

plot_rev_cancel_resort

combined_df_resort <- data.frame(revenue_by_season_resort,revenue_by_season_cancel_resort)
#View(combined_df_resort)
combined_plot_revenue_resort <- ggplot(data = combined_df_resort, aes ( x = Season, y = Avg)) + ggtitle("What could have been and What is for resort_hotel") +
  geom_line(data = combined_df_resort, aes( x= Season, y= Avg, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "blue",data = combined_df_resort, aes( x= Season, y= Avg, group = 1)) +
  geom_line(data = combined_df_resort, aes( x= Season, y= Avg_cancel, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "orange",data = combined_df_resort, aes( x= Season, y= Avg_cancel, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
combined_plot_revenue_resort


#Combining city_hotel and resort_hotel
#Plot for average revenue of hotel and resort by season for checkin
combined_df_checkin <- data.frame(revenue_by_season,revenue_by_season_resort)
#View(combined_df_checkin)
#str(combined_df_checkin)
combined_plot_checkin <- ggplot(data = combined_df_checkin, aes ( x = Season, y = Avg)) + ggtitle("Average Revenue for City Hotel and Resorts for Check-Outs")+
  geom_line(data = combined_df_checkin, aes( x= Season, y= Avg, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#BCD979",data = combined_df_checkin, aes( x= Season, y= Avg, group = 1)) +
  geom_line(data = combined_df_checkin, aes( x= Season, y= Avg.1, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "blue",data = combined_df_checkin, aes( x= Season, y= Avg.1, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
combined_plot_checkin #blue is for resort, green for hotel



#Plot for average revenue of hotel and resort by season for cancellations
combined_df_cancel <- data.frame(revenue_by_season_cancel,revenue_by_season_cancel_resort)
#View(combined_df_cancel)
#str(combined_df_cancel)
combined_plot_cancel <- ggplot(data = combined_df_cancel, aes ( x = Season_cancel, y = Avg_cancel)) + ggtitle("Average revenue for City Hotels and Resorts if no cancellations")+
  geom_line(data = combined_df_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "#ec4646",data = combined_df_cancel, aes( x= Season_cancel, y= Avg_cancel, group = 1)) +
  geom_line(data = combined_df_cancel, aes( x= Season_cancel, y= Avg_cancel.1, group = 1))+
  geom_point(shape = 21, size = 2,color = "black", fill = "orange",data = combined_df_cancel, aes( x= Season_cancel, y= Avg_cancel.1, group = 1)) +
  ylab("Average Revenue (in millions)") +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
combined_plot_cancel #orange is for resort, red for hotel

#--------------- Linear Models -------------------------

#Linear Model for ADR based on Assigned Room type
lm_room_city <- lm(data = city_hotel, ADR ~ AssignedRoomType)
lm_room_city
summary(lm_room_city)
g <- data.frame(AssignedRoomType = 'G')
pred_room_city <- predict(lm_room_city, g) #Predicting the ADR value for the roomtype assigned.
pred_room_city #184.474

lm_room_resort <- lm(data = resort_hotel, ADR ~ AssignedRoomType)
lm_room_resort
summary(lm_room_resort)
c <- data.frame(AssignedRoomType = 'C')
pred_room_resort <- predict(lm_room_resort, c) #Predicting the ADR value for the roomtype assigned.
pred_room_resort #114.3632


#Linear Model for ADR based on Reserved Roomtype, total number of people, meal chosen
lm_people_meal_room_city <- lm(ADR ~ ReservedRoomType + TotalPeople + Meal, data=city_hotel)
lm_people_meal_room_city
summary(lm_people_meal_room_city)
lm_sample <- data.frame(ReservedRoomType = "G", TotalPeople = 5, Meal ="HB")
pred_people_meal_room_city <- predict(lm_people_meal_room_city,lm_sample)
pred_people_meal_room_city #243.4406
#View(city_hotel)
#View(resort_hotel)
lm_people_meal_room_resort <- lm(ADR ~ ReservedRoomType + TotalPeople + Meal, data=resort_hotel)
lm_people_meal_room_resort
summary(lm_people_meal_room_resort)
pred_people_meal_room_resort <- predict(lm_people_meal_room_resort,lm_sample)
pred_people_meal_room_resort #213.5494


#Linear Model for ADR based on Season,Customer Type
lm_season_city <- lm(ADR~Season, data= city_hotel)
lm_season_city
summary(lm_season_city)
se <- data.frame(Season = "Summer")
pred_season_city <- predict(lm_season_city, se)
pred_season_city #Summer : 113.7119

lm_season_resort <- lm(ADR~Season, data= resort_hotel)
lm_season_resort
summary(lm_season_resort)
pred_season_resort <- predict(lm_season_resort, se)
pred_season_resort #Summer : 156.6577

#Linear Model for ADR based on LeadTime, DepositFactor, ReservedRoomType, Meal, Visitor and Season
final_lm_city <- lm(ADR ~ LeadTime + DepositFactor + ReservedRoomType + Meal + VisitorType + Season, data= city_hotel)
summary(final_lm_city)
df_city <- data.frame(LeadTime= 25,Meal= 'HB', DepositFactor= 1 ,ReservedRoomType = 'B', VisitorType='Family', Season= 'Fall')
pred_city <- predict(final_lm_city,df_city)
pred_city #134.2936

final_lm_resort <- lm(ADR ~ LeadTime + DepositFactor + ReservedRoomType + Meal + VisitorType + Season, data= resort_hotel)
summary(final_lm_resort)
df_resort <- data.frame(LeadTime= 25,Meal= 'HB', DepositFactor= 1 ,ReservedRoomType = 'B', VisitorType='Family', Season= 'Fall')
pred_resort <- predict(final_lm_resort,df_resort)
pred_resort #139.436

#Generating apriori rules for who is likely to cancel.
#Converting needed variables into factors
city_hotel$IsCanceled <- as.factor(city_hotel$IsCanceled)
city_hotel$Season <- as.factor(city_hotel$Season)
resort_hotel$IsCanceled <- as.factor(resort_hotel$IsCanceled)
resort_hotel$Season <- as.factor(resort_hotel$Season)



city_hotel$DepositType <- as.factor(city_hotel$DepositType)
city_hotel$Meal <- as.factor(city_hotel$Meal)
city_hotel$CustomerType <- as.factor(city_hotel$CustomerType)
city_hotel$Season <- as.factor(city_hotel$Season)
city_hotel$Country <- as.factor(city_hotel$Country)
city_hotel$ReservedRoomType <- as.factor(city_hotel$ReservedRoomType)

resort_hotel$DepositType <- as.factor(resort_hotel$DepositType)
resort_hotel$Meal <- as.factor(resort_hotel$Meal)
resort_hotel$CustomerType <- as.factor(resort_hotel$CustomerType)
resort_hotel$Season <- as.factor(resort_hotel$Season)
resort_hotel$Country <- as.factor(resort_hotel$Country)
resort_hotel$ReservedRoomType <- as.factor(resort_hotel$ReservedRoomType)



city_sample <- city_hotel[ , c("IsCanceled" , "DepositType" , "Meal" , "CustomerType" , "Season","ReservedRoomType","Country")]
city_sample_transactions <- as(city_sample,"transactions")
itemLabels(city_sample_transactions)
rules_cancel_city <- apriori(city_sample_transactions, parameter = list(support = 0.05,conf = 0.5),
                             control = list(verbose = FALSE),
                             appearance = list(default = "lhs", rhs = ( "IsCanceled=1" )))
inspect(rules_cancel_city)
itemLabels(rules_cancel_city)
itemFrequencyPlot(city_sample_transactions,cex.names = 0.7)
plot(rules_cancel_city,method = "graph",engine = "htmlwidget")

resort_sample <- resort_hotel[ , c("IsCanceled" , "DepositType" , "Meal" , "CustomerType" , "Season","ReservedRoomType","Country")]
resort_sample_transactions <- as(resort_sample,"transactions")
itemLabels(resort_sample_transactions)
rules_cancel_resort <- apriori(resort_sample_transactions, parameter = list(support = 0.005,conf = 0.5),
                               control = list(verbose = FALSE),
                               appearance = list(default = "lhs", rhs = ( "IsCanceled=1" )))
inspect(rules_cancel_resort)
itemLabels(rules_cancel_resort)
itemFrequencyPlot(resort_sample_transactions,cex.names = 0.7)
plot(rules_cancel_resort,method = "graph",engine = "htmlwidget")

#------------------- SVM Algorithm -----------------------

hotel_train_list_city <- createDataPartition(city_hotel$IsCanceled, p = .40,list = FALSE)
hotel_train_set_city <- city_hotel[hotel_train_list_city,]
#View(hotel_train_set_city)
#str(hotel_train_set_city)
hotel_test_set_city <- city_hotel[-hotel_train_list_city,]
svmOut_city <- ksvm(IsCanceled ~ PreviousCancellations + LeadTime +Meal + DaysInWaitingList+ Season +ReservedRoomType,data = hotel_train_set_city,kernel ="rbfdot", kpar="automatic" , C=5, cross= 3,prob.model= T)   
svmOut_city 
svm_pred_city <- predict(svmOut_city, newdata = hotel_test_set_city)
svm_pred_city

confusionMatrix(svm_pred_city,hotel_test_set_city$IsCanceled) #Has an accuracy of 74.8%

hotel_train_list_resort <- createDataPartition(city_hotel$IsCanceled, p = .40,list = FALSE)
hotel_train_set_resort <- city_hotel[hotel_train_list_resort,]
#View(hotel_train_set_resort)
#str(hotel_train_set_resort)
hotel_test_set_resort <- city_hotel[-hotel_train_list_resort,]
svmOut_resort <- ksvm(IsCanceled ~ PreviousCancellations + LeadTime +Meal + DaysInWaitingList+ Season +ReservedRoomType,data = hotel_train_set_resort,kernel ="rbfdot", kpar="automatic" , C=5, cross= 3,prob.model= T)   
svmOut_resort
svm_pred_resort <- predict(svmOut_resort, newdata = hotel_test_set_resort)
svm_pred_resort

confusionMatrix(svm_pred_resort,hotel_test_set_resort$IsCanceled) 


#-------------------------------------------------------------------------------------#

