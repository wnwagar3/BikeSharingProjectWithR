
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

# add missing columns in the washington df and fill with NA values so 
# it matches other two dfs for concatentation purposes
wash['Gender'] <- NA
wash['Birth.Year'] <- NA

# add a city column to each df so when dfs are concatenated we can retain 
# which table the rows are from
ny['City'] <- 'New York City'
wash['City'] <- 'Washington'
chi['City'] <- 'Chicago'

# ensure all columns match other two dfs and in proper order
head(ny)

# ensure all columns match other two dfs and in proper order
head(wash)

# ensure all columns match other two dfs and in proper order
head(chi)

# create function to concatenate dataframes
concat <- function(df1, df2) {
    return(rbind(df1, df2))
}

# concatenate first two dfs
combined.with.city.column <- concat(ny, wash)

# combine newly created df with last table
combined.with.city.column <- concat(combined.with.city.column, chi)

# rename new df with more concise name but still explanatory
combined_df <-combined.with.city.column

# take a look at some random lines from new df to visually inspect
sample(combined_df)

# import dplyr library to manipulate df
# create new df filtered by city and average travel time per city
library(dplyr)
travel_time_per_city = combined_df %>% group_by(City)%>%summarise( AvgTravelTime = mean(Trip.Duration,na.rm=TRUE))
travel_time_per_city

# obtain the mean travel time across all cities
mean(combined_df$Trip.Duration, na.rm=TRUE)

# graph the average travel time for each city against the average travel time 
# regardless of city
# a separate column for the average could be added, but the abline appears
# more visually explanatory

barplot(height=travel_time_per_city$AvgTravelTime, 
        names.arg=travel_time_per_city$City, main='Trip Duration By City', 
                  ylab='Seconds', xlab= 'Cities')
abline(h=mean(combined_df$Trip.Duration, na.rm=TRUE))
text(x=1.5, y=mean(combined_df$Trip.Duration, na.rm=TRUE) + 25, 'Average Trip Duration For All Cities', col='red')

# obtain the trips per station for all starting stations
city_start_averages = combined_df %>%
group_by(City,Start.Station) %>%
summarise(TripsPerStation = n()) 

city_start_averages

# obtain the average trips per starting station by city
start_station_city_averages = city_start_averages %>%
group_by(City) %>%
summarise(Avg_Trips = mean(TripsPerStation))

start_station_city_averages

# assign the chicago start station average to a variable for graphing purposes
chicago_start_avg = subset(start_station_city_averages, City=='Chicago')$Avg_Trips

# ensure variable contains correct value
chicago_start_avg

# obtain the top 5 starting stations by city
all_city_start_averages = combined_df %>%
group_by(City,Start.Station) %>%
summarise(TripsPerStation = n()) %>%
top_n(n=5,wt=TripsPerStation)


all_city_start_averages

# separate each city's top 5 into a variable for each city
chicago_start_top5 = subset(all_city_start_averages, City=='Chicago')
nyc_start_top5 = subset(all_city_start_averages, City=='New York City')
wash_start_top5 = subset(all_city_start_averages, City=='Washington')

# graph top 5 Chicago starting stations against city average
par(mar=c(15,4,4,4))
barplot(height=chicago_start_top5$TripsPerStation, 
        names.arg=chicago_start_top5$Start.Station, main='Top 5 Chicago Starting Stations vs City Start Station Average', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(chicago_start_avg, na.rm=TRUE))
text(x=3, y=mean(chicago_start_avg, na.rm=TRUE) + 5, col ='red','Average Trips Per Starting Station')

# check NYC top 5 variable
nyc_start_top5

# put NYC starting station average in variable for graphing purposes
nyc_start_avg = subset(start_station_city_averages, City=='New York City')$Avg_Trips

# ensure variable contains correct value as seen in the table in the 4th cell of
# this section
nyc_start_avg

# graph NYC top 5 starting stations against city average
par(mar=c(15,4,4,4))
barplot(height=nyc_start_top5$TripsPerStation, 
        names.arg=nyc_start_top5$Start.Station, main='Top 5 NYC Starting Stations vs City Start Station Avg', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(nyc_start_avg, na.rm=TRUE))
text(x=3, y=mean(nyc_start_avg, na.rm=TRUE) + 15, col ='red','Average Trips Per Starting Station')

# put Washington starting station average in variable for graphing purposes
wash_start_avg = subset(start_station_city_averages, City=='Washington')$Avg_Trips

# ensure variable contains correct value as seen in table in the 4th cell of 
# this section
wash_start_avg

# graph Washington top 5 starting stations against city average
par(mar=c(16,4,4,4))
barplot(height=wash_start_top5$TripsPerStation, 
        names.arg=wash_start_top5$Start.Station, main='Top 5 Washington Starting Stations vs City Start Station Avg', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(wash_start_avg, na.rm=TRUE))
text(x=3, y=mean(wash_start_avg, na.rm=TRUE) + 45, col ='red','Average Trips Per Starting Station')

# obtain the trips per station for all starting stations
city_end_averages = combined_df %>%
group_by(City,End.Station) %>%
summarise(TripsPerStation = n()) 

# check df was created successfully
city_end_averages

# obtain the average trips per starting station by city
end_station_city_averages = city_end_averages %>%
group_by(City) %>%
summarise(Avg_Trips = mean(TripsPerStation))

# check new df
end_station_city_averages

# obtain the top 5 end stations by city
all_city_end_averages = combined_df %>%
group_by(City,End.Station) %>%
summarise(TripsPerStation = n()) %>%
top_n(n=5,wt=TripsPerStation)

# check new df
all_city_end_averages

# assign each city a variable with their respective top 5 end stations
chicago_end_top5 = subset(all_city_end_averages, City=='Chicago')
nyc_end_top5 = subset(all_city_end_averages, City=='New York City')
wash_end_top5 = subset(all_city_end_averages, City=='Washington')

# check chicago top 5 df
chicago_end_top5

# obtain Chicago average for end stations
chicago_end_avg = subset(end_station_city_averages, City=='Chicago')$Avg_Trips

# ensure variable is holding correct value
chicago_end_avg

# graph Chicago top 5 end station trips vs city average
par(mar=c(15,4,4,4))
barplot(height=chicago_end_top5$TripsPerStation, 
        names.arg=chicago_end_top5$End.Station, main='Top 5 Chicago End Stations vs City End Station Average', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(chicago_end_avg, na.rm=TRUE))
text(x=3, y=mean(chicago_end_avg, na.rm=TRUE) + 10, col ='red','Average Trips Per End Station')

# check nyc top 5 
nyc_end_top5

# obtain NYC end station average for entire city
nyc_end_avg = subset(end_station_city_averages, City=='New York City')$Avg_Trips

# ensure variable holds correct value
nyc_end_avg

# graph NYC top 5 end station trips vs city average
par(mar=c(15,4,4,4))
barplot(height=nyc_end_top5$TripsPerStation, 
        names.arg=nyc_end_top5$End.Station, main='Top 5 NYC End Stations vs City End Station Average', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(nyc_end_avg, na.rm=TRUE))
text(x=3, y=mean(nyc_end_avg, na.rm=TRUE) + 15, col ='red','Average Trips Per End Station')

# check Washington top 5 ending stations
wash_end_avg = subset(end_station_city_averages, City=='Washington')$Avg_Trips

#ensure variable holds correct value
wash_end_avg

# graph Washington top 5 ending station trips vs city average
par(mar=c(15,4,4,4))
barplot(height=wash_end_top5$TripsPerStation, 
        names.arg=wash_end_top5$End.Station, main='Top 5 Washington End Stations vs City End Station Average', 
                  ylab='Trips', xlab= '', las=2)
mtext('Cities', side=1, line=12)
abline(h=mean(wash_end_avg, na.rm=TRUE))
text(x=3, y=mean(wash_end_avg, na.rm=TRUE) + 15, col ='red','Average Trips Per End Station')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
