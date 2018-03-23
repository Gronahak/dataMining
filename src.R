pairs(Smoking_Attribuable_Mortality)
BRFSS_Prevalence_And_Trends_Data__Tobacco_Use_._Adults_Who_Are_Current_Smokers_for_1995.2010


# Cleaning TaxTable :

Financial_data = The_Tax_Burden_on_Tobacco_Volume_51__1970.2016
SaM = Smoking_Attribuable_Mortality
Raw_census_data_from_1995_to_2010 = BRFSS_Prevalence_And_Trends_Data__Tobacco_Use_._Adults_Who_Are_Current_Smokers_for_1995.2010

str(Financial_data)
str(SaM)
print(unique(SaM$Gender))
print(unique(SaM$LocationDesc))

str(Raw_census_data_from_1995_to_2010)
print(unique(Raw_census_data_from_1995_to_2010$State))
# We want to get rid of useless colums : "Datasource", "TopicDesc", "Source", "MeasureDesc", TopicTypeId",
# , "Source", TopicId", "MeasureId", "SubMeasureID", "SubMeasureIdDisplayOrder".

Financial_data[0,]

# get the structure :
str(Financial_data)

# $ LocationAbbr            : Factor w/ 51 levels "AK","AL","AR",..: 2 1 4 3 5 6 7 9 8 10 ...
# $ LocationDesc            : Factor w/ 51 levels "Alabama","Alaska",..: 1 2 3 4 5 6 7 8 9 10 ...
# $ Year                    : int  1970 1970 1970 1970 1970 1970 1970 1970 1970 1970 ...
# $ Datasource              : Factor w/ 1 level "OW": 1 1 1 1 1 1 1 1 1 1 ...
# $ TopicDesc               : Factor w/ 1 level "The Tax Burden on Tobacco": 1 1 1 1 1 1 1 1 1 1 ...
# $ MeasureDesc             : Factor w/ 1 level "Cigarette Sales": 1 1 1 1 1 1 1 1 1 1 ...
# $ SubMeasureDesc          : Factor w/ 6 levels "Average Cost per pack ",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Data_Value              : num  0.427 0.418 0.385 0.388 0.397 0.311 0.455 0.413 0.326 0.438 ...
# $ Data_Value_Unit         : Factor w/ 3 levels "","$","%": 2 2 2 2 2 2 2 2 2 2 ...
# $ Data_Value_Type         : Factor w/ 3 levels "Dollars","Pack",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ GeoLocation             : Factor w/ 51 levels "(21.304850435000446, -157.85774940299973)",..: 7 51 11 10 16 21 32 23 22 2 ...
# $ Source                  : Factor w/ 4 levels "Table 11- Orzechowski and Walker, Tax Burden on Tobacco",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ TopicTypeId             : Factor w/ 1 level "POL": 1 1 1 1 1 1 1 1 1 1 ...
# $ TopicId                 : Factor w/ 1 level "450POL": 1 1 1 1 1 1 1 1 1 1 ...
# $ MeasureId               : Factor w/ 1 level "450CGS": 1 1 1 1 1 1 1 1 1 1 ...
# $ SubMeasureID            : Factor w/ 6 levels "451CGS","452CGS",..: 5 5 5 5 5 5 5 5 5 5 ...
# $ SubMeasureIdDisplayOrder: int  1 1 1 1 1 1 1 1 1 1 ...

# We immediately notice that some columns are useless, either because the value is the same for all the data
# or because they do not carry significant information. We therefore get rid of them :

length(Financial_data[1,])
columsToRemove = c("Datasource", "TopicDesc", "Source", "MeasureDesc", "TopicTypeId", "Source", "TopicId", "MeasureId", "SubMeasureID", "SubMeasureIdDisplayOrder")
Financial_data = Financial_data[ , !(names(Financial_data) %in% columsToRemove)]

# 51 States ???! District of columbia

Financial_data$SubMeasureDesc

unique(Financial_data$SubMeasureDesc)
min(Financial_data$Year)
2016-1970
# This command tells us the different data we are able to extract
# [1] Average Cost per pack                                 
# [2] Cigarette Consumption (Pack Sales Per Capita)         
# [3] Federal and State tax as a Percentage of Retail Price 
# [4] Federal and State Tax per pack                        
# [5] State Tax per pack                                    
# [6] Gross Cigarette Tax Revenue 


# Using regExp to clean up colum Location :
location = Raw_census_data_from_1995_to_2010$Location.1
location
matches=regexpr("\\(-?[0-9]+.[0-9]+, -?[0-9]+.[0-9]+\\)",location, perl=TRUE)
location = regmatches(location,matches)


library(ggplot2)
library(fiftystater)
install.packages("fiftystater")
install.packages("mapproj")

createPictures(feature,y,newCol)

rawCensus = Raw_census_data_from_1995_to_2010
createPictures(2010)
createPictures = function(year){
  al = rawCensus
  al = al[al$Year == year,]
  
  al = data.frame(state = tolower(al$State), al)
  
  
  yesVect = 
  yesVectWithPercentage = al$Yes
  yesVectWithPercentage
  matches = regexpr("[0-9]+.?[0-9]+",yesVectWithPercentage, perl=TRUE)
  matches
  yesVectWithoutPercentage = regmatches(yesVectWithPercentage, matches)
  yesVectWithoutPercentage = as.numeric(yesVectWithoutPercentage )
  yesVectWithoutPercentage
  al$Yes = yesVectWithoutPercentage
  #names(al)
  #names(al)[names(al)=="Data_Value"] = newCol
  p <- ggplot(al, aes(map_id = state)) + 
    geom_map(aes(fill = Yes), map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          panel.background = element_blank())
    
    p
}

createPictures = function(subMeasureDesc, year, newCol){
  al = TaxTable[TaxTable$SubMeasureDesc == subMeasureDesc,]
  al = al[al$Year == year,]
  al = data.frame(state = tolower(al$LocationDesc), al)
  #names(al)
  #names(al)[names(al)=="Data_Value"] = newCol
  p <- ggplot(al, aes(map_id = state)) + 
    geom_map(aes(fill = Data_Value), map = fifty_states) + 
    expand_limits(x = fifty_states$long, y = fifty_states$lat) +
    coord_map() +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          panel.background = element_blank())
    
  p
}

newColumn = c("Avg Cost per pack",
               "Pack sales per capita",
               "Fed+State Tax (%)",
              "Fed + State Tax (/pack)",
              "State Tax (/pack)",
              "Gross Cigarette Tax Revenue")

for (y in 1970:2006){
  png(filename=paste("pictures/testScale/year",y,sep=""))
  idx = 2
  feature = features[idx]
  newCol = newColumn[idx]
  print(createPictures(feature,y, newCol))
  dev.off()
}

for (y in 2003:2010){
  png(filename=paste("pictures/testScale/year",y,sep=""))
  print(createPictures(y))
  dev.off()
}

createPictures(feature,y,newCol)

al = TaxTable[TaxTable$Data_Value_Type == "Pack",]
al = al[al$Year == 1984,]
al = data.frame(state = tolower(al$LocationDesc), al)
al
p <- ggplot(al, aes(map_id = state)) + 
  geom_map(aes(fill = Data_Value), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())

png(filename="pictures/consuptionEvolution/y")
p
dev.off()

selectDataFromState = function(state, feature){
  data = Financial_data
  data = data[data$LocationDesc == state,]
  data = data[data$SubMeasureDesc == feature,]
  data
}

states = unique(Financial_data$LocationDesc)
states

features=c("Average Cost per pack ",
           "Cigarette Consumption (Pack Sales Per Capita)",
           "Federal and State tax as a Percentage of Retail Price ",
           "Federal and State Tax per pack ",
           "State Tax per pack ",
           "Gross Cigarette Tax Revenue ")


al

scatter.smooth(x=al$Year, y=al$Yes, main=paste( features[i]," / year",sep=""),xlab = "",ylab= "") 


for (i in 1:5){
  myData = selectDataFromState(states,features[i])
  png(filename=paste("pictures/graphs/",paste( features[i],"PerYear"),sep=""))
  scatter.smooth(x=myData$Year, y=myData$Data_Value, main=paste( features[i]," / year",sep=""),xlab = "",ylab= "") 
  dev.off()
}

