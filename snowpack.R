library(dplyr)
library(readr)
library(tidyr)
library(knitr)
library(scales)
library(data.table)
library(stringr)
library(car)
library(jsonlite)
library(basicTrendline)
setwd("~/Desktop/Projects/snowpack")

#read in Just Colorado
snow_water_Colorado <- read.csv("~/Desktop/Projects/snowpack/Colorado/snowWaterEquiv_Colorado.csv")

#format Colorado and filter out summer months
snow_water_Colorado$Date = format(as.Date(snow_water_Colorado$Date, format = "%m/%d/%y"), "%Y-%m-%d") #convert dates, first date format is how the dates come, second is how I want it. Lit
snow_water_Colorado_filtered = snow_water_Colorado %>% filter(str_detect(Date, pattern = ("-01-|-02-|-03-|-04-|-05-|-11-|-12-")))
#write.csv(snow_water_Colorado_filtered, file = "snowWater_Colorado_Filtered.csv", na="")


#find max value for each month by year
colorado_byMonth = snow_water_Colorado_filtered %>% separate(Date, into = c("year","month","day"), sep = '-') %>% select(-c(day))
colorado_byMonth  = colorado_byMonth %>% gather("mountain", "value", 3:16)
colorado_byMonth = colorado_byMonth %>% group_by(year, month, mountain) %>% summarise(max = max(value))%>% filter(!is.na(max)) 
colorado_byMonth = colorado_byMonth %>% spread(mountain, max)
#rename the months
colorado_byMonth$month = str_replace(colorado_byMonth$month, "01", "January") 
colorado_byMonth$month = str_replace(colorado_byMonth$month, "02", "February") 
colorado_byMonth$month = str_replace(colorado_byMonth$month, "03","March")
colorado_byMonth$month = str_replace(colorado_byMonth$month, "04","April") 
colorado_byMonth$month = str_replace(colorado_byMonth$month, "05","May")
colorado_byMonth$month = str_replace(colorado_byMonth$month, "11" , "November")
colorado_byMonth$month = str_replace(colorado_byMonth$month, "12","December")
#write.csv(colorado_byMonth, file= "colorado_byMonth.csv")

#find max value for each month by mountain (for all years combined)
colorado_byMonth_allYears = colorado_byMonth %>% gather("mountain", "value", 3:16)

colorado_byMonth_allYears = colorado_byMonth_allYears %>% filter(!is.na(value)) %>% group_by(month, mountain) %>% summarise(mean = mean(value))
#write.csv(colorado_byMonth_allYears, file= "colorado_byMonth_allYears.csv")


#snowbird Monthly data wrangling
snowbird_monthly <- read_csv("~/Desktop/Projects/snowpack/Elsewhere/snowbird_monthly.csv")
colnames(snowbird_monthly)= c("November", "December", "January", "February", "March", "April", "May")
snowbird_monthly = snowbird_monthly %>% gather("month", "mean", 1:7)
snowbird_monthly$mountain = "Snowbird"
snowbird_monthly = snowbird_monthly[c(1,3,2)]







#write.csv(snowbird_monthly, file = "snowbird_monthly_cleaned.csv")

#revelstoke data wrangling
revelstoke =  read.csv("~/Desktop/Projects/snowpack/Canada/revelstoke.csv")
revelstoke$Timestamp = format(as.Date(revelstoke$Timestamp, format = "%m/%d/%y %H:%M"), "%Y-%m-%d") 
revelstoke$mountain = "Revelstoke"
colnames(revelstoke) = c("year", "max", "mountain")
revelstoke = revelstoke[c(1,3,2)]


#prepare for annual max chart
revelstoke_annualMax = revelstoke %>% separate(year, into = c("year","month","day"), sep = '-') %>% select(-c(day, month)) %>% filter(max != "NA")
revelstoke_annualMax$year = as.integer(revelstoke_annualMax$year)
revelstoke_annualMax = revelstoke_annualMax %>% group_by(year, mountain) %>% summarise(max = max(max))
revelstokeTrendline = trendline(revelstoke_annualMax$year, revelstoke_annualMax$max, summary = TRUE, model="line2P", plot = TRUE,ePos = c("center"))



#find mean monthly over all years
revelstoke_monthly_mean = revelstoke %>% separate(year, into = c("year","month","day"), sep = '-') %>% select(-c(day)) %>%filter(month %in% c("01","02","03","04","05","11","12"))
revelstoke_monthly_mean = revelstoke_monthly_mean %>% group_by(month) %>% summarise(mean = mean(max, na.rm=TRUE)) 
revelstoke_monthly_mean$mountain = "Revelstoke"
  
  #rename the months
  revelstoke_monthly_mean = revelstoke_monthly_mean %>% spread(month, mean)
  colnames(revelstoke_monthly_mean)= c("mountain", "January", "February", "March", "April", "May","November", "December")
  revelstoke_monthly_mean = revelstoke_monthly_mean %>% gather("month", "mean", 2:8)
  revelstoke_monthly_mean = revelstoke_monthly_mean[c(2, 1, 3)]

#prepare for layered radar chart
  revelstoke_monthlyMax = revelstoke %>% separate(year, into = c("year","month","day"), sep = '-') %>% select(-c(day, mountain)) %>%filter(month %in% c("01","02","03","04","05","11","12")) %>% filter(max != "NA")
  revelstoke_monthlyMax = revelstoke_monthlyMax %>% group_by(year, month) %>% summarise(max = max(max))
  #write.csv(revelstoke_monthlyMax, file="revelstoke_monthly_max.csv")
  
  #convert to JSON for layered barchart
  revelstoke_monthlyMax_json = revelstoke_monthlyMax
  revelstoke_monthlyMax_json$month = as.numeric(revelstoke_monthlyMax_json$month)
  revelstoke_monthlyMax_json$year = as.numeric(revelstoke_monthlyMax_json$year)
  revelstoke_monthlyMax_json = revelstoke_monthlyMax %>% spread(month, max)
  revelstoke_monthlyMax_json$year = as.numeric(revelstoke_monthlyMax_json$year)
  #write.csv(revelstoke_monthlyMax_json, file="revelstoke_monthlyMax_json.csv")
  
  revelstoke_monthlyMax_json = toJSON(revelstoke_monthlyMax, dataframe = "rows")
  write_json(revelstoke_monthlyMax_json, "revelstoke_monthlyMax.json")
  

#whistler data wranglind
  whistler =  read.csv("~/Desktop/Projects/snowpack/Canada/whistler.csv")
  whistler$year = format(as.Date(whistler$year, format = "%m/%d/%y %H:%M"), "%Y-%m-%d") 
  whistler = whistler %>% separate(year, into = c("year","month","day"), sep = '-') %>% select(-c(day, mountain)) %>% filter(!month %in% c("06","07","08","09","10"))
  whistler$year = as.integer(whistler$year)
  whistler = whistler  %>% group_by(year,month) %>% summarise(max = max(max, na.rm=TRUE))
  whistler_annualMax = whistler %>% group_by(year) %>% summarise(max = max(max)) 
  whistler_annualMax$mountain = "Whistler"
  whistler_annualMax = whistler_annualMax[c(1,3,2)]
  
  whistlerTrenline = trendline(whistler_annualMax$year, whistler_annualMax$max, summary = TRUE, model="line2P", plot = TRUE)
  
  #rename the months
  whistlerMonth = whistler$month
  whistlerMonth = recode(whistlerMonth, "'01'='January'; '02' = 'February'; '03' = 'March';'04'='April';'05'='May';'11'='November';'12'='December'")
  whistler$month=whistlerMonth 
  
  #prepare for monthly chart

  whistler_monthly = whistler %>% filter(max != "-Inf") %>% group_by(month) %>% summarise(mean = mean(max))
  whistler_monthly$mountain = "Whistler"
  whistler_monthly = whistler_monthly[c(1,3,2)]
  
  
#jackson data wrangling for monthly chart
  jackson_monthly <- read_csv("~/Desktop/Projects/snowpack/Elsewhere/jackson_monthly.csv")
  jackson_monthly = jackson_monthly %>% gather ("month", "mean", 1:7)
  jackson_monthly$mountain= "Jackson"
  jackson_monthly = jackson_monthly[c(1,3, 2)]
  
  
#squaw data wrangling for monthly chart
  squaw_monthly <- read_csv("~/Desktop/Projects/snowpack/Elsewhere/squaw_monthly.csv")
  squaw_monthly = squaw_monthly %>% gather ("month", "mean", 1:7)
  squaw_monthly$mountain= "squaw"
  squaw_monthly = squaw_monthly[c(1,3, 2)]
  
#mtBaker data wrangling for monthly chart
  mtBaker_monthly <- read_csv("~/Desktop/Projects/snowpack/Elsewhere/mtBaker_monthly.csv")
  mtBaker_monthly = mtBaker_monthly %>% gather ("month", "mean", 1:7)
  mtBaker_monthly$mountain= "mtBaker"
  mtBaker_monthly = mtBaker_monthly[c(1,3, 2)]
  

  #bigSky data wrangling for monthly chart
  bigSky_monthly <- read_csv("~/Desktop/Projects/snowpack/Elsewhere/bigSky_monthly.csv")
  bigSky_monthly = bigSky_monthly %>% gather ("month", "mean", 1:7)
  bigSky_monthly$mountain= "bigSky"
  bigSky_monthly = bigSky_monthly[c(1,3, 2)]  
  

  
  #add in non-Colorado Mountains for line chart


jackson = read.csv("~/Desktop/Projects/snowpack/Elsewhere/jackson.csv")
squaw = read.csv("~/Desktop/Projects/snowpack/Elsewhere/squaw.csv")
snowbird = read.csv("~/Desktop/Projects/snowpack/Elsewhere/snowbird.csv")

bigSky = read.csv("~/Desktop/Projects/snowpack/Elsewhere/bigsky.csv")
bigSkyTrendline = trendline(bigSky$year, bigSky$max, summary = TRUE, model="line2P", plot = TRUE)

mtBaker = read.csv("~/Desktop/Projects/snowpack/Elsewhere/mtBaker.csv")
mtBakerTrendline = trendline(mtBaker$year, mtBaker$max, summary = TRUE, model="line2P", plot = TRUE)

#for annual line graph
snow_water_year_summarised = snow_water_Colorado %>% gather("mountain", "value", 2:15) %>% separate(Date, into = c("year","month","day"), sep = '-') %>% select(-c(day, month)) 
snow_water_year_summarised = snow_water_year_summarised %>% group_by(year, mountain) %>% summarise(max = max(value)) %>% filter(max != "NA")
snow_water_year_summarised$year = as.integer(snow_water_year_summarised$year)


allmountains = snow_water_year_summarised %>%union(jackson) %>% union(squaw) %>% union(snowbird) %>% union(revelstoke_annualMax) %>% union(bigSky) %>% union(mtBaker) %>% union(whistler_annualMax)
allmountains = allmountains %>% filter(max != "NA")
colnames(allmountains) = c("date", "mountain", "value")

write.csv(allmountains, file="allMountains_annualMax.csv")






#for monthly charts
allMountains_byMonth_allYears = colorado_byMonth_allYears %>% union(snowbird_monthly) %>% union(revelstoke_monthly_mean) %>% union(whistler_monthly) %>% union(jackson_monthly) %>% union(squaw_monthly) %>% union(mtBaker_monthly)
#write.csv(allMountains_byMonth_allYears, file="allMountains_monthly_allYears.csv")


