require(dplyr)
require(data.table)

### Variables to update to pull in data for relevant year's contest

#Date first forecasts received
start.date <- as.Date(paste(2015, "Nov", 02, sep='-'), format='%Y-%b-%d')
season <- "2015/2016"   #Flu season
start.wk <- 42    #First week of ILINet data used for forecasts
end.wk <- 18+52     #Last week of ILINet data used for forecasts

##############################################################
###Read in ILINet results
week.flu <- as.data.table(read.csv(file="15-16_ILINet_Flu.csv",na=""))

#Add 52 to weeks in new year to keep weeks in order
week.flu[week<40, week := as.integer(week+52)]

#Only keep weeks of interest for weekly forecasts received
week.flu <- week.flu[week.flu$week>=start.wk & week.flu$week<=(end.wk+4),]


##############################################################
### Calculate weekly targets and assign date values to join to submission data

targets <- c("1wk","2wk","3wk","4wk")
week.target <- data.frame(target=character(),
                          location=character(),
                          forecast.date=as.Date(character()),
                          observation=integer(),
                          season=character())

for(this.location in levels(as.factor(week.flu$location))){
  for(this.target in targets){
    wk <- as.numeric(substr(this.target,1,1))
    for(this.week in start.wk:end.wk){
      #Set forecast date
      forecast.date=start.date + (this.week-start.wk)*7
      #Set forecast location
      this.point <- filter(week.flu, location==this.location & 
                             week==this.week+wk) %>%
                    mutate(
                           target=this.target,
                           forecast.date=forecast.date) %>%
                    select(-week,observation=percent)
      week.target <- rbind(week.target,this.point)
    }
  }
}

### Ad hoc forecast date corrections due to Thanksgiving/Christmas/New Years
week.target$forecast.date[week.target$forecast.date == "2015-11-30"] <- "2015-12-01"
week.target$forecast.date[week.target$forecast.date == "2015-12-28"] <- "2015-12-30"
week.target$forecast.date[week.target$forecast.date == "2016-01-04"] <- "2016-01-06"

###Rearrange columns to match existing flu_targets csv
setcolorder(week.target, c("target","location","season","forecast.date","observation"))

###Export to CSV
write.csv(week.target, file="weekly_flu_targets.csv", na="",row.names=F)

