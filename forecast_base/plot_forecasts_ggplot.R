require(stringr)
require(stringi)
require(dplyr)
require(ggplot2)
require(grid)
require(gridExtra)

### Define types of targets for code below
static.targets <- c("onset","pkwk","pkper")
dynamic.targets <- c("1wk","2wk","3wk","4wk")
week.targets <- c("onset","pkwk")
percent.targets <- c("pkper","1wk","2wk","3wk","4wk")


##### Plot individual team's forecast with approximate confidence bands #####
plot.forecast <- function(this.team, forecast.data, file, these.seasons, these.locations=NULL, these.targets=NULL) {
  
  ### Remove season requirements for now - could add in back in later after editing seasons
  #if (these.seasons == "train") seasons <- c("2005/2006", "2006/2007", "2007/2008", "2008/2009")
  #else if (these.seasons == "test") seasons <- c("2009/2010", "2010/2011", "2011/2012", "2012/2013")
  #else stop("Enter \"train\" or \"test\" for these.seasons")
  ###
  
  
  ### If no targets specified in function call, use all targets
  if(is.null(these.targets)) these.targets <- levels(as.factor(forecast.data$target))
  
  ### Create file with forecasts to be plotted
  these.forecasts <- summarize.forecasts(forecast.data, this.team, ci=c(0.5, 0.95), these.seasons, these.targets)
  
  ### If no locations specified in function call, use all locations forecast data exist for
  if(is.null(these.locations)) these.locations <- levels(as.factor(these.forecasts$location))
  
  
  ### Set up output file for graphs
  pdf(file=file, width=8.5, height=11, 
  		onefile=T, paper='letter')
  
  for (this.location in these.locations) {
    i<-1
    #Loop through seasons and locations to create graphs    
  	for (this.season in these.seasons) {
  	  for (this.target in these.targets) {
  	    
  	    ### Better formatted names for display
  	    if (this.target == "pkwk") target.name <- "Peak Week"
  	    if (this.target == "pkper") target.name <- "Peak Percentage ILI"
  	    if (this.target == "onset") target.name <- "Onset Week"
  	    if (this.target == "1wk") target.name <- "1 Week Prediction"
  	    if (this.target == "2wk") target.name <- "2 Week Prediction"
  	    if (this.target == "3wk") target.name <- "3 Week Prediction"
  	    if (this.target == "4wk") target.name <- "4 Week Prediction"
  	    
  			this.forecast <- filter(these.forecasts, team == this.team & target == this.target & 
  			   season == this.season & location == this.location)
  		  # check for missing predictions; if missing, skip 
  			if (dim(this.forecast)[1] == 0 | all(is.na(this.forecast[ , c("point", "lwr2.5", "upr97.5")]))) {
  				plot(seq(0, 48, by=4), seq(0, 48, by=4), type='n', axes=F, 
  				     main=this.season, xlab="forecast date", ylab='')
  			  axis(1, at=seq(0, 48, by=4))
  			} else {
    			# if there are any upper bounds of Inf, modify to y max
    			if (any(this.forecast[ , c("upr75", "upr97.5")] == Inf, na.rm=T)) {
    				y.max <- range(as.matrix(this.forecast[ , c("point", "upr97.5")]), finite=T)[2]
    				this.forecast[this.forecast[ , "upr75"] == Inf, "upr75"] <- y.max
    				this.forecast[this.forecast[ , "upr97.5"] == Inf, "upr97.5"] <- y.max
    			}
  		 	
  			  #Plotting function for week forecasts
  			  if(this.target %in% week.targets){
  			    if(targets[targets$target == this.target & targets$season == this.season & targets$location == this.location, "observation"]>=40){
  			      target.value <- targets[targets$target == this.target & targets$season == this.season & targets$location == this.location, "observation"]
  			    } else {target.value <- targets[targets$target == this.target & targets$season == this.season & targets$location == this.location, "observation"] + 52}
  			    
    			  this.plot[[i]] <- ggplot(this.forecast, aes(forecast.date,point)) +
    			                  geom_hline(aes_string(yintercept=target.value),
    			                            lty=2) +
    			                  geom_line(size=1) +
              			        geom_ribbon(aes(ymin=lwr25, ymax=upr75), alpha=0.3) +
              			        geom_ribbon(aes(ymin=lwr2.5, ymax=upr97.5), alpha=0.1) +
              			        
              			        scale_x_date(name="Forecast Date", date_breaks="8 weeks") +
              			        scale_y_continuous(name="MMWR Week", breaks=seq(40,72,2),
              			                           labels=c("40","42","44","46","48","50","52","2","4","6","8","10","12","14","16","18","20"),
              			                           limits=range(this.forecast[ , c("point", "lwr2.5", "upr97.5")], target.value, na.rm=T)) +
              			        theme_classic() +
              			        labs(title=paste(target.name, toupper(this.location), sep=' - ')) 
  			  }
  			 
  			  #Plotting functions for percentage forecasts
  			  if(this.target %in% percent.targets){
  			    if(this.target %in% static.targets){
  			      this.plot[[i]] <- ggplot(this.forecast, aes(forecast.date,point)) +
              			        geom_line(size=1) +
              			        geom_ribbon(aes(ymin=lwr25, ymax=upr75), alpha=0.3) +
              			        geom_ribbon(aes(ymin=lwr2.5, ymax=upr97.5), alpha=0.1) +
              			        geom_hline(data=targets, aes_string(yintercept=targets[targets$target == this.target & targets$season == this.season & 
                                                                           targets$location == this.location, "observation"]), lty=2) +
              			        scale_x_date(name="Forecast Date", date_breaks="8 weeks") +
              			        scale_y_continuous(name="Percent") +
              			        theme_classic() +
              			        labs(title=paste(target.name, toupper(this.location), sep=' - '))
  			    } else {
  			      
  			      
  			     #Join weekly targets as part of data frame - enables aes to plot correctly
  			     this.forecast <- inner_join(this.forecast,targets)
  			      
  			      this.plot[[i]] <- ggplot(this.forecast, aes(forecast.date,point)) +
              			        geom_line(size=1) +
              			        geom_ribbon(aes(ymin=lwr25, ymax=upr75), alpha=0.3) +
              			        geom_ribbon(aes(ymin=lwr2.5, ymax=upr97.5), alpha=0.1) +
              			        geom_line(aes(forecast.date,observation), lty=2) +
              			        scale_x_date(name="Forecast Date", date_breaks="8 weeks") +
              			        scale_y_continuous(name="Percent") +
              			        theme_classic() +
              			        labs(title=paste(target.name, toupper(this.location), sep=' - '))
  			    }
  			  }
  			  i <- i+1
  			}
  	  }
  	  #Set up 4x2 output of target plots on pdf page
  	  args.list <- c(this.plot[1:i-1],list(nrow=4,ncol=2))
  	  do.call(grid.arrange,args.list)
  	}
  }
  dev.off()
}



###Summarizes forecasts for particular team, target, location, and season
summarize.forecasts <- function(forecast.data, this.team, ci=0.95, seasons=NULL, these.targets=NULL) {
  these.preds <- filter(forecast.data, team == this.team & 
                          !is.na(value) &
                          value != 0)
  if (!is.null(seasons)) these.preds <- filter(these.preds, season %in% seasons)
  
  
  these.forecasts <- data.frame()
  
  ### Add 52 to all weeks in new calendar year to keep sequential order of weeks
  for (i in 1:length(these.preds$bin.lwr)){
    if(these.preds$bin.lwr[i] < 30 & these.preds$bin.lwr[i] > 0 & !is.na(these.preds$bin.lwr[i]) & these.preds$target[i] %in% week.targets){
      these.preds$bin.lwr[i] <- these.preds$bin.lwr[i]+52
      these.preds$bin.upr.strict[i] <- these.preds$bin.upr.strict[i]+52
    }
  }

  for (this.target in these.targets) {
	  for (this.season in levels(as.factor(these.preds$season))) {
	    for (this.location in levels(as.factor(these.preds$location))) {
	      for (this.wk in levels(as.factor(these.preds$forecast.date))) {
    	    this.point <- filter(these.preds, target == this.target & season == this.season & 
              location == this.location & forecast.date == this.wk & pred.type == "point")[ , "value"]
    	    #Only calculate approximate bins if there is a point value to have estimates around
    	    if(length(this.point)!=0){
      	    if(this.point<30 & this.point>0 &  this.target %in% week.targets) this.point <- this.point + 52
      	    this.forecast <- filter(these.preds, target == this.target & season == this.season & 
        	        location == this.location & forecast.date == this.wk & pred.type == "bin") %>% 
                  find.bin.intervals(ci) %>%
            	    mutate(
            	        team=this.team, 
                      target=this.target,
            	        location=this.location,
                      season=this.season, 
                      forecast.date=this.wk,
                      point=this.point)
        	   these.forecasts <- rbind(these.forecasts,this.forecast)
    	    }
	      }
	    }
	  }
  }
  #Coerce forecast date back into date format from factor
  these.forecasts$forecast.date <- as.Date(these.forecasts$forecast.date)
  return(these.forecasts)
}

###Returns data frame of bins with forecasted prediction value > CI limit/2
find.bin.intervals <- function(preds, ci) {
  forecast <- numeric()
  preds <- arrange(preds, bin.lwr)
  #Set CIs to NA if percentage totals don't add up
  if(sum(preds$value) < 0.9 | sum(preds$value)>1.1){
    for (this.ci in ci){
      p.lwr <- (1 - this.ci)/2
      p.upr <- 1 - (1 - this.ci)/2
      forecast[paste0("lwr",100*p.lwr)] <- NA
      forecast[paste0("upr",100*p.upr)] <- NA
    }
  }
  else {
    for (this.ci in ci) {
      p.lwr <- (1 - this.ci)/2
      p.upr <- 1 - (1 - this.ci)/2
      forecast[paste0("lwr",100*p.lwr)] <- 
          preds$bin.lwr[min(which(cumsum(preds$value) >= p.lwr))]
      forecast[paste0("upr",100*p.upr)] <- 
          preds$bin.upr[min(which(cumsum(preds$value) >= p.upr))]
      if (is.na(forecast[paste0("upr",100*p.upr)])) forecast[paste0("upr",100*p.upr)] <- Inf
      if (forecast[paste0("lwr",100*p.lwr)] == forecast[paste0("upr",100*p.upr)]) {
        forecast[paste0("upr",100*p.upr)] <- 
            preds$bin.upr.strict[min(which(cumsum(preds$value) >= p.upr))]
      }
    }
  }
  return(as.data.frame(t(forecast)))
}



##### Plot comparing forecasts of different teams #####
forecast.compare <- function(forecast.data, file, these.seasons, these.targets=NULL,
                             these.teams=NULL,blinded.teams=NULL,these.locations=NULL,facet=TRUE,
                             week.breaks="16 weeks") {
  ### If no targets specified in function call, use all targets
  if(is.null(these.targets)) these.targets <- levels(as.factor(forecast.data$target))
  ### If no locations specified in function call, use all locations
  if(is.null(these.locations)) these.locations <- levels(as.factor(forecast.data$location))
  ### If no teams specified in function call, use all teams
  if(is.null(these.teams)) these.teams <- levels(as.factor(forecast.data$team))
 
  ### Create dataset of targets/locations/teams of interest - split seasonal targets and weekly targets to join correctly
  forecast.data.season <- select(forecast.data,-bin.lwr,-bin.upr.strict,-name) %>%
    filter(target %in% c("onset","pkwk","pkper"),
           pred.type == 'point',
           team %in% these.teams,
           location %in% these.locations) %>%
    inner_join( 
      select(targets,-forecast.date)) 
  forecast.data.weekly <- select(forecast.data,-bin.lwr,-bin.upr.strict,-name) %>%
    filter((!target %in% c("onset","pkwk","pkper")),
           pred.type == 'point',
           team %in% these.teams,
           location %in% these.locations) %>%
    inner_join(
      targets)
  compare.forecasts <- rbind(forecast.data.season, forecast.data.weekly) %>%
    filter(target %in% these.targets)

  ### Add 52 to weeks in new calendar year for graphing purposes
  for(i in 1:length(compare.forecasts$value)){
    if(compare.forecasts$target[i] %in% week.targets){
      if(compare.forecasts$value[i]<40 & !is.na(compare.forecasts$value[i])) compare.forecasts$value[i] <- compare.forecasts$value[i]+52
      if(compare.forecasts$observation[i]<40 & !is.na(compare.forecasts$observation[i])) compare.forecasts$observation[i] <- compare.forecasts$observation[i]+52
      if(compare.forecasts$observation2[i]<40 & !is.na(compare.forecasts$observation2[i])) compare.forecasts$observation2[i] <- compare.forecasts$observation2[i]+52
    }
  }
  
  ### Replace team names with blinded letters as needed
  if(!is.null(blinded.teams)){
    blind <- filter(team.blind, team %in% blinded.teams)
    compare.forecasts$team <- stri_replace_all_fixed(compare.forecasts$team,
                                                   blind$team,
                                                   blind$blind,
                                                   vectorize_all = FALSE)
  }
 
  pdf(file=file, width=11, height=8.5,onefile=T, paper='letter')
  for(this.location in levels(as.factor(compare.forecasts$location))){
    for(this.season in levels(as.factor(compare.forecasts$season))){
      for(this.target in levels(as.factor(compare.forecasts$target))){
        
        ### Better formatted names for display
        if (this.target == "pkwk") target.name <- "Peak Week"
        if (this.target == "pkper") target.name <- "Peak Percentage ILI"
        if (this.target == "onset") target.name <- "Onset Week"
        if (this.target == "1wk") target.name <- "1 Week Prediction"
        if (this.target == "2wk") target.name <- "2 Week Prediction"
        if (this.target == "3wk") target.name <- "3 Week Prediction"
        if (this.target == "4wk") target.name <- "4 Week Prediction"
        
        
        #Pull out relevant forecasts to graph
        this.forecast <- filter(compare.forecasts,target == this.target & 
                                  season == this.season & location == this.location)
        
        #Plotting function for week forecasts
        if(this.target %in% week.targets){
          this.plot <- ggplot(this.forecast, aes(forecast.date,value)) +
                        geom_line(aes(color=team),size=1) +
                        geom_line(aes(forecast.date,observation),lty=2) +
                        scale_x_date(name="Forecast Date", date_breaks=week.breaks) +
                        scale_y_continuous(name="MMWR Week", breaks=seq(40,72,2),
                                           labels=c("40","42","44","46","48","50","52","2","4","6","8","10","12","14","16","18","20"),
                                           limits=range(this.forecast[ , c("value", "observation")], na.rm=T)) +
                        scale_color_discrete(name="Team") +
                        theme_classic() +
                        labs(title=paste(target.name, toupper(this.location), sep=' - ')) 
        }else{this.plot <- ggplot(this.forecast, aes(forecast.date,value)) +
              geom_line(aes(color=team),size=1) +
              geom_line(aes(forecast.date,observation),lty=2) +
              scale_x_date(name="Forecast Date", date_breaks=week.breaks) +
              scale_y_continuous(name="Percent") +
              scale_color_discrete(name="Team") +
              theme_classic() +
              labs(title=paste(target.name, toupper(this.location), sep=' - ')) +
              facet_wrap(~target)
        }
  
        # Create facets of individual teams rather than one messy plot
        if(facet==TRUE){
          this.plot <- this.plot + facet_wrap(~team)
        }

        print(this.plot)
        
      }
    }
  }
  dev.off()
}


  
  

