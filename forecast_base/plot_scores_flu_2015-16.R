require(ggplot2)
require(grid)
require(gridExtra)
require(stringi)
require(dplyr)



plot.scores.team <- function(forecast.scores, these.teams, blind=FALSE,
                             file) {
  #this.color <- 'darkred'
  pdf(file=file, width=8.5, height=11, 
   		onefile=T, paper='letter')
	#par(mfrow=c(4, 2), oma=c(0, 0, 0, 0), mar=c(3, 3, 3, 0.5), mgp=c(1.5, 0.5, 0))
	#plot.num <- 1
	
  #If blinding desired, set up vector of blinded team names
  if(blind==TRUE){
    blinded.teams <- levels(as.factor(forecast.scores$team))[!(levels(as.factor(forecast.scores$team)) %in% these.teams)]
  }
  
	for (this.location in levels(as.factor(forecast.scores$location))) {
	  for (this.target in levels(as.factor(forecast.scores$target))) {
		  
	    #Better target name for display on graph
	    if (this.target == "pkwk") target.name <- "Peak Week"
	    if (this.target == "pkper") target.name <- "Peak Percentage ILI"
	    if (this.target == "onset") target.name <- "Onset Week"
	    if (this.target == "1wk") target.name <- "1 Week Prediction"
	    if (this.target == "2wk") target.name <- "2 Week Prediction"
	    if (this.target == "3wk") target.name <- "3 Week Prediction"
	    if (this.target == "4wk") target.name <- "4 Week Prediction"
	    
	    
	    these.scores <- filter(forecast.scores,
	                           location == this.location) %>% # & target == this.target) %>%
			      arrange(forecast.date) %>%
			      select(forecast.date, score, team,target)
	    
	    #Better target name for display on graph
	    these.scores$target.name[these.scores$target == "pkwk"] <- "Peak Week"
	    these.scores$target.name[these.scores$target == "pkper"]  <- "Peak Percentage ILI"
	    these.scores$target.name[these.scores$target == "onset"]  <- "Onset Week"
	    these.scores$target.name[these.scores$target == "1wk"]  <- "1 Week Prediction"
	    these.scores$target.name[these.scores$target == "2wk"]  <- "2 Week Prediction"
	    these.scores$target.name[these.scores$target == "3wk"]  <- "3 Week Prediction"
	    these.scores$target.name[these.scores$target == "4wk"]  <- "4 Week Prediction"
	    
		  
		  if(!is.null(blinded.teams)){
		    blind <- filter(team.blind, team %in% blinded.teams)
		    these.scores$team <- stri_replace_all_fixed(these.scores$team,
		                                                     blind$team,
		                                                     blind$blind,
		                                                     vectorize_all = FALSE)
		  }
		  
		  #Set transparency differently for teams of interest and comparison teams
		  these.scores$alpha <- 0.5
		  these.scores$alpha[these.scores$team %in% these.teams] <- 1
		  
		  this.plot <- ggplot(these.scores, aes(forecast.date,score, color=team, alpha=alpha, size=alpha)) +
		    geom_line() +
		    scale_alpha_continuous(range=c(0.25,1)) +
		    scale_size_continuous(range=c(0.5,1)) +
		    scale_x_date(name="Forecast Date", date_breaks="8 weeks") +
		    scale_y_continuous(name="Log Score",limits=c(-10,0)) +
		    guides(color=guide_legend(title="Team"), alpha = "none", size="none") +
		    labs(title=toupper(this.location)) +
		    theme_classic() +
		    facet_wrap(~target.name,ncol=2)
		  
		  
			# plot(these.scores, type='n', ylim=c(-10, 0), axes=F,
			# 		xlab="forecast date", ylab="3-bin log score", lwd=1.5, cex.lab=1)
			# for (this.other.team in levels(as.factor(forecast.scores$team))) {
			#   these.other.scores <- filter(forecast.scores, 
			#       team == this.other.team & target == this.target & location == this.location) %>%
			#       arrange(forecast.date) %>%
			#       select(forecast.date, score)
			# 	lines(these.other.scores, col=adjustcolor(this.color, 0.25), lwd=1.5)
			# }
			# lines(these.scores, col=adjustcolor(this.color, 1), lwd=1.5)
			# axis.Date(1, these.scores$forecast.date)
			# axis(2)
			# box(bty='l')
			# 
			# mtext(paste(target.name, toupper(this.location), sep=' - '), side=3, line=1, adj=0.5)
	  }
	  print(this.plot)
	}
	dev.off()
}

plot.point.bins <- function(forecast.data, these.targets=NULL, these.locations=NULL) {
  
  ### If no targets specified in function call, use all targets
  if(is.null(these.targets)) these.targets <- levels(as.factor(forecast.data$target))
  ### If no locations specified in function call, use all locations
  if(is.null(these.locations)) these.locations <- levels(as.factor(forecast.data$location))
  
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
  these.forecasts <- rbind(forecast.data.season, forecast.data.weekly) %>%
    filter(target %in% these.targets & location %in% these.locations)
  
  for(this.target in these.targets){
    for(this.location in these.locations){
      these.results <- data.frame()
      for(this.date in levels(as.factor(forecast.data$forecast.date))){
        this.count <- filter(these.forecasts, target == this.target & location == this.location &
                                  forecast.date == this.date & value>=(observation-1) & value <=(observation+1)) %>%
                              count() %>%
                              as.numeric()
        this.result <- data.frame(count=this.count,
                                target=this.target,
                                location=this.location,
                                forecast.date=this.date)
        these.results <-rbind(this.result,these.results)

        
      }
      these.results <- arrange(these.results, desc(forecast.date))

      if (this.target == "pkwk") target.name <- "Peak week"
      if (this.target == "pkper") target.name <- "Peak percentage ILI"
      if (this.target == "onset") target.name <- "Onset week"
      if (this.target == "1wk") target.name <- "1 Week Prediction"
      if (this.target == "2wk") target.name <- "2 Week Prediction"
      if (this.target == "3wk") target.name <- "3 Week Prediction"
      if (this.target == "4wk") target.name <- "4 Week Prediction"
      
      jpeg(paste0(this.target,"_",this.location,"_accuracy_counts.jpeg"), 
           width=9, height=6, units="in", res=72) 
      #Create bar chart of totals
      barplot(these.results$count, 
              names.arg=as.Date(these.results$forecast.date),
              col="dark blue",
              main=paste0(target.name," accurate to within 1 unit"),
              xlab="Forecast date",
              ylab="Number of teams")
           
      dev.off()
   
    }
  }
}




