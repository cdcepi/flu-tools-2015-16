rm(list=ls())

require(stringr)
require(dplyr)
require(tidyr)
require(data.table)

#Set directory where your submissions and template are stored
submission.dir <- "forecast_submissions/"
template.dir <- "templates/"

#Import targets for scoring
targets <- read.csv("flu_targets.csv") %>%
  mutate(
    location = tolower(location),
    forecast.date=as.Date(forecast.date, "%m/%d/%Y"),
    observation = as.numeric(observation),
    observation2 = as.numeric(observation2))

##############################################################
### import forecast and template files
source("forecast_base/import_forecasts.R")
### submissions - reads all csv files in this directory
submissions <- import.forecasts(submission.dir)

### templates
templates <- import.templates(template.dir)[[1]]
names(templates) <- str_replace(names(templates), '\\..*', '')

##############################################################
### verify forecasts and normalize probabilities
source("forecast_base/verify_forecasts.R")
submissions <- verify.subs(submissions, templates)

##############################################################
### convert forecast data into data table for analysis
source("forecast_base/import_forecast_data.R")
forecast.data <- import.forecast.data(submissions)

### add forecast.date
forecast.data <- mutate(forecast.data,
                        mo = tolower(str_sub(forecast.wk, 1, 3)),
                        dy = str_sub(forecast.wk, 4, 5),
                        yr = ifelse(mo %in% c("oct", "nov", "dec"), 2015, 2016),
                        forecast.date = as.Date(paste(yr, mo, dy, sep='-'), format='%Y-%b-%d')) %>%
  select(-forecast.wk, -mo, -dy, -yr)

### save as csv file
write.csv(forecast.data, file="forecast_data.csv")

### save as R object
save(forecast.data, file="forecast_data.RData")

##############################################################
### plot forecasts
source("forecast_base/plot_forecasts_ggplot.R")

### Add forecast year for 2015/2016
### Keep until data are imported with forecast year for potential graphing of multiple seasons later
forecast.data$season <- "2015/2016"
 
### Generate pdfs of forecast plots for all teams for specified seasons
#Loop to plot forecasts for all teams with approximate confidence bands
for(this.team in levels(as.factor(forecast.data$team))){
  plot.forecast(this.team, forecast.data,
                paste0("team plots/",this.team,"_predictions", ".pdf"),
                these.seasons)
}

### get approximate (binned) intervals on distribution
# Not used for scoring, but can be accessed from this data frame if desired
forecast.intervals <- summarize.forecasts(forecast.data, this.team, ci=c(0.5, 0.95))


### Generate comparitive plots for teams with other teams blinded

#Create folder for output if it doesn't exist
dir.create(file.path("compare_plots/"), showWarnings = FALSE)
#Create data frame matching team names with blinded letters
team.blind <- data.frame(team=c("4sight","arete","cu1","cu2","delphi-archefilter","delphi-epicast",
                                   "delphi-stat","isu","jl","kbsi1","kot","neu","psi","umn"),
                         blind=c("A","B","C","D","E","F","G","H","I","J","K",
                                 "L","M","N"))
blinded.teams <- c("4sight","arete","cu1","cu2","delphi-archefilter","delphi-epicast",
                   "delphi-stat","isu","jl","kbsi1","kot","neu","psi","umn")

### generate a pdf of comparitive forecast plots
forecast.compare(forecast.data, "compare_plots/all_teams_compare_predictions.pdf",
                 "2015/2016")
forecast.compare(forecast.data, "compare_plots/all_blind_compare_predictions.pdf",
                 "2015/2016",blinded.teams=blinded.teams)
forecast.compare(forecast.data, "compare_plots/test_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="4sight"))
forecast.compare(forecast.data, "compare_plots/arete_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="arete"))
forecast.compare(forecast.data, "compare_plots/cu_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="cu1" &
                                                    blinded.teams != "cu2"))
forecast.compare(forecast.data, "compare_plots/delphi_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="delphi-archefilter" &
                                                     blinded.teams != "delphi-epicast" &
                                                     blinded.teams != "delphi-stat"))
forecast.compare(forecast.data, "compare_plots/isu_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="isu"))
forecast.compare(forecast.data, "compare_plots/jl_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="jl"))
forecast.compare(forecast.data, "compare_plots/kbsi1_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="kbsi1"))
forecast.compare(forecast.data, "compare_plots/kot_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="kot"))
forecast.compare(forecast.data, "compare_plots/neu_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="neu"))
forecast.compare(forecast.data, "compare_plots/psi_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="psi"))
forecast.compare(forecast.data, "compare_plots/umn_compare_predictions.pdf",
                 "2015/2016",blinded.teams=subset(blinded.teams,blinded.teams!="umn"))

### Plots with all teams together for presentation
forecast.compare(forecast.data, "compare_plots/all_blind_compare_predictions_oneplot.pdf",
                 "2015/2016",blinded.teams=blinded.teams, facet=F,week.breaks="4 weeks")




##############################################################
### calculate 3-bin log score
source("forecast_base/three_bin_score.R")


forecast.scores <- calculate.three.bin.scores(forecast.data, targets)


### if score is less than -10 or NA, change to -10
forecast.scores <- mutate(forecast.scores, 
                          score = ifelse(score < -10 | is.na(score), -10, score))

### Create folder for output if it doesn't exist
dir.create(file.path("team_score_sheets/"), showWarnings = FALSE)

### save as csv file with files for each team
write.csv(forecast.scores, file="forecast_scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="4sight",],
          file="team_score_sheets/4sight_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="arete",],
          file="team_score_sheets/ARETE_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="cu1"|forecast.scores$team=="cu2",],
          file="team_score_sheets/Columbia_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="delphi-archefilter" | 
                            forecast.scores$team=="delphi-epicast" | 
                            forecast.scores$team=="delphi-stat",],
          file="team_score_sheets/Delphi_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="isu",],
          file="team_score_sheets/ISU_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="jl",],
          file="team_score_sheets/JL_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="kbsi1",],
          file="team_score_sheets/KBSI1_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="kot",],
          file="team_score_sheets/KoT_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="neu",],
          file="team_score_sheets/Northeastern_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="psi",],
          file="team_score_sheets/PredictiveScience_Scores.csv", row.names=F)
write.csv(forecast.scores[forecast.scores$team=="umn",],
          file="team_score_sheets/UMN_Scores.csv", row.names=F)


### save as R object
save(forecast.scores, file="forecast_scores.RData")

##############################################################
### plot scores
source("forecast_base/plot_scores_flu_2015-16.R")

#Create folder for output if it doesn't exist
dir.create(file.path("team_plots/"), showWarnings = FALSE)

plot.scores.team(forecast.scores, these.teams="4sight",blind=T,
                 file=paste0("team_plots/4sight_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="arete",blind=T,
                 file=paste0("team_plots/arete_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams=c("cu1","cu2"),blind=T,
                 file=paste0("team_plots/cu_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams=c("delphi-archefilter",
                                                "delphi-epicast",
                                                "delphi-stat"),blind=T,
                 file=paste0("team_plots/delphi_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="isu",blind=T,
                 file=paste0("team_plots/isu_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="jl",blind=T,
                 file=paste0("team_plots/jl_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="kbsi1",blind=T,
                 file=paste0("team_plots/kbsi1_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="kot",blind=T,
                 file=paste0("team_plots/kot_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="neu",blind=T,
                 file=paste0("team_plots/neu_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="psi",blind=T,
                 file=paste0("team_plots/psi_score_plots.pdf"))
plot.scores.team(forecast.scores, these.teams="umn",blind=T,
                 file=paste0("team_plots/umn_score_plots.pdf"))


### plot number of plots within 1 unit of true amount
plot.point.bins(forecast.data, these.targets=c("onset","pkwk","pkper"),
                 these.locations="us")

##############################################################
### tables of scores
overall.scores <- forecast.scores %>%
  # can add a time filter here to restrict the time range over which evaluation takes place
  group_by(team, location, target) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  arrange(location,target, desc(score))
overall.scores$ExpScore <- exp(overall.scores$score)  

write.csv(overall.scores, "Overall_Scores.csv")


#Average scores by target
target.scores <- forecast.scores %>%
  # can add a time filter here to restrict the time range over which evaluation takes place
  group_by(team, target) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  arrange(target, desc(score))

write.csv(target.scores, "Target_Scores.csv")

#Average scores by location  
location.scores <- forecast.scores %>%
  # can add a time filter here to restrict the time range over which evaluation takes place
  group_by(team, location) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  arrange(location, desc(score))

write.csv(location.scores, "Location_Scores.csv")

#Average across targets and location
average.scores <- forecast.scores %>%
  # can add a time filter here to restrict the time range over which evaluation takes place
  group_by(team) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  arrange(desc(score))

write.csv(average.scores, "Overall_Average_Scores.csv")
  