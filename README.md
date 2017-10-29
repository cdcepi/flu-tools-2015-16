# Flu Forecasting Scoring Tools

Here are all of the files/code used to score the 2015/2016 flu forecasting challenge submissions. I've outlined the dependency structures below and how the files are used below. Please let me know if you have any questions or suggestions to improve the code!

## Folders

`compare_plots` - folder to store plots comparing forecasts between teams. Created automatically by Process_Submission.R

`forecast_base` - folder storing R code used in main Process_Submission file

`forecast_submissions` - folder to store submissions to be scored. Must be structured in the same manner as
	"templates" folder. **You** must create this folder to store your submission in.

`team_plots` - folder to store plots of team forecasts with approximated confidence bands. Created automatically by `Process_Submission.R`

`team_score_sheets` - folder to store CSV files of team specific scores. Created automatically by `Process_Submission.R`

`templates` - folder storing null templates of scores sheets. Used to verify structure of forecast submissions


## Files

`15-16_ILINet_Flu.csv` - ILINet data from the season as of Week 28

`flu_scores_2015-16_share.Rproj` - R project file

`flu_targets` - Targets imported into `Process_Submission.R` to score the forecasts. Seasonal targets were handcoded
		based on ILINet data, weekly targets were copied and pasted from `weekly_flu_targets.csv`

`Process_Submission.R` - main file to score submissions and generate plots of forecasts and scores

### Dependencies
`import_forecast.R` - import forecast CSV files from `forecast_submissions` folder and templates
from `templates` folder

`import_forecast_data.R` - convert forecasts from lists to single data frame

`plot_forecasts_ggplot.R` - graph individual forecasts with approximate confidence bands and comparative
		plots between teams

`plot_scores_flu_2015-16.R` - plot individual forecast scores and comparative scores between forecasts

`three_bin_score.R` - generate log score based on the three bin scoring system

`verify_forecasts.R` - verify forecasts submitted. Normalize probabilities if sum to between 0.9 and 1.1,
		otherwise generate error and set to missing.

`weekly_flu_targets.csv` - weekly targets for `now/nearcasting` generated from `weekly_targets.R`

`weekly_targets.R` - generates weekly targets to score 1-4 week ahead forecasts.


## Notes
Your forecasts to be scored must be structured in the same manner as the "templates" folder. If probabilities
in a particular column do not sum to between 0.9 and 1.1 then the probabilities in that row will be set to NA.
As the `verify_forecasts` file runs it will output the number of columns that have been normalized in a particular
submission and also generate warnings for any missing values.
