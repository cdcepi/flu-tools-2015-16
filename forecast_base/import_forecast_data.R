require(dplyr)
require(tidyr)
require(stringr)
require(data.table)

import.forecast.data <- function(submissions) {
  forecast.data <- data.table()
  for (i in 1:length(submissions)) {
    for (j in 1:length(submissions[[1]])) {
      forecast.data <- bind_rows(forecast.data, expand.prediction(submissions, i, j))
    }
  }
  return(as.data.table(forecast.data))
}

expand.prediction <- function(submissions, i, j) {
  team <- names(submissions)[i]
  name.data <- unlist(str_split(names(submissions[[i]])[j], "[.]"))
  prediction <- as.data.table(submissions[[i]][[j]], keep.rownames=T) %>%
    dplyr::rename(name=rn)
  prediction.expanded <- gather(prediction, forecast.wk, value, -name) %>%
      as.data.table() %>% 
      mutate(
          team = team,
    		  location = name.data[[2]],
    			target = name.data[[1]],
    			pred.type = ifelse(name == 'Point Prediction', 'point', 'bin'),
    			name = str_replace_all(name, "\\s", ""),
          
          ### lower limit for bin - match first number
    			# if a number appears, the first number, otherwise (point prediction or none) -> NA
          bin.lwr = as.numeric(ifelse(str_detect(name, "\\d+\\.?\\d+"),
              str_extract(name, "\\d+\\.?\\d+"), NA)),

          ### upr bin limit - match number after '-' if present
    			# if a week -> bin.lwr + 1
          bin.upr.strict = ifelse(str_detect(name, "EW"), bin.lwr + 1, NA),
    			# if there is an equals sign, a number after "<", and not a week -> number after "<" 
          bin.upr.strict = ifelse(!str_detect(name, "EW") & str_detect(name, "<\\d+\\.?\\d+"), 
              str_extract(name, "<\\d+\\.?\\d+"), bin.upr.strict),
    			bin.upr.strict = ifelse(str_detect(bin.upr.strict, "<"), 
              as.numeric(str_replace(bin.upr.strict, "<", "")), bin.upr.strict),
          
          ### if Pr(none), set to 0-0
          bin.lwr = ifelse(target %in% c("onset", "pkwk") & name == "Pr(none)", 
              0, bin.lwr),
          bin.upr.strict = ifelse(target %in% c("onset", "pkwk") & name == "Pr(none)", 
              0, bin.upr.strict))
  
  return(prediction.expanded)
}

