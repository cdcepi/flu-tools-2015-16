require(dplyr)

calculate.three.bin.scores <- function(forecast.data, targets) {
  ### add observations - split seasonal targets and weekly targets to join correctly
  forecast.data.season <- filter(forecast.data,
                                 target %in% c("onset","pkwk","pkper"),
                                 pred.type != 'point') %>%
                          inner_join( 
                                    select(targets,-forecast.date)) 
  forecast.data.weekly <- filter(forecast.data,
                                 (!target %in% c("onset","pkwk","pkper")),
                                 pred.type != 'point') %>%
                          inner_join(
                                     targets)
  forecast.data <- rbind(forecast.data.season, forecast.data.weekly)
  
  ### create score table with one row per forecast
  forecast.scores <- select(forecast.data, -name, -value, -pred.type, 
      -bin.lwr, -bin.upr.strict) %>% 
    as.data.frame() %>%
    distinct()
  ### calculate three bin log score
  for (i in 1:dim(forecast.scores)[1]) {
    forecast.scores$score[i] <- 
        filter(forecast.data, 
            team == forecast.scores$team[i] & 
            location == forecast.scores$location[i] & 
            target == forecast.scores$target[i] & 
            forecast.date == forecast.scores$forecast.date[i]) %>%
        three.bin.score()
    if(i%%100==0){
      cat(i)
      cat("..")
    }
  }
  return(as.data.table(forecast.scores))
}

three.bin.score <- function(these.probs) {
  #if (is.na(these.probs$observation[1])) return(NA)
  these.probs <- arrange(these.probs, bin.lwr)
  correct.bin <- max(which(these.probs$bin.lwr <= these.probs$observation))
  #If correct bin is first or last bin, only take that bin. Otherwise include bin before and after
  if(correct.bin==1 | correct.bin==dim(these.probs)[1]){
    these.bins <- correct.bin
  }else{
    these.bins <- correct.bin + -1:1
    these.bins <- these.bins[1 <= these.bins & these.bins <= dim(these.probs)[1]]
  }
  
  #Figure out additional correct bins if multiple peaks
  if(!is.na(these.probs$observation2[1])){
    correct.bin2 <- max(which(these.probs$bin.lwr <= these.probs$observation2))
    if(correct.bin2==1 | correct.bin2==dim(these.probs)[1]){
      these.bins2 <- correct.bin2
    }else{
      these.bins2 <- correct.bin2 + -1:1
      these.bins2 <- these.bins2[1 <= these.bins2 & these.bins2 <= dim(these.probs)[1]]
    }
    these.bins <- union(these.bins,these.bins2)
  }
  return(log(sum(these.probs$value[these.bins])))
}
