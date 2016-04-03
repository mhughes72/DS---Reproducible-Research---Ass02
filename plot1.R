plot1 <- function() {
  
  library("dplyr")
  library("plyr")
  library ("ggplot2")
  
  
  data<-read.csv("data/repdata-data-StormData.csv", as.is = TRUE)
  
  #data <- read.csv(bzfile(file_name), as.is = TRUE)
  
  #Physical Injuries: FATALITIES, INJURIES per EVTYPE
  
  
  data_trimmed <- data %>%
    select(BGN_DATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
  
  humanCost <- data_trimmed %>% 
  group_by(EVTYPE) %>%
  filter(FATALITIES > 0 | INJURIES > 0) %>%
  summarize(total_fatalities = sum(FATALITIES),
            total_injuries = sum(INJURIES))
#   arrange(desc(total_injuries),
#           desc(total_fatalities),
#           EVTYPE)
  

  
  return(humanCost)
  
}