plot2 <- function() {
  library("dplyr")
  library ("ggplot2")
  
  data <- read.csv("data/repdata-data-StormData.csv", as.is = TRUE, nrows = 1000)
  
  data_trimmed <- data  %>%
    select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
  
  propertyCost <- data_trimmed %>%
    group_by(EVTYPE) %>%
    filter(PROPDMG > 0 | CROPDMG > 0) %>%
    summarise(
      PROPDMG = sum(PROPDMG),
      CROPDMG = sum(CROPDMG),
      TOTAL = sum(PROPDMG,CROPDMG)
    ) %>%
    arrange(desc(TOTAL)) %>%
    head(25)
#   qplot(
#     data = propertyCost,x = TOTAL,y = EVTYPE,xlab = "Total Combined Property Damage",ylab =
#       "Event Type"
#   )
  
  return(propertyCost)
  
}