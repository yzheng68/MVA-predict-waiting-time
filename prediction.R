library(dplyr)
library(ggplot2)
library(kimisc)
library(lubridate)
load("mvadata.rda")

create_time_index <- function(){
  time <- c()
  start <- "08:30:00"
  end <- "16:30:00"
  while (start <= end){
    time <- c(time, start)
    start <- seconds.to.hms(hms.to.seconds(start) + 300)
  }
  return(time)
}

wait_time_trend <- function(office, service, day, time_index, arrival_time, timelabel){
  
  subset_idx <- which(mvadata$office == office & mvadata$service == service & mvadata$day == day) 
  subset_data <- mvadata[subset_idx,]
  
  model.lo <- loess(wait_time ~ index, subset_data,span=0.2)
  max_idx <- max(subset_data$index)
  min_idx <- min(subset_data$index)
  prediction_curve <- predict(model.lo, data.frame(index = seq(min_idx, max_idx, 0.05)), se = TRUE)
  # plot(seq(1,95,0.1),prediction$fit)
  # points(subset_data$index,subset_data$wait_time,col="red")
  
  expect_wait <- predict(model.lo, data.frame(index = time_index), se = TRUE)$fit
  if (expect_wait < 0){expect_wait <- 0}
  
  subset_dataframe <- data.frame(timepoints = seq(min_idx,max_idx,0.05), predict_wait = prediction_curve$fit)
  
  p <- ggplot(data = subset_dataframe, aes(x=timepoints, y=predict_wait)) + 
    geom_point(size = 0.2) + 
    scale_x_continuous(breaks = seq(1,length(timelabel),5), labels = timelabel[seq(1,length(timelabel),5)]) + 
    labs(title = paste0("wait time trend for service ", service," at ", office, " office")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "arrival time") + 
    labs(y = "expected wait time") +
    geom_segment(aes(x = time_index, y = 0, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = 0, y = expect_wait, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
    geom_segment(aes(x = time_index + 5, y = expect_wait + 2, xend = time_index - 0.3, yend = expect_wait + 0.2),
                 arrow = arrow(length = unit(0.5, "cm")), color = "blue") +
    annotate("text", x = 3, y = expect_wait + 0.8, label= paste0(round(expect_wait, digits = 1), " minutes")) + 
    annotate("text", x = time_index, y = -0.8, label= paste0(arrival_time))
  p
  return(p)
}




choose_office <- function(office, service, day, arrival_time){
  result <- list()
  
  arrival_time_seconds <- hms.to.seconds(arrival_time)+1-hms.to.seconds("08:30:00")
  time_index <- ceiling(arrival_time_seconds/300)
  
  wait_time_list <- c()
  for (i in c(1:length(office))){
    
    subset_idx <- which(mvadata$office == office[i] & mvadata$service == service & mvadata$day == day) 
    subset_data <- mvadata[subset_idx,]
    if (nrow(subset_data)==0){
      expect_wait <- NA
    }else{
      model.lo <- loess(wait_time ~ index, subset_data,span=0.2)
      max_idx <- max(subset_data$index)
      min_idx <- min(subset_data$index)
      prediction_curve <- predict(model.lo, data.frame(index = seq(min_idx, max_idx, 0.05)), se = TRUE)
      
      expect_wait <- predict(model.lo, data.frame(index = time_index[i]), se = TRUE)$fit
      if (!is.na(expect_wait) & expect_wait < 0){expect_wait <- 0}
    }
    
    wait_time_list <- c(wait_time_list, expect_wait)
  }
  
  service_starttime_seconds <- arrival_time_seconds + wait_time_list * 60
  if (which(!is.na(service_starttime_seconds)) %>% length() == 0){
    result <- NA
  }else{
    choose_idx <- which(service_starttime_seconds == min(service_starttime_seconds[!is.na(service_starttime_seconds)]))
    if (service_starttime_seconds[choose_idx] < hms.to.seconds("16:30:00") + 1 - hms.to.seconds("08:30:00")){
      result[[1]] <- office[choose_idx] ## predicted office to go
      result[[2]] <- time_index[choose_idx] ## office (predicted) arrival time index
      result[[3]] <- arrival_time[choose_idx] ## office (predicted) actual arrival time
      result[[4]] <- wait_time_list[choose_idx] ## expected wait time in the predicted office
      result[[5]] <- seconds.to.hms(service_starttime_seconds[choose_idx] + hms.to.seconds("08:30:00")) ## expected time to start the service in that office
      result[[6]] <- office ## print all offices
      result[[7]] <- arrival_time ## arrival time (corresponding to the order of the previous office list)
      result[[8]] <- wait_time_list ## wait time (corresponding to the order of the previous office list)
      result[[9]] <- seconds.to.hms(service_starttime_seconds + hms.to.seconds("08:30:00")) ## service start time (corresponding to the order of the previous office list)
    }else{result <- NA}
  }
  return(result)
}

#######################################################################
## function outputs testing

# #output.data <- feiyang()
# load("sample_output.RData")
# office <- levels(droplevels(output.data$office))
# service <- "LearnersPermit"
# day <- output.data$day[1] 
# arrival_time <- output.data$arr.time
# 
# predict_result <- choose_office(office, service, day, arrival_time)
# print(predict_result)
# 
# #shiny_output <- output.data[output.data$office == predict_result[[1]],]
# #shiny_output
# #shiny_output$est.time %>% seconds_to_period()
# 
# office <- predict_result[[1]]
# time_index <- predict_result[[2]]
# arrival_time <- predict_result[[3]]
# timelabel <- create_time_index()
# graph <- wait_time_trend(office, service, day, time_index, arrival_time, timelabel)
# print(graph)

#output.data <- feiyang()
# load("sample_output.RData")
# office <- levels(droplevels(output.data$office))
# service <- "LearnersPermit"
# day <- output.data$day[1] 
# arrival_time <- output.data$arr.time
# 
# predict_result <- choose_office(office, service, day, arrival_time)
# print(predict_result)

#shiny_output <- output.data[output.data$office == predict_result[[1]],]
#shiny_output
#shiny_output$est.time %>% seconds_to_period()

# office <- predict_result[[1]]
# time_index <- predict_result[[2]]
# arrival_time <- predict_result[[3]]
# timelabel <- create_time_index()
# graph <- wait_time_trend(office, service, day, time_index, arrival_time, timelabel)
# print(graph)