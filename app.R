Sys.setenv(TZ = "EST")

library(shiny)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Maryland MVA Recommendation"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    
    sidebarPanel(
      textInput(inputId="id.street", label="Street", value = "1 W Pratt St"),
      textInput(inputId="id.city", label="City", value = "Baltimore"),
      textInput(inputId="id.zipcode", label="Zipcode", value = "21201"),
      selectInput(inputId = "id.travel.method",
                  label = "Select Mode of Travel",
                  choices = c("Driving","Walking","Bicycling","Transit")),
      selectInput(inputId = "id.visit.reason",
                  label = "Select Service",
                  choices = c("Driver License Renewal", "Insurance Compliance Division", "Learners Permit",
                              "Other Drivers Services", "Other Vehicle Services", "Registration Renewal", 
                              "Tag Return","Title")),
      textInput(inputId = "id.date", label="Date of Visit", value = "2018-01-01"),
      
      
      uiOutput('slider1'),
      # sliderInput(inputId = "id.time",
      #             label = "Select Time",
      #             min = as.POSIXct("08:30:00", format = "%H:%M:%S"),
      #             max = as.POSIXct("16:30:00", format = "%H:%M:%S"),
      #             value = as.POSIXct("09:00:00", format = "%H:%M:%S"),
      #             step = 300,
      #             timeFormat = "%H:%M:%S", ticks = F, animate = T),
      submitButton("Update")
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Instructions",
                           "This Shiny app takes a users location and a specific time and date and lists which Maryland MVA office  is best for them to visit to take care of their business as early as possible. This app incorporates travel distance and time between the users location and MVA offices and wait times for services offered at each of the MVA offices.",
                           h3("Methods"),
                           em("Data"),
                           br(),
                           br(),
                           "The MVA of Maryland posts wait times every 5 minutes while the locations are open. Using R, we scraped wait times for each service at each MVA office for 4 weeks from November 9, 2017 to December 6, 2017. Data scraped on holidays (Veteran's Day: November 10-11, 2017 and Thanksgiving day: November 23), were excluded from prediction modeling.",
                           br(),
                           br(),
                           em("Prediction Model"),
                           br(),
                           br(),
                           "The prediction model takes in user's address, expected leaving date and time, and gives user the suggested MVA office to go where user can start his/her service first compared to other MVA offices. We use the wait times at each MVA office between November 9, 2017 and December 6, 2017 as our training data in a local polynomial regression and makes predictions on expected wait time for future dates at different time slots. Our App takes account of actual travel time from user's location to MVA offices using Google Map API, predicts wait time at each MVA office for the planned service based on user's expected arrival time, and then reports the expected service start time at the suggested MVA office where both travel time and wait time are minimized. Note that our App only reports the suggested MVA office to user when the user's expected service start time is before the office closes.",
                           h3("Instructions"),
                           "1. Please type in your street address, city, and zip code (user location/departure address)",
                           br(),
                           "2. Please select your mode of transportation and service you would like to get at an MVA office.",
                           br(),
                           "3. Please type in the date of visit in the format of yyyy-mm-dd.",
                           br(),
                           "4. Please select the time of departure.",
                           br(),
                           "5. Click 'Update' and wait for a few seconds to process your request.",
                           h3("Notes"),
                           "According to the Maryland MVA website (http://www.mva.maryland.gov/sebin/customerwaittimes/), the MVA's busiest days are at the end of each month and the first days of the next month, so MVA strongly recommends that you plan your visit during the middle of the month. In addition, MVA expect high volume days on Mondays, Fridays, and Saturdays, so MVA recommends that you plan your visit on a Tuesday, Wednesday, or Thursday to minimize your time at MVA.",
                           h3("Contributors to this App"),
                           "Su Jin Lim, Siruo Wang, Yeya Zheng, Jing Li, Chih-Kai Chang and Feiyang Zheng",
                           br(),
                           "This app was created for the Advanced Data Science II class (2nd term, 2017) at the Johns Hopkins Bloomberg School of Public Health",
                           br(),
                           a("Github Link", href = "https://github.com/adv-datasci/mva/")),
                  tabPanel("Recommended MVA Office", 
                           h3("Our Recommendation"), 
                           textOutput("office"),
                           leafletOutput(outputId = "id.distPlot1")),
                  tabPanel("Travel distance and time",
                           h3("Estimated Travel Time and Distance From Your Location to MVA Offices"),
                           DT::dataTableOutput("gmapresultTable")),
                  tabPanel("Wait Time Trend",
                           h3("Wait Time Trend at the Recommended Office"),
                           plotOutput(outputId = "id.distPlot2")))
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output){
  
  setwd("C:/Users/yzheng/Downloads/mva")
  library(leaflet)
  library(DT)
  library(maps)
  library(plyr)
  library(dplyr)
  library(ggplot2)
  library(gmapsdistance)
  library(ggmap)
  library(ggalt)
  library(kimisc)
  library(lubridate)
  library(udunits2)
  geo.office = read.csv("./mva data.csv")
  des<-paste(geo.office$lat , geo.office$lon,sep = "+")
  
  ########################
  ## Take user input
  ########################
  
  source("distance.R")
  source("prediction.R")
  
  output$slider1 <- renderUI({
    slider2.value <- format(as.Date(input$id.date),"%A")
    if (slider2.value=="Sunday"){
      sliderInput("id.time","MVA is close on Sunday", 
                  min = as.POSIXct("08:30:00", format = "%H:%M:%S"), 
                  max = as.POSIXct("08:30:00", format = "%H:%M:%S"),
                  value=as.POSIXct("08:30:00", format = "%H:%M:%S"),
                  step=300,timeFormat = "%H:%M:%S", ticks = F, animate = T)
    } else if(slider2.value=="Saturday"){
      sliderInput("id.time","Select Time",
                  min = as.POSIXct("08:30:00", format = "%H:%M:%S"),
                  max=as.POSIXct("12:00:00", format = "%H:%M:%S"),
                  value=as.POSIXct("09:00:00", format = "%H:%M:%S"),
                  step=300,timeFormat = "%H:%M:%S", ticks = F, animate = T)
    } else{sliderInput("id.time","Select Time", 
                       min = as.POSIXct("08:30:00", format = "%H:%M:%S"), 
                       max = as.POSIXct("16:30:00", format = "%H:%M:%S"),
                       value=as.POSIXct("09:00:00", format = "%H:%M:%S"),
                       step=300,timeFormat = "%H:%M:%S", ticks = F, animate = T)
    }
    
  })
  
  ## 1. obtain user's location in plain english
  user.address = reactive({
    paste(paste(input$id.street ,input$id.city,"MD",sep=", "),input$id.zipcode)
  })
  
  
  ## 2. obtain longtidue and latitude of user's location
  user.address.cord = reactive({
    register_google(key="AIzaSyBucAhtOn805M1SKeSgN-81dnwKqFN5eSE")
    x = try({ # try google geocode first
      geocode(user.address(), source = "google")
    }, silent = TRUE)
    if(inherits(x, "try-error")){ 
      x = try({ # try dsk geocode if google geocode fails
        geocode(user.address(), source = "dsk")
      }, silent = TRUE)
    } 
    if(inherits(x, "try-error")){ # if dsk geocode fails, return NULL
      cat("coordiates not found")
      return(NULL)
    } else {
      return(x)
    }
  })
  
  
  ## 3. alculate travel time and distance from the user location to MVA offices
  output.data = reactive({
    # we need to have valid longitde and latitude to proceed, otherwise we output the following error message
    validate(
      need(try(sum(is.na(user.address.cord())) == 0), "Retry in 5 seconds - Coordinates not found!")
    )
    validate(
      need(try(format(as.Date(input$id.date),"%A")!="Sunday"), "MVA office is closed on Sunday!")
    )
    # we need to have future departure date, otherwise we output the following error message
    validate(
      need(try(as.POSIXct(paste(as.Date(input$id.date), input$id.time), tz = "EST") >= Sys.time()), "The departure time has to be some time in the future!")
    )
    
    ori <- gsub(" ", "+", user.address())
    disinfor <- try({gmapsdistance(origin = ori, destination = des %>% as.vector(),
                                   mode = tolower(input$id.travel.method), 
                                   dep_date = as.character((as.Date(input$id.date)+7)), 
                                   dep_time = as.character(gsub("^.* ","", input$id.time)), 
                                   shape = "long",key="AIzaSyBucAhtOn805M1SKeSgN-81dnwKqFN5eSE")}, silent = TRUE)
    if(inherits(disinfor, "try-error")){
      cat("fail to use the Google Maps Distance Matrix API to compute the distances and times between your location and MVA offices.")
      disinfor = NULL 
    } 
    # we need to have valid travel distance and times, otherwise we output the following error message
    validate(
      need(try(is.null(disinfor) == FALSE), "Retry in 15 seconds - Travel distances and times are not computed!")
    )
    
    output.data <- geo.office$Name %>% as.data.frame()
    colnames(output.data)[1] <- "office"
    output.data$est.time <- disinfor$Time[[3]]/60 %>% round(.,0)
    output.data$dis <- udunits2::ud.convert(disinfor$Distance[[3]], "m", "km") %>% round(.,1)
    arr.time <- seconds_to_period(disinfor$Time[[3]]+period_to_seconds(hms(as.character(gsub("^.* ","", input$id.time)))))
    output.data$arr.time <- sprintf("%02i:%02i:%02i", hour(arr.time), minute(arr.time),second(arr.time))
    output.data$day <- format(as.Date(input$id.date), "%A")
    output.data
  })
  
  
  ## 4. incorporate travel time, wait time and choose the office that gives the earliest service start time
  predict_result0 = reactive({
    output.data = output.data()
    #office <- levels(droplevels(output.data$office)) <- The order of office is different from output.data$office
    office <- output.data$office %>% as.character()
    service = gsub(pattern=" ",replacement="",input$id.visit.reason)
    day = output.data$day[1]
    arrival_time <- gsub("^.* ","", as.POSIXct(output.data$arr.time, format = "%H:%M:%S") - hours(5))
    
    predict_result <- choose_office(office, service, day, arrival_time)
    
    # we need to have office
    validate(
      need(try(sum(is.na(predict_result)) == 0), 
           "You depart too late, so MVA will be closed by the time you receive the service.Or, the service is not available at any office by the time you arrive.")
    )
    predict_result
  })
  
  predict_result = reactive({
    predict_result = predict_result0()
    predict_result = predict_result[c(1:5)]
    
    # the default time zone of shinyapps.io is UTC, which is 5 hours ahead of EST
    predict_result <- as.data.frame(t(ldply(predict_result))) %>% 
      mutate(V2 = as.numeric(as.character(V2)),
             V3 = gsub("^.* ","", as.POSIXct(V3, format = "%H:%M:%S")),
             V4 = round(as.numeric(as.character(V4)),2),
             V5 = gsub("^.* ","", as.POSIXct(V5, format = "%H:%M:%S")))
    #colnames(predict_result)=c("Office","Arrival Time","Waiting Time","Service Time")
    rownames(predict_result) = NULL
    predict_result
  })
  
  
  ########################
  ## Output results
  ########################
  
  ## 5. output text
  output$office <- renderText({
    predict_result <- predict_result()
    paste("We recommend you to go to", predict_result$V1, "office to get", input$id.visit.reason,
          "service. You are expected to arrive at the office at", predict_result$V3,
          ", wait approximately", predict_result$V4, "minutes, and get the service at", predict_result$V5)
  })
  
  ## 6. output map
  output$id.distPlot1 <- renderLeaflet({
    
    # define variables 
    a <- predict_result()
    a = filter(geo.office, Name == as.character(a$V1))
    b = user.address()
    c = user.address.cord()
    
    # generate map
    maryland = map("county","maryland",fill=TRUE, plot=FALSE)
    leaflet(data = maryland) %>%
      #addTiles() %>%
      addTiles() %>%
      addPolygons(fillColor = topo.colors(10, alpha = 0.2), stroke = TRUE, color = "blue", weight = 2) %>%
      # set default view to Baltimore area
      # setView(lng = -77.03687 , lat = 38.90719, zoom = 8) %>%
      fitBounds(lng1 = a$lon, lat1 = a$lat, lng2 = c$lon, lat2 = c$lat) %>%
      addAwesomeMarkers(lng = a$lon, lat = a$lat, label = paste(a$Name, "office:", a$Address),
                        icon = makeAwesomeIcon(icon = "flag", markerColor = "black", iconColor = "white"),
                        labelOptions = labelOptions(noHide = T)) %>%
      addAwesomeMarkers(lng = c$lon, lat = c$lat, label = paste("Your Location :", b),
                        icon = makeAwesomeIcon(icon = "home", markerColor = "red", iconColor = "white"),
                        labelOptions = labelOptions(noHide = T))
  })
  
  
  ## 7. output table
  output$gmapresultTable <- DT::renderDataTable({
    
    a = merge(output.data(), geo.office, by.x = "office", by.y = "Name")
    a = a %>%
      select(office, Address, est.time, dis, arr.time) %>%
      mutate(est.time = round(est.time, 2),
             dis = round(dis, 2),
             arr.time = gsub("^.* ","", as.POSIXct(arr.time, format = "%H:%M:%S") - hours(5))) %>%arrange(arr.time)%>%
      setNames(c("Office","Address","Travel Time (min)","Distance (km)","Arrival Time"))
    b = predict_result0()
    b = b[c(6:9)] %>%
      ldply() %>% t() %>% as.data.frame() %>% 
      mutate(V3 = round(as.numeric(as.character(V3)),2)) %>% 
      select(V1, V3, V4) %>%
      setNames(c("Office","Wait Time (min)","Service Start Time"))
    c = merge(a, b, by="Office")
    c = c[order(c$`Service Start Time`),]
    DT::datatable(c)
  })
  
  output$approvalBox <- renderValueBox({
    valueBox(
      "80%", "Approval", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  
  
  ## 8. output wait time trend
  timelabel <- create_time_index()
  output$id.distPlot2 <- renderPlot({
    
    # define variables
    a <- predict_result()
    b <- output.data()
    office <- a$V1 %>% as.character()
    time_index <- a$V2 %>% as.character() %>% as.numeric()
    arrival_time <- a$V3
    service <- gsub(pattern = " ", replacement = "", x = input$id.visit.reason)
    
    # subset data based on office, service and day
    load("mvadata.rda")
    subset_idx <- which(mvadata$office == office & 
                          mvadata$service == gsub(pattern = " ", replacement = "", x = input$id.visit.reason) & 
                          mvadata$day == b$day[1])
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
    
    ggplot(data = subset_dataframe, aes(x=timepoints, y=predict_wait)) + 
      geom_line(size = 1) + theme_bw() +
      scale_x_continuous(breaks = seq(1,length(timelabel),5), labels = timelabel[seq(1,length(timelabel),5)]) +
      labs(title = paste0("Waiting Time Trend for Service ", service," at ", office, " office")) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x = "Arrival Time") + 
      labs(y = "Expected Wait Time (minutes)") +
      geom_segment(aes(x = time_index, y = 0, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
      geom_segment(aes(x = 0, y = expect_wait, xend = time_index, yend = expect_wait), linetype="dotted", color = "blue", size=1) +
      geom_segment(aes(x = time_index + 5, y = expect_wait + 2, xend = time_index - 0.3, yend = expect_wait + 0.2),
                   arrow = arrow(length = unit(0.5, "cm")), color = "red") +
      annotate("text", x = 3, y = expect_wait + 0.8, label= paste0(round(expect_wait, digits = 2), " minutes")) + 
      annotate("text", x = time_index, y = -0.8, label= paste0(arrival_time))
  })
  
}



## Run the application 
shinyApp(ui = ui, server = server)