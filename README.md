# MVA-predict-waiting-time

This Shiny app takes a users location and a specific time and date and lists which Maryland MVA office is best for them to visit to take care of their business as early as possible. This app incorporates travel distance and time between the users location and MVA offices and wait times for services offered at each of the MVA offices.

mva_scrape.R contains the code for scraping the raw data from MVA websites and the data folder contains the actual data. clean_raw_data.R includes the code for pre-possessing and cleaning the raw data, and mvadata.csv is the final data we used for Shiny app. Note that the final version of the mva data has also been saved in Rdata format (mvadata.rda).

In order to calculate the distance and travel time between MVA and user’s location, functions in gmapsdistance package are being used. Distance.Time.R contains the code for distance/time calculation and mva data.csv contains the coordinate information for all MVAs.

prediction_model.R has the code for predicting the waiting time and app.R contains the code for the basic version of our Shiny app. The published shiny app could be found at [https://yzheng68.shinyapps.io/mva_prediction/](https://yzheng68.shinyapps.io/mva_prediction/)
