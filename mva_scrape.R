library(rvest)
library(dplyr)
library(lubridate)

# setwd("~/Documents/datascience/mva_datascience/")
# gmapdistance packagre to calculate distence using google map

## request today's date
today <- Sys.Date()
## last date to collect data
lastday <- ymd("2017-12-07")
## county code to be put in the url 
county_code <- c("garret","allega","washin","freder","carrol",
                 "baltco","baltci","harfor","cecilc","montgo",
                 "howard","annear","prince","charle", "calver",
                 "stmary","talbot","wicomi")

while (today < lastday){
  ## MVA service time is between 8:30am to 4:30pm, Monday to Saturday
  opentime <- ymd_hms(paste0(today," 8:30:00"), tz = "EST")
  closetime <- ymd_hms(paste0(today, " 16:30:00"), tz = "EST")
  now <- Sys.time()
  
  while (now > opentime & now < closetime ){
    
    for (i in c(1:length(county_code))){
      url <- paste0("http://www.mva.maryland.gov/sebin/customerwaittimes/",county_code[i],".htm")
      
      ## CSS node "p" points to contents on the webpage
      full_content <- read_html(url) %>% 
        html_nodes("p") %>%
        html_text() %>% 
        gsub("\r\n","",.)
      
      full_content <- full_content[full_content != ""]
      
      ## second line of webpage contents (cleaned up version) are mva office names in one county
      tempnodes <- full_content[2]
      tempnodes
      
      ## separate mva office names to retrieve CSS node for each office
      if (grepl("Closed", tempnodes)) {
        office <- tempnodes %>% 
          tolower() %>% 
          strsplit(" ")
        office <- office[[1]][1]
        
        write.table(data.frame(tempnodes,now), file = paste0(office,".txt"), 
                    append = TRUE, col.names = FALSE, row.names = FALSE)
        run <- 0
      } else if (grepl("\\|", tempnodes)) {
        tempnodes <- tempnodes %>% 
          gsub(" ","-",.) %>% 
          tolower() 
        css_nodes <- strsplit(tempnodes, "-\\|\\-")[[1]]
        run <- 1
      } else {
        css_nodes <- "content td"
        run <- 1
      }
      
      if (run == 1) {
        ## add for loop here to loop over css_nodes for multiple offices under the same county
        ## scrape data for each mva office by its specific CSS node
        for (j in c(1:length(css_nodes))){
          
          data <- read_html(url) %>% 
            html_nodes(paste0("#", css_nodes[j])) %>% 
            html_text()
          
          text <- strsplit(data,"\r\n")[[1]] %>% 
            gsub(" ", "", .) %>% 
            gsub("\t","",.) 
          
          text <- text[text != ""]
          
          office <- text[1] 
          service_all <- c("DriverLicenseRenewal", "InsuranceComplianceDivision", "LearnersPermit", 
                           "Miscellaneous", "OtherDriversServices", "OtherVehicleServices",
                           "RegistrationRenewal", "TagReturn", "Title")
          
          service <- service_all[which(service_all %in% text)]
          num_of_people <- text[grep("minutes",text)-1] %>% as.numeric()
          wait_time <- text[grep("minutes",text)] %>% gsub("minutes","",.) %>% as.numeric()
          scrape_time <- rep(now,length(service))
          
          write.table(data.frame(service,num_of_people,wait_time, scrape_time), file = paste0(office,".txt"), 
                      append = TRUE, col.names = FALSE, row.names = FALSE)
        } # end for loop for looping over css_nodes
        
      } # end if (run == 1)
      
    } # end for loop for looping over country_code
    
    Sys.sleep(300)
    now <- Sys.time()
    
  } # end inner while loop to scrape data daily within opening hours
  
  Sys.sleep(300)
  today <- Sys.Date()
} # end outter while loop to scrape data until December


# echo R CMD BATCH mva_scrape.R| qsub -N mva -cwd -l mf=2G,h_vmem=2G

