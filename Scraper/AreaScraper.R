library("rvest")
library("magrittr")
library("jsonlite")


###### Set working directory
FOLDER <- "/Users/tiko/R_Programs/Crime_Rate_Scraper"
setwd(FOLDER)


### Reads html 
tempHTML <- read_html("http://www.usa.com/rank/california-state--land-area--city-rank.htm") 

### Saves html file
tempHTML %>%  
  write_html("AreaTables/AreaTable.html")

### reads html file gets table and saves as .csv
read_html("AreaTables/AreaTable.html")%>%
  html_nodes("#hcontent")%>%
  html_table() %>%
  as.data.frame() %>%
  write.csv(paste0("AreaTables/AreaTable.csv"))

