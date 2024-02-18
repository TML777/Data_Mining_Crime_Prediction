library("rvest")
library("magrittr")
library("jsonlite")


###### Set working directory
FOLDER <- "/Users/tiko/R_Programs/Crime_Rate_Scrapper"
setwd(FOLDER)


###############
# For 2005, there is a set link
# For 2006-2009 there is a link directly to table
# For 2010-2019 the most similar link between the pages was the "offenses-known-to-law-enforcement" page
#     after that page the algorithm finds table named 8, and opens that link
#     after opening table named 8 it opens the table with the link "ca" or "california"
# For 2016 the table name is "table 6"
###############

for(year in 2005:2019){
  tryCatch({
    
    if(year < 2010){
      
      
      if(year == 2005){
        ### Link to 2005 table
        tableLink <- "https://www2.fbi.gov/ucr/05cius/data/table_08_ca.html"
      }else{
        ### Link to 2006-2009 tables
        tableLink <- paste0("https://www2.fbi.gov/ucr/cius", year, "/data/table_08_ca.html")
      }
      
      ### Reads html finds the table and saves it 
      read_html(tableLink) %>%
        html_nodes("#datatable")%>%
        html_table() %>%
        as.data.frame() %>%
        write.csv(paste0("FBITables/",year,"table.csv"))
      
      Sys.sleep(5)
    }else{
      
      ### Link to "offenses-known-to-law-enforcement" page
      if(year < 2016){
        offensesKnownLink <- paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/offenses-known-to-law-enforcement")
      } else {
        offensesKnownLink <- paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year, "/crime-in-the-u.s.-", year, "/topic-pages/offenses-known-to-law-enforcement")
      }
      
      ### Reading the "offenses-known-to-law-enforcement" page
      read_html(offensesKnownLink) %>%
        html_nodes("#col_b") %>%
        html_nodes("a") %>%
        html_attr("href")->stateLinkz
      
      Sys.sleep(5)
      
      ### finding the link that leads to the Offenses Known to Law Enforcement by State by City page
      ### for 2016 thats table-6 for the rest its ether table-8 or table_8
      if(year == 2016){
        for(stateLink in stateLinkz){
          if(grepl("table-6", stateLink, fixed = TRUE)){
            break
          }
        }
      } else {
        for(stateLink in stateLinkz){
          if(grepl("table-8", stateLink, fixed = TRUE) | grepl("table_8", stateLink, fixed = TRUE)){
            break
          }
        }
      }
      
      
      ### Reading the Offenses Known to Law Enforcement by State by City 
      read_html(stateLink) %>%
        html_nodes("#page_content") %>%
        html_nodes("a") %>%
        html_attr("href")->tableLinkz
      
      Sys.sleep(5)
      
      ### Finding the page that leads to Californians table
      for(tableLink in tableLinkz){
        if(((grepl("ca", tableLink, fixed = TRUE) | grepl("california", tableLink, fixed = TRUE) | grepl("California", tableLink, fixed = TRUE)) & !grepl("declaration", tableLink, fixed = TRUE))){
          break
        }
      }
      
      ### Reading the Californians table and saving it as a .csv file
      read_html(tableLink) %>%
        html_nodes("#table-data-container")%>%
        html_table() %>%
        as.data.frame() %>%
        write.csv(paste0("FBITables/",year,"table.csv"))
      
      Sys.sleep(5)
    }
    
    
  }, error = function(e){
    ### if a website gives an error the year is printed 
    print(paste0(year, " gave an error ", e))
  })
}







# for(year in 2005:2009){
#   
#   paste0("https://ucr.fbi.gov/crime-in-the-u.s/", year) %>%
#     read_html() %>%
#     write_html(paste0("HTML/fbi", year, ".html"))
#   Sys.sleep(5)
# }
