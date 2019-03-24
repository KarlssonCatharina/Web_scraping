
library(magrittr)
install.packages('rvest')
library(rvest)


x <- c(0,4,8,12,16,20)

#using the transpose function and cbind
t(cbind(x,x))

#doing the same thing using the magritter function to pipe through x
x %>%cbind(.,x) %>%t()

#now specify the URL we want to scrape
URL <- "https://scistarter.com/finder?phrase=&lat=&lng=&activity=At%20the%20beach&topic=&search_filters=&search_audience=&page=1#view-projects"

#load the web page into R
scistarter_html <- read_html(URL)
scistarter_html

#grab the nodes that have links in them
scistarter_html %>%
  html_nodes("a") %>%
  head()

#check all of the divs
scistarter_html %>%
  html_nodes("div") %>%
  head()

#by div class
scistarter_html %>%
  html_nodes("div.nav-tools") %>%
  head()

#calling the nodes by ID
scistarter_html %>%
  html_nodes("div#project-listing") %>%
  head()

#call all of the tables
scistarter_html %>%
  html_nodes("table") %>%
  head()

#combine rvest and the piping function
scistarter_html %>%
  html_nodes("div#project-listing") %>%
  html_nodes("table") %>%
  html_table() %>%
  "["(1) %>% str()

#
scistarter_html %>%
  html_nodes("div#project-listing") %>% #filter to the projec-listing div
  html_nodes("h3") # filter the tables in the project-listing div 

scistarter_html %>%
  html_nodes("div#project-listing") %>% #filter to the projec-listing div
  html_nodes("h3") %>%                  # get the headings
  html_text() %>%                       #get the text, not the HTML tags
  gsub("^\\s+|\\s+$", "", .)            #strip the white space from the beginning and end of a string.

#when we highlight one of the "goals" on the webpage and "inspect element" we get told that the start of this tag starts with "td"
scistarter_html %>%
  html_nodes("td") %>% #grab the <td> tags
  html_text() %>% # isolate the text from the html tages
  gsub("^\\s+|\\s+$", "", .) %>% #strip the white space from the beginning and end of a string.
  head(n=12) # take a peek at the first 12 records

page_list <- scistarter_html %>%
  html_nodes("td") %>%
  html_text() %>%
  gsub("^\\s+|\\s+$", "", .) #strip the white space from the beginning and end of a string.

#the last list have a pattern where we have scraped and every third column is either a task, goal or a location so now we can try and separate it into columns
goals <- page_list[seq(from=1, to=30,by=3)] # make a sequence to select the goals
task <- page_list[seq(from=2, to=30,by=3)]
location <- page_list[seq(from=3, to=30,by=3)]

title <- scistarter_html %>%
  html_nodes("div#project-listing") %>% #filter to the projec-listing div
  html_nodes("h3") %>%                  # get the headings
  html_text() %>%                       #get the text, not the HTML tags
  gsub("^\\s+|\\s+$", "", .) 

scistarter_df <- data.frame(title, goals, task, location)

#now we can write a loop to scrape multiple pages at once
pages <- ceiling(832/10) # number of pages to go through
sci_df <- data.frame()

for (page in (1:pages)) { #Uncomment this if you want all the pages.
#for (page in (1:5)) {
  
  
  print(paste0("geting data for page: " , page ))
  URL <- paste0("https://scistarter.com/finder?phrase=&lat=&lng=&activity=&topic=&search_filters=&search_audience=&page=", page, "#view-projects")
  
  sci_html <- read_html(URL)
  page_list <- sci_html %>%
    html_nodes("td") %>%
    html_text() %>%
    gsub("^\\s+|\\s+$", "", .) #strip the white space from the beginning and end of a string.
  
  goal <- page_list[seq(from=1, to=30,by=3)]
  task <- page_list[seq(from=2, to=30,by=3)]
  location <- page_list[seq(from=3, to=30,by=3)]
  
  title <- sci_html %>%
    html_nodes("div#project-listing") %>% #filter to the projec-listing div
    html_nodes("h3") %>% # get the headings
    html_text() %>% #get the text, not the HTML tags
    gsub("^\\s+|\\s+$", "", .) #strip the white space from the beginning and end of a string.
  
  tmp <- data.frame(title, goal, task, location)
  if (pages == 1 ) {
    sci_df <- data.frame(tmp)
  } else {
    sci_df <- rbind(sci_df, tmp)  
  }
}

####################################################################################
# To scrape a table from a website, the html_table() function can be a game-changer.
# But it doesn't give us the right output right away. 
URL2 <- "https://www.nis.gov.kh/cpi/Apr14.html"

# TIP: When debugging or building your scraper, assign a variable to the raw HTML.
# That way you only have to read it once
accounts <- read_html(URL2) 

table <- accounts %>%
  html_nodes("table") %>%
  html_table(header=T)

# You can clean up the table with the following code, or something like it. 
# table[[1]]
dict <- table[[1]][,1:2]
accounts_df <- table[[1]][6:18,-1]

names <- c('id', 'weight.pct', 'jan.2013', 'dec.2013', 'jan.2014', 'mo.pctch', 'yr.pctch', 'mo.cont', 'yr.cont')
colnames(accounts_df) <- names

accounts_df #%>% str()

#################################################
#scraping using xpath
library(lubridate)

URL <- "https://www.wunderground.com/history/airport/WSSS/2016/1/1/DailyHistory.html?req_city=Singapore&req_statename=Singapore"

raw <- read_html(URL)

max <- raw %>% 
  html_nodes(xpath='//*[@id="historyTable"]/tbody/tr[3]/td[2]/span/span[1]')  %>%
  html_text() %>% as.numeric()
min <- raw %>%
  html_nodes(xpath='//*[@id="historyTable"]/tbody/tr[4]/td[2]/span/span[1]') %>%
  html_text() %>% as.numeric()
date <- ymd(paste("2016","1","1", sep="/"))

record <- data.frame(date, min, max)

record













