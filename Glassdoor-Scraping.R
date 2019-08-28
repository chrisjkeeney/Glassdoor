library(httr)  
library(xml2)  
library(rvest)
library(tidyverse)

### Glassdoor cuts job searches after 30 pages. Probablly to prevent scraping 
### One way to get behind this is to do the search by state and limit each
### State by 30 pages maximum, then can get more job pulls than 900

# Glassdoor nodes for pulling specific elements
CSSjobct <- ".jobsCount"
CSSjobtitle <- ".jobHeader+ .jobTitle"
CSScompany <- ".jobEmpolyerName"
CSScitystate <- "#MainCol .loc"
CSSrating <- ".compactStars"
CSSsalary <- ".salaryText"
CSStime <- ".jobLabel.nowrap"
CSSsize <- ".infoEntity:nth-child(2)"

# Pull in text file of Glassdoor Links by state and convert to list
URLstate <- read_tsv("Glassdoor-State-Links.txt", col_names = F)
URLstate <- as.list(URLstate$X1)
URLjob <- paste0(URLstate,".htm")

# Find the number of total jobs per state search
totJobs <- lapply(URLjob, function(i){
  read_html(i) %>%
    html_nodes(".jobsCount") %>%
    html_text() %>%
    gsub("[^0-9]","",.) %>%
    as.integer()
})

# 30 jobs per pages, want to max at 30 total.
totPages <- lapply(totJobs,function(i){as.integer(min(ceiling(i/30),30))})
rm(totJobs)

# Loop along each state link to create the pages link
#URLall <- Map(function(i,j)paste0(i,"_IP",seq(j),".htm"),test2,maxRes)
URLall <- lapply(seq_along(URLstate), function(i) paste0(URLstate[[i]], "_IP", seq_len(totPages[[i]]),".htm"))
URLall <- unlist(URLall)
# Save it just to be safe
saveRDS(URLall,file = "ALL-GlassDoor-Data-Science-URLS.Rds")


### Pull the individual elements into a dataframe
GD_Scraper <-function(x=1:100) {map_df(x, function(i) {
  cat(" P", i, sep = "")
  pg <- read_html(GET(URLall[i]))
  jobTitle <- pg %>% html_nodes(CSSjobtitle) %>% 
    html_text() %>% data.frame(Job_Title = ., stringsAsFactors = F)
  
  company <- pg %>% html_nodes(CSScompany) %>% 
    html_text() %>% data.frame(Company_Name = ., stringsAsFactors = F)
  
  cityState <- pg %>% html_nodes(CSScitystate) %>% 
    html_text() %>% data.frame(City_State = ., stringsAsFactors = F)
  
  rating <- pg %>% html_nodes(".jl") %>% html_node(CSSrating) %>% 
    html_text() %>% data.frame(Company_Rating = ., stringsAsFactors = F)
  
 salary <- pg %>% html_nodes(".jl") %>% html_node(CSSsalary) %>% 
    html_text() %>% data.frame(salary_Range = ., stringsAsFactors = F)
 
 length <- pg %>% html_nodes(".jl") %>% html_node(CSStime) %>% 
   html_text() %>% data.frame(Post_Date = ., stringsAsFactors = F)
 
data.raw<- cbind(jobTitle,company,cityState,rating,salary,length)
}) }

# Do the pings as a batch to prevent errors
data.raw1 <- GD_Scraper(1:100)
data.raw2 <- GD_Scraper(101:200)
data.raw3 <- GD_Scraper(201:300)
data.raw4 <- GD_Scraper(301:400)
data.raw5 <- GD_Scraper(401:500)
data.raw6 <- GD_Scraper(501:length(URLall))

data.total <- data.raw1 %>% 
  rbind(data.raw2,data.raw3,data.raw4,data.raw5,data.raw6)

saveRDS(data.total,"Data-Raw-Total.RDS")
rm(data.raw1,data.raw2,data.raw3,data.raw4,data.raw5,data.raw6)

