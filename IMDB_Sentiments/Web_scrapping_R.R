
### Web scrapping using R
library(rvest)
read_html('https://en.wikipedia.org/wiki/Table_(information)') %>%
  # then extract the first node with class of wikitable
  html_node('.wikitable') %>%
  # then convert the HTML table into a data frame.
  html_table()# only worls with nicely formatted HTML tables.


# What about non-table data ? -> SelectorGadget + rvest to rescue
# Extract links to download reports.
domain <- 'http://www.sec.gov'
susp <- file.path(domain, 'litigation/suspensions.shtml')
hrefs <- read_html(susp) %>% 
  html_nodes('p+ table a') %>%html_attr(name='href')
head(hrefs)


hrefs <- hrefs[!is.na(hrefs)]
pdfs <- paste0(domain, hrefs)
mapply(download.file, pdfs, basename(pdfs))


# What about dynamic Web pages ?
# First, note on Web APIs -> With 'httr' packages
library(httr)
me <- GET('https://api.github.com/users/sunnysingh30')
content(me)[c('name', 'company','html_url','location','created_at')]

# Scraping Dynamic pages -> rvest to rescue
library(rvest)
read_html('http://www.techstars.com/companies/stats') %>%
  html_nodes(".results_section") %>%
  html_table()
# Above will not work or generate error. browser parse HTML doc and constructs the DOM. package 'rdom' can return the DOM as HTML.

# devtools::install_github('cpsievert/rdom')
install.packages('devtools')
library(devtools)
install_github('cpsievert/rdom')

library(rdom)
install.packages('phantomjs')
rdom('http://www.techstars.com/companies/stats/') %>%
  html_node('.table75') %>%
  html_table()


## What about non-HTML data
- HTML :: great for sharing content between people, but ist great 
for exchanging data between machines.
- Most popular ways to exchange data over the web, but by far the most popular onesare JSON and XML. 
## 
install.packages('XML2R')
library(XML2R) # coerce XML into flat list of observations. 
obs <- XML2Obs('https://gist.githubusercontent.com/sunnysingh30/d049d396fa1a46c7e4b639486dcaade7/raw/fa1c39e72f51f3fcca295e582c01a47474b19232/sunnykart.xml')
table(names(obs))
collapse_obs(obs) # Aggregates similar names objects into common table. But you have lost mapping between them.

obs <- add_key(obs, parent='sunnykart//driver', recycle='name')
collapse_obs(obs)
# Now, if want I can merge the tables into single table..
tabs <- collapse_obs(obs)
tabs <- merge(tabs[[1]], tabs[[2]], by='name')
tabs[,-c(3,7)]


## Wat about JSON ?
JSON is quickl becoming the format for the data on the web.
Its comprised of 2 components:
  arrays : [value1, value2]
  objects : {'key1':value1, 'key2':[vallue2, value3]}
##
The preferred R package for R <--> JSON conversion has been 'RJSONIO'. However 'jsonlite' gaining lot of momentum/attention.
##
library(RJSONIO)
library(jsonlite)
mario <- fromJSON('http://bit.ly/mario-json')
str(mario) # nested data frames ? 
mario$vehicles
# How do we get 2 tables, with common id, like in XML example.
#THis maply statement is essentiall equivalent to add_key
vehicles <- mapply(function(x, y) cbind(x, driver = y),
                   mario$vehicles, mario$driver, SIMPLIFY = F)
Reduce(rbind, vehicles)
mario[!grepl('vehicles', names(mario))]

####################################################################################################
####################################################################################################
####################################################################################################
####################################################################################################
.rs.restartR()
sessionInfo()
library(rvest)
#install.packages("rvest")
#install.packages('PogromcyDanych')
library(PogromcyDanych)

#lego_movie <- read_html('http://www.imdb.com/title/tt1490017')
#lego_reviews <- read_html('http://www.imdb.com/title/tt1490017/reviews?ref_=tt_urv')
#Scrape the website for the movie rating
rm(list = ls())

serialsToParse <- levels(serialeIMDB$imdbId)
# prepare matrix for results
demograph_bd = matrix("", length(serialsToParse), 14)
rownames(demograph_bd) = serialsToParse
colnames(demograph_bd) = c("Males", "Females", "Aged under 18", "Males under 18", 
                           "Females under 18", "Aged 18-29", "Males Aged 18-29", "Females Aged 18-29", 
                           "Aged 30-44", "Males Aged 30-44", "Females Aged 30-44", "Aged 45+", 
                           "Males Aged 45+", "Females Aged 45+")

head(demograph_bd)
# for all series
for (serials in serialsToParse) {
  page = read_html(paste0("http://www.imdb.com/title/",serials,"/ratings")) 
  nodes3 = html_nodes(page, "table:nth-child(11) td:nth-child(3)")
  demograph_bd[serials,] = gsub(html_text(nodes3)[-1], pattern="[^0-9.]", replacement="")[1:14]
}

# Remove those serials which does not have any ratings for all parameters
head(demograph_bd)
demograph_bd_new <- as.data.frame(demograph_bd)
demograph_bd_new <- demograph_bd_new[rowSums(is.na(demograph_bd_new))!=14,]

# Add serial id & serial_name as new columns
demograph_bd_new <- cbind(serial_id = rownames(demograph_bd_new),serial_name="", demograph_bd_new)
head(demograph_bd_new)

serialsToParse <- as.vector.factor(demograph_bd_new[['serial_id']])
class(as.vector.factor(demograph_bd_new[['serial_id']]))
demograph_bd_new['serial_name'] <- NA

# Get all the serial names
library(stringr)
for (serials in serialsToParse){
  title_page <- read_html(paste0('http://www.imdb.com/title/',serials))
  serials_name <- title_page %>% html_nodes('h1') %>% html_text()
  demograph_bd_new['serial_name'][serials,] <- gsub(serials_name, pattern="[[:blank:]]*$", replacement = "")
}

getwd()
setwd('/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies')
write.csv(demograph_bd_new, 'demograp_bd_new.csv', row.names = T)

# Get all the reviews 
for (reviews in serialsToParse){
  review_page <- read_html(paste0('http://www.imdb.com/'))
}

library(data.table)
reviews_df <- data.table()
review_page <- read_html(paste0('http://www.imdb.com/title/', 'tt0903747','/reviews'))
review_head <- review_page %>% html_nodes('h2') %>% html_text()
location <- as.vector(review_page %>% html_nodes('small') %>% html_text())
reviews <- review_page %>% html_nodes('p') %>% html_text()
t4 <- review_page %>% html_nodes('hr+ table a') %>% html_attr('href')
length(reviews)
paste0('http://www.imdb.com/title/','serials','/',t4)
