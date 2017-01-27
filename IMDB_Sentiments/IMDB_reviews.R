rm(list=ls())
.rs.restartR()
sessionInfo()
getwd()
library(rvest)
library(PogromcyDanych)
library(stringr)
library(data.table)
#Scrape the website for the movie rating
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
absolute_web_add <- "http://www.imdb.com/title/"
web_add_filter_ratings <- "/ratings"
for (serials in serialsToParse) {
  page = read_html(paste0(absolute_web_add,serials,web_add_filter_ratings)) 
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
for (serials in serialsToParse){
  title_page <- read_html(paste0('http://www.imdb.com/title/',serials))
  serials_name <- title_page %>% html_nodes('h1') %>% html_text()
  demograph_bd_new['serial_name'][serials,] <- gsub(serials_name, pattern="[[:blank:]]*$", replacement = "")
}

#Exporting the demograph_bd_new data frame
setwd('/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies')
list.files()
write.csv(demograph_bd_new, 'demograp_bd_new.csv', row.names = T)

demograph_bd_new <- read.csv('demograp_bd_new.csv')
  head(demograph_bd_new)

serialsToParse <- as.vector(demograph_bd_new[['serial_id']])
serial_name <- as.vector(demograph_bd_new[['serial_name']])
serials <- serialsToParse
length(serialsToParse)
###### Getting Reviews for opinion mining-------------------------------------
-- ----------------------------------------------------------------------------------------------------------------------
# pad and bind table fields
cbindPad <- function(...){
  args <- list(...)
  n <- sapply(args, nrow)
  mx <- max(n)
  pad <- function(x, mx){
    if (nrow(x) < mx){
      nms <- colnames(x)
      padTemp <- matrix(NA, mx-nrow(x), ncol(x))
      colnames(padTemp) <- nms
      if(ncol(x)== 0){
        return(padTemp)
      }else{
        return(rbind(x, padTemp))
      }
    }else{
      return (x)
    }
  }
  rs <- lapply(args, pad, mx)
  return(do.call(cbind, rs))
}

#review_page <- read_html('http://www.imdb.com/title/tt0898266/reviews?filter=prolific&spoiler=hide;start=0')

#review_page <- read_html('http://www.imdb.com/title/tt1358522/reviews?filter=prolific&spoiler=hide')
review_extract_func <- function(review_page, serial_id, serial_name){
      review_page <- read_html(review_page)
      reviews <- as.vector(review_page %>% html_nodes('#tn15content div+ p') %>% html_text())
      reviews_detail <- str_replace_all(reviews, "[[:cntrl:][:punct:]]", replacement = " ")
      review_head <- review_page %>% html_nodes('h2') %>% html_text()
      review_head <- str_replace_all(review_head, "[[:cntrl:][:punct:]]", replacement = " ")
    
      # Getting location & date of the reviews
      extract_ <- as.vector(review_page %>% html_nodes('br+ small , a+ small') %>% html_text())
      
      if(length(extract_) <= 1){
        location <- as.vector(extract_)
        review_date <- as.vector(extract_)
      }else{
        location <- as.vector(extract_[seq(1, length(extract_), 2)])
        review_date <- as.vector(extract_[seq(2, length(extract_), 2)])
      }
      
      reviews_detail_tab <- as.data.table(cbindPad(as.data.frame(review_head), as.data.frame(reviews_detail), as.data.frame(review_date), as.data.frame(location)))
      reviews_detail_tab[,`:=`(serial_name=serial_name, serial_id=serial_id)]
      setcolorder(reviews_detail_tab, c(length(reviews_detail_tab), length(reviews_detail_tab)-1, 1:(length(reviews_detail_tab)-2)))
      return(reviews_detail_tab)
}

navigation_prepare_fun <- function(naviate_link){
  navigation <- paste0(str_extract(string = navigation, pattern = "([a-z]*)([[:punct:]])"), 'filter=prolific&spoiler=hide', 
                       gsub(x=str_extract(navigation, pattern="([[:punct:]])start=[0-9]{1,2}"), pattern = "\\?", replacement = ";"))
  navigate_links <- as.character(paste0('http://www.imdb.com/title/', serial_id,'/',navigation, ""))
  len <- length(navigate_links)-2
  navigate_links <- navigate_links[1:len]
  return(navigate_links)
}

getwd()
setwd('/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies/')
demograph_bd_new <- read.csv('demograp_bd_new.csv')
head(demograph_bd_new)

serialsToParse <- as.vector(demograph_bd_new[['serial_id']])
serial_title <- as.vector(demograph_bd_new[['serial_name']])
serials <- serialsToParse
length(serialsToParse)

# Start of reading 
vector_of_tables <- vector(mode="list")
for(i in 1:length(serials)){
  serial_id <- serials[i]
  serial_name <- serial_title[i]
  cat("Starting for serial:",serial_id)
  cat("\n")
  
  absolute_web_add <- 'http://www.imdb.com/title/'
  web_add_filter <- '/reviews?filter=prolific&spoiler=hide'
  URL_WEB_ADD <- paste0(absolute_web_add, serial_id, web_add_filter)
  
  review_page <- read_html(URL_WEB_ADD)
  cat('\n\nReviewing page::::',URL_WEB_ADD,'\n')
  
  # Preparing Navigation Links:::::::::::
  cat('Navigation path retirieved and prepared::\n')
  navigation <- review_page %>% html_nodes("table:nth-child(6) td+ td a") %>% html_attr("href")
  #Getting reviews head
  review_head <- review_page %>% html_nodes('h2') %>% html_text()
  cat("Head extracted")
  cat('\n')
  
  if(length(navigation) ==0 & length(review_head) != 0){
    vector_of_tables[[i]] <- review_extract_func(URL_WEB_ADD, serial_id,serial_name)
#    if(isTRUE(class(vector_of_tables[i]) == "try-error")){return("Exception in review_extract_function")}
    cat("Review Extracted for page with no navigation::")
    cat('\n')
  }else if(length(navigation) !=0){
    navigate_links <- navigation_prepare_fun(navigation)
    cat('\nNavigation Links prepared:\n')
    
    temp_vector_tab <- vector(mode = "list")
    for(j in 1:length(navigate_links)){
      cat("\n\nNavigating page::", navigate_links[j])
      review_page <- navigate_links[j]
      temp_vector_tab[[j]] <- try(review_extract_func(review_page, serial_id, serial_name))
      cat("Done::::::::::::::::::::::::::")
    }
    cat('Gettting:::::::::::')
    vector_of_tables[[i]] <- rbindlist(temp_vector_tab)
    cat("\nVector of tables created for page with navigation.\n")
    
  }else{
    cat('\n\n\nNavigation lenght and header length is:::',length(navigation), length(review_head))
    cat('\nNo navigation and reviews found for::',paste0(absolute_web_add, serial_id,web_add_filter,'\n'))
    cat("Lookin for next page:\n\n\n")
    next
  }
}

imdb_review_dataset <- rbindlist(vector_of_tables)
write.table(imdb_review_dataset, 'imdb_review_dataset.xls', row.names = F, quote = T, sep = '\t')

#############################################################################################################
#############################################################################################################
rm(list=ls())
.rs.restartR()
# Extracting sentiments
library(twitteR)
library(stringr)
library(ROAuth)
library(RCurl)
library(ggplot2)
library(reshape)
library(tm)
library(RJSONIO)
library(wordcloud)
library(gridExtra)
library(plyr)
#install.packages("shinyIncubator")
library(shinyIncubator)
library(shiny)
library(ggplot2)
library(tidyr)
library(reshape2)

setwd('/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies/')
reviews_ds <- read.table('imdb_review_dataset.xls', header = TRUE)
reviews_ds <- data.table(reviews_ds, key = "serial_id",keep.rownames = F)
head(reviews_ds,50)

# Agregating reviews and review heads by serial_ids & serial_name
aggregated_reviews_by_srs <- reviews_ds[, .(review_head=paste0(review_head, collapse = " "), 
                                            reviews_detail=paste0(reviews_detail, collapse = " ")), by=list(serial_id, serial_name)]
colnames(aggregated_reviews_by_srs)
write.table(aggregated_reviews_by_srs, 'aggregated_reviews_by_srs.xls', row.names = F, quote=T, sep = '\t')
aggregated_reviews_by_srs <- data.table(read.table('aggregated_reviews_by_srs.xls', header = T), key = c("serial_id", "serial_name"))
# Cleaning Reviews and review heads:
# Clean data
cleanSubjects = function(subjects){
  #subject_cl <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",subjects)
  #subject_cl <- gsub("http[^[:blank:]]+", "", subject_cl)
  subject_cl <- gsub("@\\w+", "", subjects)
  #subject_cl <- gsub("[ \t]{2,}", "", subject_cl)
  #subject_cl <- gsub("^\\s+|\\s+$", "", subject_cl)
  subject_cl <- gsub("[[:punct:]]", " ", subject_cl)
  #subject_cl <- gsub("[^[:alnum:]]", " ", subject_cl)
  subject_cl <- gsub('\\d+', '', subject_cl)
  subject_cl <- tolower(subject_cl)
  return(subject_cl)
}

# Scoring the polarity of subjects
sentiment_scale <- function(subjects, vNeg, neg, vPos, pos){
  final_scale <- matrix('',0, 5)
  
  score_scale <- laply(subjects, function(subjects, vNegList, negList, vPosList, posList){
    initial_subject <- subjects
    #cat('initial subject', initial_subject)
    subject <- cleanSubjects(subjects)
    wordlist <- str_split(subject, '\\s+')
    words <- unlist(wordlist)
    # build vector with matches etween subject and each category
    vNegMatches <- match(words, vNegList)
    negMatches <- match(words, negList)
    vPosMatches <- match(words, vPosList)
    posMatches <- match(words, posList)
    #Sum number of words in each category
    vNegMatches <- sum(!is.na(vPosMatches))
    negMatches <- sum(!is.na(negMatches))
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    
    scale_df <- c(vNegMatches, negMatches, vPosMatches, posMatches)
    add_row <- c(initial_subject, scale_df)
    final_scale <- rbind(final_scale, add_row)
    return(final_scale)
  }, vNegList, negList, vPosList, posList)
  return(score_scale)
}


#t <- aggregated_reviews_by_srs[, .(cleanSubjects(review_head))]
#t <- head(aggregated_reviews_by_srs[['review_head']])

#load pos, neg statement
setwd('/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies/Sentimetal_Analysis/twitter/')
afin_list <- read.delim(file='/Users/sunnysingh/Documents/Educational\ Content/Stats_Analytics/Case\ studies/Sentimetal_Analysis/twitter/AFINN-111.txt', header=F, stringsAsFactors=F)
names(afin_list) <- c('word', 'score')

afin_list$word <- tolower(afin_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegList <- afin_list$word[afin_list$score == -5 | afin_list$score == -4]
negList <- c(afin_list$word[afin_list$score == -3 |afin_list$score == -2 | afin_list$score == -1],
              "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")

posList <- c(afin_list$word[afin_list$score==3 | afin_list$score==2 | afin_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosList <- c(afin_list$word[afin_list$score == 5 | afin_list$score == 4],"uproarious", "riveting", "fascinating", "dazzling", "legendary")

#Removing stop words::::
library(tm)
doc_heads <- Corpus(DataframeSource(aggregated_reviews_by_srs[, .(review_head)]))
doc_rev_detail <- Corpus(DataframeSource(aggregated_reviews_by_srs[, .(reviews_detail)]))

#converting to lower case
doc_heads <- tm_map(doc_heads, tolower)
doc_rev_detail <- tm_map(doc_rev_detail, tolower)

#Remove Punctuation
doc_heads <- tm_map(doc_heads, removePunctuation)
doc_rev_detail <- tm_map(doc_rev_detail, removePunctuation)

# Removing numbers 
doc_heads <- tm_map(doc_heads, removeNumbers)
doc_rev_detail <- tm_map(doc_rev_detail, removeNumbers)

# removing stopwords
doc_heads <- tm_map(doc_heads, removeWords, stopwords("english"))
doc_heads <- tm_map(doc_heads, removeWords, c('us', 'email', 'due','show', 'shows', 'series', 'time', 'many','track','say'))
doc_rev_detail <- tm_map(doc_rev_detail, removeWords, stopwords("english"))
doc_rev_detail <- tm_map(doc_rev_detail, removeWords, c('us', 'email', 'due','show', 'shows', 'series','time', 'many','track','say'))

doc_heads[[1]][1]
doc_rev_detail[[1]][1]
# stripping unnecessary whitespace
doc_heads <- tm_map(doc_heads, stripWhitespace)
doc_rev_detail <- tm_map(doc_rev_detail, stripWhitespace)

# At the end of preprocessing, preprocess documents as text documents.
doc_heads <- tm_map(doc_heads, PlainTextDocument)
doc_rev_detail <- tm_map(doc_rev_detail, PlainTextDocument)
#head_dictCorpus <- doc_heads
#rev_detail_dictCorpus <- doc_rev_detail


# Extracting review hewader and reviews content from the Corpus
agg_reviews_src_new <- aggregated_reviews_by_srs
heads <- vector(mode="list")
reviews <- vector(mode = "list")
for(i in 1:length(doc_heads)){
  heads[i] <- do.call(function(...) c(...,recursive=T), list(doc_heads[[i]]$content))
}
for(i in 1:length(doc_rev_detail)){
  reviews[i] <- do.call(function(...) c(...,recursive=T), list(doc_rev_detail[[i]]$content))
}
heads <- unlist(heads)
reviews <- unlist(reviews)
# End of extracting content from the Corpus.
# Create new data table with cleaned headers and reviews
agg_reviews_src_new[, `:=`(review_head=heads, reviews_detail=reviews)]
write.table(agg_reviews_src_new, "CLEAN_agg_reviews_heads.xls", row.names = F, quote = T, sep='\t')


#txt <- strsplit("I wanted to use the findAssocs of the tm package. but it works only when there are more than one documents in the corpus. I have a data frame table which has one column and each row has a tweet text. Is it possible to convert the into a corpus which takes each row as a new document?", split=" ")[[1]]
#data <- data.frame(text=txt, stringsAsFactors=FALSE)
#data[1:5, ]

#dm <- Corpus(DataframeSource(data))
#y <- Corpus(VectorSource(unlist(dm)))
#meta(y, 'link') <- do.call(rbind, lapply(dm, meta))$`link`

#y <- do.call(function(...) c(...), dm[[1]]$content)

# calculate scores on each review head
polarityResult_head <- as.data.frame(sentiment_scale(as.character(agg_reviews_src_new[['review_head']]), vNegList, negList, vPosList, posList))
polarityResult_head$'2' <- as.numeric(polarityResult_head$'2')
polarityResult_head$'3' <- as.numeric(polarityResult_head$'3')
polarityResult_head$'4' <- as.numeric(polarityResult_head$'4')
polarityResult_head$'5' <- as.numeric(polarityResult_head$'5')

polarityResult_reviews <- as.data.frame(sentiment_scale(as.character(agg_reviews_src_new[['reviews_detail']]), vNegList, negList, vPosList, posList))
polarityResult_reviews$'2' <- as.numeric(polarityResult_reviews$'2')
polarityResult_reviews$'3' <- as.numeric(polarityResult_reviews$'3')
polarityResult_reviews$'4' <- as.numeric(polarityResult_reviews$'4')
polarityResult_reviews$'5' <- as.numeric(polarityResult_reviews$'5')

head(polarityResult_reviews[[5]])
head(polarityResult_head[[5]])

# Aggregating sentiments
colnames(polarityResult_head)
setnames(polarityResult_head, old=c('1','2','3','4','5'), new=c('review_head', 'WORST_hd', 'BAD_hd', 'GOOD_hd', 'BEST_hd'))
setnames(polarityResult_reviews, old=c('1','2','3','4','5'), new=c('reviews_detail', 'WORST_rev', 'BAD_rev', 'GOOD_rev', 'BEST_rev'))

colnames(agg_reviews_src_new)
aggregated_reviews_by_srs[, .(review_head)]

FINAL_IMDB_REVIEWS_SENTIMENTS <- cbind(aggregated_reviews_by_srs, polarityResult_head[2:5], polarityResult_reviews[2:5])
write.table(FINAL_IMDB_REVIEWS_SENTIMENTS, 'FINAL_IMDB_REV_SENTIMENTS.xls', row.names = F, quote = T, sep='\t')

getwd()
FINAL_IMDB_REVIEWS_SENTIMENTS <- data.table(read.table('FINAL_IMDB_REV_SENTIMENTS.xls', header = T, sep='\t'), key = c('serial_id','serial_name'))
head(FINAL_IMDB_REVIEWS_SENTIMENTS)

newdb <- FINAL_IMDB_REVIEWS_SENTIMENTS[, .(serial_name, WORST_rev, BAD_rev, GOOD_rev, BEST_rev)]
newdb <- melt(newdb,id.vars = "serial_name")
newdb[,.(unique(serial_name))]
head(newdb)

library(ggplot2)
ggplot(newdb[1:43],aes(x=serial_name,y=value,fill=factor(variable)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="sentiments",
                      breaks=c(1,2,3,4),
                      labels=c("worst", "bad", "good", "best"))+
  xlab("Series Name")+ylab("Sentiment scale")



#
wordcloudentity<-function(entitycleantext)
{
  tweetCorpus<-Corpus(VectorSource(cleanSubjects(entitycleantext)))
  tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                        stopwords=c(stopwords('english')),
                                                        removeNumbers=TRUE,tolower=TRUE))
  tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
  sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
  cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
  
  wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=80, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
  print(wcloudentity)
}


wordcloudentity(as.character(FINAL_IMDB_REVIEWS_SENTIMENTS[['reviews_detail']][10]))
