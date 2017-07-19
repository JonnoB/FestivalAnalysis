ProcessArticles<- function(FestivalName, sents = NULL){
  #This function loads and processes the corpus from nexislexis before outputting the analysed results
  
  
  festlower <- tolower(FestivalName)
  festspaceless <- gsub(" ", "",festlower)
  
  #Loads all files with the festival name into a list
  corpus<- list.files(pattern = FestivalName) %>% map(~LexisNexisSource(.x, encoding = "UTF-8") %>%
                                                        Corpus(., readerControl = list(language = NA))
  ) %>%
    #Collapses list of corpora into a single corpus
    do.call(function(...) c(..., recursive = FALSE), .)
  
  
  Papernames<- sapply(1:length(corpus),function(n) meta(corpus[[n]])$origin)
  
  Papernames <- Papernames %>% tolower %>%
    # gsub("\\(london\\)|\\(england\\)|\\(united kingdom\\)", "",.) %>%
    gsub("sunday", "", .) %>% 
    gsub(".*sun.*", "sun", .) %>%
    gsub(".*express.*", "express", .) %>%
    gsub(".*independent.*", "independent", .) %>%
    gsub(".*telegraph.*", "telegraph", .) %>%
    gsub(".*mirror.*", "mirror", .) %>%  
    gsub(".*times.*", "times", .) %>%
    gsub(".*mail.*", "mail", .) %>%
    gsub(".*observer.*", "guardian", .) %>%
    gsub(".*guardian.*", "guardian", .)
  str_trim
  Papernames %>% unique
  
  for (i in 1:length(corpus)) {
    meta(corpus[[i]])$origin <- Papernames[i]
  }
  
  corpus <- corpus %>%
    tm_map(., content_transformer(tolower)) %>%
    tm_map( PatternReplace, "[[:punct:]]", "") %>%
    tm_map( PatternReplace, "festival", " festival ") %>%   
    tm_map( PatternReplace, "carnival", " carnival ") %>%   
    tm_map( PatternReplace, festlower, festspaceless) %>%
    tm_map( removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace)  
  
  
  
  #keep only articles that mention the carnival more than once, as others are probably just mentioning it in passing
  dtm <- DocumentTermMatrix(corpus)
  keep <- dtm %>% #[, grepl("carnival$", dtm$dimnames$Terms)] %>% 
    tidy 
  
  Docnames <- data_frame(document=(unique(keep$document)))
  
  keep<-  keep %>% filter(grepl(paste0(festspaceless,"$"), term)) %>% left_join(Docnames,., by = "document") %>%
    mutate(keep = count>1)  %>%
    arrange(match(document,Docnames$document))
  
  keep<-  keep %>% filter(grepl(paste0(festspaceless,"$"), term))%>%
    mutate(keep = count>1)
  
  
  #after this sentiment analysis can be done
  corpus3 <- corpus 
  
  #words cleaned and with the paper name
  CleanWords <-corpus3 %>% DocumentTermMatrix %>% tidy %>%
    filter(document %in% keep$document) %>%
    left_join(., tidy(corpus3) %>% 
                select(id, origin), by= c("document" = "id")) %>%
    rename(Paper = origin)
  
  TotalWords <-sum(CleanWords$count, na.rm = T) 
  
  
  if(is.null(sents)){
    sents <- get_sentiments("nrc")
  }
  
  #sentiment attached to the words
  SentiWords <- CleanWords %>%
    right_join(sents, by=c("term"="word")) %>%
    filter(complete.cases(.)) 
  
  TopWords <- SentiWords %>% group_by(term) %>%
    summarise(counts = sum(count)) %>%
    arrange(desc(counts))# %>%
    #slice(1:20)
  
  #sentmiment by paper
  SentimentbyPaper<- SentiWords%>%
    group_by(Paper, sentiment) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    spread(key = Paper, value = count)
  SentimentbyPaper[is.na(SentimentbyPaper)] <-0
  
  
  #Carnival Sentiment
  FestivalSentiment<- SentiWords%>%
    group_by(sentiment) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    mutate(Festival = FestivalName)
  
  out <- list(SentiWords, SentimentbyPaper, FestivalSentiment, TotalWords, TopWords) %>% 
    setNames(c("SentiWords","SentimentbyPaper", "FestivalSentiment", "TotalWords", "TopWords"))
  
  
}