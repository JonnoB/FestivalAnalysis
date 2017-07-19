TopEmotions <- function(emotion, top =NULL, patt = "DATA"){
  out <- ls(pattern = patt, envir = globalenv()) %>% 
    map2_df(.,.y=rep(emotion, length(.)),~{
      out <- get(.x) %>%
        .$SentiWords %>%
        filter(sentiment == .y)%>%
        group_by(term) %>%
        summarise(z = sum(count)) %>%
        arrange(desc(z)) %>%
        slice(1:(ifelse(is.null(top), nrow(.), top))) %>%
        mutate(Festival = sub("DATA", "",.x),
               number = 1:n())
      
      return(out)
    }) %>%
    select(-z) %>%
    spread(key= Festival, value = "term")
  
  
  return(out)
}
