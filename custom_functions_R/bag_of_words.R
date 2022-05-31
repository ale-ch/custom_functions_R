bag_of_words <- function(..., split = " ",
         punct = FALSE,
         lower = TRUE,
         stopwords = FALSE,
         combine_files = FALSE,
         numbers = TRUE,
         ignore_missing_files = FALSE) {
  
  # implementation of bag-of-words
  file_names <- c(...)
  
  library(readtext)
  library(tidyr)
  library(dplyr)
  library(stringr)
  library(tm)
  
  
  # leave files separated or combine them into the same string
  text_list <- readtext(file_names, text_field = "texts",
                   ignore_missing_files = ignore_missing_files)
  
  if(combine_files == TRUE) text_list <- paste(text_list, collapse = " ")
  
  
  # remove breaks
  text_list <- str_remove_all(text_list, "\\n")
  # split by chosen split
  text_list <- str_split(text_list, split)
  
  
  text_list <- as.list(text_list)
  
  
  if(punct == FALSE) text_list <- lapply(text_list, removePunctuation)
  if(numbers == FALSE) text_list <- lapply(text_list, removeNumbers)
  if(lower == TRUE) text_list <- lapply(text_list, tolower)
  if(stopwords == FALSE) text_list <- qdap::rm_stopwords(text_list)
  
  # set names
  if(combine_files == FALSE) {
    names(text_list) <- file_names
  } else {
    names(text_list) <- "Combined documents"
  }
  
  # match length of list elements
  text_list <- lapply(text_list, `length<-`, max(lengths(text_list)))
  
  # create the bag of words data frame
  BOW <- as.data.frame(text_list) %>% 
    pivot_longer(
      cols = 1:length(names(text_list)),
      names_to = "doc",
      values_to = "words"
    ) %>% 
    na.omit() %>% 
    filter(words != "character(0)" & 
             str_length(words) != 1 & 
             str_length(words) != 0) %>% 
    group_by(doc, words) %>% 
    summarize(count = n()) %>% 
    ungroup() %>% 
    arrange(desc(count))
  
  # output 
  BOW
  
}
