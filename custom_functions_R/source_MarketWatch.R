source_MarketWatch <- function(symbols = character) {
  #### lowercase symbols ####
  symbols <- tolower(symbols)
  
  # vector of URLs to scrape
  urls <- vector(length = length(symbols))
  for(i in seq_along(symbols)) {
    urls[i] <- paste0("https://www.marketwatch.com/investing/stock/", symbols[i], 
                      "?mod=quote_search")
  }
  
  # list of pages 
  pages <- tibble(page = map(urls, read_html))
  
  #####
  out <- list()
  for(i in seq_len(length(pages$page))) {
    out[[i]] <- c(pages[[1]][[i]] %>% 
                    html_nodes(".article__figure+ .article__content .article__timestamp , .article__headline .text , .article__figure+ .article__content .link") %>% 
                    html_text())
  }
  
  
  
  
  df <- data.frame(headline = unlist(out)) %>% 
    mutate(headline = str_replace_all(headline, regex("  +"), "")) %>% 
    mutate(headline = str_replace_all(headline, regex("\\n"), "")) %>% 
    filter(headline != "")
  
  
  headlines <- df %>% 
    filter(!str_detect(headline, "\\w+\\. +\\d+"))
  
  timestamps <- df %>% 
    filter(str_detect(headline, "\\w+\\. +\\d+"))
  
  
  news <- bind_cols(headlines, timestamps) %>% 
    transmute(headline = `headline...1`,
              timestamp = `headline...2`) %>% 
    separate(timestamp, into = c("date", "time"), 
             sep = " at ") %>% 
    separate(time, into = c("time", "time_zone"), 
             sep = "m. ") %>% 
    mutate(time = paste0(time, "m.")) %>% 
    mutate(time = toupper(time)) %>% 
    mutate(time = str_replace_all(time, "\\.", "")) %>% 
    mutate(time = format(strptime(time, "%I:%M %p"), "%H:%M"),
           date = mdy(date), 
           time_zone = "EST") %>% 
    unite(datetime, 2:3, sep = " ") %>% 
    mutate(datetime = as.POSIXct(datetime, tz = "EST")) %>% 
    mutate(datetime = as_datetime(datetime, tz = "EST")) %>% 
    select(-time_zone)
  
  news
}