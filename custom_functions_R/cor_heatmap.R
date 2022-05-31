cor_heatmap <- function(data, cutoff = NULL) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(caret)
  
  if(!is.null(cutoff)) {
    corr_cutoff_idx <- findCorrelation(cor(data), 
                                       cutoff = cutoff, exact = FALSE)
    cor_matrix <- cor(data[, -corr_cutoff_idx])
  } else {
    cor_matrix <- cor(data)
  }

  cor_matrix <- as.data.frame(cor_matrix) %>% 
    pivot_longer(
      cols = 1:length(.),
      names_to = "var1",
      values_to = "corr"
    ) %>% 
    mutate(
      var2 = rep(rownames(cor_matrix), each = nrow(cor_matrix))
    ) %>% 
    select(var1, var2, corr)
  
  cor_matrix %>% 
    ggplot(aes(var1, var2, fill = corr)) +
    geom_raster() +
    scale_fill_continuous(trans = 'reverse') +
    theme(
      axis.text.x = element_text(angle = 90)
    )
}