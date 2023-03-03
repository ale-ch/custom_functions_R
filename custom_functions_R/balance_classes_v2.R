balance_classes <- function(data = data.frame(), x = character(), 
                            new_prop = numeric(), 
                            method = c("over-under", "over", "under")) {
  
  #### ERROR HANDLING ####
  # check number of levels in categorical variable
  if(length(levels(as.factor(unlist(data[x])))) != 2) {
    stop("Categorical variable 'x' must be dichotomous.")
  }
  
  # check for invalid new proportion 
  if(new_prop < 0 | new_prop > 1) { 
    stop("Argument 'new_prop' must be between 0 an 1.")
  }
  
  # check for invalid balancing method
  if(!(method[1] %in% c("over-under", "over", "under"))) {
    stop("Argument 'method' must be either one of 'over-under', 'over' or 'under'.")
  }
  
  #### INPUTS FOR BALANCING METHODS ####
  # values for positive and negative class
  pos_class <- names(which.max(prop.table(table(data[x]))))
  neg_class <- names(which.min(prop.table(table(data[x]))))
  
  # positions for observations of positive and negative class
  pos_idx <- which(data[x] == pos_class)
  neg_idx <- which(data[x] == neg_class)
  
  # number of observations for positive and negative class
  n_pos <- length(pos_idx)
  n_neg <- length(neg_idx)
  
  # number of observations for positive class after undersampling
  n_pos_under <- floor(new_prop * n_neg / (1 - new_prop))
  # number of observations for negative class after oversampling
  n_neg_over <- floor(n_pos * (1 - new_prop) / new_prop)
  
  n_remove <- n_pos - n_pos_under 
  n_add <- n_neg_over - n_neg
  
  #### BALANCING ####
  if(method[1] == "under") {
    # UNDERSAMPLING 
    pos_undersmp_idx <- sample(pos_idx, n_pos_under)
    df_balanced <- data[c(neg_idx, pos_undersmp_idx), ]
    
  } else if(method[1] == "over") {
    # OVERSAMPLING
    neg_oversmp_idx <- sample(neg_idx, n_add, replace = TRUE)
    df_balanced <- rbind(data[neg_oversmp_idx, ], data)
    
  } else {
    # OVER-UNDER SAMPLING 
    pos_undersmp_idx <- sample(pos_idx, n_pos - floor(n_remove / 2))
    neg_oversmp_idx <- sample(neg_idx, n_neg + floor(n_add / 2), 
                              replace = TRUE)
    df_balanced <- data[c(pos_undersmp_idx, neg_oversmp_idx), ]
  }
  
  df_balanced
  
}
