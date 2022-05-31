col_type_count <- function(df = data.frame()) {
  types_frequencies <- sapply(df, typeof)
  table(types_frequencies)
}

col_class_count <- function(df = data.frame()) {
  classes_frequencies <- sapply(df, class)
  table(types_frequencies)
}