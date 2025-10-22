
is_labs <- function(x) any(grepl("(^|::)labels$", class(x)))

norm_spaces <- function(x) gsub("\\s+", " ", trimws(x))
