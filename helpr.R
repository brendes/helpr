#!/usr/bin/env Rscript

# path     Character. Path to folder containing the data. Should end on /.
# pattern  Character. Regular expression (file extension or other parts of the file name).
# read_fun Function. Function used to read data files (e.g. read.csv).
# ...      Other parameters passed to read_fun.
batch_read <- function(path, pattern, recursive = FALSE, read_fun, ...) {
  data.files <- list.files(path, pattern = pattern, recursive = recursive)
  data <- lapply(paste0(path, data.files), read_fun, ...)
  data <- do.call("rbind", data)
  data
}

# Return value of variable name of interest that is also found in rows
# data:  Data frame
# var:   Variable of interest in data frame
match_col <- function(data, var) {
  sapply(1:nrow(data), function(i) data[i, data$var[i]])
}
