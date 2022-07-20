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

# Return value of variable of interest whose name is also found in rows
# data:  Data frame
# var:   Variable of interest in data frame
match_col <- function(data, var) {
  sapply(1:nrow(data), function(i) data[i, data$var[i]])
}

# Convert a "horizontal" table into a "vertical" data frame.  Removes
# blank rows from input data as well.  NOTE: However you load the
# original horizontal table, be sure to make sure it has no header
# defined, e.g., `read.delim(file, header = FALSE))`.
verticalize <- function(data) {
  data[!apply(data == "", 1, all), ]
  vert <- as.data.frame(t(data), row.names = NULL)
  names(vert) <- vert[1,]
  row.names(vert) <- NULL
  vert[-1,]
}
