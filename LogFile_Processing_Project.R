# Author: Laksha Karthikeyan
# Snippet of Data Processing Project
# Using regular expressions to extract elements of lines from log file
# to create a usable data frame

library(stringr)

filepath = "/Users/laksha/Downloads/MergedAuth.log"

# function that will take each line of the entire log file extract parts of the metadata that will be in each column
# returns a line as a data frame (to be added to overall dataframe)
extract_lines <- function(line){
  # file name is found in a line 
  file_name <- substr(log_lines, 1, regexpr(" ", log_lines)-1)
  print(file_name)
  
  #substring to take out the file name
  fname_start <- 1
  fname_len <- nchar(file_name)
  print(fname_len)
  fname_end = fname_start+fname_len
  match <- regexpr(file_name, log_line)
  match_start <- match[1]
  match_len <- attr(match, "match.length")
  end_match = match_start+match_len
  ex_fname <- log_line
  i = 1
  ex_fname <- substr(ex_fname, end_match, nchar(ex_fname))
  ex_fname <- trimws(ex_fname, which=c("left"))
  print(ex_fname)
  
  #https://stackoverflow.com/questions/2192316/extract-a-regular-expression-match 
  # substring is now line with file name removed (starts from date)
  
  # extracting date and time from log file line
  #regexpr resource: http://www.endmemo.com/r/regexpr.php
  dt <- regexpr("[0-9]{2}:[0-9]{2}:[0-9]{2}", ex_fname)
  dt_start <- dt[1]
  dt_len <- attr(dt, "match.length")
  end_ind = dt_start+dt_len
  
  datetime <- substr(ex_fname,1,dt+dt_len-1)
  print(datetime)
  # substring to splice out datetime
  ex_dt <- substr(ex_fname, end_ind, nchar(ex_fname))
  ex_dt <- trimws(ex_dt, which=c("left"))
  print(ex_dt)
  