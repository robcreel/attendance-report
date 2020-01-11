library(pdftools)
library(magrittr)
library(lubridate)
library(dplyr)

a0 <- "A00000"
df <- data.frame(Student=character(), 
                 Presence=character(), 
                 Date=character(), 
                 stringsAsFactors=FALSE) 

# Get raw attendance text, a list of lists.
att <- pdf_text("Attendance Report.pdf") %>% strsplit(split = "\n")
header <- att[[1]][2]

# Iterate over each page, over each line.
for (i in 1:length(att)) {
  page <- att[[i]]
  for (j in 1:length(page)) {
    line <- page[[j]]
    linelist <- strsplit(line, "\\s+")[[1]]
    n <- length(linelist)
    if (!is.na(substr(linelist[5], 1, 6))){
      if (substr(linelist[n - 2], 1, 6) == a0) {
        if (linelist[6] == "Left") {
          new_row <- c(paste(linelist[2:3], collapse = " "), 
                       paste(linelist[(n - 2):(n - 1)], collapse = " "),
                       linelist[n])
        } else {
          new_row <- c(paste(linelist[2:3], collapse = " "), linelist[(n - 1):(n)])
        }
        df[nrow(df)+1,] <- new_row
      }
    }
  }
}

df$Date <- mdy(df$Date)

# df <- tibble(df)


df %>% group_by(Student)
