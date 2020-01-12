rmarkdown::render("Attendance_Report_R.Rmd", clean = FALSE)
# library(pdftools)
# library(magrittr)
# library(lubridate)
# library(tibble)
# library(dplyr)
# library(ggplot2)
# library(here)
# 
# 
# # Store flag for row of student data.
# a0 <- "A00000"
# 
# # Initialize empty data frame.
# df <- data.frame(Student=character(), 
#                  Presence=character(), 
#                  Date=character(), 
#                  stringsAsFactors=FALSE) 
# 
# # Get raw attendance text, a list of lists.
# att <- pdf_text(here("fromCAMS.pdf")) %>% strsplit(split = "\n")
# header <- att[[1]][2]
# 
# # Iterate over each page, over each line.
# for (i in 1:length(att)) {
#   page <- att[[i]]
#   for (j in 1:length(page)) {
#     line <- page[[j]]
#     linelist <- strsplit(line, "\\s+")[[1]]
#     n <- length(linelist)
#     if (!is.na(substr(linelist[5], 1, 6))){
#       if (substr(linelist[n - 2], 1, 6) == a0) {
#         if (linelist[6] == "Left") {
#           new_row <- c(paste(linelist[2:3], collapse = " "), 
#                        paste(linelist[(n - 2):(n - 1)], collapse = " "),
#                        linelist[n])
#         } else {
#           new_row <- c(paste(linelist[2:3], collapse = " "), linelist[(n - 1):(n)])
#         }
#         df[nrow(df)+1,] <- new_row
#       }
#     }
#   }
# }
# 
# df$Date <- mdy(df$Date)
# 
# df %>% group_by(Date) %>% count(name = "total") -> date_df
# df %>% group_by(Date) %>% filter(Presence == "Present" | Presence == "Late" | Presence == "Left Early") %>% count(name = "in_class") -> date_p_df
# date_df %>% add_column(in_class = date_p_df$in_class) -> date_df
# date_df %>% mutate(percent = in_class / total * 100) -> date_df