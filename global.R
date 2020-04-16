library(pdftools)
library(magrittr)
library(lubridate)
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(stringi)

# Set flag for identifying rows with desired data.
a0 <- "A00000"

# Set flag for holiday because CAMS is too stupid to exclude holidays from reports.
# MLK Day

# holiday <- "2020-01-20"
holiday <- "01-20"

# Function to extract raw data from PDF
pre_process <- function(input_file){
  raw <- pdf_text(input_file)
  raw <- strsplit(raw, split = "\n")
  return(raw)
}

# Function to get course name from raw data.
get_course_name <- function(raw){
  return(raw[[1]][2])
}

# Function to extract name from a linelist of strings
get_name <- function(a_line_list){
  student_name <- paste(a_line_list[3:2], collapse = " ")
  student_name <- str_remove(student_name, ",") # Strip out the comma
  return(student_name)
}

# Function to extract presence status from a linelist of strings
get_presence <- function(a_line_list){
  suppressWarnings({
    if (TRUE %in%  stri_detect_fixed(a_line_list, "Left")) {
      return("Left Early")
    } else if (TRUE %in%  stri_detect_fixed(a_line_list, "Present")) {
      return("Present")
    } else if (TRUE %in%  stri_detect_fixed(a_line_list, "Absent")) {
      return("Absent")
    } else if (TRUE %in%  stri_detect_fixed(a_line_list, "Other")) {
      return("Other")
    } else if (TRUE %in%  stri_detect_fixed(a_line_list, "Tardy")) {
      return("Tardy")
    } else if (TRUE %in%  stri_detect_fixed(a_line_list, "Late")) {
      return("Late")
    }
  })
}

# Function to extract the date from a linelist of strings
get_date <- function(a_line_list){
  suppressWarnings({
    for (it in 1:length(a_line_list)){
      if (!is.na(mdy(a_line_list[it]))){
        return(a_line_list[it])
      } else {
        result <- NA
      }
    }
  })
  return(result)
}

# Function to build master dataframe
build_df <- function(input_data){
  # Initialize empty data frame.
  df <- data.frame(Student=character(),
                   Presence=character(),
                   Date=character(),
                   stringsAsFactors=FALSE)
  
  # Iterate over each page, over each line.
  for (i in 1:length(input_data)) {
    page <- input_data[[i]]
    for (j in 1:length(page)) {
      line <- page[j]
      # Strip each line into a list, and store its length.
      linelist <- strsplit(line, "\\s+")[[1]]
      n <- length(linelist)
      
      # Check if line contains student data by
      # checking whether that flagged string is in
      # any word in the list.
      if (TRUE %in%  stri_detect_fixed(linelist, a0)) {
        # Get name, presence, and date and save them to new_row
        new_row <- c(get_name(linelist), 
                     get_presence(linelist), 
                     get_date(linelist))
        # Add new_row to dataframe.
        df[nrow(df)+1,] <- new_row
      }
    }
  }
  # Correct Student name capitalization.
  df$Student <- str_to_title(df$Student)
  # Set the date string as a Date object.
  df$Date <- mdy(df$Date)
  df$Date <- format(df$Date, format="%m-%d")
  # Exclude holidays.
  df %>% filter(Date != holiday) -> df
  # Get number of class days.
  class_days_n <- length(unique(df$Date))
  return(df)
}

# Function to build dataframe of attendance by date
build_date_df <- function(input_df){
  # Group data by date and get total number of enrolled students each day.
  input_df %>% group_by(Date) %>% 
    count(name = "Total") -> date_df
  
  # Group data by date and count all who were in class any time that day.
  input_df %>% group_by(Date) %>% 
    filter(
      Presence == "Present" | 
        Presence == "Late" | 
        Presence == "Left Early"
    ) %>% 
    count(name = "In_Class") -> date_p_df
  
  # Aggregate counts for In_Class, Total (Enrolled), and Percent.
  date_df %>% add_column(In_Class = date_p_df$In_Class)  %>% 
    mutate(Percent = round(In_Class / Total * 100)) %>% 
    select(Date, In_Class, Total, Percent) -> date_df
  
  return(date_df)
}

# Function to build plot of attendance by date
build_date_plot <- function(input_df){
  date_plot <- ggplot(input_df, aes(x = factor(input_df$Date), y = input_df$Percent, group = 1)) +
    geom_line() +
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) +
    xlab("Date") +
    ylim(0, 100)
  return(date_plot)
}

# Function to build plot of attendance by date in Plotly
build_date_plotly <- function(input_df){
  date_plotly <- plot_ly(
    x = input_df$Date,
    y = input_df$Percent, 
    type = 'scatter',
    mode = 'lines+markers'
    )
  date_plotly %>% layout(title = 'Percentage of Enrolled Students Attending',
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Percent In Classroom", 
                                    ticksuffix = "%",
                                    range = c(0, 100)),
                       margin = list(t = 108)
                        ) -> date_plotly
    return(date_plotly)
}

# Function to subset master dataframe to only currently enrolled students
get_current_df <- function(input_df){
  # Get currently enrolled students only.
  input_df %>% filter(Date == max(Date)) %>% 
    pull(Student) -> cur_enr_students
  
  # Restrict data frame to include only currently enrolled students.
  input_df %>% filter(Student %in% cur_enr_students) -> cur_enr_df
  return(cur_enr_df)
}

# Function to build table of attendance by student
build_student_df <- function(input_df){
  
  # Get table of students' days in class.
  input_df %>% 
    group_by(Student) %>% 
    filter(Presence == "Present" | 
             Presence == "Late" | 
             Presence == "Left Early" |
             Presence == "Tardy") %>% 
    count(name = "In_Class", .drop = FALSE) -> inc_table
  
  # Get table of students' days absent.
  input_df %>%
    group_by(Student) %>%
    filter(Presence == "Absent") %>%
    count(name = "Absent", .drop = FALSE) -> abs_table
  
  # Merge the tables and create percentage column.
  merge(inc_table, abs_table, by = "Student", all = TRUE) %>% 
    replace_na(replace = list(In_Class = 0, Absent = 0))  %>% 
    mutate(Percent = round(In_Class / (In_Class + Absent) * 100)) -> att_table
  
  # Return final table.
  return(att_table)
}

# Function to build table of attendance by date and student
build_date_student_df <- function(input_df){
  abrev_cur_enr_df <- input_df %>% mutate(Presence = substr(Presence, 1, 2))
  abrev_cur_enr_df %>% 
    pivot_wider(names_from = Date, values_from  = Presence) -> untidy
  return(untidy)
}

# Function to get list of no-shows
get_noshows <- function(input_df){
  input_df %>% filter(In_Class == 0)
}
