library(ggplot2)
library(dplyr)

# List of CSV files
file_paths <- c("statewide.csv", "hartford.csv")

# Initialize a list to store dataframes
data_list <- list()

# Read each CSV file into a dataframe and store it in the list
for (file_path in file_paths) {
  data <- read.csv(file_path)
  data_list[[file_path]] <- data
}


# Perform data analysis on each dataframe
i <- 0
for (file_path in file_paths) {
  data <- data_list[[file_path]]
  
  # SUBJECT RACE DISTRIBUTION
  if("subject_race" %in% colnames(data)){
    
    # Plotting the distribution of the 'subject_race' column
    plot <- ggplot(data, aes(x = subject_race)) +
      geom_bar() + 
      theme_minimal() +
      labs(title = "Distribution of Subject Race",
           x = "Race",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # This rotates the x-axis labels for better readability
    j <- paste0(i, '_race_distribution.png')
    ggsave(j, plot = plot, dpi = 330)
    
    j <- paste0(i, '_race_distribution.jpeg')
    ggsave(j, plot = plot)
    
  } else {
    cat("The column 'subject_race' does not exist in the dataset.")
  }
  
   # SEARCH CONDUCTED
  data_search_conducted <- filter(data, search_conducted == TRUE)
  
  # Check if 'subject_race' column exists to avoid errors
  if("subject_race" %in% colnames(data_search_conducted)) {
    
    # Plotting the distribution of 'subject_race' for the filtered data
    plot <- ggplot(data_search_conducted, aes(x = subject_race)) +
      geom_bar() + 
      theme_minimal() +
      labs(title = "Distribution of Subject Race where Search was Conducted",
           x = "Race",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # This rotates the x-axis labels for better readability
    j <- paste0(i, '_search_conducted.png')
    ggsave(j, plot = plot, dpi = 330)
    j <- paste0(i, '_search_conducted.jpeg')
    ggsave(j, plot = plot, dpi = 330)
  } else {
    cat("The column 'subject_race' does not exist in the dataset.")
  }
  
  # RATIO
  data$search_conducted <- as.logical(data$search_conducted)
  
  race_summary <- data %>%
    group_by(subject_race) %>%
    summarise(
      Total_Count = n(),
      Search_Conducted_Count = sum(search_conducted, na.rm = TRUE)
    ) %>%
    mutate(Ratio = Search_Conducted_Count / Total_Count)
  
  # Check if both 'subject_race' and 'search_conducted' columns exist to avoid errors
  if(all(c("subject_race", "search_conducted") %in% colnames(data))) {
    
    # Calculating the total count and search conducted count by race
    race_summary <- data %>%
      group_by(subject_race) %>%
      summarise(
        Total_Count = n(),
        Search_Conducted_Count = sum(search_conducted, na.rm = TRUE)
      ) %>%
      mutate(Ratio = Search_Conducted_Count / Total_Count)
    
    # Plotting the ratio of search conducted for each race
    a <- ggplot(race_summary, aes(x = subject_race, y = Ratio, fill = subject_race)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Ratio of Searches Conducted to Total Instances by Race",
           x = "Race",
           y = "Ratio") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # This rotates the x-axis labels for better readability
    j <- paste0(i, 'ratio.png')
    ggsave(j, plot = a, dpi=330)
    j <- paste0(i, 'ratio.jpeg')
    ggsave(j, plot = a, dpi=330)
  } else {
    cat("The necessary columns do not exist in the dataset.")
  }
  
  i <- i+1
}
