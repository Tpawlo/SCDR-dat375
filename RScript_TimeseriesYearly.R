install.packages("ggplot2");
install.packages("dplyr");


# Load required libraries

library(ggplot2)

library(dplyr)


setwd("C:/Users/Public/Desktop/DAT-375") 

crimestormdataQ <- read.csv("crimeStormQ.csv")
print(crimestormdataQ)

crimenostormdataQ <- read.csv("crimenostormQ.csv")
print(crimenostormdataQ)


# Add a monthly date column starting Jan 2017

crimestormdataQ$Date <- seq(as.Date("2017-01-01"), by = "month", length.out = nrow(crimestormdataQ))

crimenostormdataQ$Date <- seq(as.Date("2017-01-01"), by = "month", length.out = nrow(crimenostormdataQ))



# Add labels for merging later

crimestormdataQ$StormStatus <- "Storm"

crimenostormdataQ$StormStatus <- "No Storm"



# Combine both datasets and keep relevant columns

combined_data <- bind_rows(
  
  crimestormdataQ[, c("Date", "Loss", "StormStatus")],
  
  crimenostormdataQ[, c("Date", "Loss", "StormStatus")]
  
)



# Convert Loss to cumulative totals in thousands

combined_data <- combined_data %>%
  
  group_by(StormStatus) %>%
  
  arrange(Date) %>%
  
  mutate(CumLoss = cumsum(Loss) / 1000)



# Plot the results

ggplot(combined_data, aes(x = Date, y = CumLoss, color = StormStatus)) +
  
  geom_line(size = 1.2) +
  
  labs(
    
    title = "Victim Loss From Crimes for Jan 2017 - Dec 2019",
    
    subtitle = "Cumulative Loss in Thousands of Dollars",
    
    x = "By Month by Year",
    
    y = "Victim Loss (K$)",
    
    color = "Crime Condition"
    
  ) +
  
  theme_minimal()

