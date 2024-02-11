library(readr)
library(dplyr)
library(ggplot2)
library(maps)

# Read the data
data <- read.csv("Border_Crossing_Entry_Data.csv")

# Convert 'Date' column to date format
data$Date <- as.Date(paste0("01 ", data$Date), format = "%d %b %Y")

# Extract year
data$Year <- format(data$Date, "%Y")

# Summarize total number of crossings by year
yearly_summary <- data %>%
  group_by(Year) %>%
  summarise(Total_Crossings = sum(Value))

# Plot
ggplot(yearly_summary, aes(x = Year, y = Total_Crossings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Year", y = "Total Crossings", title = "Border Crossings by Year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# 1. Border Crossings by State
state_summary <- data %>%
  group_by(State) %>%
  summarise(Total_Crossings = sum(Value))

ggplot(state_summary, aes(x = reorder(State, -Total_Crossings), y = Total_Crossings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "State", y = "Total Crossings", title = "Border Crossings by State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels = scales::comma)

# 2. Border Crossings by Measure
measure_summary <- data %>%
  group_by(Measure) %>%
  summarise(Total_Crossings = sum(Value))

ggplot(measure_summary, aes(x = Measure, y = Total_Crossings)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Measure", y = "Total Crossings", title = "Border Crossings by Measure") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(labels = scales::comma)

# 3. Border Crossings Over Time
time_summary <- data %>%
  group_by(Date) %>%
  summarise(Total_Crossings = sum(Value))

ggplot(time_summary, aes(x = Date, y = Total_Crossings)) +
  geom_line() +
  labs(x = "Date", y = "Total Crossings", title = "Border Crossings Over Time") + scale_y_continuous(labels = scales::comma)

# 4. Border Crossings by Port (Top 10)
port_summary <- data %>%
  group_by(Port.Name) %>%
  summarise(Total_Crossings = sum(Value)) %>%
  arrange(desc(Total_Crossings)) %>%
  top_n(10)