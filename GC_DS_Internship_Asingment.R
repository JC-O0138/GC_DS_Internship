options(scipen = 999)
library(tidyverse)
library(httr)
library(dplyr)

df <- read_csv("~/Downloads/Detailed_Statistics_Departures.csv", skip = 6)

df <- df %>%
  mutate(
    # Split HH:MM:SS into components
    scheduled_hours = as.numeric(sub(":.*", "", `Scheduled departure time`)),
    scheduled_minutes = as.numeric(sub("^[^:]*:([^:]*):.*", "\\1", `Scheduled departure time`)),
    scheduled_seconds = as.numeric(sub(".*:", "", `Scheduled departure time`)),
    
    # Convert to total seconds
    scheduled_total_seconds = scheduled_hours * 3600 + scheduled_minutes * 60 + scheduled_seconds,
    
    # Convert to decimal hours
    scheduled_total_hours = scheduled_hours + scheduled_minutes/60 + scheduled_seconds/3600,
    
   
    actual_hours = as.numeric(sub(":.*", "", `Actual departure time`)),
    actual_minutes = as.numeric(sub("^[^:]*:([^:]*):.*", "\\1", `Actual departure time`)),
    actual_seconds = as.numeric(sub(".*:", "", `Actual departure time`)),
    
    actual_total_seconds = actual_hours * 3600 + actual_minutes * 60 + actual_seconds,
    actual_total_hours = actual_hours + actual_minutes/60 + actual_seconds/3600
  )

#departure delay in hours
df$departure_delay <- df$actual_total_hours - df$scheduled_total_hours

#   Each row of the dataset represents a Spirit airlines flight departing from 
#Pittsburgh international airport
#   The most important columns are the scheduled_total_hours column, scheduled_
# departure_delay and the destination airport column. The departure delay shows 
# the departure delay in hours. I had to skip the first 6 columns because of 
# csv formatting and some of the outliers are present in the data are because of
# the conversion from time formatting to numerical units.

orlando_flights <- df %>%
  filter(`Destination Airport` == "MCO")  # Orlando International

las_vegas_flights <- df %>%
  filter(`Destination Airport` == "LAS")  # Las Vegas

newark_flights <- df %>%
  filter(`Destination Airport` == "EWR")  # Newark Liberty International

los_angeles_flights <- df %>%
  filter(`Destination Airport` == "LAX")  # Los Angeles

fort_lauterdale_flights <- df %>%
  filter(`Destination Airport` == "FLL")  # Fort Lauterdale

miami_flights <- df %>%
  filter(`Destination Airport` == "MIA")  # Miami

tampa_flights <- df %>%
  filter(`Destination Airport` == "TPA")  # Tampa

myrtle_beach_flights <- df %>%
  filter(`Destination Airport` == "MYR")  # Myrtle Beach

southwest_florida_flights <- df %>%
  filter(`Destination Airport` == "RSW")  # Fort Myers

morning_flights <- df %>%
  filter(`Scheduled departure time` >= hms::as_hms("06:00:00") &
           `Scheduled departure time` < hms::as_hms("12:00:00"))

evening_flights <- df %>%
  filter(`Scheduled departure time` >= hms::as_hms("18:00:00") &
           `Scheduled departure time` < hms::as_hms("24:00:00"))

afternoon_flights <- df %>%
  filter(`Scheduled departure time` >= hms::as_hms("12:00:00") &
           `Scheduled departure time` < hms::as_hms("18:00:00"))

delay_mean <- mean(df$departure_delay)
delay_median <- median(df$departure_delay)

time_mean <- mean(df$scheduled_total_hours)
time_median <- median(df$scheduled_total_hours)

afternoon_del_mean <- mean(afternoon_flights$departure_delay)
afternoon_del_median <- median(afternoon_flights$departure_delay)

evening_del_median <- median(evening_flights$departure_delay)
evening_del_mean <- mean(evening_flights$departure_delay)

morning_del_mean <- mean(morning_flights$departure_delay)
morning_del_median <- median(morning_flights$departure_delay)

f1=lm(df$departure_delay~df$scheduled_total_hours)
summary(f1)

r1=cor.test(df$departure_delay,
            + df$scheduled_total_hours)
r1
 
#   A single variable regression model showed that there was an inverse 
# relationship between departure delay and scheduled departure time, however the
# relationship is not statistically significant. But out of morning flights have
# a greater delay mean and delay median than evening flights so i think it's 
# reasonable to conclude that evening flights are more likley to be delayed than
# morning flights. Afternoon flights had greater mean delay and median delay 
# than both morning and evening.


mean_del_MCO <- mean(orlando_flights$departure_delay)
mean_del_LAS <- mean(las_vegas_flights$departure_delay)
mean_del_EWR <- mean(newark_flights$departure_delay)
mean_del_LAX <- mean(los_angeles_flights$departure_delay)
mean_del_FLL <- mean(fort_lauterdale_flights$departure_delay)
mean_del_MIA <- mean(miami_flights$departure_delay)
mean_del_TPA <- mean(tampa_flights$departure_delay)
mean_del_MYR <- mean(myrtle_beach_flights$departure_delay)
mean_del_RSW <- mean(southwest_florida_flights$departure_delay)

#   Flights arriving at Newark Liberty International airport(EWR) had the 
# greatest mean departure delay, which was approximatley 0.236 hours
# flights arriving at Fort Lauterdale (FLL) and the least mean departure delay, 
# which was approximatley -0.218 hours

#histogram
p1 <- ggplot(morning_flights, aes(x = as.numeric(departure_delay))) +
  geom_histogram(binwidth = 0.25, fill = "deepskyblue", color = "white", alpha = 0.8) +
  labs(title = "Distribution of Morning Departure Delays",
       subtitle = paste("Spirit Airlines from Pittsburgh International Airport",
                        "\nOctober - December 2023"),
       x = "Delay Hours",
       y = "Count") +
  theme_minimal() +
  geom_vline(xintercept = 0.0126, linetype = "dashed", color = "red")
#   I chose the histogram (p1) because it clearly shows how many flights can be 
# expected to leave earlier or later than the mean delay time during the morning.
# The distribution seems almost normal however positive outliers skew the 
# distributuion, and give explanation for the mean being greater than the median


#bar chart
p2 <- ggplot(df, aes(x = scheduled_total_hours, y = departure_delay)) +
  # individual points with transparency
  geom_point(alpha = 0.3, color = "steelblue", size = 2, 
             position = position_jitter(width = 0.2, height = 0)) +
  # smoothed trend line
  geom_smooth(method = "loess", color = "red", size = 1.5, se = TRUE, 
              fill = "pink", alpha = 0.2) +
  # horizontal line at zero (on-time reference)
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen", 
             linewidth = 1, alpha = 0.7) +
  # Customize axis
  scale_x_continuous(
    breaks = seq(0, 23, 2),
    labels = function(x) sprintf("%02d:00", x),
    expand = expansion(mult = 0.02)
  ) +
  scale_y_continuous(
    breaks = seq(-60, 180, 30),
    labels = function(x) ifelse(x < 0, paste0("Early ", abs(x), "m"), 
                                ifelse(x == 0, "On Time", paste0("Late ", x, "m")))
  ) +
  # Labels and theme
  labs(
    title = "Relationship Between Time of Day and Departure Delay",
    subtitle = paste("Spirit Airlines from Pittsburgh International Airport",
                     "\nOctober - December 2023"),
    x = "Scheduled Departure Time (Hour of Day)",
    y = "Departure Delay",
    caption = paste("Negative values = Early departure",
                    "\nPositive values = Late departure",
                    "\nRed line shows LOESS smoothed trend",
                    "\nGreen dashed line = Perfectly on time")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white")
  )
#   I chose the scatter plot (p2) because it gives visual confirmation that the 
# relationshup between time of day and flight delays isn't statistically significant
# however the delays are concentrated at the far left and far right of the plot.
p2
p1



#   The main limitation of the dataset and by proxy my analysis is that I could 
# only analyze the flights departing from a single airport and a single airline 
# using one dataset. This made it so that the findings of my analysis can most
# reasonably be applied to Spirit Airlines flights departing from Pittsburgh 
# International airport. However there are outliers present when the dataset is 
# broken up into times of day, which skewed the means for morning, afternoon and
# evening departure delays.
# Although flights arriving to EWR had the largest mean 
# delay, flights arriving at MYR, RSW, and FLL, had negative mean delays, 
# therefore it can be inferred that flights to those three airports usually leave
# earlier than scheduled. Afternoon flights tend to be the most delayed out of 
# the categories for time of day, this can be inferred by Afternoon flights 
# having the greatest mean delay and the only median value greater than zero. 














