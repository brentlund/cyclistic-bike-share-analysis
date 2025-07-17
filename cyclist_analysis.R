# =====================
# LIBRARIES
# =====================
library(tidyverse)
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(lubridate)

# =====================
# STEP 1: LOAD DATA
# =====================
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# =====================
# STEP 2: WRANGLE DATA
# =====================

# Rename columns in q1_2019 to match q1_2020
q1_2019 <- rename(q1_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype
)

# Convert ride_id and rideable_type to character for consistent stacking
q1_2019 <- q1_2019 %>%
  mutate(
    ride_id = as.character(ride_id),
    rideable_type = as.character(rideable_type)
  )

# Combine both data frames
all_trips <- bind_rows(q1_2019, q1_2020)

# Drop columns not present in 2020 data
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, tripduration))

# =====================
# STEP 3: CLEAN DATA
# =====================

# Standardize member/casual labels
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"
  ))

# Add date-based columns
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(all_trips$date, "%m")
all_trips$day <- format(all_trips$date, "%d")
all_trips$year <- format(all_trips$date, "%Y")
all_trips$day_of_week <- format(all_trips$date, "%A")

# Calculate ride_length (in seconds)
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at, units = "secs"))

# Remove bad data (e.g., HQ QR, negative durations)
all_trips_v2 <- all_trips %>%
  filter(start_station_name != "HQ QR", ride_length >= 0)

# ============================
# STEP 4: DESCRIPTIVE ANALYSIS
# ============================

# Summary statistics
summary(all_trips_v2$ride_length)

# Ride length by user type
aggregate(ride_length ~ member_casual, data = all_trips_v2, mean)
aggregate(ride_length ~ member_casual, data = all_trips_v2, median)
aggregate(ride_length ~ member_casual, data = all_trips_v2, max)
aggregate(ride_length ~ member_casual, data = all_trips_v2, min)

# Average ride time by day of week and user type
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,
                                    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
)

avg_by_day <- aggregate(
  ride_length ~ member_casual + day_of_week,
  data = all_trips_v2,
  FUN = mean
)

# Ridership count and duration by weekday
summary_table <- all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(
    number_of_rides = n(),
    average_duration = mean(ride_length)
  ) %>%
  arrange(member_casual, weekday)

# =====================
# STEP 5: VISUALIZATION
# =====================

# Number of rides by weekday
ggplot(summary_table, aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Rider Type and Weekday", y = "Number of Rides")

# Average duration by weekday
ggplot(summary_table, aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average Ride Duration by Rider Type and Weekday", y = "Average Duration (sec)")

# =====================
# STEP 6: EXPORT DATA
# =====================
write.csv(avg_by_day, file = "avg_ride_length.csv", row.names = FALSE)
