library(dplyr)
library(ggplot2)
library(ggthemes)
library(sf)
library(purrr)
library(tidycensus)
library(tigris)
library(opentripplanner)
library(progress)

# Load 2014/15 survey data -----------------------------------------------------
# Park and ride data were collected at a different time, but this 2015-10-07 data
# file appears to contain both the P&R and local data sets. Some serial numbers
# are identical across sets and contain the same data. 

survey1 <- read.csv("data/SURVEY_DATA_2015-10-07.csv")
survey1$boarding_factor <- survey1$RESPONSE_FACTOR * survey1$SAMPLING_FACTOR * 
  survey1$CONTROL_FACTOR

# Match numbered of returned surveys from data book
sum(survey1$boarding_factor, na.rm = TRUE) # 238,822.5
# This is close to the 239,503 boardings listed on p. 17
# None of the park and ride trips have weights
# It's unclear how a trip was/was not assigned a weight
# 9,697 trips have valid expansion factors

sum(survey1$boarding_factor[survey1$O_MODE == 4], na.rm = TRUE) # 193,636
193636 / 238822 # 81.08% exactly matches p. 22
sum(survey1$boarding_factor[survey1$O_MODE == 1], na.rm = TRUE) # 23,788.3
23788.3 / 238882 # 9.96%

# Variables needed to skim public transit traveled times
# O_LON, O_LAT
# D_LON, D_LAT
# SCHEDULED_TRIP_START_TIME 0 - 1 mapped from time
# ggplot(survey1, aes(x = SCHEDULED_TRIP_START_TIME)) + geom_histogram()

# Create a departure time variable - before
survey1$deptime <- 
  as.POSIXct("2015-06-10") + 
  survey1$SCHEDULED_TRIP_START_TIME * 60 * 60 * 24

# Create a departure time variable - after
survey1$deptime2 <- 
  as.POSIXct("2015-08-19") + 
  survey1$SCHEDULED_TRIP_START_TIME * 60 * 60 * 24

# Create origin/destination sf data frames - before
origins <- 
  na.omit(
    data.frame(
      serial = survey1$SERIAL_NUMBER,
      o_lon = survey1$O_LON, 
      o_lat = survey1$O_LAT))

dests <- na.omit(
  data.frame(
    serial = survey1$SERIAL_NUMBER,
    d_lon = survey1$D_LON, 
    d_lat = survey1$D_LAT))

trips <- inner_join(origins, dests)

times <- survey1[survey1$SERIAL_NUMBER %in% trips$serial, "deptime"]

origins <- st_as_sf(trips[!is.na(times), ], coords = c("o_lon", "o_lat"),
                    crs = "+proj=longlat +datum=WGS84")

dests <- st_as_sf(trips[!is.na(times), ], coords = c("d_lon", "d_lat"),
                    crs = "+proj=longlat +datum=WGS84")

times <- times[!is.na(times)]

# Point map of origins and destinations
# tx_counties <- counties(state = "TX")
# harris <- tx_counties[tx_counties$NAME == "Harris", ]
#
# ggplot() + 
#   geom_sf(data = harris, fill = NA) + 
#   geom_sf(data = origins, alpha = 0.5, color = "green") + 
#   geom_sf(data = dests, alpha = 0.5, color = "blue") +
#   coord_sf(xlim = c(-96, -94.85), ylim = c(29.4, 30.2)) + theme_map()

# Query travel times using OTP - BEFORE ----------------------------------------
path_otp <- "otp/otp-1.3.0-shaded.jar"
path_data <- "otp"

otp_setup(otp = path_otp, dir = path_data, router = "before")
otpcon <- otp_connect(hostname =  "localhost", router = "before")
router_config <- otp_make_config("router")
router_config$routingDefault$clampInitialWait <- 0
otp_write_config(router_config, dir = path_data, router = "before")

routes <- data.frame(duration = 1:9314, waittime = 0, transfers = 0)

for(i in 1:10) {
  tryCatch(thisRoute <- 
             otp_plan(otpcon, 
                      fromPlace = origins[i, ], 
                      toPlace = dests[i, ], 
                      date_time = times[i], 
                      mode = c("TRANSIT", "WALK"), 
                      numItineraries = 1, 
                      get_geometry = FALSE, 
                      ncores = 2))
  
  if(!is.na(thisRoute)) {
    routes[i, "duration"] <- thisRoute[1, "duration"]
    routes[i, "waittime"] <- thisRoute[1, "waitingTime"]
    routes[i, "transfers"] <- thisRoute[1, "transfers"] }
  else {
    routes[i, "duration"] <- NA
    routes[i, "waittime"] <- NA
    routes[i, "transfers"] <- NA
  }
}

otp_stop()

write.csv(routes, "skims_before.csv")

# Query travel times using OTP - BEFORE ----------------------------------------
origins <- na.omit(data.frame(serial = survey1$SERIAL_NUMBER,
                              o_lon = survey1$O_LON, 
                              o_lat = survey1$O_LAT))

dests <- na.omit(data.frame(serial = survey1$SERIAL_NUMBER,
                            d_lon = survey1$D_LON, 
                            d_lat = survey1$D_LAT))

trips <- inner_join(origins, dests)

times <- survey1[survey1$SERIAL_NUMBER %in% trips$serial, "deptime2"]

origins <- st_as_sf(trips[!is.na(times), ], coords = c("o_lon", "o_lat"),
                    crs = "+proj=longlat +datum=WGS84")
dests <- st_as_sf(trips[!is.na(times), ], coords = c("d_lon", "d_lat"),
                    crs = "+proj=longlat +datum=WGS84")

times <- times[!is.na(times)]

otp_setup(otp = path_otp, dir = path_data, router = "after")
otpcon <- otp_connect(hostname =  "localhost", router = "after")
router_config <- otp_make_config("router")
router_config$routingDefault$clampInitialWait <- 0
otp_write_config(router_config, dir = path_data, router = "after")

routes <- data.frame(duration = 1:9314, waittime = 0, transfers = 0)

for(i in 1:9314) {
  tryCatch(thisRoute <- 
             otp_plan(otpcon, 
                      fromPlace = origins[i, ], 
                      toPlace = dests[i, ], 
                      date_time = times[i], 
                      mode = c("TRANSIT", "WALK"), 
                      numItineraries = 1, 
                      get_geometry = FALSE, 
                      ncores = 2))
  
  if(!is.na(thisRoute)) {
    routes[i, "duration"] <- thisRoute[1, "duration"]
    routes[i, "waittime"] <- thisRoute[1, "waitingTime"]
    routes[i, "transfers"] <- thisRoute[1, "transfers"] }
  else {
    routes[i, "duration"] <- NA
    routes[i, "waittime"] <- NA
    routes[i, "transfers"] <- NA
  }
}


write.csv(routes, "skims_after.csv")

# Analysis ---------------------------------------------------------------------

names(skims_before) <- c("x", "duration_b", "wait_b", "tran_b")
names(skims_after) <- c("y", "duration_a", "wait_a", "tran_a")

results <- cbind(serial = origins$serial, skims_before, skims_after)

results <- inner_join(survey1, results, by = c("SERIAL_NUMBER" = "serial"))
results$race <- fct_recode(as.factor(results$RACE_ETHNICITY),
                           white = "1",
                           other = "2",
                           other = "3",
                           black = "4",
                           Latinx = "5",
                           Asian = "6",
                           other = "7",
                           other = "8",
                           refused = "99")
results$purp <- fct_recode(as.factor(results$D_PURPOSE),
                           work = "1",
                           school = "3",
                           recreation = "5",
                           shopping = "6",
                           other = "2",
                           other = "4",
                           other = "7",
                           other = "99")


ggplot(results, aes(x = duration_b/60, y = duration_a/60)) + geom_point()

# Compare TLFDs


# Racial category 
ggplot(filter(results, race != "refused"), aes(x = duration_b/60, y = duration_a/60), alpha=0.8) + 
  geom_point() + 
  facet_wrap(~race) +
  scale_color_viridis_d() + 
  xlim(0, 200) + ylim(0, 200)
  
racial_sum <- results %>%
  filter(!is.na(boarding_factor)) %>%
  group_by(race, purp) %>%
  summarize(duration_b = weighted.mean(duration_b / 60, boarding_factor, na.rm = TRUE),
            duration_a = weighted.mean(duration_a / 60, boarding_factor, na.rm = TRUE),
            transfers_b = weighted.mean(tran_b, boarding_factor, na.rm = TRUE),
            transfers_a = weighted.mean(tran_a, boarding_factor, na.rm = TRUE))

racial_sum_long <- gather(racial_sum, duration, time, duration_a, duration_b)
racial_sum_long$duration <- fct_recode(racial_sum_long$duration, before = "duration_b",
                                       after = "duration_a")

ggplot(filter(racial_sum_long, race %in% c("white", "black", "Latinx", "Asian") &
                purp %in% c("work", "shopping", "school")),
              aes(x = race, y = time, fill = fct_rev(duration))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~ purp) +
  xlab(NULL) + 
  ylab("journey time (minutes)") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL))

ggsave("prepostSR_purpose+race.png", width = 8, height = 3)


distributions <- rbind(data.frame(time = results$duration_a,
                                  purp = results$purp, 
                                  race = results$race, when = "after"), 
                       data.frame(time = results$duration_b, 
                                  purp = results$purp, 
                                  race = results$race, when = "before"))


ggplot(distributions, aes(x = time / 60, fill = fct_rev(when))) + 
  geom_density(stat = "density", alpha = 0.5) +
  # scale_fill_brewer(palette = "Dark2") + 
  facet_wrap(~ race) +
  xlab(NULL) + 
  ylab("density") + 
  xlim(0, 150)
  theme_bw() + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(title = NULL))