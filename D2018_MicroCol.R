# PROJECT | METADATA========================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# May 29, 2018

# Model microcolony response to landscape-simulated food treatments
# Experiment 1 dates: March 15, 2018 - May 24, 2018
# Experiment 2 dates: 


# Load packages ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(lme4)
library(viridis)
library(ggcorrplot)
library(zoo)

# Round 1 - Spring 2018 -------------------------------------------------
## **Import and clean data -----------------------------------------------
mc1.df <- read_csv("D2018_MicroCol_Round1.csv", skip = 2, 
                   col_names = c(
                     "id",
                     "date",
                     "time",
                     "initials",
                     "n_new_drones",
                     "n_drones",
                     "n_worker_deaths",
                     "age_class",
                     "workers_replaced",
                     "drones_removed",
                     "activity",
                     "dom_worker",
                     "mc_mass",
                     "p_mass_rm",
                     "n_mass_rm",
                     "p_mass_fd",
                     "n_mass_fd",
                     "n_culled",
                     "frozen?",
                     "temp",
                     "humidity"
                   ), 
                   na = c("-", "", " "))
# Change date column to an actual date format
mc1.df$date <- paste(mc1.df$date, "2018", sep = "/")
mc1.df$date <- mdy(mc1.df$date)
mc1.df$date <- parse_date(mc1.df$date)
# Remove end values from data frame that aren't used in plotting/analysis
mc1.df <- mc1.df[-c(409:425), ]

# Remove unused columns from data frame
mc1.df$initials = NULL
mc1.df$time = NULL
mc1.df$n_culled = NULL
mc1.df$dom_worker = NULL
mc1.df$`frozen?` = NULL
mc1.df$age_class = NULL

# Bind final observations not on MC data sheets
mc1end.df <- read_csv("D2018_MicroCol_Breakdown.csv", skip = 1,
                    col_names = c("id", "end_mc_mass", "end_mass_comb", "mass_box", "date"))
mass.box.df <- data_frame(id = mc1end.df$id, mass_box = mc1end.df$mass_box)

# Add mass of microcolony box to all obsvs
mc1.df <- inner_join(mc1.df, mass.box.df, by = "id")

# Create treatment variable
mc1.df$treatment <- ifelse(mc1.df$id < 2,
                           paste("zone.1"),
                           ifelse(
                             mc1.df$id < 3 & mc1.df$id > 1,
                             paste("zone.2"),
                             ifelse(mc1.df$id < 4 & mc1.df$id > 3, 
                                    paste("zone.3"), 
                                    paste("zone.4"))
                           ))
mc1end.df$treatment <- ifelse(mc1end.df$id < 2,
                           paste("zone.1"),
                           ifelse(
                             mc1end.df$id < 3 & mc1end.df$id > 1,
                             paste("zone.2"),
                             ifelse(mc1end.df$id < 4 & mc1end.df$id > 3, 
                                    paste("zone.3"), 
                                    paste("zone.4"))
                           ))
mc1.df <- mc1.df %>%
dplyr::group_by(id) %>%
dplyr::mutate(fd.day = row_number())
# Correct worker replace/drone removed to be amount actually replaced/removed
# e.g. 3/5 means 3 of 5 total were replaced/removed 
mc1.df <- separate(mc1.df, drones_removed,
                   into = c("drones_removed", "delete"),
                   sep = "[[:punct:]]", 
                   remove = TRUE)
mc1.df$delete = NULL 
# Values to be over-written <- values copied - only if drones_removed = 'yes'
mc1.df[which(mc1.df$drones_removed == "yes"), 8] <- mc1.df[which(mc1.df$drones_removed == "yes"), 4] 

mc1.df$n_new_drones[is.na(mc1.df$n_new_drones)] <- 0
# Export cleaned .csv ready for summary and analysis
write_csv(mc1.df, "./D2018_MicroCol_Round1_Clean.csv")
mc1.df <- read_csv("./D2018_MicroCol_Round1_Clean.csv")


## **Calculate food consumption/colony growth ------------------------------
mc1.df <- mc1.df %>%
  group_by(id) %>%
  mutate(true_mc_mass = mc_mass - mass_box - lag(p_mass_fd))

# Split off feed variables to seperate data frame
mc1.feed.df <- data_frame(id = mc1.df$id, 
                   date = mc1.df$date,
                   p_mass_rm = mc1.df$p_mass_rm, 
                   p_mass_fd = mc1.df$p_mass_fd, 
                   n_mass_fd = mc1.df$n_mass_fd, 
                   n_mass_rm = mc1.df$n_mass_rm)
# Remove end records not used in feed calcs
mc1.feed.df <- mc1.feed.df[-c(409:456), ] # OR
mc1.feed.df <- mc1.feed.df[-c(409:432), ] 

# Two options to replace NA with 0, second one is better given else statement
# is unnecessary
# mc1.feed.df$p_mass_rm <- ifelse(is.na(mc1.feed.df$p_mass_fd), paste("0"), mc1.feed.df$p_mass_rm)
# food.rm <- which(!is.na(mc1.feed.df2$p_mass_rm)) # grab index of pollen remaining to be subtracted
mc1.feed.df$p_mass_rm[is.na(mc1.feed.df$p_mass_fd)] <- 0.00 # Add 0 for start position on replicate
mc1.feed.df$p_mass_fd[is.na(mc1.feed.df$p_mass_fd)] <- 2.00 # Initial feed for all treatments

pollen.subtract <- mc1.feed.df %>%
  filter(!is.na(p_mass_rm)) # Pull pollen remaining to be subtracted
pollen.subtract <- as.vector(pollen.subtract$p_mass_rm) # Convert to vector
pollen.subtract <- tail(pollen.subtract, -1) # Remove first value which is 0 - 
# this aligns the values to be subtracted to the correct sum

start <- which(!is.na(mc1.feed.df$p_mass_rm)) # & mc1.feed.df$p_mass_rm > 0) # start position for summing pollen feed
stop <- start - 1 # stop points are 1 behind the start
start <- head(start, -1) # trim off last start index
stop <- tail(stop, -1) # trim off first stop index to correctly align

pollen.sum <- c(rep(0, length(start))) # Initialize pollen.sum vector
for (i in 1:length(start)) {
    for (j in start[i]:stop[i]) {
      pollen.sum[i] <- pollen.sum[i] + mc1.feed.df$p_mass_fd[j]
  }
} # Nested for loop to sum pollen provided over start:stop index values, append
# to the pollen.sum vector

pollen.subtract[pollen.subtract == 0] <- NA # Replace 0's (start points) with NA
pollen.consumed <- pollen.sum - pollen.subtract # Calculate differences - NA's 
# for very last feed as we didn't measure final pollen remaining
length(pollen.subtract) == length(pollen.sum - pollen.subtract) # Check to see that
# vector lengths are equal
mc1.feed.df$p_mass_cons <- NA # Create column for pollen consumption
mc1.feed.df$p_mass_cons[stop + 1] <- pollen.consumed # Paste pollen consumed values

fd.days <- ((stop - start) + 1) # Number of days between feedings
mc1.feed.df$p_mass_cons_avg <- NA # initialize new column and fill with NAs

# Where mc1.feed.df$p_pass_cons is not an NA, subtract 1 from that index value and
# then write the value of where mc1.feed.df$p_mass_cons is not an NA divided by 
# the number of days between feedings (average interval pollen consumption)
mc1.feed.df$p_mass_cons_avg[which(!is.na(mc1.feed.df$p_mass_cons)) - 1] <-
  mc1.feed.df$p_mass_cons[which(!is.na(mc1.feed.df$p_mass_cons))] / fd.days 

# Fill those values up since they're the same for each record in that interval
mc1.feed.df <- fill(mc1.feed.df, p_mass_cons_avg, .direction = "up")

# Nectar consumption and cleanup
mc1.feed.df[52, 6] <- 0.00 # Fix error from original csv
mc1.feed.df[329, 6] <- 17.67 # Fix error from original csv
mc1.feed.df$n_mass_cons <- NA # Initialize new column for nectar consumption 

# Where n_mass_rm is an NA, paste 0 in the n_mass_rm column
mc1.feed.df$n_mass_rm[which(is.na(mc1.feed.df$n_mass_rm))] <- 0.00 
# Calculate nectar consumed 
mc1.feed.df$n_mass_cons <- mc1.feed.df$n_mass_fd - lead(mc1.feed.df$n_mass_rm)

# Move values of n_mass_cons up 1 to line up with microcolony id
mc1.feed.df <- mc1.feed.df %>%
  mutate(n_mass_cons = lag(n_mass_cons, 1))
mc1.feed.df$n_mass_cons[which(mc1.feed.df$n_mass_rm == 0)] <- NA
mc1.feed.df$n_mass_cons[which(is.na(mc1.feed.df$n_mass_cons))] <- 0.00

# Create treatment variable in mc1.feed.df 
mc1.feed.df$treatment <- ifelse(
  mc1.feed.df$id < 2,
  paste("zone.1"),
  ifelse(
    mc1.feed.df$id < 3 & mc1.feed.df$id > 1,
    paste("zone.2"),
    ifelse(
      mc1.feed.df$id < 4 &
        mc1.feed.df$id > 3,
      paste("zone.3"),
      paste("zone.4")
    )
  )
)
mc1.feed.df$treatment <- as.factor(mc1.feed.df$treatment)

# Calculate true microcolony mass 
x <- list()
y <- 0
for (i in fd.days) {
  y <- y + 1
  if (i == 4) {
    z <- 3:0
  } else {
    z <- 3:1
  }
  x[[y]] <- z
}

for (i in fd.days) {
  if (i == 4) {
    print("true")
  } else {
    print("false")
  }
}

fd.days.count <- unlist(x)
fd.days.count <- c(fd.days.count, c(NA, NA, NA))
mc1.feed.df$fd.day.count <- fd.days.count

mc1.feed.df <- mc1.feed.df %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(fd.day.count = c(1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 1 ,2))

mc1.feed.df <- mc1.feed.df %>%
  group_by(id) %>%
  mutate(fd.day = row_number())
mc1.feed.df <- mc1.feed.df %>%
  ungroup() %>%
  mutate(mc_mass = mc1.df$mc_mass) %>%
  group_by(id) %>%
  mutate(mc_mass_init = mc_mass[fd.day == 1] - 3)

mc1.feed.df <- mc1.feed.df %>%
  ungroup() %>%
  mutate(mc_mass_true = ifelse(
    fd.day.count == 1,
    ((mc_mass - mc_mass_init) + p_mass_cons_avg),
    ifelse(
      fd.day.count == 2,
      (((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) + p_mass_cons_avg),
      ifelse(
        fd.day.count == 3,
        ((((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) - lag(p_mass_fd, n = 2)) + p_mass_cons_avg),
        ifelse(fd.day.count == 0,
               ((((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) - lag(p_mass_fd, n = 2)) - lag(p_mass_fd, n = 3)) + p_mass_cons_avg, 0)
      )
    )
  ))

# Create before/during/after/between pulse cateogorizations 
mc1.feed.df <- mc1.feed.df %>%
  ungroup() %>%
  mutate(pulse_cat = ifelse(
    fd.day <= 3, 
    "b1",
    ifelse(
      fd.day > 3 & fd.day <= 6,
      "p1",
      ifelse(
        fd.day > 6 & fd.day <= 10,
        "a1.b2",
        ifelse(fd.day >10 & fd.day <= 13,
               "p2",
               "a2")
      )
    )
  ))
mc1.feed.df$pulse_cat <- as.factor(mc1.feed.df$pulse_cat)

# Interpolate missing values of mc_mass_true

mc1.feed.df <- mc1.feed.df %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(mc_mass_true_intrp = na.approx(mc_mass_true, 
                                        maxgap = 4,
                                        rule = 2))

# Write csv file to working directory
write_csv(mc1.feed.df, "./D2018_MicroCol_Round1_Feed_Clean.csv")
mc1.feed.df <- read_csv("./D2018_MicroCol_Round1_Feed_Clean.csv")


# **Drone fitness ---------------------------------------------------------
mc1.drone.it.df <- read_csv("./D2018_MC1_DroneIT.csv")
mc1.drone.mass.df <- read_csv("./D2018_MC1_DroneMass.csv")

mc1.drone.it.df$X5 <- NULL
mc1.drone.it.df <- mc1.drone.it.df %>%
  fill(date, id, .direction = "down")


mc1.drone.it.df$treatment <- ifelse(
  mc1.drone.it.df$id < 2,
  paste("zone.1"),
  ifelse(
    mc1.drone.it.df$id < 3 & mc1.drone.it.df$id > 1,
    paste("zone.2"),
    ifelse(
      mc1.drone.it.df$id < 4 & mc1.drone.it.df$id > 3,
      paste("zone.3"),
      paste("zone.4")
    )
  )
)

mc1.drone.mass.df$treatment <- ifelse(
  mc1.drone.mass.df$id < 2,
  paste("zone.1"),
  ifelse(
    mc1.drone.mass.df$id < 3 & mc1.drone.mass.df$id > 1,
    paste("zone.2"),
    ifelse(
      mc1.drone.mass.df$id < 4 & mc1.drone.mass.df$id > 3,
      paste("zone.3"),
      paste("zone.4")
    )
  )
)


mc1.drone.it.df <- mc1.drone.it.df %>%
  filter(!is.na(it.distance))
mc1.drone.mass.df <- mc1.drone.mass.df %>%
  filter(!is.na(avg.individ.mass))

write_csv(mc1.drone.it.df, "./D2018_MC1_DroneIT_Clean.csv")
write_csv(mc1.drone.mass.df, "./D2018_MC1_DroneMass_Clean.csv")

mc1.drone.it.df <- read_csv("./D2018_MC1_DroneIT_Clean.csv")
mc1.drone.mass.df <- read_csv("./D2018_MC1_DroneMass_Clean.csv")


# Summarise IT distance by id, date
mc.drone.it.df %>%
  group_by(date, id) %>%
  summarise(total.drones = n(), 
            mean.it = mean(it.distance, 
                           na.rm = TRUE),
            sd.it = sd(it.distance, 
                       na.rm = TRUE),
            se.it = sd.it / sqrt(total.drones))

# Summarise IT distance by treatment, date
mc.drone.it.df %>%
  group_by(date, treatment) %>%
  summarise(total.drones = n(), 
            mean.it = mean(it.distance,
                           na.rm = TRUE),
            sd.it = sd(it.distance,
                       na.rm = TRUE),
            se.it = sd.it / sqrt(total.drones))
  
# Summarise IT distance by treatment
mc.drone.it.df %>%
  group_by(treatment) %>%
  summarise(total.drones = n(), 
            mean.it = mean(it.distance,
                           na.rm = TRUE),
            sd.it = sd(it.distance,
                       na.rm = TRUE),
            se.it = sd.it / sqrt(total.drones))

# check if drone numbers match after summary
all_equal(sum(mc.drone.it.sum.df$total.drones), 
          nrow(mc.drone.it.df)) 

# Summarise mass by id
mc.drone.mass.df %>%
  group_by(id) %>%
  summarise(total.drones = sum(n.drones),
            mean.mass = mean(avg.individ.mass, 
                              na.rm = TRUE),
            sd.mass = sd(avg.individ.mass,
                          na.rm = TRUE),
            se.mass = sd.mass / sqrt(total.drones))

# Summarise mass by date, treatment
mc.drone.mass.df %>%
  group_by(date, treatment) %>%
  summarise(total.drones = sum(n.drones),
            mean.mass = mean(avg.individ.mass, 
                             na.rm = TRUE),
            sd.mass = sd(avg.individ.mass,
                         na.rm = TRUE),
            se.mass = sd.mass / sqrt(total.drones))

# Summarise mass by treatment
mc.drone.mass.df %>%
  group_by(treatment) %>%
  summarise(total.drones = sum(n.drones),
            mean.mass = mean(avg.individ.mass, 
                             na.rm = TRUE),
            sd.mass = sd(avg.individ.mass,
                         na.rm = TRUE),
            se.mass = sd.mass / sqrt(total.drones))



# Round 2 - Summer 2018 ---------------------------------------------------
# **Import and clean data -------------------------------------------------
mc2.df <- read_csv("D2018_MicroCol_Round2.csv",
                   skip = 1,
                   col_names = c(
                     "id",
                     "date",
                     "time",
                     "initials",
                     "temp",
                     "humidity",
                     "activity",
                     "mc_mass",
                     "n_drones",
                     "n_new_drones",
                     "n_worker_deaths",
                     "brood",
                     "workers_replaced",
                     "drones_removed",
                     "p_mass_rm",
                     "n_mass_rm",
                     "p_mass_fd",
                     "n_mass_fd"),
                     na = c(" ", "N/A", "x")
                   )
mc2.df$date <- mdy(mc2.df$date)
mc2.df$date <- parse_date(mc2.df$date)
mc2.end.df <- read_csv("./D2018_MicroCol_Round2_BroodMass.csv",
                       skip = 1,
                       col_names = c(
                         "id",
                         "end_mass_comb",
                         "mass_box"
                       ))
mc2.end.df$mass_box <- NULL
mc2.df$time <- NULL
mc2.df$initials <- NULL

mc.boxmass.df <- data_frame(id = mc1end.df$id, 
                            mass_box = mc1end.df$mass_box)
mc.boxmass.avg <- mean(mc.boxmass.df$mass_box, 
                       na.rm = TRUE)

mc2.df <- left_join(mc2.df, 
                     mc.boxmass.df, 
                     by = "id")
mc2.df <- left_join(mc2.df, 
                     mc2.end.df,
                     by = "id")

mc2.df$mass_box[is.na(mc2.df$mass_box)] <- mc.boxmass.avg

mc2.df$treatment <- ifelse(mc2.df$id < 2,
                           paste("zone.1"),
                           ifelse(
                             mc2.df$id < 3 & mc2.df$id > 1,
                             paste("zone.2"),
                             ifelse(mc2.df$id < 4 & mc2.df$id > 3,
                                    paste("zone.3"),
                                    paste("zone.4"))
                           ))
mc2.end.df$treatment <- ifelse(
  mc2.end.df$id < 2,
  paste("zone.1"),
  ifelse(
    mc2.end.df$id < 3 & mc2.end.df$id > 1,
    paste("zone.2"),
    ifelse(
      mc2.end.df$id < 4 & mc2.end.df$id > 3,
      paste("zone.3"),
      paste("zone.4")
    )
  )
)
mc2.df <- mc2.df %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(fd.day = row_number())

mc2.feed.df <- mc2.feed.df %>%
  mutate(mc_mass_diff = c(NA, diff(mc_mass_true)))

write_csv(mc2.df, "./D2018_MicroCol_Round2_Clean.csv")
mc2.df <- read_csv("./D2018_MicroCol_Round2_Clean.csv")


# **Calc food consumption/colony growth -----------------------------------
mc2.feed.df <- data_frame(id= mc2.df$id, 
                          date = mc2.df$date,
                          mc_mass = mc2.df$mc_mass,
                          p_mass_rm = mc2.df$p_mass_rm,
                          p_mass_fd = mc2.df$p_mass_fd,
                          n_mass_fd = mc2.df$n_mass_fd,
                          n_mass_rm = mc2.df$n_mass_rm,
                          mass_box = mc2.df$mass_box,
                          treatment = mc2.df$treatment)

mc2.feed.df <- mc2.feed.df %>%
  group_by(id) %>%
  mutate(fd.day = row_number())

# Insert row above first feed day (day 0) to account for 3g of pollen for 
# microcolony initiation 
mc2.feed.init <- mc2.feed.df[mc2.feed.df$fd.day == 1, ]
mc2.feed.init$n_mass_rm <- NA
mc2.feed.init$n_mass_fd <- NA
mc2.feed.init$p_mass_fd <- 3.000
mc2.feed.init$fd.day <- 0

mc2.feed.df <- mc2.feed.df %>%
  bind_rows(mc2.feed.init) %>%
  arrange(id, fd.day)

pollen.subtract.r2 <- mc2.feed.df %>%
  filter(!is.na(p_mass_rm))
pollen.subtract.r2 <- as.vector(pollen.subtract.r2$p_mass_rm)

mc2.feed.df$p_mass_rm[mc2.feed.df$p_mass_fd == 3] <- 0.0 

start.r2 <- which(!is.na(mc2.feed.df$p_mass_rm))
stop.r2 <- start.r2 - 1 # stop points are 1 behind the start
#start.r2 <- head(start.r2, -1) # trim off last start index
stop.r2 <- tail(stop.r2, -1) # trim off first stop index to correctly align
stop.r2 <- c(stop.r2, 476)

print(start.r2)
print(stop.r2)

pollen.sum.r2 <- c(rep(0, length(start.r2))) # Initialize pollen.sum vector
for (i in 1:length(start.r2)) {
  for (j in start.r2[i]:stop.r2[i]) {
    pollen.sum.r2[i] <- pollen.sum.r2[i] + mc2.feed.df$p_mass_fd[j]
  }
}
print(pollen.sum.r2)

length(pollen.subtract.r2) == length(pollen.sum.r2 - pollen.subtract.r2) # Check equality
# not - need to remove last pollen sum from each MC id - remove every 6th entry of pollen sum 
psum.rm <- seq(6, 168, by = 6) # Index pos. of each 
pollen.sum.r2 <- pollen.sum.r2[-psum.rm]
print(pollen.sum.r2)
length(pollen.subtract.r2) == length(pollen.sum.r2) # TRUE
pollen.consumed.r2 <- pollen.sum.r2 - pollen.subtract.r2
print(pollen.consumed.r2) # 15 values are negative w/mean value of 0.107 grams

mc2.feed.df$p_mass_cons <- NA
pcons.index <- stop.r2[-psum.rm]
mc2.feed.df$p_mass_cons[pcons.index + 1] <- pollen.consumed.r2
mc2.feed.df$p_mass_cons[which(mc2.feed.df$p_mass_rm == 0)] <- 0.00

# Calculate average daily pollen/nectar consumption 
mc2.feed.df$p_mass_cons_avg <- NA
fd.days.r2 <- ((stop.r2 - start.r2) + 1)
mc2.feed.df$p_mass_cons_avg[which(!is.na(mc2.feed.df$p_mass_cons))] <-
  mc2.feed.df$p_mass_cons[which(!is.na(mc2.feed.df$p_mass_cons))] / fd.days.r2
mc2.feed.df <- fill(mc2.feed.df, 
                    p_mass_cons_avg, 
                    .direction = "up")
mc2.feed.df$p_mass_cons_avg[mc2.feed.df$p_mass_cons_avg == 0] <- NA
mc2.feed.df$p_mass_cons_avg <- as.numeric(mc2.feed.df$p_mass_cons_avg) # for some 
# reason it was boolean...? 

mc2.feed.df$n_mass_cons <- NA
mc2.feed.df$n_mass_rm <- as.numeric(mc2.feed.df$n_mass_rm)
mc2.feed.df$n_mass_cons <- mc2.feed.df$n_mass_fd - lead(mc2.feed.df$n_mass_rm)
mc2.feed.df$n_mass_cons <- as.numeric(mc2.feed.df$n_mass_cons)

mc2.feed.df$treatment <- as.factor(mc2.feed.df$treatment)

write_csv(mc2.feed.df, "./D2018_MicroCol_Round2_Feed_Clean.csv")

# Old school way to do feed days - did in tidy version below
x <- list()
y <- 0
for (i in fd.days.r2) {
  y <- y + 1
  if (i == 4) {
    z <- 3:0
  } else {
    z <- 3:1
  }
  x[[y]] <- z
}
fd.days.count <- unlist(x)
test <- mc2.feed.df
test$fd.day.count <- fd.days.count

# Feed day counts for use in true mass calculation
mc2.feed.df <- mc2.feed.df %>%
  group_by(id) %>%
  mutate(fd.day.count = c(0, rep(seq(3, 1, by = -1), 5), 0))

# Calculate initial mass at start of experiment - first obsv. minus 3 grams of 
# pollen added to initiate microcolony.  Mass gains/losses from this point are 
# calculated relative to this mc_mass_init value. 
mc2.feed.df <- mc2.feed.df %>%
  group_by(id) %>%
  mutate(mc_mass_init = mc_mass[fd.day == 0] - 3)

# Calculate "true" mass relative to initiation mass.
mc2.feed.df <- mc2.feed.df %>%
  ungroup() %>%
  mutate(mc_mass_true = ifelse(
    fd.day.count == 3,
    ((mc_mass - mc_mass_init) + p_mass_cons_avg),
    ifelse(
      fd.day.count == 2,
      (((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) + p_mass_cons_avg),
      ifelse(
        fd.day.count == 1,
        ((((mc_mass - mc_mass_init) - lag(p_mass_fd, n = 1)) - lag(p_mass_fd, n = 2)) + p_mass_cons_avg),
        0
        )
      )
    ))

mc2.feed.df$mc_mass_true[mc2.feed.df$mc_mass_true == 0] <- NA
mc2.feed.df$mc_mass_true[mc2.feed.df$fd.day == 0] <- 0.00
mc2.feed.df$date[mc2.feed.df$mc_mass_true == 0] <- "2018-06-19"

mc2.feed.df <- mc2.feed.df %>%
  ungroup() %>%
  mutate(pulse_cat = ifelse(
    fd.day <= 3, 
    "b1",
    ifelse(
      fd.day > 3 & fd.day <= 6,
      "p1",
      ifelse(
        fd.day > 6 & fd.day <= 10,
        "a1.b2",
        ifelse(fd.day >10 & fd.day <= 13,
               "p2",
               "a2")
      )
    )
  ))
mc2.feed.df$pulse_cat <- as.factor(mc2.feed.df$pulse_cat)

write_csv(mc2.feed.df, "./D2018_MicroCol_Round2_Feed_Clean.csv")
mc2.feed.df <- read_csv("./D2018_MicroCol_Round2_Feed_Clean.csv")




# **Drone fitness ---------------------------------------------------------
mc2.drone.df <- mc2.df %>%
  group_by(id, treatment) %>%
  summarise(total_drones = sum(n_new_drones, na.rm = TRUE))


# Save data to WD for Rmd -------------------------------------------------
save(mc1.df, file = "mc1df.Rdata")
save(mc2.df, file = "mc2df.Rdata")
save(mc1end.df, file = "mc1enddf.Rdata")
save(mc2.end.df, file = "mc2enddf.Rdata")
save(mc1.feed.df, file = "mc1feeddf.Rdata")
save(mc2.feed.df, file = "mc2feeddf.Rdata")
save(mc.massgain, file = "mcmassgain.Rdata")
save(mc1.drone.it.df, file = "mc1droneit.Rdata")
save(mc1.drone.mass.df, file = "mc1dronemass.Rdata")
