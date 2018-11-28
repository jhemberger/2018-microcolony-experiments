# METADATA===================================================================
# 2018 BUMBLE BEE MICROCOLONY EXPERIMENTS
# Jeremy Hemberger - j.hemberger.wisc@gmail.com
# May 29, 2018

# Model microcolony response to landscape-simulated food treatments
# Experiment 1 dates: March 15, 2018 - May 24, 2018
# Experiment 2 dates: 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

##### Import relevant packages #####
library(tidyverse)
library(lubridate)
library(lme4)
library(viridis)
library(ggcorrplot)

##### Import/clean data #####
# Experiment 1
mc1.df <- read_csv("D2018_MicroCol_Round1.csv", skip = 2, 
                   col_names = c("id", "date", "time", "initials", "n_new_drones", 
                                 "n_drones", "n_worker_deaths", "age_class",
                                 "workers_replaced", "drones_removed",
                                 "activity", "dom_worker", "mc_mass", "p_mass_rm", 
                                 "n_mass_rm", "p_mass_fd", "n_mass_fd", "n_culled", 
                                 "frozen?", "temp", "humidity"), 
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
                             ifelse(mc1.df$id < 4 &
                                      mc1.df$id > 3, paste("zone.3"), paste("zone.4"))
                           ))
mc1end.df$treatment <- ifelse(mc1end.df$id < 2,
                           paste("zone.1"),
                           ifelse(
                             mc1end.df$id < 3 & mc1end.df$id > 1,
                             paste("zone.2"),
                             ifelse(mc1end.df$id < 4 &
                                      mc1end.df$id > 3, paste("zone.3"), paste("zone.4"))
                           ))

# Correct worker replace/drone removed to be amount actually replaced/removed
# e.g. 3/5 means 3 of 5 total were replaced/removed 
mc1.df <- separate(mc1.df, drones_removed,
                   into = c("drones_removed", "delete"),
                   sep = "[[:punct:]]", 
                   remove = TRUE)
mc1.df$delete = NULL 
# Values to be over-written <- values copied - only if drones_removed = 'yes'
mc1.df[which(mc1.df$drones_removed == "yes"), 8] <- mc1.df[which(mc1.df$drones_removed == "yes"), 4] 

# Export cleaned .csv ready for summary and analysis
write_csv(mc1.df, "./D2018_MicroCol_Round1_Clean.csv")
mc1.df <- read_csv("./D2018_MicroCol_Round1_Clean.csv")

##### Summarize/calculate colony growth and resource consumption #####
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
mc1.feed.df$treatment <- as.factor(mc1.feed.df$treatment) # Coerce treatment as factor

# Write csv file to working directory
write_csv(mc1.feed.df, "./D2018_MicroCol_Round1_Feed_Clean.csv")

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
mc.massgain$fd.days <- fd.days.count

mc.massgain.test <- mc.massgain %>%
  filter(id == 3.3) %>%
  mutate(mass_box = 433.61)
mc.massgain.test <- mc.massgain.test %>%
  mutate(interval_day = 1:nrow(mc.massgain.test)) %>%
  mutate(mc_mass_true = ifelse(
    fd.days == 3,
    mc_mass - mass_box + p_mass_cons,
    ifelse(
      fd.days == 2,
      mc_mass - mass_box - lag(p_mass_fd, n = 1) + p_mass_cons,
      ifelse(
        fd.days == 1,
        mc_mass - mass_box - lag(p_mass_fd, n = 1) - lag(p_mass_fd, n = 2) + p_mass_cons,
        ifelse(
          fd.days == 0,
          mc_mass - mass_box - lag(p_mass_fd, n = 1) - lag(p_mass_fd, n = 2), - lag(p_mass_fd, n = 3) + p_mass_cons
        )
      )
    )))
  
  


##### Basic summary plots/tables #####
# Final comb mass
mc1end.df %>%
  filter(!is.na(end_mass_comb)) %>%
  group_by(treatment) %>%
  summarise(mean.comb.mass = mean(end_mass_comb), se = sd(end_mass_comb) / sqrt(n())) %>%
  ggplot() + 
    #geom_col(mapping = aes(x = treatment, y = mean.comb.mass, width = 0.5)) + 
    geom_pointrange(mapping = aes(x = treatment, y = mean.comb.mass,
                                  ymax = mean.comb.mass + se, 
                                  ymin = mean.comb.mass - se)) +
    scale_y_continuous(limits = c(0, 10)) + 
    theme_bw()

ggplot(na.rm = TRUE) + 
  geom_line(data = mc1.df, mapping = aes(x = date, 
                                         y = true_mc_mass, 
                                         group = id, 
                                         color = treatment,
                                         na.rm = FALSE)) + 
  theme_bw()

# Total Males Produced as point range plot
mc1.df %>%
  group_by(id, treatment) %>%
  filter(id != 2.4) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  summarise(mean_males = mean(total_males), se = sd(total_males) / sqrt(n())) %>%
  ggplot() + 
    geom_pointrange(mapping = aes(x = treatment, y = mean_males, 
                                  ymax = mean_males + se,
                                  ymin = mean_males - se)) + 
    theme_bw()

# Total males produced as boxplot
mc1.df %>%
  group_by(id, treatment) %>%
  filter(id != 2.4) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  group_by(treatment) %>%
  ggplot(mapping = aes(x = treatment, y = total_males)) + 
    geom_boxplot() + 
    theme_minimal()

# Male production normalized across microcolonies - diverging dot plot
mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(drone_z = round((total_males - mean(total_males, na.rm = TRUE)) / sd(total_males, na.rm = TRUE), digits = 2)) %>%
  mutate(drone_score = ifelse(drone_z < 0, "below", "above")) %>%
  arrange(desc(drone_z)) %>%
  mutate(z_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = z_order, y = drone_z, label = drone_z)) + 
    geom_point(stat = "identity", mapping = aes(col = drone_score), size = 8) + 
    scale_color_manual(name = "Drone Production", 
                       labels = c("Above Average", "Below Average"),
                       values = c("above" = "green2", "below" = "red2")) + 
    geom_text(color = "black", size = 2) + 
    labs(y = "Drone Production Z-Score", x = "Microcolony ID") + 
    coord_flip() + 
    theme_bw() 

# Mass gain over experiment (smoothed)
ggplot(data = mc1.df) + 
  # geom_point(mapping = aes(x = date, y = mc_mass, #- mass_box - lag(p_mass_fd),
  #                          color = treatment)) + 
  geom_smooth(span = 0.75, 
              mapping = aes(x = date, 
                            y = mc_mass, #- mass_box - lag(p_mass_fd), 
                            color = treatment,
                            fill = treatment)) + 
  theme_bw()

# Nectar consumptiion grouped by treatment
mc1.feed.df %>%
  group_by(treatment, date) %>%
  summarise(nectar_cons = mean(n_mass_cons),
            nectar_se = (sd(n_mass_cons) / (sqrt(n()))), 
            pollen_cons = mean(p_mass_cons_avg), 
            pollen_se = (sd(p_mass_cons_avg) / sqrt(n())))

mc1.feed.df %>%
  group_by(id) %>%
  mutate(cum_pollen = cumsum(p_mass_cons_avg), 
         cum_nectar = cumsum(n_mass_cons)) %>%
  group_by(date, treatment) %>%
  summarise(nectar_cons = mean(cum_nectar), 
            nectar_se = (sd(cum_nectar) / (sqrt(n()))), 
            pollen_cons = mean(cum_pollen, na.rm = TRUE), 
            pollen_se = (sd(cum_pollen, na.rm = TRUE) / (sqrt(n())))) %>%
  # ggplot() +
  # geom_pointrange(mapping = aes(x = date,
  #                     y = nectar_cons,
  #                     ymin = nectar_cons - nectar_se,
  #                     ymax = nectar_cons + nectar_se,
  #                     color = treatment)) +
  # geom_smooth(mapping = aes(x = date,
  #                           y = nectar_cons,
  #                           color = treatment), se = FALSE) + 
  # theme_minimal() #%>% 
  ggplot() +
  geom_pointrange(mapping = aes(x = date,
                                y = pollen_cons,
                                ymin = pollen_cons - pollen_se,
                                ymax = pollen_cons + pollen_se,
                                color = treatment)) +
  geom_smooth(mapping = aes(x = date,
                          y = pollen_cons,
                          color = treatment), se = FALSE) +
  theme_bw()

# Average interval mass gain across entire experiment - diverging dot plot
mc.massgain <- mc1.df %>%
  select(id, date, mc_mass, p_mass_rm, p_mass_fd, treatment, mass_box) %>%
  mutate(p_mass_cons = mc1.feed.df$p_mass_cons_avg) %>%
  replace_na(list(mc_mass = 0)) %>%
  mutate(mc_mass_diff = c(0, diff(mc1.df$mc_mass))) %>%
  #filter(id == "3.3") %>%
  mutate(mc_mass_gain = ifelse(lag(!is.na(p_mass_rm == TRUE)), 
                               mc_mass_diff + lag(p_mass_rm) - lag(p_mass_fd) + p_mass_cons, 
                               mc_mass_diff - lag(p_mass_fd) + p_mass_cons)) # this needs to be checked...

mc.massgain$mc_mass_gain[which(diff(mc.massgain$id) != 0) + 1] <- NA

# plot using z-scores
mc.massgain %>% 
  group_by(id, treatment) %>%
  summarise(mean_intv_massgain = mean(mc_mass_gain, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(mass_gain_z = round((mean_intv_massgain - mean(mean_intv_massgain, na.rm = TRUE)) / sd(mean_intv_massgain, na.rm = TRUE), digits = 2)) %>%
  mutate(mass_score = ifelse(mass_gain_z < 0, "below", "above")) %>%
  arrange(desc(mass_gain_z)) %>%
  mutate(z_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = z_order, y = mass_gain_z, label = mass_gain_z)) + 

  geom_point(stat = "identity", mapping = aes(col = mass_score), size = 8) + 
  scale_color_manual(name = "Mass gain between feeding intervals", 
                     labels = c("Above Average", "Below Average"),
                     values = c("above" = "green2", "below" = "red2")) + 
  geom_text(color = "black", size = 2) + 
  labs(y = "Mass Gain Z-Score", x = "Microcolony ID") + 
  coord_flip() + 
  theme_bw()
  
# plot using raw average mass gains (still above/below 0)
mc.massgain %>% 
  group_by(id, treatment) %>%
  summarise(mean_intv_massgain = round(mean(mc_mass_gain, na.rm = TRUE), digits = 2)) %>%
  ungroup() %>%
  mutate(mass_score = ifelse(mean_intv_massgain < 0, "below", "above")) %>%
  arrange(desc(mean_intv_massgain)) %>%
  mutate(mass_order = factor(`id`, levels = `id`)) %>%
  ggplot(aes(x = mass_order, y = mean_intv_massgain, label = mean_intv_massgain)) + 
  geom_point(stat = "identity", mapping = aes(col = mass_score), size = 8) + 
  scale_color_manual(name = "Average change in mass between feeding intervals", 
                     labels = c("Gained Mass", "Lost Mass"),
                     values = c("above" = "slateblue", "below" = "powderblue")) + 
  geom_text(color = "white", size = 2) + 
  labs(y = "Mean feeding interval mass gain", x = "Microcolony ID") + 
  coord_flip() + 
  theme_bw()
         

# Analysis with Agathe
mc1.drone.df <- mc1.df %>%
  group_by(id, treatment) %>%
  summarise(total_males = sum(n_new_drones, na.rm = TRUE)) %>%
  filter(id != 2.4) # drop due to infighting issues
mc1.drone.df$treatment <- as.factor(mc1.drone.df$treatment)

mod.1 <- aov(total_males ~ treatment, data = mc1.drone.df)
lm.mod.1 <- lm(total_males ~ treatment, data = mc1.drone.df)
summary(lm.mod.1)
TukeyHSD(lm.mod.1)
summary(mod.1)
TukeyHSD(mod.1)

mc.massgain$treatment <- as.factor(mc.massgain$treatment)
mod.2 <- aov(mc_mass_gain ~ treatment + p_mass_cons + Error(id), data = mc.massgain)
mod.2.1 <- aov(mc_mass_gain ~ treatment + Error(id), data = mc.massgain)
summary(mod.2)
summary(mod.2.1)

mc.massgain2 <- mc.massgain %>%
  filter(id != 2.4)
mod.x <- lm(mc_mass_gain ~ treatment, data = mc.massgain2)
summary(mod.x)
anova(mod.x)
